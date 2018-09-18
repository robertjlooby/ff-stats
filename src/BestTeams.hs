{-# LANGUAGE RankNTypes #-}

module BestTeams where

import Control.Lens (Lens', (^.), set)
import Control.Monad (foldM, replicateM)
import Control.Monad.Logger (MonadLogger)
import Data.List (sortBy)
import Data.Set (difference, fromList, size)
import Data.Vector (Vector)

import BestTeamsConfig
import Fitness
import GenerateTeam
import Teams
import Types

pickBestLineups :: MonadLogger m => Vector PlayerWithProjected -> App m [Team]
pickBestLineups players = do
  iterationRounds <- App $ asks _iterationRounds
  poolSize <- fromInteger <$> (App $ asks _poolSize)
  teams <- replicateM poolSize (generateTeam pool)
  newTeams <- foldM (\ts _ -> nextGeneration ts) teams [1 .. iterationRounds]
  getTop newTeams
  where
    pool = generatePlayerPool players

getTop :: Monad m => [Team] -> App m [Team]
getTop allTeams = do
  minTeamDifference <- App $ asks _minTeamDifference
  resultCount <- App $ asks _resultCount
  fitnessFn <- fitness
  return $ go minTeamDifference resultCount (sortedTeams fitnessFn) []
  where
    sortedTeams fitnessFn =
      sortBy (\t1 t2 -> compare (fitnessFn t2) (fitnessFn t1)) allTeams
    go _ _ [] results = results
    go minTeamDifference count (nextTeam:rest) results
      | count <= 0 = reverse results
      | otherwise =
        if noOverlap minTeamDifference nextTeam results
          then go minTeamDifference (count - 1) rest (nextTeam : results)
          else go minTeamDifference count rest results
    noOverlap minTeamDifference team results =
      let teamPlayers = fromList $ allPlayers team
          differences =
            toInteger . size . difference teamPlayers . fromList . allPlayers <$>
            results
       in all (>= minTeamDifference) differences

nextGeneration :: MonadLogger m => [Team] -> App m [Team]
nextGeneration teams = do
  fitnessFn <- fitness
  let fitnesses = fitnessFn <$> teams
  let maxFitness = maximum fitnesses
  logInfo $
    "max fitness: " ++
    show maxFitness ++
    " avg fitness: " ++ show (sum fitnesses / fromIntegral (length teams))
  newTeams <- replicateM (length teams) (selectFrom maxFitness fitnessFn teams)
  mutateTeams newTeams

selectFrom :: Monad m => Float -> (Team -> Float) -> [Team] -> App m Team
selectFrom maxFitness fitnessFn teams = do
  prob <- getRandom (0, 1)
  team <- (teams !!) <$> getRandom (0, length teams - 1)
  if fitnessFn team / maxFitness >= prob
    then return team
    else selectFrom maxFitness fitnessFn teams

mutateTeams :: Monad m => [Team] -> App m [Team]
mutateTeams (first:second:rest) = do
  mutated <- mutatePair first second
  mutatedRest <- mutateTeams rest
  return $ mutated ++ mutatedRest
mutateTeams teams = return teams

mutatePair :: Monad m => Team -> Team -> App m [Team]
mutatePair first second = do
  (first', second') <-
    mutate qb (first, second) >>= mutate rb1 >>= mutate rb2 >>= mutate wr1 >>=
    mutate wr2 >>=
    mutate wr3 >>=
    mutate te >>=
    mutate flex >>=
    mutate dst
  return [first', second']

mutate ::
     Monad m
  => Lens' Team PlayerWithProjected
  -> (Team, Team)
  -> App m (Team, Team)
mutate position (first, second) = do
  crossoverProb <- App $ asks _crossoverProb
  prob <- getRandom (0, 1)
  if prob < crossoverProb && okToSwap
    then return
           ( set position (second ^. position) first
           , set position (first ^. position) second)
    else return (first, second)
  where
    okToSwap =
      notElem (second ^. position) (allPlayers first) &&
      notElem (first ^. position) (allPlayers second)
