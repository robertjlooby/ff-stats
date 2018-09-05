{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module BestTeams where

import Control.Lens (Lens', (^.), set)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, asks)
import Data.List (sortBy)
import Data.Set (difference, fromList, size)
import Data.Vector (Vector)
import Dhall (Generic, Interpret)
import System.Random (getStdRandom, randomR)

import Fitness
import GenerateTeam
import Teams
import Types

data Config = Config
  { _crossoverProb :: Double
  , _iterationRounds :: Integer
  , _minTeamDifference :: Integer
  , _poolSize :: Integer
  , _resultCount :: Integer
  , _salaryCap :: Integer
  , _strategy :: Strategy
  } deriving (Generic)

instance Interpret Config

pickBestLineups :: Vector PlayerWithProjected -> ReaderT Config IO [Team]
pickBestLineups players = do
  config <- ask
  teams <-
    liftIO $ replicateM (fromInteger $ _poolSize config) (generateTeam pool)
  newTeams <-
    iterateIO
      (_iterationRounds config)
      (nextGeneration (rate config))
      (return teams)
  getTop (rate config) newTeams
  where
    pool = generatePlayerPool players
    rate config = fitness (_strategy config) (_salaryCap config)

iterateIO :: Monad m => Integer -> (a -> m a) -> m a -> m a
iterateIO times iterator state
  | times <= 0 = state
  | otherwise = do
    currentState <- state
    iterateIO (times - 1) iterator (iterator currentState)

getTop :: (Team -> Float) -> [Team] -> ReaderT Config IO [Team]
getTop fitnessFn allTeams = do
  minTeamDifference <- asks _minTeamDifference
  resultCount <- asks _resultCount
  return $ go minTeamDifference resultCount sortedTeams []
  where
    sortedTeams =
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

nextGeneration :: (Team -> Float) -> [Team] -> ReaderT Config IO [Team]
nextGeneration fitnessFn teams = do
  let maxFitness = maximum $ fitnessFn <$> teams
  newTeams <-
    liftIO $ do
      putStrLn $
        "max fitness: " ++
        show maxFitness ++
        " avg fitness: " ++
        show (sum (fitnessFn <$> teams) / fromIntegral (length teams))
      replicateM (length teams) (selectFrom maxFitness fitnessFn teams)
  mutateTeams newTeams

selectFrom :: Float -> (Team -> Float) -> [Team] -> IO Team
selectFrom maxFitness fitnessFn teams = do
  index <- getStdRandom $ randomR (0, length teams - 1)
  prob <- getStdRandom $ randomR (0, 1)
  let team = teams !! index
  if fitnessFn team / maxFitness >= prob
    then return team
    else selectFrom maxFitness fitnessFn teams

mutateTeams :: [Team] -> ReaderT Config IO [Team]
mutateTeams (first:second:rest) = do
  mutated <- mutatePair first second
  mutatedRest <- mutateTeams rest
  return $ mutated ++ mutatedRest
mutateTeams teams = return teams

mutatePair :: Team -> Team -> ReaderT Config IO [Team]
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
     Lens' Team PlayerWithProjected
  -> (Team, Team)
  -> ReaderT Config IO (Team, Team)
mutate position (first, second) = do
  prob <- liftIO $ getStdRandom $ randomR (0, 1)
  crossoverProb <- asks _crossoverProb
  if prob < crossoverProb && okToSwap
    then return
           ( set position (second ^. position) first
           , set position (first ^. position) second)
    else return (first, second)
  where
    okToSwap =
      notElem (second ^. position) (allPlayers first) &&
      notElem (first ^. position) (allPlayers second)
