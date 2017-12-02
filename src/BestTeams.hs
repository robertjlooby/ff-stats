{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BestTeams where

import           Control.Lens (Lens', (^.), set)
import           Control.Monad (replicateM)
import           Data.List (sortBy)
import           Data.Set (difference, fromList, size)
import           Data.Vector (Vector)
import           Dhall (Generic, Interpret)
import           System.Random (getStdRandom, randomR)

import           Fitness
import           GenerateTeam
import           Teams
import           Types

data Config = Config
    { _iterationRounds :: Integer
    , _minTeamDifference :: Integer
    , _poolSize :: Integer
    , _resultCount :: Integer
    , _salaryCap :: Integer
    } deriving (Generic)

instance Interpret Config

pickBestLineups :: Config -> Vector PlayerWithProjected -> IO [Team]
pickBestLineups config players = do
    teams <- replicateM (fromInteger $ _poolSize config) (generateTeam pool)
    newTeams <- iterateIO (_iterationRounds config) (nextGeneration rate) (return teams)
    return $ getTop rate (_minTeamDifference config) (_resultCount config) newTeams
  where
    pool = generatePlayerPool players
    rate = fitness (_salaryCap config)

iterateIO :: Integer -> (a -> IO a) -> IO a -> IO a
iterateIO times iterator state
    | times <= 0 = state
    | otherwise  = do
        currentState <- state
        iterateIO (times - 1) iterator (iterator currentState)

getTop :: (Team -> Float) -> Integer -> Integer -> [Team] -> [Team]
getTop fitnessFn minTeamDifference resultCount allTeams =
    go resultCount sortedTeams []
  where
    sortedTeams = sortBy (\t1 t2 -> compare (fitnessFn t2) (fitnessFn t1)) allTeams
    go count teams results
        | count <= 0 = reverse results
        | otherwise  =
            let (nextTeam:rest) = teams
            in
                if nextTeam `noOverlap` results then
                    go (count - 1) rest (nextTeam:results)
                else
                    go count rest results
    noOverlap team results =
        let teamPlayers = fromList $ allPlayers team
            differences = (toInteger . size . (difference teamPlayers) . fromList . allPlayers) <$> results
        in
            all (\diff -> diff >= minTeamDifference) differences

nextGeneration :: (Team -> Float) -> [Team] -> IO [Team]
nextGeneration fitnessFn teams = do
    let maxFitness = maximum $ fitnessFn <$> teams
    putStrLn $
        "max fitness: " ++ show maxFitness ++
        " avg fitness: " ++ (show ((sum $ fitnessFn <$> teams) / (fromIntegral $ length teams)))
    newTeams <- replicateM (length teams) (selectFrom maxFitness fitnessFn teams)
    mutateTeams newTeams

selectFrom :: Float -> (Team -> Float) -> [Team] -> IO Team
selectFrom maxFitness fitnessFn teams = do
    index <- getStdRandom $ randomR (0, length teams - 1)
    prob <- getStdRandom $ randomR (0, 1)
    let team = teams !! index
    if fitnessFn team / maxFitness >= prob then
        return team
    else
        selectFrom maxFitness fitnessFn teams

mutateTeams :: [Team] -> IO [Team]
mutateTeams (first:second:rest) = do
    mutated <- mutatePair first second
    mutatedRest <- mutateTeams rest
    return $ mutated ++ mutatedRest
mutateTeams teams = return teams

crossoverProb :: Float
crossoverProb = 0.05

mutatePair :: Team -> Team -> IO [Team]
mutatePair first second = do
    (first', second') <- return (first, second)
        >>= mutate qb
        >>= mutate rb1
        >>= mutate rb2
        >>= mutate wr1
        >>= mutate wr2
        >>= mutate wr3
        >>= mutate te
        >>= mutate flex
        >>= mutate dst
    return $ [first', second']

mutate :: Lens' Team PlayerWithProjected -> (Team, Team) -> IO (Team, Team)
mutate position (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (set position (second^.position) first, set position (first^.position) second)
       else return (first, second)
  where
      okToSwap = not (second^.position `elem` allPlayers first) &&
                 not (first^.position `elem` allPlayers second)