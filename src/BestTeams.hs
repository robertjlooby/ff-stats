{-# LANGUAGE OverloadedStrings #-}

module BestTeams where

import           Control.Monad (replicateM)
import           Data.List (sortBy)
import           Data.Set (difference, fromList, size)
import           Data.Vector (Vector)
import           System.Random (getStdRandom, randomR)

import           Fitness
import           GenerateTeam
import           Teams
import           Types

pickBestLineups :: Int -> Int -> Int -> Vector PlayerWithProjected -> IO [Team]
pickBestLineups salaryCap resultCount poolSize players = do
    teams <- replicateM poolSize (generateTeam pool)
    newTeams <- iterateIO 150 (nextGeneration rate) (return teams)
    return $ getTop rate resultCount newTeams
  where
    pool = generatePlayerPool players
    rate = fitness salaryCap

iterateIO :: Int -> (a -> IO a) -> IO a -> IO a
iterateIO times iterator state
    | times <= 0 = state
    | otherwise  = do
        currentState <- state
        iterateIO (times - 1) iterator (iterator currentState)

minTeamDifference :: Int
minTeamDifference = 4

getTop :: (Team -> Float) -> Int -> [Team] -> [Team]
getTop fitnessFn resultCount allTeams =
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
            differences = (size . (difference teamPlayers) . fromList . allPlayers) <$> results
        in
            all (\diff -> diff >= minTeamDifference) differences

nextGeneration :: (Team -> Float) -> [Team] -> IO [Team]
nextGeneration fitnessFn teams = do
    let maxFitness = maximum $ fitnessFn <$> teams
    putStrLn $
        "max fitness: " ++ show maxFitness ++
        " avg fitness: " ++ (show ((sum $ fitnessFn <$> teams) / (fromIntegral $ length teams)))
    newTeams <- replicateM (length teams) (selectFrom maxFitness fitnessFn teams)
    mutate newTeams

selectFrom :: Float -> (Team -> Float) -> [Team] -> IO Team
selectFrom maxFitness fitnessFn teams = do
    index <- getStdRandom $ randomR (0, length teams - 1)
    prob <- getStdRandom $ randomR (0, 1)
    let team = teams !! index
    if fitnessFn team / maxFitness >= prob then
        return team
    else
        selectFrom maxFitness fitnessFn teams

mutate :: [Team] -> IO [Team]
mutate (first:second:rest) = do
    mutated <- mutatePair first second
    mutatedRest <- mutate rest
    return $ mutated ++ mutatedRest
mutate teams = return teams

crossoverProb :: Float
crossoverProb = 0.05

mutatePair :: Team -> Team -> IO [Team]
mutatePair first second = do
    (first', second') <- return (first, second)
        >>= mutateQbs
        >>= mutateRb1s
        >>= mutateRb2s
        >>= mutateWr1s
        >>= mutateWr2s
        >>= mutateWr3s
        >>= mutateTes
        >>= mutateFlexes
        >>= mutateDsts
    return $ [first', second']

mutateQbs :: (Team, Team) -> IO (Team, Team)
mutateQbs (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb
       then return (first{_qb = _qb second}, second{_qb = _qb first})
       else return (first, second)

mutateRb1s :: (Team, Team) -> IO (Team, Team)
mutateRb1s (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (first{_rb1 = _rb1 second}, second{_rb1 = _rb1 first})
       else return (first, second)
  where
      okToSwap = not (_rb1 second `elem` allPlayers first) &&
                 not (_rb1 first `elem` allPlayers second)

mutateRb2s :: (Team, Team) -> IO (Team, Team)
mutateRb2s (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (first{_rb2 = _rb2 second}, second{_rb2 = _rb2 first})
       else return (first, second)
  where
      okToSwap = not (_rb2 second `elem` allPlayers first) &&
                 not (_rb2 first `elem` allPlayers second)

mutateWr1s :: (Team, Team) -> IO (Team, Team)
mutateWr1s (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (first{_wr1 = _wr1 second}, second{_wr1 = _wr1 first})
       else return (first, second)
  where
      okToSwap = not (_wr1 second `elem` allPlayers first) &&
                 not (_wr1 first `elem` allPlayers second)

mutateWr2s :: (Team, Team) -> IO (Team, Team)
mutateWr2s (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (first{_wr2 = _wr2 second}, second{_wr2 = _wr2 first})
       else return (first, second)
  where
      okToSwap = not (_wr2 second `elem` allPlayers first) &&
                 not (_wr2 first `elem` allPlayers second)

mutateWr3s :: (Team, Team) -> IO (Team, Team)
mutateWr3s (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (first{_wr3 = _wr3 second}, second{_wr3 = _wr3 first})
       else return (first, second)
  where
      okToSwap = not (_wr3 second `elem` allPlayers first) &&
                 not (_wr3 first `elem` allPlayers second)

mutateTes :: (Team, Team) -> IO (Team, Team)
mutateTes (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (first{_te = _te second}, second{_te = _te first})
       else return (first, second)
  where
      okToSwap = not (_te second `elem` allPlayers first) &&
                 not (_te first `elem` allPlayers second)

mutateFlexes :: (Team, Team) -> IO (Team, Team)
mutateFlexes (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb && okToSwap
       then return (first{_flex = _flex second}, second{_flex = _flex first})
       else return (first, second)
  where
      okToSwap = not (_flex second `elem` allPlayers first) &&
                 not (_flex first `elem` allPlayers second)

mutateDsts :: (Team, Team) -> IO (Team, Team)
mutateDsts (first, second) = do
    prob <- getStdRandom $ randomR (0, 1)
    if prob < crossoverProb
       then return (first{_dst = _dst second}, second{_dst = _dst first})
       else return (first, second)
