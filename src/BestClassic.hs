{-# LANGUAGE OverloadedStrings #-}

module BestClassic where

import           Control.Monad (replicateM)
import           Data.List (maximumBy)
import           Data.Vector (Vector)

import           Fitness
import           GenerateTeam
import           Teams
import           Types

pickBestLineup :: Int -> Int -> Vector ClassicPlayerWithProjected -> IO ClassicTeam
pickBestLineup salaryCap poolSize players = do
    teams <- replicateM poolSize (generateTeam pool)
    let best = maximumBy (\t1 t2 -> compare (rate t1) (rate t2)) teams
    return best
  where
    pool = generatePlayerPool players
    rate = fitness salaryCap
