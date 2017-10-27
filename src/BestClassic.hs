{-# LANGUAGE OverloadedStrings #-}

module BestClassic where

import           Control.Monad (replicateM)
import           Data.List (sortBy)
import           Data.Vector (Vector)

import           Fitness
import           GenerateTeam
import           Teams
import           Types

pickBestLineups :: Int -> Int -> Int -> Vector ClassicPlayerWithProjected -> IO [ClassicTeam]
pickBestLineups salaryCap resultCount poolSize players = do
    teams <- replicateM poolSize (generateTeam pool)
    let best = take resultCount $ sortBy (\t1 t2 -> compare (rate t2) (rate t1)) teams
    return best
  where
    pool = generatePlayerPool players
    rate = fitness salaryCap
