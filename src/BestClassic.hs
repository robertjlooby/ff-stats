{-# LANGUAGE OverloadedStrings #-}

module BestClassic where

import           Control.Monad (replicateM)
import           Data.List (maximumBy)
import           Data.Vector (Vector)

import           Fitness
import           GenerateTeam
import           Teams
import           Types

pickBestLineup :: Int -> Vector ClassicPlayerWithProjected -> IO ClassicTeam
pickBestLineup poolSize players = do
    teams <- replicateM poolSize (generateTeam pool)
    let best = maximumBy (\t1 t2 -> compare (fitness t1) (fitness t2)) teams
    return best
  where
    pool = generatePlayerPool players
