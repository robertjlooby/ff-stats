{-# LANGUAGE OverloadedStrings #-}

module BestClassic where

import           Data.Vector (Vector)

import           GenerateTeam
import           Teams
import           Types

pickBestLineup :: Vector ClassicPlayerWithProjected -> IO ClassicTeam
pickBestLineup players = do
    team <- generateTeam pool
    return team
  where
    pool = generatePlayerPool players
