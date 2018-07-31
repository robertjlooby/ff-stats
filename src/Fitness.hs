{-# LANGUAGE DeriveGeneric #-}

module Fitness where

import Dhall (Generic, Interpret)
import Teams
import Types

data Strategy
  = Normal
  | Stacked
  deriving (Eq, Generic)

instance Interpret Strategy

salary :: Team -> Integer
salary = toInteger . sum . (fmap . fmap) (_salary . _player) allPlayers

fitness :: Strategy -> Integer -> Team -> Float
fitness strategy salaryCap team
  | salary team > salaryCap = 0
  | strategy == Stacked = base + bonus
  | otherwise = base
  where
    base = sum $ _projectedPoints <$> allPlayers team
    qbTeam = _team . _player . _qb $ team
    passCatchersOnQBsTeam =
      length
        [ p
        | p <- _player <$> allPlayers team
        , _position p `elem` [TE, WR]
        , _team p == qbTeam
        ]
    bonus
      | passCatchersOnQBsTeam >= 2 = 4
      | passCatchersOnQBsTeam == 1 = 2
      | otherwise = 0
