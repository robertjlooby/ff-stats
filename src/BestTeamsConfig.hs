{-# LANGUAGE DeriveGeneric #-}

module BestTeamsConfig where

import Dhall (Generic, Interpret)

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

data Strategy
  = Normal
  | Stacked
  deriving (Eq, Generic)

instance Interpret Strategy
