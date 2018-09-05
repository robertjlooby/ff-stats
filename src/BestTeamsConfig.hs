{-# LANGUAGE DeriveGeneric #-}

module BestTeamsConfig
  ( module BestTeamsConfig
  , ask
  , asks
  ) where

import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Dhall (Generic, Interpret)

type App = ReaderT Config IO

runApp :: App a -> Config -> IO a
runApp = runReaderT

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
