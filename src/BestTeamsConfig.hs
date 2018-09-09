{-# LANGUAGE DeriveGeneric #-}

module BestTeamsConfig
  ( module BestTeamsConfig
  , ask
  , asks
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)
import Dhall (Generic, Interpret)
import System.Random (Random, StdGen, randomR)

type App = ReaderT Config (StateT StdGen IO)

runApp :: App a -> Config -> StdGen -> IO a
runApp app config = evalStateT (runReaderT app config)

getRandom :: Random a => (a, a) -> App a
getRandom range =
  lift $ do
    (val, seed') <- randomR range <$> get
    put seed'
    return val

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
