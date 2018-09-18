{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BestTeamsConfig
  ( module BestTeamsConfig
  , ask
  , asks
  , lift
  ) where

import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN, runStdoutLoggingT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)
import Data.Text (pack)
import Dhall (Generic, Interpret)
import System.Random (Random, StdGen, randomR)

newtype App m a = App
  { _runApp :: ReaderT Config (StateT StdGen m) a
  } deriving (Applicative, Functor, Monad)

instance MonadTrans App where
  lift = App . lift . lift

runApp :: App (LoggingT IO) a -> Config -> StdGen -> IO a
runApp app config seed =
  runStdoutLoggingT (evalStateT (runReaderT (_runApp app) config) seed)

getRandom :: (Random a, Monad m) => (a, a) -> App m a
getRandom range =
  App . lift $ do
    (val, seed') <- randomR range <$> get
    put seed'
    return val

logInfo :: MonadLogger m => String -> App m ()
logInfo = lift . logInfoN . pack

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
