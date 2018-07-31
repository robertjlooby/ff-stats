{-# LANGUAGE DeriveGeneric #-}

module FetchProjections where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Dhall (Generic, Interpret)

import FetchWeekProjection
import Types

data PlayerKey = PlayerKey
  { name :: PlayerName
  , position :: Position
  , team :: TeamName
  } deriving (Eq, Generic, Ord)

instance Interpret PlayerKey

data Config = Config
  { _nameOverrides :: Map PlayerKey T.Text
  }

getPlayersWithProjected ::
     Config
  -> T.Text
  -> Vector Player
  -> IO (Either String (Vector PlayerWithProjected))
getPlayersWithProjected config week' players = do
  playersWithProjected <- traverse (playerWithProjected config week') players
  return . sequence $ V.filter includePlayer playersWithProjected
  where
    includePlayer (Left _) = False
    includePlayer (Right player) = _projectedPoints player >= 1

playerWithProjected ::
     Config -> T.Text -> Player -> IO (Either String PlayerWithProjected)
playerWithProjected config week' player = do
  projected <-
    getProjectedScore
      (getNameWithOverride config player)
      week'
      (shouldUsePPR player)
  case projected of
    Right score -> return . Right $ PlayerWithProjected player score
    Left err -> do
      let msg =
            (T.unpack . getTeamName . _team $ player) <> " " <>
            show (_position player) <>
            " " <>
            (T.unpack . getPlayerName . _name $ player) <>
            " -- " <>
            err
      _ <- print msg
      return $ Left msg

getNameWithOverride :: Config -> Player -> T.Text
getNameWithOverride config player =
  let defaultName = paramifyName $ _name player
      playerKey = PlayerKey (_name player) (_position player) (_team player)
   in Map.findWithDefault defaultName playerKey (_nameOverrides config)
