{-# LANGUAGE OverloadedStrings #-}

module FetchProjections where

import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

import FetchWeekProjection
import SpecialCases (nameOverrides)
import Types


withProjected :: Player -> Float -> PlayerWithProjected
withProjected player projected =
    PlayerWithProjected
        (_name player)
        (_position player)
        (_salary player)
        (_gameInfo player)
        (_avgPointsPerGame player)
        projected
        (_team player)

getPlayersWithProjected :: T.Text -> Vector Player -> IO (Either String (Vector PlayerWithProjected))
getPlayersWithProjected week' players = do
    playersWithProjected <- traverse (playerWithProjected week') players
    return . sequence $ V.filter includePlayer playersWithProjected
  where
      includePlayer (Left _) = False
      includePlayer (Right player) = cpProjectedPoints player >= 1


playerWithProjected :: T.Text -> Player -> IO (Either String PlayerWithProjected)
playerWithProjected week' player = do
    projected <- getProjectedScore (getNameWithOverride player) week' (shouldUsePPR player)
    case projected of
      Right score -> return . Right $ withProjected player score
      Left err -> do
          let msg = (T.unpack . getTeamName . _team $ player)
                      <> " "
                      <> show (_position player)
                      <> " "
                      <> (T.unpack . getPlayerName . _name $ player)
                      <> " -- "
                      <> err
          _ <- print msg
          return $ Left msg

getNameWithOverride :: Player -> T.Text
getNameWithOverride player =
    let defaultName = paramifyName $ _name player
        playerKey = (_name player, _position player, _team player)
    in
        Map.findWithDefault defaultName playerKey nameOverrides
