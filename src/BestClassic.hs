{-# LANGUAGE OverloadedStrings #-}

module BestClassic where

import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

import FetchWeekProjection (getProjectedScore, paramifyName)
import SpecialCases (nameOverrides)
import Types


withProjected :: ClassicPlayer -> Float -> ClassicPlayerWithProjected
withProjected player projected =
    ClassicPlayerWithProjected
        (cName player)
        (cPosition player)
        (cSalary player)
        (cGameInfo player)
        (cAvgPointsPerGame player)
        projected
        (cTeam player)

getPlayersWithProjected :: T.Text -> Vector ClassicPlayer -> IO (Either String (Vector ClassicPlayerWithProjected))
getPlayersWithProjected week' players = do
    playersWithProjected <- traverse (playerWithProjected week') players
    return . sequence $ V.filter includePlayer playersWithProjected
  where
      includePlayer (Left _) = False
      includePlayer (Right player) = cpProjectedPoints player >= 1


playerWithProjected :: T.Text -> ClassicPlayer -> IO (Either String ClassicPlayerWithProjected)
playerWithProjected week' player = do
    projected <- getProjectedScore (getNameWithOverride player) week' (shouldUsePPR' player)
    case projected of
      Right score -> return . Right $ withProjected player score
      Left err -> do
          let msg = T.unpack (cTeam player)
                      <> " "
                      <> show (cPosition player)
                      <> " "
                      <> T.unpack (cName player)
                      <> " -- "
                      <> err
          _ <- print msg
          return $ Left msg

getNameWithOverride :: ClassicPlayer -> T.Text
getNameWithOverride player =
    let defaultName = paramifyName $ cName player
        playerKey = (cName player, cPosition player, cTeam player)
    in
        Map.findWithDefault defaultName playerKey nameOverrides
