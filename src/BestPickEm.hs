{-# LANGUAGE OverloadedStrings #-}

module BestPickEm where

import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Vector (Vector, maximumBy, uniq)
import           Data.Vector.Algorithms.Merge (sort)

import FetchWeekProjection (getProjectedScore, paramifyName)
import Types


withProjected :: Player -> Float -> PlayerWithProjected
withProjected player projected =
    PlayerWithProjected
        (name player)
        (position player)
        (rosterPosition player)
        (gameInfo player)
        (avgPointsPerGame player)
        projected
        (team player)

pickBestLineupByAvgPoints :: Vector Player -> Vector Player
pickBestLineupByAvgPoints players =
    rosterPositions players
      & fmap maxAtRosterPosition
  where
    maxAtRosterPosition rp =
        players
          & V.filter (\player -> rp == rosterPosition player)
          & maximumBy (\p1 p2 -> compare (avgPointsPerGame p1) (avgPointsPerGame p2))

pickBestLineupByProjectedPoints :: T.Text -> Vector Player -> IO (Either String (Vector PlayerWithProjected))
pickBestLineupByProjectedPoints week' players = do
    players' <- playersWithProjected week' players
    case players' of
      Left err -> return $ Left err
      Right playersWithProjected' ->
        rosterPositions players
          & fmap (maxAtRosterPosition playersWithProjected')
          & Right
          & return
  where
    maxAtRosterPosition players' rp =
        players'
          & V.filter (\player -> rp == pRosterPosition player)
          & maximumBy (\p1 p2 -> compare (pProjectedPoints p1) (pProjectedPoints p2))

playersWithProjected :: T.Text -> Vector Player -> IO (Either String (Vector PlayerWithProjected))
playersWithProjected week' players =
    traverse (\pl -> (fmap . fmap) (withProjected pl) (getProjectedScore (getNameWithOverride pl) week' (shouldUsePPR pl))) players
      & fmap sequence

shouldUsePPR :: Player -> Bool
shouldUsePPR player = position player /= "QB"

rosterPositions :: Vector Player -> Vector T.Text
rosterPositions players =
    players
      & fmap rosterPosition
      & V.modify sort
      & uniq

getNameWithOverride :: Player -> T.Text
getNameWithOverride player =
    let defaultName = paramifyName $ name player
        playerKey = (name player, position player, team player)
    in
        Map.findWithDefault defaultName playerKey nameOverrides

nameOverrides :: Map (T.Text, T.Text, T.Text) T.Text
nameOverrides =
    Map.empty
      & Map.insert ("Alex Smith", "QB", "KC") "alex-smith-sf"
      & Map.insert ("David Johnson", "RB", "ARI") "david-johnson-rb"
      & Map.insert ("Michael Thomas", "WR", "NO") "michael-thomas-wr"
