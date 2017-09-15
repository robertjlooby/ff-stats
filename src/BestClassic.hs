{-# LANGUAGE OverloadedStrings #-}

module BestClassic where

import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import           Data.Vector (Vector)

import FetchWeekProjection (getProjectedScore, paramifyName)
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

playersWithProjected :: T.Text -> Vector ClassicPlayer -> IO (Either String (Vector ClassicPlayerWithProjected))
playersWithProjected week' players =
    traverse (\pl -> (fmap . fmap) (withProjected pl) (getProjectedScore (getNameWithOverride pl) week' (shouldUsePPR' pl))) players
      & fmap sequence

getNameWithOverride :: ClassicPlayer -> T.Text
getNameWithOverride player =
    let defaultName = paramifyName $ cName player
        playerKey = (cName player, cPosition player, cTeam player)
    in
        Map.findWithDefault defaultName playerKey nameOverrides

nameOverrides :: Map (T.Text, Position, T.Text) T.Text
nameOverrides =
    Map.empty
      & Map.insert ("Alex Smith", QB, "KC") "alex-smith-sf"
      & Map.insert ("David Johnson", RB, "ARI") "david-johnson-rb"
      & Map.insert ("Michael Thomas", WR, "NO") "michael-thomas-wr"
