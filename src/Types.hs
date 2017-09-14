{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Csv (FromNamedRecord(..), (.:))
import qualified Data.Text as T

data Player = Player
    { name :: T.Text
    , position :: T.Text
    , rosterPosition :: T.Text
    , gameInfo :: T.Text
    , avgPointsPerGame :: Float
    , team :: T.Text }
    deriving (Eq, Show)

instance FromNamedRecord Player where
    parseNamedRecord m =
        Player
          <$> m .: "Name"
          <*> m .: "Position"
          <*> m .: "Roster_Position"
          <*> m .: "GameInfo"
          <*> m .: "AvgPointsPerGame"
          <*> m .: "teamAbbrev"

data PlayerWithProjected = PlayerWithProjected
    { pName :: T.Text
    , pPosition :: T.Text
    , pRosterPosition :: T.Text
    , pGameInfo :: T.Text
    , pAvgPointsPerGame :: Float
    , pProjectedPoints :: Float
    , pTeam :: T.Text }
    deriving (Eq, Show)
