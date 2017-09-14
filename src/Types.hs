{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Csv (FromField(..), FromNamedRecord(..), (.:))
import           Data.Semigroup ((<>))
import qualified Data.Text as T

data Position =
      QB
    | RB
    | WR
    | TE
    | DST
    deriving (Eq, Ord, Show)

instance FromField Position where
    parseField s
        | s == "QB" = pure QB
        | s == "RB" = pure RB
        | s == "WR" = pure WR
        | s == "TE" = pure TE
        | s == "DST" = pure DST
        | otherwise = fail . show $ "Cannot parse Position from: " <> s

data Player = Player
    { name :: T.Text
    , position :: Position
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
    , pPosition :: Position
    , pRosterPosition :: T.Text
    , pGameInfo :: T.Text
    , pAvgPointsPerGame :: Float
    , pProjectedPoints :: Float
    , pTeam :: T.Text }
    deriving (Eq, Show)
