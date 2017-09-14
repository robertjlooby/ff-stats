{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.ByteString.Char8 as BS
import           Data.Csv (DefaultOrdered(..), FromField(..), FromNamedRecord(..), ToField(..), ToNamedRecord(..), (.:), (.=), header, namedRecord)
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

instance ToField Position where
    toField = BS.pack . show

data PickEmPlayer = PickEmPlayer
    { name :: T.Text
    , position :: Position
    , rosterPosition :: T.Text
    , gameInfo :: T.Text
    , avgPointsPerGame :: Float
    , team :: T.Text }
    deriving (Eq, Show)

instance FromNamedRecord PickEmPlayer where
    parseNamedRecord m =
        PickEmPlayer
          <$> m .: "Name"
          <*> m .: "Position"
          <*> m .: "Roster_Position"
          <*> m .: "GameInfo"
          <*> m .: "AvgPointsPerGame"
          <*> m .: "teamAbbrev"

data PickEmPlayerWithProjected = PickEmPlayerWithProjected
    { pName :: T.Text
    , pPosition :: Position
    , pRosterPosition :: T.Text
    , pGameInfo :: T.Text
    , pAvgPointsPerGame :: Float
    , pProjectedPoints :: Float
    , pTeam :: T.Text }
    deriving (Eq, Show)

instance ToNamedRecord PickEmPlayerWithProjected where
    toNamedRecord player =
        namedRecord [ "Name" .= pName player
                    , "Position" .= pPosition player
                    , "Roster_Position" .= pRosterPosition player
                    , "GameInfo" .= pGameInfo player
                    , "AvgPointsPerGame" .= pAvgPointsPerGame player
                    , "teamAbbrev" .= pTeam player
                    , "Projected" .= pProjectedPoints player
                    ]

instance DefaultOrdered PickEmPlayerWithProjected where
    headerOrder _ = header [ "Name"
                           , "Position"
                           , "Roster_Position"
                           , "GameInfo"
                           , "AvgPointsPerGame"
                           , "teamAbbrev"
                           , "Projected"
                           ]

shouldUsePPR :: PickEmPlayer -> Bool
shouldUsePPR player = position player /= QB
