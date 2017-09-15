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

data ClassicPlayer = ClassicPlayer
    { cName :: T.Text
    , cPosition :: Position
    , cSalary :: Int
    , cGameInfo :: T.Text
    , cAvgPointsPerGame :: Float
    , cTeam :: T.Text }
    deriving (Eq, Show)

instance FromNamedRecord ClassicPlayer where
    parseNamedRecord m =
        ClassicPlayer
          <$> m .: "Name"
          <*> m .: "Position"
          <*> m .: "Salary"
          <*> m .: "GameInfo"
          <*> m .: "AvgPointsPerGame"
          <*> m .: "teamAbbrev"

data ClassicPlayerWithProjected = ClassicPlayerWithProjected
    { cpName :: T.Text
    , cpPosition :: Position
    , cpSalary :: Int
    , cpGameInfo :: T.Text
    , cpAvgPointsPerGame :: Float
    , cpProjectedPoints :: Float
    , cpTeam :: T.Text }
    deriving (Eq, Show)

instance ToNamedRecord ClassicPlayerWithProjected where
    toNamedRecord player =
        namedRecord [ "Name" .= cpName player
                    , "Position" .= cpPosition player
                    , "Salary" .= cpSalary player
                    , "GameInfo" .= cpGameInfo player
                    , "AvgPointsPerGame" .= cpAvgPointsPerGame player
                    , "teamAbbrev" .= cpTeam player
                    , "Projected" .= cpProjectedPoints player
                    ]

instance DefaultOrdered ClassicPlayerWithProjected where
    headerOrder _ = header [ "Name"
                           , "Position"
                           , "Salary"
                           , "GameInfo"
                           , "AvgPointsPerGame"
                           , "teamAbbrev"
                           , "Projected"
                           ]

shouldUsePPR :: PickEmPlayer -> Bool
shouldUsePPR player = position player /= QB

shouldUsePPR' :: ClassicPlayer -> Bool
shouldUsePPR' player = cPosition player /= QB
