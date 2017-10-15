{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types where

import qualified Data.ByteString.Char8 as BS
import           Data.Csv (DefaultOrdered(..), FromField(..), FromNamedRecord(..), ToField(..), ToNamedRecord(..), (.:), (.=), header, namedRecord)
import           Data.Semigroup ((<>))
import           Data.String (IsString)
import qualified Data.Text as T
import           Test.QuickCheck.Arbitrary (Arbitrary(..))
import           Test.QuickCheck.Gen (oneof)
import           Test.QuickCheck.Instances ()

data Position =
      QB
    | RB
    | WR
    | TE
    | DST
    deriving (Eq, Ord, Show)

instance Arbitrary Position where
    arbitrary = oneof
        [ return QB
        , return RB
        , return WR
        , return TE
        , return DST
        ]

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

newtype PlayerName =
    PlayerName { getPlayerName :: T.Text }
    deriving (Eq, FromField, IsString, Ord, Show, ToField)

instance Arbitrary PlayerName where
    arbitrary = PlayerName <$> arbitrary

newtype TeamName =
    TeamName { getTeamName :: T.Text }
    deriving (Eq, FromField, IsString, Ord, Show, ToField)

instance Arbitrary TeamName where
    arbitrary = TeamName <$> arbitrary

data PickEmPlayer = PickEmPlayer
    { name :: PlayerName
    , position :: Position
    , rosterPosition :: T.Text
    , gameInfo :: T.Text
    , avgPointsPerGame :: Float
    , team :: TeamName }
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
    { pName :: PlayerName
    , pPosition :: Position
    , pRosterPosition :: T.Text
    , pGameInfo :: T.Text
    , pAvgPointsPerGame :: Float
    , pProjectedPoints :: Float
    , pTeam :: TeamName }
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
    { cName :: PlayerName
    , cPosition :: Position
    , cSalary :: Int
    , cGameInfo :: T.Text
    , cAvgPointsPerGame :: Float
    , cTeam :: TeamName }
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
    { cpName :: PlayerName
    , cpPosition :: Position
    , cpSalary :: Int
    , cpGameInfo :: T.Text
    , cpAvgPointsPerGame :: Float
    , cpProjectedPoints :: Float
    , cpTeam :: TeamName }
    deriving (Eq, Show)

instance Arbitrary ClassicPlayerWithProjected where
    arbitrary = ClassicPlayerWithProjected
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

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
shouldUsePPR' player = pos /= QB && pos /= DST
  where
      pos = cPosition player
