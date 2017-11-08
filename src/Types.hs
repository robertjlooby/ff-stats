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

data Player = Player
    { _name :: PlayerName
    , _position :: Position
    , _salary :: Int
    , _gameInfo :: T.Text
    , _avgPointsPerGame :: Float
    , _team :: TeamName }
    deriving (Eq, Show)

instance FromNamedRecord Player where
    parseNamedRecord m =
        Player
          <$> m .: "Name"
          <*> m .: "Position"
          <*> m .: "Salary"
          <*> m .: "GameInfo"
          <*> m .: "AvgPointsPerGame"
          <*> m .: "teamAbbrev"

data PlayerWithProjected = PlayerWithProjected
    { cpName :: PlayerName
    , cpPosition :: Position
    , cpSalary :: Int
    , cpGameInfo :: T.Text
    , cpAvgPointsPerGame :: Float
    , cpProjectedPoints :: Float
    , cpTeam :: TeamName }
    deriving (Eq, Ord, Show)

instance Arbitrary PlayerWithProjected where
    arbitrary = PlayerWithProjected
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance FromNamedRecord PlayerWithProjected where
    parseNamedRecord m =
        PlayerWithProjected
          <$> m .: "Name"
          <*> m .: "Position"
          <*> m .: "Salary"
          <*> m .: "GameInfo"
          <*> m .: "AvgPointsPerGame"
          <*> m .: "Projected"
          <*> m .: "teamAbbrev"

instance ToNamedRecord PlayerWithProjected where
    toNamedRecord player =
        namedRecord [ "Name" .= cpName player
                    , "Position" .= cpPosition player
                    , "Salary" .= cpSalary player
                    , "GameInfo" .= cpGameInfo player
                    , "AvgPointsPerGame" .= cpAvgPointsPerGame player
                    , "teamAbbrev" .= cpTeam player
                    , "Projected" .= cpProjectedPoints player
                    ]

instance DefaultOrdered PlayerWithProjected where
    headerOrder _ = header [ "Name"
                           , "Position"
                           , "Salary"
                           , "GameInfo"
                           , "AvgPointsPerGame"
                           , "teamAbbrev"
                           , "Projected"
                           ]

shouldUsePPR :: Player -> Bool
shouldUsePPR player = pos /= QB && pos /= DST
  where
      pos = _position player
