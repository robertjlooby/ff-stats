{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Applicative ( (<|>) )
import qualified Data.ByteString.Char8 as BS
import           Data.Csv
       ( (.:), (.=), DefaultOrdered(..), FromField(..), FromNamedRecord(..)
       , NamedRecord, Parser, ToField(..), ToNamedRecord(..), header
       , namedRecord )
import           Data.Semigroup ( (<>) )
import           Data.String ( IsString )
import qualified Data.Text as T

import           Dhall ( Generic, Interpret )
import           Test.QuickCheck.Arbitrary ( Arbitrary(..) )
import           Test.QuickCheck.Gen ( oneof )
import           Test.QuickCheck.Instances ()

data Position
    = QB
    | RB
    | WR
    | TE
    | DST
    deriving ( Eq, Generic, Ord, Show )

instance Interpret Position

instance Arbitrary Position where
    arbitrary =
        oneof [ return QB, return RB, return WR, return TE, return DST ]

instance FromField Position where
    parseField s
      | s == "QB" = pure QB
      | s == "RB" = pure RB
      | s == "WR" = pure WR
      | s == "TE" = pure TE
      | s == "DST" = pure DST
      | s == "DEF" = pure DST
      | otherwise = fail . show $ "Cannot parse Position from: " <> s

instance ToField Position where
    toField = BS.pack . show

newtype PlayerName =
    PlayerName
        { getPlayerName :: T.Text
        }
    deriving ( Eq, FromField, Generic, IsString, Ord, Semigroup, Show, ToField )

instance Interpret PlayerName

instance Arbitrary PlayerName where
    arbitrary = PlayerName <$> arbitrary

newtype PlayerNameAndId =
    PlayerNameAndId
        { getPlayerNameAndId :: T.Text
        }
    deriving ( Eq, FromField, IsString, Ord, Semigroup, Show, ToField )

instance Arbitrary PlayerNameAndId where
    arbitrary = PlayerNameAndId <$> arbitrary

newtype TeamName =
    TeamName
        { getTeamName :: T.Text
        }
    deriving ( Eq, FromField, Generic, IsString, Ord, Show, ToField )

instance Interpret TeamName

instance Arbitrary TeamName where
    arbitrary = TeamName <$> arbitrary

data Player =
    Player
        { _name :: PlayerName
        , _nameAndId :: PlayerNameAndId
        , _position :: Position
        , _salary :: Int
        , _gameInfo :: T.Text
        , _team :: TeamName
        }
    deriving ( Eq, Ord, Show )

instance Arbitrary Player where
    arbitrary =
        Player <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

parseYahooName :: NamedRecord -> Parser PlayerName
parseYahooName m = do
    fn <- m .: "First Name"
    ln <- m .: "Last Name"
    return $ fn <> " " <> ln

parseYahooNameAndId :: NamedRecord -> Parser PlayerNameAndId
parseYahooNameAndId m = do
    fn <- m .: "First Name"
    ln <- m .: "Last Name"
    playerId <- m .: "Id"
    return $ fn <> " " <> ln <> " (" <> playerId <> ")"

parseYahooGameInfo :: NamedRecord -> Parser T.Text
parseYahooGameInfo m = do
    game <- m .: "Game"
    time <- m .: "Time"
    return $ game <> " " <> time

instance FromNamedRecord Player where
    parseNamedRecord m =
        Player <$> (m .: "Name" <|> parseYahooName m)
        <*> (m .: "Name + ID" <|> parseYahooNameAndId m)
        <*> m .: "Position"
        <*> m .: "Salary"
        <*> (m .: "Game Info" <|> parseYahooGameInfo m)
        <*> (m .: "TeamAbbrev" <|> m .: "Team")

data PlayerWithProjected =
    PlayerWithProjected
        { _player :: Player
        , _projectedPoints :: Float
        }
    deriving ( Eq, Ord, Show )

instance Arbitrary PlayerWithProjected where
    arbitrary = PlayerWithProjected <$> arbitrary <*> arbitrary

instance FromNamedRecord PlayerWithProjected where
    parseNamedRecord
        m = PlayerWithProjected <$> parseNamedRecord m <*> m .: "Projected"

instance ToNamedRecord PlayerWithProjected where
    toNamedRecord playerWithProjected =
        namedRecord
            [ "Name" .= _name player
            , "Name + ID" .= _nameAndId player
            , "Position" .= _position player
            , "Salary" .= _salary player
            , "Game Info" .= _gameInfo player
            , "TeamAbbrev" .= _team player
            , "Projected" .= _projectedPoints playerWithProjected
            ]
      where
        player = _player playerWithProjected

instance DefaultOrdered PlayerWithProjected where
    headerOrder _ =
        header
            [ "Name"
            , "Name + ID"
            , "Position"
            , "Salary"
            , "Game Info"
            , "TeamAbbrev"
            , "Projected"
            ]

shouldUsePPR :: Player -> Bool
shouldUsePPR player = pos /= QB && pos /= DST
  where
    pos = _position player
