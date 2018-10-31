{-# LANGUAGE TemplateHaskell #-}

module Teams where

import Control.Lens.TH (makeLenses)
import Data.ByteString.Lazy (ByteString)
import Data.Csv (ToRecord(..), encode, record, toField)
import Data.List (nub)
import Test.QuickCheck.Arbitrary (Arbitrary(..), vector)
import Test.QuickCheck.Gen (suchThat)

import Types

data Team = Team
  { _qb1 :: PlayerWithProjected
  , _qb2 :: PlayerWithProjected
  , _qb3 :: PlayerWithProjected
  , _qb4 :: PlayerWithProjected
  , _qb5 :: PlayerWithProjected
  , _qb6 :: PlayerWithProjected
  } deriving (Eq, Show)

makeLenses ''Team

instance ToRecord Team where
  toRecord team =
    record [get _qb1, get _qb2, get _qb3, get _qb4, get _qb5, get _qb6]
    where
      get position = toField . _nameAndId . _player . position $ team

teamHeaders :: ByteString
teamHeaders =
  encode
    [["QB" :: ByteString, "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DST"]]

allPlayers :: Team -> [PlayerWithProjected]
allPlayers team = [_qb1, _qb2, _qb3, _qb4, _qb5, _qb6] <*> pure team

data PlayerPool = PlayerPool
  { _qbs :: [PlayerWithProjected]
  } deriving (Eq, Show)

instance Arbitrary PlayerPool where
  arbitrary = do
    qbs <- mkPlayers QB
    return $ PlayerPool qbs
    where
      setPosition pos playerWithProjected =
        let player = _player playerWithProjected
         in playerWithProjected {_player = player {_position = pos}}
      mkPlayers pos = do
        players <- vector 5 `suchThat` (\l -> nub l == l)
        return $ setPosition pos <$> players
