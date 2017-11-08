module Teams where

import           Data.List (nub)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), vector)
import           Test.QuickCheck.Gen (suchThat)

import           Types

data Team = Team
    { _qb :: PlayerWithProjected
    , _rb1 :: PlayerWithProjected
    , _rb2 :: PlayerWithProjected
    , _wr1 :: PlayerWithProjected
    , _wr2 :: PlayerWithProjected
    , _wr3 :: PlayerWithProjected
    , _te :: PlayerWithProjected
    , _flex :: PlayerWithProjected
    , _dst :: PlayerWithProjected
    } deriving (Eq, Show)

allPlayers :: Team -> [PlayerWithProjected]
allPlayers team = [_qb, _rb1, _rb2, _wr1, _wr2, _wr3, _te, _flex, _dst] <*> pure team

data PlayerPool = PlayerPool
    { _qbs :: [PlayerWithProjected]
    , _rbs :: [PlayerWithProjected]
    , _wrs :: [PlayerWithProjected]
    , _tes :: [PlayerWithProjected]
    , _dsts :: [PlayerWithProjected]
    } deriving (Eq, Show)

instance Arbitrary PlayerPool where
    arbitrary = do
        qbs <- mkPlayers QB
        rbs <- mkPlayers RB
        wrs <- mkPlayers WR
        tes <- mkPlayers TE
        dsts <- mkPlayers DST
        return $ PlayerPool
            qbs
            rbs
            wrs
            tes
            dsts
      where
        setPosition pos playerWithProjected =
            let player = _player playerWithProjected
            in playerWithProjected { _player = player { _position = pos }}
        mkPlayers pos = do
            players <- vector 5 `suchThat` (\l -> nub l == l)
            return $ (setPosition pos) <$> players
