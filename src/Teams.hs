module Teams where

import           Data.List (nub)
import           Test.QuickCheck.Arbitrary (Arbitrary(..), vector)
import           Test.QuickCheck.Gen (suchThat)

import           Types

data ClassicTeam = ClassicTeam
    { _qb :: ClassicPlayerWithProjected
    , _rb1 :: ClassicPlayerWithProjected
    , _rb2 :: ClassicPlayerWithProjected
    , _wr1 :: ClassicPlayerWithProjected
    , _wr2 :: ClassicPlayerWithProjected
    , _wr3 :: ClassicPlayerWithProjected
    , _te :: ClassicPlayerWithProjected
    , _flex :: ClassicPlayerWithProjected
    , _dst :: ClassicPlayerWithProjected
    } deriving (Eq, Show)

allPlayers :: ClassicTeam -> [ClassicPlayerWithProjected]
allPlayers team = [_qb, _rb1, _rb2, _wr1, _wr2, _wr3, _te, _flex, _dst] <*> pure team

data PlayerPool = PlayerPool
    { _qbs :: [ClassicPlayerWithProjected]
    , _rbs :: [ClassicPlayerWithProjected]
    , _wrs :: [ClassicPlayerWithProjected]
    , _tes :: [ClassicPlayerWithProjected]
    , _dsts :: [ClassicPlayerWithProjected]
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
        setPosition pos player = player { cpPosition = pos }
        mkPlayers pos = do
            players <- vector 5 `suchThat` (\l -> nub l == l)
            return $ (setPosition pos) <$> players
