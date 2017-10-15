module Teams where

import Data.Vector
import Types

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
    { _qbs :: Vector ClassicPlayerWithProjected
    , _rbs :: Vector ClassicPlayerWithProjected
    , _wrs :: Vector ClassicPlayerWithProjected
    , _tes :: Vector ClassicPlayerWithProjected
    , _flexes :: Vector ClassicPlayerWithProjected
    , _dsts :: Vector ClassicPlayerWithProjected
    } deriving (Eq, Show)
