{-# LANGUAGE RankNTypes #-}

module GenerateTeam where

import qualified Data.Vector as V

import BestTeamsConfig
import Teams (PlayerPool(..), Team(..))
import Types

generatePlayerPool :: V.Vector PlayerWithProjected -> PlayerPool
generatePlayerPool players = PlayerPool (getPlayers QB)
  where
    getPlayers pos = V.toList . V.filter (\p -> getPos p == pos) $ players
    getPos = _position . _player

generateTeam :: PlayerPool -> App Team
generateTeam pool = do
  (qb1, qbs') <- popRandom $ _qbs pool
  (qb2, qbs'') <- popRandom $ qbs'
  (qb3, qbs''') <- popRandom $ qbs''
  (qb4, qbs'''') <- popRandom $ qbs'''
  (qb5, qbs''''') <- popRandom $ qbs''''
  (qb6, qbs'''''') <- popRandom $ qbs'''''
  return $ Team qb1 qb2 qb3 qb4 qb5 qb6

popRandom :: [a] -> App (a, [a])
popRandom list = do
  i <- getRandom (0, length list - 1)
  let (h, a:t) = splitAt i list
  return (a, h <> t)
