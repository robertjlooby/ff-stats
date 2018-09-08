module GenerateTeam where

import Control.Monad.Trans.State.Lazy (StateT, get, put)
import Data.Monoid ((<>))
import qualified Data.Vector as V
import System.Random (StdGen, randomR)
import Teams (PlayerPool(..), Team(..))
import Types

generatePlayerPool :: V.Vector PlayerWithProjected -> PlayerPool
generatePlayerPool players =
  PlayerPool
    (getPlayers QB)
    (getPlayers RB)
    (getPlayers WR)
    (getPlayers TE)
    (getPlayers DST)
  where
    getPlayers pos = V.toList . V.filter (\p -> getPos p == pos) $ players
    getPos = _position . _player

generateTeam :: Monad m => PlayerPool -> StateT StdGen m Team
generateTeam pool = do
  (qb, _) <- popRandom $ _qbs pool
  (rb1, rbs') <- popRandom $ _rbs pool
  (rb2, rbs'') <- popRandom rbs'
  (wr1, wrs') <- popRandom $ _wrs pool
  (wr2, wrs'') <- popRandom wrs'
  (wr3, wrs''') <- popRandom wrs''
  (te, tes') <- popRandom $ _tes pool
  (flex, _) <- popRandom $ rbs'' <> wrs''' <> tes'
  (dst, _) <- popRandom $ _dsts pool
  return $ Team qb rb1 rb2 wr1 wr2 wr3 te flex dst

popRandom :: Monad m => [a] -> StateT StdGen m (a, [a])
popRandom list = do
  seed <- get
  let (i, nextSeed) = randomR (0, length list - 1) seed
  put nextSeed
  let (h, a:t) = splitAt i list
  return (a, h <> t)
