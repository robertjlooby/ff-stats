module GenerateTeam where

import qualified Data.Vector as V
import Teams
import Types

generatePlayerPool :: V.Vector ClassicPlayerWithProjected -> PlayerPool
generatePlayerPool players = PlayerPool
  (V.filter (\p -> cpPosition p == QB) players)
  (V.filter (\p -> cpPosition p == RB) players)
  (V.filter (\p -> cpPosition p == WR) players)
  (V.filter (\p -> cpPosition p == TE) players)
  (V.filter (\p -> cpPosition p == WR || cpPosition p == RB || cpPosition p == TE) players)
  (V.filter (\p -> cpPosition p == DST) players)
