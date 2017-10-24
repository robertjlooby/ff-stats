module Fitness where

import Teams
import Types

fitness :: Int -> ClassicTeam -> Float
fitness salaryCap team =
    if (sum $ cpSalary <$> players) <= salaryCap then
        sum $ cpProjectedPoints <$> players
    else
        0
  where
    players = allPlayers team
