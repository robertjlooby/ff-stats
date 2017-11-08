module Fitness where

import Teams
import Types

salary :: Team -> Int
salary = sum . (fmap . fmap) (_salary . _player) allPlayers

fitness :: Int -> Team -> Float
fitness salaryCap team =
    if salary team <= salaryCap then
        sum $ _projectedPoints <$> allPlayers team
    else
        0
