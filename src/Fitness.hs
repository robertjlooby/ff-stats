module Fitness where

import Teams
import Types

salary :: Team -> Integer
salary = toInteger . sum . (fmap . fmap) (_salary . _player) allPlayers

fitness :: Integer -> Team -> Float
fitness salaryCap team =
    if salary team <= salaryCap then
        sum $ _projectedPoints <$> allPlayers team
    else
        0
