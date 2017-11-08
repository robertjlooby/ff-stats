module Fitness where

import Teams
import Types

salary :: Team -> Int
salary = sum . (fmap . fmap) cpSalary allPlayers

fitness :: Int -> Team -> Float
fitness salaryCap team =
    if salary team <= salaryCap then
        sum $ cpProjectedPoints <$> allPlayers team
    else
        0
