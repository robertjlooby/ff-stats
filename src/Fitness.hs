module Fitness where

import BestTeamsConfig
import Teams
import Types

salary :: Team -> Integer
salary = toInteger . sum . (fmap . fmap) (_salary . _player) allPlayers

fitness :: App (Team -> Float)
fitness = do
  salaryCap <- asks _salaryCap
  return $ fitness' salaryCap

fitness' :: Integer -> Team -> Float
fitness' salaryCap team
  | salary team > salaryCap = 0
  | otherwise = base
  where
    base = sum $ _projectedPoints <$> allPlayers team
