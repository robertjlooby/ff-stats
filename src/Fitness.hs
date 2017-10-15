module Fitness where

import Teams
import Types

fitness :: ClassicTeam -> Float
fitness team = sum $ cpProjectedPoints <$> allPlayers team
