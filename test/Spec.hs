module Main where

import           FitnessSpec ( spec )
import           GenerateTeamSpec ( spec )
import           Test.Hspec ( hspec )

main :: IO ()
main = hspec $ do
    FitnessSpec.spec
    GenerateTeamSpec.spec
