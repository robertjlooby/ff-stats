module Main where

import           Test.Hspec (hspec)

import           FitnessSpec (spec)
import           GenerateTeamSpec (spec)

main :: IO ()
main = hspec $ do
    FitnessSpec.spec
    GenerateTeamSpec.spec
