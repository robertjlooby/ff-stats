module Main where

import           Test.Hspec (hspec)

import           FitnessSpec (spec)

main :: IO ()
main = hspec $ do
    FitnessSpec.spec
