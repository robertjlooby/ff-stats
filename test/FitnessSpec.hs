{-# LANGUAGE OverloadedStrings #-}

module FitnessSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Fitness
import Teams
import Types

spec :: Spec
spec = describe "Fitness" $ do
    it "is the sum of the projected scores of players on the team" $
        let p = ClassicPlayerWithProjected "name" QB 100 "" 0 10 (TeamName "chi")
            team = ClassicTeam p p p p p p p p p
        in
            fitness team `shouldBe` 90
