{-# LANGUAGE OverloadedStrings #-}

module FitnessSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Fitness
import Teams
import Types

spec :: Spec
spec = describe "Fitness" $ do
    it "is the sum of the projected scores of players on the team" $
        let player = Player "name" "name and id" QB 100 "" (TeamName "chi")
            p = PlayerWithProjected player 10
            team = Team p p p p p p p p p
        in
            fitness 1000 team `shouldBe` 90

    it "is 0 if the team is over the salary cap" $
        let player = Player "name" "name and id" QB 100 "" (TeamName "chi")
            p = PlayerWithProjected player 10
            team = Team p p p p p p p p p
        in
            fitness 10 team `shouldBe` 0
