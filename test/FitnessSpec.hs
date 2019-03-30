module FitnessSpec where

import           BestTeamsConfig
import           Fitness
import           Teams
import           Test.Hspec ( Spec, describe, it, shouldBe )
import           Types

spec :: Spec
spec = describe "Fitness" $ do
    it "is the sum of the projected scores of players on the team" $
        let playerOn = Player "name" "name and id" QB 100 ""
            otherQB = PlayerWithProjected (playerOn "CHI") 10
            p = PlayerWithProjected (playerOn "GB") 10
            team = Team otherQB p p p p p p p p
            teamSalary = 1000
        in fitness' Normal teamSalary team `shouldBe` 90
    it "is 0 if the team is over the salary cap" $
        let player = Player "name" "name and id" QB 100 "" "CHI"
            p = PlayerWithProjected player 10
            team = Team p p p p p p p p p
            teamSalary = 10
        in fitness' Normal teamSalary team `shouldBe` 0
    it "applies a bonus if the TE is on the same team as the QB" $
        let playerOn pos = Player "name" "name and id" pos 100 ""
            chiQb = PlayerWithProjected (playerOn QB "CHI") 10
            chiTe = PlayerWithProjected (playerOn TE "CHI") 10
            p = PlayerWithProjected (playerOn RB "GB") 10
            team = Team chiQb p p p p p chiTe p p
            teamSalary = 1000
        in fitness' Stacked teamSalary team `shouldBe` 92
    it "applies a bonus if the a WR is on the same team as the QB" $
        let playerOn pos = Player "name" "name and id" pos 100 ""
            chiQb = PlayerWithProjected (playerOn QB "CHI") 10
            chiWr = PlayerWithProjected (playerOn WR "CHI") 10
            p = PlayerWithProjected (playerOn RB "GB") 10
            team = Team chiQb p p p p p chiWr p p
            teamSalary = 1000
        in fitness' Stacked teamSalary team `shouldBe` 92
    it "maxes the bonus at 4" $
        let playerOn pos = Player "name" "name and id" pos 100 ""
            chiQb = PlayerWithProjected (playerOn QB "CHI") 10
            chiWr = PlayerWithProjected (playerOn WR "CHI") 10
            p = PlayerWithProjected (playerOn RB "GB") 10
            team = Team chiQb p p p chiWr chiWr chiWr p p
            teamSalary = 1000
        in fitness' Stacked teamSalary team `shouldBe` 94
    it "doesn't apply a bonus for normal" $
        let playerOn pos = Player "name" "name and id" pos 100 ""
            chiQb = PlayerWithProjected (playerOn QB "CHI") 10
            chiWr = PlayerWithProjected (playerOn WR "CHI") 10
            p = PlayerWithProjected (playerOn RB "GB") 10
            team = Team chiQb p p p chiWr chiWr chiWr p p
            teamSalary = 1000
        in fitness' Normal teamSalary team `shouldBe` 90
