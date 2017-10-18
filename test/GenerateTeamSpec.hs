module GenerateTeamSpec where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===), conjoin, ioProperty, property)
import Test.QuickCheck.Property (disjoin)

import GenerateTeam
import Teams
import Types

spec :: Spec
spec = describe "Teams" $ do
    describe "generate player pool" $ do
        it "separates out the QBs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === QB) <$> (_qbs $ generatePlayerPool players)

        it "separates out the RBs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === RB) <$> (_rbs $ generatePlayerPool players)

        it "separates out the WRs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === WR) <$> (_wrs $ generatePlayerPool players)

        it "separates out the TEs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === TE) <$> (_tes $ generatePlayerPool players)

        it "separates out the DSTs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === DST) <$> (_dsts $ generatePlayerPool players)

    describe "generate team" $ do
        it "generates a team with a qb" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ QB === (cpPosition $ _qb team)

        it "generates a team with a rb1" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ RB === (cpPosition $ _rb1 team)

        it "generates a team with a rb2" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ RB === (cpPosition $ _rb2 team)

        it "has unique running backs" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ _rb1 team /= _rb2 team

        it "generates a team with a wr1" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ WR === (cpPosition $ _wr1 team)

        it "generates a team with a wr2" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ WR === (cpPosition $ _wr2 team)

        it "generates a team with a wr3" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ WR === (cpPosition $ _wr3 team)

        it "has unique wide receivers" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ conjoin
                [ _wr1 team /= _wr2 team
                , _wr1 team /= _wr3 team
                , _wr2 team /= _wr3 team
                ]

        it "generates a team with a te" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ TE === (cpPosition $ _te team)

        it "generates a team with a flex" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ disjoin
                [ RB === (cpPosition $ _flex team)
                , WR === (cpPosition $ _flex team)
                , TE === (cpPosition $ _flex team)
                ]

        it "has unique flex" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool

            return $ conjoin
                [ _flex team /= _rb1 team
                , _flex team /= _rb2 team
                , _flex team /= _wr1 team
                , _flex team /= _wr2 team
                , _flex team /= _wr3 team
                , _flex team /= _te team
                ]

        it "generates a team with a dst" $ property $ \pool -> ioProperty $ do
            team <- generateTeam pool
            return $ DST === (cpPosition $ _dst team)
