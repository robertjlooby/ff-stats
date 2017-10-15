module GenerateTeamSpec where

import Data.Vector
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===), conjoin, property)

import GenerateTeam
import Teams
import Types

spec :: Spec
spec = describe "Teams" $ do
    describe "generate player pool" $ do
        it "separates out the QBs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === QB) <$> (toList $ _qbs $ generatePlayerPool players)

        it "separates out the RBs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === RB) <$> (toList $ _rbs $ generatePlayerPool players)

        it "separates out the WRs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === WR) <$> (toList $ _wrs $ generatePlayerPool players)

        it "separates out the TEs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === TE) <$> (toList $ _tes $ generatePlayerPool players)

        it "separates out the Flex positions" $ property $ \players ->
            let pool = generatePlayerPool players
                flexCount = Data.Vector.length $ _flexes $ pool
                rbCount = Data.Vector.length $ _rbs $ pool
                wrCount = Data.Vector.length $ _wrs $ pool
                teCount = Data.Vector.length $ _tes $ pool
            in
                flexCount === rbCount + wrCount + teCount

        it "separates out the DSTs" $ property $ \players ->
            conjoin $ (\player -> cpPosition player === DST) <$> (toList $ _dsts $ generatePlayerPool players)
