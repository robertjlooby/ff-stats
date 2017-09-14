{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv (decodeByName)
import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Vector (Vector, maximumBy, uniq)
import           Data.Vector.Algorithms.Merge (sort)
import           Options.Applicative (Parser, (<**>), execParser, fullDesc, helper, info, long, option, progDesc, str)
import           Options.Applicative.Text (text)

import FetchWeekProjection (getProjectedScore, paramifyName)
import Types


withProjected :: Player -> Float -> PlayerWithProjected
withProjected player projected =
    PlayerWithProjected
        (name player)
        (position player)
        (rosterPosition player)
        (gameInfo player)
        (avgPointsPerGame player)
        projected
        (team player)

data Params = Params
    { file :: String
    , week :: T.Text }
    deriving (Eq, Show)

paramsParser :: Parser Params
paramsParser = Params
    <$> option str ( long "file")
    <*> option text ( long "week")

main :: IO ()
main = do
    params <- execParser opts
    csvData <- BL.readFile (file params)
    case decodeByName csvData of
      Right (_, players) -> do
          putStrLn "Best lineup by avg. points:"
          _ <- traverse print (pickBestLineupByAvgPoints players)
          putStrLn "Best lineup by projected points:"
          byPoints <- pickBestLineupByProjectedPoints (week params) players
          case byPoints of
            Left err -> putStrLn err
            Right pls -> do
                _ <- traverse print pls
                return ()
      left -> print left
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "The best pick 'em lineup from a csv" )

pickBestLineupByAvgPoints :: Vector Player -> Vector Player
pickBestLineupByAvgPoints players =
    rosterPositions players
      & fmap maxAtRosterPosition
  where
    maxAtRosterPosition rp =
        players
          & V.filter (\player -> rp == rosterPosition player)
          & maximumBy (\p1 p2 -> compare (avgPointsPerGame p1) (avgPointsPerGame p2))

pickBestLineupByProjectedPoints :: T.Text -> Vector Player -> IO (Either String (Vector PlayerWithProjected))
pickBestLineupByProjectedPoints week' players = do
    players' <- playersWithProjected week' players
    case players' of
      Left err -> return $ Left err
      Right playersWithProjected' ->
        rosterPositions players
          & fmap (maxAtRosterPosition playersWithProjected')
          & Right
          & return
  where
    maxAtRosterPosition players' rp =
        players'
          & V.filter (\player -> rp == pRosterPosition player)
          & maximumBy (\p1 p2 -> compare (pProjectedPoints p1) (pProjectedPoints p2))

playersWithProjected :: T.Text -> Vector Player -> IO (Either String (Vector PlayerWithProjected))
playersWithProjected week' players =
    traverse (\pl -> (fmap . fmap) (withProjected pl) (getProjectedScore (getNameWithOverride pl) week' (shouldUsePPR pl))) players
      & fmap sequence

shouldUsePPR :: Player -> Bool
shouldUsePPR player = position player /= "QB"

rosterPositions :: Vector Player -> Vector T.Text
rosterPositions players =
    players
      & fmap rosterPosition
      & V.modify sort
      & uniq

getNameWithOverride :: Player -> T.Text
getNameWithOverride player =
    let defaultName = paramifyName $ name player
        playerKey = (name player, position player, team player)
    in
        Map.findWithDefault defaultName playerKey nameOverrides

nameOverrides :: Map (T.Text, T.Text, T.Text) T.Text
nameOverrides =
    Map.empty
      & Map.insert ("Alex Smith", "QB", "KC") "alex-smith-sf"
      & Map.insert ("David Johnson", "RB", "ARI") "david-johnson-rb"
      & Map.insert ("Michael Thomas", "WR", "NO") "michael-thomas-wr"
