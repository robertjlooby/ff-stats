{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv (decodeByName)
import           Data.Semigroup ((<>))
import           Options.Applicative (Parser, (<**>), auto, execParser, fullDesc, helper, info, long, option, progDesc, str)
import           System.Remote.Monitoring

import           BestClassic
import           Fitness
import           Teams (allPlayers)
import           Types (cpName, getPlayerName)

data Params = Params
    { file :: String
    , initialPoolSize :: Int
    , resultCount :: Int
    , salaryCap :: Int
    } deriving (Eq, Show)

paramsParser :: Parser Params
paramsParser = Params
    <$> option str ( long "file")
    <*> option auto ( long "pool")
    <*> option auto ( long "count")
    <*> option auto ( long "salary")

main :: IO ()
main = do
    _ <- forkServer "localhost" 8000
    params <- execParser opts
    csvData <- BL.readFile (file params)
    case decodeByName csvData of
      Right (_, players) -> do
          lineups <- pickBestLineups (salaryCap params) (resultCount params) (initialPoolSize params) players
          mapM_ (showTeam (fitness (salaryCap params))) lineups
      left -> print left
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "The classic lineup from a csv (with projections)" )
    showTeam fitnessFn lineup = do
        putStrLn "Team:"
        putStrLn $ "  Fitness: " <> (show . fitnessFn $ lineup)
        putStrLn $ "  Salary: " <> (show . salary $ lineup)
        putStrLn $ "  Team: " <> (show $ getPlayerName . cpName <$> allPlayers lineup)
