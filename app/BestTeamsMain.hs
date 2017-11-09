{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv (decodeByName, encode)
import           Data.Semigroup ((<>))
import           Options.Applicative (Parser, (<**>), auto, execParser, fullDesc, helper, info, long, option, progDesc, str)
import           System.Remote.Monitoring

import           BestTeams
import           Fitness
import           Teams (allPlayers, teamHeaders)
import           Types (_name, _player, getPlayerName)

data Params = Params
    { file :: String
    , out  :: String
    , initialPoolSize :: Int
    , resultCount :: Int
    , salaryCap :: Int
    } deriving (Eq, Show)

paramsParser :: Parser Params
paramsParser = Params
    <$> option str ( long "file")
    <*> option str ( long "out")
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
          BL.writeFile (out params) $ teamHeaders <> encode lineups
          mapM_ (showTeam (fitness (salaryCap params))) lineups
      left -> print left
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "The lineup from a csv (with projections)" )
    showTeam fitnessFn lineup = do
        putStrLn "Team:"
        putStrLn $ "  Fitness: " <> (show . fitnessFn $ lineup)
        putStrLn $ "  Salary: " <> (show . salary $ lineup)
        putStrLn $ "  Team: " <> (show $ getPlayerName . _name . _player <$> allPlayers lineup)
