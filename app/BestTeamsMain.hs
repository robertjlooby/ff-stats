module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv ( decodeByName, encode )
import           Data.Semigroup ( (<>) )
import           Data.Text ( Text )
import           System.Random ( newStdGen )

import           BestTeams
import           BestTeamsConfig
import           Dhall ( auto, input )
import           Fitness
import           Options.Applicative
       ( (<**>), Parser, execParser, fullDesc, helper, info, long, option
       , progDesc, str )
import           Teams ( allPlayers, teamHeaders )
import           Types ( _name, _player, getPlayerName )

data Params =
    Params
        { configFile :: Text
        , file :: String
        , out :: String
        }

paramsParser :: Parser Params
paramsParser =
    Params <$> option str (long "config")
    <*> option str (long "file")
    <*> option str (long "out")

main :: IO ()
main = do
    params <- execParser opts
    config <- input auto (configFile params)
    csvData <- BL.readFile (file params)
    case decodeByName csvData of
        Right ( _, players ) -> do
            lineups <- runApp (pickBestLineups players) config =<< newStdGen
            BL.writeFile (out params) $ teamHeaders <> encode lineups
            fitnessFn <- runApp fitness config =<< newStdGen
            mapM_ (showTeam fitnessFn) lineups
        left -> print left
  where
    opts =
        info
            (paramsParser <**> helper)
            (fullDesc <> progDesc "The lineup from a csv (with projections)")

    showTeam fitnessFn lineup = do
        putStrLn "Team:"
        putStrLn $ "  Fitness: " <> (show . fitnessFn $ lineup)
        putStrLn $ "  Salary: " <> (show . salary $ lineup)
        putStrLn $
            "  Team: "
            <> show (getPlayerName . _name . _player <$> allPlayers lineup)
