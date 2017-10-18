{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv (decodeByName)
import           Data.Semigroup ((<>))
import           Options.Applicative (Parser, (<**>), execParser, fullDesc, helper, info, long, option, progDesc, str)

import           BestClassic
import           Fitness

data Params = Params
    { file :: String }
    deriving (Eq, Show)

paramsParser :: Parser Params
paramsParser = Params
    <$> option str ( long "file")

main :: IO ()
main = do
    params <- execParser opts
    csvData <- BL.readFile (file params)
    case decodeByName csvData of
      Right (_, players) -> do
          lineup <- pickBestLineup players
          putStrLn $ "Best lineup by projected points: " <> (show . fitness $ lineup) <> " pts"
          print lineup
          return ()
      left -> print left
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "The classic lineup from a csv (with projections)" )