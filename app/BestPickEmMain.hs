{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv (decodeByName)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Options.Applicative (Parser, (<**>), execParser, fullDesc, helper, info, long, option, progDesc, str)
import           Options.Applicative.Text (text)

import           BestPickEm

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
