{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv (decodeByName, encodeDefaultOrderedByName)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Vector (toList)
import           Options.Applicative (Parser, (<**>), execParser, fullDesc, helper, info, long, option, progDesc, str)
import           Options.Applicative.Text (text)

import BestClassic

data Params = Params
    { file :: String
    , out  :: String
    , week :: T.Text }
    deriving (Eq, Show)

paramsParser :: Parser Params
paramsParser = Params
    <$> option str ( long "file")
    <*> option str ( long "out")
    <*> option text ( long "week")

main :: IO ()
main = do
    params <- execParser opts
    csvData <- BL.readFile (file params)
    case decodeByName csvData of
      Right (_, players) -> do
          players' <- playersWithProjected (week params) players
          case players' of
            Right ps -> BL.writeFile (out params) $ encodeDefaultOrderedByName (toList ps)
            Left err -> print err
      left -> print left
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "Fetch projected scores for a pick 'em csv" )
