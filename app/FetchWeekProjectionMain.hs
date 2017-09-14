{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Options.Applicative (Parser, (<**>), execParser, fullDesc, helper, info, long, option, progDesc)
import           Options.Applicative.Text (text)

import FetchWeekProjection

main :: IO ()
main = do
    params <- execParser opts
    projectedScore <- getProjectedScore (name params) (week params) False
    print projectedScore
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "A player's projected pts for a given week" )

data Params = Params
    { name     :: T.Text
    , week     :: T.Text }
    deriving (Eq, Show)

paramsParser :: Parser Params
paramsParser = Params
    <$> option text ( long "name")
    <*> option text ( long "week")
