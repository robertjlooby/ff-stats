{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens ((.~), (^.))
import           Data.ByteString.Lazy (ByteString)
import           Data.Function ((&))
import           Data.Maybe (listToMaybe)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.XML.Types (nodeText)
import           Network.Wreq (Response, defaults, getWith, param, responseBody)
import           Options.Applicative (Parser, (<**>), execParser, fullDesc, helper, info, long, option, progDesc)
import           Options.Applicative.Text (text)
import           Text.HTML.DOM (parseLBS)
import           Text.Read (readEither)
import           Text.XML (toXMLNode)
import           Text.XML.Cursor (Cursor, ($//), (>=>), (&//), attributeIs, content, element, fromDocument, node)

import FetchWeekProjection

main :: IO ()
main = do
    params <- execParser opts
    projectedScore <- getProjectedScore (name params) (week params)
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
