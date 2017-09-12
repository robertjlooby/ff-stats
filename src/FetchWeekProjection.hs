{-# LANGUAGE OverloadedStrings #-}

module FetchWeekProjection where

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

request :: T.Text -> T.Text -> IO (Response ByteString)
request name week =
    let opts = defaults & param "week" .~ [week]
        name' =
            name
              & T.replace " Jr." ""
              & T.replace " Sr." ""
              & T.replace " II" ""
              & T.replace "." ""
              & T.replace "'" ""
              & T.replace " " "-"
              & T.toLower
              & T.unpack
        url = "https://www.fantasypros.com/nfl/projections/" <> name' <> ".php"
    in
        getWith opts url

getProjectedScore :: T.Text -> T.Text -> IO (Either String Float)
getProjectedScore name week = do
    resp <- request name week
    let body = resp ^. responseBody
        outlook = listToMaybe $
                    (fromDocument $ parseLBS body)
                        $// (element "div" >=> attributeIs "class" "outlook")
    case outlook of
      Just cursor ->
          case cursor $// (element "span" >=> attributeIs "class" "pull-right") of
            [] -> return $ Left $ "Player projected score not found for: " <> show name <> " week " <> show week
            cursors ->
                cursors
                & last
                & node
                & toXMLNode
                & nodeText
                & T.concat
                & T.replace " pts" ""
                & T.unpack
                & readEither
                & return
      Nothing -> return . Left $ "Player outlook div not found for: " <> show name <> " week " <> show week
