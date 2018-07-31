{-# LANGUAGE BangPatterns #-}

module FetchWeekProjection where

import Control.Lens ((.~), (^.))
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.XML.Types (nodeText)
import Network.Wreq (Response, defaults, getWith, param, responseBody)
import Text.HTML.DOM (parseLBS)
import Text.Read (readEither)
import Text.XML (toXMLNode)
import Text.XML.Cursor (($//), (>=>), attributeIs, element, fromDocument, node)

import Types (PlayerName(..))

request :: T.Text -> T.Text -> Bool -> IO (Response ByteString)
request name week usePPR =
  let opts =
        defaults & param "week" .~ [week] &
        param "scoring" .~
        [ if usePPR
            then "PPR"
            else "STANDARD"
        ]
      url =
        "https://www.fantasypros.com/nfl/projections/" <> T.unpack name <>
        ".php"
   in getWith opts url

paramifyName :: PlayerName -> T.Text
paramifyName (PlayerName name) =
  name & T.replace " Jr." "" & T.replace " Sr." "" & T.replace " II" "" &
  T.replace "." "" &
  T.replace "'" "" &
  T.replace " " "-" &
  T.toLower

getProjectedScore :: T.Text -> T.Text -> Bool -> IO (Either String Float)
getProjectedScore name week usePPR = do
  resp <- request name week usePPR
  let body = resp ^. responseBody
      outlook =
        listToMaybe $
        (fromDocument $ parseLBS body) $//
        (element "div" >=> attributeIs "class" "outlook")
  case outlook of
    Just cursor ->
      case cursor $// (element "span" >=> attributeIs "class" "pull-right") of
        [] ->
          return $
          Left $
          "Player projected score not found for: " <> show name <> " week " <>
          show week
        cursors -> do
          !score <-
            cursors & last & node & toXMLNode & nodeText & T.concat &
            T.replace " pts" "" &
            T.unpack &
            readEither &
            return
          return score
    Nothing ->
      return . Left $
      "Player outlook div not found for: " <> show name <> " week " <> show week
