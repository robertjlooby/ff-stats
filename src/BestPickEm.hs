{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv (FromNamedRecord(..), (.:), decodeByName)
import           Data.Function ((&))
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Vector (Vector, maximumBy, uniq)
import           Data.Vector.Algorithms.Merge (sort)
import           Options.Applicative (Parser, (<**>), execParser, fullDesc, helper, info, long, option, progDesc, str)

data Player = Player
    { name :: T.Text
    , position :: T.Text
    , rosterPosition :: T.Text
    , gameInfo :: T.Text
    , avgPointsPerGame :: Float
    , team :: T.Text }
    deriving (Eq, Show)

instance FromNamedRecord Player where
    parseNamedRecord m =
        Player
          <$> m .: "Name"
          <*> m .: "Position"
          <*> m .: "Roster_Position"
          <*> m .: "GameInfo"
          <*> m .: "AvgPointsPerGame"
          <*> m .: "teamAbbrev"

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
          putStrLn "Best lineup by avg. points:"
          traverse print (pickBestLineupByAvgPoints players)
          return ()
      left -> print left
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "The best pick 'em lineup from a csv" )

pickBestLineupByAvgPoints :: Vector Player -> Vector Player
pickBestLineupByAvgPoints players =
    rosterPositions
      & fmap maxAtRosterPosition
  where
    rosterPositions =
        players
          & fmap rosterPosition
          & V.modify sort
          & uniq
    maxAtRosterPosition rp =
        players
          & V.filter (\player -> rp == rosterPosition player)
          & maximumBy (\p1 p2 -> compare (avgPointsPerGame p1) (avgPointsPerGame p2))
