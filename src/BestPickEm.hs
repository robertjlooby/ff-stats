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
import           Options.Applicative.Text (text)

import FetchWeekProjection (getProjectedScore)

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

data PlayerWithProjected = PlayerWithProjected
    { pName :: T.Text
    , pPosition :: T.Text
    , pRosterPosition :: T.Text
    , pGameInfo :: T.Text
    , pAvgPointsPerGame :: Float
    , pProjectedPoints :: Float
    , pTeam :: T.Text }
    deriving (Eq, Show)

withProjected :: Player -> Float -> PlayerWithProjected
withProjected player projected =
    PlayerWithProjected
        (name player)
        (position player)
        (rosterPosition player)
        (gameInfo player)
        (avgPointsPerGame player)
        projected
        (team player)

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
          traverse print (pickBestLineupByAvgPoints players)
          putStrLn "Best lineup by projected points:"
          byPoints <- pickBestLineupByProjectedPoints (week params) players
          case byPoints of
            Left err -> putStrLn err
            Right pls -> do
                traverse print pls
                return ()
      left -> print left
  where
    opts = info (paramsParser <**> helper)
      ( fullDesc <> progDesc "The best pick 'em lineup from a csv" )

pickBestLineupByAvgPoints :: Vector Player -> Vector Player
pickBestLineupByAvgPoints players =
    rosterPositions players
      & fmap maxAtRosterPosition
  where
    maxAtRosterPosition rp =
        players
          & V.filter (\player -> rp == rosterPosition player)
          & maximumBy (\p1 p2 -> compare (avgPointsPerGame p1) (avgPointsPerGame p2))

pickBestLineupByProjectedPoints :: T.Text -> Vector Player -> IO (Either String (Vector PlayerWithProjected))
pickBestLineupByProjectedPoints week players = do
    players' <- playersWithProjected week players
    case players' of
      Left err -> return $ Left err
      Right playersWithProjected' ->
        rosterPositions players
          & fmap (maxAtRosterPosition playersWithProjected')
          & Right
          & return
  where
    maxAtRosterPosition players' rp =
        players'
          & V.filter (\player -> rp == pRosterPosition player)
          & maximumBy (\p1 p2 -> compare (pProjectedPoints p1) (pProjectedPoints p2))

playersWithProjected :: T.Text -> Vector Player -> IO (Either String (Vector PlayerWithProjected))
playersWithProjected week players =
    traverse (\pl -> (fmap . fmap) (withProjected pl) (getProjectedScore (name pl) week)) players
      & fmap sequence

rosterPositions :: Vector Player -> Vector T.Text
rosterPositions players =
    players
      & fmap rosterPosition
      & V.modify sort
      & uniq
