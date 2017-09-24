{-# LANGUAGE OverloadedStrings #-}

module SpecialCases where

import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T

import Types

nameOverrides :: Map (PlayerName, Position, T.Text) T.Text
nameOverrides =
    Map.empty
      & Map.insert ("Adrian Peterson", RB, "NO") "adrian-peterson-min"
      & Map.insert ("Alex Smith", QB, "KC") "alex-smith-sf"
      & Map.insert ("Chris Ivory", RB, "JAX") "christopher-ivory"
      & Map.insert ("Danny Vitale", RB, "CLE") "dan-vitale"
      & Map.insert ("David Johnson", RB, "ARI") "david-johnson-rb"
      & Map.insert ("J.J. Nelson", WR, "ARI") "j.j.-nelson"
      & Map.insert ("Michael Thomas", WR, "NO") "michael-thomas-wr"
      & Map.insert ("Mitchell Trubisky", QB, "CHI") "mitch-trubisky"
      & Map.insert ("Rob Kelley", RB, "WAS") "robert-kelley"
      & Map.insert ("Ryan Grant", WR, "WAS") "ryan-grant-wr"
      & Map.insert ("Ted Ginn Jr.", WR, "NO") "ted-ginn-jr"
      & Map.insert ("Zach Miller", TE, "CHI") "zach-miller-chi"
      -- Defenses
      & Map.insert ("49ers ", DST, "SF") "san-francisco-defense"
      & Map.insert ("Bears ", DST, "CHI") "chicago-defense"
      & Map.insert ("Bengals ", DST, "CIN") "cincinnati-defense"
      & Map.insert ("Bills ", DST, "BUF") "buffalo-defense"
      & Map.insert ("Broncos ", DST, "DEN") "denver-defense"
      & Map.insert ("Browns ", DST, "CLE") "cleveland-defense"
      & Map.insert ("Buccaneers ", DST, "TB") "tampa-bay-defense"
      & Map.insert ("Cardinals ", DST, "ARI") "arizona-defense"
      & Map.insert ("Chargers ", DST, "LAC") "san-diego-defense"
      & Map.insert ("Chiefs ", DST, "KC") "kansas-city-defense"
      & Map.insert ("Colts ", DST, "IND") "indianapolis-defense"
      & Map.insert ("Cowboys ", DST, "DAL") "dallas-defense"
      & Map.insert ("Dolphins ", DST, "MIA") "miami-defense"
      & Map.insert ("Eagles ", DST, "PHI") "philadelphia-defense"
      & Map.insert ("Falcons ", DST, "ATL") "atlanta-defense"
      & Map.insert ("Giants ", DST, "NYG") "new-york-giants-defense"
      & Map.insert ("Jaguars ", DST, "JAX") "jacksonville-defense"
      & Map.insert ("Jets ", DST, "NYJ") "new-york-jets-defense"
      & Map.insert ("Lions ", DST, "DET") "detroit-defense"
      & Map.insert ("Packers ", DST, "GB") "green-bay-defense"
      & Map.insert ("Panthers ", DST, "CAR") "carolina-defense"
      & Map.insert ("Patriots ", DST, "NE") "new-england-defense"
      & Map.insert ("Raiders ", DST, "OAK") "oakland-defense"
      & Map.insert ("Rams ", DST, "LAR") "los-angeles-defense"
      & Map.insert ("Ravens ", DST, "BAL") "baltimore-defense"
      & Map.insert ("Redskins ", DST, "WAS") "washington-defense"
      & Map.insert ("Saints ", DST, "NO") "new-orleans-defense"
      & Map.insert ("Seahawks ", DST, "SEA") "seattle-defense"
      & Map.insert ("Steelers ", DST, "PIT") "pittsburgh-defense"
      & Map.insert ("Texans ", DST, "HOU") "houston-defense"
      & Map.insert ("Titans ", DST, "TEN") "tennessee-defense"
      & Map.insert ("Vikings ", DST, "MIN") "minnesota-defense"
