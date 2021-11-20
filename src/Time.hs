{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Time where

import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Time as T

data Plural = Plural
  { forPlural :: Int -> String,
    withPlural :: Int -> String
  }

twoFormsForPlural :: String -> String -> Int -> String
twoFormsForPlural s ss c
  | c == 0 || c == 1 = s
  | otherwise = ss

twoFormsWithPlural :: (Int -> String) -> Int -> String
twoFormsWithPlural f c = show c ++ " " ++ f c

makeTwoFormsPlural :: String -> String -> Plural
makeTwoFormsPlural s ss =
  let forPlural = twoFormsForPlural s ss
   in Plural
        { withPlural = twoFormsWithPlural forPlural,
          forPlural
        }


data Locale = Locale
  { -- The ISO name for this locale, like "fr", or "fr-FR", without
    -- encoding.
    lIsoName :: String,
    -- |
    lSecond :: Plural,
    lMinute :: Plural,
    lHour :: Plural,
    lDay :: Plural,
    lWeek :: Plural,
    lFuzzyFutureFormat :: String,
    lFuzzyPastFormat :: String
  }

fr =
  Locale
    { lIsoName = "fr-FR",
      lFuzzyFutureFormat = "dans {}",
      lFuzzyPastFormat = "il y a {}",
      lSecond = makeTwoFormsPlural "seconde" "seconds",
      lMinute = makeTwoFormsPlural "minute" "minutes",
      lHour = makeTwoFormsPlural "heure" "heures",
      lDay = makeTwoFormsPlural "jour" "jours",
      lWeek = makeTwoFormsPlural "semaine" "semaines"
    }

data Interval
  = Year
  | Quarter
  | Month
  | Week
  | Day
  | Hour
  | Minute
  | Second
  | Millisecond

data Direction = Backward | Forward

data YesterdayStrategy
  = NoYesterdays
  | Yesterday
      {
      }

data FuzzyDiffFormat = FuzzyDiffFormat
  { fGranularities :: M.Map Interval [Int],
    lowClamp :: Maybe T.NominalDiffTime,
    highClamp :: Maybe T.NominalDiffTime,
    yesterday :: (YesterdayStrategy, YesterdayStrategy)
  }

-- | Render a fuzzy time difference between two instants.
diff :: FuzzyDiffFormat -> T.UTCTime -> T.UTCTime -> String
diff format ref other = undefined
