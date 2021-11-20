{-|
Module      : Locales.Fr
Description : French translation.
Copyright   : © 2021 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental
-}

module Locales.French where

import Data.Map ((!))
import qualified Data.Map as M

nombres =
  M.fromList
    [ (0, "zéro"),
      (1, "un"),
      (2, "deux"),
      (3, "trois"),
      (4, "quatre"),
      (5, "cinq"),
      (6, "six"),
      (7, "sept"),
      (8, "huit"),
      (9, "neuf"),
      (10, "dix"),
      (11, "onze"),
      (12, "douze"),
      (13, "treize"),
      (14, "quatorze"),
      (15, "quinze"),
      (16, "seize"),
      (20, "vingt"),
      (21, "vingt-et-un"),
      (30, "trente"),
      (31, "trente-et-un"),
      (40, "quarante"),
      (41, "quarante-et-un"),
      (50, "cinquante"),
      (51, "cinquante-et-un"),
      (60, "soixante"),
      (61, "soixante-et-un"),
      (61, "soixante-et-un"),
      (70, "soixante-dix"),
      (71, "soixante-et-onze"),
      (72, "soixante-douze"),
      (73, "soixante-treize"),
      (74, "soixante-quatorze"),
      (75, "soixante-quinze"),
      (76, "soixante-seize"),
      (77, "soixante-dix-sept"),
      (78, "soixante-dix-huit"),
      (79, "soixante-dix-neuf"),
      (80, "quatre-vingt"),
      (90, "quatre-vingt-dix"),
      (91, "quatre-vingt-onze"),
      (92, "quatre-vingt-douze"),
      (93, "quatre-vingt-treize"),
      (94, "quatre-vingt-quatorze"),
      (95, "quatre-vingt-quinze"),
      (96, "quatre-vingt-seize"),
      (97, "quatre-vingt-dix-sept"),
      (98, "quatre-vingt-dix-huit"),
      (99, "quatre-vingt-dix-neuf"),
      (100, "cent"),
      (1000, "mille") ]

split base n =
  ( n `div` base,
    n `div` base * base,
    n `mod` base
  )

etlSup1 :: Int -> String
etlSup1 1 = ""
etlSup1 x = etl x ++ " "

etlNonZero :: Int -> String
etlNonZero 0 = ""
etlNonZero x = " " ++ etl x

-- Converts an int to its textual representation, in French.
etl :: Int -> String
etl i
  | i `M.member` nombres = nombres ! i
  | i < 100 =
    let (_, diz, reste) = split 10 i
     in nombres ! diz ++ "-" ++ etl reste
  | i < 1000 =
    let (cent, _, reste) = split 100 i
     in etlSup1 cent ++ nombres ! 100 ++ etlNonZero reste
  | i < 1000000 =
    let (mille, _, reste) = split 1000 i
     in etlSup1 mille ++ nombres ! 1000 ++ etlNonZero reste
  | i < 1000000000000 =
    let (millions, _, reste) = split 1000000 i
        s
          | millions == 1 = ""
          | otherwise = "s"
     in etl millions ++ " " ++ "million" ++ s ++ etlNonZero reste
  | otherwise = show i
