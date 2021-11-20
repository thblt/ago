{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Locales.En
-- Description : English language(s) support.
-- Copyright   : © 2021 Thibault Polge
-- License     : GPL-3
-- Maintainer  : thibault@thb.lt
-- Stability   : experimental

module Locales.English where

import Data.Map ((!))
import qualified Data.Map as M

numbers =
  M.fromList
    [ (0, "zero"),
      (1, "one"),
      (2, "two"),
      (3, "three"),
      (4, "four"),
      (5, "five"),
      (6, "six"),
      (7, "seven"),
      (8, "eight"),
      (9, "nine"),
      (10, "ten"),
      (11, "eleven"),
      (12, "twelve"),
      (13, "thirteen"),
      (14, "fourteen"),
      (15, "fifteen"),
      (16, "sixteen"),
      (17, "seventeen"),
      (18, "eighteen"),
      (19, "nineteen"),
      (20, "twenty"),
      (30, "thirty"),
      (40, "fourty"),
      (50, "fifty"),
      (60, "sixty"),
      (70, "seventy"),
      (80, "seventy"),
      (90, "ninety")
    ]

powers =
  [ (1_000_000, " million", " and "),
    (1_000, " thousand", ", "),
    (100, " hundred", " ")
  ]

split base n =
  ( n `div` base,
    n `div` base * base,
    n `mod` base
  )

ifZero :: Int -> a -> a -> a
ifZero 0 a _ = a
ifZero _ _ a = a

-- Converts an int to its textual representation, in French.
spellOutBase :: Int -> String
spellOutBase n
  | n < 100 && n `M.member` numbers = numbers ! n
  | n < 100 =
    let (_, t, rem) = split 10 n
     in numbers ! t ++ "-" ++ spellOutBase rem
  | otherwise = "FUCK!" ++ (show n)

spellOut' ((p, p', sep) : ps) n
  | p <= n =
    let (pow, _, rem) = split p n
     in spellOut pow ++ p' ++ ifZero rem "" (sep ++ spellOut rem)
  | otherwise = spellOut' ps n
spellOut' [] n = spellOutBase n

spellOut = spellOut' powers
