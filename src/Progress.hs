{-|
Module      : Progress
Description : Progress bars and other indicators for patient people.
Copyright   : © 2021 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental
-}

module Progress where

import Data.Ratio
import Data.Semigroup (stimesMonoid)

data BarConfig = BarConfig {
  barLength :: Int,
  barChars  :: [String]
  }

defaultConfig = BarConfig {
  barLength= 50,
  barChars = quarterBlocks
                          }

block = [" ", "█"]
halfBlocks = [ " ", "▌", "▐"]
quarterBlocks = [ " ", "▎", "▌", "▊", "█" ]

toRatio ::  (Integral a) => a -> Ratio a
toRatio a = a % 1

fromRatio :: Ratio Int -> Float
fromRatio r = (fromIntegral . numerator $ r) /
  (fromIntegral . denominator $ r)

render :: BarConfig -> Ratio Int -> String
render c r =
  let len = barLength c
      left = floor $ r * fromIntegral (barLength c) :: Int
      right = barLength c - left
      charsCount = length (barChars c)
      diff = ((r - (left % len)) * toRatio len)
      extra = (barChars c) !! (floor . fromRatio) (diff * (toRatio charsCount))
  in stimesMonoid left "█" ++ extra ++ stimesMonoid right " " ++ show diff ++ "\n"
