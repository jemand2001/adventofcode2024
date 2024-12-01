{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day1 (day1) where

import Lib
import Data.List

day1 :: IO ()
day1 = interact $ show . part2 . unzip . map ((\[x, y] -> (read x, read y)) . words) . lines

part1 :: ([Int], [Int]) -> Int
part1 (l, r) = sum $ map abs $ zipWith (-) l' r'
  where
    l' = sort l
    r' = sort r

part2 :: ([Int], [Int]) -> Int
part2 (l, r) = sum $ map similarity l
  where
    similarity x = x * count x r
