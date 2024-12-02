module Day2 (day2) where

import Lib

day2 :: IO ()
day2 = interact $ (++ "\n") . show . part2 . map (map read . words) . lines

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

part2 :: [[Int]] -> Int
part2 = length . filter isSafeWithRemoval

isSafe :: [Int] -> Bool
isSafe l = allSafeDifferences l && (allIncreasing l || allDecreasing l)
  where
    allSafeDifferences = pairwisePred safeDistance
      where
        safeDistance a b = diff > 0 && diff <= 3
          where diff = abs $ a - b

    allIncreasing = pairwisePred (<)
    allDecreasing = pairwisePred (>)

isSafeWithRemoval :: [Int] -> Bool
isSafeWithRemoval l
  | isSafe l = True
  | any isSafe $ sections l = True
  | otherwise = False
