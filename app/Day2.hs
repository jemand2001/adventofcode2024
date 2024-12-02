module Day2 (day2) where

day2 :: IO ()
day2 = interact $ show . part1 . map (map read . words) . lines

part1 :: [[Int]] -> Int
part1 = length . filter (\l -> allSafeDifferences l && (allIncreasing l || allDecreasing l))
  where
    allSafeDifferences [] = True
    allSafeDifferences [_] = True
    allSafeDifferences (x : y : xs) = diff > 0 && diff <= 3 && allSafeDifferences (y : xs)
      where
        diff = abs (x - y)
    allIncreasing [] = True
    allIncreasing [_] = True
    allIncreasing (x : y : xs) = x < y && allIncreasing (y : xs)

    allDecreasing [] = True
    allDecreasing [_] = True
    allDecreasing (x : y : xs) = x > y && allDecreasing (y : xs)
