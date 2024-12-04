module Day4 (day4) where

import Data.List
import Matrix
import Lib

day4 :: IO ()
day4 = interact $ (++ "\n") . show . part2 . matrix . lines

part1 :: Matrix Char -> Int
part1 m = sum $ map (countOccurrences "XMAS" m) $ indices m

countOccurrences :: (Eq a) => [a] -> Matrix a -> Index -> Int
countOccurrences s m i = length $ filter (isPrefixOf s . (m @@) . takeWhile inBounds) $ rays i
  where
    inBounds idx = x idx >= 0 && y idx >= 0 && x idx < width m && y idx < height m

part2 :: Matrix Char -> Int
part2 m = length $ filter (isXMAS m) $ indices m

isXMAS :: Matrix Char -> Index -> Bool
isXMAS m idx
  | x idx < 1 || y idx < 1 || x idx >= width m - 1 || y idx >= height m - 1 = False
  | m @ idx /= 'A' = False
  | otherwise = count "MAS" (map ((m @@) . map (idx +)) diagonals) == 2
  where
    diag1 = [I (-1) (-1), I 0 0, I 1 1]
    diag2 = [I (-1) 1, I 0 0, I 1 (-1)]
    diagonals = [diag1, diag2, reverse diag1, reverse diag2]
