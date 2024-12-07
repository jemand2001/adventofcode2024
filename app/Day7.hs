{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day7 (day7) where

import Data.Char (isDigit)

day7 :: IO ()
day7 = interact $ show . part2 . map ((\(v : vs) -> (read (takeWhile isDigit v), map read vs)) . words) . lines

possibleValues :: [Int] -> [Int]
possibleValues [] = []
possibleValues [x] = [x]
possibleValues (x : y : xs) = possibleValues (x + y : xs) ++ possibleValues (x * y : xs)

part1 :: [(Int, [Int])] -> Int
part1 = sum . map fst . filter (uncurry hasSolution)
  where
    hasSolution x vs = x `elem` possibleValues vs

possibleValues' :: [Int] -> [Int]
possibleValues' [] = []
possibleValues' [x] = [x]
possibleValues' (x : y : xs) = possibleValues' (x + y : xs) ++ possibleValues' (x * y : xs) ++ possibleValues' (x ||| y : xs)
  where
    a ||| b = read $ show a ++ show b

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter (uncurry hasSolution)
  where
    hasSolution x vs = x `elem` possibleValues' vs
