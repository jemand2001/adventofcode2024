{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day7 (day7) where

import Data.Char (isDigit)

day7 :: IO ()
day7 = interact $ show . part2 . map ((\(v : vs) -> (read (takeWhile isDigit v), map read vs)) . words) . lines

type Operator = Int -> Int -> Int

possibleValues :: [Operator] -> [Int] -> [Int]
possibleValues _ [] = []
possibleValues _ [x] = [x]
possibleValues ops (x : y : xs) = concatMap (possibleValues ops . (\op -> op x y : xs)) ops

part1 :: [(Int, [Int])] -> Int
part1 = sum . map fst . filter (uncurry hasSolution)
  where
    hasSolution x vs = x `elem` possibleValues [(+), (*)] vs

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter (uncurry hasSolution)
  where
    hasSolution x vs = x `elem` possibleValues [(+), (*), (|||)] vs
    a ||| b = read $ show a ++ show b
