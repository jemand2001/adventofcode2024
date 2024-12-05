{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day5 (day5) where

import Data.List
import qualified Data.List.Predicate as P
import qualified Data.Text as T
import Lib

type Rule = (Int, Int)

type Update = [Int]

day5 :: IO ()
day5 = interact $ show . part2 . (\[a, b] -> (parseRules a, parseUpdates b)) . map T.lines . T.splitOn "\n\n" . T.pack

parseRules :: [T.Text] -> [Rule]
parseRules = map $ (\[a, b] -> (read a, read b)) . map T.unpack . T.splitOn "|"

parseUpdates :: [T.Text] -> [Update]
parseUpdates = map $ map (read . T.unpack) . T.splitOn ","

part1 :: ([Rule], [Update]) -> Int
part1 (rules, updates) = sum $ map middle $ filter satisfiesRules updates
  where
    satisfiesRules = P.sortedBy $ rulesToOrdering rules

rulesToOrdering :: [Rule] -> Int -> Int -> Ordering
rulesToOrdering rules a b
  | (a, b) `elem` rules = LT
  | (b, a) `elem` rules = GT
  | otherwise = EQ

part2 :: ([Rule], [Update]) -> Int
part2 (rules, updates) = sum $ map (middle . sortBy (rulesToOrdering rules)) $ filter (not . P.sortedBy (rulesToOrdering rules)) updates
