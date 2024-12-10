module Day10 (day10) where

import Control.Monad
import Data.Char
import Data.List
import Matrix
import qualified Matrix as M

day10 :: IO ()
day10 = interact $ show . part1 . (digitToInt <$>) . matrix . lines

type Path = [Index]

part1 :: Matrix Int -> Int
part1 m = length $ concatMap (nubBy sameDestination . getPaths m) (filter ((== 0) . (m @)) $ indices m)
  where
    sameDestination p1 p2 = last p1 == last p2

part2 :: Matrix Int -> Int
part2 m = length $ concatMap (getPaths m) (filter ((== 0) . (m @)) $ indices m)

getPaths :: Matrix Int -> Index -> [[Index]]
getPaths m i = do
  d <- [M.up, M.down, M.left, M.right]
  let i' = i + d
  guard $ validIndexFor m i'
  guard $ m @ i' == currentValue + 1
  if currentValue == 8
    then
      return [i, i']
    else do
      path <- getPaths m i'
      return $ i : path
  where
    currentValue = m @ i
