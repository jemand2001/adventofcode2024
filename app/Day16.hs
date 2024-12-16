{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day16 where

import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import Debug.Trace
import GHC.Stack
import Matrix

day16 :: IO ()
day16 = interact $ show . uncurry3 part1 . (\m -> (toTile <$> m, findStart m, findEnd m)) . matrix . lines

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

data Tile = Empty | Wall deriving (Eq)

instance Show Tile where
  show Empty = "."
  show Wall = "#"

toTile :: Char -> Tile
toTile '#' = Wall
toTile _ = Empty

findStart, findEnd :: Matrix Char -> Index
findStart = fst . fromJust . find ((== 'S') . snd) . indicesWithValues
findEnd = fst . fromJust . find ((== 'E') . snd) . indicesWithValues

type Node = (Index, Index)

part1 :: Matrix Tile -> Index -> Index -> Int
part1 m start end =
  score $
    dijkstra
      end
      (M.singleton (start, right) 0)
      allNodes
  where
    allNodes =
      [ (fst v, d)
        | v <- indicesWithValues m,
          snd v /= Wall,
          d <- cardinals
      ]
    score :: (HasCallStack) => M.Map Node Int -> Int
    score dist = minimum $ map snd $ filter ((== end) . fst) $ map (first fst) $ M.toList dist

dijkstra :: Index -> M.Map Node Int -> [Node] -> M.Map Node Int
dijkstra _ dist [] = dist
dijkstra end !dist q
  | fst u == end = dist'
  | otherwise = dijkstra end dist' q'
  where
    u :: (HasCallStack) => Node
    u = minimumBy (comparing (\v -> fromMaybe maxBound $ M.lookup v dist)) q
    (p, d) = u
    q' :: (HasCallStack) => [Node]
    !q' = filter (/= u) q
    neighbors :: (HasCallStack) => [(Node, Int)]
    neighbors = filter ((`elem` q') . fst) [((p + d, d), 1), ((p, rotateRight d), 1000), ((p, rotateLeft d), 1000)]
    dist' :: (HasCallStack) => M.Map Node Int
    !dist' = foldr (\(n, c) -> M.insertWith min n (prior + c)) dist neighbors
    prior = fromJust $ M.lookup u dist
