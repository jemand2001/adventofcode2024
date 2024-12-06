{-# LANGUAGE TupleSections #-}

module Day6 (day6) where

import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Matrix

day6 :: IO ()
day6 = interact $ show . part2 . (\m -> (getMap m, getGuard m)) . matrix . lines

data Tile = Free | Obstruction deriving (Eq)

instance Show Tile where
  show Free = "."
  show Obstruction = "#"

type Guard = (Index, Index)

getMap :: Matrix Char -> Matrix Tile
getMap = fmap toTile
  where
    toTile '#' = Obstruction
    toTile _ = Free

getGuard :: Matrix Char -> Guard
getGuard m = fromJust $ ((charToDirection <$>) <$>) $ find (\(_, c) -> c /= '.' && c /= '#') $ map (\i -> (i, m @ i)) $ indices m
  where
    charToDirection '^' = 0 // -1
    charToDirection '>' = 1 // 0
    charToDirection '<' = (-1) // 0
    charToDirection 'v' = 0 // 1
    charToDirection c = error $ "undefined direction: " ++ [c]

turnRight :: Index -> Index
turnRight i = case (x i, y i) of
  (0, 1) -> (-1) // 0
  (0, -1) -> 1 // 0
  (1, 0) -> 0 // 1
  (-1, 0) -> 0 // -1
  _ -> error $ "bad direction index: " ++ show i

walkInDirection :: Matrix Tile -> Guard -> ([Index], Bool)
walkInDirection m g@(_, d) = (\l -> (l, validIndexFor m $ (d +) $ last l)) $ map fst $ takeWhile (\(i, t) -> validIndexFor m i && t /= Obstruction) $ map (\i -> (i, m @ i)) $ uncurry ray g

part1 :: (Matrix Tile, Guard) -> Int
part1 = length . nub . map fst . uncurry findPath

findPath :: Matrix Tile -> Guard -> [Guard]
findPath m g = go g [g]
  where
    go :: Guard -> [Guard] -> [Guard]
    go guard@(_, d) visited =
      let (visited', inBounds) = walkInDirection m guard
       in if not inBounds
            then
              visited ++ map (,d) visited'
            else
              go (last visited', turnRight d) $ visited ++ map (,d) visited'

lastInDirection :: Matrix Tile -> Guard -> (Index, Bool)
lastInDirection m g@(_, d) = (\l -> (l, validIndexFor m $ d + l)) $ last $ map fst $ takeWhile (\(i, t) -> validIndexFor m i && t /= Obstruction) $ map (\i -> (i, m @ i)) $ uncurry ray g

hasLoop :: Matrix Tile -> Guard -> Bool
hasLoop m g = go g []
  where
    go :: Guard -> [Guard] -> Bool
    go guard@(_, d) visited =
      let (visited', inBounds) = lastInDirection m guard
          visited'' = ((visited', d) : visited)
       in (inBounds && (((visited', d) `elem` visited) || go (visited', turnRight d) visited''))

part2 :: (Matrix Tile, Guard) -> Int
part2 (m, g) = length $ filter (\i -> hasLoop (set m i Obstruction) g) options
  where
    options :: [Index]
    options = sortBy (comparing y) $ sortBy (comparing x) $ nub $ filter (\c -> validIndexFor m c && m @ c /= Obstruction && c /= fst g) $ map (uncurry (+)) $ findPath m g
