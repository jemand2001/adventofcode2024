module Day12 (day12) where

import Data.List hiding (group)
import qualified Data.Set as S
import Data.Tuple (swap)
import Matrix
import Prelude hiding (Either (Left, Right))

type Blob = S.Set Index

day12 :: IO ()
day12 = interact $ show . part2 . matrix . lines

part1 :: Matrix Char -> Int
part1 m = sum $ map (\(_, g) -> length (perimeter g) * length g) $ areas m

areas :: (Eq a) => Matrix a -> [(a, Blob)]
areas = areas' . indicesWithValues

areas' :: (Eq a) => [(Index, a)] -> [(a, Blob)]
areas' = foldr group []
  where
    group :: (Eq a) => (Index, a) -> [(a, Blob)] -> [(a, Blob)]
    group (idx, a) l =
      hasNot ++ case has of
        [] -> [(a, S.singleton idx)]
        groups -> [(a, S.unions $ S.singleton idx : map snd groups)]
      where
        (has, hasNot) = partition (\(c, s) -> c == a && any ((`S.member` s) . (+ idx)) cardinals) l

perimeter :: Blob -> [Index]
perimeter s = concatMap (filter (not . (`S.member` s)) . (\i -> map (+ i) cardinals)) $ S.toList s

part2 :: Matrix Char -> Int
part2 m = sum $ map (\(_, g) -> sides g * length g) $ areas m

data Direction = Up | Down | Left | Right deriving (Eq, Ord, Enum, Show)

sides :: Blob -> Int
sides s = length $ ups ++ downs ++ lefts ++ rights
  where
    lines' = concatMap (filter (not . (`S.member` s) . snd) . (\i -> map ((+ i) <$>) cardinals')) $ S.toList s
    cardinals' = [(Up, up), (Down, down), (Left, left), (Right, right)]
    ups = areas' $ map swap $ filter ((== Up) . fst) lines'
    downs = areas' $ map swap $ filter ((== Down) . fst) lines'
    lefts = areas' $ map swap $ filter ((== Left) . fst) lines'
    rights = areas' $ map swap $ filter ((== Right) . fst) lines'