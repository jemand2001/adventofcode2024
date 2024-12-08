module Day8 (day8) where

import Control.Monad
import qualified Data.Set as S
import Matrix

day8 :: IO ()
day8 = interact $ show . part2 . parseMap . lines

data Block = Empty | Antenna Char deriving (Eq)

fromChar :: Char -> Block
fromChar '.' = Empty
fromChar c = Antenna c

parseMap :: [String] -> Matrix Block
parseMap = (fromChar <$>) . matrix


alphaNumerics :: [Char]
alphaNumerics = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

part1 :: Matrix Block -> Int
part1 m = S.size $ foldr (S.union . findAntinodes) S.empty alphaNumerics
  where
    findAntinodes :: Char -> S.Set Index
    findAntinodes c = S.fromList $ do
      let antennaIndices = findIndices (Antenna c) m
      a <- antennaIndices
      b <- antennaIndices
      guard $ a /= b
      let diff = a - b
      filter (validIndexFor m) [a + diff, b - diff]

part2 :: Matrix Block -> Int
part2 m = S.size $ foldr (S.union . findAntinodes) S.empty alphaNumerics
  where
    findAntinodes :: Char -> S.Set Index
    findAntinodes c = S.fromList $ do
      let antennaIndices = findIndices (Antenna c) m
      a <- antennaIndices
      b <- antennaIndices
      guard $ a /= b
      let diff = a - b
      takeWhile (validIndexFor m) (ray a (-diff)) ++ takeWhile (validIndexFor m) (ray a diff)
