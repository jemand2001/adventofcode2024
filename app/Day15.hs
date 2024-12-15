{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day15 (day15) where

import Data.Bifunctor
import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Matrix

uncurry3' :: (a -> b -> c -> d) -> ((a, b), c) -> d
uncurry3' f ((a, b), c) = f a b c

parseMap :: String -> ((Matrix Tile, Index), [Index])
parseMap = bimap ((\m -> (toTile <$> m, findRobot m)) . matrix . lines . T.unpack) (map toDirection . filter (not . C.isSpace) . T.unpack) . T.breakOn "\n\n" . T.pack

day15 :: IO ()
day15 = interact $ show . sum . map gpsCoordinate . getBoxes . uncurry3' part2 . parseMap

data Tile = Empty | Box | Wall deriving (Eq)

instance Show Tile where
  show Empty = "."
  show Box = "O"
  show Wall = "#"

gpsCoordinate :: Index -> Int
gpsCoordinate idx = (100 * y idx) + x idx

getBoxes :: (IsTile a, Eq a) => Matrix a -> [Index]
getBoxes = map fst . filter (\(_, v) -> v == boxLeft) . indicesWithValues

class IsTile a where
  boxLeft :: a
  isBox :: a -> Bool
  isWall :: a -> Bool

instance IsTile Tile where
  boxLeft = Box
  isBox = (== Box)
  isWall = (== Wall)

toTile :: Char -> Tile
toTile '#' = Wall
toTile 'O' = Box
toTile _ = Empty

toDirection :: Char -> Index
toDirection '^' = up
toDirection '<' = left
toDirection '>' = right
toDirection 'v' = down
toDirection _ = undefined

findRobot :: Matrix Char -> Index
findRobot = head . map fst . filter (\(_, v) -> v == '@') . indicesWithValues

part1 :: Matrix Tile -> Index -> [Index] -> Matrix Tile
part1 m _ [] = m
part1 m r (d : ds) = case firstSpace of
  Wall -> part1 m r ds
  Empty -> part1 pushed r' ds
  _ -> undefined
  where
    columnIndices = drop 1 $ ray r d
    column = m @@ columnIndices
    firstSpaceIndex = fromJust $ L.findIndex (not . isBox) column
    firstSpace = column !! firstSpaceIndex
    boxes = take firstSpaceIndex columnIndices
    boxes' = map (+ d) boxes
    r' = r + d
    pushed = build (size m) $ \idx ->
      if idx == r'
        then Empty
        else
          if idx `notElem` boxes'
            then m @ idx
            else Box

data Tile' = Empty' | BoxLeft | BoxRight | Wall' deriving (Eq)

instance IsTile Tile' where
  boxLeft = BoxLeft
  isBox a = a == BoxLeft || a == BoxRight
  isWall = (== Wall')

widen :: Matrix Tile -> Matrix Tile'
widen = matrix . map widen' . storage
  where
    widen' = concatMap widenTile
    widenTile Wall = [Wall', Wall']
    widenTile Box = [BoxLeft, BoxRight]
    widenTile Empty = [Empty', Empty']

instance Show Tile' where
  show Empty' = "."
  show BoxLeft = "["
  show BoxRight = "]"
  show Wall' = "#"

part2 :: Matrix Tile -> Index -> [Index] -> Matrix Tile'
part2 m' r = part2' (widen m') (r {x = x r * 2})
  where
    part2' :: Matrix Tile' -> Index -> [Index] -> Matrix Tile'
    part2' m _ [] = m
    part2' m r (d : ds) = if wallHit then part2' m r ds else part2' pushed r' ds
      where
        r' = r + d
        boxes = getBoxes S.empty r'
        getBoxes acc idx =
          case m @ idx of
            Empty' -> acc
            Wall' -> acc
            BoxLeft -> S.unions [if rightHalf `S.member` acc then S.empty else getBoxes acc' rightHalf, getBoxes acc' $ idx + d]
            BoxRight -> S.unions [if leftHalf `S.member` acc then S.empty else getBoxes acc' leftHalf, getBoxes acc' $ idx + d]
          where
            rightHalf = idx + right
            leftHalf = idx + left
            acc' = S.insert idx acc
        wallHit = any (\t -> not (validIndexFor m t) || isWall (m @ (t + d))) $ S.insert r boxes
        boxes' = S.map (+ d) boxes
        pushed = mapWithIndex getTile m
        getTile idx original
          | idx == r' = Empty'
          | idx `elem` boxes && idx `notElem` boxes' = Empty'
          | idx `notElem` boxes' = original
          | otherwise = m @ (idx - d)
