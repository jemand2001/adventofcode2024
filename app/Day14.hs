{-# LANGUAGE OverloadedStrings #-}

module Day14 (day14) where

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Text as T
import Matrix

day14 :: IO ()
day14 = do
  robots <- map parseRobot . T.lines . T.pack <$> getContents
  part2 robots

data Robot = R {position :: Index, velocity :: Index} deriving (Show, Eq)

gridSize :: Index
gridSize = 101 // 103

parseRobot :: T.Text -> Robot
parseRobot line = R (parseIndex pos) (parseIndex v')
  where
    (pos, v) = T.breakOn " " line
    v' = T.tail v
    parseIndex s =
      let s' = T.drop 2 s
          (x_, s'') = T.breakOn "," s'
          y_ = T.drop 1 s''
       in read (T.unpack x_) // read (T.unpack y_)

part1 :: [Robot] -> IO ()
part1 = print . product . countQuadrants . M.toList . M.fromListWith (+) . map (\r -> (position $ simulate 100 r, 1))
  where
    countQuadrants :: [(Index, Int)] -> [Int]
    countQuadrants m = map (\(topLeft, bottomRight) -> sum $ map snd $ filter (in2dRange topLeft bottomRight . fst) m) quadrants

simulate :: Int -> Robot -> Robot
simulate s (R p v) = R (limit $ p + fromIntegral s * v) v
  where
    limit idx = (x idx `mod` x gridSize) // (y idx `mod` y gridSize)

quadrants :: [(Index, Index)]
quadrants =
  [ (0 // 0, halfWay),
    (x halfWay + 1 // 0, x gridSize // y halfWay),
    (0 // y halfWay + 1, x halfWay // y gridSize),
    (halfWay + (1 // 1), gridSize)
  ]
  where
    halfWay = x gridSize `div` 2 // y gridSize `div` 2

data Occupation = Free | Filled

instance Show Occupation where
  show Free = " "
  show Filled = "*"

-- prints 10k iterations of the simulation along with the iteration number.
-- the wanted iteration contains the tree surrounded by a frame.
-- search the output for the string "*******************************"
part2 :: [Robot] -> IO ()
part2 rs = simulateUntil 0 rs rs
  where
    simulateUntil :: Int -> [Robot] -> [Robot] -> IO ()
    simulateUntil it current original = do
      let next = map (simulate 1) current
      guard $ it <= 10000
      print $ it + 1
      print $ build gridSize (\i -> if i `elem` map position next then Filled else Free)
      simulateUntil (it + 1) next original
