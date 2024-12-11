{-# LANGUAGE TupleSections #-}

module Day11 (day11) where

import qualified Data.Map as M

day11 :: IO ()
day11 = interact $ show . sum . M.elems . part2 . M.fromListWith (+) . map ((,1) . read) . words

-- thank you to https://github.com/nullvoid8, without whom i would probably never have arrived here

type Stones = M.Map Int Int

part1 :: Stones -> Stones
part1 = simulate 25

part2 :: Stones -> Stones
part2 = simulate 75

simulate :: Int -> Stones -> Stones
simulate 0 l = l
simulate n l
  | n > 0 = simulate (n - 1) $ blink l
  | otherwise = error "negative number of simulation steps"

blink :: Stones -> Stones
blink = M.unionsWith (+) . fmap blink' . M.toList

blink' :: (Int, Int) -> Stones
blink' (s, c)
  | s == 0 = M.singleton 1 c
  | even len =
      if l == r
        then M.singleton l $ 2 * c
        else M.fromList [(l, c), (r, c)]
  | otherwise = M.singleton (s * 2024) c
  where
    str = show s
    len = length str
    halfLen = len `div` 2
    s' = dropWhile (== '0') $ drop halfLen str
    r = read $ if null s' then "0" else s'
    l = read $ take halfLen str
