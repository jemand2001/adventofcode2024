{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day9 (day9) where

import Control.Monad (forM)
import Data.List (findIndex)
import Data.Maybe
import Debug.Trace
import GHC.Stack

day9 :: IO ()
day9 = interact $ show . checksum . part2 . readDisk

type Block = Maybe Integer

type Disk = [Block]

zero :: Int
zero = fromEnum '0'

checksum :: Disk -> Integer
checksum = sum . map (uncurry (*) . fromMaybe (0, 0)) . zipWith (\a b -> (a,) <$> b) [0 ..]

readDisk :: String -> Disk
readDisk = readFile 0
  where
    readFile _ [] = []
    readFile i (x : xs) = replicate (fromEnum x - zero) (Just i) ++ readEmpty (succ i) xs

    readEmpty _ [] = []
    readEmpty i (x : xs) = replicate (fromEnum x - zero) Nothing ++ readFile i xs

part1 :: Disk -> Disk
part1 d = compact d $ reverse d
  where
    compact :: (HasCallStack) => Disk -> Disk -> Disk
    compact [] _ = error "this cannot happen"
    compact (x@(Just i) : xs) ys@((Just i') : _)
      | i == i' = takeWhile (== Just i) ys
      | otherwise = x : compact xs ys
    compact xs (Nothing : ys) = compact xs ys
    compact (Nothing : xs) ((Just i) : ys) = Just i : compact xs ys
    compact _ [] = error "this cannot happen"

data Block' = File Integer Int | Empty Int deriving Show

type CompactDisk = [Block']

fileId :: Block' -> Maybe Integer
fileId (File i _) = Just i
fileId (Empty _) = Nothing

blockSize :: Block' -> Int
blockSize (File _ l) = l
blockSize (Empty l) = l

part2 :: Disk -> Disk
part2 = expand . (\d -> compact (maximum $ fromMaybe 0 <$> d) $ rle d)
  where
    rle :: Disk -> CompactDisk
    rle [] = []
    rle xs@(x@(Just f) : _) = File f (length file) : rle rest
      where
        (file, rest) = span (== x) xs
    rle xs@(Nothing : _) = Empty (length empty) : rle rest
      where
        (empty, rest) = span isNothing xs

    expand :: CompactDisk -> Disk
    expand = concatMap expand'
      where
        expand' (Empty l) = replicate l Nothing
        expand' (File f l) = replicate l (Just f)

    enoughFreeSpace :: Int -> Block' -> Bool
    enoughFreeSpace _ (File _ _) = False
    enoughFreeSpace l (Empty l') = l <= l'

    consolidateFreeSpace :: CompactDisk -> CompactDisk
    consolidateFreeSpace [] = []
    consolidateFreeSpace (Empty l : Empty l' : xs) = consolidateFreeSpace $ Empty (l + l') : xs
    consolidateFreeSpace (x : xs) = x : consolidateFreeSpace xs

    compact :: (HasCallStack) => Integer -> CompactDisk -> CompactDisk
    compact 0 cd = cd
    compact i cd = if isNothing spaceIndex then compact (i - 1) cd else compact (i - 1) $ consolidateFreeSpace $ start' ++ [file, Empty (emptySize - fileSize)] ++ startRest ++ rest'
      where
        fileIndex = findIndex ((== Just i) . fileId) cd
        (start, file@(File _ fileSize) : rest) = splitAt (fromJust fileIndex) cd
        rest' = Empty fileSize : rest
        spaceIndex = findIndex (enoughFreeSpace fileSize) start
        (start', (Empty emptySize) : startRest) = splitAt (fromJust spaceIndex) start
