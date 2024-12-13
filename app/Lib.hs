{-# OPTIONS_GHC -Wno-type-defaults #-}
module Lib where

import qualified Data.Map as M

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (== a)

counter :: (Ord a) => [a] -> M.Map a Int
counter = foldr (\a -> M.insertWith (+) a 1) M.empty

pairwise' :: (a -> a -> b) -> [a] -> [b]
pairwise' f l = zipWith f l $ tail l

pairwise :: [a] -> [(a, a)]
pairwise = pairwise' (,)

pairwisePred :: (a -> a -> Bool) -> [a] -> Bool
pairwisePred p = and . pairwise' p

sections :: [a] -> [[a]]
sections [] = []
sections (x : xs) = xs : map (x :) (sections xs)

middle :: [a] -> a
middle l = l !! (length l `div` 2)

isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = round (10 ^ fromIntegral n * (x - fromIntegral (round x))) == 0
