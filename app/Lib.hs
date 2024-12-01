module Lib where

import qualified Data.Map as M

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (== a)

counter :: Ord a => [a] -> M.Map a Int
counter = foldr (\a -> M.insertWith (+) a 1) M.empty
