{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day13 (day13) where

import Data.Bifunctor
import Data.Char
import Data.Either
import qualified Data.Text as T
import Lib
import Math.MFSolve
import Matrix hiding (left)

day13 :: IO ()
day13 = interact $ show . part2 . map (parseClawMachine . T.unpack) . T.splitOn "\n\n" . T.pack

data ClawMachine = CM {a :: Index, b :: Index, prize :: Index} deriving (Show, Eq)

parseClawMachine :: String -> ClawMachine
parseClawMachine s =
  let (_, axRest) = break isDigit s
      (axString, rest1) = span isDigit axRest
      (_, ayRest) = break isDigit rest1
      (ayString, rest2) = span isDigit ayRest
      (_, bxRest) = break isDigit rest2
      (bxString, rest3) = span isDigit bxRest
      (_, byRest) = break isDigit rest3
      (byString, rest4) = span isDigit byRest
      (_, pxRest) = break isDigit rest4
      (pxString, rest5) = span isDigit pxRest
      (_, pyRest) = break isDigit rest5
      (pyString, _) = span isDigit pyRest
   in CM (read axString // read ayString) (read bxString // read byString) (read pxString // read pyString)

part1 :: [ClawMachine] -> Integer
part1 = sum . map solveClawMachine

part2 :: [ClawMachine] -> Integer
part2 = sum . map (\m -> solveClawMachine m {prize = prize m + (10000000000000 // 10000000000000)})

solveClawMachine :: ClawMachine -> Integer
solveClawMachine (CM a' b' prize') =
  let [aCount, bCount] = map SimpleVar ["a", "b"]
      [aCount', bCount'] :: [Expr SimpleVar Double] = map makeVariable [aCount, bCount]
      solution = flip execSolver noDeps $ do
        aCount' * fromIntegral (x a') + bCount' * fromIntegral (x b') === fromIntegral (x prize')
        aCount' * fromIntegral (y a') + bCount' * fromIntegral (y b') === fromIntegral (y prize')
   in fromRight 0 $ do
        values <- solution
        aValue <- first (const $ UndefinedVar aCount) $ getKnown aCount values
        bValue <- first (const $ UndefinedVar bCount) $ getKnown bCount values
        -- i sincerely wish i could just make MFSolve only return integer values, and i have no idea if this number will work for every input lol
        if isInt aValue 2 && isInt bValue 2
          then
            return $ round aValue * 3 + round bValue
          else return 0
