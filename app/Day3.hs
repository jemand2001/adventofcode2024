module Day3 (day3) where

import Data.Char

day3 :: IO ()
day3 = interact $ show . part2 . getInstructions

part1 :: [Instruction] -> Int
part1 = sum . map toInt . filter isMul

data Instruction = Mul Int Int | Enable | Disable

isMul :: Instruction -> Bool
isMul (Mul _ _) = True
isMul _ = False

toInt :: Instruction -> Int
toInt (Mul a b) = a * b
toInt _ = undefined

part2 :: [Instruction] -> Int
part2 = walk True
  where
    walk _ [] = 0
    walk _ (Enable : xs) = walk True xs
    walk _ (Disable : xs) = walk False xs
    walk True (Mul a b : xs) = a * b + walk True xs
    walk False (Mul _ _ : xs) = walk False xs

getInstructions :: String -> [Instruction]
getInstructions [] = []
getInstructions ('m' : 'u' : 'l' : '(' : s) = case span isDigit s of
  ("", s') -> getInstructions s'
  (a, ',' : s') | length a <= 3 -> case span isDigit s' of
    ("", s'') -> getInstructions s''
    (b, ')' : s'') | length b <= 3 -> Mul (read a) (read b) : getInstructions s''
    (_, s'') -> getInstructions s''
  (_, s') -> getInstructions s'
getInstructions ('d' : 'o' : '(' : ')' : s) = Enable : getInstructions s
getInstructions ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : s) = Disable : getInstructions s
getInstructions (_ : s) = getInstructions s
