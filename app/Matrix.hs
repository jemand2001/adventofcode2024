module Matrix (matrix, height, width, (@), (@@), Index, x, y, (//), Matrix (), ray, rays, indices, validIndexFor) where

data Matrix a = Matrix {height :: Int, width :: Int, storage :: [[a]]}

data Index = I {x :: Int, y :: Int} deriving (Show)

matrix :: [[a]] -> Matrix a
matrix [] = Matrix 0 0 []
matrix l@(xs : xss)
  | all ((== w) . length) xss = Matrix (length l) w l
  | otherwise = error "jagged list!"
  where
    w = length xs

(@) :: Matrix a -> Index -> a
Matrix h w s @ (I x' y')
  | x' < 0 || x' >= w = error $ "x index " ++ show x' ++ " out of bounds"
  | y' < 0 || y' >= h = error $ "y index " ++ show y' ++ " out of bounds"
  | otherwise = s !! y' !! x'

infixl 9 @

instance Num Index where
  I x1 y1 + I x2 y2 = I (x1 + x2) (y1 + y2)
  I x1 y1 * I x2 y2 = I (x1 * x2) (y1 * y2)
  I x1 y1 - I x2 y2 = I (x1 - x2) (y1 - y2)
  negate (I x_ y_) = I (-x_) (-y_)
  abs (I x_ y_) = I (abs x_) (abs y_)
  signum (I x_ y_) = I (signum x_) (signum y_)
  fromInteger i = I (fromInteger i) (fromInteger i)

ray :: Index -> Index -> [Index]
ray start direction = map ((start +) . (direction *) . fromInteger) [0 ..]

rays :: Index -> [[Index]]
rays i = [ray i (I dx dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]

(@@) :: Matrix a -> [Index] -> [a]
m @@ is = map (m @) is

indices :: Matrix a -> [Index]
indices m = [I x_ y_ | x_ <- [0 .. width m], y_ <- [0 .. height m]]

(//) :: Int -> Int -> Index
(//) = I

validIndexFor :: Matrix a -> Index -> Bool
validIndexFor m idx = x idx >= 0 && y idx >= 0 && x idx < width m && y idx < height m
