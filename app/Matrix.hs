module Matrix
  ( matrix,
    height,
    width,
    size,
    storage,
    (@),
    (@@),
    Index,
    x,
    y,
    (//),
    Matrix (),
    ray,
    rays,
    indices,
    validIndexFor,
    in2dRange,
    set,
    findIndices,
    up,
    down,
    left,
    right,
    build,
    mapWithIndex,
    indicesWithValues,
    cardinals,
    sameX,
    sameY,
    rotateRight,
    rotateLeft,
  )
where

import GHC.Stack (HasCallStack)

data Matrix a = Matrix {height :: Int, width :: Int, storage :: [[a]]}

data Index = I {x :: Int, y :: Int} deriving (Show, Eq)

matrix :: [[a]] -> Matrix a
matrix [] = Matrix 0 0 []
matrix l@(xs : xss)
  | all ((== w) . length) xss = Matrix (length l) w l
  | otherwise = error "jagged list!"
  where
    w = length xs

(@) :: (HasCallStack) => Matrix a -> Index -> a
Matrix h w s @ (I x' y')
  | x' < 0 || x' >= w = error $ "x index " ++ show x' ++ " out of bounds"
  | y' < 0 || y' >= h = error $ "y index " ++ show y' ++ " out of bounds"
  | otherwise = s !! y' !! x'

infixl 9 @

instance Functor Matrix where
  fmap f (Matrix h w s) = Matrix h w $ map (map f) s

instance Num Index where
  I x1 y1 + I x2 y2 = I (x1 + x2) (y1 + y2)
  I x1 y1 * I x2 y2 = I (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)
  I x1 y1 - I x2 y2 = I (x1 - x2) (y1 - y2)
  negate (I x_ y_) = I (-x_) (-y_)
  abs (I x_ y_) = I (abs x_) (abs y_)
  signum (I x_ y_) = I (signum x_) (signum y_)
  fromInteger i = I (fromInteger i) 0

ray :: Index -> Index -> [Index]
ray start direction = map ((start +) . (direction *) . fromInteger) [0 ..]

rays :: Index -> [[Index]]
rays i = [ray i (I dx dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]

size :: Matrix a -> Index
size (Matrix w h _) = w // h

(@@) :: (HasCallStack) => Matrix a -> [Index] -> [a]
m @@ is = map (m @) is

infixl 9 @@

indices :: Matrix a -> [Index]
indices m = [I x_ y_ | x_ <- [0 .. width m - 1], y_ <- [0 .. height m - 1]]

(//) :: Int -> Int -> Index
(//) = I

infixr 2 //

validIndexFor :: Matrix a -> Index -> Bool
validIndexFor m = in2dRange (0 // 0) (width m // height m)

in2dRange :: Index -> Index -> Index -> Bool
in2dRange (I l top) (I r bottom) (I x_ y_) = x_ >= l && y_ >= top && x_ < r && y_ < bottom

set :: Matrix a -> Index -> a -> Matrix a
set m@(Matrix w h s) i@(I x_ y_) v
  | not (validIndexFor m i) = error $ "index " ++ show i ++ " out of bounds for matrix of size " ++ show w ++ "x" ++ show h
  | otherwise = Matrix w h newStorage
  where
    newStorage = zipWith replaceRow [0 ..] s
    replaceRow y' l
      | y' /= y_ = l
      | otherwise = zipWith replaceCell [0 ..] l
    replaceCell x' v'
      | x' /= x_ = v'
      | otherwise = v

instance (Show a) => Show (Matrix a) where
  show = unlines . map (concatMap show) . storage

instance Ord Index where
  I x1 y1 <= I x2 y2 = (x1, y1) <= (x2, y2)

findIndices :: (Eq a) => a -> Matrix a -> [Index]
findIndices v m = filter ((== v) . (m @)) $ indices m

up :: Index
up = 0 // -1

down :: Index
down = 0 // 1

left :: Index
left = -1 // 0

right :: Index
right = 1 // 0

build :: Index -> (Index -> a) -> Matrix a
build (I w h) f = Matrix w h [[f (x_ // y_) | x_ <- [0 .. w - 1]] | y_ <- [0 .. h - 1]]

mapWithIndex :: (Index -> a -> b) -> Matrix a -> Matrix b
mapWithIndex f (Matrix w h s) = Matrix w h [[f (x_ // y_) i | (x_, i) <- zip [0 ..] row] | (y_, row) <- zip [0 ..] s]

indicesWithValues :: Matrix a -> [(Index, a)]
indicesWithValues (Matrix _ _ s) = [(x_ // y_, i) | (y_, row) <- zip [0 ..] s, (x_, i) <- zip [0 ..] row]

cardinals :: [Index]
cardinals = [up, down, left, right]

sameX :: Index -> Index -> Bool
sameX a b = x a == x b

sameY :: Index -> Index -> Bool
sameY a b = y a == y b

rotateRight, rotateLeft :: Index -> Index
rotateRight = (* (0 // -1))
rotateLeft = (* (0 // 1))
