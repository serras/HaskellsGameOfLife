{-# language FlexibleContexts #-}
module Logic where

import Data.MemoTrie

-- | A point in the grid
type Point = (Integer, Integer)
-- | The status of a cell
data Cell = Live | Dead deriving Eq
-- | Description of a grid
type Grid = Point -> Cell

nextStep :: Cell -> [Cell] -> Cell
nextStep Live adj
  | count Live adj < 2  = Dead  -- underpopulation
  | count Live adj > 3  = Dead  -- overpopulation
  | otherwise           = Live  -- live and let live
nextStep Dead adj
  | count Live adj == 3 = Live  -- reproduction
  | otherwise           = Dead  -- nothing happens
  

gameOfLifeSuperSlow :: Grid -> Integer -> Grid
gameOfLifeSuperSlow initial 0 p
  = initial p
gameOfLifeSuperSlow initial n p
  = nextStep (gameOfLifeSuperSlow initial (n-1) p)
             (map (gameOfLifeSuperSlow initial (n-1)) (adjacents p))

gameOfLifeSlow :: Grid -> Integer -> Grid
gameOfLifeSlow initial = memo2 go
  where
    go 0 p
      = initial p
    go n p
      = nextStep (go (n-1) p) (map (go (n-1)) (adjacents p))

gameOfLife :: Grid -> Integer -> Grid
gameOfLife initial = curry (memoFix go)
  where
    go _me (0, p)
      = initial p
    go me (n, p)
      = nextStep (me (n-1, p)) (map (\x -> me (n-1, x)) (adjacents p))

adjacents :: Point -> [Point]
adjacents (x,y)
  = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0)]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

