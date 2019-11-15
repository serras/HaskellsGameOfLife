{-# language FlexibleContexts #-}
module Logic where

import Data.MemoTrie

-- | A point in the grid
type Point = (Integer, Integer)
-- | The status of a cell
data Cell = Live | Dead deriving Eq
-- | Description of a grid
type Grid = Point -> Cell

cUTOFF :: Integer
cUTOFF = 200

gameOfLife :: Grid -> Integer -> Grid
gameOfLife initial = curry (memoFix go)
  where
    go _me (0, p)
      = initial p
    go me (n, p)
      = let adj = map (\x -> me (n-1, x)) (adjacents p)
        in case me (n-1, p) of
            Live | count Live adj < 2  -> Dead  -- underpopulation
                 | count Live adj > 3  -> Dead  -- overpopulation
                 | otherwise           -> Live  -- live and let live
            Dead | count Live adj == 3 -> Live  -- reproduction
                 | otherwise           -> Dead  -- nothing happens
    

adjacents :: Point -> [Point]
adjacents (x,y)
  = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0)]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

