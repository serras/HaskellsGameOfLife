{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.MemoTrie
import Graphics.Gloss
import Logic
import System.Environment

main :: IO ()
main = do
  [file] <- getArgs
  initialGrid <- parseInitiaGrid <$> B.readFile file
  putStrLn "finished parsing"
  let f = gameOfLife initialGrid
  animate FullScreen white (gameOfLifePicture f)

dIMENSION :: Float
dIMENSION = 20

gameOfLifePicture :: (Integer -> Grid) -> Float -> Picture
gameOfLifePicture f (floor . (* 2) -> time)
  = pictures $ [ translate (fromInteger x * dIMENSION)
                           (fromInteger y * dIMENSION)
                           (color c (rectangleSolid dIMENSION dIMENSION))
               | x :: Integer <- [-50 .. 50]
               , y :: Integer <- [-50 .. 50]
               , let c = chooseColor (x, y) ]
               ++ [color green (text (show time))]
  where
    chooseColor p = case f time p of
                      Live -> red
                      Dead -> blue

parseInitiaGrid :: B.ByteString -> Grid
parseInitiaGrid s = memo go
  where
    ls = B.split '\n' s
    maxX = length ls
    go (fromInteger -> y, fromInteger -> x)
      | x < 0     = Dead
      | y < 0     = Dead
      | x >= maxX = Dead
      | y >= B.length (ls !! x)    = Dead
      | B.index (ls !! x) y == '.' = Dead
      | otherwise                  = Live