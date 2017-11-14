module Main where

import System.Environment

import Grid
import ColourGrid
import Conversions
import PPMOutput

makeImage :: Int -> Int -> Int -> IO ()
makeImage height width seed = let
  g = smoothGrid $ initState [height, width] seed
  w = concat $ map (toWord . rainbow) (flatten g)
  in output height width w

main = do
  args <- getArgs
  let [height, width, seed] = map read args in
    makeImage height width seed
