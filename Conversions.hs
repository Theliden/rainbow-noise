module Conversions where

import Data.Word

type F = Double

theta :: F
theta = atan2 1 1

phi :: F
phi = atan2 (sqrt 2) 1

x :: F
x = 1/(sqrt 3)

r :: F
r = 0.5

-- angle to point
rainbow :: F -> (F, F, F)
rainbow eta = let
  x' = (cos eta) * (cos phi) * (cos theta) - (sin eta) * (sin theta)
  y' = (cos eta) * (cos phi) * (sin theta) + (sin eta) * (cos theta)
  z' = - (cos eta) * (sin phi)
  in (x + r * x', x + r * y', x + r * z')

toDiscrete :: F -> Word8
toDiscrete x = let
  i = floor (256 * x)
  i' = if i == 256 then 255 else i
  in fromIntegral i'

toWord :: (F, F, F) -> [Word8]
toWord (x, y, z) = map toDiscrete [x, y, z]
