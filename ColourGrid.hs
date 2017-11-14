module ColourGrid where
import Grid
import System.Random

type F = Double

data Point = P F F
instance CG Point where
  plus (P x y) (P x' y') = P (x+x') (y+y')
  inv (P x y) = P (-x) (-y)
  zero = P 0 0

type Angle = F

toPoint :: Angle -> Point
toPoint angle = P (cos angle) (sin angle)

toAngle :: Point -> Angle
toAngle (P x y) = if a<0 then a + 2*pi else a
  where a = atan2 y x

smoothOnce :: Int -> Grid Angle -> Grid Angle
smoothOnce r = (fmap toAngle) . (sumBox r) . (fmap toPoint)

iterations = 10
radius = 16

smoothGrid :: Grid Angle -> Grid Angle
smoothGrid initGrid = foldr (\i->smoothOnce radius) initGrid [1..iterations]

initState :: [Int] -> Int -> Grid Angle
initState ds seed = result
  where (result, r') = generateGrid (randomR (0,2*pi)) ds (mkStdGen seed)
