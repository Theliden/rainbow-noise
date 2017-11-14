module Grid where

-- commutative group
class CG a where
  plus :: a -> a -> a
  inv :: a -> a
  zero :: a

data Grid a = Cell a | List Int [Grid a] deriving Show

instance Functor Grid where
  fmap f (Cell x) = Cell $ f x
  fmap f (List d l) = List d $ map (fmap f) l
  
makeGrid :: (CG a) => [Int] -> Grid a
makeGrid l = helper l $ length l
  where helper _ 0 = Cell zero
        helper (n:ns) d = List d [helper ns $ d-1 | i<-[1..n]]

sumGrid :: (CG a) => Grid a -> Grid a -> Grid a
sumGrid (Cell x) (Cell y) = Cell (plus x y)
sumGrid (List d l1) (List _ l2) = List d $ zipWith sumGrid l1 l2

prefixSum :: (CG a) => Grid a -> Grid a
prefixSum c@(Cell _) = c
prefixSum (List d l) = List d $ scanl1 sumGrid $ map prefixSum l

sumBox :: (CG a) => Int -> Grid a -> Grid a
sumBox _ c@(Cell _) = c
sumBox r (List d l) = let
  n = length l
  l' = map (sumBox r) l
  (lseg1,rseg1) = splitAt (n-r) l'
  (lseg2,rseg2) = splitAt (r+1) l'
  l1 = rseg1++lseg1
  l2 = rseg2++lseg2
  x = foldl1 sumGrid (rseg1++lseg2)
  helper [] [] _ = []
  helper (y:ys) (z:zs) x = x:(helper ys zs (sumGrid (sumGrid x z) (fmap inv y)))
  in List d (helper l1 l2 x)

generateGrid :: (b -> (a,b)) -> [Int] -> b -> (Grid a, b)
generateGrid g [] r = let (x,r') = g r in (Cell x, r')
generateGrid g (d:ds) r = let (l,r') = helper [1..d] [] r in (List d l, r')
  where helper [] acc r = (acc,r)
        helper (i:is) acc r = let (x, r') = generateGrid g ds r in
          helper is (x:acc) r'

flatten :: Grid a -> [a]
flatten (Cell x) = [x]
flatten (List _ l) = concat (map flatten l)
