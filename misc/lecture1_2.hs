--sqDist :: Num a => a -> a -> a
--sqDist x y = x ^ 2 + y ^ 2
sqDist :: Num a => (a, a) -> a
sqDist pt = (fst pt) ^ 2 + (snd pt) ^ 2

add :: Double -> Double -> Double
add x y = x + y

dist = sqrt . sqDist

dupl x = (x, x)
dist' = sqrt . sqDist . dupl

--main = print (sqDist 3 4)
main = do
  print $ sqDist (3, 4)
  --print $ dist' (3, 4)
  --print $ add 3 42
