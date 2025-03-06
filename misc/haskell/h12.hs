doubleFactorial :: Int -> Int
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = doubleFactorial (n - 2) * n

factorial n = go n 1
  where
    go n res
      | n > 1 = go (n - 1) (res * n)
      | otherwise = res

mult _ 0 = 0
mult m n = mult m (n - 1) + m

-- Using recursion to simulate a loop
multiply m n = go m n 0
  where
    go step counter res
      | counter == 0 = 0
      | counter > 1 = go step (counter - 1) (res + step)
      | otherwise = step + res

log2 :: Integer -> Integer
log2 1 = 0
log2 x = log2 (floor (toIntegral x / 2)) + 1
