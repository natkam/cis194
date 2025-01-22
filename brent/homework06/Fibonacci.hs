fib :: Integer -> Integer
fib n
  | n < 2 = n
  | otherwise = fib (n -1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0 ..]]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2 !! (n - 1) + fibs2 !! (n - 2) | n <- [2 ..]]
