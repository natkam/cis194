fib :: Integer -> Integer
fib n
  | n < 2 = n
  | otherwise = fib (n -1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0 ..]]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2 !! (n - 1) + fibs2 !! (n - 2) | n <- [2 ..]]

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show xs = show $ take 20 (streamToList xs)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))
