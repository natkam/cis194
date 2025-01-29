{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- ex. 1

fib :: Integer -> Integer
fib n
  | n < 2 = n
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0 ..]]

-- ex. 2

fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2 !! (n - 1) + fibs2 !! (n - 2) | n <- [2 ..]]

-- ex. 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- ex. 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- ex. 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 xs1) xs2 = Cons x1 (interleaveStreams xs2 xs1)

--TODO! (ex. 5.)
-- ruler :: Stream Integer
-- ruler = streamMap () (nats)

-- ex. 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (* (-1))
  (+) (Cons a0 as') (Cons b0 bs') = Cons (a0 + b0) (as' + bs')
  (*) (Cons a0 as') bs@(Cons b0 bs') = Cons (a0 * b0) (streamMap (* a0) bs' + as' * bs)

instance Fractional (Stream Integer) where
  (/) as@(Cons a0 as') bs@(Cons b0 bs') = Cons (a0 `div` b0) (streamMap (div b0) (as' - as / bs * bs'))
