module CardNumbers where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n = do
  if n <= 0
    then []
    else map (toInteger . digitToInt) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (map (\x -> if (even . snd) x then 2 * fst x else fst x) (zip (reverse n) [1 ..]))
