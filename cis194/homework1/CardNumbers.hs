module CardNumbers where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (toInteger . digitToInt) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n =
  reverse
    ( map
        (\(x, y) -> if even y then 2 * x else x)
        (zip (reverse n) [1 ..])
    )
