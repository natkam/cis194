module CardNumbers where

import Data.Char (digitToInt)

-- Convert an integer into a list of its digits (as integers).
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (toInteger . digitToInt) (show n)

-- Convert an integer into a list of its digits (as integers) reversed.
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Double every other number in the input list, starting from the right,
--   from the second-to-last element.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther input =
  reverse
    ( map
        (\(x, i) -> if even i then 2 * x else x)
        (zip (reverse input) [1 ..])
    )
