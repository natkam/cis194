module CardNumbers where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n = do
  if n <= 0
    then []
    else map toInteger $ map digitToInt $ show n
