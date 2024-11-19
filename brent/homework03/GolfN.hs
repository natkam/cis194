module Golf where

import Data.List (group, nub, sort, transpose)
-- Run `cabal update` and `cabal install --lib MissingH` to install Utils!
import Data.List.Utils (countElem, join)

{- Ex. 1: Hopscotch -}
skips :: [a] -> [[a]]
skips xs = concatMap (getEveryNth xs) [1 .. (length xs)]

getEveryNth :: [a] -> Int -> [[a]]
getEveryNth xs n =
  [ (map snd . filter (\(x, _) -> mod x n == 0) . zip [1 ..]) xs
  ]

skips' :: [a] -> [[a]]
skips' xs =
  [ [y | (x, y) <- zip [1 ..] xs, mod x n == 0]
    | n <- [1 .. (length xs)]
  ]

-- Version using the list index !! operator, with a very suboptimal heuristic
-- regarding the indices values (in order to avoid raising an exception).
skips'' :: [a] -> [[a]]
skips'' xs =
  [ [xs !! (n * i - 1) | i <- [1 .. length xs], n * i <= length xs]
    | n <- [1 .. length xs]
  ]

{- Ex. 2: Local Maxima -}
localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  [ xs !! i
    | i <- [1 .. length xs - 2],
      xs !! (i - 1) < xs !! i && xs !! (i + 1) < xs !! i
  ]

{- Ex. 3: Histogram -}

histogram :: [Integer] -> String
-- would actually be shorter with "0123456789" and "=========="...
histogram xs =
  join
    "\n"
    (getRows xs ++ [replicate 10 '=', concatMap show [0 .. 9], ""])

getRows :: [Integer] -> [String]
getRows xs =
  transpose
    [ replicate ((maximum . count) xs - freq) ' ' ++ replicate freq '*'
      | freq <- count xs
    ]

count :: [Integer] -> [Int]
count xs = [countElem x xs | x <- [0 .. 9]]

-- A previous attempt at getting the frequency of figures in the list
--count' :: [Integer] -> [(Integer, Int)]
--count' xs = zip ((sort . nub) xs) (map length ((group . sort) xs))
