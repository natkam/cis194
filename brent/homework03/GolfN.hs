module Golf where

import Data.List (group, nub, sort, transpose)

--import Data.List.Unique (count)
--import Data.Map (fromListWith, toList)

skips :: [a] -> [[a]]
skips xs = concatMap (getEveryNth xs) [1 .. (length xs)]

getEveryNth :: [a] -> Int -> [[a]]
getEveryNth xs n = [(map snd . filter (\(x, y) -> mod x n == 0) . zip [1 ..]) xs]

-- This function's body is pasted into the list comp in `skips'`. I'm
-- leaving it here for the sake of clarity; it does exactly the same as
-- `getEveryNth`, but I find the nested list comprehension unreadable.
getEveryNth' :: [a] -> Int -> [a]
getEveryNth' xs n = concat [[y] | (x, y) <- zip [1 ..] xs, mod x n == 0]

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

localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  [ xs !! i
    | i <- [1 .. length xs - 2],
      xs !! (i - 1) < xs !! i && xs !! (i + 1) < xs !! i
  ]

histogram :: [Integer] -> String
histogram xs = foldr1 (\c acc -> acc ++ "\n" ++ c) (getLines xs)

--count' :: [Integer] -> [(Integer, Int)]
--count' xs = zip ((sort . nub) xs) (map length ((group . sort) xs))

--count'' :: [Integer] -> [(Integer, Int)]
--count'' xs = toList (fromListWith (+) [(x, 1) | x <- xs])

--count' xs = count xs

getLines :: [Integer] -> [String]
getLines xs = concatMap show [0 .. 9] : replicate 10 '=' : (transpose . getCols) xs

-- Liczenie zerżnięte ze SO https://stackoverflow.com/q/19554984/4744341
--cnt :: Eq a => a -> [a] -> Int
--cnt x = length . filter (== x)

count :: [Integer] -> [(Integer, Int)]
count xs = [(x, (length . filter (== x)) xs) | x <- [0 .. 9]]

-- A helper function to get the max number of occurrences of any number
maxCount :: [Integer] -> Int
maxCount xs = maximum (map length ((group . sort) xs))

getCols :: [Integer] -> [String]
getCols xs = [replicate (snd pair) char ++ replicate (maxCount xs - snd pair) ' ' | pair <- count xs, char <- if snd pair == 0 then " " else "*"]
