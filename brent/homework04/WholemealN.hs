module WholemealN where

import Data.Function (fix)

{- Ex. 1: Wholemeal programming -}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- I'm not quite convinced that that was the intended way to rewrite it xD
fun2' :: Integer -> Integer
fun2' =
  fix
    ( \rec n ->
        if n == 1
          then 0
          else
            if even n
              then n + rec (n `div` 2)
              else rec (3 * n + 1)
    )
