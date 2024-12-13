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

-- TODO: Figure this out.

{- Ex. 2: Folding with trees -}
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert newObj Leaf = Node 0 Leaf newObj Leaf
insert newObj (Node n Leaf obj tr) = Node (n + 1) (insert newObj Leaf) obj tr
insert newObj (Node n tl obj Leaf) = Node n tl obj (insert newObj Leaf)
insert
  newObj
  node@(Node n tl@(Node n' tl' obj' tr') obj tr@(Node n'' tl'' obj'' tr''))
    | n' > n'' = Node n tl obj (insert newObj tr) -- to the right
    | otherwise = Node (n + 1) (insert newObj tl) obj tr -- to the left

-- TODO: This does not compute the height correctly!

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

{- Ex. 3: More folds! -}
{- Ex. 3.1. -}
xorBin :: Bool -> Bool -> Bool
xorBin True False = True
xorBin False True = True
xorBin _ _ = False

xor :: [Bool] -> Bool
xor = foldr xorBin False

{- Ex. 3.2 -}
map' :: (a -> b) -> [a] -> [b]
-- A more elegant way would be (cf. Agata's solution!):
-- map' f = foldr ((:) . f) []
map' f = foldr (\x acc -> f x : acc) []

{- Ex. 3.3 -}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

{- Ex. 4: Finding primes -}
--- Given an integer n, generate odd primes up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2 * x + 1) . notInBase

base :: Integer -> [Integer]
base n =
  [ x
    | x <- [1 .. n],
      j <- [1 .. n],
      i <- [1 .. j],
      x == i + j + 2 * i * j
  ]

-- The following is for the sake of learning. It would be easier to use `\\`
-- from Data.List: map (\x -> 2 * x + 1) ([1 .. n] \\ base n)

-- Stolen from https://stackoverflow.com/a/15030584; note: (.:) == (.) . (.)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

notInBaseFilter :: Integer -> Integer -> Bool
notInBaseFilter = not .: (flip elem . base)

-- Alternative implementations:
-- notInBaseFilter n = not . flip elem (base n)
-- notInBaseFilter = (not .) . (flip elem . base)

notInBase :: Integer -> [Integer]
notInBase n = filter (notInBaseFilter n) [1 .. n]
