module TestJoinListN where

import JoinListN
import Sized (Size (Size))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main = defaultMain tests

-- Helper functions for testing indexJ (ex. 2)
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ jl1 jl2) = jlToList jl1 ++ jlToList jl2

casesIndexJ :: [Bool]
casesIndexJ = [indexJ i t == (jlToList t !!? i) | i <- [-5 .. 5]]
  where
    l1 = Single (Size 1) 'y'
    l2 = Single (Size 1) 'e'
    l3 = Single (Size 1) 'a'
    l4 = Single (Size 1) 'h'
    t = (l1 +++ (l2 +++ l3)) +++ l4

tests =
  testGroup
    "Homework 7 tests"
    [ testCase "Ex. 2, indexJ" $ and casesIndexJ @?= True
    ]
