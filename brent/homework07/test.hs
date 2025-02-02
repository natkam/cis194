module TestJoinListN where

import JoinListN
import Sized (Size (Size))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main = defaultMain tests

-- Helper functions (ex. 2)
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ jl1 jl2) = jlToList jl1 ++ jlToList jl2

jl :: JoinList Size Char
jl = (l1 +++ (l2 +++ l3)) +++ l4
  where
    l1 = Single (Size 1) 'y'
    l2 = Single (Size 1) 'e'
    l3 = Single (Size 1) 'a'
    l4 = Single (Size 1) 'h'

casesIndexJ :: [Bool]
casesIndexJ = [indexJ i jl == (jlToList jl !!? i) | i <- [-5 .. 5]]

casesDropJ :: [Bool]
casesDropJ = [jlToList (dropJ n jl) == drop n (jlToList jl) | n <- [-5 .. 5]]

casesTakeJ :: [Bool]
casesTakeJ = [jlToList (takeJ n jl) == take n (jlToList jl) | n <- [-5 .. 5]]

tests =
  testGroup
    "Homework 7 tests"
    [ testCase "Ex. 2, indexJ" $ and casesIndexJ @?= True,
      testCase "Ex. 2, dropJ" $ and casesDropJ @?= True,
      testCase "Ex. 2, takeJ" $ and casesTakeJ @?= True
    ]
