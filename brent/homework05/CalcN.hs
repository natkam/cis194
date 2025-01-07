module Calc where

import ExprT
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul m n) = eval m * eval n
eval (Add m n) = eval m + eval n

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Just expr -> Just (eval expr)
  Nothing -> Nothing
