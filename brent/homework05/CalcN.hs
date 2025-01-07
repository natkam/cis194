module Calc where

import ExprT
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul m n) = eval m * eval n
eval (Add m n) = eval m + eval n

evalStr :: String -> Maybe Integer
-- evalStr s = maybe Nothing (Just . eval) (parseExp Lit Add Mul s)
-- evalStr s = parseExp Lit Add Mul s >>= Just . eval
evalStr s = fmap eval (parseExp Lit Add Mul s)
