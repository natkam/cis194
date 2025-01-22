{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M
import ExprT
import Parser (parseExp)
import StackVM (Program, StackExp (Add, Mul, PushB, PushI), StackVal (IVal), stackVM)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Mul m n) = eval m * eval n
eval (ExprT.Add m n) = eval m + eval n

evalStr :: String -> Maybe Integer
-- evalStr s = maybe Nothing (Just . eval) (parseExp Lit Add Mul s)
-- evalStr s = parseExp Lit Add Mul s >>= Just . eval
evalStr s = fmap eval (parseExp Lit ExprT.Add ExprT.Mul s)

reify :: ExprT -> ExprT
reify = id

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (< 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

-- I had to add Ord to the derived type classes, because max required it
newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

-- I can't think of a better way to handle it :/
instance Expr Mod7 where
  lit i = Mod7 $ mod i 7
  add i1 i2 = Mod7 $ mod (mod7ToInteger i1 + mod7ToInteger i2) 7
  mul i1 i2 = Mod7 $ mod (mod7ToInteger i1 * mod7ToInteger i2) 7

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

-- Credit for the following function: Agata Szajka
mod7ToInteger :: Mod7 -> Integer
mod7ToInteger (Mod7 m) = m

instance Expr Program where
  lit i = [PushI i]
  add p1 p2 = case (stackVM p1, stackVM p2) of
    (Right (IVal i1), Right (IVal i2)) -> [PushI i1, PushI i2, StackVM.Add]
    _ -> []
  mul p1 p2 = case (stackVM p1, stackVM p2) of
    (Right (IVal i1), Right (IVal i2)) -> [PushI i1, PushI i2, StackVM.Mul]
    _ -> []

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT
  = VarLit Integer
  | Var String
  | VarAdd VarExprT VarExprT
  | VarMul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- TODO! (ex. 6)
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = undefined
  add f g = undefined
  mul f g = undefined

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

testProg = testExp :: Maybe Program

{-
data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)
-}
{-
class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

-- This needs FlexibleInstances enabled
instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

-- This needs FlexibleInstances enabled
instance Listable (Tree Int) where
  toList Empty = []
  toList (Node x left right) = toList left ++ [x] ++ toList right

instance (Listable a, Listable b) => Listable (a, b) where
  toList (x, y) = toList x ++ toList y
-}
