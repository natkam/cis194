{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParserN where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- ex. 1
first :: (a -> b) -> (a, c) -> (b, c)
first h (a, c) = (h a, c)

-- fmap :: (a -> b) -> f a -> f b
{-
instance Functor [] where
  fmap _ [] = []
  fmap h (x : xs) = h x : fmap h xs

instance Applicative [] where
  pure a = [a]
  (<*>) h fa = map h fa
-}

runParser' :: (a -> b) -> Parser a -> String -> Maybe (b, String)
runParser' h p xs = case runParser p xs of
  Nothing -> Nothing
  Just (x, rest) -> Just (h x, rest)

instance Functor Parser where
  fmap h p = Parser {runParser = runParser' h p}

-- ex. 2
instance Applicative Parser where
  -- pure :: a -> f a
  pure a = Parser (\s -> Just (a, s))

  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = undefined
