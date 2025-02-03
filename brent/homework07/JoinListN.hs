module JoinListN where

import ScrabbleN (Score, scoreString)
import Sized (Sized, getSize, size)

-- data JoinListBasic a
--   = Empty
--   | Single a
--   | Append (JoinListBasic a) (JoinListBasic a)

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

-- ex. 1

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single mval _) = mval
tag (Append mval jl1 jl2) = mval

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- ex. 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ 0 (Single _ elem) = Just elem
indexJ _ (Single _ _) = Nothing
indexJ i (Append mval jl1 jl2)
  | i < lenl = indexJ i jl1
  | i < len = indexJ (i - lenl) jl2
  | otherwise = Nothing
  where
    lenl = (getSize . size . tag) jl1
    len = (getSize . size) mval

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n jl | n >= (getSize . size . tag) jl = Empty
dropJ n jl@(Append _ jl1 jl2) = dropJ n jl1 +++ dropJ (n - lenl) jl2
  where
    lenl = (getSize . size . tag) jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ n jl | n >= (getSize . size . tag) jl = jl
takeJ n jl@(Append _ jl1 jl2) = takeJ n jl1 +++ takeJ (n - lenl) jl2
  where
    lenl = (getSize . size . tag) jl1

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine xs = Single (scoreString xs) xs
