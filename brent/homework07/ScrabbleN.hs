{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ScrabbleN where

import Data.Char (toUpper)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

instance Semigroup Score where
  (<>) = (+)

score :: Char -> Score
score c
  | x `elem` "AEILNORSTU" = Score 1
  | x `elem` "DG" = Score 2
  | x `elem` "BCMP" = Score 3
  | x `elem` "FHVWY" = Score 4
  | x `elem` "K" = Score 5
  | x `elem` "JX" = Score 8
  | x `elem` "QZ" = Score 10
  | otherwise = Score 0
  where
    x = toUpper c

scoreString :: String -> Score
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score i) = i
