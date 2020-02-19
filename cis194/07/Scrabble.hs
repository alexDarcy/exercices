{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score x 
  | i < 0 || i > length scores - 1 = Score 0
  | otherwise = Score (scores !! i)
    where 
      scores = [
                1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 
                3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10 
               ]
      i = ord (toUpper x) - 65

scoreString :: String -> Score
scoreString x = sum $ map score x

scoreInt :: Score -> Int
scoreInt (Score x) = x
