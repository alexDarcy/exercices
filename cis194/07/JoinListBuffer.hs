{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid
import Buffer
import Sized
import Scrabble


instance Buffer (JoinList (Score, Size) String) where
  toString (Empty) = ""
  toString (Single m a) = a
  toString (Append m x y) = toString x ++ toString y

  fromString s      = foldl (+++) x l 
    where 
      (x:l) = map fromSingleString $ lines s
      fromSingleString s = Single (tag . scoreLine $ s, Size 1) s
 
  numLines          = getSize . snd . tag
  value             = scoreInt . fst . tag
  line n            = indexJ n
  replaceLine n l b 
    | n < 0 || n > numLines b = b
    | otherwise = takeJ n b +++ fromString l +++ dropJ (n+1) b


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a ->  JoinList m a -> JoinList m a 
(+++) left right = Append (mappend (tag left) (tag right)) left right

-- Return root annotation
tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty
tag (Single m a) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ n (Single m a) 
  | n == 0 = Just a
  | otherwise = Nothing
indexJ n (Append m left right) 
  | n > n'-1 = Nothing
  | n < nLeft = indexJ n left
  | otherwise = indexJ (n-nLeft) right
    where 
      [n', nLeft] = map (getSize . size) [m, tag left]


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n <= 0 = x
dropJ _ (Empty) = Empty
dropJ n (Single m a) = Empty
dropJ n (Append m left right) 
  | n > n'-1 = Empty
  | n < nLeft = (dropJ n left) +++ right
  | otherwise = dropJ (n-nLeft) right
    where 
      [n', nLeft] = map (getSize . size) [m, tag left]

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ n x | n >= n' = x
  where n' = getSize . size . tag $ x
takeJ _ Empty = Empty
takeJ n (Single m a) = Empty
takeJ n (Append m left right) 
  | n <= nLeft = takeJ n left
  | otherwise = left +++ (takeJ (n-nLeft) right)
    where 
      [n', nLeft] = map (getSize . size) [m, tag left]


scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

x = fromString "a" :: JoinList (Score, Size) String
y = fromString "b" :: JoinList (Score, Size) String
z = x +++ y +++ x
