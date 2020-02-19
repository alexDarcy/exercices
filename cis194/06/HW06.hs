{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) s = Cons x (sInterleave s xs) 

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x xs) = x : sTake (n-1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

-- Finite version
--test :: Integer -> [Integer]
--test 0 = [0]
--test k = k : concatMap test [0..k-1]

-- Could not find a Stream solution (see above for a finite solution) so had to
-- look up on the Internet (creadits to BerndSchwarzenbacher, Eric D. Burgess)
ruler :: Stream Integer
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' n = sInterleave (sRepeat n) (ruler' (n+1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate generator 
  where generator n = (1103515245*n + 12345 ) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 237 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing  
minMax (x:xs) = minMax' x x xs

{- Total Memory in use: 1 MB -}
minMax' :: Int -> Int -> [Int] -> Maybe (Int, Int)
minMax' xMin xMax [] = Just (xMin, xMax)
minMax' xMin xMax (x:xs) 
  | x > xMax = minMax' xMin x xs
  | x < xMin = minMax' x xMax xs
  | otherwise = minMax' xMin xMax xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

-- ( a b )
-- ( c d )
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix a' b' c' d') = Matrix aNew bNew cNew dNew
    where 
      aNew = a*a' +b*c'
      bNew = a*b' +b*d'
      cNew = c*a' +d*c'
      dNew = c*b' +d*d'

instance Show Matrix where
  show (Matrix a b c d) = "(" ++ show a ++ " " ++ show b ++ ")\n" ++
                          "(" ++ show c ++ " " ++ show d ++ ")"

fastFib :: Int -> Integer
fastFib n = b
  where Matrix a b c d = Matrix 1 1 1 0 ^n
