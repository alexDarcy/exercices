module Exercises.E21to30 where

import System.Random
import Data.List
import Debug.Trace

-- Exercise 21: insert an element into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = fst y ++ x : snd y
  where y = splitAt (n-1) xs

-- Exercise 22: generate a range
range :: Int -> Int -> [Int]
range a b = [a..b]

-- Exercise 23: random elements of a list
-- The seed is constant here
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n 
  | n > length xs = error "The substring is larger than the string"
  | otherwise = do 
      gen <- getStdGen
      let pos = take n . nub . randomRs (0, length xs-1) $ gen
      return $ map (\ i -> xs !! i) pos

-- Exercise 24: random elements in a range
-- The seed is constant here
diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
  gen <- getStdGen
  return $ take n . nub . randomRs (0, m) $ gen

-- Exercise 25: random permutation of a list
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

-- My solution : swap 2 elements in the list successively
-- Helper function 
rnd_permu' :: (Show a) => StdGen -> [a] -> [a]
-- rnd_permu' g x | trace ("perm" ++ show x ++ "--") False = undefined
rnd_permu' g [] = []
rnd_permu' g (x:[]) = [x]
rnd_permu' g (x:xs) = xs !! (pos-1) : rnd_permu' g xs'
  where
    pos = fst . randomR (1, length xs) $ g
    l = splitAt pos xs
    xs' = (init . fst $ l) ++ [x] ++ snd l

rnd_permu'' xs = do
  g <- getStdGen
  return $ rnd_permu' g xs

-- Exercise 26 : distinct combinations from a list, from the solution
combinations' :: Show a => Int -> [a] -> [[a]]
combinations' 0 _ = [[]]
combinations' n xs = [xs !! (i-1) : x' | i <- [1..length xs]
                                    , x' <- combinations' (n-1) . drop i $ xs]

