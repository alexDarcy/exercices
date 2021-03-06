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

-- Ex. 27. Algorithm :
-- 1. Generate all the combinations using the first elements of the list of integer (using question 26)
-- 2. For each combination:
--    a. remove this possibility of the list
--    b. call the function recursively.
--    c. the combination we "blocked" must be add to the list of result (first difficulty)
-- 3. We must combine the result of all combination (second difficulty)
groupSubsets :: (Show a, Eq a) => [Int] -> [a] -> [[[a]]]
groupSubsets _ [] = [[[]]]
groupSubsets [_] xs = [[xs]]
groupSubsets ns xs = concatMap groupSubsets' root -- Step 3
  where
    root = combinations' (head ns) xs -- Step 1.
    groupSubsets' x = map (x:) others -- Step 2.c
      where others = groupSubsets (tail ns) (xs \\ x) -- Step 2.a and 2.b

-- Ex. 28 : sort by length, then by frequency
lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

-- To sort by frequence, we need to sort by length, then group by length
-- And sorty by this new length (so lsort can be used again)
-- Concat everything to return to a list of list
lfsort :: [[a]] -> [[a]]
lfsort xs = concat . lsort . lgroup . lsort $ xs
  where lgroup = groupBy (\x y -> (length x) == (length y))
