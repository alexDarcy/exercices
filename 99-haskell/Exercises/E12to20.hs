module Exercises.E12to20 where

-- Exercise 12: Decode a run-length encoded list
data ListItem a = Single a | Multiple Int a
    deriving (Show, Eq)

decodeModified :: [ListItem a] -> [a]
-- My version, inspired from the correction
decodeModified [] = []
decodeModified (x:xs) = extract x ++ decodeModified xs
-- The correction, better perfs
-- decodeModified x = concatMap extract x
  where
    extract (Single x) = [x]
    extract (Multiple n x) = replicate n x
--decodeModified (Multiple n x) = [head x]

-- Exercise 13: Run-length encoding of a list (direct version)
-- Helper which counts the number of successive values. If we have two pair with
-- same value, there are merged as (nb+1, value)
toListItem :: Eq a => [(Int, a)] -> a -> [(Int, a)]
toListItem acc x 
    | not(null acc) && value == x = (init acc) ++ [(nb+1, x)]
    | otherwise = acc ++ [(1, x)]
    where nb = fst(last acc)
          value = snd(last acc)

-- In my version, I outputted the results as a list of (nb, value) :
encodeDirect' :: Eq a => [a] -> [(Int, a)]
encodeDirect' x = foldl toListItem [] x

-- In fact, we have to output it as ListItem
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect x = map reformat (encodeDirect' x)
  where 
    reformat (1,x) = Single x
    reformat (n,x) = Multiple n x

-- Exercise 14: duplicate elements of a list
dupli = foldr(\x acc -> x:x:acc) []  

-- Exercise 15: replicate elements of a list n times
-- We re-implement replicate
repliSingle ::  Int -> a -> [a]
repliSingle n x
  | n > 0 = x : repliSingle (n-1) x
  | n == 0 = []
  | otherwise = error "Negative length"

repli x n = concatMap (repliSingle n ) x

-- Exercise 16: drop every n-th element of a list
dropEvery :: [a] -> Int  -> [a]
dropEvery x n = dropEvery' x n n 
  where 
    dropEvery' [] p n = []
    dropEvery' (x:xs) 1 n = dropEvery' xs n n
    dropEvery' (x:xs) p n = x : dropEvery' xs (p-1) n

-- Exercise 17: split list into two at a given position
split :: [a] -> Int -> ([a], [a])
split list n= split' ([], list) n
  where 
    split' (x, []) n = (x, [])
    split' (x, y) 0  = (x, y)
    split' (x,y) n 
      | n < 0 = error "Negative length"
      | n > length y = error "Index outside length"
      | otherwise = split' (x ++ [head y], tail y) (n-1)

-- Exercise 18: extract a slice of a list
-- Apply a split successively
slice :: [a] -> Int -> Int -> [a]
slice x i k = fst $ split x' (k-i+1)
  where x' = snd $ split x (i-1) 

-- Exercise 19: rotate a list
rotate :: [a] -> Int -> [a]
rotate x n = (snd p) ++ (fst p)
  where 
    p = split x n'
    n' = if n >= 0 then n else (length x + n)

-- Exercise 20: remove k-th element of a list
removeAt :: Int -> [a] -> (a, [a])
removeAt n x = (head . drop (n-1) $ x, first ++ last)
  where 
    first = take (n-1) x
    last = drop n x
