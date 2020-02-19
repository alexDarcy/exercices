module Exercises.E01to11 where

-- Ex. 1: Last element of a list
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- Ex. 2: Last but one element of a list
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast (x:[]) = error "List with one element"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs

-- Ex. 3: k-th element of a list
elementAt :: [a] -> Int -> a
elementAt [] k = error "Empty list"
elementAt (x:xs) k
  | k > (length xs + 1) = error "Index greater that length"
  | k > 1 = elementAt xs (k-1)
  | k == 1 =  x
  | otherwise = error "Non-positive index"


-- Ex. 4: length of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) =  1 + myLength xs

-- Ex. 5: reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) =  myReverse xs ++ [x]

-- Ex. 6: find out if a list is a palindrom
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = (x == (myLast xs)) && isPalindrome(init xs)

-- Ex. 7: flatten nested list
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x ) = [x]
flatten (List (x:y) ) = flatten x ++ flatten (List y)

-- Ex. 8: remove duplicates from a list
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:xs)  
  | x == head xs = compress xs
  | otherwise = [x] ++ compress xs

-- Ex. 9: pack duplicates into sublists

-- Helper function: the first list is initially empty, the second contais the
-- list to process. At the end, all first identical values from the second list
-- will have been transferred to the first
consecutive :: (Eq a) => [a] -> [a] -> ([a], [a])
consecutive x y 
  | length y == 0    = (x, y)
  | length x == 0    = consecutive [head y] (tail y)
  | last x == head y = consecutive (x ++ [head y]) (tail y)
  | otherwise        = (x, y)

pack :: (Eq a) => [a] -> [[a]]
pack x 
  | length right == 0 = [left]
  | otherwise         = [left] ++ pack right
  where left = fst(consecutive [] x) 
        right = snd(consecutive [] x) 

-- Ex. 10: Run-length encoding of a list
-- Here is my solution
-- Helper function: for each sublist returns a pair (first elemnts, length)
-- It requires there is no empty string
llength :: (Eq a) => [[a]] -> [(a, Int)]
llength (x:[]) = [(head x, length x)]
llength (x:xs) = [(head x, length x)] ++ llength xs

encode' :: (Eq a) => [a] -> [(a, Int)]
encode' x = llength(pack x)

-- Here is a much simpler one from the answer
encode x = map (\x -> (head x, length x)) (pack x)

-- Ex. 11: Run-length encoding of a list with special case for entry without
-- duplicates
-- Inspired from the answer
data ListItem a = Single a | Multiple Int a
    deriving (Show)

llengthModif x 
  | length x == 1 = Single (head x)
  | otherwise     = Multiple (length x) (head x)

encodeModified x = map llengthModif (pack x)
