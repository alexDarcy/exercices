import Data.List

-- First, we need to factorize a number into prime factors as in problem 3.
-- For that, a list of prime numbers is generated using the sieve of Erathosthene. This list is then used for the factorization, where a factor may appear multiple times.
sieve :: Integral a => [a] -> [a] -> [a]
sieve xs [] = xs
sieve xs (y:ys) = y : sieve xs (filter (\x -> mod  x y /= 0) ys) 

primes n = sieve [] [2..n]

factorize' _ [] = []
factorize' n (p:xs)
  | mod n p == 0 = p : factorize' (n `div` p)  (p:xs)
  | otherwise = factorize' n xs

factorize n = factorize' n $ primes n

-- We then need to create the union between two list of prime factors, wheach each prime may be repeated. The union is created from the union of all largest sublists.
unionLists :: Integral a => [a] -> [a] -> [a]
unionLists xs [] = xs
unionLists [] ys = ys
unionLists xs ys 
  | elem x ys  = maximum [curX, curY] ++ (unionLists nextX nextY)
  | otherwise  = curX ++ (unionLists nextX ys)
  where
    x = head xs
    (curX, nextX) = partition (== x) xs
    (curY, nextY) = partition (== x) ys

-- Finally, the smallest number is simply the union of all prime factorizations for the numbers between 2 and 20.
problem5 = product $ foldl unionLists [] $ map factorize [2..20]
