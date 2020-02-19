import Data.List

-- We first generate a list of prime numbers using the sieve of Erathosthene.
-- The idea is to store the head of the list and cross all of its multiple in
-- the remaining list.
sieve :: Integral a => [a] -> [a] -> [a]
sieve xs [] = xs
sieve xs (y:ys) = y : sieve xs (filter (\x -> mod  x y /= 0) ys) 

-- Thanks to laziness, we can have an infinite list :
primes :: Integral a => [a]
primes = sieve [] [2..]

-- The factorization itself into prime factors is done by considering all prime
-- numbers up to the square root of the considered number. If the number is a
-- divisor, we append it to the list of divisors
factorize :: (Integral a) => a -> [a]
factorize x = filter (\n -> mod x n == 0)  primes'
  where primes' = takeWhile (\p -> p*p <= x) primes

-- Unfortunately, this version is too slow. To optimize it, we will divide the
-- number for each factor found. This decreases dramatically the number of
-- primes search and enables us to run the solution is less than one second. As
-- a bonus, it returns the factors with their multiplicity.
factorizeOpt :: (Integral a) => a -> [a] -> [a]
factorizeOpt n (p:xs)
  | p > n = []
  | mod n p == 0 = p : factorizeOpt (n `div` p)  (p:xs)
  | otherwise = factorizeOpt n xs

problem3 = maximum $ factorizeOpt 600851475143 primes
