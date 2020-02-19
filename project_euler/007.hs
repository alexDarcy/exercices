-- The list of prime numbers is generated using the sieve of Erathosthene. 
sieve :: Integral a => [a] -> [a] -> [a]
sieve xs [] = xs
sieve xs (y:ys) = y : sieve xs (filter (\x -> mod  x y /= 0) ys) 

primes = sieve [] [2..]
problem7 = primes !! (10001-1)


