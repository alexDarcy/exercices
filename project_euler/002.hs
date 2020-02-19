-- The naive way 
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)

problem2 = sum $ takeWhile (<= 4000000) $ filter even $ map fib [1..]

-- Why is it naive ? Each Fibonaci terms is actually computed several times. For
-- example, the first terms will be computed n times for a list of length n.
-- This results (with :set +s in ghci) in around 34s.  To optimize that, we want
-- to use only the two terms before in the list and avoid recursion. Here is a
-- solution from HaskellWiki, which creates an infinite list :
-- 
fibs = 1 : scanl (+) 1 fibs

--It's so fast it now takes almost 0 seconds thanks to laziness !
problem2s = sum $ takeWhile (<= 4000000) $ filter even $ fibs

