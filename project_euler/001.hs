-- In a first version, we simply brute-force the problem by enumerating all
-- matching numbers and summing them. 
sumMultiples n = sum [x | x <- [1..n], mod x 3 == 0 || mod x 5 == 0]
problem01 = sumMultiples 9999999

-- We can optimize with the analytical formula for the sum of all numbers up to
-- n :
-- $$ \sum_1^n k = \frac{n(n+1)}{2} $$
sumAll :: Int -> Int
sumAll n = n*(n+1) `div` 2

-- Therefore, we easily have the sum of all multiples of 3 and 5. For example :
-- $ \sum_1^n 3*k = \sum_1^{\floor{n/3}} k $ However, we will count twice the
-- multiple of 15 Note we need the floor function as the maximum can be not a
-- multiple of 3 or 5.  Note also we work only with Int after that.

sumMultiples' :: Int -> Int
sumMultiples' n = 3*sumAll a + 5*sumAll b - 15*sumAll c
  where 
    a = floor (fromIntegral n/3)             
    b = floor (fromIntegral n/5)             
    c = floor (fromIntegral n/15)             

-- Now some results : the first version takes around 18s on my machine for n=9
-- 999 999, while the optimized version takes around 0.1s.
problem01' = sumMultiples' 9999999


