-- (\sum_1^n i)^2
squareOfSum :: Integral a => a -> a
squareOfSum n = x*x where x = n*(n+1) `div` 2

-- \sum_1^n i^2
sumOfSquare :: Integral a => a -> a
sumOfSquare n = n*(n+1)*(2*n+1) `div` 6

problem6 = squareOfSum 100 - sumOfSquare 100
