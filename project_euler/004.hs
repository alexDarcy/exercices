-- To check if a number is a palindrome, we will first "tokenize" the number
-- into a list of digits.
tokenize' :: Integral a => a -> [a]
tokenize' x 
  | x < 10 = [x]
  | otherwise = (mod x 10) : tokenize' (x `div` 10)

tokenize :: Integral a => a -> [a]
tokenize x = reverse . tokenize' $ x

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x
 
-- To generate the list of possibilites, we can try all the 3-digits numbers
-- (brute-force).
brute = [x*y |x <- [100..999], y <- [100..999]]

-- This takes around 16s on my machine. As we want the largest number, we can
-- assume the first digit of each factor is 9.
optim = [x*y |x <- [900..999], y <- [900..999]]

-- This new version takes around 0.2s but we can still optimize it. We know the
-- last digit has to be 9, so the last digit of the factor is either 1, 3 or 9.
-- This gives a solution almost immediately.
range = [x*10 +y | x <- [90..99], y <- [1,3,9]]
optim2 = [x*y | x <- range, y <- range]

problem4 = maximum $ filter (isPalindrome . tokenize) optim2
