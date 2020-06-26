import Test.QuickCheck
import Exercises.E01to11

-- Ex. 1 to 11
prop_ex1 :: (NonEmptyList Integer) -> Bool
prop_ex1 (NonEmpty xs) = myLast xs == last xs

prop_ex2 :: (NonEmptyList Integer) -> Property
prop_ex2 (NonEmpty xs) = (length xs > 1) ==> myButLast xs == (last . init $ xs)

prop_ex3 :: (NonEmptyList Int) -> (Positive Int) -> Property
prop_ex3 (NonEmpty xs) (Positive n) = (length xs > n-1) ==> (elementAt xs n) == (head $ drop (n-1) xs)

prop_ex4 :: [Int] -> Bool
prop_ex4 xs = myLength xs == length xs

prop_ex5 :: [Int] -> Bool
prop_ex5 xs = myReverse xs == reverse xs

main = do
  quickCheck prop_ex1
  quickCheck prop_ex2
  quickCheck prop_ex3
  quickCheck prop_ex4
  quickCheck prop_ex5
  -- quickCheck (prop_ex3 :: [Int] -> Property)
