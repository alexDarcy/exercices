import Test.QuickCheck
import Exercises.E01to11

prop_myLast xs = (not $ null xs) ==> init xs ++ [myLast xs] == xs

main = do
  quickCheck $ label "myLast for Int"  ( prop_myLast :: [Int] -> Property)
  quickCheck $ label "myLast for Char" ( prop_myLast :: [Char] -> Property)
