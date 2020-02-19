{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------
instance Functor Poly where
  fmap f (P p) = P (map f p)

-- Check two polynoms are equivalent by a multiplicative constant
areMultiple :: (Num a, Eq a) => Poly a -> Poly a -> Bool
areMultiple (P []) (P []) = True
areMultiple (P _) (P []) = False
areMultiple (P []) (P _) = False
areMultiple (P xs) (P ys) = xs' == ys'
  where 
    xs' = map (* last ys) xs
    ys' = map (* last xs) ys

instance (Num a, Eq a) => Eq (Poly a) where
    (==) = areMultiple 
 
-- Exercise 3 -----------------------------------------

printPower :: (Num a, Show a, Eq a) => Int -> a -> String
printPower n p
  | p == 0 = ""
  | n == 0 = show p
  | n == 1 = p' ++ "x"
  | otherwise = p' ++ "x^" ++ show n
  where p' = if p == 1 then "" else show p

printPoly :: (Num a, Show a, Eq a) => Int -> [a] -> String
printPoly _ [] = ""
printPoly n [p] = printPower n p
printPoly n (p:ps) 
  | next == "" = cur 
  | otherwise = cur ++ sep ++ next
  where 
    sep = if p /= 0 then " + "  else ""
    cur = printPower n p
    next = printPoly (n-1) ps

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = ""
    show (P xs) = printPoly (length xs-1) (reverse xs)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P ps) (P []) = P ps
plus (P []) (P qs) = P qs
plus (P (p:ps)) (P (q:qs)) = P ((p +q) : ps')
  where P ps' = plus (P ps) (P qs)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
--times = undefined 
times (P ps) (P qs) = foldr (+) (P [0]) list
  where list = map P $ timesList ps qs

timesList :: Num a => [a] -> [a] -> [[a]]
timesList [] _ = [[]]
timesList _ [] = [[]]
timesList (p:ps) qs = map (*p) qs : timesList ps ( 0:qs)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = fmap negate
    fromInteger c = P [fromIntegral c]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P ps) y = sum $ zipWith (*) ps (iterate (*y) 1)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 y = y
    nderiv 1 y = deriv y
    nderiv n y = nderiv (n-1) (deriv y)

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P ps) = P (tail $ zipWith (*) ps [0..])

