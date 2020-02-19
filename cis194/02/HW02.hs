{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches x = length . filter (uncurry (==)) . zip x 

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map (\x -> length . filter (== x) $ c) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum $ zipWith min (countColors x) (countColors y)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonExact
  where 
    exact = exactMatches secret guess
    nonExact = matches secret guess - exact

-- Exercise 4 -----------------------------------------

getGuess :: Move -> Code
getGuess (Move guess _ _) = guess

isConsistent :: Move -> Code -> Bool
isConsistent move secret = move == getMove secret (getGuess move) 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concatMap (\x -> map (x:) (allCodes (n-1))) colors

-- Exercise 7 -----------------------------------------
-- Dumb version : try every possible move consistent with a given code
solve' :: Code -> [Code] -> [Move]
solve' _ [] = []
solve' secret l 
  | first == secret = [move]
  | otherwise       = move : solve' secret (filterCodes move (tail l))
  where 
    first = head l
    move = getMove secret first

solve :: Code -> [Move]
solve secret = solve' secret (allCodes 2)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
