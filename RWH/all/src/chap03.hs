import Data.List 
import Data.Ord
import System.Random

-- Ex 01
nbElts :: [a] -> Int
nbElts [] = 0
nbElts (x:xs) = 1 + nbElts xs

-- Ex 03
mean :: Fractional a => [a] -> a
mean x = sum x / fromIntegral (nbElts x)

-- Ex 04
palindrome :: [a] -> [a]
palindrome x = x ++ (reverse x)

-- Ex 05
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- Ex 06
mySort :: [a] -> [a] -> Ordering
mySort x y 
  | length x < length y = LT
  | length x == length y = EQ
  | otherwise = GT

sortBySub = sortBy mySort 

-- Ex 07
interSperse :: a -> [[a]] -> [a]
interSperse _ [] = []
interSperse _ [[x]] = [x]
interSperse elt (x:y:xs) = x ++ [elt] ++ y ++ (interSperse elt xs)

-- Ex 08
data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node a Empty Empty) = 1
treeHeight (Node n left right) = 1 + treeHeight left + treeHeight right

-- Ex 09
data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)

data Point = Point {xPos :: Float, yPos :: Float } 

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

printPoint (Point x y) = show x ++ "," ++ show y

instance Eq Point where
  (==) (Point x1 y1) (Point x2 y2) = y1 == y2 || x1 == x2

instance Ord Point where
  compare (Point x1 y1) (Point x2 y2) 
    | y1 == y2 = compare x1 x2
    | otherwise = compare y1 y2 

-- Ex 10 
-- Is there a left turn from p3-p2 to p1-p2 ?
turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3
  | delta < 0 = RightTurn 
  | delta == 0 = Straight
  | otherwise = LeftTurn
  where delta = crossProduct p1 p2 p3 

-- To use the cross product, we use a 3D vector with z = 0.
-- Argument a b c so the cross product is (bc, ba)
crossProduct (Point x1 y1) (Point x2 y2) (Point x3 y3) = (x3-x2)*(y1-y2) - (y3-y2)*(x1-x2)

-- Ex 11
multiTurns :: [Point] -> [Direction]
multiTurns (x:y:z:rest) = turn x y z : multiTurns (y:z:rest)
multiTurns l = []

-- Compute the convex hull using Graham scan
convexHull :: [Point] -> [Point]
convexHull = grahamScan . sortByAngleWithPivot

-- Find the pivot at place it at the beginning. Then sort by the angle between
-- the current point, P and the x-axis
sortByAngleWithPivot l = head l' : sortByAngle (head l') (tail l')
  where 
    l' = minAtStart l

-- Beware of the argument order to turn
grahamScan :: [Point] -> [Point] 
grahamScan (x:y:z:rest) 
  | (turn z y x) == LeftTurn = x:y: grahamScan (z:rest)
  | otherwise = x: grahamScan (z:rest)
grahamScan l = l

-- Find the index of the minimum y-value and put it at the beginning
minAtStart l = (maxVal : left) ++ (tail right)
  where 
    (maxVal, maxPos) = minimumBy (comparing fst) $ zip l [0..]
    (left, right) = splitAt maxPos l

-- Slope between p-x and x-axis (if different x-values !)
slope p x  = (yPos x - yPos p) / (xPos x - xPos p)

--slope :: Point -> Point -> Double
-- Special case : x is at the vertical of p and/or y is at the vertical of P
compareSlope p x y
  | (xPos x == xPos p) && (xPos y == xPos p) = compare (yPos x) (yPos y)
  | (xPos x == xPos p) || (xPos y == xPos p) = compare (xPos x) (xPos y)
  | otherwise = compare (slope p x) (slope p y)

-- Sort according to the angle between the point, P and the x-axis
sortByAngle :: Point -> [Point] -> [Point]
sortByAngle p l = sortOn (\x -> slope p x) l
  -- where offset = Point (xPos p + 1) (yPos p)

-- For plotting : add the pivot (from the beginning) to its proper place
replacePivot :: [Point] -> [Point]
replacePivot l = takeWhile (\x -> slope p x < 0) l' ++ [p] ++ dropWhile (\x -> slope p x < 0) l'
  where 
    p = head l
    l' = tail l

test = [Point 1 2, Point 2 3, Point 3 2, Point 2 1, Point 3 4, Point 2 4, Point 1 4]

main = do
  let seed = mkStdGen 10
  let n = 20
  let (x, y) = splitAt (n `div` 2) (take n $ randomRs (0, 100) seed :: [Float])
  let pts =  [Point x' y' | (x', y') <- zip x y]
  print pts

  writeFile "input.csv" $ unlines . (map printPoint) $ pts
  writeFile "hull.csv" $ unlines . (map printPoint) $ replacePivot . convexHull $ pts

  print "nothing"
