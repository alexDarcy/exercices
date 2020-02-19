module Employee where

import Data.Monoid
import Data.Tree -- Import <>


-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Read, Eq)

instance Show Employee
  where show (Emp name fun) = name ++ " (" ++ show fun ++ ")"

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

testCompany3 :: Tree Employee
testCompany3
  = Node (Emp "Joe" 5) 
    [ Node (Emp "John" 1) []
    , Node (Emp "Sue" 5) []
    ]
   

testCompany4 :: Tree Employee
testCompany4
  = Node (Emp "Bob" 2)
    [ Node (Emp "Joe" 5) 
      [ Node (Emp "John" 1) []
      , Node (Emp "Sue" 5) []
      ]
    , Node (Emp "Fred" 3) [] 
    ]


-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

-- Important, the first elements may be identical when merging
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL l1 fun1) (GL l2 fun2) 
    | null l1 = GL l2 fun2
    | null l2 = GL l1 fun1
    | (head l1 == head l2) = mappend (GL l1 fun1) (GL (tail l2) fun2)
    | otherwise = GL (l1 ++ l2) (fun1 + fun2)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l fun) = GL (e:l) (fun + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2
  | g1 > g2 = g1
  | otherwise = g2

treeFold :: (a -> [b] -> b) -> Tree a -> b
{-treeFold f e (Node x []) = f e x-}
treeFold f (Node x l) = f x (map (treeFold f) l)

-- Input : global boss of the tree and a list of pairs. Each pair is the best
-- guest list (with, without) the local boss
-- Output : pair of guest list (with, without) the global boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList) 
nextLevel boss [] = (GL [boss] (empFun boss), mempty)
nextLevel boss l = foldl mergePairs (mempty, mempty) l'
  where l' = map (concatPair boss) l

-- When merging two guest lists with the boss, we may have duplicate (at the
-- leftmost position). This is managed in mappend of GuestLists
mergePairs (a, b) (c, d) = (a <> c, b <> d)

-- If we choose to add the global boss, we remove the local boss just underneath
-- Otherwise, we simply concatenate the list
concatPair :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList) 
concatPair boss (with0, without0) = (glCons boss without0, max)
  where max = moreFun with0 without0

maxFun tree = moreFun with without
  where (with, without) = treeFold nextLevel tree
