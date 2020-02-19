module Main where

import JoinListBuffer 
import Editor
-- Why do we have to import everything ?? Without Buffer, it does not work
import Sized
import Scrabble
import Buffer

main = runEditor editor x
  where 
     x = foldl (+++) start l
     start = fromString "This buffer is for notes you \
                   \ don't want to save, and for"  :: JoinList (Score, Size) String
     l =  map fromString lines :: [JoinList (Score, Size) String]
     lines = 
        ["To load a different file, type the character L followed" 
        , "evaluation of steam valve coefficients." 
        , "by the name of the file." 
        ]
