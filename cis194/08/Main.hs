import Employee
import Data.Tree

left = GL [Emp "Bob" 3] 3 
right = GL [Emp "Sam" 4] 4
boss = Emp "Stan" 9

main = do
  print "lol"
  {-print $ nextLevel boss [(left, mempty), (right, mempty)]-}
