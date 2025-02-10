module Party where

import Data.List (sort)
import Data.Tree (Tree (Node))
import Employee (Employee (Emp, empFun, empName), GuestList (GL))

-- ex. 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es totalFun) = GL (e : es) (totalFun + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0

instance Semigroup GuestList where
  GL es1 fun1 <> GL es2 fun2 = GL (es1 ++ es2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- ex. 2
-- TODO!
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z (Node _ []) = z
treeFold f z (Node root sts) = f root (map (treeFold f z) sts)

-- ex. 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (GL [boss] (empFun boss), GL [] 0)
nextLevel boss sublists =
  ( foldr ((<>) . snd) (GL [boss] (empFun boss)) sublists,
    foldr1 (<>) (map fst sublists)
    -- take max (map fst sublists) (map snd sublists)
  )

-- ex. 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel (GL [] 0, GL [] 0)

-- ex. 5
readTree :: String -> Tree Employee
readTree = read

sortNames :: [Employee] -> [String]
sortNames emps = sort $ map ((++ "\n") . empName) emps

printGL :: GuestList -> String
printGL (GL guests totalFun) =
  "Total fun: " ++ show totalFun ++ "\n"
    ++ concat (sortNames guests)

main :: IO ()
main = do
  tree <- readFile "company.txt"
  print $ (printGL . maxFun . readTree) tree
