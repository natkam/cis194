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
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root sts) = f root (map (treeFold f) sts)

-- ex. 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss sublists =
  ( GL [boss] (empFun boss) <> mconcat withoutBosses,
    maxFunSubs
  )
  where
    withBosses = map fst sublists
    withoutBosses = map snd sublists
    maxFunSubs = moreFun (mconcat withBosses) (mconcat withoutBosses)

-- ex. 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- ex. 5
readTree :: String -> Tree Employee
readTree = read

sortNames :: [Employee] -> [String]
sortNames = sort . map ((++ "\n") . empName)

printGL :: GuestList -> String
printGL (GL guests totalFun) =
  "Total fun: " ++ show totalFun ++ "\n"
    ++ concat (sortNames guests)

main :: IO ()
main = readFile "company.txt" >>= putStrLn . printGL . maxFun . readTree

-- Using a `<-` in a `do` block is equivalent to using a bind operator (>=).
-- Or something like that. So the following also works within a `do` block:
-- tree <- readFile "company.txt"
-- print $ (printGL . maxFun . readTree) tree
