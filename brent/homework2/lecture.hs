data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

isSmall :: Thing -> Bool
isSmall King = False
isSmall Ship = False
isSmall _ = True

data FailableDouble
  = Failure
  | OK Double
  deriving (Show)

data Person = Person String Int Thing
  deriving (Show)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

failureToZero' :: FailableDouble -> Double
failureToZero' d = case d of
  Failure -> 0
  OK d -> d

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

brent :: Person
brent = Person "Brent" 31 Shoe

ford :: Person
ford = Person "Ford Perfect" 42 Ship

getAge :: Person -> Int
getAge (Person _ a _) = a

introduce :: Person -> String
introduce (Person name age thing) =
  "Meet " ++ name ++ ", who's "
    ++ show age
    ++ " years old and has a "
    ++ show thing
    ++ "."

sayName :: Person -> String
sayName p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

commentAge :: Person -> String
commentAge (Person _ 42 _) = "The best year!"
commentAge (Person _ a _) = "Dude, you're old"

data IntList
  = Empty
  | Cons Int IntList
  deriving (Show)

intListProd :: IntList -> Int
intListProd Empty = 0
intListProd (Cons x rest) = x * intListProd rest
