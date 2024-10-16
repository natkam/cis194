import Data.Char (toUpper)

-- Exercises: if/else expressions, guards, and pattern matching (chapter 9)
mySig x =
  if x < 0
    then -1
    else
      if x > 0
        then 1
        else 0

mySigG x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

score 1 = 10
score 2 = 6
score x
  | 7 > x && x >= 3 = 7 - x
  | otherwise = 0

-- Custom definitions of logical OR and AND (as infix operators)
(|||) :: Bool -> Bool -> Bool
False ||| y = y
True ||| _ = True

(&&&) :: Bool -> Bool -> Bool
(&&&) x y = (x == y) && x

{-  if x == y
    then x
    else False
-}

-- Excersises: IO, do blocks (chapter 10)
guess num = do
  putStrLn "Enter your guess:"
  x <- getLine
  if read x < num
    then do
      putStrLn "Too low!"
      guess num
    else
      if read x > num
        then do
          putStrLn "Too high!"
          guess num
        else putStrLn "That's right!"

ingratiate = do
  putStrLn "What's your name?"
  name <- getLine
  if name == "Simon" || name == "John" || name == "Phil"
    then putStrLn "Haskell is cool."
    else
      if name == "Koen"
        then putStrLn "Debugging Haskell is fun as fuck."
        else putStrLn "Nice shoes, dude."

shout =
  do
    putStrLn "What's your name?"
    name <- getLine
    let loudName = makeLoud name
    putStrLn ("Hi " ++ loudName ++ "!!! So exciting!!!11!")

makeLoud :: String -> String
makeLoud = map toUpper
