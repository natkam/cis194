module Main where

import System.Environment

main :: IO ()
  
main = do
  -- Exercise 1
  {-
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0 ++ " " ++ args !! 1)
  -}

  -- Exercise 2
  {-
  args <- getArgs
  putStrLn ("Sum: " ++ show (read (args !! 0) + read (args !! 1) )  )
  -}
  
  -- Exercise 3
  putStrLn "Gimme your name:"
  name <- getLine
  putStrLn ("Hello, " ++ name)
  
