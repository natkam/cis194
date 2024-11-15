module Golf where

skips :: [a] -> [[a]]
skips xs = concatMap (getEveryNth xs) [1 .. (length xs)]

getEveryNth :: [a] -> Int -> [[a]]
getEveryNth xs n = [(map snd . filter (\(x, y) -> mod x n == 0) . zip [1 ..]) xs]

-- This function's body is pasted into the list comp in `skips'`. I'm
-- leaving it here for the sake of clarity; it does exactly the same as
-- `getEveryNth`, but I find the nested list comprehension unreadable.
getEveryNth' :: [a] -> Int -> [a]
getEveryNth' xs n = concat [[y] | (x, y) <- zip [1 ..] xs, mod x n == 0]

skips' :: [a] -> [[a]]
skips' xs =
  [ [y | (x, y) <- zip [1 ..] xs, mod x n == 0]
    | n <- [1 .. (length xs)]
  ]

-- Version using the list index !! operator, with a very suboptimal heuristic
-- regarding the indices values (in order to avoid raising an exception).
skips'' :: [a] -> [[a]]
skips'' xs =
  [ [xs !! (n * i - 1) | i <- [1 .. length xs], n * i <= length xs]
    | n <- [1 .. length xs]
  ]

localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  [ xs !! i
    | i <- [1 .. length xs - 2],
      xs !! (i - 1) < xs !! i && xs !! (i + 1) < xs !! i
  ]
