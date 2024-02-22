module Safe where

safeSqrt x =
  if x < 0
    then Left "dupa a nie pierwiastek"
    else return (sqrt x)

safeRecSqrt x =
  safeSqrt x
    >>= \y ->
      if x == 0
        then Left "div by 0"
        else return (1 / x)
