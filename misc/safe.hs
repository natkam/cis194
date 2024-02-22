module Safe where

safeSqrt x =
  if x < 0
    then Left "dupa a nie pierwiastek"
    else return (sqrt x)

{-
safeRecSqrt x =
  safeSqrt x
    >>= \y ->
      if y == 0
        then Left "div by 0"
        else return (1 / y)
-}

{-
According to Bartosz ("Haskell-5-2"), the `Either` monad defines:
```
  return x = Right x
  ea >>= k = case ea of
    Left s -> Left s
    Right x -> k x
```
Therefore, we could write `else Right (1 / y)` instead of
`else return (1 / y)`, and we don't have to explicitly deal
with the case when `safeSqrt` returns a `Left` error string.
-}

{- The same function with the "do" notation (hides the binding),
and with the $ operator instead of parentheses, just for fun: -}
safeRecSqrt x = do
  y <- safeSqrt x
  if y == 0
    then Left "div by 0"
    else return $ 1 / y
