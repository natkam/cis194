log2 :: Integer -> Integer
log2 1 = 0
log2 x = log2 (floor (fromInteger x / 2)) + 1
