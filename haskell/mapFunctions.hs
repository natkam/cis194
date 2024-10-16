negateList :: Num a => [a] -> [a]
negateList [] = []
negateList (a : as) = (- a) : negateList as

negateList2 :: [Integer] -> [Integer]
negateList2 = map negate

divisors :: Integral a => a -> [a]
divisors p = [q | q <- [1 .. p], mod p q == 0]

listOfDivisors :: [Integer] -> [[Integer]]
listOfDivisors = map divisors

negateDivisors :: [Integer] -> [[Integer]]
negateDivisors = (map negateList2) . listOfDivisors
