module Golf where

skips :: [a] -> [[a]]
skips list = concatMap (getEveryNth list) [1 .. (length list)]

getEveryNth :: [a] -> Int -> [[a]]
getEveryNth list n = [(map snd . filter (\(x, y) -> mod x n == 0) . zip [1 ..]) list]
