
lazy :: Integer -> Integer -> [Integer] -> Integer
lazy from to input = sum (take numToTake (drop numToDrop (out input)))
  where from' = fromIntegral from
        to' = fromIntegral to
        numToTake = to' - from' + 1
        numToDrop = from' - 1

out :: [a] -> [a]
out (x:xs) = x : (next 1 xs)
  where next n (y:ys) = [y] ++ (take (2^n - 1) (out (x:xs))) ++ (next (n + 1) ys)
