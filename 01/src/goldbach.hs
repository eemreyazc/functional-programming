module Main where

isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = checkDiv n 2
    where
        checkDiv n d
            | d * d > n = True
            | n `mod` d == 0 = False
            | otherwise = checkDiv n (d + 1)

goldbach :: Integer -> Integer
goldbach n = findPair n 2
    where
        findPair n k
            | isPrime k && isPrime (n - k) = n - k
            | otherwise = findPair n (k + 1)


main :: IO ()
main = print (goldbach 256)