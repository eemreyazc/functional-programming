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

nthPrime :: Integer -> Integer
nthPrime n = findPrime n 2
    where
        findPrime count current
            | count == 0 = current - 1
            | isPrime current = findPrime (count - 1) (current + 1)
            | otherwise = findPrime count (current + 1)

main :: IO ()
main = print (nthPrime 9)