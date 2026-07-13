module Main where

factorial :: Integer -> Integer
factorial  n
    | n == 0 = 1
    | otherwise = (factorial (n - 1)) * n

main :: IO ()
main = print (factorial 5)