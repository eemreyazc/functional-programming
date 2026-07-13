module Main where

y :: (a -> a) -> a
y f = f (y f)

-- Both is OK
-- generator :: (Integer -> Integer) -> Integer -> Integer
generator :: (Integer -> Integer) -> (Integer -> Integer)
generator rec n
    | n == 0 = 1
    | otherwise = (rec (n - 1)) * n

factorial :: Integer -> Integer
factorial = y generator

main :: IO ()
main = print (factorial 5)