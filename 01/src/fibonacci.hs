module Main where

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
            
main :: IO ()
main = do
    putStrLn $ "01: " ++ show (fibonacci 1)
    putStrLn $ "02: " ++ show (fibonacci 2)
    putStrLn $ "03: " ++ show (fibonacci 3)
    putStrLn $ "04: " ++ show (fibonacci 4)
    putStrLn $ "05: " ++ show (fibonacci 5)
    putStrLn $ "06: " ++ show (fibonacci 6)
    putStrLn $ "07: " ++ show (fibonacci 7)
    putStrLn $ "08: " ++ show (fibonacci 8)
    putStrLn $ "09: " ++ show (fibonacci 9)