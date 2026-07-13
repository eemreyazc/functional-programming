module Main where

mystery1 :: Integer -> Integer
mystery1 n
    | n <= 1 = 2
    | even n = n + mystery1 (n `div` 2)
    | otherwise = mystery1 (n - 1) - mystery1 (n - 2)

mystery2 :: Integer -> Integer -> Integer
mystery2 n acc
    | n == 0 = acc
    | even n = mystery2 (n `div` 2) (acc + n)
    | otherwise = mystery2 (n - 1) (acc * 2)

alpha :: Integer -> Integer
alpha 0 = 1
alpha n = beta (n - 1) + n

beta :: Integer -> Integer
beta 0 = 2
beta n = 2 * alpha (n - 1) - beta (n - 1)

mystery3 :: Integer -> Integer
mystery3 n
    | n > 100 = n - 10
    | otherwise = mystery3 (mystery3 (n + 11))

mystery4 :: Integer -> Integer
mystery4 n
    | n <= 1 = 1
    | even n = mystery4 (n `div` 2) + mystery4 (n `div` 2 - 1) + n
    | otherwise = mystery4 (n - 1) - mystery4 (n - 3)

mystery5 :: Integer -> Integer
mystery5 n
    | n <= 0 = 0
    | n `mod` 3 == 0 = n + mystery5 (step n)
    | otherwise = mystery5 (n - 1) + step n
    where
        step x = x `div` 3

loopSearch :: Integer -> Integer
loopSearch n = go 0
    where
        go y
            | y * y == n = y
            | otherwise = go (y + 1)

main :: IO ()
main = do
    putStrLn "--- Odev Soru 5 Sonuclari ---"
    putStrLn $ "01. mystery1 9     : " ++ show (mystery1 9)
    putStrLn $ "02. mystery2 13 1  : " ++ show (mystery2 13 1)
    putStrLn $ "03. alpha 5        : " ++ show (alpha 5)
    putStrLn $ "04. beta 5         : " ++ show (beta 5)
    putStrLn $ "05. mystery3 99    : " ++ show (mystery3 99)
    putStrLn $ "06. mystery3 102   : " ++ show (mystery3 102)
    putStrLn $ "07. mystery4 8     : " ++ show (mystery4 8)
    putStrLn $ "08. mystery5 14    : " ++ show (mystery5 14)
    putStrLn $ "09. loopSearch 16  : " ++ show (loopSearch 16)
    putStrLn   "10. loopSearch 10  : Non-terminating"