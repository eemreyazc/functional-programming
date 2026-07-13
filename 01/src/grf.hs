module Main where

minimize :: ( Integer -> Bool ) -> Integer
minimize p = search p 0
    where
        search p x
            | p x = x
            | otherwise = search p (x + 1)

-- testFunc :: Integer -> Bool
-- testFunc x = (x * x >= 30)

main :: IO ()
main = print (minimize (\ x -> x * x >= 30))
-- main = print (minimize testFunc)