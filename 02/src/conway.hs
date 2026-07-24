module Main where

import Data.List (nub, sort)

type Cell = (Int, Int)
type Board = [Cell]

-- Bir hücrenin etrafındaki 8 komşu koordinatı bulur.
neighbours :: Cell -> [Cell]
neighbours (x,y) = 
  [ (x + dx, y + dy) 
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , (dx, dy) /= (0,0) 
  ]

-- Bir hücrenin etrafındaki CANLI komşu sayısını hesaplar.
liveNeighbours :: Board -> Cell -> Int
liveNeighbours board cell = 
  length [p | p <- neighbours cell, p `elem` board]

-- Bir sonraki nesilde "canlanma" veya "ölme" ihtimali olan tüm aday hücreleri bulur.
-- Adaylar: Şu an canlı olanlar + canlıların tüm komşularıdır. 
-- nub fonksiyonu mükerrer (tekrar eden) hücreleri listeden temizler.
candidates :: Board -> [Cell]
candidates board = 
  nub (board ++ concatMap neighbours board)

nextGeneration :: Board -> Board
nextGeneration board = 
  [ cell 
  | cell <- candidates board
  , let n = liveNeighbours board cell
  -- KURAL: 3 canlı komşusu olan yaşar/doğar VEYA 2 canlı komşusu olan zaten canlıysa yaşamaya devam eder[cite: 3].
  , n == 3 || (n == 2 && cell `elem` board)
  ]

runLife :: Int -> Board -> Board
runLife 0 board = board
runLife n board = runLife (n - 1) (nextGeneration board)

glider :: Board
glider = [(1,0), (2,1), (0,2), (1,2), (2,2)]

main :: IO ()
main = do
    putStrLn "--- Conway's Game of Life Test ---"
    putStrLn "Baslangic (Glider):"
    print (sort glider)
    
    putStrLn "\n1 Nesil Sonra:"
    print (sort (runLife 1 glider))

    putStrLn "\n2 Nesil Sonra:"
    print (sort (runLife 2 glider))

    putStrLn "\n3 Nesil Sonra:"
    print (sort (runLife 3 glider))
    
    putStrLn "\n4 Nesil Sonra:"
    print (sort (runLife 4 glider))
    
    putStrLn "\n5 Nesil Sonra:"
    print (sort (runLife 5 glider))
