module Main where

data Player = Red | Blue deriving (Eq, Show)
data GameTree = Node Int Player [GameTree] deriving (Eq, Show)

-- O anki taş sayısına (n) göre yapılabilecek geçerli hamleleri (1 veya 2) bulur.
legalMoves :: Int -> [Int]
legalMoves n = [ m | m <- [1, 2], m <= n ]

-- Başlangıç taş sayısı ve oyuncuyu alıp tüm olası senaryoları (ağacı) çizer.
buildGameTree :: Int -> Player -> GameTree
buildGameTree 0 p = Node 0 p [] -- Temel durum: Taş kalmadıysa çocuk (alt dal) da yoktur.
buildGameTree n p = 
    Node n p 
    -- Yapılabilen her hamle (m) için rakip oyuncuyla yeni bir dal oluştur.
    [ buildGameTree (n - m) (if p == Red then Blue else Red) 
    | m <- legalMoves n 
    ]

treeSize :: GameTree -> Int
treeSize (Node _ _ children) = 1 + sum (map treeSize children)

leafCount :: GameTree -> Int
leafCount (Node _ _ []) = 1
leafCount (Node _ _ children) = sum (map leafCount children)

main :: IO ()
main = do
    putStrLn "--- Q3: Take-Away Game Tree Testi ---"
    
    let initialTree = buildGameTree 4 Red
        
    putStrLn $ "Hesaplanan Toplam Dugum (Node) Sayisi: " ++ show (treeSize initialTree) 
    putStrLn "  -> Teorik Beklenen: 12"
    
    putStrLn $ "\nHesaplanan Toplam Yaprak (Terminal Leaf) Sayisi: " ++ show (leafCount initialTree)
    putStrLn "  -> Teorik Beklenen: 5"