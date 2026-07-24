module Main where

-- Listenin ilk k elemanını ters çevirir, kalanını aynen bırakır.
flipP :: Int -> [a] -> [a]
flipP k xs = reverse (take k xs) ++ drop k xs

pancakeSort :: Ord a => [a] -> [a]
-- Temel Durum: Boş liste veya tek elemanlı liste zaten sıralıdır.
pancakeSort [] = []
pancakeSort [x] = [x]
pancakeSort xs = 
    -- Adım (d): Kalan (en büyük hariç) kısmı özyineli sırala ve 
    -- bizim asıl en büyük elemanımızı (artık en altta) listenin sonuna ekle.
    pancakeSort remaining ++ [maxElem]
  where
    -- Adım (a): Sırasız kısımdaki en büyük elemanı bul.
    maxElem = maximum xs
    
    -- En büyük elemanın kaçıncı sırada (1-tabanlı indeks) olduğunu hesapla.
    -- (Ondan farklı olan elemanları alıp sayısına 1 ekleyerek buluruz).
    maxIndex = length (takeWhile (/= maxElem) xs) + 1
    
    -- Adım (b): Dizinin başından, en büyük elemanın olduğu yere kadar ters çevir.
    -- Böylece en büyük eleman dizinin EN BAŞINA (yığının en üstüne) gelir[cite: 2].
    flippedToTop = flipP maxIndex xs
    
    -- Adım (c): Şimdi tüm listeyi ters çevir.
    -- Böylece en üstteki en büyük eleman, EN SONA (yığının en altına) gider[cite: 2].
    -- flippedToBottom = reverse flippedToTop
    flippedToBottom = flipP (length xs) flippedToTop
    
    -- Artık en büyük eleman en sonda ve yeri garanti.
    -- Geriye kalan (init) yani son eleman hariç kısmı tekrar sıralamak üzere ayır[cite: 2].
    remaining = init flippedToBottom

main :: IO ()
main = do
    putStrLn "--- Q5: Pancake Sort Testi ---"
    
    let test1 = [5, 2, 4, 1]
    putStrLn $ "\nTest 1 Baslangic: " ++ show test1
    putStrLn $ "Test 1 Sirali:    " ++ show (pancakeSort test1)
    
    let test2 = [8, 3, 5, 9, 1, 4, 7]
    putStrLn $ "\nTest 2 Baslangic: " ++ show test2
    putStrLn $ "Test 2 Sirali:    " ++ show (pancakeSort test2)