module Main where

-- (j) şıkkında verilen sonsuz döngü fonksiyonu[cite: 3].
-- Eğer listedeki çift sayılar bitmezse (ki çift bir sayıya 2 eklemek onu yine çift yapar),
-- fonksiyon recursive olarak kendini çağırmaya devam eder.
process :: [Int] -> [Int]
process xs
    | null (filter even xs) = xs
    | otherwise             = process (map (+2) xs)

main :: IO ()
main = do
    putStrLn "--- Q4 List Comprehension & Higher-Order Functions Sonuclari ---"

    -- (a) 1'den 80'e kadar olan, tek (odd) ve 3'e tam bölünemeyen sayıların kareleri[cite: 3].
    putStrLn "\n(a) Sonucu:"
    print [x^2 | x <- [1..80], odd x, x `mod` 3 /= 0]

    -- (b) Önce 3'ten büyükleri filtrele [6,5,8], sonra her birini 2 ile çarpıp 1 çıkar (2x-1)[cite: 3].
    putStrLn "\n(b) Sonucu:"
    print $ map (\x -> 2*x - 1) (filter (>3) [1,6,2,5,3,8])

    -- (c) x, y ve z için iç içe döngüler kurulur. (x+y+z) çift (even) olduğunda,
    -- sadece (x,y) ikilisini listeye ekler[cite: 3].
    putStrLn "\n(c) Sonucu:"
    print [(x,y) | x <- [1..4], y <- [x..4], z <- [y..4], even (x+y+z)]

    -- (d) Ana listedeki her bir iç liste için: sadece çift sayıları alıp topla[cite: 3].
    -- [1,2,3]->2, [4,5]->4, [6,7,8]->6+8=14
    putStrLn "\n(d) Sonucu:"
    print $ map (\xs -> sum (filter even xs)) [[1,2,3], [4,5], [6,7,8]]

    -- (e) x için [1,2,3] ve y için 1'den x'e kadar olan sayılar seçilip (x-y) işlemi yapılır[cite: 3].
    putStrLn "\n(e) Sonucu:"
    print [x-y | x <- [1,2,3], y <- [1..x]]

    -- (f) İç listelerdeki tek (odd) sayıları filtrele. Eğer sonuçta boş bir liste kalırsa ([]),
    -- o boş listeyi en dıştaki filtre ile komple çöpe at[cite: 3].
    putStrLn "\n(f) Sonucu:"
    print $ filter (\xs -> xs /= []) (map (filter odd) [[1,2], [2,4], [3,5], [6,7]])

    -- (g) 1'den 5'e kadar (x) dön. Her x için 1'den x'e kadar olan çift sayıları (y) bul.
    -- Çıkan bu iç listelerin uzunluklarını (length) hesapla[cite: 3].
    putStrLn "\n(g) Sonucu:"
    print $ map length [[y | y <- [1..x], even y] | x <- [1..5]]

    -- (h) İlk listenin elemanlarını 2 ile çarp ([2,4,6,8]), 
    -- ikinci listedeki tek sayıları al ([1,3,5,7]).
    -- zipWith (+) ile bu iki listeyi karşılıklı olarak topla[cite: 3].
    putStrLn "\n(h) Sonucu:"
    print $ zipWith (+) (map (*2) [1,2,3,4]) (filter odd [1..8])

    -- (i) 1'den 8'e kadar olan sayılara 1 ekle ([2,3..9]). 
    -- Bunların içinden hem çift (even) hem de 7'den küçük olanları al[cite: 3].
    putStrLn "\n(i) Sonucu:"
    print [x | x <- map (+1) [1..8], even x, x < 7]

    -- (j) Fonksiyon non-terminating (sonsuz döngü) olduğu için çalıştırılamaz[cite: 3].
    putStrLn "\n(j) Sonucu:"
    putStrLn "NON-TERMINATING (Sonsuz donguye girecegi icin yoruma alindi.)"
    -- Eger sonsuz donguyu bizzat gormek istersen, asagidaki satirin basindaki yorumu (--) kaldirabilirsin.
    -- print (process [1,2,3])