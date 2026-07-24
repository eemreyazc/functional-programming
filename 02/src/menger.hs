module Main where

type Point = (Float, Float, Float)
type Triangle = (Point, Point, Point)
type Shape = [Triangle]

cubeAt :: Point -> Float -> Shape
cubeAt (cx, cy, cz) s =
  let 
      -- Küpün merkezinden köşelere olan mesafe (kenar uzunluğunun yarısı)
      h = s / 2.0
      
      -- Küpün 8 köşesinin (vertex) koordinatları
      v0 = (cx - h, cy - h, cz - h) -- Sol  - Alt - Arka
      v1 = (cx + h, cy - h, cz - h) -- Sağ  - Alt - Arka
      v2 = (cx + h, cy + h, cz - h) -- Sağ  - Üst - Arka
      v3 = (cx - h, cy + h, cz - h) -- Sol  - Üst - Arka
      v4 = (cx - h, cy - h, cz + h) -- Sol  - Alt - Ön
      v5 = (cx + h, cy - h, cz + h) -- Sağ  - Alt - Ön
      v6 = (cx + h, cy + h, cz + h) -- Sağ  - Üst - Ön
      v7 = (cx - h, cy + h, cz + h) -- Sol  - Üst - Ön

  in [
      -- Bir küpün 6 yüzeyi vardır. Her yüzey 2 adet üçgenden oluşur (Toplam 12 üçgen).
      
      -- 1. Ön Yüzey (Front Face)
      (v4, v5, v6), (v4, v6, v7),
      
      -- 2. Arka Yüzey (Back Face)
      (v1, v0, v3), (v1, v3, v2),
      
      -- 3. Üst Yüzey (Top Face)
      (v3, v2, v6), (v3, v6, v7),
      
      -- 4. Alt Yüzey (Bottom Face)
      (v4, v5, v1), (v4, v1, v0),
      
      -- 5. Sağ Yüzey (Right Face)
      (v5, v1, v2), (v5, v2, v6),
      
      -- 6. Sol Yüzey (Left Face)
      (v0, v4, v7), (v0, v7, v3)
     ]

mengerSponge :: Int -> Shape
mengerSponge n = menger n (0.0, 0.0, 0.0) 60.0

menger :: Int -> Point -> Float -> Shape
menger 0 p s = cubeAt p s
menger n (x, y, z) s = 
    concat [ menger (n - 1) (x + i * subS, y + j * subS, z + k * subS) subS
           | i <- [-1, 0, 1]
           , j <- [-1, 0, 1]
           , k <- [-1, 0, 1]
           , let zeros = length (filter (== 0) [i, j, k])
           , zeros < 2 
           ]
  where
    subS = s / 3.0

main :: IO ()
main = do
    putStrLn "--- Menger Sponge Test Sonuclari ---"
    putStrLn $ "mengerSponge 0 ücgen sayisi: " ++ show (length (mengerSponge 0)) ++ " (Beklenen: 12)"
    putStrLn $ "mengerSponge 1 ücgen sayisi: " ++ show (length (mengerSponge 1)) ++ " (Beklenen: 240)"
    putStrLn $ "mengerSponge 2 ücgen sayisi: " ++ show (length (mengerSponge 2)) ++ " (Beklenen: 4800)"
