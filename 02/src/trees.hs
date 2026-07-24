data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
data Expr = Val Int | Add Expr Expr | Neg Expr deriving (Show, Eq)

-- (a) Sadece kök düğümün (root) iki çocuğunun da "Nil olmayan" (Node olan) 
-- durumunda True dönen pattern[cite: 3].
hasTwoChildren :: Tree a -> Bool
hasTwoChildren (Node _ (Node _ _ _) (Node _ _ _)) = True
hasTwoChildren _ = False

-- (b) Kökün sağ çocuğunun boş olmadığı (Node olduğu) durumda Sol Rotasyon yapan pattern[cite: 3].
-- Eşitliğin sağı verilmiş: Node y (Node x left middle) right[cite: 3]
-- Demek ki gelen ağaçta x kök, y sağ çocuk olmalı.
rotateLeft :: Tree a -> Tree a
rotateLeft (Node x left (Node y middle right)) = Node y (Node x left middle) right
rotateLeft tree = tree

-- (c) Kökün sol çocuğu yok (Nil), ama sağ çocuğu bir yaprak (çocukları Nil olan Node)[cite: 3].
specialTree :: Tree a -> Bool
specialTree (Node _ Nil (Node _ Nil Nil)) = True
specialTree _ = False

-- (d) Sadeleştirme kuralları[cite: 3]. 
-- İlk boşluk: Sıfır ile toplamayı (Add e (Val 0)) yakalar[cite: 3].
-- İkinci boşluk: İki ardışık negatiflemeyi (Neg (Neg e)) yakalar[cite: 3].
simplify :: Expr -> Expr
simplify (Add e (Val 0)) = simplify e
simplify (Neg (Neg e)) = simplify e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify (Neg e) = Neg (simplify e)
simplify e = e