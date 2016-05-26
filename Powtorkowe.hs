-- Zadania powtórkowe

-- Zad. 7 a) sortowanie przez wstawianie

mnm :: Ord a => [a] -> a
mnm [] = error "empty list"
mnm [x] = x
mnm (x:xs) = min x (mnm xs)

removeFst :: Eq a => a-> [a] -> [a]
removeFst x [] = []
removeFst x (y:ys) | x == y = ys
                   | otherwise = y : (removeFst x ys)

sort [] = []
sort xs = m : (sort (removeFst m xs)) where m = mnm xs

-- Zad. 7 b) sortowanie bąbelkowe

bubblesort :: Ord a => [a] -> [a]
bubblesort s = case bsort s of
               t | t == s    -> t
                 | otherwise -> bubblesort t
               where bsort [x] = [x]
                     bsort (x:xs) | x > head xs = [head xs] ++ bubblesort (x:drop 1 xs)
                                  | otherwise = [x] ++ bubblesort xs

-- Zad. 8 połączenie posortowanych list

con :: Ord a => [a] -> [a] -> [a]
con x [] = x
con [] y = y
con (x:xs) (y:ys) | x <= y = con xs (x:y:ys)
                  | otherwise = con xs (ins x (y:ys))
                              where ins :: Ord a => a -> [a] -> [a]
                                    ins a [] = [a]
                                    ins a (b:bs) | a <= b = (a:b:bs)
                                                 | otherwise = (b: ins a bs)

-- Zad. 9 sprawdzenie, czy jedno drzewo jest poddrzewem drugiego

data Tree a = Empty | Node a (Tree a) (Tree a)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x:xs) (y:ys) | length (x:xs) > length (y:ys) = False
                      | x == y = sublist xs ys
                      | otherwise = sublist (x:xs) ys                    

subtree :: Eq a => Tree a -> Tree a -> Bool
subtree t s = sublist (inorder t) (inorder s)

-- Zad. 10 sprawdzenie, czy drzewo jest uporządkowane

lsorted :: Ord a => [a] -> Bool
lsorted [] = True
lsorted [x] = True
lsorted (x:xs) | x <= head xs = True && lsorted xs
               | otherwise = False 


trsorted :: Tree Int -> Bool
trsorted Empty = True
trsorted t = lsorted (inorder t)

-- Zad. 11 a) długość najdłuższej gałęzi
