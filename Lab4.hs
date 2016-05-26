-- Zad. 1 zbiór potęgowy

powerlist :: [a] -> [[a]]
powerlist [] = [[]]
powerlist (x:xs) = powerlist xs ++ map (x:) (powerlist xs)

-- Zad. 2 podzbiór

ifsubset :: Eq a => [a] -> [a] -> Bool
ifsubset [] y = True
ifsubset (x:xs) y | elem x y == True = ifsubset xs y
                  | otherwise = False

-- Zad. 3 iloczyn

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] y = []
intersection (x:xs) y | elem x y == True = [x] ++ intersection xs y
                      | otherwise = intersection xs y

-- Zad. 4 suma

union :: Eq a => [a] -> [a] -> [a]
union [] y = y
union (x:xs) y | elem x y == True = union xs y
               | otherwise = [x] ++ union xs y

-- Zad. 5 a)

-- 6 / (12 / (24 / (8 / 2))) = 6 / (12 / (24 / 4)) = 6 / (12 / 6) = 6 / 2 = 3

-- Zad. 5 b)

-- 1>2 && (3>2 && (5==5 && True)) = 1>2 && (3>2 && True) = 1>2 && True = False

-- Zad. 5 c)

-- max 3 (max 6 (max 12 (max 4 (max 55 (max 11 18))))) = max 3 (max 6 (max 12 (max 4 (max 55 18)))) = max 3 (max 6 (max 12 (max 4 55))) = max 3 (max 6 (max 12 55)) = max 3 (max 6 55) = max 3 55 = 55

-- Zad. 5 d)

-- max 3 (max 6 (max 12 (max 4 (max 55 (max 11 81))))) = max 3 (max 6 (max 12 (max 4 (max 55 81)))) = max 3 (max 6 (max 12 (max 4 81))) = max 3 (max 6 (max 12 81)) = max 3 (max 6 81) = max 3 81 = 81

-- Zad. 5 e)

-- ((((54 + 6) / 2 + 10) / 2 + 4) / 2 + 24) / 2 = 18

-- Zad. 5 f)

-- ((((54 + 2) / 2 + 4) / 2 + 10) / 2 + 6) / 2 = 9.5

-- Zad. 5 g)

-- ((64 / 4) / 2) / 4 = (16 / 2) / 4 = 8 / 4 = 2

-- Zad. 5 h)

-- 2 * (2 * (2 * 8 + 1) + 2) + 3 = 2 * (2 * 17 + 2) + 3 = 2 * 36 + 3 = 75

-- Zad. 6 elem przy pomocy foldl

foldelem :: Eq a => a -> [a] -> Bool
foldelem x = foldl (\q y -> q || (x == y)) False

-- Zad. 7 a)

foldlmap f xs = foldl (\ys x -> ys ++ [f x]) [] xs

-- Zad. 7 b) map przy pomocy foldr

foldrmap f xs = foldr (\x ys -> (f x):ys) [] xs

-- Zad. 8 a) last za pomocą fold1

foldlast :: [a] -> a
foldlast = foldl1 (\x y -> y)

-- Zad. 8 b) head za pomocą fold1

foldhead :: [a] -> a
foldhead = foldr1 (\x y -> x)

-- Zad. 8 c) maksimum za pomocą fold1

foldmax :: Ord a => [a] -> a
foldmax = foldr1 (\x y -> max x y)

-- Zad.9 zip i unzip

zip' :: [a] -> [b] -> [(a,b)]
zip' [] y = []
zip' x [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' x = (map fst x, map snd x)

-- Zad.10 a) średnia arytmetyczna dwóch liczb

arimean = \x y -> (x + y) / 2

-- Zad.10 b) średnia geometryczna dwóch liczb

geomean = \x y -> sqrt (x * y)

-- Zad. 10 c) średnia harmoniczna dwóch liczb

harmean = \x y -> 2 / (1 / x + 1 / y)

-- Zad. 10 d) objętość stożka

conevol = \r h -> pi * r * r * h / 3

-- Zad. 10 e) pole trapezu

traparea = \a b h -> (a + b) * h / 2
