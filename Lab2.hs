-- Zad. 1 przedostatni element listy
penultimate :: [a] -> a
penultimate (x) | length x < 2 = error "too less elements"
                | otherwise = last (init x)
-- Zad. 2 a) drugi element listy
second :: [a] -> a
second (x) | length (x) > 1 = x !! 1
           | otherwise = error "too less elements"
-- Zad. 2 b) trzeci element listy
third :: [a] -> a
third (x) | length (x) > 2 = x !! 2
           | otherwise = error "too less elements"
-- Zad. 3 odwracanie listy
my_reverse :: [a] -> [a]
my_reverse [] = []
my_reverse (x) = [last x] ++ my_reverse (init x)
-- Zad. 4 przestawienie pierwszego i ostatniego elementu
change :: [a] -> [a]
change [] = []
change [x] = [x]
change (x) = [last x] ++ (init (tail x)) ++ [head x]
-- Zad. 5 a) liczba dodatnich parzystych
even_positive :: [Int] -> Int
even_positive (x) = length (filter (>0) (filter even x))
-- Zad. 5 b) liczba podzielnych przez 3 od 1 do n
div_by_3 :: Int -> Int 
div_by_3 n = length [x | x <- [1 .. n], mod x 3 == 0]
-- Zad. 5 c) suma podzielnych przez 3 od 1 do n
sum_div_by_3 :: Int -> Int
sum_div_by_3 n = sum [x | x <- [1 .. n], mod x 3 == 0]
-- Zad. 6 parzystość długości listy
even_or_odd_length :: [a] -> String
even_or_odd_length x | mod (length x) 2 == 0 = "even"
                     | otherwise = "odd"
-- Zad. 7 a) lista kwadratów (z map)
sqrlist_a :: Num a => [a] -> [a]
sqrlist_a (x) = map (^2) (x)
-- Zad. 7 b) lista kwadratów (bez map)
sqrlist_b :: Num a => [a] -> [a]
sqrlist_b (x) = [y^2 | y <- x]
-- Zad. 8 liczba wystąpień na liście
count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (list) | x == head list = 1 + count x (drop 1 list)
               | otherwise = count x (drop 1 list)
-- Zad. 9 powielanie obiektu
duplicate :: a -> Int -> [a]
duplicate a 0 = []
duplicate a n = (a : duplicate a (n-1))
-- Zad. 10 sprawdzanie, czy lista jest palindromem
if_palindrome :: Eq a => [a] -> Bool
if_palindrome (x) | x == reverse x = True
                  | otherwise = False
-- Zad. 11 usuwanie pierwszego elementu
remove_first :: [a] -> [a]
remove_first [] = error "empty list"
remove_first (x) = tail x
-- Zad. 12 usuwanie n-tego elementu
remove_nth :: Int -> [a] -> [a]
remove_nth n x | length x < n = error "too less elements"
               | otherwise = take (n-1) x ++ drop n x
-- Zad. 13 potęga akumulatorowa
powtail a n = powtemp a n 1
powtemp a n m = if n == 0 then m
                else powtemp(a)(n-1)(a*m)
-- Zad. 14 sprawdzenie, czy wszystkie elementy jednej listy występują na drugiej
if_inc :: Eq a => [a] -> [a] -> Bool
if_inc [] y = True
if_inc (x:xs) y | elem x y == True = if_inc xs y
                | otherwise = False
-- Zad. 15 przestawianie elementów w krotkach
tuple_reverse :: [(a,b)] -> [(b,a)]
tuple_reverse [] = []
tuple_reverse [(a,b)] = [(b,a)]
tuple_reverse (x) = tuple_reverse [head x] ++ tuple_reverse (tail x)
