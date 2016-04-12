-- Zad. 1 samochody

data Moto = Ferrari | Porshe | AstonMartin | Renault | Chevrolet
            deriving (Show)

type Kraj = [Char]

brand :: Kraj -> Moto
brand x = case x of
          "Wlochy" -> Ferrari
          "Niemcy" -> Porshe
          "Wielka Brytania" -> AstonMartin
          "Francja" -> Renault
          "USA" -> Chevrolet

speed :: Moto -> Int
speed x = case x of
          Ferrari -> 310
          Porshe -> 300
          AstonMartin -> 305
          Renault -> 260
          Chevrolet -> 270

-- Zad. 2 przechodzenie drzewa

data Tree a = Empty | Node a (Tree a) (Tree a)

t :: Tree Int
t = Node 1 (Node 2 (Node 4 Empty Empty)
                   (Node 5 Empty (Node 8 Empty Empty)))
           (Node 3 (Node 6 Empty (Node 9 Empty Empty))
                   (Node 7 Empty Empty))

s :: Tree Char
s = Node 'a' (Node 'b' Empty (Node 'd' (Node 'f' Empty Empty) Empty)) (Node 'c' (Node 'e' Empty (Node 'g' Empty Empty)) Empty)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

-- wydruki z GHCi :
-- *Main> inorder t
-- [4,2,5,8,1,6,9,3,7]
-- *Main> preorder t
-- [1,2,4,5,8,3,6,9,7]
-- *Main> postorder t
-- [4,8,5,2,9,6,7,3,1]
-- *Main> inorder s
-- "bfdaegc"
-- *Main> preorder s
-- "abdfceg"
-- *Main> postorder s
-- "fdbgeca"

-- Zad. 3 sprawdzanie, czy element należy do drzewa

-- według metody preorder

prelem :: Eq a => Tree a -> a -> Bool
prelem Empty x = False
prelem (Node a l r) x | a == x = True
                      | prelem l x == True = True
                      | otherwise = prelem r x

-- według metody inorder

inelem :: Eq a => Tree a -> a -> Bool
inelem Empty x = False
inelem (Node a l r) x | inelem l x == True = True
                      | a == x = True
                      | otherwise = inelem r x

-- według metody postorder

postelem :: Eq a => Tree a -> a -> Bool
postelem Empty x = False
postelem (Node a l r) x | postelem l x == True = True
                        | postelem r x == True = True
                        | a == x = True
                        | otherwise = False

-- Zad. 4 przechodzenie wszerz

bfwalk :: Tree a -> [a]
bfwalk Empty = []
bfwalk tr = bf [tr]
    where
        bf [] = []
        bf xs = map root xs ++ bf (concat (map children xs))

        root (Node x _ _) = x
        children (Node _ Empty Empty) = []
        children (Node _ Empty b) = [b]
        children (Node _ a Empty) = [a]
        children (Node _ a b) = [a,b] 
