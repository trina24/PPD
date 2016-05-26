import Control.Monad

-- Zad. 1 moduł

-- module Mojzbior where

podzbior :: Eq a => [a] -> [a] -> Bool
podzbior [] y = True
podzbior (x:xs) y | elem x y == True = podzbior xs y
                  | otherwise = False

iloczyn :: Eq a => [a] -> [a] -> [a]
iloczyn [] y = []
iloczyn (x:xs) y | elem x y == True = [x] ++ iloczyn xs y
                 | otherwise = iloczyn xs y

suma :: Eq a => [a] -> [a] -> [a]
suma [] y = y
suma (x:xs) y | elem x y == True = suma xs y
              | otherwise = [x] ++ suma xs y

roznica :: Eq a => [a] -> [a] -> [a]
roznica [] y = []
roznica (x:xs) y | elem x y == True = roznica xs y
                 | otherwise = [x] ++ roznica xs y

-- Zad. 2

-- m >> k =  m >>= \_ -> k

-- Zad. 3 suma, iloczyn, różnica liczb

dwieliczby = do putStr "Podaj pierwsza liczbe: "
                n <- getLine
                let x = read n
                putStr "Podaj druga liczbe: "
                n <- getLine
                let y = read n
                    z1 = x + y
                    z2 = x * y
                    z3 = x - y
                putStrLn ("Suma: " ++ show z1)
                putStrLn ("Iloczyn: " ++ show z2)
                putStrLn ("Roznica: " ++ show z3)

-- Zad. 4 imię i nazwisko

imie = do putStr "Podaj imie: "
          im <- getLine
          putStr "Podaj nazwisko: "
          naz <- getLine
          let x = [head im] ++ [head naz]
          putStr ("Inicjaly: " ++ show x ++ "\n")

-- Zad. 5 gra

gameloop 0 = do putStr "Przegrales! \n"
gameloop n = do putStr "Podaj liczbe (0-99): "
                x <- getLine
                let num = read x :: Int
                when (num == 37) $ do
                     putStr "Gratulacje, wygrales! \n"
                when (num < 37) $ do
                     putStr "Twoja liczba jest za mala! \n"
                     gameloop (n-1)
                when (num > 37) $ do
                     putStr "Twoja liczba jest za duza! \n"
                     gameloop (n-1)
game = gameloop 10

-- Zad. 6 adres, adres e-mail

class Adres a where
    ifmail :: a -> Bool
    ifmail a = False

data Email = Mail1 | Mail2 | Mail3

instance Adres Email where
    ifmail a = True

mail :: Email -> String
mail x = case x of
         Mail1 -> "katarzyna.kalek@wp.pl"
         Mail2 -> "katrina1922@gmail.com"
         Mail3 -> "kk64239@st.amu.edu.pl"

showaddress :: Email -> String
showaddress x | ifmail x == True = mail x
