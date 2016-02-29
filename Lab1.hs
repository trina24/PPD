-- Zad. 2 a)
f x | x > 2 = x^2
     | x <= 2 && x > 0 = x-1
     | otherwise = abs(x)
-- Zad. 2 b) NWD
nwd :: Int -> Int -> Int
nwd a 0 = a
nwd a b = nwd b (mod a b)
-- Zad 2. c) NWW
nww :: Int -> Int -> Int
nww a b = a * b `div` nwd a b
-- Zad. 2 d) Nierówność trójkąta
iftriangle a b c = a+c >= b && a+b >= c && b+c >= a
-- Zad. 2 e) Objętość stożka
v r h = pi * r * r * h / 3
-- Zad. 2 f) Tworząca stożka
l r h = sqrt(r^2 + h^2)
-- Zad. 2 g) Potęga rekurencyjna
pow a 0 = 1
pow a n = a * pow(a)(n-1)
-- Zad. 2 h) Potęga akumulatorowa
powtail a n = powtemp a n 1
powtemp a n m = if n == 0 then m
                else powtemp(a)(n-1)(a*m) 
-- Zad. 2 i) 10-ty element Fibonacciego
fib n = if n == 0 then 1
        else if n == 1 then 1
             else fib(n-1) + fib(n-2)
iffibtenth x = x == fib 10
-- Zad. 2 j) Element Fibonacciego
iffib x = fibtemp x 4
fibtemp x n = if x /= fib n && n < 10 then fibtemp(x)(n+1)
                                      else x == fib n
