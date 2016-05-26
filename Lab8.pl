% Zad.4 nwd

nwd(X, Y, Z) :- X == Y, Z is X.
nwd(X, Y, Z) :- X > Y, New is X-Y, nwd(Y, New, Z).
nwd(X, Y, Z) :- X < Y, New is Y-X, nwd(X, New, Z).

% Zad. 5 znajomi

jarosz(ola).
jarosz(pawel).
jarosz(jan).
jarosz(ewa).

kawa(iza).
kawa(piotr).
kawa(pawel).

ksiazki(ola).
ksiazki(iza).
ksiazki(pawel).

sport(iza).
sport(ola).
sport(piotr).
sport(pawel).

lubi(ola,X) :- jarosz(X), sport(X).
lubi(ewa,X) :- not(kawa(X)), jarosz(X).
lubi(iza, X) :- ksiazki(X); (sport(X), not(kawa(X))).
lubi(janek, X) :- sport(X).
lubi(piotr, X) :- (jarosz(X), sport(X)); (ksiazki(X)).
lubi(pawel, X) :- jarosz(X), sport(X), ksiazki(X).

przyjaciele(X,Y) :- lubi(X,Y), lubi(Y,X), X\=Y.

% Zad. 8 maksimum na liście

max([Head], Head).
max([Head|Tail], Head):- max(Tail, X), Head >= X.
max([Head|Tail], X):- max(Tail, X), X > Head.

% Zad. 9 sprawdzanie, czy lista jest początkiem innej

poczatek([Head],[Head|_]).
poczatek([Head1|Tail1],[Head2|Tail2]) :- Head1 == Head2, poczatek(Tail1,Tail2).

% Zad. 10 ostatni element listy

ostatni([X],X).
ostatni([_|Tail],X) :- ostatni(Tail,X).
