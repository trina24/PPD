% Zad. 2

iloczyn([],_,[]).
iloczyn([Head1|Tail1], Y, [Head1|Tail]) :- member(Head1, Y), iloczyn(Tail1, Y, Tail).
iloczyn([_|Tail], Y, Z) :- iloczyn(Tail, Y, Z).

suma([],[],[]).
suma(X, Y, [Head|Tail]) :- (member(Head,X); member(Head,Y)), delete(X,Head,X1), delete(Y,Head,Y1), suma(X1,Y1,Tail).


roznica(X,X,[]).
roznica([Head|Tail],Y,[]) :- member(Head,Y), roznica(Tail,Y,[]).
roznica(X,[Head|Tail],X) :- not(member(Head,X)), roznica(X, Tail, X).
roznica(X,Y,[Head|Tail]) :- member(Head,X), not(member(Head,Y)), delete(X,Head,X1), roznica(X1,Y,Tail).
