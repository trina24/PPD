% Zad. 3

% Zakomentowane poniżej fragmenty kodu realizują zapisywanie faktów do pliku
% oraz poszerzanie bazy prologowej o zawartość pliku. Ponieważ nie udało mi
% się zrealizować usuwania faktów z pliku, wyłączyłam z kodu zapisywanie i
% czytanie, ponieważ program przy przeglądaniu listy studentów nadal widziałby
% fakty, które mogły zostać usunięte w programie, ale ciągle są w pliku.
% Ponadto po dołączeniu faktów z pliku do bazy nie można było już dodawać
% nowych - odmowa dostępu.

start:-
    write('Aby przejsc do trybu dodwania, wpisz dodaj.'), nl,
    write('Aby przejsc do trybu usuwania, wpisz usun.'), nl,
    write('Aby przegladac, wpisz przegladaj.'), nl,
    read(X), dzialaj(X).

dzialaj(dodaj) :- dodanie.
dzialaj(usun) :- usuniecie.
dzialaj(przegladaj) :- przegladaj.

dodanie:-
    write('Aby dodac, wpisz [imie, nazwisko, indeks, kierunek].'), nl,
    read(Input_List),
    dodaj(Input_List).

usuniecie:-
    write('Aby usunac, wpisz [imie, nazwisko, indeks, kierunek].'), nl,
    write('Mozesz usunac wiele rekordow, zamiast parametru wpisujac _ .'), nl,
    read(Input_List),
    usun(Input_List).

dodaj([X1, X2, X3, X4]) :-
    Fact =.. [student, X1, X2, X3, X4],
    assertz(Fact),
%    append('knowledge'),
%    write('student('),
%    write(X1),
%    write(','),
%    write(X2),
%    write(','),
%    write(X3),
%    write(','),
%    write(X4),
%    write(').'),
%    nl,
%    told,
    start.

usun([X1, X2, X3, X4]) :-
    Fact =.. [student, X1, X2, X3, X4],
    retractall(Fact),
    start.

przegladaj :-
%    consult('knowledge'),
    listing(student),
    start.
