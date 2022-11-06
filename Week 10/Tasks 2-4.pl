% student(Name, Group)
student(alisa, 2).
student(bob, 1).
student(chloe, 2).
student(denise, 1).
student(edward, 2).

% friend(Name, Name)
friend(alisa, bob).
friend(alisa, denise).
friend(bob, chloe).
friend(bob, edward).
friend(chloe, denise).
friend(denise, edward).

% parent(Parent, Child)
parent(marjorie, bart).
parent(marjorie, lisa).
parent(marjorie, maggie).
parent(homer, bart).
parent(homer, lisa).
parent(homer, maggie).
parent(abraham, homer).
parent(mona, homer).
parent(jacqueline, marjorie).
parent(jacqueline, patty).
parent(jacqueline, selma).
parent(clancy, marjorie).
parent(clancy, patty).
parent(clancy, selma).
parent(selma, ling).

% unary(Number)
unary(z).
unary(s(X)) :- unary(X).

% Task 2
groupmates( X, Y ) :- 
    X \= Y,
    student(X, A),
    student(Y, A).

% Task 3
relative( X, Y ) :-
    X \= Y,
    parent( Z, X ),
    parent( Z, Y ).
relative( X, Y ) :-
    X \= Y,
    parent( Z, X ), 
    relative( Z, Y ).
relative( X, Y ) :-
    X \= Y,
    parent( Z, Y ), 
    relative( X, Z ).

% Task 4
% a)
double( z, z ).
double( s(X), s(s(Y)) ) :- double(X, Y), unary(X), unary(Y).

% b)
leq( z, s(_) ).
leq( z, z ).
leq( s(X), s(Y) ) :- leq(X, Y), unary(X), unary(Y).

% c)
add(z, Y, Y).
add(s(X), Y, s(R)) :- add(X, Y, R).

mult( z, _, z ).
mult( s(_), z, z ).
mult( s(X), s(Y), R ) :- 
    add( s(Y), R1, R ),
    mult( X, s(Y), R1 ),
    unary(X), unary(Y), unary(R).

% d)
powerOf2( z, s(z) ).
powerOf2( s(X), s(Y) ) :-
	double( Y1, s(Y) ),
	powerOf2( X, Y1 ).
    % two_power_n_minus_one(X, Y). 











