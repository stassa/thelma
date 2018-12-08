:-module(tiny_kinship, [background_knowledge/1
		       ,positive_example/2
		       ,negative_example/2
		       ,metarules/1
		       ,father/2
		       ,mother/2
		       ,parent/2
		       ]).

background_knowledge([father/2,mother/2]).

metarules([chain,identity]).

father(stathis, kostas).
father(stefanos, dora).
father(kostas, stassa).

mother(alexandra, kostas).
mother(paraskevi, dora).
mother(dora, stassa).

parent(X, Y):-
	father(X,Y).
parent(X, Y):-
	mother(X,Y).

positive_example(grandfather,grandfather(A,B)):-
	grandfather(A,B).

negative_example(grandfather,grandfather(A,B)):-
	grandmother(A,B).

% Target theory.
grandfather(A,B):- father(A,C), parent(C,B).

% Not the target theory:
grandmother(A,B):- mother(A,C), parent(C,B).
