:-module(tiny_kinship, [background_knowledge/2
		       ,metarules/2
		       ,positive_example/2
		       ,negative_example/2
		       ,father/2
		       ,mother/2
		       ,parent/2
		       ,male/1
		       ,female/1
		       ]).

background_knowledge(father/2,[parent/2,male/1]).
background_knowledge(grandfather/2,[father/2,mother/2]).
background_knowledge(male/2,[male/1,female/1]).

% Learn father/2
metarules(father/2,[chain,projection]).
metarules(grandfather/2,[chain,inverse]).
metarules(male/2,[identity,projection]).

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

male(stathis).
male(stefanos).
male(kostas).

female(dora).
female(stassa).
female(alexandra).
female(paraskevi).

positive_example(grandfather/2,grandfather(A,B)):-
	grandfather(A,B).
positive_example(father/2,father(A,B)):-
	father(A,B).
positive_example(male/2,male(A,A)):-
	male(A).

negative_example(grandfather/2,grandfather(A,B)):-
	grandmother(A,B).
negative_example(father/2,father(A,B)):-
	mother(A,B).
negative_example(male/2,male(A,A)):-
	female(A).

% Target theory.
grandfather(A,B):- father(A,C), parent(C,B).

% Not the target theory:
grandmother(A,B):- mother(A,C), parent(C,B).
