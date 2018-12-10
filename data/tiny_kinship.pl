:-module(tiny_kinship, [background_knowledge/1
		       ,positive_example/2
		       ,negative_example/2
		       ,metarules/1
		       ,father/2
		       ,mother/2
		       ,parent/2
		       ,male/1
		       ,female/1
		       ]).

% Learn father/2
%background_knowledge([parent/2,male/1]).
% Learn grandfather/2
background_knowledge([father/2,mother/2]).
% Learn male/1
%background_knowledge([male/1,female/1]).

% Learn father/2
%metarules([chain,projection]).
% Learn grandfather/2
metarules([chain,inverse]).
% Learn male/1
%metarules([identity,projection]).

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

positive_example(male,male(A,A)):-
	male(A).
positive_example(father,father(A,B)):-
	father(A,B).
positive_example(grandfather,grandfather(A,B)):-
	grandfather(A,B).

negative_example(grandfather,grandfather(A,B)):-
	grandmother(A,B).
negative_example(father,father(A,B)):-
	mother(A,B).
negative_example(male,male(A,A)):-
	female(A).

% Target theory.
grandfather(A,B):- father(A,C), parent(C,B).

% Not the target theory:
grandmother(A,B):- mother(A,C), parent(C,B).
