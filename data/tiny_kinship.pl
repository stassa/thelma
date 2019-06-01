:-module(tiny_kinship, [background_knowledge/2
		       ,metarules/2
		       ,positive_example/2
		       ,negative_example/2
		       ,ancestor/2
		       ,father/2
		       ,mother/2
		       ,parent/2
		       ,male/1
		       ,female/1
		       ]).

/** <module> Experiment file for a small family domain.

Includes examples for ancestor/2, father/2, grandfather/2 and male/2.

Usage
=====

1. Set depth_limits(2,1).

2. Remember to initialise the experiment:

?- initialise_experiment.
true.

3. Run a query:

?- experiment_data(ancestor/2,_Pos,_Neg,_BK,_MS), learn(_Pos,_Neg,_Prog), print_clauses(_Prog).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
ancestor(A,B):-parent(A,C),ancestor(C,B).
ancestor(A,B):-parent(A,B).
true .

Change ancestor/2 to father/2, grandfather/2 or male/2 to learn
definitions for those predicates.

4. Be nice and cleanup afterwards:

?- cleanup_experiment.
true.
*/

background_knowledge(ancestor/2,[parent/2]).
background_knowledge(father/2,[parent/2,male/1]).
background_knowledge(grandfather/2,[father/2,mother/2]).
background_knowledge(male/2,[male/1,female/1]).

% Learn father/2
metarules(ancestor/2,[tailrec,identity]).
metarules(father/2,[chain,projection]).
metarules(grandfather/2,[chain,inverse]).
metarules(male/2,[identity,projection]).

father(stathis, kostas).
father(stefanos, dora).
father(kostas, stassa).

mother(alexandra, kostas).
mother(paraskevi, dora).
mother(dora, stassa).

ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,parent(Z,Y).

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

positive_example(ancestor/2,ancestor(A,B)):-
	ancestor(A,B).
positive_example(grandfather/2,grandfather(A,B)):-
	grandfather(A,B).
positive_example(father/2,father(A,B)):-
	father(A,B).
positive_example(male/2,male(A,A)):-
	male(A).

negative_example(ancestor/2,ancestor(A,B)):-
	ancestor(B,A).
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
