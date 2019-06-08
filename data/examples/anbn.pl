:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,'A'/2
	       ,'B'/2
	       ]).

/** <module> Experiment file for a^nb^n grammar.

Setup to learn a^nb^n from three positive examples with one invented
predicate.

Usage
=====

1. Ensure this file is set as the current experiment file in
configuration.pl

==
experiment_file('data/examples/anbn.pl',anbn).
==

2. Set sufficient clause limits in configuration.pl:

==
depth_limits(3,1).
==

3. Run the following query to train Thelma on the data in this file:

==
?- learn('S'/2,_Prog),print_clauses(_Prog).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
% Clauses: 2; Invented: 1
% Clauses: 3; Invented: 0
% Clauses: 3; Invented: 1
'S'(A,B):-'S_1'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
'S_1'(A,B):-'A'(A,C),'S'(C,B).
true .
==

This translates to the following DCG notation:

==
'S' --> 'A', 'B'.
'S' --> 'S_1', 'B'.
'S_1' --> 'A', 'S'.
==

To test the grammar correctly recognises a^nb^n strings up to n =
100,000, past the learned hypothesis at the start of this file,
reconsult it, then run this query:

==
?- _N = 100_000, findall(a, between(1,_N,_), _As), findall(b, between(1,_N,_),_Bs), append(_As,_Bs,_AsBs), anbn:'S'(_AsBs,[]).
true ;
==

Does it recognise strings it shouldn't? Try these little tests:

==
% Not empty.
?- phrase(anbn:'S', []).
false.

% Not only a
?- phrase(anbn:'S', [a]).
false.

% Not only b
?- phrase(anbn:'S', [b]).
false.

% Not starting with b
?- phrase(anbn:'S', [b|_]).
false.

% Not more a's than b's.
?- phrase(anbn:'S', [a,a,b]).
false.

% Not more b's than a's.
?- phrase(anbn:'S', [a,b,b]).
false.
==
*/

% Chain with one less second-order constraint.
configuration:metarule(unchain, [P,Q,R], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).
configuration:order_constraints(unchain,[P,Q,_R],[X,Y,Z],[P>Q],[X>Z,Z>Y]).

background_knowledge('S'/2,['A'/2,'B'/2]).

metarules('S'/2,[unchain]).

positive_example('S'/2,E):-
	member(E, ['S'([a,b],[])
		  ,'S'([a,a,b,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ]).

negative_example('S'/2,_):-
	fail.

'A'([a|A], A).
'B'([b|A], A).
