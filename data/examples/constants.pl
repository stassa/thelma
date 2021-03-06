:-module(constants, [background_knowledge/2
		    ,metarules/2
		    ,positive_example/2
		    ,negative_example/2
		    ]).

/** <module> Experiment file showing how to learn theories with constants.

This file shows examples of target theories with a single body literal
and one or more constants.

Usage
=====

1. Ensure this file is set as the current experiment file in
configuration.pl:

==
experiment_file('data/examples/constants.pl',constants).
==

2. Set sufficient clause limits in configuration.pl:

==
depth_limits(3,0).
==

3. Run the following query to train Thelma on the data in this file and
print the results to the top-level:

==
?- learn(c_5/2).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
% Clauses: 3; Invented: 0
c_5(1,A).
c_5(A,2).
c_5(4,A).
true .
==

Change the name of the predicate in the first argument of
experiment_data/5 to one of the targets defined in
background_knowledge/2, to learn definitions of more predicates.
*/

configuration:metarule(unit_const, [P,X,Y], [], mec(P,X,Y) :- true).
configuration:metarule(unit_const_1, [P,X], [X,Y], mec(P,X,Y) :- true).
configuration:metarule(unit_const_2, [P,Y], [X,Y], mec(P,X,Y) :- true).

configuration:order_constraints(unit_const,_Ss,_Fs,[],[]).
configuration:order_constraints(unit_const_1,_Ss,_Fs,[],[]).
configuration:order_constraints(unit_const_2,_Ss,_Fs,[],[]).

background_knowledge(c_1/2,[]).
background_knowledge(c_2/2,[]).
background_knowledge(c_3/2,[]).
background_knowledge(c_4/2,[]).
background_knowledge(c_5/2,[]).

metarules(c_1/2,[unit_const]).
metarules(c_2/2,[unit_const]).
metarules(c_3/2,[unit_const_1]).
metarules(c_4/2,[unit_const_2]).
metarules(c_5/2,[unit_const_1,unit_const_2]).

% Bind a pair of constants in a unit clause.
positive_example(c_1/2, E):-
	member(E,[c_1(1,1)
		 ]).
% Bind two pairs of constants in two different unit clauses.
positive_example(c_2/2, E):-
	member(E,[c_2(1,1)
		 ,c_2(1,2)
		 ]).
% Bind two constants in the first argument of two different unit
% clauses.
positive_example(c_3/2, E):-
	member(E,[c_3(1,_)
		 ,c_3(2,_)
		 ]).
% Bind two constants in the second argument of two different unit
% clauses.
positive_example(c_4/2, E):-
	member(E,[c_4(_,1)
		 ,c_4(_,2)
		 ]).
% Bind three constants in three different unit clauses.
% Totally cribbed off Metagol's examples in examples/constants1.pl.
positive_example(c_5/2, E):-
	member(E,[c_5(1,2),
		  c_5(1,3),
		  c_5(1,4),
		  c_5(1,1),
		  c_5(2,2),
		  c_5(4,4)
		 ]).

negative_example(c_1/2, _):-
	fail.
negative_example(c_2/2, _):-
	fail.
negative_example(c_3/2, E):-
	member(E,[c_3(3,_)
		 ,c_3(4,_)
		 ]).
negative_example(c_4/2, E):-
	member(E,[c_4(_,3)
		 ,c_4(_,4)
	       ]).
negative_example(c_5/2, E):-
	member(E,[c_5(2,4),
		  c_5(3,4),
		  c_5(3,1)
		 ]).

