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

1. Set sufficient clause limits in configuration.pl:

depth_limits(3,0).

2. Run a query:

?- experiment_data(c_1/2,_Pos,_Neg,_BK,_MS), learn(_Pos,_Neg,_Prog), print_clauses(_Prog).
% Clauses: 1; Invented: 0
c_1(1,1).
true ;

Change the name of the predicate in the first argument of
experiment_data/5 to learn definitions of more predicates.
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

