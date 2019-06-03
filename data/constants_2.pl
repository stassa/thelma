:-module(constants_2, [background_knowledge/2
		      ,metarules/2
		      ,positive_example/2
		      ,negative_example/2
		      ,successor/2
		     ]).

/** <module> Experiment file showing how to learn a theory with constants.

The example in this file shows how to learn a theory with more than one
body literal, unlike in constants.pl.

Usage
=====

1. Set depth_limits(4,0).

2. Run a query:

?- experiment_data(const/2,_Pos,_Neg,_BK,_MS), learn(_Pos,_Neg,_Prog), print_clauses(_Prog).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
% Clauses: 3; Invented: 0
% Clauses: 4; Invented: 0
const(2,A):-successor(A,2).
const(3,A):-successor(A,3).
const(A,B):-successor(A,2),successor(2,B).
const(A,B):-successor(A,3),successor(3,B).
true ;
false.
*/

configuration:metarule(inverse_const,[P,Q,X],[X,Y],mec(P,X,Y):-mec(Q,Y,X)).
configuration:metarule(chain_const,[P,Q,R,Z],[X,Y,Z],(mec(P,X,Y):-mec(Q,X,Z),mec(R,Z,Y))).

configuration:order_constraints(inverse_const,[P,Q,_X],_Fs,[P>Q],[]).
configuration:order_constraints(chain_const,[P,Q,R,_Z],_Fs,[P>Q,P>R],[]).

background_knowledge(const/2,[successor/2]).

metarules(const/2,[inverse_const,chain_const]).

positive_example(const/2,E):-
	member(E,[const(2,1)
		 ,const(3,2)
		 ,const(1,3)
		 ,const(2,4)
	       ]).

negative_example(const/2,E):-
	member(E,[
	       ]).

successor(A,B):-
	between(1,10,A)
	,succ(A,B).
