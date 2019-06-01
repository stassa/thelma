:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,'A'/2
	       ,'B'/2
	       ]).

/** <module> Experiment file for a^nb^n grammar.

Current setup learns a^nb^n from three positive examples.

Usage
=====

a) Set depth_limits(3,1).

b) Run the query:

?- experiment_data('S'/2,_Pos,_Neg,_BK,_MS), learn(_Pos,_Neg,_Prog), print_clauses(_Prog).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
% Clauses: 2; Invented: 1
% Clauses: 3; Invented: 0
% Clauses: 3; Invented: 1
S(A,B):-A(A,C),B(C,B).
S(A,B):-S_1(A,C),B(C,B).
S_1(A,B):-A(A,C),S(C,B).
true ;

More results are possible. See end of file for longer discussion.

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
		  %,'S'([a,a,a,a,b,b,b,b],[])
		  ]).

negative_example('S'/2,_):-
	fail.

'A'([a|A], A).
'B'([b|A], A).


/*
Target theory, as ordinary Prolog.

'S'(A, C):- 'A'(A, B),'B'(B, C).
'S'(A,B):- 'A'(A,C),'S_1'(C,B).
'S_1'(A,B):-'S'(A,C),'B'(C,B).

Target theory, as a DCG.

'S' --> 'A', 'B'.
'S' --> 'A', 'S_1'.
'S_1' --> 'S', 'B'.

Actually learned (first) theory:

'S'(A,B):-'A'(A,C),'B'(C,B).
'S'(A,B):-'S_1'(A,C),'B'(C,B).
'S_1'(A,B):-'A'(A,C),'S'(C,B).

This is basically the same as the target theory except the invented
predicate S_1/2 starts with A/2 instead of S/2. The important point is
that it comes down to ASB - which produces a tree with an equal number
of A and B leaves.

Longer target theory with an invented predicate equivalent to 'S'/2:

'S'(A, C) :- 'A'(A, B), 'B'(B, C).
'S'(A, C) :- 'A'(A, B),'S_1'(B, C).
'S_1'(A, C) :- 'S_2'(A, B),'B'(B, C).
'S_2'(A, C) :- 'A'(A, B), B'(B, C).
'S_2'(A, C) :- 'A'(A, B),'S_1'(B, C).

As a DCG:

'S' --> 'A', 'B'.
'S' --> 'A', 'S_1'.
'S_1' --> 'S_2', 'B'.
'S_2' --> 'A', 'B'.
'S_2' --> 'A', 'S_1'.
'A' --> [a].
'B' --> [b].

Set depth_limits(5,2) to learn that one. It's easier to learn if you
uncomment the fourth positive example. I know. A single example makes a
difference.

This is still equivalent to the three-clause target theory, but more
broken up. It is also learned, but only after much search. In the
process it also learns the following, which is wrong:

S(A,B):-A(A,C),B(C,B).
S(A,B):-A(A,C),S(C,B).
S(A,B):-B(A,C),B(C,B).
S(A,B):-B(A,C),S(C,B).

?- phrase(anbn:'S', P).
P = [a, b] ;
P = [a, a, b] ;
P = [a, a, a, b] ;
P = [a, a, a, a, b] ;
P = [a, a, a, a, a, b] ;
P = [a, a, a, a, a, a, b] ;
P = [a, a, a, a, a, a, a, b] ;
P = [a, a, a, a, a, a, a, a, b] .

Negative examples are needed to avoid learning this.
*/
