:-module(even, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,predecessor/2
	       ]).

/** <module> Experiment file for even/1 generator and acceptor.

Example of using MIL to learn mutually recursive theories taken from
_Meta-Interpretive Learning of higher-order dyadic datalog: predicate
invention revisited_ (Muggleton and Lin, MLJ, 2015).

The setup in this experiment file is used to learn a generator and
acceptor of even integers from 0 to infinity by inventing a generator
and acceptor for odd integers from 0 to infinity.

Usage
=====

1. Ensure this file is set as the current experiment file in
configuration.pl

==
experiment_file('data/examples/even.pl',even).
==

2. Set sufficient clause limits in configuration.pl:

==
depth_limits(3,1).
==

3. Run the following query to train Thelma on the data in this file and
print the results to the top-level:

==
?- learn(even/1).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
% Clauses: 2; Invented: 1
% Clauses: 3; Invented: 0
% Clauses: 3; Invented: 1
even(0).
even(A):-predecessor(A,B),even_1(B).
even_1(A):-predecessor(A,B),even(B).
true .
==

The invented predicate even_1/1 represent the concept of an odd integer.
Note that even/1 and even_1/1 are mutually recursive.

5. Copy/paste the hypothesis into this file, reconsult it and test it:

==
?- _N = 100, findall(X, (between(0,_N,X), even:even(X), \+ (X == 0 ; 0 is X mod 2)), Xs).
Xs = [].

?- _N = 100, findall(X, (between(0,_N,X), even:even(X), 0 is X mod 2), _Xs), writeln(_Xs).
[0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100]
true.
==

In the two testing queries above, binding _N to 100 stops even/1 from
generating an infinite series of odd numbers (even/1 is tail recursive
so if you let it go, it would keep going). You can increase _N to any
number you want: the definition of even/1 learned by Thelma is true for
any n.
*/

configuration:metarule(unit_monadic,[P,X],[X],mec(P,X):-true).
configuration:metarule(postcon_unit,[P,Q,R],[X,Y],(mec(P,X):-mec(Q,X,Y),mec(R,Y))).

configuration:order_constraints(unit_monadic,_Ss,_Fs,[],[]).
configuration:order_constraints(postcon_unit,_Ss,_Fs,[],[]).

background_knowledge(even/1,[predecessor/2]).

metarules(even/1,[unit_monadic,postcon_unit]).

positive_example(even/1,even(A)):-
	member(A,[0,2,4,6,8,10]).

negative_example(even/1,even(A)):-
	member(A,[1,3,5,7,9,11]).

%!	predecesor(+A,-B) is nondet.
%
%	True when A > B and both are integers.
%
%	@tbd Use of predecessor taken from Metagol examples by Andrew
%	Cropper (in metagol/examples/mutual_recursion.pl).
%
predecessor(A,B):-
	between(0,inf,A)
	,succ(B,A).

% The two clauses below override the automatic ordering of the Herbrand
% base derived by order_constraints/4.
%
% The empty interval ordering defined in constant_order/2, allows the
% use of an infinite range in predecessor/2. Without it,
% order_constraints/4 would call predecessor/2 to collect the constants
% in the Herbrand universe, and, since predecessor/2 ranges from 0 to
% infinity, would never terminate.
%
% The predicate_order/2 clause defines the same "order" as would be
% derived by order_constraints/4, which is to say, just [predecessor/2].
% It's a bit pointless but it's currently required: if one type of
% ordering override is specified, currently, the other one must be also.
%
predicate_order([predecessor/2],[predecessor/2]).
constant_order([predecessor/2],[]).
