:-module(even, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,predecessor/2
	       ]).

/** <module> Experiment file for even/1 generator and acceptor.

Example taken from the _MIL - predicate invention revisited_ paper.

Learns a generator and acceptor of even integers from 0 to 100 by
inventing a generator and acceptor for odd integers from 0 to 100.

even/1 and the invented predicate corresponding to odd/1, even_1/1 are
definted in terms of each other making this an example of using MIL to
learn mutually recursive theories.

Usage
=====

1. Set depth_limits(3,1).

2. Remember to initialise the experiment:

?- initialise_experiment.
true.

3. Run the query:

?- experiment_data(even/1,_Pos,_Neg,_BK,_MS), learn(_Pos,_Neg,_Prog), print_clauses(_Prog).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
% Clauses: 2; Invented: 1
% Clauses: 3; Invented: 0
% Clauses: 3; Invented: 1
even(0).
even(A):-predecessor(A,B),even_1(B).
even_1(A):-predecessor(A,B),even(B). % Invention of odd/1
true .

4. Consult hypothesis and test:

?- findall(X, (even:even(X), \+ (X == 0 ; 0 is X mod 2)), Xs).
Xs = [].

?- findall(X, (even:even(X), (X == 0 ; 0 is X mod 2)), _Xs), writeln(_Xs).
[0,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100]

5. Remember to cleanup afterwards:

?- cleanup_experiment.
true.

Expanding the range of the learned theory
=========================================

The learned definition of even/1 only generates and accepts integers
from 0 to 100. This is the effect of the line between(0,100,A) in the
body of predecessor/2. The reason for this restriction is that a)
allowing an infinite range means learning must take an infinite time
and, b) a general theory can be learned from a small range, even one
smaller than [0,100].

If required, the range covered by even/1 can be extended (or reduced)
either during learning, or afterwards, by changing the 100 in
between(0,100,A) with a different value.

For instance, by changing between(0,100) to between(0,inf) the learned
theory can cover the full range of integers from to to infinity.

But if you do that during learning you'll wait a long time to see the
nice results.

*/

configuration:metarule(unit_monadic,[P,X],[X],mec(P,X):-true).
configuration:metarule(postcon_unit,[P,Q,R],[X,Y],(mec(P,X):-mec(Q,X,Y),mec(R,Y))).

configuration:order_constraints(unit_monadic,_Ss,_Fs,[],[]).
configuration:order_constraints(postcon_unit,_Ss,[X,Y],[],[X>Y]).

background_knowledge(even/1,[predecessor/2]).

metarules(even/1,[unit_monadic,postcon_unit]).

positive_example(even/1,even(A)):-
	member(A,[0,2,4,6,8,10]).

negative_example(even/1,even(A)):-
	member(A,[1,3,5,7,9,11]).

%!	predecesor(?A,?B) is nondet.
%
%	True when A > B and both are integers.
%
%	@tbd Use of predecessor taken from Metagol examples by Andrew
%	Cropper (in metagol/examples/mutual_recursion.pl).
%
predecessor(A,B):-
	between(0,100,A)
	,succ(B,A).


/* Target theory from the MIL paper.

even(0).
even(A):- even_1(A,B), even_2(B).
even_1(A,B):- succ(B,A).
even_2(A):- even_1(A,B), even(B).

This is basically inventing odd _and_ predecessor. I don't quite
understand why Thelma never seems to learn this, given successor/2
rather than predecessor/2 as BK. Instead, it learns a strange inversion
of the predecessor-defined theory:

even_1(101).
even(A):-successor(A,B),even_1(B).
even_1(A):-successor(A,B),even(B).

I think it's just unnecessary to invert successor, since using it as it
is is sufficient. So there is never a point at which the
inverted-successor theory _must_ be learned to cover the examples.
*/
