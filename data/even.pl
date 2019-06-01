:-module(even, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,successor/2
	       ]).

configuration:metarule(unit_monadic,[P,X],[X],mec(P,X):-true).
configuration:metarule(postcon_unit,[P,Q,R],[X,Y],(mec(P,X):-mec(Q,X,Y),mec(R,Y))).

configuration:order_constraints(unit_monadic,_Ss,_Fs,[],[]).
configuration:order_constraints(postcon_unit,_Ss,[X,Y],[],[X>Y]).

background_knowledge(even/1,[successor/2]).

metarules(even/1,[unit_monadic,postcon_unit,inverse]).

positive_example(even/1,even(A)):-
	member(A,[0,2,4,6,8,10]).

negative_example(even/1,even(A)):-
	member(A,[1,3,5,7,9,11]).

successor(A,B):-
	between(0,100,A)
	,succ(A,B).

/* Target theory:
even(0).
even(A):- even_1(A,B), even_2(B).
even_1(A,B):- succ(B,A).
even_2(A):- even_1(A,B), even(B).
*/


/* These are actually ... odd.
even_1(101).
even(A):-successor(A,B),even_1(B).
even_1(A):-successor(A,B),even(B).

even(0).
even_1(101).
even(A):-successor(A,B),even_1(B).
even_1(A):-successor(A,B),even(B).

These were learned with identity rather than inverse.
Why? I got negative examples that should have stopped them.

Ooops. Haha. Actually, even_1/1 is odd. even/1 is even!
But- it stops at 101. Meh.

Can be tested with:

?- findall(X, (even:even(X), \+ (X == 0 ; 0 is X mod 2)), Xs).
Xs = [].

*/
