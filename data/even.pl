:-module(even, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,successor/2
	       ]).


background_knowledge(even/2,[successor/2]).

metarules(even/2,[projection,chain,inverse,unit]).

successor(A,B):-
	between(0,100,A)
	,succ(A,B).


positive_example(even/2,even(A,A)):-
	member(A,[1,3,5,7,9,11]).

negative_example(even/2,even(A,A)):-
	member(A,[2,4,6,8,10]).


even(0,0).
even(A,A) :- even_1(A,B),even_2(B,B).
even_1(A,B):- successor(B,A).
even_2(A,A) :- even_1(A,B), even(B,B).


/*
successor(A,s(A)):-
	nat(A).

nat(0).
nat(s(N)):-
	nat(N).

sum(0,X,X):-
	nat(X).
sum(s(X),Y,s(Z)):-
	sum(X,Y,Z).

	*/
