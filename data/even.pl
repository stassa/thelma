:-module(even, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,successor/2
	       ]).

% Not sure why these are like that.
% metarule(precon, [P,Q,R], [X,Y], (mec(P,X,X) :- mec(Q,X,Y), mec(R,Y,Y))).
% metarule(prerec, [P,Q,R], [X,Y], (mec(P,X,X) :- mec(Q,X,Y), mec(R,Y,Y))).
% Is this right? What is this?
configuration:metarule(prerec, [P,Q], [X,Y], (mec(P,X,X) :- mec(Q,X,Y), mec(P,Y,Y))).

%order_constraints(prerec,[_P,_Q,_R],[X,Y],[],[X>Y]).
configuration:order_constraints(prerec,[P,Q],[X,Y],[P>Q],[X>Y]).


background_knowledge(even/2,[successor/2]).
background_knowledge(even/1,[successor/2]).

%metarules(even/2,[projection,chain,inverse,unit]).
metarules(even/2,[unit,inverse,precon,prerec]).
metarules(even/1,[unit_monadic,postcon_unit,inverse]).

successor(A,B):-
	between(0,100,A)
	,succ(A,B).


positive_example(even/1,even(A)):-
	member(A,[0,2,4,6,8,10]).
positive_example(even/2,even(A,A)):-
	member(A,[0,2,4,6,8,10]).

negative_example(even/1,even(A)):-
	member(A,[1,3,5,7,9,11]).
negative_example(even/2,even(A,A)):-
	member(A,[1,3,5,7,9,11]).


/*
even(0).
even(A):- even_1(A,B), even_2(B).
even_1(A,B):- succ(B,A).
even_2(A):- even_1(A,B), even(B).

*/

/*even(0,0).
even(A,A) :- even_1(A,B),even_2(B,B).
even_1(A,B):- successor(B,A).
even_2(A,A) :- even_3(A,B), even(B,B).
even_3(A,B):- successor(B,A).
*/

/*even(0,0).
even(A,A) :- even_1(A,B),even_2(B,B).
even_1(A,B):- successor(B,A).
even_2(A,A) :- even_1(A,B), even(B,B).
*/

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
