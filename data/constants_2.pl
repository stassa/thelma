:-module(constants_2, [background_knowledge/2
		      ,metarules/2
		      ,positive_example/2
		      ,negative_example/2
		      ,successor/2
		     ]).

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


% target theory:
const(X,Y):- successor(Y,X).
const(X,Y):- successor(X,2),successor(2,Y).
const(X,Y):- successor(X,3),successor(3,Y).
