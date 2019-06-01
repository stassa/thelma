:-module(constants, [background_knowledge/2
		    ,metarules/2
		    ,positive_example/2
		    ,negative_example/2
		    ]).

configuration:metarule(unit_const, [P,X,Y], [], mec(P,X,Y) :- true).
configuration:order_constraints(unit_const,_Ss,_Fs,[],[]).

background_knowledge(c_1/2,[]).

metarules(c_1/2,[unit_const]).

positive_example(c_1/2, E):-
	member(E,[c_1(1,1)
		 ]).

negative_example(c_1/2, _):-
	fail.
