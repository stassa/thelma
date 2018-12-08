:-module(thelma, [learn/3
		 ]).

:-use_module(configuration).
:-use_module(auxiliaries).

/** <module> A simple Meta-Interpretive Learner, meant for educational purposes.

An example learning session
---------------------------

?- initialise_experiment.
true.

?- experiment_data(grandfather,_Pos,_Neg,_BK,_MS,false), learn(_Pos,_Neg,_Prog),print_clauses(_Prog).
grandfather(A,B):-father(A,C),parent(C,B).
true ;
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-father(A,C),father(C,B).
true ;
grandfather(A,B):-father(A,C),mother(C,B).
grandfather(A,B):-father(A,C),father(C,B).
true ;
grandfather(A,B):-father(A,C),parent(C,B).
true ;
grandfather(A,B):-father(A,C),mother(C,B).
grandfather(A,B):-father(A,C),parent(C,B).
true ;
false.

?- cleanup_experiment.
true.

*/


%!	learn(+Positive,+Negative,-Program) is nondet.
%
%	Learn a Program from Positive and Negative input examples.
%
learn(Pos,Neg,Prog):-
	configuration:depth_limits(C,I)
	,predicate_signature(PS)
	,depth_level(C,I,C_,I_)
	,invented_symbols(I_,Pos,Ss)
	,append(PS,Ss,PS_)
	,debug(depth,'Clauses: ~w; Invented: ~w',[C_,I_])
	,prove(0,C_,Pos,PS_,[],Ps)
	,disprove(Neg,Ps)
	,project_metasubs(Ps, true, Prog).


%!	depth_level(+Clause_Max,+Invented_Max,-Clauses,-Invented) is
%!	det.
%
%	Increment the search depth level.
%
depth_level(1,0,1,0):-
	!.
depth_level(C,I,C_,I_):-
	between(1,C,C_)
	,between(0,I,I_)
	,I_ < C_.


%!	invented_symbols(+Symbols,+Examples,-Invented) is det.
%
%	Create new symbols for Invented predicates.
%
invented_symbols(K,[[S|_As]|_],Ss):-
	findall(S_
	       ,(between(1,K,I)
		,atomic_list_concat([S,I],'_',S_)
		)
	       ,Ss).


%!	prove(+Depth,+Limit,+Atoms,+Signature,+Acc,-Metasubstitutions)
%!	is det.
%
%	Prove a list of Atoms and derive a list of Metasubstitutions.
%
%	Depth and Limit are the current and maximum depthts,
%	respectively, for the iterative deepening search.
%
prove(_,_,[],_,MS,MS_):-
	!
	,reverse(MS,MS_).
prove(I,K,[A|As],PS,Acc,Bind):-
	background_predicate(A)
	,!
	,A_ =.. A
	,user:call(A_)
	,prove(I,K,As,PS,Acc,Bind).
prove(I,K,[A|As],PS,Acc1,Bind):-
	member(MS,Acc1)
	,once(metasubstitution(A,PS,MS,Bs))
	,prove(I,K,Bs,PS,Acc1,Acc2)
	,! % Very red cut. Stops adding some
	% redundant clauses- but will it stop
	% adding necessary ones, also?
	,prove(I,K,As,PS,Acc2,Bind).
prove(I,K,[A|As],PS,Acc1,Bind):-
	metasubstitution(A,PS,MS,Bs)
	,abduction(MS,Acc1,Acc2)
	,succ(I,I_)
	,I_ =< K
	,prove(I_,K,Bs,PS,Acc2,Acc3)
	,prove(I_,K,As,PS,Acc3,Bind).

/*?- findall([grandfather,A,B], tiny_kinship:grandfather(A,B), _Gs), prove(1,2,_Gs,[father,parent],[],[Ps]), thelma:project_metasub(Ps,Ps_), numbervars(Ps_).
Ps = sub(chain, [grandfather, father, parent]),
Ps_ =  (grandfather(A, B):-father(A, C), parent(C, B)) ;
Ps = sub(chain, [grandfather, parent, parent]),
Ps_ =  (grandfather(A, B):-parent(A, C), parent(C, B)) ;
false.*/


%!	disprove(+Program,+Metasubs) is det.
%
%	True when Program does not cover Metasubs.
%
disprove(Neg,Ms):-
	project_metasubs(Ms,false,Prog)
	,assert_program(thelma,Prog)
	,forall(member(A,Neg)
	       ,(A_ =.. A
		,\+ call(A_)
		)
	       )
	,retract_program(thelma,Prog)
	,!.
disprove(_Neg,Ms):-
	project_metasubs(Ms,false,Prog)
	,retract_program(thelma,Prog)
	,fail.


%!	background_predicate(+Atom) is det.
%
%	True when Atom is an atom of a background knowledge predicate.
%
background_predicate([F|Args]):-
	configuration:experiment_file(_P,M)
	,M:background_knowledge(BK)
	,length(Args, A)
	,member(F/A, BK).


%!	metasubstitution(+Atom,+Signature,+Metasub,-Body) is det.
%
%	Perform a second-order metasubstitution.
%
%	Metasub is a term sub(Id,Hs), where Id is the id of a metarule
%	and Hs is a list of second-order variables, to be eventually
%	bound to first-order predicate terms from the predicate
%	Signature.
%
metasubstitution([A|As],PS,sub(Id,[A,P|Ss]),Bs):-
	member(P,PS)
	% Backstop to avoid cyclicity until ordering constraints
	% are properly implemented.
	,P \= A
	,metarule_instance(Id,[A,P|Ss],As,[_Hs|Bs]).
metasubstitution([A|As],PS,sub(Id,[A,P1,P2|Ss]),Bs):-
	member(P1,PS)
	,member(P2,PS)
	,P1 \= A
	,P2 \= A
	,metarule_instance(Id,[A,P1,P2|Ss],_Fs,[[A|As]|Bs]).


%!	abduction(+Metasubstitution,+Store,-Adbduced) is det.
%
%	Add a new Metasubstitution to the abduction Store.
%
abduction(MS,Prog,[MS|Prog]):-
	\+ memberchk(MS, Prog).


%!	metarule_instanece(+Id,+Second_order,+First_order,-Rule) is det.
%
%	A Generator of metarule instances.
%
metarule_instance(Id,Ss,Fs,Bs):-
	configuration:experiment_file(_P,M)
	,metarule_functor(F)
	,M:metarules(Ms)
	,member(Id,Ms)
	,T =.. [F,Id,Ss,Fs,Bs]
	,user:call(T).


%!	project_metasubs(+Metasubstitutions,+Skolemise,-Program) is det.
%
%	Project a list of Metasubstitutions to a Program.
%
%	Skolemise is a boolean denoting whether Program should be
%	skolemised or not.
%
project_metasubs(Ms,S,Prog):-
	findall(C
		,(member(Mi,Ms)
		 ,project_metasub(Mi,C)
		 ,(   S
		  ->  numbervars(C)
		  ;   true
		  )
		 )
		,Prog).


%!	project_metasub(+Metasubstitution,-Clause) is det.
%
%	Porject a second-order Metasubstitution to a definite Clause.
%
project_metasub(sub(Id,Ss),H:-Ps):-
	metarule_functor(F)
	,M =.. [F,Id,Ss,_Fs,Bs]
	,M
	,project_metasub(Bs,[],[H|Ps_])
	% If the body of the projected metasub is monadic
	% join it to the head; else, join the literals by ','
	% then join the resulting compound to the head.
	,(   Ps_ = [Ps]
	 ->  true
	 ;   Ps =.. [,|Ps_]
	 ).


%!	project_metasub(+Literals,+Acc,-Atoms) is det.
%
%	Business end of project_metasub/2.
%
%	Literals is a list of lists representing literals. Atoms is a
%	list of atoms, derived from the literals in the list.
%
project_metasub([],Ps,Ps_):-
	reverse(Ps, Ps_).
project_metasub([L|Ls],Acc,Ps):-
	L_ =.. L
	,project_metasub(Ls,[L_|Acc],Ps).
