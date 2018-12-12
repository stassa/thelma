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
	,target_predicate(Pos,T)
	,depth_level(C,I,C_,I_)
	,program_signature(I_,T,Po,Co)
	,debug(depth,'Clauses: ~w; Invented: ~w',[C_,I_])
	,prove(T,C_,Pos,Po-Co,[],Ps)
	,disprove(Neg,Ps)
	,project_metasubs(Ps, true, Prog_)
	,sort(Prog_,Prog).


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


%!	program_signature(+Invented,+Target,-Predicates,-Constants) is
%!	det.
%
%	Determine the predicate signature for a target program.
%
%	Determines the predicate signature and also the ordering of
%	Predicates and Constants. It's just that the predicate ordering,
%	in list Predicates, is also the predicate signature.
%
program_signature(K,F/A,Ps,Cs):-
	order_constraints(F/A,Ps_,Cs)
	,invented_symbols(K,F,Ss)
	,append([F|Ss],Ps_,Ps).


%!	invented_symbols(+Symbols,+Target_Symbol,-Invented) is det.
%
%	Create new symbols for Invented predicates.
%
invented_symbols(K,T,Ss):-
	findall(S_
	       ,(between(1,K,I)
		,atomic_list_concat([T,I],'_',S_)
		)
	       ,Ss).


%!	target_predicate(+Examples,-Target) is det.
%
%	Determine the Target predicate from a set of Examples.
%
%	Target is a predicate indicator.
%
%	Makes no attempt to check that Examples are all atoms of the
%	same predicate, etc.
%
target_predicate([[F|Args]|_Es],F/A):-
	length(Args,A).


%!	prove(+Target,+Depth_Limit,+Atoms,+Signature,+Acc,-Metasubstitutions)
%!	is det.
%
%	Prove a list of Atoms and derive a list of Metasubstitutions.
%
%	Depth_Limit is the maximum depth for the iterative deepening
%	search. More precisely, it's the maximum size of a theory, i.e.
%	the maximum number of elements in the list Metasubstitutions.
%
prove(_,_,[],_,MS,MS_):-
	!
	,reverse(MS,MS_).
prove(T,K,[A|As],PS-Cs,Acc,Bind):-
	background_predicate(T,A)
	,!
	,A_ =.. A
	,user:call(A_)
	,prove(T,K,As,PS-Cs,Acc,Bind).
prove(T,K,[A|As],PS-Cs,Acc1,Bind):-
	member(MS,Acc1)
	,once(metasubstitution(T,A,PS-Cs,MS,Bs))
	,prove(T,K,Bs,PS-Cs,Acc1,Acc2)
	,! % Very red cut. Stops adding some
	% redundant clauses- but will it stop
	% adding necessary ones, also?
	,prove(T,K,As,PS-Cs,Acc2,Bind).
prove(T,K,[A|As],PS-Cs,Acc1,Bind):-
	metasubstitution(T,A,PS-Cs,MS,Bs)
	,abduction(MS,Acc1,Acc2)
	,length(Acc2,N)
	,N =< K
	,prove(T,K,Bs,PS-Cs,Acc2,Acc3)
	,prove(T,K,As,PS-Cs,Acc3,Bind).

/*?- findall([grandfather,A,B], tiny_kinship:grandfather(A,B), _Gs), prove(1,2,_Gs,[father,parent],[],[Ps]), thelma:project_metasub(Ps,Ps_), numbervars(Ps_).
Ps = sub(chain, [grandfather, father, parent]),
Ps_ =  (grandfather(A, B):-father(A, C), parent(C, B)) ;
Ps = sub(chain, [grandfather, parent, parent]),
Ps_ =  (grandfather(A, B):-parent(A, C), parent(C, B)) ;
false.*/


%!	background_predicate(+Target,+Atom) is det.
%
%	True when Atom is an atom of a background knowledge predicate.
%
background_predicate(T,[F|Args]):-
	configuration:experiment_file(_P,M)
	,M:background_knowledge(T,BK)
	,length(Args, A)
	,member(F/A, BK).


%!	metasubstitution(+Target,+Atom,+Signature,+Metasub,-Body) is
%!	det.
%
%	Perform a second-order metasubstitution.
%
%	Metasub is a term sub(Id,Hs), where Id is the id of a metarule
%	and Hs is a list of second-order variables, to be eventually
%	bound to first-order predicate terms from the predicate
%	Signature.
%
metasubstitution(T,[A|As],PS-Cs,sub(Id,[A,P]),Bs):-
	member(P,PS)
	,metarule_instance(T,Id,[A,P],As,PS-Cs,[_Hs|Bs]).
metasubstitution(T,[A|As],PS-Cs,sub(Id,[A,P1,P2]),Bs):-
	member(P1,PS)
	,member(P2,PS)
	,metarule_instance(T,Id,[A,P1,P2],_Fs,PS-Cs,[[A|As]|Bs]).


%!	abduction(+Metasubstitution,+Store,-Adbduced) is det.
%
%	Add a new Metasubstitution to the abduction Store.
%
abduction(MS,Prog,[MS|Prog]):-
	\+ memberchk(MS, Prog).


%!	metarule_instanece(+Target,+Id,+Second_order,+First_order,-Rule)
%!	is det.
%
%	A Generator of metarule instances.
%
metarule_instance(T,Id,Ss,Fs,PS-CS,Bs):-
	configuration:experiment_file(_P,M)
	,metarule_functor(F)
	,M:metarules(T,Ms)
	,member(Id,Ms)
	,MR =.. [F,Id,Ss,Fs,Bs]
	,user:call(MR)
	,configuration:order_constraints(Id,Ss,Fs,STs,FTs)
	,order_tests(PS,CS,STs,FTs).


%!	order_tests(+Predicates,+Constants,+First_Order,+Second_Order)
%!	is det.
%
%	Test the ordering constraints associated with a metarule.
%
order_tests(_,_,[],[]):-
	!.
order_tests(PS,_,STs,_):-
	STs \= []
	,forall(member(A>B,STs)
	      ,above(A,B,PS)
	      )
	,!.
order_tests(_,CS,_,FTs):-
	FTs \= []
	,forall(member(A>B,FTs)
	      ,above(A,B,CS)
	      ).


%!	above(+Term1,+Term2,+Ordering) is det.
%
%	True when Term1 is above Term2 in the given Ordering.
%
above(A,B,Cs):-
% Remember- if either A or B is a variable
% this test will succeed.
	A \== B
	,right_scan(A, Cs, Rs)
	,right_scan(B, Rs, _).

%!	right_scan(+X,+Ls,-Ys) is det.
%
%	Scan a list left-to-right until an element is found.
%
%	Ys is the list that remains when X and all elements before it
%	are removed from Ls.
%
right_scan(A,[B|Cs],Cs):-
% Avoid binding a variable in A or B.
	unifiable(A,B,_)
	,!.
right_scan(A,[_|Cs],Acc):-
	right_scan(A,Cs,Acc).




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
project_metasub(sub(Id,Ss),C):-
	metarule_functor(F)
	,M =.. [F,Id,Ss,_Fs,Bs]
	,M
	,project_metasub(Bs,[],[H|Ps_])
	% If the body of the projected metasub is monadic
	% join it to the head; if it's empty leave the head alone
	% else, join the literals by ',' then join the resulting
	% compound to the head.
	,(   Ps_ = [Ps]
	 ->  C = (H:-Ps)
	 ;   Ps_ = []
	 ->  C = H
	 ;   Ps =.. [,|Ps_]
	    ,C = (H:-Ps)
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
