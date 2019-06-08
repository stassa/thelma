:-module(thelma, [learn/2
		 ,learn/5
		 ]).

:-use_module(configuration).
:-use_module(auxiliaries).

/** <module> A Meta-Interpretive Learning system.
*/

%!	learn(+Predicate,-Definition) is nondet.
%
%	Learn a Definition of a predicate.
%
%	High level interface to learn/5 with data taken from
%	experiment_data/5, by passing Predicate as the target.
%
%	Use this predicate to learn from the examples, background
%	knowledge and metarules declared for Predicate in the current
%	experiment file.
%
%	Use learn/5 to learn from an arbitrary set of examples,
%	metarules and background knowledge.
%
learn(T,Prog):-
	experiment_data(T,Pos,Neg,BK,MS)
	,learn(Pos,Neg,BK,MS,Prog).



%!	learn(+Pos,+Neg,+Background,+Metarules,-Program) is nondet.
%
%	Learn a Program from the given data.
%
%	Returns each Program possible to learn from the given data on
%	successive backtracking.
%
%	Use this predicate to learn from arbitrary lists of examples,
%	background knowledge and metarules. However, note that
%	background predicates must be defined and accessible to this
%	module, i.e. they must be defined in module user, or a module
%	exporting to module user. The current experiment file exports to
%	module user.
%
learn(Pos,Neg,BK,MS,Prog):-
	configuration:depth_limits(C,I)
	,initialise_experiment
	,target_predicate(Pos,T)
	,depth_level(C,I,C_,I_)
	,program_signature(I_,T,BK,Po,Co)
	,debug(depth,'Clauses: ~w; Invented: ~w',[C_,I_])
	,prove(C_,Pos,BK,MS,Po-Co,Ps)
	,disprove(Neg,Ps)
	,project_metasubs(Ps, Prog).
learn(_Pos,_Neg,_BK,_MS,_Prog):-
	cleanup_experiment
	,fail.


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


%!	program_signature(+Invented,+Target,+BK,-Predicates,-Constants)
%!	is det.
%
%	Determine the predicate signature for a target program.
%
%	Also calls order_constraints/3 to derive a lexicographic and
%	interval ordering for Predicates and Constants.
%
program_signature(K,F/A,BK,Ps,Cs):-
	order_constraints(BK,Ps_,Cs)
	,invented_symbols(K,F/A,Ss)
	,append([F/A|Ss],Ps_,Ps).


%!	invented_symbols(+Symbols,+Target_Symbol,-Invented) is det.
%
%	Create new symbols for Invented predicates.
%
invented_symbols(K,F/A,Ss):-
	findall(S_/A
	       ,(between(1,K,I)
		,atomic_list_concat([F,I],'_',S_)
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


%!	prove(+Depth,+Atoms,+BK,+Metarules,+Orders,-Metasubstitutions)
%!	is nondet.
%
%	Prove a list of Atoms and derive a list of Metasubstitutions.
%
%	Depth_Limit is the maximum depth for the iterative deepening
%	search. More precisely, it's the maximum size of a theory, i.e.
%	the maximum number of elements in the list Metasubstitutions.
%
prove(K,Pos,BK,MS,Po-Co,Ss):-
	prove(K,Pos,BK,MS,Po-Co,[],Ss_)
	,reverse(Ss_,Ss).


%!	prove(+Depth,+Atoms,+BK,+Metarules,+Orders,+Acc,-Metasubs)
%!	is nondet.
%
%	Business end of prove/7.
%
prove(_K,[],_BK,_MS,_PS,Ss,Ss):-
	!.
prove(K,[A|As],BK,MS,PS-Cs,Acc,Bind):-
	background_predicate(BK,A)
	,!
	,A_ =.. A
	,user:call(A_)
	,prove(K,As,BK,MS,PS-Cs,Acc,Bind).
prove(K,[A|As],BK,MS,PS-Cs,Acc1,Bind):-
	member(Msub,Acc1)
	,once(metasubstitution(MS,A,PS-Cs,Msub,Bs))
	% move cut here?
	,prove(K,Bs,BK,MS,PS-Cs,Acc1,Acc2)
	,! % Very red cut. Stops adding some
	% redundant clauses- but will it stop
	% adding necessary ones, also?
	,prove(K,As,BK,MS,PS-Cs,Acc2,Bind).
prove(K,[A|As],BK,MS,PS-Cs,Acc1,Bind):-
	length(Acc1,N)
	,N < K
	,metasubstitution(MS,A,PS-Cs,Msub,Bs)
	,new_metasub(Msub,Acc1,Acc2)
	,prove(K,Bs,BK,MS,PS-Cs,Acc2,Acc3)
	,prove(K,As,BK,MS,PS-Cs,Acc3,Bind).


%!	background_predicate(+BK,+Atom) is det.
%
%	True when Atom is an atom of a predicate in the BK.
%
background_predicate(BK,[F|Args]):-
	length(Args, A)
	,memberchk(F/A, BK).


%!	metasubstitution(+Metarules,+Atom,+Signature,?Metasub,-Body) is
%!	nondet.
%
%	Perform a second-order metasubstitution.
%
%	Metasub is a term sub(Id,Hs), where Id is the id of a metarule
%	and Hs is a list of second-order variables, to be eventually
%	bound to first-order predicate terms from the predicate
%	Signature.
%
metasubstitution(MS,[S|Args],PS-Cs,sub(Id,[S/A|Ss]),Bs):-
	atom_symbol_arity([S|Args],S/A)
	,next_metarule(MS,[Id,[S/A|Ss],Fs,[[S|Args]|Bs]])
	,second_order_bindings(PS,Fs,Ss)
	,configuration:order_constraints(Id,[S/A|Ss],Fs,STs,FTs)
	,order_tests(PS,Cs,STs,FTs).


%!	next_metarule(+Metarules,-Metarule) is nondet.
%
%	Select the next Metarule for Target.
%
next_metarule(MS,[Id,Ss,Fs,Bs]):-
	metarule_functor(F)
	,member(Id,MS)
	,M =.. [F,Id,Ss,Fs,Bs]
	,user:call(M).


%!	second_order_bindings(+Signature,+First_Order,-Bindings) is
%!	nondet.
%
%	Ground second order terms to symbols in the Signature.
%
second_order_bindings(_,_,[]):-
	!.
second_order_bindings(PS,Fs,[C|_Ss]):-
% C is to be bound to a constant and so are all remaining terms.
	bound_constant(C,Fs)
	,!
	,second_order_bindings(PS,Fs,[]).
second_order_bindings(PS,Fs,[S/A|Ss]):-
	member(S/A,PS)
	,second_order_bindings(PS,Fs,Ss).


%!	bound_constant(+Term,+First_order)  is det.
%
%	True when a Term is a constant in the hypothesis.
%
%	Term is taken from the set of existentially quantified terms in
%	a metarule. Such a term is a constant in the hypothesis if it
%	is an atomic Prolog term, or if it is a variable that is also in
%	the set of First_order variables in the metarule.
%
bound_constant(C,_Fs):-
	atomic(C)
	,!.
bound_constant(C,Fs):-
	copy_term([C|Fs],[C_|Fs_])
	,numbervars([C_|Fs_])
	,memberchk(C_,Fs_).


%!	atom_symbol_arity(+Atom,-Predicate_Indicator) is det.
%
%	Figure out the symbol and arity of an Atom given as a list.
%
atom_symbol_arity([A|As],A/N):-
	length(As,N).


%!	order_tests(+Predicates,+Constants,+First_Order,+Second_Order)
%!	is det.
%
%	Test the ordering constraints associated with a metarule.
%
order_tests(_,_,[],[]):-
	!.
order_tests(PS,_,STs,_):-
	STs \= []
	,!
	,forall(member(A>B,STs)
	      ,above(A,B,PS)
	      ).
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


%!	new_metasub(+Metasub,+Metasubs,-Metasubs_new) is det.
%
%	Add a new Metasubstitution to the list of Metasubstitutions.
%
new_metasub(MS,Prog,[MS|Prog]):-
	new_metasub(MS,Prog).

%!	new_metasub(+Metasub,+Metasubs) is det.
%
%	Business end of new_metasub/3.
%
%	True when Metaub is not in Metasubs.
%
new_metasub(_,[]):-
	!.
new_metasub(MS1,[MS2|Prog]):-
	MS1 \== MS2
	,new_metasub(MS1,Prog).


%!	disprove(+Negative,+Program) is det.
%
%	True when a Program does not cover Negative examples.
%
disprove([],_Ms):-
% Skip further processing if there are no negative examples.
	!.
disprove(Neg,Ms):-
	project_metasubs(Ms,Prog)
	,assert_program(thelma,Prog,Refs)
	% Succeed if the program fails, otherwise fail;
	% Erase the newly asserted clauses eitherwise.
	,(	forall(member(A,Neg)
		      ,(A_ =.. A
		       ,\+ once(call(A_))
		       )
		      )
	 ->  erase_program_clauses(Refs)
	 ;   erase_program_clauses(Refs)
	    ,fail
	 ).


%!	project_metasubs(+Metasubstitutions,-Program) is det.
%
%	Project a list of Metasubstitutions to a Program.
%
project_metasubs(Ms,Prog):-
	findall(C
		,(member(Mi,Ms)
		 ,project_metasub(Mi,C)
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
	,project_metasub(Bs,[],Ls)
	,literals_list_to_clause(Ls,C).


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


%!	literals_list_to_clause(+Literals,-Clause) is det.
%
%	Transforma  list of Literals to a Clause.
%
literals_list_to_clause([H|[]],H):-
% No body literals
	!.
literals_list_to_clause([H|[B]],(H:-B)):-
% One body literal
	!.
literals_list_to_clause([H|Ls],(H:-Bs)):-
% A vector of body literals that should be joined by ','/2.
	Bs =.. [,|Ls].

