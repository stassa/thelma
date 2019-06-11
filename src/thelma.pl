:-module(thelma, [learn/1
		 ,learn/2
		 ,learn/5
		 ]).

:-use_module(configuration).
:-use_module(auxiliaries).

/** <module> A Meta-Interpretive Learning system.
*/

%!	learn(+Target) is nondet.
%
%	Learn a Definition of a Target predicate.
%
%	As learn/2 but prints each learned program to the Prolog
%	top-level rather than binding it to an output variable.
%
%	Use this predicate when you don't need to process a learned
%	program any further, i.e. by passing it to another predicate,
%	and so on.
%
learn(T):-
	learn(T,Ps)
	,print_clauses(Ps).



%!	learn(+Target,-Definition) is nondet.
%
%	Learn a Definition of a Target predicate.
%
%	Target is the predicate symbol and arity of the predicate to be
%	learned. Target is used to collect positive and negative
%	examples, background knowledge and metarules from the current
%	experiment file.
%
%	Definition is a list of definite datalog clauses, the
%	learned definition of target.
%
%	learn/2 is a high level interface to learn/5 with data taken
%	from experiment_data/5, by passing Predicate as the target.
%
%	Use this predicate to learn from the examples, background
%	knowledge and metarules declared for Target in the current
%	experiment file. Use learn/5 to learn from an arbitrary set of
%	examples, metarules and background knowledge.
%
learn(T,Prog):-
	experiment_data(T,Pos,Neg,BK,MS)
	,learn(Pos,Neg,BK,MS,Prog).



%!	learn(+Pos,+Neg,+Background,+Metarules,-Program) is nondet.
%
%	Learn a Program from the given data.
%
%	Pos, Neg are lists of unit clauses representing positive and
%	negative examples, respectively, of the predicate to be
%	learned: the _target predicate_.
%
%	Background is a list of predicate symbols and arities, S/N, of
%	the predicates to be used as background knowledge. Each
%	predicate in the list Background must have a definition in
%	module user or a module exporting to module user (e.g. the
%	current experiment file).
%
%	Metarules is a list of atoms, the names of metarules to be used
%	in learning.
%
%	Program is a list of definite Datalog clauses, the learned
%	definition of the target predicate. Traditionally a program
%	learned by an ILP algorithm is known as a _hypothesis_.
%
%	learn/5 returns each Program possible to learn from the given
%	data on successive backtracking.
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
%!	nondet.
%
%	Increment the search depth level.
%
%	Clause_Max, Invented_Max, Clauses and Invented are integers.
%	Clauses and Invented increase by one on backtracking. Clauses
%	ranges from 1 to Clauses_Max. Invented ranges from 0 to
%	Invented_Max or Clause-Max - 1, whichever is lower.
%
%	Explanation
%	===========
%
%	Thelma performs an iterative deepening search for a _hypothesis_
%	that explains its training examples.
%
%	A hypothesis is a list of definite datalog clauses that may
%	include definitions of one or more invented predicates. The
%	depth of the search at each iteration corresponds to the _size_
%	of the hypothesis, defined as the number of all clauses in the
%	hypothesis.
%
%	This predicate is responsible for controlling the depth of the
%	search by incrementing the values of Clauses and Invented.
%
%	Clauses is the maximum hypothesis size at the current search
%	depth. Invented is the maximum number of clauses in all invented
%	definitions in the hypothesis at the current search depth.
%
%	Clause_Max and Invented_Max are the maximum number of all
%	clauses in the hypothesis and the maximum number of all clauses
%	of invented predicates in the hypothesis, in the entire search.
%
%	Clauses and Invented are first bound to Clauses=1, Invented=0
%	and then to that number plus 1 on each successive backtracking
%	into depth_level/4. The search exits without further
%	backtracking when Clauses is equal to Clause_Max and Invented
%	equal to Invented_Max, or when no more hypotheses can be found.
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
%	Construct the program signature for a learning attempt.
%
%	The _program signature_ is a list of predicate symbols that can
%	be used to form a hypothesis. This list consists of:
%	a) The symbol and arity of the target predicate, at its head.
%	b) The symbols and arities of invented predicates, following.
%	c) The symbols and arities of background predicates ordered by
%	lexicographic order, at the end.
%
%	Invented is the maximum number of all clauses in invented
%	definitions in a hypothesis in the current search depth.
%
%	Target is the symbol and arity of the target predicate.
%
%	BK is a list of symbols and arities of predicates to be used as
%	background knowledge for Target.
%
%	Predicates is the program signature. The order of symbols and
%	arities of background knowledge predicates in BK is ordered by a
%	call to order_constraints/3.
%
%	Constants is the list of constants in the Herbrand universe of
%	the background knowledge predicates, also ordered by a call to
%	order_constraints/3.
%
%	@tbd It is a little strange that this predicate returns the list
%	of constants, alongside the program signature. Perhaps the call
%	to order_constraints/3 can be be moved outside this predicate,
%	in the body of learn/5. Then program_signature/3 can take the
%	resutling ordered list of BK predicates, add the symbols of the
%	target predicate and invented predicates to it, and return it.
%
program_signature(K,F/A,BK,Ps,Cs):-
	order_constraints(BK,Ps_,Cs)
	,invented_symbols(K,F/A,Ss)
	,append([F/A|Ss],Ps_,Ps).


%!	invented_symbols(+Symbols,+Target,-Invented) is det.
%
%	Create new symbols for Invented predicates.
%
%	Symbols is an integer, the number of invented predicates that
%	may be defined in the current search iteration.
%
%	Target is the symbol and arity of the target predicate.
%
%	Invented is a list of symbols and arities of invented predicates
%	that may be defined in the current search iteration. Each
%	invented predicate has the symbol of Target indexed by an
%	integer from 1 to Symbols and the same arity as Target.
%
%	@tbd What happens if a predicate must be invented with an arity
%	different than the arity of Target?
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
%	Examples is a list of lists, where each sublist is an atom in
%	the form of a list, [F|As], such that F is the predicate symbol
%	of the atom and As is the list of its terms.
%
%	Target is the symbol and arity of the learning target. The
%	predicate symbol and arity of Target are the predicate symbol
%	and number of terms in the first sub-list of Examples.
%
%	target_predicate/2 makes no attempt to check that Examples are
%	all atoms of the same predicate, etc.
%
target_predicate([[F|Args]|_Es],F/A):-
	length(Args,A).


%!	prove(+Depth,+Atoms,+BK,+Metarules,+Orders,-Metasubstitutions)
%!	is nondet.
%
%	Prove a list of Atoms and derive a list of Metasubstitutions.
%
%	Depth is the maximum depth for the iterative deepening search.
%	It's the maximum size of a hypothesis, i.e. the maximum number
%	of elements in the list Metasubstitutions.
%
%	Atoms is a list of positive examples of the learning target. It
%	is a list of lists where each sublist is an atom in the form of
%	a list [F|As], where F the symbol of the target predicate and As
%	the list of the atom's terms.
%
%	BK is a list of predicate symbols and arities of the background
%	knowledge predicates for the learning target.
%
%	Metarules is a list of atoms, the names of metarules for the
%	learning target.
%
%	Orders is a term Ps-Cs, where Ps is the program signature and Cs
%	is the _constant signature_ a list of all constants in the
%	Herbrand universe of the background predicates ordered by
%	interval inclusion order.
%
%	Metasubstitutions is a list of metasubstitutions. Each
%	metasubstitution is a Prolog coumpound, sub(Id, Ps). Id is the
%	name of a metarule in Metarules and Ps is a list of symbols and
%	arities of predicates in the program signature.
%
%	When prove/6 exits, each sub/2 term in the list of
%	Metasubstitutions is projected onto the corresponding metarule
%	to form a clause in a hypothesis. The hypothesis is then tested
%	for consistency with the negative examples in disprove/2 and
%	returned if it is consistent.
%
%	On backtracking, each list of metasubstitutions representing a
%	hypothesis that is correct with respect to the positive examples
%	and consistent withe the negative examples are returned.
%
prove(K,Pos,BK,MS,Os,Ss):-
	prove(K,Pos,BK,MS,Os,[],Ss_)
	,reverse(Ss_,Ss).


%!	prove(+Depth,+Atoms,+BK,+Metarules,+Orders,+Acc,-Metasubs)
%!	is nondet.
%
%	Business end of prove/7.
%
prove(_K,[],_BK,_MS,_PS,Ss,Ss):-
	!.
prove(K,[A|As],BK,MS,Os,Acc,Bind):-
	background_predicate(BK,A)
	,!
	,prove_atom(A)
	,prove(K,As,BK,MS,Os,Acc,Bind).
prove(K,[A|As],BK,MS,Os,Acc1,Bind):-
	select_metasub(Acc1,MS,A,Os,Bs)
	,prove(K,Bs,BK,MS,Os,Acc1,Acc2)
	,! % Very red cut. Avoids adding (many!)
	% redundant clauses- but will it cut
	% out necessary ones, also?
	,prove(K,As,BK,MS,Os,Acc2,Bind).
prove(K,[A|As],BK,MS,Os,Acc1,Bind):-
	new_metasub(K,Acc1,A,MS,Os,Acc2,Bs)
	,prove(K,Bs,BK,MS,Os,Acc2,Acc3)
	,prove(K,As,BK,MS,Os,Acc3,Bind).


%!	prove_atom(+Atom) is det.
%
%	Prove an Atom of a predicate in the Background knowledge.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
%	Atom is proved by calling it with call/1.
%
prove_atom(A):-
	A_ =.. A
	,user:call(A_).


%!	background_predicate(+BK,+Atom) is det.
%
%	True when Atom is an atom of a predicate in BK.
%
%	BK is a list of symbols and arities of predicates given as
%	background knowledge.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
background_predicate(BK,[F|Args]):-
	length(Args, A)
	,memberchk(F/A, BK).


%!	select_metasub(+Metasubs,+Metarules,+Atom,+Orders,-Body) is
%!	nondet.
%
%	Get the next known metasubstitution.
%
%	Metasubs is a list of metasubstitutions, the accumulator built
%	by prove/7 during learning.
%
%	Metarules is a list of constants, the names of metarules for the
%	learning target.
%
%	Atom is the atom currently being proved.
%
%	Orders is the association Ps-Cs of the predicate signature and
%	the constant signature, ordered by lexicographic and interval
%	orders.
%
%	Body is the list of body literals in a metasubstitution in
%	Metasubs. It is a list of lists, where each sub-list is of the
%	form [F|As], representing an atom of a predicate with symbol F
%	and where As are the terms of the atom.
%
select_metasub(Msubs,MS,A,Os,Bs):-
	member(Msub,Msubs)
	,once(metasubstitution(MS,A,Os,Msub,Bs)).


%!	new_metasub(+Depth,+Metasubs,+Atom,+Metarules,+Orders,-New,-Body)
%!	is nondet.
%
%	Create a new metasubstitution.
%
%	Depth is an integer, the maximum depth for the iterative
%	deepening search in prove/7.
%
%	Metasubs is a list of metasubstitutions, the accumulator built
%	by prove/7 during learning.
%
%	Atom is the atom currently being proved.
%
%	Metarules is a list of constants, the names of metarules for the
%	learning target.
%
%	Orders is the association Ps-Cs of the predicate signature and
%	the constant signature, ordered by lexicographic and interval
%	orders.
%
%	New is the accumulator of metasubstitutions extended by the
%	addition of a new metasubstitution, created by this predicate.
%
%	Body is the list of body literals in the new metasubstitution.
%	It is a list of lists, where each sub-list is of the form
%	[F|As], representing an atom of a predicate with symbol F and
%	where As are the terms of the atom.
%
%	new_metasub/7 fails if the length of Metasubs is equal to (or
%	larger than) Depth, which is when the learned hypothesis (i.e.
%	the accumulator of metasubstitutions) has reached the maximum
%	hypothesis size for the current search iteration.
%
new_metasub(K,Msubs,A,MS,Os,Msubs_,Bs):-
	length(Msubs,N)
	,N < K
	,metasubstitution(MS,A,Os,Msub,Bs)
	,save_metasub(Msub,Msubs,Msubs_).


%!	metasubstitution(+Metarules,+Atom,+Signature,?Metasub,-Body) is
%!	nondet.
%
%	Perform a second-order metasubstitution.
%
%	Metarules is a list of constants, the names of metarules for a
%	learning target.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
%	Signature is the program signature.
%
%	Metasub is a Prolog compound sub(Id,Hs), where Id is the id of a
%	metarule and Hs is a list of second-order variables, to be
%	eventually bound to the predicate symbols and arities in
%	Signature.
%
%	Body is a list of atoms in list form, [F|As], the body literals
%	of the clause resulting from the projection fo Metasub to one of
%	the metarules named in Metarules.
%
%	See project_metasubs/2 for an explanation of how a
%	metasubstitution is projected onto a metarule to produce a
%	clause.
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
%	Metarules is a list of constants, the names of metarules for a
%	learning target.
%
%	Metarule is a Prolog compound of the form:
%	==
%	F(Id, Ss, Fs, Bs)
%	==
%
%	Where F is the metarule symbol defined in metarule_functor/1; Id
%	is the name of a metarule in Metarules; Ss is the list of
%	existentially quantified terms in the metarule; Fs is the list
%	of universally quantified terms in the metarule; and Bs is a
%	list of atoms in list form, [F|As], the body literals in the
%	metarule.
%
next_metarule(MS,[Id,Ss,Fs,Bs]):-
	metarule_functor(F)
	,member(Id,MS)
	,M =.. [F,Id,Ss,Fs,Bs]
	,user:call(M).


%!	second_order_bindings(+Signature,+Constants,?Bindings) is
%!	nondet.
%
%	Bind second order terms to symbols in the Signature.
%
%	Signature is the program signature.
%
%	Constants is the constant signature, the list of constants in
%	the Herbrand universe of predicates in the background knowledge
%	ordered by interval inclusion order.
%
%	Bindings is a list of the existentially quantified terms in a
%	metarule. Each can be one of the following:
%	a) A term S/A, where both F and A are variables
%	b) A constant, or,
%	c) A single variable.
%
%	When a member of Bindings is a term S/A, S is bound to a symbol
%	and arity of a predicate in Signature. All possible bindings are
%	generated on backtracking.
%
%	When a member of Bindings is a constant, it is left alone.
%
%	When a member of Bindings is a single variable, if that variable
%	is also in the list Constants, it is understood to be a constant
%	in the learned hypothesis. All such variables must be together
%	at the end of the list Bindings.
%
%	The identity of two variables is tested in bound_constant/2 by
%	first skolemising all variables in Bindings and then checking
%	whether two skolemised terms match.
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
%	If Term is a variable, to test that it is included in
%	First_order, first Term and First_order are copied to a list
%	[Term_1|First_order_1]. Then, this list is skolemised (by
%	numbervars/3). Finally, First_order_1 is searched for Term_1.
%	bound_constant/2 succeeds only if Term is a constant, or if
%	Term_1 is in First_order_1.
%
bound_constant(C,_Fs):-
	atomic(C)
	,!.
bound_constant(C,Fs):-
	copy_term([C|Fs],[C_|Fs_])
	,numbervars([C_|Fs_])
	,memberchk(C_,Fs_).


%!	atom_symbol_arity(+Atom,-Predicate) is det.
%
%	Figure out the symbol and arity of an Atom given as a list.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
%	Predicate is a predicate indicator, F/A, where the predicate
%	symbol, F, is the predicate symbol in Atom and the arity, A, is
%	the length of the list of terms, As, in Atom.
%
atom_symbol_arity([A|As],A/N):-
	length(As,N).


%!	order_tests(+Predicates,+Constants,+First_Order,+Second_Order)
%!	is det.
%
%	Test the order constraints associated with a metarule.
%
%	Predicates is the program signature.
%
%	Constants is the constant signature.
%
%	First_order and Second_order are the lexicographic and interval
%	inclusion order constraints imposed by a metarule.
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
%	Term1 and Term2 are one of the following:
%	a) Two predicates in the program signautre
%	b) Two constants in the constant signature
%
%	Ordering is the corresponding ordering list: either the program
%	signature, or the constant signature, both of which are ordered
%	by order_constraints/3 at the start of a learning attempt.
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


%!	save_metasub(+Metasub,+Metasubs,-Metasubs_new) is det.
%
%	Add a new Metasubstitution to the list of Metasubstitutions.
%
%	Metasub is a metasubstitution in the form sub(Id, Ps), where Id
%	is the name of a metarule and Ps a list of symbols and arities
%	of predicates in the program signature.
%
%	Metasubs is the list of metasubstitutions accumulated so far by
%	prove/7.
%
%	Metasubs_new is the list [Metasub|Metasubs], if Metasub is not
%	already in Metasubs; otherwise, save_metasub/3 fails.
%
save_metasub(MS,Prog,[MS|Prog]):-
	save_metasub(MS,Prog).

%!	save_metasub(+Metasub,+Metasubs) is det.
%
%	Business end of save_metasub/3.
%
%	True when Metaub is not in Metasubs.
%
save_metasub(_,[]):-
	!.
save_metasub(MS1,[MS2|Prog]):-
	MS1 \== MS2
	,save_metasub(MS1,Prog).


%!	disprove(+Atoms,+Program) is det.
%
%	True when a Program does not cover negative examples.
%
%	Atoms is a list of negative examples of the learning target. It
%	is a list of lists where each sublist is an atom in the form of
%	a list [F|As], where F the symbol of the target predicate and As
%	the list of the atom's terms.
%
%	Program is a list of definite datalog clauses, a hypothesis
%	formed by a call to prove/6.
%
%	disprove/2 fails iff an atom in Atoms is entailed by Program.
%	This is tested by first asserting Program to the dynamic
%	database and then proving each atom in Atoms with call/1.
%	Program is retracted from the database after the proof
%	completes.
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
%	Metasubstitutions is a list of metasubstitutions accumulated by
%	prove/7, in the form sub(Id, Ps), where Id is the name of a
%	metarule and Ps is a list of existentially quantified terms
%	in the metarule. Ps may include the symbols and arities of
%	predicates in the signature, or constants.
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
%	Metasubstitution is a metasubstitution in the form sub(Id, Ps),
%	where Id is the name of a metarule and Ps is a list of
%	existentially quantified terms in the metarule. Ps may include
%	the symbols and arities of predicates in the signature, or
%	constants.
%
%	Clause is the projection of Metasubstitution onto the named
%	metarule, binding the members of Ps to the existentially
%	quantified terms in the metarule.
%
%	For example, suppose Metasubstitution is as follows:
%	==
%	sub(chain, [grandfather/2,father/2,parent/2]).
%	==
%
%	Projecting this metasubstitution onto the chain metarule will
%	produce the following clause:
%	==
%	grandfather(A,B):- father(A,C), parent(C,B).
%	==
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
%	Literals is a list of lists representing literals.
%
%	Atoms is a list of atoms, derived from the literals in the list.
%
project_metasub([],Ps,Ps_):-
	reverse(Ps, Ps_).
project_metasub([L|Ls],Acc,Ps):-
	L_ =.. L
	,project_metasub(Ls,[L_|Acc],Ps).


%!	literals_list_to_clause(+Literals,-Clause) is det.
%
%	Transforma  a list of Literals to a Clause.
%
%	Literals is a list of lists representing literals.
%
%	Clause is a definite clause, H:-B where the head literal, H, is
%	the first literal in Literals and the body literals, B, are the
%	remaining literals in Literals.
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

