:-module(auxiliaries, [order_constraints/3
		      ,experiment_data/5
		      ,assert_program/2
		      ,assert_program/3
		      ,retract_program/2
		      ,erase_program_clauses/1
		      ,initialise_experiment/0
		      ,cleanup_experiment/0
		      ]).

:-use_module(configuration).
% Load libraries in /lib into module user.
:-user:use_module(lib(lifting/lifting)).
:-user:use_module(lib(bottom_clause/bottom_clause)).
:-user:use_module(lib(combinatorics/combinatorics)).
:-user:use_module(lib(term_utilities/term_utilities)).
:-user:use_module(lib(mathemancy/mathemancy)).
:-user:use_module(lib(program_reduction/program_reduction)).
:-user:use_module(lib(sampling/sampling)).
:-user:use_module(lib/tp/tp).
:-user:use_module(src(louise_compatibility)).

/** <module> Helper predicates for Thelma.

*/

:- dynamic user:'$metarule'/4.


%!	order_constraints(+Target,-Predicates,-Constants,+Default) is
%!	det.
%
%	Assign an automatic ordering to the Herbrand base.
%
%	Description
%	-----------
%
%	Target can be one of two things:
%	a) A compound, Q/N, the symbol and arity of a target predicate.
%	b) A list of predicates' symbols and arities.
%
%	If Target is a compound, Q/N, it's used to access the
%	definitions of the predicates declared as background knowledge
%	for the target predicate Q/N in the currently configured
%	experiment file.
%
%	If Target is a list of symbols and arities, it is used to
%	accesss the definitions of the predicates with those symbols and
%	arities in the currently configured experiment file.
%
%	The first option allows quick experimentation with the settings
%	in an experiment file. The second option allows for finer
%	control, e.g. to check the ordering of arbitrary combinations of
%	background predicates.
%
%	Note however that if you pass in a list of predicates' symbols
%	and arities as Target, their definitions must be in the current
%	experiment file module, or a module imported by the experiment
%	file module, otherwise an existence error will be raised.
%
%	Motivation
%	----------
%
%	The purpose of this predicate is to allow a user to define the
%	lexicographic and interval ordering of background predicates
%	declared in an experiment file and their constants, without any
%	additional work, besides the declaration of those predicates.
%	Specifically, it is meant to obviate the need for a special
%	predicate storing a hand-crafted list of each kind of term
%	in the required order.
%
%	The relevant ordering is instead automatically determined a) by
%	the order in which predicates are declared as background
%	knowledge (by including their symbols and aritities in the list
%	given as the second argument of the background_knowledge/2
%	predicate), b) in the order in which atoms of background
%	predicates are generated, or appear in the source file
%	(depending on whether a predicate is intentionally or
%	extensionally defined) and, c) the order in which constants
%	appear in predicates' atoms.
%
%	Although this ordering may still require some consideration on
%	the part of the user, it can be managed as part of the normal
%	process of declaring background knowledge which the user will
%	probably have to perform anyway.
%
%	Explanation
%	-----------
%
%	Predicates is a list representing an ordered sequence of unique
%	predicate symbols {P1, P2,... Pn} where each Pi is the symbol of
%	a predicate in the background knowledge and Pj is above Pk in
%	the lexicographic ordering of predicates iff {Pj,Pk} is a
%	subsequence of the sequence represented by Predicates.
%
%	Similarly, Constants is a list [C1, C2, ... Cn] where each
%	Ci is a constant in an atom of a background predicate and Cj
%	is above Ck in the interval ordering of constants iff [Cj,Ck]
%	are in Constants.
%
%	The two orderings are assigned according to the appearance of
%	(Prolog) terms in an experiment file, provided by the user.
%	Specifically:
%
%	a) Each predicate is assigned a rank equal to its position in
%	the list in the second argument of background_knowledge/2,
%	defined in the experiment file.
%
%	b) Each constant C is assigned one or more indexing terms I/J/K,
%	where: I is the rank of a predicate P such that one or more
%	atoms of P include C as a term; J is 1 if C is the first term or
%	2 if it's the second term of that predicate; and K is the index
%	of the atom of P in which C appears as a term (K coincides with
%	the index, in the dynamic database, of the clause of P
%	representing that atom, if P is defined extensionally; a clause
%	index can be obtained in Swi Prolog with the built-in
%	nth_clause/3).
%
%	The relative order of constants then depends on the I/J/K
%	indexing terms assigned to them. Given a pair of constants, C1,
%	C2, C1 is above C2 in the total interval ordering iff I1/J1/K1 >
%	I2/J2/K2 (where the indexing subscripts match the contstant
%	subscripts).
%
%	When a constant appears multiple times in different atoms of
%	possibly different predicates, it is assigned multiple
%	indexings. This is resolved by taking into account the
%	value of the configuration option default_ordering/1. If that is
%	set to "lower", the indexing resulting in the lowest possible
%	ordering is used to determine the order of the constant in
%	Constants. If it is set to "higher", the highest ordering is
%	used instead.
%
%	@tbd This could be refactored to allow passing an arbitrary list
%	of predicate indicators defined in any module (not just the
%	current experiment file), or, even better, an arbitrary list of
%	predicate definitions.
%
order_constraints(T,Ps,Cs):-
% Allow the user to override constraints - undocumented.
	experiment_file(_P,M)
	,predicate_property(M:predicate_order(_,_), defined)
	,M:predicate_order(T,Ps)
	,predicate_property(M:constant_order(_,_), defined)
	,M:constant_order(T,Cs)
	,!.
order_constraints(F/A,Ps,Cs):-
% Allow a target predicate's symbol as first argument.
	!
	,configuration:experiment_file(_P,M)
	,M:background_knowledge(F/A,BK)
	,order_constraints(BK,Ps,Cs).
order_constraints([],[],[]):-
% BK may be empty! e.g. see data/constants.pl
	!.
order_constraints(BK,Ps,Cs):-
	configuration:experiment_file(_P,M)
	,configuration:default_ordering(D)
	,must_be(oneof([lower,higher]),D)
	,predicate_order(BK,Ps)
	,constants_indexing(M,BK,Is)
	,unique_indices(Is,Is_,D)
	,sort(1,@<,Is_,Is_s)
	,findall(C
		,member(c(_/_/_,C),Is_s)
		,Cs).


%!	predicate_order(+Background,-Order) is det.
%
%	Derive a lexicographic Order on Background predicates.
%
%	Background is a list of predicate symbols and arities of
%	background predicates defined in the current experiment file.
%	Order is the same list ordered by lexicographic ordering.
%
%	@tbd Currently this simply returns the list Background in the
%	same way it receives it. The purpose of having a separate
%	predicate is that I'd also like to allow the user to set a
%	specific ordering to override the ordering in which background
%	predicates are declared in an experiment file, or even to
%	declare some more complex ordering logic.
%
predicate_order(BK,BK).


%!	constants_indexing(+Module,+Background,-Indexed) is det.
%
%	Index Background constants according to their declaration order.
%
constants_indexing(M,BK,Is):-
	findall(ITs
	       ,(nth1(I,BK,F/A)
		,functor(T,F,A)
		,findall(T
			,M:call(T)
			,Ts)
		,nth1(K,Ts,Ti)
		,Ti =.. Ti_
		,indexing_terms(F,I,K,Ti_,ITs)
		)
	       ,Cs_)
	,flatten(Cs_, Cs_flat)
	,sort(2,@=<,Cs_flat,Is).


%!	indexing_terms(+Symbol,+Predicate,+Atom,+Term,-Indexings) is
%!	det.
%
%	Assing an indexing to one or two arguments of a Term.
%
%	Predicate and Atom are the predicate rank and the index of its
%	current atom, used to assign an indexing to its arguments, as
%	detailed in order_constraints/2.
%
indexing_terms(F,I,K,[F,A1,A2],[c(I/1/K,A1),c(I/2/K,[])]):-
% Specifically meant to deal with DCGs where A1 may be a
% difference list and A2 a free var, e.g. ['A',[a|X],Y].
	var(A2)
	,!.
indexing_terms(F,I,K,[F,A1,A2],[c(I/1/K,A1),c(I/2/K,A2)]).
indexing_terms(F,I,K,[F,A1],[c(I/1/K,A1)]).


%!	unique_indices(+Indices,+Order,-Unique) is det.
%
%	Remove duplicate ordering indices.
%
unique_indices(Is,Is_,O):-
	unique_indices(O,Is,[],Is_).


%!	unique_indices(+Order,+Indices,+Acc,-Unique) is det.
%
%	Business end of unique_indices/2.
%
unique_indices(_,[],Is,Is):-
	!.
unique_indices(lower,[c(_/_/_,C1),c(I/J/K,C1)|Ss],Acc,Bind):-
	!
	,unique_indices(lower,[c(I/J/K,C1)|Ss],Acc,Bind).
unique_indices(higher,[c(I/J/K,C1),c(_/_/_,C1)|Ss],Acc,Bind):-
	!
	,unique_indices(higher,[c(I/J/K,C1)|Ss],Acc,Bind).
unique_indices(O,[c(I/J/K,C)|Ss],Acc,Bind):-
	unique_indices(O,Ss,[c(I/J/K,C)|Acc],Bind).



%!	experiment_data(+Target,-Positive,-Negative,-BK,-Metarules) is
%!	det.
%
%	Data about a Target theory from the current experiment file.
%
%	Target is the predicate indicator of the predicate to be
%	learned.
%
%	experiment_data/5 expects an experiment file to be loaded into
%	memory and will fail without warning otherwise.
%	initialise_experiment/0 should be called before it, and
%	cleanup_experiment/0 after it if cleanup is required between
%	experiments.
%
experiment_data(T,_,_,_,_):-
	learning_targets(Ts)
	,\+ memberchk(T,Ts)
	,throw('Unknown learning target':T).
experiment_data(T,Pos,Neg,BK,MS):-
	configuration:experiment_file(P,M)
	,use_module(P)
	,findall(E
		,M:positive_example(T,E)
		,Pos)
	,findall(E
		,M:negative_example(T,E)
		,Neg)
	,M:background_knowledge(T,BK)
	,M:metarules(T,MS).



%!	assert_program(+Module,+Program) is det.
%
%	Add all clauses of a Program into a Module.
%
assert_program(_,[]):-
	!.
assert_program(M,[C|P]):-
	assert(M:C)
	,assert_program(M,P).


%!	assert_program(+Module,+Program,-Clause_References) is det.
%
%	As assert_program/2 but also binds a list of Clause_References.
%
assert_program(M,Ps,Rs):-
	assert_program(M,Ps,[],Rs).

assert_program(_,[],Rs,Rs):-
	!.
assert_program(M,[C|P],Acc,Bind):-
	assert(M:C,Ref)
	,assert_program(M,P,[Ref|Acc],Bind).


%!	retract_program(+Module,+Program) is det.
%
%	Remove all clauses of a Program into a Module.
%
retract_program(_,[]):-
	!.
retract_program(M,[C|P]):-
	retract(M:C)
	,retract_program(M,P).


%!	erase_program_clauses(-Clause_References) is det.
%
%	Erase a list of Clause_References from the dynamic database.
%
%	Clause_References is meant to be a list of references of a
%	program's clauses asserted to the dynamic database with
%	assert_program/3.
%
%	The purpose of this predicate is, very specifically, to allow a
%	learned theory previously asserted by invoking assert_program/3
%	during disprove/2, to be removed from the dynamic database
%	without stumbling over module scoping that can be complicated
%	when a predicate is declared in one module and then clauses of
%	it are added in another module.
%
%	For example, the following is what you should expect to see in
%	the dynamic database after a theory of father/2 is learned and
%	asserted in the dynamic database, while there is also background
%	knowledge of father/2:
%
%	==
%	[debug] [1]  ?- listing(thelma:father/2).
%	:- dynamic tiny_kinship:father/2.
%
%	tiny_kinship:father(stathis, kostas).
%	tiny_kinship:father(stefanos, dora).
%	tiny_kinship:father(kostas, stassa).
%	tiny_kinship:father(A, C) :-
%	    thelma:
%	    (   father_1(A, B),
%	        parent(B, C)
%	    ).
%
%	true.
%	==
%
%	This happens because we allow the same experiment modules to
%	export background predicates that have the same symbol and
%	arities with target predicates declared in the same module. It
%	means that it's very fiddly to remove the clauses of the learned
%	theory, especially while leaving the background predicate
%	untouched.
%
erase_program_clauses([]):-
	!.
erase_program_clauses([Ref|Rs]):-
	erase(Ref)
	,erase_program_clauses(Rs).


%!	initialise_experiment is det.
%
%	Load the currently configured experiment file.
%
initialise_experiment:-
	configuration:metarule(Id,_,_,_)
	,\+ configuration:order_constraints(Id,_,_,_,_)
	,format(atom(E),'Missing constraints for metarule ~w',[Id])
	,throw(E).
initialise_experiment:-
	configuration:experiment_file(P,_M)
	,user:use_module(P)
	,transform_metarules.


%!	transform_metarules
%
%	Transform metarules to internal representation.
%
%	@tbd Probably use bagof/3 or setof/3 here?
%
transform_metarules:-
	findall(metarule(Id,Ss,Fs,Bs)
	       ,configuration:metarule(Id,Ss,Fs,Bs)
	       ,Ms)
	,assert_metarules(Ms).

/*?- thelma:transform_metarules.
$metarule(projection,[A,B],[C],[[A,C,C],[B,C]])
$metarule(identity,[A,B],[C,D],[[A,C,D],[B,C,D]])
$metarule(inverse,[A,B],[C,D],[[A,C,D],[B,D,C]])
$metarule(chain,[A,B,C],[D,E,F],[[A,D,E],[B,D,F],[C,F,E]])
true.*/


%!	assert_metarules(+Metarules) is det.
%
%	Add a list of Metarules to the dynamic database.
%
assert_metarules([]):-
	!.
assert_metarules([metarule(Id,_,_,_)|Ms]):-
% A metarule with this Id already exists in the dynamic database.
	metarule_functor(F)
	,T =.. [F,Id,_,_,_]
	,predicate_property(T, number_of_clauses(N))
	,N > 1
	,call(user:T)
	,!
	,assert_metarules(Ms).
assert_metarules([metarule(Id,Ss,Fs,Bs)|Ms]):-
	metarule_functor(F)
	,metarule_body(Bs, [], Bs_)
	,symbols_arities(Ss,Bs_,Ss_)
	,T =.. [F,Id,Ss_,Fs,Bs_]
	,user:assert(T)
	%,numbervars(T) ,writeln(T)
	,assert_metarules(Ms).


%!	symbols_arities(+Symbols,+Literals,-Predicate_Indicators) is
%!	det.
%
%	Derive the Predicate_Indicators of a list of predicate Symbols.
%
%	In the configuration notation for metarules, second-order
%	predicate symbols are not given an arity. Here, their arities
%	are derived from the arities of the literals in the encapsualted
%	body of a metarule.
%
symbols_arities(Ss,Bs,Ss_):-
	symbols_arities(Ss,Bs,[],Ss_).

%!	symbols_arities(+Symbols,+Literals,+Acc,-PIs) is det.
%
%	Business end of symbols_arities/3.
%
symbols_arities([],[],Ss,Ss_):-
	reverse(Ss,Ss_)
	,!.
symbols_arities([],[_],Ss,Ss_):-
% Handles tailrec metarule where one symbol is shared by two literals.
% Not convinced this is robust enough to cover any recursive pattern.
	reverse(Ss,Ss_)
	,!.
symbols_arities(As,[],Ss,Ss_):-
% Handles metarules meant to bind constants.
% These should always come after existentially quantified variables.
% Stupid reverse-append-reverse needs killing.
        reverse(Ss, Ss_r)
	,append(Ss_r,As,Ss_)
	,!.
symbols_arities([S|Ss],[[S|As]|Bs],Acc,Bind):-
	length(As,N)
	,symbols_arities(Ss,Bs,[S/N|Acc],Bind).


%!	metarule_body(+Metarule,+Acc,-Body) is det.
%
%	Transform a metarule's body from a tree to a list.
%
metarule_body((H:-true),Acc,Bind):-
	!
	,metarule_body(H,Acc,Bind).
metarule_body((H:-Bs),Acc,Bind):-
	H =.. [_F|As]
	,!
	,metarule_body(Bs,[As|Acc],Bind).
metarule_body((L,Ls),Acc,Bind):-
	L =.. [_F|As]
	,!
	,metarule_body(Ls,[As|Acc],Bind).
metarule_body((L),Acc,Bs_):-
	L =.. [_F|As]
	,reverse([As|Acc], Bs_).



%!	cleanup_experiment is det.
%
%	Unload the currently configured experiment file.
%
%	Also removes metarule clauses from the dynamic database.
%
cleanup_experiment:-
	%configuration:experiment_file(P,_M)
	%,unload_file(P)
	%,
	cleanup_metarules.


%!	cleanup_metarules is det.
%
%	Remove transformed metarules from the dynamic database.
%
cleanup_metarules:-
	metarule_functor(F)
	,functor(T,F,4)
	,user:retractall(T).
