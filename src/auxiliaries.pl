:-module(auxiliaries, [order_constraints/3
		      ,experiment_data/6
		      ,predicate_signature/1
		      ,assert_program/2
		      ,retract_program/2
		      ,initialise_experiment/0
		      ,cleanup_experiment/0
		      ,print_clauses/1
		      ,tree_list/2
		      ]).

:-use_module(configuration).

/** <module> Helper predicates for Thelma.

*/

:- dynamic user:'$metarule'/4.


%!	order_constraints(-Predicates,-Constants,+Default) is det.
%
%	Assign an automatic ordering to the Herbrand base.
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
%	the order in which predicates are declaed as background
%	knowledge (by including their symbols and aritities in the list
%	given as the first argument of the background_knowledge/1
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
%	Default must be one of [lower, higher], denoting whether
%	constants found in multiple predicates are ordered according to
%	their higher, or lower possible ordering.
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
%	terms in an experiment file, provided by the user. Specifically:
%
%	a) Each predicate is assigned a rank equal to its position in
%	the list in the first argument of background_knowledge/1,
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
%	value of Default. If Default is lower, the indexing resulting in
%	the lowest possible ordering is used to determine the order of
%	the constant in Constants. If Default is higher, the highest
%	ordering is used instead.
%
order_constraints(Ps,Cs,D):-
	must_be(oneof([lower,higher]),D)
	,configuration:experiment_file(_P,M)
	,M:background_knowledge(BK)
	,predicate_order(BK,Ps)
	,constants_indexing(M,BK,Is)
	,unique_indices(Is,Is_,D)
	,sort(1,@<,Is_,Is_s)
	,findall(C
		,member(c(_/_/_,C),Is_s)
		,Cs).


%!	predicate_order(+Background,-Order) is det.
%
%	Order predicates by their appearance in the Background.
%
%	@tbd Note that this is essentially the same as
%	predicate_signature/1.
%
predicate_order(BK,Ps):-
	findall(F
	       ,member(F/_,BK)
	       ,Ps).


%!	constants_indexing(+Module,+Background,-Indexed) is det.
%
%	Index Background constants according to their declaration order.
%
constants_indexing(M,BK,Is):-
	findall([c(I/1/J,A1),c(I/2/J,A2)]
	       ,(nth1(I,BK,F/A)
		,functor(T,F,A)
		,findall(T
			,M:call(T)
			,Ts)
		,nth1(J,Ts,Ti)
		,Ti =.. [F,A1,A2]
		)
	       ,Cs_)
	,flatten(Cs_, Cs_flat)
	,sort(2,@=<,Cs_flat,Is).


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



%!	experiment_data(+Target,-Positive,-Negative,-BK,-Metarules,+Unload)
%!	is det.
%
%	Data about a Target theory from the current experiment file.
%
%	Unload is a boolean that denotes whether to unload the
%	experiment file after collecting all the required data from it,
%	or not.
%
%	@tbd This should not load and unload the experiment file.
%	initialise_experiment/0 and cleanup_experiment/0 should be the
%	only points where an experiment file is loaded and unloaded and
%	every predicate that wants to access terms in the experiment
%	file must be called between calls to those two.
%
experiment_data(T,Pos,Neg,BK,MS,Ul):-
	configuration:experiment_file(P,M)
	,use_module(P)
	,findall([F|As]
		,(M:positive_example(T,E)
		 ,E =.. [F|As]
		 ),
		 Pos)
	,findall([F|As]
		,(M:negative_example(T,E)
		 ,E =.. [F|As]
		 )
		,Neg)
	,M:background_knowledge(BK)
	,M:metarules(MS)
	,(   Ul
	 ->  unload_file(P)
	 ;   true
	 ).



%!	predicate_signature(+Signature) is det.
%
%	Predicate Signature from the current experiment file.
%
predicate_signature(PS):-
	configuration:experiment_file(_,M)
	,M:background_knowledge(BK)
	,findall(F
		,member(F/_A,BK)
		,PS).



%!	assert_program(+Module,+Program) is det.
%
%	Add all clauses of a Program into a Module.
%
assert_program(_,[]):-
	!.
assert_program(M,[C|P]):-
	assert(M:C)
	,assert_program(M,P).



%!	retract_program(+Module,+Program) is det.
%
%	Remove all clauses of a Program into a Module.
%
retract_program(_,[]):-
	!.
retract_program(M,[C|P]):-
	retract(M:C)
	,retract_program(M,P).



%!	initialise_experiment is det.
%
%	Load the currently configured experiment file.
%
initialise_experiment:-
	configuration:experiment_file(P,_M)
	,user:use_module(P)
	,transform_metarules.



%!	cleanup_experiment is det.
%
%	Unload the currently configured experiment file.
%
cleanup_experiment:-
	configuration:experiment_file(P,_M)
	,unload_file(P)
	,cleanup_metarules.


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
assert_metarules([metarule(Id,Ss,Fs,_Bs)|Ms]):-
	metarule_functor(F)
	,predicate_property(T, number_of_clauses(N))
	,N > 1
	,T =.. [F,Id,Ss,Fs|[_]]
	,call(user:T)
	,!
	,assert_metarules(Ms).
assert_metarules([metarule(Id,Ss,Fs,Bs)|Ms]):-
	metarule_functor(F)
	,metarule_body(Bs, [], Bs_)
	,T =.. [F,Id,Ss,Fs,Bs_]
	,user:assert(T)
	%,numbervars(T) ,writeln(T)
	,assert_metarules(Ms).


%!	metarule_body(+Metarule,+Acc,-Body) is det.
%
%	Transform a metarule's body from a tree to a list.
%
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


%!	cleanup_metarules is det.
%
%	Remove transformed metarules from the dynamic database.
%
cleanup_metarules:-
	metarule_functor(F)
	,functor(T,F,4)
	,user:retractall(T).



%!	print_clauses(+Clauses) is det.
%
%	Print a list of Clauses to standard output.
%
print_clauses(Cs):-
	forall(member(C,Cs)
	      ,(write_term(C, [fullstop(true)
			      ,nl(true)
			      ,numbervars(true)])
	       )
	      ).



%!	tree_list(+Tree, -List) is det.
%
%	Convert between a Prolog Tree and a List
%
tree_list('()', []):-
	!.
tree_list(Ts, Ls):-
	phrase(list_tree(Ts), Ls)
	,!.


%!	list_tree(?Tree) is nondet.
%
%	Business end of list_tree/2.
%
list_tree((T,Ts)) --> [T], list_tree(Ts).
list_tree((T:-Ts)) --> [T], list_tree(Ts).
list_tree(T) --> [T].
