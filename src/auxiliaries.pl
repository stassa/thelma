:-module(auxiliaries, [experiment_data/6
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


%!	experiment_data(+Target,-Positive,-Negative,-BK,-Metarules,+Unload)
%!	is det.
%
%	Data about a Target theory from the current experiment file.
%
%	Unload is a boolean that denotes whether to unload the
%	experiment file after collecting all the required data from it,
%	or not.
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
