:-module(bottom_clause, [ground_bottom_clause/3
			,bottom_clause/3
			]).

:-use_module(configuration).
:-use_module(src/auxiliaries).
:-use_module(lib(lifting/lifting)).

/** <module> Construct the most specific clause in a subsumption lattice.

*/


%!	ground_bottom_clause(+Example,+Signature,-Clause) is det.
%
%	As bottom_clause/3 but Clause is ground.
%
%	Useful for debugging purposes, mostly.
%
ground_bottom_clause(E,Ps,(H:-Bs)):-
	E =.. [_F|Ts]
	,sort(Ts, Ts_)
	,bottom_clause(Ts_,Ps,[],Cs)
	,once(list_tree([E|Cs],(H,Bs)))
	,once(list_tree([E|Cs],(H:-Bs))).


%!	bottom_clause(+Example,+Signature,-Clause) is det.
%
%	Construct a bottom Clause.
%
%	Example is a ground unit clause. Signature is a list of
%	predicate symbols and their arities of background predicates.
%	Clause is a single definite clause, the most specific clause in
%	the subsumption lattice between clauses entailing Example.
%
bottom_clause(E,Ps,C):-
	E =.. [_F|Ts]
	,sort(Ts, Ts_)
	,bottom_clause(Ts_,Ps,[],Cs)
	,once(list_tree([E|Cs],(H,Bs)))
	,lifted_program([H:-Bs],[C]).


%!	bottom_clause(+Terms,+Signature,+Acc,-Clause) is det.
%
%	Business end of bottom_clause/3.
%
%	Performs one outer step in the bottom clause construction
%	process, where atoms of each predicate in the Signature are
%	instantiated to each of a list of Terms and theose atoms that
%	are true are added to the bottom Clause.
%
bottom_clause(_Ts,[],Bs,Bs):-
	!.
bottom_clause(Ts,[P|Ps],Acc1,Bind):-
	bottom_clause(Ts,P,Ts,Acc1,Acc2)
	,!
	,bottom_clause(Ts,Ps,Acc2,Bind).
bottom_clause(Ts,[_P|Ps],Acc,Bind):-
	bottom_clause(Ts,Ps,Acc,Bind).


%!	bottom_clause(+Terms,+Predicate,+Difference,+Acc,-Clause) is
%!	det.
%
%	Business end of bottom_clause/4.
%
%	Performs one inner step of the bottom clause construction
%	process where atoms of a single predicate are instantiated to
%	a list of Terms and those atoms that are found true are added to
%	the bottom Clause.
%
bottom_clause(_Ts,_P,[],Acc,Bs):-
	flatten(Acc,Bs)
	,!.
bottom_clause(Ts,P,_Ds,Acc,Bind):-
	instantiations_of(Ts,P,Cs)
	,flatten(Cs, Cs_f)
	,new_literals(Acc,Cs_f,Acc_)
	,new_terms(Ts,Cs,Ns)
	,ord_union(Ts, Ns, Ts_)
	,!
	,bottom_clause(Ts_,P,Ns,Acc_,Bind).
bottom_clause(_Ts,_P,_Ds,Acc,Bs):-
	flatten(Acc,Bs).


%!	instantiations_of(+Terms,+Predicate,-Instantiations) is det.
%
%	Perform all Instantiations of a Predicate to a list of Terms.
%
instantiations_of(Ts,F/A,Ps):-
	findall(P
	       ,(member(T,Ts)
		,functor(P,F,A)
		,P =.. [F|As]
		,member(T,As)
		,call(P)
		)
	       ,Ps).


%!	new_terms(+Current,+Found,-New) is det.
%
%	Collect terms found in the current step.
%
new_terms(Ts,Cs,Ns):-
	setof(A
	     ,C^Cs^As^Ts^(member(C,Cs)
			 ,C =.. [_|As]
			 ,member(A, As)
			 ,\+ memberchk(A, Ts)
			 )
	     ,Ns).


%!	new_literals(+Old,+Found,-New) is det.
%
%	Collect New literals found in the current step.
%
new_literals(Ls1,Ls2,Ls3):-
	reverse(Ls1,Ls1_)
	,new_literals_(Ls2,Ls1_,Ls3).

new_literals_([],Acc,Ls):-
	reverse(Acc,Ls)
	,!.
new_literals_([L|Ls],Acc,Bind):-
	\+ memberchk(L,Acc)
	,!
	,new_literals_(Ls,[L|Acc],Bind).
new_literals_([_L|Ls],Acc,Bind):-
	new_literals_(Ls,Acc,Bind).
