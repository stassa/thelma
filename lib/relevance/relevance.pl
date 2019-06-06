:-module(relevance, [predicate_determinations/2
		    ]).

/** <module> Relevance identification and scoring.
*/


%!	predicate_determinations(+Predicate,-Determinations) is det.
%
%	Find all Determinations of a Predicate.
%
%	Predicate is the predicate indicator of a target predicate.
%
%	Determinations is a list of key-value pairs, P-Q, where both P
%	and Q are predicate indicators: P is the predicate indicator of
%	a target predicate (which is not necessarily Predicate) and Q is
%	the predicate indicator of one determination of P.
%
%	Determinations includes key-value pairs relating Predicate to
%	each of its determinations and all the determinations of
%	Predicate to _their_ determinations.
%
%	A predicate S is a determination of a predicate T if S is
%	a literal determination of T, or if a predicate P is a
%	literal determination of S and P is a determination of T.
%
%	Or, in other words:
%	==
%	determination(S,T):-
%		literal_determination(S,T).
%	determination(S,T):-
%		literal_determination(S,P)
%		,deteramination(P,T).
%	==
%
%	Although note that this is not how predicate_determinations/2 is
%	actually defined. The nice, clean definition above does not
%	terminate when the target predicate, or one of its
%	determinations, is defined recursively. For this reason,
%	predicate_determinations/2 keeps track of determinations of a
%	target found so far and avoids adding the same one twice.
%
%	See literal_determination/2 for an explanation of what a
%	"literal determination" is.
%
predicate_determinations(P,Ds):-
	predicate_determinations(P,[],Ds).

%!	predicate_determinations(+Predicate,+Acc,-Determinations) is det.
%
%	Business end of predicate_determinations/2.
%
predicate_determinations(P,Acc1,Ds):-
	literal_determination(P,Q)
	,\+ memberchk(P-Q,Acc1)
	,!
	,predicate_determinations(Q,[P-Q|Acc1],Acc2)
	,predicate_determinations(P,Acc2,Ds).
predicate_determinations(_P,Ds,Ds).


%!	literal_determination(+Predicate,-Determination) is nondet.
%
%	Find a literal Deteramination of a Predicate.
%
%	Predicate is a predicate indicator of a target predicate.
%	Determination is the predicate indicator of a predicate that is
%	a literal determination of the target predicate.
%
%	A predicate P is a literal determination of a predicate Q iff an
%	atom of P is a literal in a clause of a definition of Q.
%
%	All literal determinations of the target predicate are generated
%	on backtracking.
%
literal_determination(P,F/A):-
	program([P],user,Ps)
	,member(C,Ps)
	,clause_literals(C,[_H|Bs])
	,member(L,Bs)
	,functor(L,F,A).
