:-module(combinatorics, [random_permutations/4
			,random_combinations/4
			,max_permutations/4
			,powerset_member/2
			,n_tuple/3
			,partial_permutation/3
			,combination/3
			]).

/** <module> Generate list permutations.
*/

%!	max_permutations(+Type,+N,+K,-Max) is det.
%
%	Maximum number of K-permutations of a set of length N.
%
%	Type is one of: [ordered,unordered].
%
%	For Type = ordered Max is the number of ordered subsets of
%	length K of a set of length N, calculated as:
%	==
%	N! / (N	- K)!
%	==
%
%	For Type = unordered, Max is the number of un-ordered subsets of
%	length K of aset of length N, calculated as:
%	==
%	N! / (N	- K)!K!
%	==
%
%	Example:
%	==
%	?- max_permutations(ordered,100,2,_M),format('~G~n',[_M]).
%	9900 true.
%
%	?- max_permutations(unordered,100,2,_M),format('~G~n',[_M]).
%	4950 true.
%	==
%
%	Use this to figure out the maximum number of N-permutations of a
%	list before requesting a number in excess of this maximum from
%	random_permutations/4, in which case an error will be raised.
%
%	Alternatively, this can be used to figure out the maximum number
%	of N-combinations that should be returned by
%	random_combinations/4, although that predicate will not throw a
%	hissy fit if you ask for more, since it does not guarantee
%	uniqueness of the returned permutations.
%
max_permutations(ordered,N,K,M):-
	!
	,factorial(N,FN)
	,D is N - K
	,factorial(D,FD)
	,M is FN / FD.
max_permutations(unordered,N,K,M):-
	factorial(N,FN)
	,factorial(K,FK)
	,D is N - K
	,factorial(D,FD)
	,M is FN / (FD * FK).


%!	factorial(+N,-Factorial) is det.
%
%	Calculate the Factorial of N
%
factorial(0,1):-
	!.
factorial(N, F):-
	factorial(N, 1, F).

%!	factorial(+N,+F,-G) is det.
%
%	Business end of factorial/2.
%
factorial(1,F,F):-
	!.
factorial(N,F,G):-
	F_ is N * F
	,N_ is N - 1
	,factorial(N_,F_,G).



%!	partial_permutation(+K, +Xs, ?Ys) is det.
%
%	Ys is a K-permutation of Xs.
%
%	A K-permutation of a set S, or a partial permutation of S, is
%	an ordered subset of S, of length K.
%
%	The number of partial permutations of length k, of a set of
%	length n is given by the formula:
%	==
%	n! / (n - k)!
%	==
%
partial_permutation(K, Xs, Ys):-
	combination(K, Xs, Xs_)
	,permutation(Xs_, Ys).


%!	combination(+K,+Xs,?Ys) is nondet.
%
%	Ys is the K-element combination of Xs.
%
%	A K-element combination of a set, S, is an un-ordered K-element
%	subset of S.
%
%	The number of combinations of length k of a set of length n is:
%	==
%	n! / (n - k)!k!
%	==
%
combination(K, Xs, Ys):-
	must_be(nonvar, Xs)
	,must_be(nonvar, K)
	,combination_(K, Xs, Ys).


%!	combination_(+Ks,+Xs,?Ys) is nondet.
%
%	Business end of combination/3.
%
%	Copied from 99 Prolog problems:
%	https://sites.google.com/site/prologsite/prolog-problems/1
%
combination_(0,_,[]).
combination_(K,L,[X|Xs]) :-
	K > 0
	,del(X,L,R)
	,K1 is K-1
	,combination_(K1,R,Xs).


%!	del(+X, +Xs, -Ys) is det.
%
%	Ys is Xs with X removed.
%
del(X,[X|L],L).
del(X,[_|L],R) :-
	del(X,L,R).



%!	powerset_member(-Subset,+Set) is nondet.
%
%	True when Subset is a member of the powerset of Set.
%
powerset_member(Ys, Xs):-
	length(Xs,N)
	,between(0,N,K)
	,partial_permutation(K,Xs,Ys).



%!	n_tuple(+N,+Xs,-Ys) is det.
%
%	Generate all N-tuples of elements in list Xs.
%
%	An n-tuple is a sequence of length n, with repetitions, drawn
%	from the elements in a set. For a set of cardinality k, there
%	are k^n n-tuples.
%
n_tuple(K, Xs, Ys):-
	length(Ys,K)
	,n_tuple(Xs,Ys).

%!	n_tuple(+Xs,-Ys) is det.
%
%	Business end of n_tuple/3.
%
n_tuple(_,[]):-
	!.
n_tuple(Xs,[X|Ys]):-
	member(X,Xs)
	,n_tuple(Xs,Ys).



%!	random_permutations(+K,+N,+List,-Permutations) is det.
%
%	Collect K random N-Permutations of a List.
%
%	Permutations is a list of K unique lists of N unique elements
%	drawn from List. In other words, it is a random set of size K of
%	N-permutations of the input List.
%
%	Raises Type error if K is greater than the maximum number of
%	partial permutations of length N of List, calculated as:
%	==
%	n! / (n - k)!
%	==
%
%	Where n is the length of List and k = K.
%
%	Permutations is created by generating a random permutation of
%	List and either keeping it if it is not already included in
%	Permutations, or discarding it and generating a new one
%	otherwise.
%
%	The point of this predicate is to sample unique sub-lists of
%	length N from List at random without having to first generate
%	all such sublists. The trade-off is that as K approaches the
%	maximum number of N-permutations of List more and more
%	duplicates are found, meaning more and more duplicate steps are
%	taken, which might really slow down processing.
%
random_permutations(K,N,Xs,Ys):-
	length(Xs,L)
	,max_permutations(ordered,L,N,M)
	,must_be(between(0,M),K)
	,empty_nb_set(S)
	,random_permutations(0,K,N,Xs,S)
	,nb_set_to_list(S, Ys).


%!	random_permutations(+I,+K,+N,+Xs,-Ys) is det.
%
%	Business end of random_permutations/4.
%
%	This predicate proceeds by generating a random permutation of Xs
%	in each timestep, then either keeping it if it is unique, or
%	discarding it and trying another random permutation, if it
%	isn't.
%
%	I is a counter ranging over K, updated each time a new
%	permutation of length N of the list Xs is generated.
%
%	Uniquneness is tested by adding a permutation to a
%	non-backtrackable set. If the permutation is already a member of
%	the set, a new permutation is generated and tested.
%
random_permutations(K,K,_N,_Xs,_S):-
	!.
random_permutations(I,K,N,Xs,S):-
	random_permutation(Xs,Xs_)
	,last_k(N,Xs_,Ns)
	,add_nb_set(Ns, S, true)
	,!
	,succ(I,I_)
	,random_permutations(I_,K,N,Xs,S).
random_permutations(I,K,N,Xs,S):-
	random_permutations(I,K,N,Xs,S).



%!	random_combinations(+K,+N,+List,-Combinations) is det.
%
%	Collect K random N-Combinations of List.
%
%	Combinations is a list of K lists of N unique elements drawn
%	from List. Combinations might include duplicates.
%
random_combinations(K,N,Xs,Ys):-
	random_combinations(0,K,N,Xs,[],Ys).

random_combinations(K,K,_N,_Xs,Ys,Ys):-
	!.
random_combinations(I,K,N,Xs,Acc,Bind):-
	random_permutation(Xs, Xs_)
	,last_k(N, Xs_,Ns)
	,succ(I,I_)
	,random_combinations(I_,K,N,Xs,[Ns|Acc],Bind).


%!	last_k(+K,+List,-Last_K) is det.
%
%	Last K elements of a List.
%
%	Used in random_combinations/4 to collect the last K elements of
%	a List. Because it's easier and more efficient to get the last K
%	elements using difference lists than it is to split List to its
%	K first and rest however many elements using append/3.
%
last_k(K,Xs,Ys):-
	length(Xs,N)
	,must_be(between(0,N),K)
	,J is N - K
	,length(Ls,J)
	,phrase(Ls,Xs,Ys).
