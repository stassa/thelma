:-module(sampling, [goal_samples/4
		   ,goal_sample/4
		   ,goal_partition/4
		   ,n_list_partitions/3
		   ,p_list_partitions/4
		   ,p_list_samples/3
		   ,k_list_partitions/4
		   ,k_list_samples/3
		   ,list_samples/3
		   ,list_sample/3
		   ,list_partition/4
		   ,list_partitions/4
		  ]).

:-use_module(configuration).

/** <module> Randomly sample from the results of a goal.
*/


%!	n_list_partitions(+Size,+List,-Partitions) is det.
%
%	Partition a List to random Partitions of equal Size.
%
%	Size may be an integer or a float. If it is an integer,
%	Partitions is a list of sublists of List of the given Size. If
%	it is a float, the length of sublists is Size * |List|. In both
%	cases, the elements of each sublist are selected at random and
%	without replacement and the union of Partitions is List.
%
%	Example:
%	==
%	?- interval(1,10,1,_Is), n_list_partitions(0.5,_Is,Ps).
%	Ps = [[1, 4, 7, 8, 9], [2, 3, 5, 6, 10]].
%
%	?- interval(1,10,1,_Is), n_list_partitions(5,_Is,Ps).
%	Ps = [[1, 2, 3, 6, 8], [4, 5, 7, 9, 10]].
%	==
%
%	Raises an error if the length of List cannot be divided exactly
%	by Size.
%
%	Note that when Size is a float, because of floating point
%	arithmetic it may be necessary to give multiple decimal digits
%	to avoid errors. For instance:
%	==
%	?- interval(1,9,1,_Is), sampling:n_list_partitions(0.3,_Is,Ps).
%	ERROR: Unhandled exception: 'Partition size must be exact divisor of list size!'
%
%	?- interval(1,9,1,_Is), sampling:n_list_partitions(0.3333333333333333,_Is,Ps).
%	Ps = [[2, 6, 8], [3, 4, 5], [1, 7, 9]].
%	==
%
%	Er. I think the exact size of decimal digits depends on the
%	words size of your computer's processor architecture. That's as
%	far as I'm prepared to think about this.
%
n_list_partitions(S,Ls,Ps):-
	valid_partition_size(S,Ls)
	,(   integer(S)
	 ->  K = S
	 ;   float(S)
	    ,length(Ls, N)
	    ,K is round(S * N)
	 )
	,n_list_partitions(K,Ls,[],Ps).


%!	valid_partition_size(+Size,+List) is det.
%
%	True when Size is a valid size for equal partitioning of List.
%
%	Raises an error when false.
%
valid_partition_size(S,Ls):-
	integer(S)
	,length(Ls,N)
	,M is N mod S
	,M > 0
	,throw('Partition size must be exact divisor of list size!').
valid_partition_size(S,Ls):-
	float(S)
	,length(Ls,N)
	,M is N * S
	,X is float_fractional_part(M)
	,P is N mod round(M)
	,(   X > 0
	 ;   P > 0
	 )
	,throw('Partition size must be exact divisor of list size!').
valid_partition_size(_,_).


%!	n_list_partitions(+K,+List,+Acc,-Bind) is det.
%
%	Business end of n_list_partitions/3.
%
n_list_partitions(_K,[],Ps,Ps):-
	!.
n_list_partitions(K,Ls,Acc,Bind):-
	k_list_partitions(K,Ls,Ks,Ss)
	,n_list_partitions(K,Ss,[Ks|Acc],Bind).



%!	p_list_partitions(+Ratio,+List,-Keeps,-Skips) is det.
%
%	Partition a list into two disjoint partitions.
%
%	Keeps and Skips are two subsets of the elements of List such
%	that:
%	a) List is the union of Keeps and Skips, and,
%	b) The intersection of Keeps and Skips is empty.
%
%	Ratio is the ratio of elements of List in Keeps. It may be given
%	as either an integer or a floating-point. The number of elements
%	in Skips is the complement of Ratio out of 100 (if Ratio is an
%	integer) or out of 1.0 (if a float).
%
%	If Ratio is an integer, it must be between 1 and 100; if it is a
%	floating point number, it must be between 0.1 and 1.0.
%	Otherwise, a type error is raised.
%
%	Example:
%	==
%	?- interval(1,10,1,_Is), p_list_partitions(0.2,_Is,Ks,Ss).
%	Ks = [5, 3],
%	Ss = [4, 2, 1, 6, 7, 8, 9, 10].
%
%	?- interval(1,10,1,_Is), p_list_partitions(20,_Is,Ks,Ss).
%	Ks = [8, 7],
%	Ss = [6, 5, 4, 3, 2, 1, 9, 10].
%	==
%
%	Use this to create two list partitions when a) you know the size
%	of the original List, b) you need partitions of a specific size
%	or ratio and c) you don't mind storing three, possibly large,
%	lists in memory.
%
%	@tbd The "three, possibly large, lists" that must be kept in
%	memory, mentioned above are List, Keeps and Skips, although note
%	that List gets smaller as the other two grow, so the amount of
%	memory taken up by this should not be much larger than twice the
%	size of List. I think.
%
p_list_partitions(P,Ls,Ks,Ss):-
	integer(P)
	,!
	,p_list_partitions(int,P,Ls,Ks,Ss).
p_list_partitions(P,Ls,Ks,Ss):-
	float(P)
	,!
	,p_list_partitions(float,P,Ls,Ks,Ss).


%!	p_list_partitions(+Type,+Ratio,+List,-Keeps,-Skips) is det.
%
%	Business end of p_list_partitions/4.
%
%	Clauses are selected according to Type, which is one of [int,
%	float], to distinguish between a Ratio given as an integer
%	(taken to indicate a percentage) or a float (taken as a
%	fraction).
%
p_list_partitions(int,P,Ls,Ks,Ss):-
	must_be(between(1,100),P)
	,length(Ls, N)
	,K is max(round(P * N / 100), 1)
	% Generate K random indices between 1 and N
	,randset(K,N,Is)
	,p_list_partitions(1,Is,Ls,[],Ks,[],Ss).
p_list_partitions(float,P,Ls,Ks,Ss):-
	must_be(between(0.0,1.0),P)
	,length(Ls, N)
	,K is max(round(P * N), 1)
	,randset(K,N,Is)
	,p_list_partitions(1,Is,Ls,[],Ks,[],Ss).


%!	p_list_partitions(+I,+Is,+List,+Ks_Acc,-Ks_Bind,+Ss_Acc,-Ss_Bind)
%!	is det.
%
%	Business end of p_list_partitions/5.
%
%	Is is a list of randomly generated indices of the elements to
%	select for membership in the set of Keeps. I is the index of the
%	current element in List. When the two coincide, the current
%	element is added to the Keep-list accumulator, Ks_Acc, else it's
%	added to the Skip-list accumulator, Ss_Acc.
%
p_list_partitions(_,[],[],Ks,Ks,Ss,Ss):-
	!.
p_list_partitions(_,[],Ls,Ks,Ks,Ss_,Ss):-
	append(Ss_,Ls,Ss)
	,!.
p_list_partitions(I,[I|Is],[L|Ls],Ks_Acc,Ks_Bind,Ss_Acc,Ss_Bind):-
	!
	,succ(I,I_)
	,p_list_partitions(I_,Is,Ls,[L|Ks_Acc],Ks_Bind,Ss_Acc,Ss_Bind).
p_list_partitions(I,Is,[L|Ls],Ks_Acc,Ks_Bind,Ss_Acc,Ss_Bind):-
	succ(I, I_)
	,p_list_partitions(I_,Is,Ls,Ks_Acc,Ks_Bind,[L|Ss_Acc],Ss_Bind).



%!	k_list_partitions(+K,+List,-Keeps,-Skips) is det.
%
%	Partition a list to K and remaining elements.
%
%	As p_list_partitions/4, but K is an integer denoting the exact
%	number of elements in Keeps, rather than a ratio.
%
%	K is an integer. List is a list. Keeps is a list of K elements
%	from List selected at random and without replacement and Skips
%	is a list of the elements remaining in List when Keeps is
%	substracted from it.
%
%	Example:
%	==
%	?- interval(1,10,1,_Is), k_list_partitions(3,_Is,Ks,Ss).
%	Ks = [3, 4, 9],
%	Ss = [1, 2, 5, 6, 7, 8, 10].
%	==
%
k_list_partitions(K,Ls,Ks,Ss):-
	length(Ls,N)
	,must_be(between(0,N),K)
	,randset(K,N,Is)
	,p_list_partitions(1,Is,Ls,[],Ks_,[],Ss_)
	,sort(Ks_, Ks)
	,sort(Ss_, Ss).



%!	p_list_samples(+Proportion,+List,-Sample) is det.
%
%	Randomly sample the given Proportion of elements from a list.
%
%	As k_list_samples/3 but Percentage is either an integer from 1
%	to 100, denoting the size of Sample as a percentage of List, or
%	a float denoting the size of Sample as a fraction of List.
%
%	As long as Proportion > 0, at least 1 element will be selected
%	for Sample.
%
%	Example:
%	==
%	?- interval(1,10,1,_Is), p_list_samples(30,_Is,Ss).
%	Ss = [2, 5, 10].
%
%	?- interval(1,10,1,_Is), p_list_samples(0.30,_Is,Ss).
%	Ss = [2, 4, 9].
%
%	?- interval(1,10,1,_Is), p_list_samples(0.001,_Is,Ss).
%	Ss = [1].
%
%	?- interval(1,10,1,_Is), p_list_samples(0.0,_Is,Ss).
%	Ss = [].
%
%	?- interval(1,10,1,_Is), p_list_samples(0,_Is,Ss).
%	Ss = [].
%	==
%
p_list_samples(R,_,[]):-
	R =:= 0
	,!.
p_list_samples(P,Ls,Ss):-
	integer(P)
	,!
	,must_be(between(1,100),P)
	,length(Ls, N)
	,K is max(round(P * N / 100),1)
	,randset(K,N,Is)
	,L =.. [l|Ls]
	,findall(S
		,(member(I,Is)
		 ,arg(I,L,S)
		 )
		,Ss).
p_list_samples(P,Ls,Ss):-
	float(P)
	,must_be(between(0.0,1.0),P)
	,length(Ls, N)
	,K is max(round(P * N),1)
	,randset(K,N,Is)
	,L =.. [l|Ls]
	,findall(S
		,(member(I,Is)
		 ,arg(I,L,S)
		 )
		,Ss).



%!	k_list_samples(+K,+List,-Samples) is det.
%
%	Draw exactly K Samples from List with uniform probability.
%
%	The way samples are drawn depends on the value of the
%	configuration option, k_random_sampling/1, which must be one of
%	[knuth,randset].
%
%	Option "knuth" implements Knuth's algorith S for drawing a
%	random sample of size n from a set of length m, where m is not
%	necessarily known beforehand and each item in the set has an
%	equal probability of being selected.
%
%	Option "randset" generates a list of random integers using
%	library(random)'s randset/3, then walks over the input List
%	selecting each element whose position matches one of the random
%	indices.
%
%	There are tradeoffs between the two methods: Knuth's algorithm
%	must necessarily walk over the entire list to select a sample,
%	whereas randset must only walk over the list of random indices.
%	randset is slow when the sample size is small and the list
%	length is high (according to the documentation for randset/3).
%	However, both methods should return the same number of elements
%	and they should both generate uniformly random results.
%
%	Both methods sample without replacement.
%
%	Raises type error if K is not between 0 and the length of List.
%
k_list_samples(K,Ls,Ss):-
	configuration:k_random_sampling(S)
	,length(Ls,N)
	,must_be(between(0,N),K)
	,k_list_samples(S,K,N,Ls,Ss).


%!	k_list_samples(+Type,+K,+Length,+List,-Samples) is det.
%
%	Business end of k_list_samples/3.
%
%	Clauses are selected according to Type, taking values from the
%	configuration option k_list_sampling/1.
%
%	Length is the length of List. This is only used when randset is
%	the k_list_sampling/1 value.
%
k_list_samples(randset,K,N,Ls,Ss):-
	!
	,randset(K,N,Is)
	,k_list_samples_(Is,Ls,Ss).
k_list_samples(knuth,K,_N,Ls,Ss):-
	first_k_rest(K,Ls,Ks,Rs)
	,replace_ks(K,Ks,Rs,Ss).


%!	k_list_samples(+Positions,+List,-Elements) is det.
%
%	Collect the Elements of List at the given Positions.
%
k_list_samples_(Is,Ls,Ss):-
	k_list_samples_(1,Is,Ls,[],Ss).

%!	k_list_samples_(+Positions,+List,+Acc,-Elements) is det.
%
%	Business end of k_list_samples_/3.
%
k_list_samples_(_,[],_,Acc,Ss):-
	sort(Acc,Ss)
	,!.
k_list_samples_(I,[I|Is],[L|Ls],Acc,Bind):-
	!
	,succ(I,I_)
	,k_list_samples_(I_,Is,Ls,[L|Acc],Bind).
k_list_samples_(I,Is,[_L|Ls],Acc,Bind):-
	succ(I,I_)
	,k_list_samples_(I_,Is,Ls,Acc,Bind).


%!	replace_ks(+K,+Xs,+Ys,-Zs) is det.
%
%	Randomly replace each of K elements in a sample.
%
%	K is the sample size; Xs is the K elements sampled so far; Ys is
%	the list of remaining elements; Zs is the list Xs where each
%	element is replaced by the i-th element in Ys with probability
%	k/i.
%
%	@tbd Normally, Knuth's algorithm expects Ys to be unknown and
%	its elements to come in one by one, such as from backtracking
%	over a goal. Here we're cheating a bit and selecting from a
%	list.
%
replace_ks(K,Xs,Ys,Zs):-
	I is K + 1 % First number after K
	,replace_ks(I,K,Ys,Xs,Zs).

%!	replace_ks(+I,+K,+Ys,+Xs,-Zs) is det.
%
%	Business end of replace_ks/4.
%
%	I is the index of the current item in Ys, after K.
%
%	@tbd Note that the order of Xs, Ys in the arguments list is
%	reversed here; Ys is the list of current items after the first K
%	selected deterministically in order; Xs is the list of the first
%	K items, to be replaced randomly by elements in Ys. Satan made
%	me do it.
%
replace_ks(_,_,[],Zs,Zs):-
	!.
replace_ks(I,N,[Y|Ys],Xs,Bind):-
	random_between(1,I,R)
	,R < N
	,!
	,random_select(_,Xs,Xs_)
	,succ(I, I_)
	,replace_ks(I_,N,Ys,[Y|Xs_],Bind).
replace_ks(I,N,[_|Ys],Xs,Bind):-
	succ(I, I_)
	,replace_ks(I_,N,Ys,Xs,Bind).


%!	first_k_rest(+K,+Xs,-Ys,-Zs) is det.
%
%	Splice a list to elements before and after the Kth.
%
first_k_rest(K,Xs,Ys,Zs):-
	length(Ys,K)
	,append(Ys,Zs,Xs).



%!	goal_samples(+Probability,+Module,+Goal,-Samples) is det.
%
%	Sample from the solutions of Goal, with the given Probability.
%
%	Module is the name of the module where Goal is defined.
%
%	Examples:
%	==
%	?- goal_samples(2,user,between(1,100,C),Ss).
%	Ss = [between(1, 100, 27)
%	     ,between(1, 100, 68)
%	     ,between(1, 100, 76)
%	     ,between(1, 100, 94)].
%	==
%
goal_samples(P, M, G, Ss):-
	findall(S
	       ,goal_sample(P, M, G, S)
	       ,Ss).



%!	goal_sample(+Probability,+Module,+Goal,-Sample) is det.
%
%	Draw a random sample from the soutions of Goal.
%
%	Module is the name of the module where Goal is defined.
%
%	Example:
%	==
%	?- goal_sample(2,user,between(1,100,C),Ss).
%	C = 14,
%	Ss = between(1, 100, 14) ;
%	C = 28,
%	Ss = between(1, 100, 28) ;
%	C = 93,
%	Ss = between(1, 100, 93) ;
%	false.
%	==
%
goal_sample(P, M, G, G):-
	goal_partition(P,M,G,Keep)
	,Keep.



%!	goal_partition(+P,+Module,+Goal,-Keep) is det.
%
%	Keep, or skip, results of Goal according to a probability value.
%
%	This predicate performs a Bernoulli trial with P as the
%	probability of success.
%
%	Module is the name of the module where Goal is defined (or a
%	module that imports it, e.g. 'user').
%
%	If Goal is true, then the atom 'true' is bound to Keep with
%	probability P or to 'false' with probability 1-P. If Goal fails,
%	goal_partition/3 fails.
%
%	Example:
%	==
%	?- goal_partition(10,user,between(1,5,C),Ss).
%	C = 1,
%	Ss = false ;
%	C = 2,
%	Ss = false ;
%	C = 3,
%	Ss = false ;
%	C = 4,
%	Ss = true ;
%	C = 5,
%	Ss = false.
%	==
%
goal_partition(P, M, G, Keep):-
	must_be(between(1,100), P)
	,M:G
	,random_between(1,100,R)
	,(   R =< P
	 ->  Keep = true
	 ;   Keep = false
	 ).



%!	list_samples(+Probability,+List,-Samples) is det.
%
%	Draw Samples from List with the given Probability.
%
%	Performs Bernoulli sampling.
%
%	For each element of List, the probability that it will be added
%	to Samples is equal to the given Probability value.
%
list_samples(P, Ls, Ss):-
	findall(S
	       ,list_sample(P,Ls,S)
	       ,Ss).



%!	list_sample(+Probability,+List,-Sample) is det.
%
%	Draw a Sample from List with the given Probability.
%
%	Performs Bernoulli sampling.
%
%	Each member of List has the same Probability to be bound to
%	Sample.
%
%	@tbd Unlike list_partition/4 on which it is based, this
%	predicate does not allow a partial element to be passed as a
%	"filter".
%
list_sample(P,Ls,S):-
	list_partition(P,S,Ls,Keep)
	,Keep.



%!	list_partitions(+Probability,+List,-Keeps,-Skips) is det.
%
%	Partition List according to a Probability value.
%
%	Keeps is a list of elements in List, selected with the given
%	Probability, whereas Skips is the rest of the List.
%
%	List doesn't have to be of a uniform type (i.e. not every
%	element of it needs to have the same functor and arity). Each
%	element in List will be selected with the same Probability (or
%	not selected with the same 1-Probability).
%
%	Example:
%	==
%	?- interval(1,5,1,_Is)
%	,findall(n(I,K),(member(I,_Is),nextto(I,K,_Is)),_Ns)
%	,list_partitions(10,_Ns,Ks,Ss).
%	Ks = [n(2, 3)],
%	Ss = [n(1, 2), n(3, 4), n(4, 5)].
%	==
%
list_partitions(P, Ls, Ks, Ss):-
	empty_nb_set(Ks_)
	,empty_nb_set(Ss_)
	,forall(list_partition(P, L, Ls, Keep)
	       ,(   Keep
		->  add_nb_set(L, Ks_, true)
		;   add_nb_set(L, Ss_, true)
		)
	       )
	,nb_set_to_list(Ks_, Ks)
	,nb_set_to_list(Ss_, Ss).



%!	list_partition(+Probability,:Element,+List,-Keep) is det.
%
%	Keep, or skip, Elements of a List according to a Probability.
%
%	Probability can be given as either a real number, in [0,1],
%	or a percentage, in [0,100].
%
%	Element may be a nonground compound term, for instance
%	p(X,Y). If List is of a non-uniform type, this will act as a
%	filter of sorts, only binding terms that conform to the pattern
%	in Element. For this purpose, it is not necessary that List is
%	of a uniform type (i.e. not every element of it needs to have
%	the same functor and arity). Elements not matching Element are
%	simply not selected.
%
%	Example:
%	==
%	?- interval(1,5,1,_Is)
%	,findall(n(I,K),(member(I,_Is),nextto(I,K,_Is)),_Ns)
%	,list_partition(10,n(X,Y),_Ns,K).
%	K = false,
%	X = 1,
%	Y = 2 ;
%	K = false,
%	X = 2,
%	Y = 3 ;
%	K = true,
%	X = 3,
%	Y = 4 ;
%	K = false,
%	X = 4,
%	Y = 5.
%	==
%
list_partition(P,L,Ls,Keep):-
	must_be(between(1,100),P)
	,must_be(list, Ls)
	,member(L, Ls)
	,random_between(1,100,R)
	,(   R =< P
	 ->  Keep = true
	 ;   Keep = false
	 ).
