:-module(configuration, [default_ordering/1
			,depth_limits/2
			,experiment_file/2
			,metarule/4
			,metarule_functor/1
			]).

/** <module> Configuration options for Thelma.

*/

/* Debug levels */
:-debug(depth). % Debug number of clauses and invented predicates.


%!	default_ordering(?Order) is semidet.
%
%	The default for automatically assigned interval ordering.
%
%	Order is one of [higher,lower] and determines what to do when a
%	constant is assigned multiple indexings in the process of
%	automatically determining its interval ordering.
%
default_ordering(lower).


%!	depth_limits(?Clauses,?Invented) is semidet.
%
%	The maximum number of Clauses and Invented predicates.
%
depth_limits(4,3).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file('data/tiny_kinship.pl',tiny_kinship).


%!	metarule(?Name,?Second_order,?First_order,?Literals) is semidet.
%
%	A named Metarule.
%
%	Name is an atomic metarule handle- it can be an atom or a
%	number. It's only used as a reference, to find the metarule int
%	he dynamic database.
%
metarule(projection, [P,Q], [X], mec(P,X,X) :- mec(Q,X)).
metarule(identity, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,X,Y)).
metarule(inverse, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,Y,X)).
metarule(chain, [P,Q,R], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).


%!	metarule_functor(?Functor) is semidet.
%
%	Functor for the internal representation of metarules.
%
metarule_functor('$metarule').
