:-module(configuration, [default_ordering/1
			,depth_limits/2
			,experiment_file/2
			,metarule/4
			,metarule_functor/1
			,order_constraints/5
			]).

:-reexport(lib(sampling/sampling_configuration)).
:-reexport(lib(program_reduction/reduction_configuration)).

/** <module> Configuration options for Thelma.

*/

:-multifile metarule/4
	   ,order_constraints/5.

/* Debug levels */
:-debug(depth). % Debug number of clauses and invented predicates.
%:-debug(learn). % Debug learned metasubs


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
depth_limits(2,1).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file('data/tiny_kinship.pl',tiny_kinship).
%experiment_file('data/anbn.pl',anbn).
%experiment_file('data/even.pl',even).
%experiment_file('data/constants.pl',constants).
%experiment_file('data/constants_2.pl',constants_2).


%!	metarule(?Name,?Second_order,?First_order,?Literals) is semidet.
%
%	A named Metarule.
%
%	Name is an atomic metarule handle- it can be an atom or a
%	number. It's only used as a reference, to find the metarule int
%	he dynamic database.
%
%	This option lists commonly used metarules. metarule/4 is marked
%	as multifile, so that experiment files can declare their own
%	special metarules if and as they need to, by adding a new
%	metarule/4 clause to user, like this:
%	==
%	:-module(my_experiment_file_module, [... ]).
%
%	% ...
%
%	user:metarule(my_special_metarule,....).
%
%	% ...
%
%	user:order_constraints(my_special_metarule,...).
%	==
%
%	Don't forget the order constraints!
%
%	@tbd Adding two metarules (and constraints) with the same name
%	will likely cause funny things to happen. Try it out if you're
%	in the mood and let me know.
%
/* Unit will need the ability to bind constants.*/
metarule(unit, [P], [X,Y], mec(P,X,Y) :- true).
metarule(projection, [P,Q], [X,X], mec(P,X,X) :- mec(Q,X)).
metarule(identity, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,X,Y)).
metarule(inverse, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,Y,X)).
metarule(chain, [P,Q,R], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).
metarule(tailrec, [P,Q], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(P,Z,Y))).
metarule(precon, [P,Q,R], [X,Y], (mec(P,X,Y) :- mec(Q,X), mec(R,X,Y))).
metarule(postcon, [P,Q,R], [X,Y], (mec(P,X,Y) :- mec(Q,X,Y), mec(R,Y))).


%!	order_constraints(+M,+Second_Order,+First_Order,+SO_Constraints,+FO_Constraints)
%!	is det.
%
%	A set of order constraints for a metarule, M.
%
%	Lists order constraints for commonly used metarules. Like
%	metarule/4, this predicate is also marked as multifile so
%	experiment files can declare their own special order
%	constraints. See notes in metarule/4.
%
order_constraints(unit,_Ss,_Fs,[],[]).
order_constraints(projection,[P,Q],_Fs,[P>Q],[]).
order_constraints(inverse,[P,Q],_Fs,[P>Q],[]).
order_constraints(identity,[P,Q],_Fs,[P>Q],[]).
order_constraints(chain,[P,Q,R],_Fs,[P>Q,P>R],[]).
% Bias reformulation paper lists the constraints of the tailrec metarule
% as P > Q and x > z > y; see Figure 3 in the paper.
order_constraints(tailrec,[P,Q],[X,Y,Z],[P>Q],[X>Z,Z>Y]).
order_constraints(precon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(postcon,[P,Q,R],_Fs,[P>Q,P>R],[]).


%!	metarule_functor(?Functor) is semidet.
%
%	Functor for the internal representation of metarules.
%
metarule_functor('$metarule').
