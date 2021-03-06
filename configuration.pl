:-module(configuration, [default_ordering/1
			,depth_limits/2
			,experiment_file/2
			,learner/1
			,metarule/4
			,metarule_functor/1
			,order_constraints/5
			]).

:-user:use_module(src(experiment_file)).
:-reexport(lib(sampling/sampling_configuration)).
:-reexport(lib(program_reduction/reduction_configuration)).
:-reexport(lib(evaluation/evaluation_configuration)).

/** <module> Configuration options for Thelma.

*/

:-multifile metarule/4
	   ,order_constraints/5.

/* Debug levels */
%:-debug(depth). % Debug number of clauses and invented predicates.
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
experiment_file('data/examples/tiny_kinship.pl',tiny_kinship).
%experiment_file('data/examples/anbn.pl',anbn).
%experiment_file('data/examples/even.pl',even).
%experiment_file('data/examples/membership.pl',membership).
%experiment_file('data/examples/constants.pl',constants).
%experiment_file('data/examples/constants_2.pl',constants_2).
%experiment_file('data/examples/builtins.pl',builtins).
%experiment_file('data/examples/recipes.pl',recipes).


%!	learner(?Name) is semidet.
%
%	Name of the learning system this configuration is for.
%
%	Name is one of [louise,thelma].
%
%	Used to switch context between Louise and Thelma, where this is
%	needed. The typical use case is when experiment code must check
%	the values of configuration options that are particular to one
%	or the other system (e.g. depth_limits/2 is not present in
%	Louise etc).
%
learner(thelma).


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
metarule(unit, [P], [X,Y], mec(P,X,Y) :- true).
metarule(projection_21, [P,Q], [X,X], mec(P,X,X) :- mec(Q,X)).
metarule(projection_12, [P,Q], [X,X], mec(P,X) :- mec(Q,X,X)).
metarule(identity, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,X,Y)).
metarule(inverse, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,Y,X)).
metarule(chain, [P,Q,R], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).
metarule(tailrec, [P,Q], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(P,Z,Y))).
metarule(precon, [P,Q,R], [X,Y], (mec(P,X,Y) :- mec(Q,X), mec(R,X,Y))).
metarule(postcon, [P,Q,R], [X,Y], (mec(P,X,Y) :- mec(Q,X,Y), mec(R,Y))).
metarule(switch, [P,Q,R], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Y,Z))).

metarule(chain_abduce_x, [P,Q,R,X], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).
metarule(chain_abduce_y, [P,Q,R,Y], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).
metarule(chain_abduce_z, [P,Q,R,Z], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).


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
order_constraints(projection_21,[P,Q],_Fs,[P>Q],[]).
order_constraints(projection_12,[P,Q],_Fs,[P>Q],[]).
order_constraints(inverse,[P,Q],_Fs,[P>Q],[]).
order_constraints(identity,[P,Q],_Fs,[P>Q],[]).
order_constraints(chain,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(tailrec,[P,Q],[X,Y,Z],[P>Q],[X>Z,Z>Y]).
order_constraints(precon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(postcon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(switch,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(chain_abduce_x,[P,Q,R,_X],_Fs,[P>Q,P>R],[]).
order_constraints(chain_abduce_y,[P,Q,R,_Y],_Fs,[P>Q,P>R],[]).
order_constraints(chain_abduce_z,[P,Q,R,_Z],_Fs,[P>Q,P>R],[]).


%!	metarule_functor(?Functor) is semidet.
%
%	Functor for the internal representation of metarules.
%
metarule_functor('$metarule').


% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:-experiment_file:reload.
