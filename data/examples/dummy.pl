:-module(dummy, [background_knowledge/2
		,metarules/2
		,positive_example/2
		,negative_example/2
		,bk1/1
		]).

/** <module> Dummy experiment file.

Use as skeleton to structure your own experiment files.

Replace target/1, bk1/1 with the target and background predicates you
want.
*/

% Special metarule and order constraints for this file. These are not
% always necessary. If you do need special metarules, make sure they are
% declared as shown below, with a "configuration:" module qualifier.
%
% configuration:metarule(my_unit, [P], [X,Y], mec(P,X,Y) :- true).
% configuration:order_constraints(my_unit,_Ss,_Fs,[],[]).


% Predicate and constant orders can be defined by hand if required.
% That is usually not necessary. The automatic ordering assigned by
% order_constraints/3 should serve most of the time.
%
% predicate_order(target/1,[bk1/1]).
% constant_order(target/1,[1,2]).


%!	background_knowledge(?Target, ?Background) is semidet.
%
%	A list of Background knowledge for the Target predicate.
%
%	Make sure all the predicates listed in Background are defined in
%	and exported from this module to module user.
%
background_knowledge(target/1,[bk1/1]).


%!	metarules(?Target, ?Metarules) is semidet.
%
%	A list of Metarules for the Target predicate.
%
%	Metarules must be a list of atoms that match the names of named
%	metarules, defined in metarule/4 clauses. Each must also have a
%	corresponding order_constraints/5 clause.
%
metarules(target/1,[unit]).


%!	positive_example(+Target, -Example) is nondet.
%
%	Generator of positive examples of a Target predicate.
%
positive_example(target/1,E):-
	member(E, [target(1)
		  ,target(2)
		  ]).


%!	negative_example(+Target, -Example) is nondet.
%
%	Generator of negative examples of a Target predicate.
%
%	Negative examples can be empty. In this case, this predicate
%	should fail, not bind Example to an empty list!
%
negative_example(target/1,_):-
	fail.

% Background predicate declarations.
bk1(1).
bk1(2).
