:-module(builtins, [background_knowledge/2
		   ,metarules/2
		   ,positive_example/2
		   ,negative_example/2
		   ]).

/** <module> Use builtins as background knowledge.
*/

%!	background_knowledge(?Target, ?Background) is semidet.
%
%	A list of Background knowledge for the Target predicate.
%
background_knowledge(successor/2,[succ/2]).

%!	metarules(?Target, ?Metarules) is semidet.
%
%	A list of Metarules for the Target predicate.
%
metarules(successor/2,[identity]).

%!	positive_example(+Target, -Example) is nondet.
%
%	Generator of positive examples of a Target predicate.
%
positive_example(successor/2,E):-
	member(E, [successor(1,2)
		  ,successor(2,3)
		  ,successor(3,4)
		  ]).

%!	negative_example(+Target, -Example) is nondet.
%
%	Generator of negative examples of a Target predicate.
%
negative_example(successor/2,_):-
	fail.

% First arg must be the list of BK symbols and arities.
predicate_order([succ/2],[succ/2]).
constant_order([succ/2],[]).
