:-module(membership, [background_knowledge/2
		     ,metarules/2
		     ,positive_example/2
		     ,negative_example/2
		     ,head/2
		     ,tail/2
		     ]).

/** <module> Example setups to learn membership relation.

This file shows how to learn the membership relation, typically defined
as member/2. Two different setups allow learning two definitions: one
that traverses its input list by use of an accumulator variable and one
that traverses its input list using the background predicates head/2 and
tail/2.

The two setups show how Thelma can be trained to learn typical Prolog
list traversal skeletons that are found in many list-processing
programs.

Usage
=====

1. Ensure this file is set as the current experiment file in
configuration.pl

==
experiment_file('data/examples/membership.pl',membership).
==

2. Set sufficient clause limits in configuration.pl:

==
depth_limits(2,0).
==

3.1. Run the following query to learn a definition of the membership
relation that uses an accumulator:

==
?- learn(member_acc/2).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
member_acc(A,[A|B]).
member_acc(A,[B|C]):-member_acc(A,C).
true .
==

3.2. Run the following query to learn a definition of the membership
relation that uses head/2 and tail/2.

==
?- learn(member_nacc/2).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
member_nacc(A,B):-head(B,A).
member_nacc(A,B):-tail(B,C),member_nacc(A,C).
true .
==

Learning a program with an accumulator variable
===============================================

This setup uses two metarules that explicitly bind universally
quantified variables to a difference list (i.e. a list of the form
[H|T]).

==
head: P(X,[X|Y])
tail: P(X,[Y|Z]):- P(X,Z)
==

The combination of the two metarules represents a program that
implements an accumulator. This is a typical Prolog list traversal
skeleton that recursively picks a list apart to process its elements
one-by-one. In this case, no processing is needed other than to bind
each element when the base-case clause is reached. This means that this
setup does not need any additional background knowledge.

Learning a program with head/2 and tail/2
=========================================

This setup uses the inverse metarule (from the main configuration file)
and a tail-recursive metarule that allows the use of the background
predicates tail/2 and head/2 to traverse a list.

==
inverse:  P(X,Y):- Q(Y,X)
rec_tail: P(X,Y):-Q(Y,Z),P(X,Z)
==

The end result of using this setup is the same as for the
explicit-accumulator setup. However, this one is a little less specific
and so can be more useful when one is not sure exactly how literals
should best share variables.

Remarks
=======

Note that both the tail metarule and rec_tail metarule are
tail-recursive and that both bind the last variable of the last literal
to a rest-of-list variable.

These metarules may look overly specific, but in practice, they define a
very common pattern found in many Prolog programs that process lists and
so they are likely to be useful in many other problems.
*/

configuration:metarule(head, [P], [X,Y], mec(P,X,[X|Y]) :- true).
configuration:metarule(tail, [P], [X,Y,Z], mec(P,X,[Y|Z]) :- mec(P,X,Z)).
configuration:metarule(rec_tail, [P,Q], [X,Y,Z], (mec(P,X,Y):-mec(Q,Y,Z),mec(P,X,Z))).

configuration:order_constraints(head,_Ss,_Fs,[],[]).
configuration:order_constraints(tail,_Ss,_Fs,[],[]).
configuration:order_constraints(rec_tail,[P,Q],[X,Y,Z],[P>Q],[X>Y,Y>Z]).


%!	background_knowledge(?Target, ?Background) is semidet.
%
%	A list of Background knowledge for the Target predicate.
%
%	Make sure all the predicates listed in Background are defined in
%	and exported from this module to module user.
%
background_knowledge(member_acc/2,[]).
background_knowledge(member_nacc/2,[head/2,tail/2]).

%!	metarules(?Target, ?Metarules) is semidet.
%
%	A list of Metarules for the Target predicate.
%
%	Metarules must be a list of atoms that match the names of named
%	metarules, defined in metarule/4 clauses. Each must also have a
%	corresponding order_constraints/5 clause.
%
metarules(member_acc/2,[head,tail]).
metarules(member_nacc/2,[inverse,rec_tail]).


%!	positive_example(+Target, -Example) is nondet.
%
%	Generator of positive examples of a Target predicate.
%
positive_example(member_acc/2,E):-
	member(E, [member_acc(a, [a,b])
		  ,member_acc(a, [b,a])
		  ]).
positive_example(member_nacc/2,E):-
	member(E, [member_nacc(a, [a,b])
		  ,member_nacc(a, [b,a])
		  ]).


%!	negative_example(+Target, -Example) is nondet.
%
%	Generator of negative examples of a Target predicate.
%
%	Negative examples can be empty. In this case, this predicate
%	should fail, not bind Example to an empty list!
%
negative_example(member_acc/2,E):-
	member(E, [member_acc(_, [])
		  ,member_acc(x, [a,b,c])
		  ]).
negative_example(member_nacc/2,E):-
	member(E, [member_nacc(_, [])
		  ,member_nacc(x, [a,b,c])
		  ]).


%!	head(?List,?Head) is semidet.
%
%	True when Head is the head of List.
%
head([X|_Xs],X).

%!	tail(?List,?Tail) is semidet.
%
%	True when Tail is the tail of List.
%
tail([_X|Xs],Xs).
