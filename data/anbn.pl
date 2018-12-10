:-module(anbn, [background_knowledge/1
	       ,positive_example/2
	       ,negative_example/2
	       ,metarules/1
	       ,'A'/2
	       ,'B'/2
	       ]).

background_knowledge(['A'/2,'B'/2]).

metarules([tailrec]).

positive_example('S',E):-
	member(E, ['S'([a,b],[])
		  ,'S'([a,a,b,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ,'S'([a,a,a,a,b,b,b,b],[])
		  /*,'S'([a,a,a,a,a,b,b,b,b,b],[])
		  ,'S'([a,a,a,a,a,a,b,b,b,b,b,b],[])
		  ,'S'([a,a,a,a,a,a,a,b,b,b,b,b,b,b],[])
		  ,'S'([a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,b],[])
		  ,'S'([a,a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,b,b],[])
		  ,'S'([a,a,a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,b,b,b],[])*/
		  ]).

negative_example('S',_):-
	fail.

'A'([a|A], A).
'B'([b|A], A).


% Target theory, as ordinary Prolog.
/*
'S'(A, C):- 'A'(A, B),'B'(B, C).
'S'(A,B):- 'A'(A,C),'S_1'(C,B).
'S_1'(A,B):-'S'(A,C),'B'(C,B).

% With an invented predicate equivalent to 'S'/2:

'S'(A, C) :- 'A'(A, B), 'B'(B, C).
'S'(A, C) :- 'A'(A, B),'S_1'(B, C).
'S_1'(A, C) :- 'S_2'(A, B),'B'(B, C).
'S_2'(A, C) :- 'A'(A, B), B'(B, C).
'S_2'(A, C) :- 'A'(A, B),'S_1'(B, C).

*/

% Target theory, as a DCG.
/*
'S' --> 'A', 'B'.
'S' --> 'A', 'S_1'.
'S_1' --> 'S', 'B'.
'A' --> [a].
'B' --> [b].

% With an invented 'S'//0 equivalent:

'S' --> 'A', 'B'.
'S' --> 'A', 'S_1'.
'S_1' --> 'S_2', 'B'.
'S_2' --> 'A', 'B'.
'S_2' --> 'A', 'S_1'.
'A' --> [a].
'B' --> [b].

*/

/*
Metagol fails to learn anything with the current setup. With fewer
positive examples, it learns incorrect hypotheses, like the one below,
learned from the first 4 positive examples in learn_an_bn/0:

'S'(A,B):-'S_1'(A,C),'S_1'(C,B).
'S'(A,B):-'A'(A,C),'S_1'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
'S_1'(A,B):-'S_2'(A,C),'S_2'(C,B).
'S_1'(A,B):-'B'(A,C),'S_2'(C,B).
'S_1'(A,B):-'A'(A,C),'S_2'(C,B).
'S_2'(A,B):-'A'(A,C),'A'(C,B).
'S_2'(A,B):-'B'(A,C),'B'(C,B).

Example of incorrect derivations of this grammar:

?- phrase('S', P).
P = [a, a, a, a, a, a, a, a] ;
P = [a, a, a, a, a, a, b, b] ;
P = [a, a, a, a, b, b, a, a] ;
P = [a, a, a, a, b, b, b, b] ;
P = [a, a, a, a, b, a, a] ;
P = [a, a, a, a, b, b, b] ;
P = [a, a, a, a, a, a, a] ;
P = [a, a, a, a, a, b, b] ;

*/

/*
%'S' --> [].
%'S' --> 'T', 'S'.
'T' --> [].
'T' --> [0] ,'U'.
'U' --> 'T', [1].
*/

/*
Target theory from discussions with Stephen:
'T' --> [].
'T' --> 'A' ,'U'.
'U' --> 'T', 'B'.

'T'(A, A).
'T'(A, C) :-
        'A'(A, B),
        'U'(B, C).

'U'(A, C) :-
        'T'(A, B),
        'B'(B, C).

This would also need this metarule:

metarule([P], ([P,A,A]:- [])).
*/


