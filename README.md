Thelma - a Theory Learning Machine for Meta-Interpretive Learning
=================================================================

Example of use
--------------

Thelma runs on Swi-Prolog version 8.0.0 or later.

Follow the steps below to run an example that learns the a^nb^n CFG from three
positive examples.


### Edit configuration.pl:

Choose the a^nb^n experiment file:

```prolog
experiment_file('data/anbn.pl',anbn). 
```

Set an appropriate depth limit:

```prolog
depth_limits(3,1).
```

### Load the project:

```prolog
?- [load].
```

This opens source files in the Swi IDE and starts the documentation browser. You
should see this README file in your browser.

### Initialise the experiment:

```prolog
?- initialise_experiment.
true.
```

### Run a query:

```prolog
?- experiment_data('S'/2,_Pos,_Neg,_BK,_MS), learn(_Pos,_Neg,_Prog), print_clauses(_Prog).
% Clauses: 1; Invented: 0
% Clauses: 2; Invented: 0
% Clauses: 2; Invented: 1
% Clauses: 3; Invented: 0
% Clauses: 3; Invented: 1
S(A,B):-A(A,C),B(C,B).
S(A,B):-S_1(A,C),B(C,B).
S_1(A,B):-A(A,C),S(C,B).
true ;
```

### Remember to cleanup afterwards to avoid strange errors later on:

```prolog
?- cleanup_experiment.
true.
```

### Understanding the results of the example

The hypothesis learned in this example translates to the following BNF notation:

```
'S' := 'A', 'B'.
'S' := 'S_1', 'B'.
'S_1' := 'A', 'S'.
```

The a^nb^n definition learned by Thelma is a general definition that covers all
correct strings and no incorrect strings, for arbitrary n. What's more it is
both a recogniser and a generator, two-in-one.

#### Test correctness

To test the grammar correctly recognises a^nb^n strings up to n = 100,000, save
the learned hypothesis in a file, consult it, then run this query:

```
?- _N = 100_000, findall(a, between(1,_N,_), _As), findall(b, between(1,_N,_),_Bs), append(_As,_Bs,_AsBs), anbn:'S'(_AsBs,[]).
true ;
```

You can try higher numbers if your computer will allow. The grammar is correct
for any n. Note again that it was learned from 3 positive examples.

Compare this result with attempts to learn a^nb^n with deep neural nets. See
[Weiss et al.] where different recurrent neural nets are trained on 100 examples
of a^nb^n strings and fail to generalise to arbitrary n. After n ~256 it starts
to lose the plot.

Does the learned theory recognise strings it shouldn't? Try these little tests:

```
% Not empty.
?- anbn:'S'([],[]).
false.

% Not only a
?- anbn:'S'([a],[]).
false.

% Not only b
?- anbn:'S'([b],[]).
false.

% Not starting with b
?- anbn:'S'([b|_],[]).
false.

% Not more a's than b's.
?- anbn:'S'([a,a,b],[]).
false.

% Not more b's than a's.
?- anbn:'S'([a,b,b],[]).
false.
```

#### Run as a generator

Run the learned theory as a generator by leaving its first variable unbound:

```
?- anbn:'S'(A,[]).
A = [a, b] ;
A = [a, a, b, b] ;
A = [a, a, a, b, b, b] ;
A = [a, a, a, a, b, b, b, b] ;
A = [a, a, a, a, a, b, b, b, b|...] .
```

Motivation
==========

Thelma is an implementation of Meta-Interpretive Learning, as defined in
[(Muggleton et al. 2014)]. It is a simpler, no-frills version of [Metagol], the
original implementation of MIL in Prolog described in that paper. It is
primarily meant to help instruct beginners interested in MIL. Where necessary,
concessions were made to readability over efficiency in the implementation.
Thelma only supports directly the most basic features of MIL identified in
[(Muggleton et al. 2014)] : dyadic datalog hypotheses; predicate invention;
learning of recursive theories controlled by ordering constraints; and not much
more.

Note that the current version of Thelma is still a very early one and it's
therefore likely to break in many interesting ways. Please report errors to the
owner of this repository.

For further reading, see the reference section. For usage instructions consult
the online documentation initiated when the load.pl file is consulted into
Swi-Prolog.

Bibliography and references
===========================

1. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad: Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited, [Machine Learning 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

2. [Metagol](https://github.com/metagol/metagol "Metagol")

[(Muggleton et al. 2014)]: https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf "Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited"
[Metagol]: https://github.com/metagol/metagol "Metagol"
