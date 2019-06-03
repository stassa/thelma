Thelma - a Theory Learning Machine for Meta-Interpretive Learning
=================================================================

Thelma is an Inductive Logic Programming system. In particular, it is an
implementation of Meta-Interpretive Learning [(Muggleton et al. 2014)], similar
to [Metagol]. It learns first order logic theories in the form of dyadic datalog
definite programs. It is trained on examples given as ground unit clauses and
with background knowledge given as arbitrary Prolog programs. 

As a MIL implementation, Thelma can perform predicate invention and can learn
recursive theories, including mutually recursive theories. Thelma is still new
and feature-light, for the time being.

The plan is to keep Thelma's interface and source code as straightforward and
user-friendly as possible, sacrificing efficiency where necessary. This would
make it more appropriate as an introductory system, to help teach interested
beginners the basic concepts of MIL. This is in contrast with Metagol that is
optimised for efficiency and speed, and should be preferred for applications
that require efficiency.

Planned additional features include: an "experiment harness", comprising
libraries for sampling, training, evaluation and logging; a MIL dataset
generator; implementations of Plotkin's clause and program reduction algorithms;
and a library for bagging aggregation for learning in noisy domains.

Example of use
--------------

Thelma runs on Swi-Prolog version 8.0.0 or later.

See the data/ directory for examples.

Follow the steps below to run an example that learns the a^nb^n CFG from three
positive examples.

### Edit the configuration file:

Open experiment.pl in your favourite text editor and set the name of the
experiment file to a^nb^n.pl:

```prolog
experiment_file('data/anbn.pl',anbn). 
```

Also set an appropriate depth limit:

```prolog
depth_limits(3,1).
```

### Load the project:

```prolog
?- [load].
```

This opens source files in the Swi IDE and starts the documentation browser
loading this README file.

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

### Understanding the results

The hypothesis learned in this example translates to the following BNF notation:

```bnf
<S> ::= <A> <B>
<S> ::= <S_1> <B>
<S_1> ::= <A> <S>
```

The a^nb^n definition learned by Thelma is a general definition that covers all
correct strings and no incorrect strings, for arbitrary n. What's more, as a
Prolog program it can be run both as a recogniser and a generator.

#### Test correctness

To test the grammar correctly recognises a^nb^n strings up to n = 100,000, save
the learned hypothesis in a file, consult it, then run this query:

```prolog
?- _N = 100_000, findall(a, between(1,_N,_), _As), findall(b, between(1,_N,_),_Bs), append(_As,_Bs,_AsBs), anbn:'S'(_AsBs,[]).
true .
```

You can try higher numbers if your computer allows it. The grammar is correct
for any n.

Does the learned theory recognise strings it shouldn't? Try these tests:

```prolog
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

```prolog
?- anbn:'S'(A,[]).
A = [a, b] ;
A = [a, a, b, b] ;
A = [a, a, a, b, b, b] ;
A = [a, a, a, a, b, b, b, b] ;
A = [a, a, a, a, a, b, b, b, b|...] .
```

For further reading, see the reference section. For usage instructions consult
the online documentation initiated when the load.pl file is consulted into
Swi-Prolog.

Bibliography and references
===========================

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. Metagol System [Andrew Cropper and Stephen Muggleton, 2016](https://github.com/metagol/metagol "Metagol")

[(Muggleton et al. 2014)]: https://link.springer.com/article/10.1007/s10994-013-5358-3 "Meta-interpretive learning: application to grammatical inference"
[(Muggleton et al. 2015)]: https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf "Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited"
[Metagol]: https://github.com/metagol/metagol "Metagol"
