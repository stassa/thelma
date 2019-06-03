Thelma - a Theory Learning Machine for Meta-Interpretive Learning
=================================================================

Thelma is an Inductive Logic Programming system. In particular, it is an
implementation of Meta-Interpretive Learning [(Muggleton et al. 2014)], similar
to [Metagol]. It learns first order logic hypotheses in the form of dyadic
datalog definite programs. It is trained on examples given as ground unit
clauses and with background knowledge given as arbitrary Prolog programs. 

As a MIL implementation, Thelma can perform predicate invention and can learn
recursive hypotheses, including hypotheses with mutually recursive clauses.
Thelma is still new and feature-light, for the time being.

The plan is to keep Thelma's interface and source code as straightforward and
user-friendly as possible, sacrificing efficiency where necessary. That would
make it more appropriate as an introductory system, to help teach interested
beginners the basic concepts of MIL. For applications that require efficiency
and speed, Metagol should be preferred.

Planned additional features include: an "experiment harness", comprising
libraries for sampling, training, evaluation and logging; a MIL dataset
generator; implementations of Plotkin's clause and program reduction algorithms;
and a library for bagging aggregation for learning in noisy domains.

Example of use
--------------

Thelma runs on Swi-Prolog version 8.0.0 or later.

See the `data/` directory for examples.

Follow the steps below to run an example that learns the a^nb^n Context Free
Ggrammar from three positive examples.

### Edit the configuration file:

Open `configuration.pl` in your favourite text editor and set the name of the
experiment file to `anbn.pl`:

```prolog
experiment_file('data/anbn.pl',anbn). 
```

Set appropriate upper limits for total clauses and invented clauses:

```prolog
depth_limits(3,1).
```

### Load the project:

Consult the project's load file to load the project:

```prolog
?- [load].
```

This will load up the necessary source files and the currently configured
experiment file, which should now be `anbn.pl` if you followed the instructions
above.

It will also bring up the loaded source files and the experiment file in the
Swi-Prolog IDE and start the documentation browser loading this README file and
documentation for the loaded source files.

### The a^nb^n experiment file.

The contents of `anbn.pl` are as follows:

```prolog
configuration:metarule(unchain, [P,Q,R], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).
configuration:order_constraints(unchain,[P,Q,_R],[X,Y,Z],[P>Q],[X>Z,Z>Y]).

background_knowledge('S'/2,['A'/2,'B'/2]).

metarules('S'/2,[unchain]).

positive_example('S'/2,E):-
	member(E, ['S'([a,b],[])
		  ,'S'([a,a,b,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ]).

negative_example('S'/2,_):-
	fail.

'A'([a|A], A).
'B'([b|A], A).
```

The first two lines declare a new metarule called _unchain_ and its ordering
constraints. _unchain_ is a version of the commonly used _chain_ metarule,
defined in `configuration.pl`, having one less interval ordering constraint.

The third line of code declares the background knowledge for the target
predicate 'S'/2. 'S'/2 is the start symbol of the grammar. The next lines
generate positive and negative examples for 'S'/2. The positive examples are
three strings in the a^nb^n language. These suffice to learn a grammar for the
entire language. The negative examples generator generates the empty list- no
negative examples are needed.

The last two lines are the definitions of the two predicates defined as
background knowledge, 'A'/2 and 'B'/2. These are the terminals in the a^nb^n
language.

### Run a query:

Once `anbn.pl` is loaded, you can run the following query to train Thelma:

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

The predicate `S_1/2` is _invented_. Although it is not given in the background
knowledge, it is necessary to complete the learning process. It is reconstructed
from existing background knowledge and metarules, during learning. Note also
that `S_1/2` is mutually recursive with `S/2`.

### Understanding the training results

The hypothesis learned in this example translates to the following BNF notation:

```bnf
<S> ::= <A> <B>
<S> ::= <S_1> <B>
<S_1> ::= <A> <S>
```

This is a general definition of the a^nb^n grammar that covers all strings in
the language and no strings outside the language, for arbitrary n. What's more,
as a Prolog program it can be run both as a recogniser and a generator.

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

#### Test correctness

To test the grammar correctly recognises a^nb^n strings up to n = 100,000, save
the learned hypothesis in a file, consult it, then run this query:

```prolog
?- _N = 100_000, findall(a, between(1,_N,_), _As), findall(b, between(1,_N,_),_Bs), append(_As,_Bs,_AsBs), 'S'(_AsBs,[]).
true .
```

You can try higher numbers up to the limits of your computational resources. The
grammar is correct for any n. 

Does the learned theory recognise strings it shouldn't? Try these tests:

```prolog
% Not empty.
?- 'S'([],[]).
false.

% Not only a
?- 'S'([a],[]).
false.

% Not only b
?- 'S'([b],[]).
false.

% Not starting with b
?- 'S'([b|_],[]).
false.

% Not more a's than b's.
?- 'S'([a,a,b],[]).
false.

% Not more b's than a's.
?- 'S'([a,b,b],[]).
false.
```

See [Context Free Grammars] for an explanation of how a definition of a^nb^n
like the one learned by Thelma works.

For further reading on Meta-Interpretive Learning, see the reference section.
For usage instructions consult the online documentation initiated when the
`load.pl` file is consulted into Swi-Prolog.

Bibliography and references
===========================

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. Metagol System [Andrew Cropper and Stephen Muggleton, 2016](https://github.com/metagol/metagol "Metagol")

[(Muggleton et al. 2014)]: https://link.springer.com/article/10.1007/s10994-013-5358-3 "Meta-interpretive learning: application to grammatical inference"
[(Muggleton et al. 2015)]: https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf "Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited"
[Metagol]: https://github.com/metagol/metagol "Metagol"
[Context Free Grammars]:http://cs.union.edu/~striegnk/courses/nlp-with-prolog/html/node37.html#l6a.sec.cfgs
