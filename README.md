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
a library for bagging aggregation for learning in noisy domains; etc assorted
libraries.

Example of use
--------------

Thelma runs on Swi-Prolog version 8.0.0 or later.

See the `data/` directory for examples.

Follow the steps below to run an example that learns the a^nb^n Context Free
Grammar from three positive examples.

### A first experiment: a^nb^n

1. Consult the project's load file to load the project:
   
   ```prolog
   ?- [load].
   ```

2. Edit the configuration file:

   Open `configuration.pl` in your favourite text editor and set the name of the
   experiment file to `anbn.pl`:
   
   ```prolog
   experiment_file('data/anbn.pl',anbn).
   ```
   
   Set appropriate upper limits for total clauses and invented clauses:
   
   ```prolog
   depth_limits(3,1).
   ```

3. Recompile the project

   ```prolog
   ?- make.
   ```

4. Run a query:

   Once `anbn.pl` is loaded as described above, you can run the following query
   to train Thelma:
   
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

   The learned hypothesis is printed to the Swi-Prolgo console by
   `print_clauses/1`.
    
   In the learned hypothesis the predicate `S_1/2` is _invented_. Although it is
   not given in the background knowledge, it is necessary to complete the
   learning process. It is reconstructed from existing background knowledge and
   metarules, during learning. Note also that `S_1/2` is mutually recursive with
   `S/2`.


### Understanding the training results

The hypothesis learned in the anbn experiment, above translates to the following
BNF notation:

```bnf
<S> ::= <A> <B>
<S> ::= <S_1> <B>
<S_1> ::= <A> <S>
```

This is a general definition of the a^nb^n grammar that covers all strings in
the language and no strings outside the language, for arbitrary n. What's more,
as a Prolog program it can be run both as a recogniser and a generator.

#### Run as a generator

At the end of learning the learned grammar is only printed at the Swi-Prolog
top-level. In order to use it you first have to save it in a file and then
consult the file, to load its clauses into memory.

You can always copy/paste the learned hypothesis into `anbn.pl` itself. Remember
to save and reload it afterwards.

Once this is done, run the learned theory as a generator by leaving its first
variable unbound:

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

### The a^nb^n experiment file.

The contents of the experiment file used in the first experiment above,
`anbn.pl`, are as follows (excluding comments):

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

#### Metarules and order constraints

The first two lines declare a new metarule called _unchain_ and its order
constraints. _unchain_ is a version of the commonly used _chain_ metarule,
defined in `configuration.pl`, having one less interval order constraint.

Metarules and order constraints are central to Meta-Interpretive Learning. The
following is a high-level discussion in the context of their implementation in
Thelma. For an in-depth discussion see the links in the reference section.

##### Metarules

Metarules are second-order definite Datalog clauses used as "templates" for
clauses added to a hypothesis. In the metarule/4 clause in the `anbn.pl`
experiment file, above, `mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y)` is an
_encapsulation_ of the metarule `P(X,Y):- Q(X,Z), R(Z,Y)` in a first order term.
This encapsulation is necessary to allow Prolog to process the metarule (Prolog
is a first-order language).

In the `metarule/4` clause above, `[P,Q,R]` is the set of second order,
existentially qualified variables and `[X,Y,Z]` the set of first order,
existentially quantified variables, in _unchain_. 

The existentially quantified variables in a metarule range over the set of
predicate symbols comprising the predicate symbol of the target predicate, the
predicate symbols in the background knowledge and any invented symbols. In the
anbn experiment, the predicate symbol of the target predicate is `'S'`, the
predicate symbols of the background knowledge are `'A'` and `'B'` and `'S_1'` is
the predicate symbol of an invented predicate.

The universally quantified variables in a metarule range over the constants in
the examples of the target predicate and the bakcground knowledge (the Herbrand
universe). In the anbn experiment, the Herbrand universe is the set [a,b].

During the search for a hypothesis, the variables in a metarule are bound to
predicate symbols and a _metasubstitution_ is created. For example, a
metasubstitution for the _unchain_ metarule in the anbn experiment might be:

```
{P/'S',Q/'A',R/'B'}
```

Where a term `V/T` means that the variable V is bound to the term T.

During learning, the body literals of a metarule instantiated by the
metasubstitution are proved. If the succeeds, the metasubstitution is added
to an abduction store. At the end of learning the metasubstitutions in the
abduction store are projected onto their corresponding metarules to form the
clause of the hypothesis.

For instance, the example metasubstitution above would be be projected onto the
_unchain_ metarule to add the following clause to the learned hypothesis:

```
'S'(A,B):-'A'(A,C),'B'(C,B).
```

##### Order constraints

Order constraints guarantee termination and so allow the learning of recursive
theories. In the `order_constraints/5` clause for _unchain_, in the `anbn.pl`
experiment file, above, there are two sets of constraints: `[P>Q]` and
`[X>Z,Z>Y]`. `[P>Q]` is a _lexicographic order_ constraint and `[X>Z,Z>Y]` an
_interval inclusion_ constraint.

Ordering constraints require a total ordering of the predicate symbols and a
total ordering of the constants in the Herbrand base of predicates defined in
the background knowledge. In Thelma, these orders are derived automatically from
the order in which background predicates, their clauses and their terms, are
encountered in an experiment file during compilation. Predicate symbols in the
background knowledge are assigned a _lexicographic_ ordering while their
constants are assigned an _interval inclusion_ order. See the structured
documentation of the predicate `order_constraints/3` in module
`src/auxiliaries.pl` for an explanation of how this is done.

During the search for a hypothesis, termination is guaranteed because atoms with
a higher order are proved by the meta-interpreter before atoms with a lower
order. The two orders are finite and so cannot descend infinitely. See [Knuth
and Bendix, 1970] for the original description and [Zhang et al. 2005] for a
proof.

In _unchain_, the lexicographic constraint `[P>Q]` means that any predicate
symbol bound to the variable `P` in _unchain_ must be above any predicate symbol
bound to the variable 'Q', with respect to the lexicographic ordering over the
Herbrand base.

Accordingly, the intervalinclusion constraint `[X>Z,Z>Y]` means that any
constant bound to the variable `X` in _unchain_ must be above any constant bound
to the variable `X` and so on for `Z>Y`, with respect to the interval inclusion
order over the Herbrand base.

### Background knowledge and examples

ILP algorithms are characterised by their use of background knowledge to
constraint the search for hypotheses.

The third line of code in `anbn.pl` declares the background knowledge for the
target predicate 'S'/2. 'S'/2 is the start symbol of the grammar. 'A'/2 and
'B'/2, the terminals in the a^nb^n language, are given as background knowledge
in the experiment file.

Conceptually, the function of the background knowledge is to constraint the
search to hypotheses that satisfy the cirterion that the background knowledge
definitions and the hypothesis must entail all positive examples, and none of
the negative examples.

Operationally, atoms of background knowledge predicates become literals in the
clauses of the learned hypothesis.

The next lines in `anbn.pl`, after the background knowledge declarations,
generate positive and negative examples for 'S'/2. The positive examples are
three strings in the a^nb^n language. These suffice for Thelma to learn a
grammar for the entire language. The negative examples generator generates the
empty list- no negative examples are needed.

The last two lines are the definitions of the two predicates defined as
background knowledge, 'A'/2 and 'B'/2.

### Further reading

A complete manual for Thelma with full usage instructions and more examples will
be added at some point. Until then, consult the online documentation initiated
when the `load.pl` file is loaded into Swi-Prolog.

For further reading on Meta-Interpretive Learning, see the reference section.

Bibliography and references
===========================

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. Metagol System [Andrew Cropper and Stephen Muggleton, 2016](https://github.com/metagol/metagol "Metagol")

4. Knuth, D., & Bendix, P. (1970). Simple word problems in universal algebras. [In J. Leech (Ed.), Computational problems in abstract algebra (pp. 263–297). Oxford: Pergamon.](https://link.springer.com/chapter/10.1007/978-3-642-81955-1_23)

5. Zhang, T., Sipma, H., & Manna, Z. (2005). The decidability of the first-order theory of Knuth–Bendix order. [In Automated deduction-CADE-20 (pp. 738–738). Berlin: Springer.](https://link.springer.com/chapter/10.1007/11532231_10)

[(Muggleton et al. 2014)]: https://link.springer.com/article/10.1007/s10994-013-5358-3 "Meta-interpretive learning: application to grammatical inference"
[(Muggleton et al. 2015)]: https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf "Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited"
[Metagol]: https://github.com/metagol/metagol "Metagol"
[Context Free Grammars]:http://cs.union.edu/~striegnk/courses/nlp-with-prolog/html/node37.html#l6a.sec.cfgs
[Knuth and Bendix, 1970]: https://link.springer.com/chapter/10.1007/978-3-642-81955-1_23
[Zhang et al. 2005]: https://link.springer.com/chapter/10.1007/11532231_10
