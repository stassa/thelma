Thelma - a Theory Learning Machine for Meta-Interpretive Learning
=================================================================

Thelma is an Inductive Logic Programming system. In particular, it is an
implementation of Meta-Interpretive Learning [(Muggleton et al. 2014)], similar
to [Metagol]. It learns first order logic theories in the form of dyadic datalog
definite programs. It is trained on examples given as ground unit clauses and
with background knowledge given as arbitrary Prolog programs. 

As a MIL implementation, Thelma can perform predicate invention and can learn
recursive hypotheses, including hypotheses with mutually recursive clauses.

Example of use
--------------

Thelma runs on Swi-Prolog version 8.0.0 or later.

See the `data/examples/` directory for examples.

Follow the steps below to run an example that learns the a^nb^n Context Free
Grammar from three positive examples.

### A first experiment: learning a grammar for the a^nb^n context-free language

1. Consult the project's load file to load the project:
   
   ```prolog
   ?- [load].
   ```

   This should start the Swi-Prolog IDE and open the project source files in the
   editor, including `configuration.pl` used in the next step. It should also start
   the documentation server and open this README file in your browser.

2. Edit the configuration file:

   Edit `configuration.pl` in the Swi-Prolog editor (or your favourite text
   editor) and make sure the name of the current experiment file is set to
   `anbn.pl`:
   
   ```prolog
   experiment_file('data/examples/anbn.pl',anbn).
   ```
   
   Also set appropriate upper limits for total clauses and invented clauses in
   the learned hypothesis:
   
   ```prolog
   depth_limits(3,1).
   ```

3. Recompile the project:

   ```prolog
   ?- make.
   ```

4. Run the following query to train Thelma on the data in `anbn.pl` and print
   the results to the console.
   
   ```prolog
   ?- learn('S'/2).
   % Clauses: 1; Invented: 0
   % Clauses: 2; Invented: 0
   % Clauses: 2; Invented: 1
   % Clauses: 3; Invented: 0
   % Clauses: 3; Invented: 1
   'S'(A,B):-'A'(A,C),'B'(C,B).
   'S'(A,B):-'S_1'(A,C),'B'(C,B).
   'S_1'(A,B):-'A'(A,C),'S'(C,B).
   true .
   ```

### Understanding the training results

The hypothesis learned in the a^nb^n experiment, above translates to the
following context-free grammar, in [Definite Clause Grammars] notation (DCG):

```
'S' --> 'A', 'B'.
'S' --> 'S_1', 'B'.
'S_1' --> 'A', 'S'.
```

Or, in BNF notation:

```bnf
<S> ::= <A> <B>
<S> ::= <S_1> <B>
<S_1> ::= <A> <S>
```

This is a general definition of the a^nb^n grammar that covers all strings in
the language and no strings outside the language, for arbitrary n. 

In the learned hypothesis the predicate `'S_1'/2` is _invented_. Although it is
not given in the background knowledge, it is necessary to complete the learning
process. It is reconstructed from existing background knowledge and metarules,
during learning. Note also that `'S_1'/2` is mutually recursive with `'S'/2`.

The grammar learned by Thelma is a Prolog program and so it can be run both as a
recogniser and a generator. See [Context Free Grammars] for an explanation of
how a [DCG] definition of a^nb^n like the one learned by Thelma works.

#### Run as a generator

At the end of training the learned hypothesis is only printed at the Swi-Prolog
top-level. In order to use it you first have to save it in a file and then
consult the file, to load its clauses into memory.

You can always copy/paste the learned hypothesis into `anbn.pl` itself. Remember
to save and reload it afterwards.

Once this is done, you can run the learned hypothesis as a generator by leaving
its first variable unbound. For example, if you saved the learned grammar to
`anbn.pl`, run the following query:

```prolog
?- anbn:'S'(A,[]).
A = [a, b] ;
A = [a, a, b, b] ;
A = [a, a, a, b, b, b] ;
A = [a, a, a, a, b, b, b, b] ;
A = [a, a, a, a, a, b, b, b, b|...] .
```

If you saved the learned grammar in a different file, instead replace the module
qualifier "anbn:" with that file's module name followed by a colon. If the file
is not a module, simply call `'S'(A, [])` without a module qualifier.

#### Test correctness

To test the grammar correctly recognises a^nb^n strings up to n = 100,000, run
this query (assuming you have saved the grammar in the module 'anbn.pl'):

```prolog
?- _N = 100_000, findall(a, between(1,_N,_), _As), findall(b, between(1,_N,_),_Bs), append(_As,_Bs,_AsBs), anbn:'S'(_AsBs,[]).
true .
```

You can try higher numbers up to the limits of your computational resources. The
grammar is correct for any n. 

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
defined in `configuration.pl`, having one less lexicographic order constraint.

Metarules and order constraints are central to Meta-Interpretive Learning. The
following is a high-level discussion in the context of their implementation in
Thelma. For an in-depth discussion see the links in the bibliography section.

##### Metarules

Metarules are second-order definite Datalog clauses used as "templates" for
clauses added to a hypothesis. In the metarule/4 clause in the `anbn.pl`
experiment file, above, `mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y)` is an
_encapsulation_ of the metarule `P(X,Y):- Q(X,Z), R(Z,Y)` in a first order term
("mec" stands for "metarule encapsulation"). This encapsulation is necessary to
allow second-order metarules to be processed by a first-order language like
Prolog.

In the `metarule/4` clause above, `[P,Q,R]` is the set of second order,
existentially qualified variables and `[X,Y,Z]` the set of first order,
universally quantified variables in _unchain_. 

The existentially quantified variables in a metarule range over the set of
predicate symbols including a) the predicate symbol of the target predicate, b)
each of the predicate symbols of predicates in the background knowledge and c)
any symbols invented during learning. In the a^nb^n experiment, the predicate
symbol of the target predicate is `'S'`, the predicate symbols of the background
knowledge are `'A'` and `'B'` and `'S_1'` is the predicate symbol of an invented
predicate.

The universally quantified variables in a metarule range over the constants in
the examples of the target predicate and the bakcground knowledge (the Herbrand
universe). In the a^nb^n experiment, the Herbrand universe is the set [a,b].

During the search for a hypothesis, the variables in a metarule are bound to
predicate symbols and a _metasubstitution_ is created. A metasubstitution is a
substitution of existentially quantified variables for predicate symbols. For
example, a metasubstitution created from the _unchain_ metarule in the a^nb^n
experiment might be:

```
{P/'S',Q/'A',R/'B'}
```

Where a term `V/T` means that the variable V is bound to the term T.

During learning, the body literals of a metarule instantiated by the
metasubstitution are proved. If the proof succeeds, the metasubstitution is
added to an abduction store. At the end of learning the metasubstitutions in the
abduction store are projected onto their corresponding metarules to form the
clause of the hypothesis.

For instance, the example metasubstitution above would be projected onto the
_unchain_ metarule to add the following clause to the learned hypothesis:

```
'S'(A,B):-'A'(A,C),'B'(C,B).
```

##### Order constraints

Order constraints guarantee termination and so allow learning of recursive
theories. In the `order_constraints/5` clause for _unchain_, in the `anbn.pl`
experiment file, above, there are two sets of constraints: `[P>Q]` and
`[X>Z,Z>Y]`. `[P>Q]` is a _lexicographic order_ constraint and `[X>Z,Z>Y]` an
_interval inclusion_ constraint.

Ordering constraints require a total ordering of the predicate symbols and a
total ordering of the constants in the Herbrand base of the predicates defined
in the background knowledge. In Thelma, these orders are derived automatically
from the order in which background predicates, their clauses and their terms,
are encountered in an experiment file during compilation. Predicate symbols in
the background knowledge are assigned a _lexicographic_ order while their
constants are assigned an _interval inclusion_ order. See the structured
documentation of the predicate `order_constraints/3` in module
`src/auxiliaries.pl` for an explanation of how this is done.

During the search for a hypothesis, termination is guaranteed because atoms with
a higher order are proved by the meta-interpreter before atoms with a lower
order. The two orders are finite and so cannot descend infinitely. See [(Knuth
and Bendix, 1970)] for the original description and [(Zhang et al. 2005)] for a
more recent discussion.

In _unchain_, the lexicographic constraint `[P>Q]` means that any predicate
symbol bound to the variable `P` in _unchain_ must be above any predicate symbol
bound to the variable 'Q', with respect to the lexicographic ordering over the
Herbrand base.

Accordingly, the intervalinclusion constraint `[X>Z,Z>Y]` means that any
constant bound to the variable `X` in _unchain_ must be above any constant bound
to the variable `X` and so on for `Z>Y`, with respect to the interval inclusion
order over the Herbrand base.

You can query `order_constraints/3` to inspect the total orders it derives from
an experiment file. For example, for the `anbn.pl` experiment file, the
following orders are derived:

```prolog
?- order_constraints('S'/2,Ps,Cs).
Ps = ['A'/2, 'B'/2],
Cs = [[a|_9572], [b|_9560], []]
```

Above, the two lists, `Ps` and `Cs` represent the lexicographic and interval
orders, respectively. In this representation, a term `p` is above another term
`q` in their respective order, iff `p` is before `q` in an ordering list.

The symbol of the target predicate is always added to the start of the
lexicographic order list (this is done after `order_constraints/2` is called, so
you cannot see it in the query above). Additionally, any predicates during
learning are added to the lexicographic order list right after the target
predicate. The complete lexicographic order list for the `anbn.pl` experiment
file with the invented predicate symbol `S_1/2` would look like the following:

```prolog
['S'/2, 'S_1'/2, 'A'/2, 'B'/2]
```

Given the two orders above and the lexicographic constraint `P>Q` in _unchain_,
the metasubstitution `{P/'S',Q/'A',R/'B'}` upholds the constraint, whereas a
metasubstitution `{P/`B`,Q/'A',R/'S'}` violates the constraint because it binds
P to 'B' and Q to 'A' when `P>Q` and `A/2>B/2`.

Violation of a constraint means that the corresponding metasubstitution is not
allowed and a new metasubstitution is tried. This way, the search for hypotheses
only considers hypotheses with clauses that satisfy the constraints and are
guaranteed to terminate.

#### Background knowledge and examples

ILP algorithms are characterised by their use of background knowledge to
constraint the search for hypotheses.

The third line of code in `anbn.pl` declares the background knowledge for the
target predicate 'S'/2. 'S'/2 is the start symbol of the grammar. 'A'/2 and
'B'/2, the terminals in the a^nb^n language, are given as background knowledge
in the experiment file.

Conceptually, the function of the background knowledge is to constrain the
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

#### The need for inductive bias in machine learning

Background knowledge, metarules and order constraints impose a strong bias on
the hypotheses that can be learned. The need for inductive bias in machine
learning merits a longer discussion that is well beyond the scope of this
introduction to Thelma. Suffice it to say that, without bias, there can be no
learning. For a discussion, see [(Mitchell, 1997)]. There are no machine
learning algorithms that do not encode bias in some form- neural network
architectures, Support Vector Machines' kernels, distance functions, Bayesian
priors- are all instances of such bias encoding. The advantage of MIL and ILP
algorithms in general is that inductive bias has a clear and precise definition
that is easy to inspect and modify.

Future work
-----------

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

5. Machine Learning, Tom Mitchell, [McGraw Hill, 1997.](https://www.cs.cmu.edu/afs/cs.cmu.edu/user/mitchell/ftp/mlbook.html)

6. Zhang, T., Sipma, H., & Manna, Z. (2005). The decidability of the first-order theory of Knuth–Bendix order. [In Automated deduction-CADE-20 (pp. 738–738). Berlin: Springer.](https://link.springer.com/chapter/10.1007/11532231_10)

[(Muggleton et al. 2014)]: https://link.springer.com/article/10.1007/s10994-013-5358-3 "Meta-interpretive learning: application to grammatical inference"
[(Muggleton et al. 2015)]: https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf "Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited"
[Metagol]: https://github.com/metagol/metagol "Metagol"
[Context Free Grammars]:http://cs.union.edu/~striegnk/courses/nlp-with-prolog/html/node37.html#l6a.sec.cfgs
[(Knuth and Bendix, 1970)]: https://link.springer.com/chapter/10.1007/978-3-642-81955-1_23
[(Zhang et al. 2005)]: https://link.springer.com/chapter/10.1007/11532231_10
[(Mitchell, 1997)]: https://www.cs.cmu.edu/afs/cs.cmu.edu/user/mitchell/ftp/mlbook.html
[Definite Clause Grammars]:https://en.wikipedia.org/wiki/Definite_clause_grammar
[DCG]:https://en.wikipedia.org/wiki/Definite_clause_grammar
