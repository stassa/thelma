Thelma - a Theory Learning Machine for Meta-Interpretive Learning
=================================================================

Example of use
--------------

Thelma runs on Swi-Prolog version 8.0.0 or later.

Follow the steps below to run an example that learns the a^nb^n CFG from three
positive examples.


1. Edit configuration.pl:

Choose the a^nb^n experiment file:

```
experiment_file('data/anbn.pl',anbn). 
```

Set an appropriate depth limit:

```
depth\_limits(3,1).
```

2. Load the project:

```
?- [load].
```

This opens source files in the Swi IDE and starts the documentation browser. You
should see this README file in your browser.

3. Initialise the experiment:

```
?- initialise_experiment.
true.
```

4. Run a query:

```
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

The learned hypothesis translates to the following DCG notation:

```
'S' --> 'A', 'B'.
'S' --> 'S_1', 'B'.
'S_1' --> 'A', 'S'.
```

5. Remember to cleanup afterwards to avoid strange errors later on:

```
?- cleanup_experiment.
true.
```

Motivation
==========

Thelma is an implementation of Meta-Interpretive Learning, as defined in
['Muggleton et al. 2014']. It is a simpler version of [Metagol], the original
implementation of MIL in Prolog that was originally described in that paper.

[Metagol] has gone through many development iterations since its description in
['Muggleton et al. 2014']. Many of the features described in that paper have
been replaced or extended. For example [Metagol] no longer uses ordering
constraints to control recursion. In addition, its source code is highly
optimised for efficient execution and perhaps not as easy to read any more. It
would be nice to have a system that stays closer to the original description of
MIL in ['Muggleton et al. 2014'] and has a more straightforward implementation,
to make it easier to introduce MIL to interested beginners.

Thelma was created as just such a system. It sticks close to the description of
Metagol and MIL in ['Muggleton et al. 2014'] and where a choice had to be made
between efficiency and readability of the source code, efficiency was
sacrificed. Thelma only supports directly the most basic features of MIL
identified in ['Muggleton et al. 2014']: dyadic datalog hypotheses; predicate
invention; learning of recursive theories controlled by ordering constraints;
and not much more.

For further reading, see the reference section. For usage instructions consult
the online documentation initiated when the load.pl file is consulted into
Swi-Prolog.

Bibliography and references
===========================

['Muggleton et al. 2014' "Meta Interpretive Learning of higher-order dyadicdatalog: predicate invention revisited"]:(https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

[Metagol]:(https://github.com/metagol/metagol)
