:-module(evaluation, [train_and_test/5
		     ,train_and_test/6
		     ,print_evaluation/3
		     ,print_evaluation/7
		     ,list_results/3
		     ,list_results/6
		     ,print_metrics/2
		     ,print_metrics/5
		     ,false_positives/3
		     ,false_negatives/3
		     ,true_positives/3
		     ,true_negatives/3
		     ]).

:-use_module(configuration).
:-use_module(src(auxiliaries)).
:-learner(L)
  ,(   L = thelma
   ->  use_module(lib(tp/tp))
   ;   true
   ).

/** <module> Evaluation metrics for experiment results.
*/


%!	train_and_test(+Target,+Sample,-Program,+Metric,-Value) is det.
%
%	Learn a Program and evaluate it by the requested Metric.
%
%	This version obtains the MIL problem (examples, BK and
%	metarules) from the current experiment file.
%
%	Sample is the size of the training partition for each of the
%	positive and negative examples, as a float between 0 and 1.0 or
%	as an integer. The testing partition is the complement of Sample
%	with respect to the entire set of positive or negative examples.
%
%	Metric is one of: [acc, err, fpr, fnr, tpr, tnr, pre, fsc],
%	corresponding to the metrics calculated by evaluation/6.
%
%	Value is the value of the corresponding metric, computed by
%	evaluating Program on the testing partition selected according
%	to Sample.
%
train_and_test(T,S,Ps,M,V):-
	experiment_data(T,Pos,Neg,BK,MS)
	,train_and_test(T,S,[Pos,Neg,BK,MS],Ps,M,V).



%!	train_and_test(+Target,+Sample,+Pos,+Neg,+BK,+MS,-Prog,+Metric,-Val)
%!	is det.
%
%	Learn a hypothesis and print out evaluation results.
%
%	Sample is the size of the training partition for each of the
%	positive and negative examples, as a float between 0 and 1.0 or
%	as an integer. The testing partition is the complement of Sample
%	with respect to the entire set of positive or negative examples.
%
%	Pos, Neg, BK and MS are the example atoms, background knowledge
%	symbols and arities and metarule names of a MIL problem.
%
%	Prog is the program learned from the given MIL problem.
%
%	Metric is one of: [acc, err, fpr, fnr, tpr, tnr, pre, fsc],
%	corresponding to the metrics calculated by evaluation/6.
%
%	Val is the value of the corresponding metric, computed by
%	evaluating Prog on the testing partition selected according to
%	Sample.
%
train_and_test(T,S,[Pos,Neg,BK,MS],Ps,M,V):-
	train_test_splits(S,Pos,Pos_Train,Pos_Test)
	,train_test_splits(S,Neg,Neg_Train,Neg_Test)
	,learn(Pos_Train,Neg_Train,BK,MS,Ps)
	,program_results(T,Ps,BK,Rs)
	,evaluation(Rs,Pos_Test,Neg_Test,_Ts,_Bs,Cs)
	,metric(M,Cs,V).



%!	print_evaluation(+Target,+Sample,+Program) is det.
%
%	Print evaluation metrics of a learned Program.
%
%	Program is evaluated in the context of a MIL problem (examples
%	and background knowledge) obtained from the current experiment
%	file.
%
print_evaluation(T,S,Ps):-
	experiment_data(T,Pos,Neg,BK,MS)
	,print_evaluation(T,S,Pos,Neg,BK,MS,Ps).



%!	print_evaluation(+Target,+Sample,+Pos,+Neg,+BK,+Metarules,+Program)
%!	is det.
%
%	Print evaluation metrics of a learned Program.
%
print_evaluation(T,S,Pos,Neg,BK,MS,Ps):-
	train_test_splits(S,Pos,Pos_Train,Pos_Test)
	,train_test_splits(S,Neg,Neg_Train,Neg_Test)
	,learn(Pos_Train,Neg_Train,BK,MS,Ps)
	,print_evaluation(T,Ps,Pos_Test,Neg_Test,BK).


%!	print_evaluation(+Target,+Program,+Pos,+Neg,+BK) is det.
%
%	Print evaluation metrics of a learned Program.
%
print_evaluation(T,Ps,Pos,Neg,BK):-
	program_results(T,Ps,BK,Rs)
	,print_clauses(Ps)
	,clause_count(Ps,N,D,U)
	,nl
	,format('Hypothesis size:  ~w~n',[N])
	,format('Definite clauses: ~w~n',[D])
	,format('Unit clauses:	  ~w~n',[U])
	,nl
	,print_confusion_matrix(Rs,Pos,Neg).


%!	train_test_splits(+Size,+Examples,-Training,-Testing) is det.
%
%	Split a set of Examples to Training and Testing partitions.
%
%	Raises error if Size is equal to 1.0 or 0.0.
%
train_test_splits(P,_Es,_Train,_Test):-
	P >= 1.0
	,throw('The size of the testing partition must be more than 0!').
train_test_splits(P,_Es,_Train,_Test):-
	P =< 0.0
	,throw('The size of the training partition must be more than 0!').
train_test_splits(_P,[],[],[]):-
% An example set may be empty- but it better be the negative examples!
	!.
train_test_splits(P,Es,Train,Test):-
	float(P)
	,!
	,p_list_partitions(P,Es,Train,Test).
train_test_splits(K,Es,Train,Test):-
	integer(K)
	,p_list_partitions(K,Es,Train,Test).


%!	metric(?Metric,?Metrics,?Value) is det.
%
%	Obtain a Value for the given Metric in a list of Metrics.
%
%	Metrics is the last argument of evaluation/6. Metric is one of:
%	[acc, err, fpr, fnr, tpr, tnr, pre, fsc]. Value is the value in
%	Metrics corresponding to Metric.
%
metric(acc,[ACC,_ERR,_FPR,_FNR,_TPR,_TNR,_PRE,_FSC],ACC).
metric(err,[_ACC,ERR,_FPR,_FNR,_TPR,_TNR,_PRE,_FSC],ERR).
metric(fpr,[_ACC,_ERR,FPR,_FNR,_TPR,_TNR,_PRE,_FSC],FPR).
metric(fnr,[_ACC,_ERR,_FPR,FNR,_TPR,_TNR,_PRE,_FSC],FNR).
metric(tpr,[_ACC,_ERR,_FPR,_FNR,TPR,_TNR,_PRE,_FSC],TPR).
metric(tnr,[_ACC,_ERR,_FPR,_FNR,_TPR,TNR,_PRE,_FSC],TNR).
metric(pre,[_ACC,_ERR,_FPR,_FNR,_TPR,_TNR,PRE,_FSC],PRE).
metric(fsc,[_ACC,_ERR,_FPR,_FNR,_TPR,_TNR,_PRE,FSC],FSC).



%!	convert_examples(+Pos,+Neg,-Converted_Pos,-Converted_Neg) is
%!	det.
%
%	Convert examples from a learner's internal representation.
%
%	This predicate handles the necessary transformations from the
%	internal representation used in the two MIL learners, Thelma and
%	Louise, to unit clauses.
%
%	Examples are represented internally in Louise as definite
%	clauses: unit clauses for positive examples, and goals, :-A, for
%	negative examples. In Thelma both positive and negative examples
%	are represented as lists [P,A1,...,An] of the predicate symbol
%	and arguments of a unit clause. The predicates in this module
%	expect both positive and negative examples to be given as unit
%	clauses (with no negative literals) so some transformation is
%	required.
%
%	@tbd This library is shared between Thelma and Louise, hence the
%	use of learner/1 to identify the learning system and select the
%	clause that performs the appropriate transformation.
%
%	@tbd It turns out the transformation is only necessary for
%	Louise, currently.
%
convert_examples(Pos,Neg,Pos,Neg_):-
	configuration:learner(louise)
	,!
	,G = findall(E,member(:-E,Neg),Neg_)
	,call(G).
convert_examples(Pos,Neg,Pos,Neg):-
	configuration:learner(thelma).


%!	list_results(+Target,+Program,+Results) is det.
%
%	List True/False Positives/Negative atoms of Program.
%
%	Results is either a list of one or more of the constants
%	[pp,nn,np,pn], or the constant "all". Their meaning is as
%	follows:
%	* pp: Print true positive atoms
%	* nn: Print true negative atoms
%	* np: Print false positive atoms
%	* pn: Print false negative atoms
%	* all: Print all atoms.
%
list_results(T,Ps,Rs):-
	experiment_data(T,Pos,Neg,BK,_MS)
	,list_results(T,Ps,Pos,Neg,BK,Rs).

%!	list_results(+Target,+Program,+Pos,+Neg,+BK,+Results) is det.
%
%	Business end of list_results/3.
%
list_results(T,Ps,Pos,Neg,BK,Rs):-
	convert_examples(Pos,Neg,Pos_c,Neg_c)
	,program_results(T,Ps,BK,As)
	,maplist(sort,[As,Pos_c,Neg_c],[As_,Pos_,Neg_])
	,false_positives(As_,Neg_,NP)
	,length(NP,NP_n)
	,false_negatives(As_,Pos_,PN)
	,length(PN,PN_n)
	,true_positives(As_,Pos_,PP)
	,length(PP,PP_n)
	,true_negatives(As_,Neg_,NN)
	,length(NN,NN_n)
	,(   Rs == all
	 ->  Rs_ = [pp,nn,np,pn]
	 ;   Rs_ = Rs
	 )
	% Yeah, I know.
	,(   memberchk(pp,Rs_)
	 ->  format('\nTrue positives: ~w~n',[PP_n])
	    ,print_clauses(PP)
	 ;   true
	 )
	,(   memberchk(nn,Rs_)
	 ->  format('\nTrue negatives: ~w~n',[NN_n])
	    ,print_clauses(NN)
	 ;   true
	 )
	,(   memberchk(np,Rs_)
	 ->  format('\nFalse positives: ~w~n',[NP_n])
	    ,print_clauses(NP)
	 ;   true
	 )
	,(   memberchk(pn,Rs_)
	 ->  format('\nFalse negatives: ~w~n',[PN_n])
	    ,print_clauses(PN)
	 ;   true
	 ).


%!	program_results(+Target,+Program,+BK,-Results) is det.
%
%	Collect Results of a learned Program.
%
%	Program is a learned hypothesis. Results is a list of atoms that
%	are immediate consequences of the Program with respect to the
%	background knowledge, BK.
%
program_results(T,Ps,BK,Rs):-
	ground_background(T,BK,BK_)
	,lfp_query(Ps,BK_,_Is,Rs).


%!	ground_background(+Target,+BK,-Ground) is det.
%
%	Collect ground BK atoms.
%
%	Also remove from the BK atoms of the learning Target. That's to
%	allow lfp/e to succeed if the learning Target is also a
%	predicate in the BK (more precisely, if it is a determinant of
%	another BK predicate).
%
ground_background(F/A,BK,BK_):-
	closure(BK,user,Cs)
	,flatten(Cs, Ps)
	,lfp(Ps,As)
	,findall(At
		,(member(At,As)
		 ,\+ functor(At,F,A)
		 )
		,BK_).



%!	clause_count(+Hypothesis,-Size,-Definite,-Unit) is det.
%
%	Count the clauses in a hypothesis.
%
%	Size is the number of all clauses in Hypothesis. Definite is the
%	number of Definite clauses in Hypothesis and Unit the number of
%	unit clauses in Hypothesis.
%
clause_count(Ps,N,D,U):-
	findall(H:-B
	       ,member(H:-B,Ps)
	       ,Ds)
	,length(Ps,N)
	,length(Ds,D)
	,U is N - D.


%!	print_confusion_matrix(+Results,+Pos,+Neg) is det.
%
%	Print a confusion matrix for a set of learning Results.
%
print_confusion_matrix(Rs,Pos,Neg):-
	evaluation(Rs,Pos,Neg
		  ,[P,N],[PP,NN,NP,PN],[ACC,ERR,FPR,FNR,TPR,_TNR,PRE,FSC])
	,PPNP is PP + NP
	,PNNN is PN + NN
	,S is P + N
	,format_confusion_matrix([PP,PN,NP,NN]
				,[P,N,PPNP,PNNN,S]
				,[ACC,ERR,FPR,FNR,PRE,TPR,FSC]
				).


%!	format_confusion_matrix(+Counts,+Totals,+Metrics) is det.
%
%	Bussiness end of print_confusion_matrix/2.
%
%	Actually prints a confusion matrix for a result.
%
%	Counts is a list of numbers [PP,PN,NP,NN,T], where:
%	* PP: positivie instances predicted as true
%	* PN: positive instances predicted as negative
%	* NP: negative instances predicted as positive
%	* NN: negative instances predicted as negative
%	* T:  total positive and negatives predicted
%
%	Totals is a list of numbers [TP,TN,PPNP,PNNN,T], where:
%	* TP: the total number of positive instances
%	* TN: the total number of negative instances
%	* PPNP: the sum of PP + NP
%	* PNNN: the sum of PN + NN
%
%	Metrics is a list of numbers [Acr,Err,FPR,FNR,PRE,REC,FSC],
%	where:
%	* Acr: Accuracy, calculated as  PP + NN / T
%	* Err: Error, calculated as NP + PN / T
%	* FPR: False Positive Rate, NP / TN
%	* FNR: False Negative Rate, PN / TP
%	* PRE: Precision, calculated as PP / PPNP
%	* REC: Recall, calculated as PP / TP (i.e. TPR)
%	* FSC: F-Score, PRE * REC / PRE + REC
%
%	Given the above lists of numbers, format_confusion_matrix/3 will
%	print approximately the following table (with some differences
%	in formatting):
%
%			Predicted +	Predicted -	Total
%	Actual +	PP		PN		TP
%	Actual -	NP		NN		TN
%	-----------------------------------------------------
%	Total		PPNP		PNNN		T
%
%	Accuracy:		PP + NN / T
%	Error:			NP + PN / T
%	False Positive Rate:	NP / TN
%	False Negative Rate:	PN / TP
%	Precision:		PP / PPNP
%	Recall(TPR):		PP / TP
%	F-Score:                Precision * Recall / Precision + Recall
%
format_confusion_matrix([PP,PN,NP,NN]
		       ,[TP,TN,PPNP,PNNN,T]
		       ,[ACC,ERR,FPR,FNR,PRE,REC,FSC]):-
	configuration:decimal_places(D)
	,atom_chars('Actual + Predicted + Predicted - Total', Hs)
	% Length of an entire header line
	,length(Hs, L1)
	,atom_chars('Actual + ', Act)
	% Length of second line's first column
	,length(Act, L21)
	,atom_chars('Predicted + ', Pred_p)
	,length(Pred_p, L22)
	,atom_chars('Predicted - ', Pred_n)
	,length(Pred_n, L23)
	% Printing header line
	,format('~*+~w ~*+~w ~*+~w~*+~n'
	       ,[L21,'Predicted +',L22,'Predicted -',L23,'Total',L1])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Actual +',L21,PP,L22,PN,L23,TP])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Actual -',L21,NP,L22,NN,L23,TN])
	,format('-------------------------------------~n',[])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Total',L21,PPNP,L22,PNNN,L23,T])
	% Longest left column
	,atom_chars('False Positive Rate: ',TPR_cs)
	,length(TPR_cs, TPR_cs_L)
	,nl
	,format('Accuracy: ~*+~*f~n', [TPR_cs_L,D,ACC])
	,format('Error: ~*+~*f~n', [TPR_cs_L,D,ERR])
	,format('False Positive Rate: ~*+~*f~n', [TPR_cs_L,D,FPR])
	,format('False Negative Rate: ~*+~*f~n', [TPR_cs_L,D,FNR])
	,format('Precision: ~*+~*f~n', [TPR_cs_L,D,PRE])
	,format('Recall (TPR): ~*+~*f~n', [TPR_cs_L,D,REC])
	,format('F-Score: ~*+~*f~n', [TPR_cs_L,D,FSC]).



%!	print_metrics(+Target,+Program) is det.
%
%	Print a simple listing of evaluation metrics.
%
%	Program is evaluated in the context of a MIL problem (examples
%	and BK) obtained from the current experiment file.
%
print_metrics(T,Ps):-
	experiment_data(T,Pos,Neg,BK,_MS)
	,print_metrics(T,Ps,Pos,Neg,BK).



%!	print_metrics(+Target,+Program,+Pos,+Neg,+BK) is det.
%
%	Print a simple listing of evaluation metrics.
%
print_metrics(T,Ps,Pos,Neg,BK):-
	configuration:decimal_places(P)
	,program_results(T,Ps,BK,Rs)
	,evaluation(Rs,Pos,Neg
		   ,[_P,_N],[_PP,_NN,_NP,_PN],[ACC,ERR,FPR,FNR,TPR,TNR,PRE,FSC])
	,format('ACC: ~*f~n',[P,ACC])
	,format('ERR: ~*f~n',[P,ERR])
	,format('FPR: ~*f~n',[P,FPR])
	,format('FNR: ~*f~n',[P,FNR])
	,format('PRE: ~*f~n',[P,PRE])
	,format('REC: ~*f~n',[P,TPR])
	,format('SPE: ~*f~n',[P,TNR])
	,format('FSC: ~*f~n',[P,FSC]).



%!	evaluation(+Results,+Pos,+Neg,-Totals,+Base,-Calculated)
%!	is det.
%
%	Business end of evaluation/5
%
%	Evaluate a set of Results according to the Positive and Negative
%	examplse of the learning Target.
%
evaluation(Rs,Pos,Neg
	  ,[P,N],[PP_,NN_,NP_,PN_],[ACC,ERR,FPR,FNR,TPR,TNR,PRE,FSC]):-
	convert_examples(Pos,Neg,Pos_c,Neg_c)
	,maplist(sort,[Rs,Pos_c,Neg_c],[Rs_,Pos_,Neg_])
	,maplist(length,[Pos_,Neg_],[P,N])
	,true_positives(Rs_,Pos_,PP)
	,true_negatives(Rs_,Neg_,NN)
	,false_positives(Rs_,Neg_,NP)
	,false_negatives(Rs_,Pos_,PN)
	,maplist(length,[PP,NN,NP,PN],[PP_,NN_,NP_,PN_])
	,acc(PP,NN,Pos_,Neg_,ACC)
	,err(ACC,ERR)
	,tpr(PP,Pos_,TPR)
	,tnr(NN,Neg_,TNR)
	,fnr(PN,Pos_,FNR)
	,fpr(NP,Neg_,FPR)
	,pre(Rs_,PP,PRE)
	,fsc(PRE,TPR,FSC).


%!	acc(+PP,+NN,+Pos,+Neg,-ACC) is det.
%
%	Calculate the Accuracy of a result.
%
acc(PP,NN,Pos,Neg,ACC):-
	total(PP,NN,Ps)
	,total(Pos,Neg,As)
	,ACC is Ps / As.

%!	err(+ACC,-ERR) is det.
%
%	Calculate the Error of a result.
%
err(ACC,Er):-
	Er is 1	- ACC.

%!	tpr(+PP,+Pos,+TPR) is det.
%
%	Calculate the True Positive Rate of a result.
%
tpr(PP,Pos,TPR):-
	ratio(PP,Pos,TPR).

%!	tpr(+NN,+Neg,+TNR) is det.
%
%	Calculate the True Negative Rate of a result.
%
tnr(NN,Neg,TNR):-
	ratio(NN,Neg,TNR).

%!	tpr(+PN,+Pos,+FNR) is det.
%
%	Calculate the False Negative Rate of a result.
%
fnr(PN,Pos,FNR):-
	ratio(PN,Pos,FNR).

%!	tpr(+NP,+NEG,+FPR) is det.
%
%	Calculate the False Positive Rate of a result.
%
fpr(NP,Neg,FPR):-
	ratio(NP,Neg,FPR).

%!	pre(+Results,+PP,+PRE) is det.
%
%	Calculate the Precision metric of a result.
%
pre(Rs,PP,PRE):-
	ratio(PP,Rs,PRE).

%!	fsc(+PRE,+REC,+FSC) is det.
%
%	Calculate the F1-score metric of a result.
%
fsc(PRE,REC,FSC):-
	P is PRE * REC
	,S is PRE + REC
	,safe_division(P,S,D)
	,FSC is 2 * D.


%!	false_positives(+Results,+Negative,-NP) is det.
%
%	False positives in a result.
%
%	Under a closed world assumption an instance, X, is a false
%	positive iff X is in the intersection of the set of Negative
%	examples and the set of positive predicted Results.
%
false_positives(Rs,Neg,NP):-
	ord_intersection(Rs,Neg,NP).


%!	false_negaives(+Results,+Positive,-PN) is det.
%
%	False negatives in a result.
%
%	Under a closed world assumption an instance, X, is a false
%	negative iff X is in the set of Positive examples and X is not
%	in the set of positive predicted Results.
%
false_negatives(Rs,Pos,PN):-
	ord_subtract(Pos,Rs,PN).


%!	true_positives(+Results,+Positive,-PP) is det.
%
%	True positives in a result.
%
%	Under a closed world assumption an instance, X, is a true
%	positive iff X is in the intersection of the set of Positive
%	examples and the set of positive predicated Results.
%
true_positives(Rs,Pos,PP):-
	ord_intersection(Rs,Pos,PP).


%!	true_negatives(+Results,+Negative,-TN) is det.
%
%	True negatives in a result.
%
%	Under a closed world assumption an instance, X, is a true
%	negative iff X is in the set of Negative examples and X is not
%	in the set of positive reported Results.
%
true_negatives(Rs,Neg,NN):-
	ord_subtract(Neg,Rs,NN).


%!	difference(+Xs,+Ys,-Difference) is det.
%
%	Difference of the lengths of two lists.
%
difference(Xs,Ys,D):-
	length(Xs,N)
	,length(Ys,M)
	,D is N	- M.


%!	total(+Xs,+Ys,-Sum) is det.
%
%	Sum of the lengths of two lists.
%
total(Xs,Ys,S):-
	length(Xs,N)
	,length(Ys,M)
	,S is N + M.


%!	ratio(+Xs,+Ys,-Ratio) is det.
%
%	Ratio of the lengths of two lists.
%
ratio(Xs,Ys,R):-
	length(Xs,N)
	,length(Ys,M)
	,safe_division(N,M,R).


%!	safe_division(+A,+B,-C) is det.
%
%	Avoid dividing by zero.
%
%	If the denominator of a division os 0, return C, else do the
%	division.
%
safe_division(A,0,A):-
	!.
safe_division(A,B,C):-
	C is A / B.
