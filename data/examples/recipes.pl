:-module(recipes, [background_knowledge/2
		  ,metarules/2
		  ,positive_example/2
		  ,negative_example/2
		  %,recipe/2
		  ,replace/4
		  ,break_eggs/2
		  ,whisk_eggs/2
		  ,heat_oil/2
		  ,fry_eggs/2
		  ,season/2
		  ,replace/4
		  ]).

%recipe([eggs,salt,pepper,olive_oil,egg_whisk,pan],[omelette]).

background_knowledge(recipe/2,[break_eggs/2
			      ,whisk_eggs/2
			      ,heat_oil/2
			      ,fry_eggs/2
			      ,season/2
			      %,replace/4
			      ]).

metarules(recipe/2,[chain]).

positive_example(recipe/2,E):-
	member(E, [recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette])
		  ]).

negative_example(recipe/2,_):-
	fail.

break_eggs(Xs,Ys):-
	replace([eggs],Xs,[egg_whites,egg_yolks],Ys).
whisk_eggs(Xs,Ys):-
	replace([egg_whisk,egg_whites,egg_yolks],Xs,[whisked_eggs],Ys).
heat_oil(Xs,Ys):-
	replace([frying_pan,olive_oil],Xs,[frying_oil],Ys).
fry_eggs(Xs,Ys):-
	replace([frying_oil,whisked_eggs],Xs,[frying_eggs],Ys).
season(Xs,Ys):-
	replace([frying_eggs,pepper,salt],Xs,[omelette],Ys).


%!	replace(+Set1,+Set2,+Set3,+Set4) is det.
%
%	Replace Set1 in Set3 with Set2 to make Set4.
%
%	Set1, Set2, Set3 and Set4 are ordered sets, i.e. list sorted to
%	the standard order of terms and without any duplicates.
%
%	Set4 is Set3 subtracting Set1 and adding Set2. Or, more
%	formally:
%	==
%	Set4 = (Set3 \ Set1) U Set2
%	==
%
%	@tbd Makes no attempt to test whether any of its arguments is an
%	ordered set.
%
replace(Xs,Is,Ys,Os):-
	ground(Xs)
	,ground(Is)
	,ground(Ys)
	,ord_subset(Xs,Is)
	,ord_subtract(Is,Xs,Zs_)
	,ord_union(Ys,Zs_,Os).


% Target theory for omelette
% Thelma learns a better one with a bit of invention.
recipe_(As,Fs):-
	break_eggs(As,Bs)
	,whisk_eggs(Bs,Cs)
	,heat_oil(Cs,Ds)
	,fry_eggs(Ds,Es)
	,season(Es,Fs).

/*
So the idea is to reprsent the state of the world as a list of
ingredients and possibly implements, such as:

[eggs,salt,pepper,oil,pan]

Then, each successive processing step, represented by a dyadic action
modifies the state until it matches the desired state, which is the
prepared dish. Flunets may also test various conditions on the
ingredients and pan. The state list can be modified by adding or
removing new elements to or from it. For instance, if you break an egg
to make an omelette, the program should change:

[eggs,salt,pepper,oil,pan]

To:

[broken_eggs,salt,pepper,oil,pan]

The recipe obviously is the program that goes from the starting list of
ingredients and implements to the prepared dish. So the preparation
"instructions" of the recipe are the dyadics and possibly also fluents
in the hyppthesis.

?- recipes:cook([eggs,salt,pepper,oil,pan],B).
B = [fried_eggs, salt, pepper, oil, pan] ;
false.

cook(Es, Fs):-break_eggs(Es, Bs), fry_eggs(Bs, Fs).


break_eggs(S1, S2):-
	select(eggs,S1,broken_eggs,S2).

fry_eggs(S1, S2):-
	select(broken_eggs,S1,fried_eggs,S2).
*/
