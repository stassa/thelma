:-module(experiment_file, [reload/0
			  ,experiment_file_interface/1]).

/** <module> Experiment file interface and reloading.

The most important predicate in this module is reload/0. This is well, a
bit of a hack, or rather a whole bunch of hacks, performed in order to
allow switching between experiment files without having to exit Prolog
and start a new session to clear up interface predicates from memory.
See the documentation in that predicate for more details.

This module also exports experiment_file_interface/1 that determines the
interface of an experiment file, i.e. the least predicates it must
export. This is currently only cosmetic (although it is used by
reload/0, nothing else requires it).

*/

:-dynamic '$experiment_file'/2.

%!	reload is det.
%
%	Reload the currently configured experiment file.
%
%	Ensures interface predicates are updated to the ones exported by
%	the module currently set as the experiment file module, in
%	configuration option experiment_file/2.
%
%	Motivation
%	----------
%
%	Experiment file modules have a common interface, consisting of a
%	number of predicate symbols that they are all expected to
%	export. Unfortunately, in Swi-Prolog, attempting to export the
%	same predicate symbols from two different modules raises a
%	permission error. This means that, to allow two different
%	experiment files to be used without errors, the user must exit
%	Swi-Prolog and start a new session.
%
%	To avoid this inconvenience, this predicate performs a number of
%	steps that ensure that only the interface predicates exported by
%	the experiment file set in the configuration option
%	experiment_file/2 are loaded memory in and that loading a new
%	experiment file does not raise permission errors.
%
%	Reloading process
%	-----------------
%
%	These steps are as follows:
%	a) The path and module name of the currently configured
%	experiment file are registered (as a dynamic term).
%	b) The dynamic term registering the path and module name of the
%	previously configured experiment file is retracted.
%	c) The experiment file interface predicates are abolished from
%	module user.
%	d) The experiment file interface predicates are abolished from
%	the previously registered experiment module.
%	e) The experiment fiel interface predicates declared in the
%	currently configured experiment module are re-exported by this
%	module.
%
%	The end result of this is that only the definitions of the
%	experiment file interface predicates that are given in the
%	experiment file currently listed in experiment_file/2 are in the
%	database.
%
%	Note that those interface predicates are loaded into module
%	user so that they can be readily accessed from any predicate in
%	the project, and on the command line. To do this, the present
%	module is loaded into module user by the configuration module,
%	which is the first module loaded when the project is started.
%
%	This predicate should generally not be called by user code. It
%	is called at the end of the configuration module, to ensure that
%	the latest configured experiment_file/2 option is available.
%
reload:-
	configuration:experiment_file(P, M)
	,replace_experiment_file(P,M)
	,abolish_experiment_file_interface(user)
	,abolish_experiment_file_interface(experiment_file)
	,interface_reexports(Es)
	,reexport(P, except(Es)).


%!	replace_experiment_file(+Path,+Module) is det.
%
%	Replace the previous experiment file with a new one.
%
%	Path and Module are the path and module of the experiment file
%	listed in the configuration option experiment_file/2.
%
%	This predicate is responsible for registering the latest
%	experiment file Path and Module name, while unregistering those
%	of the previous one. It is called by reload/0. If an experiment
%	file is not already registered, the new experiment file Path and
%	Module name are registered. If an experiment file is already
%	registered, it is first unregistered and then the new Path and
%	Module name are registered.
%
%	By "register" and "unregister" what is meant is that a dynamic
%	predicae is added to the database that has the Path and Module
%	arguments as paramters.
%
replace_experiment_file(P,M):-
	\+ '$experiment_file'(_,_)
	,assert('$experiment_file'(P,M))
	,!.
replace_experiment_file(P1,M1):-
	'$experiment_file'(P0,_M0)
	,unload_file(P0)
	,retractall('$experiment_file'(_,_))
	,assert('$experiment_file'(P1,M1)).


%!	abolish_experiment_file_interface(+Module) is det.
%
%	Abolish all instances of experiment file interface from Module.
%
%	Used to clean up the user module and experiment_file module from
%	definitions of the experiment file predicates that are created
%	when these are re-exported from an actual experiment file
%	module. Called by reload/0.
%
abolish_experiment_file_interface(M):-
	experiment_file_interface(Is)
	,forall(member(F/A, Is)
	       ,abolish(M:F/A)
	       ).


%!	experiment_file_interfact(?Interface) is semidet.
%
%	List of predicate symbols exported by experiment files.
%
experiment_file_interface([background_knowledge/2
			  ,metarules/2
			  ,positive_example/2
			  ,negative_example/2
			  ]).


%!	interface_reexports(+Exports) is det.
%
%	Collect the names of reexported interface predicates.
%
%	Exports is a list of compounds, P as R, where P the predicate
%	symbol and arity of an interface predicate, as defined by
%	interface_predicate/1, and R its renaming term used in
%	reexport/2. In practice, P itself is always the renaming term.
%
interface_reexports(Es):-
	experiment_file_interface(Is)
	,findall(F/A as F
		,member(F/A,Is)
		,Es).
