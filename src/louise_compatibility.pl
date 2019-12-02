:-module(louise_compatibility, [list_config/0
			       ,print_or_debug/3
			       ]).


/** <module> Predicates ensuring compatibility with Louise

*/


%!	list_config is det.
%
%	Print configuration options to the console.
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	printed.
%
list_config:-
	list_config(print,user_output).


%!	list_config(+Print_or_Debug,+Atom) is det.
%
%	Print or debug current configuration options.
%
list_config(T,S):-
	module_property(configuration, exports(Es))
	,findall(Opt_
		,(member(F/A,Es)
		 ,\+ memberchk(F, [metarule,metarule_constraints])
		 ,functor(Opt,F,A)
		 ,predicate_property(Opt, implementation_module(configuration))
		 ,call(configuration:Opt)
		 % Keep as list to sort by functor only
		 % Standard order of terms sorts by arity also.
		 ,Opt =.. Opt_
		 )
		,Opts)
	,sort(1,@<,Opts, Opts_)
	,forall(member(Opt, Opts_)
	       ,(Opt_ =.. Opt
		,print_or_debug(T,S,Opt_)
		)
	       ).



%!	print_or_debug(+Print_or_Debug,+Stream_or_Subject,+Atom) is
%!	det.
%
%	Print or debug an Atom.
%
%	Print_or_Debug can be one of: [print,debug,both]. If "print",
%	Stream_or_Subject should be the name or alias of a stream
%	and Atom is printed at that Stream. If "debug",
%	Stream_or_Subject should be a debug subject and Atom is printed
%	to the current debug stream, iff the specified subject is being
%	debugged. If "both", Stream_or_Subject should be a term Str/Sub,
%	where Str the name or alias of a stream and Sub the name of
%	debug topic; then Atom is printed to the specified stream and
%	also to the current debug topic if Sub is being debugged.
%
print_or_debug(debug,S,C):-
	debug(S,'~w',[C]).
print_or_debug(print,S,C):-
	format(S,'~w~n',[C]).
print_or_debug(both,Str/Sub,C):-
	print_or_debug(print,Str,C)
	,print_or_debug(debug,Sub,C).
