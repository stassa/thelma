:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).
user:file_search_path(output, project_root(output)).

:-doc_browser.

:-use_module(configuration).
:-use_module(src(auxiliaries)).
:-use_module(src(thelma)).

edit_files:-
	edit(project_root(load_project))
	,edit(project_root(configuration))
	,edit(src(auxiliaries))
	,edit(src(thelma))
	,edit(data(tiny_kinship))
	.
:-edit_files.

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
%:-set_prolog_stack(global, limit(2**9*10**6)).
:-prolog_stack_property(global, limit(X))
,format('Global stack limit ~D~n',[X]).
