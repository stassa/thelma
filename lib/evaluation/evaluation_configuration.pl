:-module(evaluation_configuration, [decimal_places/1
                                   ,success_set_generation/1
                                   ]).


%!	decimal_places(?P) is semidet.
%
%	Number of decimal places to report in evaluation metrics.
%
%	@tbd Could use a better name...
%
decimal_places(2).


%!	success_set_generation(?Type) is semidet.
%
%	How to generate the success set of learned hypotheses.
%
%       The success set, SS(H) of a learned hypothesis H is generated as
%       a first step in evaluating H against the testing examples.
%
%	Type can be one of: [tp,sld]
%
%       If Type is "tp", SS(H) is generated by first grounding the
%       background knowledge for the relevant MIL problem and then
%       deriving the Least Herbrand Model of H in the context of the
%       grounded background knowledge. The Least Herbrand Model of H is
%       derived using a TP operator, with lfp_query/4.
%
%       If Type is "sld", SS(H) is generated by proving H by SLD
%       resolution and adding each true atom derived during the proof to
%       SS(H). This is done by calling a term T, such that T has the
%       predicate symbol and arity of the target predicate in H and each
%       argument of T is a free variable. For example, if the predicate
%       symbol of the target predicate for H is p/2, the term T = p(A,B)
%       is called and all bindings of T are added to SS(H). The call to
%       T is performed in module user and in the context of the
%       background knowledge for H, so the background definitions for H
%       are first written to module user, unless they are already loaded
%       in that module.
%
%       Tradeoffs
%       ---------
%
%       The primary tradeoff between the two types is efficiency versus
%       termination. Type "tp" derives SS(H) by evaluating H bottom-up
%       and so is guaranteed to terminate even when H has left-recursive
%       clauses, but the TP operator implementation in lib(tp/tp) is
%       slow and inefficient. Type "sld" may enter an infinite recursion
%       but it is reasonably fast and efficient (not least because it
%       doesn't have to ground the background knowledge before
%       deriving SS(H)). If you care about efficiency in evaluation, use
%       Type "sld". If you expect H to have left-recursive clauses, use
%       Type "tp".
%
%       A secodary consideration is the type of clauses in H and the
%       background knowledge. Type "tp" cannot correctly derive SS(H)
%       unless the background knowledge for H a) can be ground and b) is
%       definite datalog (i.e. has no clauses with function symbols of
%       arity more than 0, or literals negated by negation as failure).
%       This is basically necessary for bottom-up evaluation but it's
%       not always easy to do. For example, it is difficult to find a
%       sensible way to ground DCG grammars (the cons operator in
%       difference lists is a function symbol, after all) or background
%       definitions that do not have an extensionally defined predicate
%       in their transitive closure. If you have background knowledge
%       that is non-ground or is not datalog, prefer Type "sld" to
%       avoid misleading evaluation results.
%
%       A further consideratino is that Type "sld" uses the dynamic
%       database to store H before deriving SS(H). If a definition of
%       the target predicate in H is already in the dynamic database,
%       this can lead to spurious results, so one must make sure that
%       this is not the case. Additionally, writing to the dynamic
%       database can be slow (because the program must be recompiled)
%       and that may be a problem if a hypothesis gets to grow to
%       a really large size.
%
%       Motivation
%       ----------
%
%       The reason why there are two types of success set generation is
%       complicated and it may be that one, in particular, Type "tp",
%       will be deprecated in the future.
%
%       A TP operator was originally used to derive SS(H) because of the
%       tendency of Louise to learn left-recursive hypotheses that make
%       generation of a success set difficult using SLD resolution. At
%       the time, the project did not use SLG resolution and the
%       preferred way to control infinite recursion for left-recursive
%       predicates was by using bottom-up evaluation with the TP
%       operator implementation in lib(tp/tp). Now that SLG resolution
%       is commonly used in the project, it may be that bottom up
%       evaluation is no longer needed.
%
success_set_generation(sld).
