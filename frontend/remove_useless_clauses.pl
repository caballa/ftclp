% Author: Jorge. A Navas, The University of Melbourne 2014

:- module(remove_useless_clauses, [remove_useless_clauses/2, remove_useless_clauses/5]).

%  Own libraries
:- use_module(readprog).
:- use_module(writeprog).
:- use_module('../analysis/scc').
%  Ciao libraries
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(filenames), [file_name_extension/3]).

%------------------------------------------------------------------------------%
% remove_useless_clauses(+,-)
% remove_useless_clauses(+,+,+,+,-)
%------------------------------------------------------------------------------%
% Remove useless clauses (i.e., dead code).
%   - recursive predicates without a base case.
%------------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).
remove_useless_clauses(InFile,OutFile):-
        readprog(InFile, Prog, Dirs), 	
        sortClauses(Prog,Ps,Procs),
        remove_useless_clauses(Ps, Procs, Dirs, InFile,  OutFile).
remove_useless_clauses(Ps, Procs, Dirs, InFile, OutFile):-
        recursive_preds(Ps, Procs, RecPreds, _),
        infer_nonterminating_predicates(Procs, RecPreds, NonTermPs),
        remove_non_terminating_predicates(Procs, NonTermPs, [], NewCls),
        /* here write the transformed program into OutFile */		 
        file_name_extension(InFile,Base,Extension),	
        atom_concat(Base,'__useless_clause_removal', Base0),
        atom_concat(Base0,Extension, OutFile),
        open(OutFile,write,Stream),	
        writeprog(NewCls,  Dirs, Stream),
        close(Stream).
:- pop_prolog_flag(multi_arity_warnings).	

remove_non_terminating_predicates([], _NonTermProcs, NewCls, NewCls).
remove_non_terminating_predicates([proc(F/A, _) | Procs], NonTermPs, NewCls0, NewCls1):-
        member(F/A, NonTermPs), % non-terminating predicate
        !,
        remove_non_terminating_predicates(Procs, NonTermPs, NewCls0, NewCls1).
remove_non_terminating_predicates([proc(_, Cls) | Procs], NonTermPs, NewCls0, NewCls2):-
        replace_literal_with_failure(Cls, NonTermPs, NewCls0, NewCls1),
        remove_non_terminating_predicates(Procs, NonTermPs, NewCls1, NewCls2).
        
replace_literal_with_failure([], _NonTermPs, NewCls, NewCls).
replace_literal_with_failure([clause((H:-B),Vs)|Cls], NonTermPs, NewCls0, NewCls2):-
        replace_literal_with_failure__body(B, NonTermPs, NewB),
        append(NewCls0, [clause((H:-NewB),Vs)], NewCls1),
        replace_literal_with_failure(Cls, NonTermPs, NewCls1, NewCls2).
                
replace_literal_with_failure__body(true, _NonTermPs, true).
replace_literal_with_failure__body((B, Bs), NonTermPs, (fail, NewBs)):-
        functor(B, F, A), 
        member(F/A, NonTermPs),
        !,
        replace_literal_with_failure__body(Bs, NonTermPs, NewBs).
replace_literal_with_failure__body((B, Bs), NonTermPs, (B, NewBs)):-
        replace_literal_with_failure__body(Bs, NonTermPs, NewBs).
replace_literal_with_failure__body(B, NonTermPs, fail):-
        functor(B, F, A), 
        member(F/A, NonTermPs),
        !.
replace_literal_with_failure__body(B, _NonTermPs, B).
                       
%---------------------------------------------------------------------------%
% infer_nonterminating_predicates(+Proc,+RecPs, -NonTermPs).
%---------------------------------------------------------------------------%
infer_nonterminating_predicates([], _, []).
infer_nonterminating_predicates([ proc(F/A, Cls) | Procs], RecPs, [ F/A | NonTermProcs]):-
        has_no_solutions( proc(F/A, Cls), RecPs),
        !,
        infer_nonterminating_predicates(Procs, RecPs, NonTermProcs).
infer_nonterminating_predicates([ _ | Procs], RecPs, NonTermProcs):-
        infer_nonterminating_predicates(Procs, RecPs, NonTermProcs).

%---------------------------------------------------------------------------%
% has_no_solutions(+,+)
%---------------------------------------------------------------------------%
% A predicate has no solutions if all its clauses are direct
% recursive. The general problem is undecidable so this just tries to
% catch a common pattern when the CLP program is originated from a C
% frontend like the one from the VeriMAP tool.
%---------------------------------------------------------------------------%
has_no_solutions(proc(_, []), _):- !, fail.
has_no_solutions(proc(F/A, Cls), RecPs):-
        member(F/A, RecPs),        
        True=1,
        all_clauses_are_direct_recursive(Cls, F/A, True, Flag), 
        !, 
        Flag == True.

all_clauses_are_direct_recursive([], _, F, F).
all_clauses_are_direct_recursive([ clause((_H:-B),_Vs) | Cls], F/A, F0, F2):-
        ( is_direct_recursive_clause(B, F, A) ->
	F1 is F0 * 1
        ;
          F1 is F0 * 0   
        ),
        all_clauses_are_direct_recursive(Cls, F/A, F1, F2).
	                
is_direct_recursive_clause((B, _Bs), F, A):- 
        functor(B, F, A), !.
is_direct_recursive_clause((_B, Bs), F, A):- 
        is_direct_recursive_clause(Bs, F, A).        
is_direct_recursive_clause(B, F, A):- 
        functor(B, F, A), !.


