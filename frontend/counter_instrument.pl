% Author: Jorge. A Navas, The University of Melbourne 2013

:- module(counter_instrument, [counter_instrument/2, counter_instrument/5]).

%  Own libraries
:- use_module(readprog).
:- use_module(writeprog).
:- use_module('../adt/balanced_tree').
:- use_module('../analysis/scc').
:- use_module(well_formed).
%  Ciao libraries
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(filenames), [file_name_extension/3]).
:- use_module(library(lists),     [last/2]).
:- use_module(library(assoc)).

% To understand clpr operators
:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

% TODO/FIXME: for simplicity, this transformation add new variables
% which are not added into the dictionary in the clause/2
% predicate. Thus, make sure that the dictionary is never used.

% Also, the current transformation does not work for nonlinear horn
% clauses although it should not be hard to fix that.
       	                  
%------------------------------------------------------------------------------%
% counter_instrument(+,+,+,+,-)
% counter_instrument(+,+,+,+,+,+,+,-)
%------------------------------------------------------------------------------%
% For each recursive clause of the form p(X):- ..., q(X) it will transform
% it into: p(X,K):- K>=0, ..., K1=K-1, p(X,K1).
%------------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).
counter_instrument(InFile,OutFile):-
        readprog(InFile, Prog, Dirs), 	
        sortClauses(Prog,Ps,Procs),
        counter_instrument(Ps, Procs, Dirs, InFile,  OutFile).
counter_instrument(Ps, Procs, Dirs, InFile, OutFile):-
        /* here transformations */
        recursive_preds(Ps, Procs, RecPreds, OldSCCs),
        is_linear(Procs, OldSCCs), % well-formed condition
        % add_last_arg_in_rec_atom/6 invalidates Procs, Dirs,
        % RecPreds, and NewPs. Thus, they must be recomputed.
        add_last_arg_in_rec_atom(Procs,Dirs,RecPreds,NewPs,NewProcs,NewDirs),
        % TODO/FIXME: for simplicity SCCs are computed twice!
        recursive_preds(NewPs, NewProcs, NewRecPreds, SCCs),
        recursive_clauses(NewPs, NewProcs, RecCls),	
        find_backedges(NewPs, NewProcs, BackEdges),
        empty_assoc(EmptyClsMap),
        compute_stable_map_pred_to_clauses(NewProcs, EmptyClsMap, ClsMap),
        instrument_rec_preds(NewPs,root,_,ClsMap,SCCs,NewRecPreds,RecCls,BackEdges,InstrCls),
        /* here write the transformed program into OutFile */		 
        file_name_extension(InFile,Base,Extension),	
        atom_concat(Base,'__counter_instr', Base0),
        atom_concat(Base0,Extension, OutFile),
        open(OutFile,write,Stream),	
        writeprog(InstrCls,  NewDirs, Stream),
        close(Stream).
:- pop_prolog_flag(multi_arity_warnings).	

% This predicate takes the whole SCC and tranform it.
instrument_rec_preds([],MarkList,MarkList,_,_,_,_,_,[]).
instrument_rec_preds([P|Ps], MarkListIn, MarkListOut, ClsMap, SCCs, RecPreds, RecCls, Backs, NewCls):-   
        % already transformed
        search_tree(MarkListIn, P, black), % P already visited
        instrument_rec_preds(Ps, MarkListIn, MarkListOut, ClsMap, SCCs, RecPreds, RecCls, Backs, NewCls).
instrument_rec_preds([P|Ps],MarkListIn,MarkListOut,ClsMap,SCCs,RecPreds, RecCls, Backs, NewCls):-
        % recursive predicate
        member(P,RecPreds),
        !,
        find_scc_pred(SCCs,P,SCC),
        instrument_scc(SCC,MarkListIn,MarkListMid,ClsMap,RecPreds,RecCls,Backs,SCC_Cls),
        instrument_rec_preds(Ps, MarkListMid, MarkListOut, ClsMap, SCCs, RecPreds, RecCls, Backs, RestCls),
        append(SCC_Cls, RestCls, NewCls).
instrument_rec_preds([P|Ps], MarkListIn, MarkListOut, ClsMap, SCCs, RecPreds, RecCls, Backs, NewCls):-
        % non-recursive predicate
        get_assoc(P, ClsMap, P_Cls),
        !,
        instrument_rec_preds(Ps, MarkListIn, MarkListOut, ClsMap, SCCs, RecPreds, RecCls, Backs, RestCls),
        append(P_Cls, RestCls, NewCls).
instrument_rec_preds([_|Ps], MarkListIn, MarkListOut, ClsMap, SCCs, RecPreds, RecCls, Backs, NewCls):-
        % builtin
        instrument_rec_preds(Ps, MarkListIn, MarkListOut, ClsMap, SCCs, RecPreds, RecCls, Backs, NewCls).
                                    
instrument_scc(SCCs,MarkListIn,MarkListOut,ClsMap,RecPreds,RecCls,Backs,NewCls):-
        instrument_counter_guard_scc(SCCs,ClsMap,RecPreds,RecCls,NewClsMap),
        instrument_counter_decr_scc(SCCs,SCCs,MarkListIn,MarkListOut,NewClsMap,Backs,NewCls).
                    
instrument_counter_guard_scc([],ClsMap,_,_,ClsMap).
instrument_counter_guard_scc([P|Ps],ClsMap,RecPreds,RecCls,ClsMap_Out):-
        get_assoc(P, ClsMap, P_Cls),
        add_counter_guard_scc(P_Cls,P,1,RecPreds,RecCls, P_NewCls),
        update_assoc(P, ClsMap, P_NewCls, ClsMap_Mid, _),
        instrument_counter_guard_scc(Ps,ClsMap_Mid,RecPreds,RecCls,ClsMap_Out).
        
instrument_counter_decr_scc([], _, MarkList, MarkList, _, _, []).
instrument_counter_decr_scc([P|Ps],SCCs,MarkListIn,MarkListOut,ClsMap,Backs,NewCls):-
        get_assoc(P, ClsMap, P_Cls),
        add_counter_decr(P_Cls, SCCs, Backs, New_P_Cls),
        insert_tree(MarkListIn,P,black,MarkListMid), % mark node as visited
        instrument_counter_decr_scc(Ps,SCCs,MarkListMid,MarkListOut,ClsMap,Backs,RestCls),
        append(New_P_Cls, RestCls, NewCls).

add_counter_decr([], _, _, []).
add_counter_decr([clause((H:-B),Vs)|Cls], SCCs, Backs, [clause((H:-NewB),Vs)|NewCls]) :-
        add_counter_decr_body(B, H, SCCs, Backs, NewB),
        add_counter_decr(Cls, SCCs, Backs, NewCls).

% add_counter_decr_body(+Body,+Term,+SCCs,+BackEdges,-NewBody).
add_counter_decr_body(true,_,_,_,true).
add_counter_decr_body((B,Bs),ClHead,SCCs,Backs,(NewB,NewBs)):-
        add_counter_decr_lit(B,ClHead,SCCs,Backs,NewB),
        add_counter_decr_body(Bs,ClHead,SCCs,Backs,NewBs).
add_counter_decr_body(B,ClHead,SCCs,Backs,NewB):-
        add_counter_decr_lit(B,ClHead,SCCs,Backs,NewB).

% is_backedge(+,+,+)
is_backedge(Backs, GoalA, GoalB):-
        functor(GoalA,FA,NA),
        functor(GoalB,FB,NB),
        member(FA/NA-FB/NB, Backs),
        !.

add_counter_decr_lit(B,ClHead,SCCs, Backs,NewB):-
        ( (functor(B,F,A), member(F/A, SCCs)) ->
          % unify K with last argument of the clause head and K1 with
          % the last argument of the body literal.
	arg_counter_param(ClHead, K),
	% needed if some unification has been already done
	%free_last_argument(B, NB),
	NB=B,
	arg_counter_param(NB, K1),
	% TODO/FIXME: add K1 into dictionary of clause/2.
	( is_backedge(Backs, ClHead, B) ->
            NewB = (counter_inst__clp_meta([ K1 .=. K - 1]), NB)
	;
            NewB = NB,
	  K = K1
          )
          % unify K with last argument of the clause head
          %arg_counter_param(ClHead, K),
	%arg_counter_param(B, K)
        ;
	NewB = B
        ).

% free_last_argument(+term,-term)
free_last_argument(Term, NewTerm):-
        Term =.. L,
        free_last_argument_aux(L, NL),
        NewTerm =.. NL.
free_last_argument_aux([],[]).
free_last_argument_aux([_],[_Fresh]).
free_last_argument_aux([H|T],[H|NT]):-
        free_last_argument_aux(T, NT).

% This predicate assumes that the counter parameter introduced by
% instrumentation is always the last one.
arg_counter_param(Term, K):- functor(Term,_F,A), arg(A,Term,K).
        
% add_counter_guard_scc(+Cls,F/A,ClId,+RecPreds,+RecCls,-NewCls).
% IMPORTANT: add the constraint only if the clause is recursive.
% Pre: H = F/A
add_counter_guard_scc([], _, _, _, _, []).
add_counter_guard_scc([clause((H:-B),Vs)|Cls], F/A, ClId, RecPreds, RecCls, 
                      [clause((H:-NewB),Vs)| NewCls]) :-
        member(F/A/ClId, RecCls),
        !,
        % unify K with the last argument of the clause head.
        arg_counter_param(H,K),
        NewB = (counter_inst__clp_meta([ K .>=. 0]), B),
        % unify K with the last argument of each recursive body literal.
        %unify_arg_with_rec_body_lits(B,K,RecPreds),
        NextClId is ClId + 1,
        add_counter_guard_scc(Cls, F/A, NextClId,  RecPreds, RecCls, NewCls).
add_counter_guard_scc([Cl|Cls], F/A, ClId,  RecPreds, RecCls, [Cl|NewCls]):-
        NextClId is ClId + 1,
        add_counter_guard_scc(Cls, F/A, NextClId, RecPreds, RecCls, NewCls).
        
% unify_arg_with_rec_body_lits(?Body, -var,-list).
% traverse a body and unify the last argument of any recursive literal
% with Arg.
unify_arg_with_rec_body_lits(true, _, _).
unify_arg_with_rec_body_lits((B,Bs), Arg, RecPreds):-
        functor(B,F,A),
        ( member(F/A,RecPreds) -> arg_counter_param(B,Arg) ; true),
        unify_arg_with_rec_body_lits(Bs, Arg, RecPreds).
unify_arg_with_rec_body_lits(B, Arg, RecPreds):-
        functor(B,F,A),
        ( member(F/A,RecPreds) -> arg_counter_param(B,Arg) ; true).
        

%-------------------------------------------------------------------------------%
% add_last_arg_in_rec_atom(+Procs,+Dirs,+RecPreds,-NewPs,-NewProcs,-NewDirs).
%-------------------------------------------------------------------------------%
% for each atom that appears in the head or body literal it adds a new
% argument in the last position.
%-------------------------------------------------------------------------------%
add_last_arg_in_rec_atom(Procs, Dirs, RecPreds, NewPs, NewProcs, NewDirs):-
        add_last_arg_in_rec_atom_(Procs,RecPreds,NewProcs),
        unzip_proc_list(NewProcs, NewPs),
        update_directives(Dirs, RecPreds, NewDirs).

% IMPORTANT: this predicate must be updated if more directives are
% added in tclp_package.pl
update_directives([],_,[]).
update_directives([Dir|Dirs],RecPreds,[NewDir|NewDirs]):-
        update_directive(Dir, RecPreds, NewDir),
        update_directives(Dirs,RecPreds,NewDirs).

update_directive(tabled(Pred)       , RecPs, tabled(NewPred)):-
        add_last_arg_in_atom(Pred, RecPs, NewPred, Last, ChangedFlag),
        ( ChangedFlag==1 ->
          Last = num
        ; 
          true
        ).
update_directive(no_cache(Pred)     , RecPs, no_cache(NewPred)):-
        add_last_arg_in_atom(Pred, RecPs, NewPred, _, _).
update_directive(discriminants(Pred), RecPs, discriminants(NewPred)):-
        add_last_arg_in_atom(Pred, RecPs, NewPred, Last, ChangedFlag),
        ( ChangedFlag==1 ->
          Last = d
        ; 
          true
        ).
update_directive(mode(Pred)         , RecPs, mode(NewPred)):-
        add_last_arg_in_atom(Pred, RecPs, NewPred, Last, ChangedFlag),
        ( ChangedFlag==1 ->
          Last = in
        ; 
          true
        ).
update_directive(Dir, _, Dir).

unzip_proc_list([],[]).
unzip_proc_list([proc(P,_Cls)|Procs],[P|RestPs]):-  
        unzip_proc_list(Procs,RestPs).

add_last_arg_in_rec_atom_([], _, []).
add_last_arg_in_rec_atom_([proc(F/A,Cls)|Procs], RecPreds, [proc(NewP,NewCls)| NewProcs]):-
        add_last_arg_in_rec_atom_clause(Cls, RecPreds, NewCls),
        ( member(F/A,RecPreds)->
	A1 is A + 1,
	NewP = F/A1
        ;
	NewP = F/A
        ),
        add_last_arg_in_rec_atom_(Procs, RecPreds, NewProcs).
        
                         
add_last_arg_in_rec_atom_clause([],_,[]).
add_last_arg_in_rec_atom_clause([clause((H:-B),Vs) | Cls], RecPreds, 
                                [clause((NewH:-NewB),Vs) | NewCls]):-
        add_last_arg_in_atom(H, RecPreds, NewH, _Last, _ChangedFlag),
        % TODO/FIXME: add Last into dictionary of clause/2 if ChangedFlag=1
        add_last_arg_in_body(B, RecPreds, NewB),
        add_last_arg_in_rec_atom_clause(Cls, RecPreds, NewCls).
        
% Last is a free variable corresponding to the last argument of Goal
% if ChangedFlag=1.
add_last_arg_in_atom(Goal, RecPreds, NewGoal, Last, ChangedFlag):-
        functor(Goal,F,A),
        ( member(F/A, RecPreds) ->
          Goal =.. L,
          append(L,[Last], L1),
          NewGoal =.. L1,
	ChangedFlag=1
        ;
          NewGoal = Goal,
	ChangedFlag=0
        ).

add_last_arg_in_body(true, _, true).
add_last_arg_in_body((B,Bs), RecPreds, (NewB,NewBs)):-
        add_last_arg_in_atom(B, RecPreds, NewB, _Last, _ChangedFlag),
        add_last_arg_in_body(Bs, RecPreds, NewBs).
add_last_arg_in_body(B, RecPreds, NewB):-
        add_last_arg_in_atom(B, RecPreds, NewB, _Last, _ChangedFlag).
        

%-------------------------------------------------------------------------------%
% compute_map_pred_to_clauses(+Procs,+Assoc0,-Assoc1)                   
%-------------------------------------------------------------------------------%
% For each predicate F/A it finds its clauses and store them into an
% associative list.  Clauses are stored in the same order than appear
% in Procs.
%-------------------------------------------------------------------------------%
compute_stable_map_pred_to_clauses([],Assoc,Assoc).
compute_stable_map_pred_to_clauses([proc(F/A, Cls) | Procs], Assoc0, Assoc2):-
        compute_stable_map_pred_to_clauses_aux(Cls, F/A, Assoc0, Assoc1),
        compute_stable_map_pred_to_clauses(Procs, Assoc1, Assoc2).
                
compute_stable_map_pred_to_clauses_aux([], _, Assoc, Assoc).
compute_stable_map_pred_to_clauses_aux([clause((H:-B),Vs) | Cls], F/A, 
                                       Assoc, Assoc2):-
        functor(H, F, A),
        !,
        ( get_assoc(F/A, Assoc, PsCls) ->
	append(PsCls, [clause((H:-B),Vs)], NewPsCls)
        ;
          NewPsCls = [clause((H:-B),Vs)]
        ),
        put_assoc(F/A,Assoc,NewPsCls,Assoc1),
        compute_stable_map_pred_to_clauses_aux(Cls, F/A, Assoc1, Assoc2).

% find_backedges(+,+,-)	        
find_backedges(Ps, Prog, BackEdges):-
        make_callgraph(Ps, Prog, G),
        wgb_dfs(G, BackEdges,_).

% White-Grey-Black DFS
wgb_dfs(Graph, BackEdges, CrossEdges) :-
        traversekey_tree(Graph,Nodes),
        dfsSweep(Nodes,Graph,root,_,[],BackEdges,[],CrossEdges).

dfsSweep([], _, MarkList, MarkList, Back, Back, Cross, Cross).
dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, BackIn, BackOut, CrossIn, CrossOut) :-
        search_tree(MarkListIn,N,_),   
        % N already visited
        dfsSweep(Ns, Graph, MarkListIn, MarkListOut, BackIn, BackOut, CrossIn, CrossOut).

dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, BackIn, BackOut, CrossIn, CrossOut) :-
        % N is white
        dfsNode(Graph, N, MarkListIn, MarkListMid, BackIn, BackMid, CrossIn, CrossMid),
        dfsSweep(Ns, Graph, MarkListMid, MarkListOut, BackMid, BackOut, CrossMid, CrossOut).

dfsNode(Graph,N,M0,M3,B0,B1,C0,C1) :-
        insert_tree(M0,N,grey,M1),                % mark node as discovered being exploredy
        find_succs(Graph,N,SuccList),
        dfs_each(SuccList,Graph,N,M1,M2,B0,B1,C0,C1),
        search_replace_tree(M2, N, _, M3, black). % mark node as black (completed)

find_succs(Graph,N,SuccList) :-
        search_tree(Graph,N,links(SuccList,_)), !.
find_succs(_,_,[]).

dfs_each([],_,_,M,M,B,B,C,C).
dfs_each([N|Ns],G,Par,M0,M1,B0,B2,C0,C2) :-
        search_tree(M0,N, Color),        % N is black or grey
        !,
        ( Color ==  grey ->  % backedge 
          B1 = [Par-N | B0],
          C1 = C0
        ;
	% Color == black   % cross edge  
	B1 = B0,
	C1 = [Par-N | C0]
        ),
        dfs_each(Ns,G,Par,M0,M1,B1,B2,C1,C2).
dfs_each([N|Ns],G,Par,M0,M2,B0,B2,C0,C2) :-    % N is white
        dfsNode(G,N,M0,M1,B0,B1,C0,C1),
        dfs_each(Ns,G,Par,M1,M2,B1,B2,C1,C2).



                   

