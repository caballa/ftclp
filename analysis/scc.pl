% Author: J. Gallagher 31/03/2006.(c) Roskilde University.

:- module(scc, [ recursive_preds/3, 
                 recursive_preds/4,
                 recursive_clauses/2, 
	       recursive_clauses/3,
	       find_scc_pred/3,
	       make_callgraph/3]).
%  Ciao libraries
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(sort)).
%  Own libraries
:- use_module('../frontend/readprog', [user_clauses/3, sortClauses/3]).
:- use_module('../adt/balanced_tree').
:- use_module('../adt/mystrings').
:- use_module('../frontend/readprog', [readprog/2]).

%-------------------------------------------------------------------------------%
% find_scc_pred(+SCCs,+Pred,-SCC)
%-------------------------------------------------------------------------------%
% SCC is a list with all the predicates in the same strongly connected
% component than Pred if Pred is recursive. Otherwise, it will fail.
%-------------------------------------------------------------------------------%
find_scc_pred([(non_recursive,_)|SCCs],F/A, SCC):- find_scc_pred(SCCs,F/A,SCC).
find_scc_pred([(recursive,SCC)| _],F/A, SCC)    :- member(F/A,SCC),!.
find_scc_pred([(recursive,_)|SCCs],F/A, SCC)    :- find_scc_pred(SCCs,F/A,SCC).

%-------------------------------------------------------------------------------%
% recursive_preds(+Filename,-RecPreds, -SCCs)
% recursive_preds(+Ps,+Procs,-RecPreds, -SCCs)
%-------------------------------------------------------------------------------%
% RecPreds is a list with the recursive predicates defined in Filename. 
% SCCs are the strongly connected components.
%-------------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).
recursive_preds(F, RecPreds, SCCs):-
	readprog(F, Prog), 	
	sortClauses(Prog,Ps,Procs),
	recursive_preds(Ps, Procs, RecPreds, SCCs).
recursive_preds(Ps, Procs, RecPreds, SCCs):-
	scc(Ps,Procs,SCCs),
	make_list_recursive_preds(SCCs, RecPreds).
:- pop_prolog_flag(multi_arity_warnings).	

make_list_recursive_preds([],[]).
make_list_recursive_preds([(non_recursive,_)|Xs], Ys):-
          make_list_recursive_preds(Xs,Ys).
make_list_recursive_preds([(recursive,Ps)|Xs], Zs):-
          make_list_recursive_preds(Xs,Ys),
          append(Ps,Ys,Zs).

%-------------------------------------------------------------------------------%
% recursive_clauses(+Filename,-RecCls)
% recursive_clauses(+Ps,+Procs,-RecCls)
%-------------------------------------------------------------------------------%
% RecCls is a list with the recursive clauses of the form P/N/K
% defined in Filename.
%
% Note: scc/3 does not provide any information about clauses. The
% trick is to lift clauses to predicates, compute the SCCs from the
% transformed program, and map back that information to the original
% program.
%-------------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).
recursive_clauses(F, RecCls):-
	readprog(F,Prog), 	
	sortClauses(Prog,Ps,Procs),
	recursive_clauses(Ps,Procs,RecCls).
recursive_clauses(_Ps,Procs,RecCls):-
	add_bridge_predicates(Procs, [], Procs1),
	get_preds(Procs1, Ps1),
	scc(Ps1,Procs1,G),
	make_list_recursive_preds(G, RecPreds),
	make_list_recursive_clauses(RecPreds,RecCls).
:- pop_prolog_flag(multi_arity_warnings).	

make_list_recursive_clauses([],[]).
make_list_recursive_clauses([P/N|Ps], [C/N/K|Cls]):-
          convert_pred_to_clause(P/N,C/N/K),
          !,
          make_list_recursive_clauses(Ps,Cls).
make_list_recursive_clauses([_|Ps], Cls):-
          make_list_recursive_clauses(Ps,Cls).

get_preds([],[]).
get_preds([proc(P/A,_)|Rs],[P/A|Ps]):- get_preds(Rs,Ps). 

% Convert a predicate into a clause if the predicate is of the form
% p_###_X/N. Otherwise, it fails.
convert_pred_to_clause(P/N,C/N/K):-
          atom_codes(P,Str),
          special_marker_str(Pattern),
          split(Str,Pattern,ListStr),
          ( ListStr = [A,B] ->	
            atom_codes(C,A),
            atom_codes(K_atm,B),
            atom_number(K_atm,K)
          ; 
	 fail
          ).
        
%-------------------------------------------------------------------------------%
% add_bridge_predicates(+,+,-)
% Transform a program such that each clause H :- B is renamed to
%    H :- P. 
%    P :- B.
%-------------------------------------------------------------------------------%
add_bridge_predicates([], Ps, Ps ).
add_bridge_predicates([proc(P/A,Cls) | Ps], AccPs, AllPs ):-
        lift_clauses_to_preds(Cls, 1, NewPs),
        length(NewPs, N),
        add_bridge_clauses(1, N, P/A, BridgeCls),
        append([proc(P/A,BridgeCls) | NewPs], AccPs, NewAccPs),
        add_bridge_predicates(Ps, NewAccPs, AllPs).

add_bridge_clauses(K, NumClauses, _, []):-
        K > NumClauses,
        !.
add_bridge_clauses(K, NumClauses, P/A, [clause((H :- B), _Vs)|Cls]):-
        functor(H,P,A),
        make_pred(P,K,NewP),
        functor(B,NewP,A),        
        K1 is K + 1,
        add_bridge_clauses(K1, NumClauses, P/A, Cls).
        
lift_clauses_to_preds([],_,[]).
lift_clauses_to_preds([clause((H :- B), _Vs) | Cls], K,
                      [proc(NewP/N, [clause((NewH :- B), _Vs)]) | Procs]):-
        functor(H,P,N), 
        make_pred(P, K, NewP), 
        functor(NewH,NewP,N),
        K1 is K + 1,
        lift_clauses_to_preds(Cls, K1, Procs).

special_marker('_###_').
special_marker_str([95,35,35,35,95]). % ascii codes
make_pred(P, K, P1):-
        atom_number(K_atm, K),
        special_marker(M),
        atom_concat(P,M, P0),
        atom_concat(P0,K_atm, P1).


%-------------------------------------------------------------------------------%
% Strongly connected components based on depth-first search of a
% graph.
%
% Algorithm by M. Sharir, adapted from Baase and Van Gelder, Chapter 7.5
% JPG 20/8/01
%-------------------------------------------------------------------------------%
% scc(Ps,Prog,Cs):   
% scc(+,+,-):
%-------------------------------------------------------------------------------%
%  Ps: a list of predicates.
%  Prog: list of clauses (read in by readprog/2)
%  Cs: a list of components, each labelled as recursive or non-recursive
%-------------------------------------------------------------------------------%	
:- push_prolog_flag( multi_arity_warnings, off ).
scc(Ps,Prog,Cs) :-
	make_callgraph(Ps,Prog,G),
	scc_sharir(G,Cs),
	!.
scc(Ps,Prog,Cs,G) :-
	make_callgraph(Ps,Prog,G),
	scc_sharir(G,Cs),
	!.
:- pop_prolog_flag( multi_arity_warnings ).


% scc_sharir: the SCC procedure.
scc_sharir(root,[]) :-
	!.
scc_sharir(Graph,SCCs) :-
	phase1(Graph,Stack),
	phase2(Stack,Graph, SCCs),
	!,
	recursive_classify(SCCs,Graph).
	
phase1(Graph,Stack) :-
	traversekey_tree(Graph,Nodes),
	dfsSweep(Nodes,Graph,root,_,[],Stack).

dfsSweep([], _, MarkList, MarkList, Stack, Stack).
dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, StackIn, StackOut) :-
	search_tree(MarkListIn,N,black),   % N already visited
	!,
	dfsSweep(Ns, Graph, MarkListIn, MarkListOut, StackIn, StackOut).
dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, StackIn, StackOut) :-
	dfsNode(Graph, N, MarkListIn, MarkListMid, StackIn, StackMid),
	dfsSweep(Ns, Graph, MarkListMid, MarkListOut, StackMid, StackOut).

dfsNode(Graph,N,M0,M2,S0,S1) :-
	insert_tree(M0,N,black,M1),   % mark node as visited
	find_succs(Graph,N,SuccList),
	dfs_each(SuccList,Graph,N,M1,M2,S0,S1).

find_succs(Graph,N,SuccList) :-
	search_tree(Graph,N,links(SuccList,_)),
	!.
find_succs(_,_,[]).

dfs_each([],_,Par,M,M,S,[Par|S]).
dfs_each([N|Ns],G,Par,M0,M1,S0,S1) :-
	search_tree(M0,N,black),
	!,
	dfs_each(Ns,G,Par,M0,M1,S0,S1).
dfs_each([N|Ns],G,Par,M0,M2,S0,S2) :-
	dfsNode(G,N,M0,M1,S0,S1),
	dfs_each(Ns,G,Par,M1,M2,S1,S2).

% phase 2:  use the depth-first ordering from phase 1
% to traverse the transposed graph.

phase2(Nodes,Graph,SCCs) :-
	dfsSweep2(Nodes,Graph,root,_,[],SCCs).

dfsSweep2([], _, MarkList, MarkList, S,S).
dfsSweep2([N|Ns], Graph, MarkListIn, MarkListOut, S0,S1) :-
	search_tree(MarkListIn,N,black),  % N already visited
	!,
	dfsSweep2(Ns, Graph, MarkListIn, MarkListOut, S0,S1).
dfsSweep2([N|Ns], Graph, MarkListIn, MarkListOut, S0,S2) :-
	dfsNode2(Graph, N, N,MarkListIn, MarkListMid,[],S1),
	dfsSweep2(Ns, Graph, MarkListMid, MarkListOut, [(_,S1)|S0],S2).

dfsNode2(Graph,N,L,M0,M2,S0,S1) :-
	insert_tree(M0,N,black,M1),  % mark node as visited
	search_tree(Graph,N,links(_,PrecList)),
	dfs_each2(PrecList,Graph,N,L,M1,M2,[N|S0],S1).

dfs_each2([],_,_,_,M,M,S,S).
dfs_each2([N|Ns],G,L,Par,M0,M1,S0,S1) :-
	search_tree(M0,N,black),
	!,
	dfs_each2(Ns,G,Par,L,M0,M1,S0,S1).
dfs_each2([N|Ns],G,Par,L,M0,M2,S0,S2) :-
	dfsNode2(G,N,L,M0,M1,S0,S1),
	dfs_each2(Ns,G,L,Par,M1,M2,S1,S2).

recursive_classify([],_).
recursive_classify([(recursive,[_,_|_])|Cs],G) :-
	!,
	recursive_classify(Cs,G).
recursive_classify([(recursive,[P])|Cs],G) :-
	direct_recursive(P,G),
	!,
	recursive_classify(Cs,G).
recursive_classify([(non_recursive,_)|Cs],G) :-
  	recursive_classify(Cs,G).

direct_recursive(P,G) :-
	search_tree(G,P,links(Ss,_)),
	member(P,Ss).

% starting from a list of predicates, 
% make an adjacency list representation of the call graph 
% and the transposed call graph (reversing links).

make_callgraph([],_,root).
	
make_callgraph([P|Ps],Prog,G) :-
	make_callgraph(Ps,Prog,G1),
	!,
	% could be optimised by using tree instead of list
	immed_depends(Prog,P,[],Es),	
	%write('Start forward links '), write(P),nl,
	insert_forward_links(G1,P,Es,G2),
	%write('Start backward links'),nl,
	insert_back_links(Es,P,G2,G).

insert_forward_links(G1,P,Es,G2) :-
	search_replace_tree(G1,P,links(_,Ss),G2,links(Es,Ss)),
	!.
insert_forward_links(G1,P,Es,G2) :-
	insert_tree(G1,P,links(Es,[]),G2).

insert_back_links([],_,G,G).
insert_back_links([Q|Qs],P,G0,G2) :-
	search_replace_tree(G0,Q,links(Ps,Ss),G1,links(Ps,Ss1)),
	!,
	union([P],Ss,Ss1),
	insert_back_links(Qs,P,G1,G2).
insert_back_links([Q|Qs],P,G0,G2) :-
	insert_tree(G0,Q,links([],[P]),G1),
	insert_back_links(Qs,P,G1,G2).

immed_depends(Prog,P/N,Rs,Rs1) :-
	user_clauses(Prog,P/N,Cls),
	body_preds(Cls,Rs,Rs1).

body_preds([(_ :- B)|Cs],S,S2) :-	
	bodylits(B,S,S1),
	body_preds(Cs,S1,S2).
body_preds([],S,S).

% to allow for domain programs
bodylits(domainBody(B,BD,HD),S,S3) :- 	
	!,
	bodylits(B,S,S1),
	bodylits(BD,S1,S2),
	bodylits(HD,S2,S3).
bodylits((B,Bs),S,S2) :-
	functor(B,T,N),
	insertp(T/N,S,S1),
	bodylits(Bs,S1,S2).
bodylits((_,Bs),S,S1) :-
	!,
	bodylits(Bs,S,S1).
bodylits(B,S,S1) :-
	functor(B,T,N),
	insertp(T/N,S,S1).
bodylits(_,S,S).

insertp(X,L,L) :-
	memb1(X,L),
	!.
insertp(X,L,[X|L]).


memb1(X,[X|_]) :-
	!.
memb1(X,[_|Xs]) :-
	memb1(X,Xs).
