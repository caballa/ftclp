% Author: J. Gallagher 08/08/2006.(c) Roskilde University.
% Some extensions done by J.Navas on 28/08/12.
:- module(union_find,
	[ % Standard union-find operations
	  unionfind_make/1,
	  unionfind_make/2,
	  unionfind_make/4,
	  unionfind_find/5,
	  unionfind_findSimple/4,	  
	  unionfind_merge/5,
	  % Nonstandard union-find operations
	  unionfind_add_and_merge/4,
	  unionfind_replace/4,
	  unionfind_to_list/2
	]).

:- use_module(balanced_tree).
:- use_module(library(sets), [ ord_union/3]).


% Small test:
% ?- unionfind_make(Es0), unionfind_make(a,[a],Es0,Es1), 
%    unionfind_make(b,[b],Es1,Es2), unionfind_merge(_,a,b,Es2,Es3,_),  
%    unionfind_replace(b,[],Es3,Es4).


unionfind_createData([]).
unionfind_mergeData(Ts1,Ts2,Ts3):- ord_union(Ts1,Ts2,Ts3).


%----------------------------------------------------------------------%
% unionfind_find(+N,+Key,+S,-S1,-Val).
%----------------------------------------------------------------------%
unionfind_find(_N,T,Es0,Es2,R) :-
	search_replace_tree(Es0,T,PT,Es1,R1),
	%N1 is N+1,
	checkRoot(_N1,PT,T,R,R1,Es1,Es2).
	
checkRoot(_N1,data(T,Fs),T,data(T,Fs),data(T,Fs),Es,Es) :-
	%write(N1), write('steps for find '), nl,
	!.
checkRoot(_N,data(PT,_),_,data(R,Fs),data(R,Data),Es0,Es1) :-
	% N1 is N+1,
	unionfind_createData(Data),
	unionfind_find(_N1,PT,Es0,Es1,data(R,Fs)).

%----------------------------------------------------------------------%
% unionfind_findSimple(+N,+Key,+S,-Val).
%----------------------------------------------------------------------%
unionfind_findSimple(_N,T,Es0,R) :-
	search_tree(Es0,T,PT),
	%N1 is N+1,
	checkRootSimple(_N1,PT,T,R,Es0).
	
checkRootSimple(_N1,data(T,Fs),T,data(T,Fs),_) :-
	%write(N1), write('steps for simple find '), nl,
	!.
checkRootSimple(_N,data(PT,_),_,R,Es0) :-
	% N1 is N+1,
	unionfind_findSimple(_N1,PT,Es0,R).


:- push_prolog_flag(multi_arity_warnings,off).
%----------------------------------------------------------------------%
% unionfind_make(+Keys,-S0)
%----------------------------------------------------------------------%
unionfind_make(Xs,Es) :- 
	unionfind_make_aux(Xs,root,Es).

unionfind_make_aux([],Es,Es).
unionfind_make_aux([X|Xs], Es0, Es2) :-
	unionfind_createData(Data),
	insert_tree(Es0,X,data(X,Data),Es1),
	unionfind_make_aux(Xs,Es1,Es2).

%----------------------------------------------------------------------%
% unionfind_make(+Key,+Val,+S0,-S1)
%----------------------------------------------------------------------%
unionfind_make(X, Data, Es0, Es1) :-
	insert_tree(Es0,X,data(X,Data),Es1).

%----------------------------------------------------------------------%
% unionfind_make(-S0)
%----------------------------------------------------------------------%	
unionfind_make(root).
:- pop_prolog_flag(multi_arity_warnings).

%----------------------------------------------------------------------%
% unionfind_merge(+Key1,+Key2,+S0,-S1,-MergedVal)
%----------------------------------------------------------------------%	
unionfind_merge(X,Y,Es0,Es4,data(RX,Ts)) :-
	unionfind_find(0,X,Es0,Es1,data(RX,TsX)),
	unionfind_find(0,Y,Es1,Es2,data(RY,TsY)),
	!,
	unionfind_mergeData(TsX,TsY,Ts),
	unionfind_createData(Data),
	search_replace_tree(Es2,RY,_,Es3,data(RX,Data)),
	search_replace_tree(Es3,RX,_,Es4,data(RX,Ts)).

%----------------------------------------------------------------------%
% unionfind_add(+Key,+Val,+S0,-S1)
% Add Val by ***merging*** with old value of Key.
%----------------------------------------------------------------------%	
unionfind_add_and_merge(K,NewData,Es0,Es1) :-
	unionfind_findSimple(_,K,Es0,data(RK,_)),
	search_replace_tree(Es0,RK,OldV,Es1,NewV),
	OldV = data(RK,OldData),
	unionfind_mergeData(OldData,NewData,Data),
	NewV = data(RK,Data).


%----------------------------------------------------------------------%
% unionfind_replace(+Key,+Val,+S0,-S1)
% Replace old value of Key with Val. If Key is not in S0 then we add a
% new equivalence class for Key with value Val.
%----------------------------------------------------------------------%	
unionfind_replace(K,NewData,Es0,Es1) :-
	( unionfind_findSimple(_,K,Es0,data(RK,_)) ->
	  search_replace_tree(Es0,RK,OldV,Es1,NewV),
	  OldV = data(RK,_OldData),
	  NewV = data(RK,NewData)
	;
	  unionfind_make(K, NewData, Es0, Es1)
          ).
	    
unionfind_to_list(Es, L):- traverse_tree(Es, L).
        