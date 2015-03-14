% Author: J. Gallagher 08/08/2006. (c) Roskilde University.

:- module(readprog, 
	[readprog/2, 
	 readprog/3,
	 % For scc
	 sortClauses/3,
	 user_clauses/3
	]).

%  Ciao libraries
:- use_module(library(lists)).
:- use_module(library(read)).
:- use_module(library(write)).

:- include(ops).

%-------------------------------------------------------------------------%
% 
% Limitations.  Input program consists of definite clauses.  Doesn't
%               handle metagoals like if-then-else, disjunction, bagof
%               etc.  Maybe the ciaopp program parser will enable this
%               later.
% Usage 1
% readprog(+File,-Program).
% +File      a filename containing the program to be transformed
% -Program   a list containing the program clauses.
%            - first element of list a term predicates(Ps) where Ps
%              is a list of the predicates in the transformed program.
%            - remaining elements, terms clause((H :- B), Vs) where H:-B
%              is a clause, Vs is a binding list with the
%              original variable names.
%           
%
% Example query (using naive reverse program)
%
%       ?- readprog('rev.pl', Cls).
%
% Ps = [predicates([rev/2,app/3]),
%       cl((rev([],[]):-true),[]),
%       cl((rev([_B|_C],_D):-rev(_C,_A),app(_A,[_B],_D)),
%              ['Ws'=_A,'X'=_B,'Xs'=_C,'Ys'=_D]),
%       cl((app([],_E,_E):-true),['Ys'=_E]),
%       cl((app([_F|_G],_H,[_F|_I]):-app(_G,_H,_I)),
%              ['X'=_F,'Xs'=_G,'Ys'=_H,'Zs'=_I])] 
%-------------------------------------------------------------------------%

:- dynamic(myClause/3).

%:- op(1150, fx, entry).
%:- op(1150, fx, residual).
%:- op(1150, fx, filter).
%:- op(750 , fx, type).


:- push_prolog_flag(multi_arity_warnings,off).
readprog(F,Prog, Dirs) :-
	open(F,read,Stream),
	readprog1(Stream,  _ClauseCount, Prog, Dirs),
	close(Stream).

readprog(F,Prog) :-
	open(F,read,Stream),
	readprog1(Stream,  _ClauseCount, Prog, _Dirs),
	close(Stream).
	%% output info
 	%write(user_output,'Number of clauses: '), 
 	%write(user_output,ClauseCount),
 	%nl(user_output).
:- pop_prolog_flag(multi_arity_warnings).
	
% Return the list of terms in the file
readterms(F,Ts) :-
	open(F,read,Stream),
	readterms1(Stream,  _ClauseCount, Ts),
	close(Stream).
	
readterms1(Stream, Clausecount,Ts) :-
	read(Stream, C),
	readMoreTerms(Stream, C,0, Clausecount,Ts),
	!.
readterms1(_,0,[]) :-
	write(user_error,'Problems while reading file '), 
	nl(user_error).
	
readMoreTerms(_,end_of_file,N,N,[]) :-
	!.
readMoreTerms(S,C,N0,N2,[C|Ts]) :-
	read(S,C1),
	N1 is N0+1,
	readMoreTerms(S,C1,N1,N2,Ts).


readprog1(Stream, Clausecount,[predicates(Ps)|Prog], Dirs) :-
	read_clause(Stream, C),
	read_clauses(Stream, C,Ps,0, Clausecount,Prog, Dirs),
	!.
readprog1(_,0,[],_) :-
	write(user_error,'Problems while reading file '), 
	nl(user_error).


read_clauses(_,cl(end_of_file,_),Ps,N,N,[],[]) :- 
	!,
	close_list(Ps).
% 	write(user_output,'Number of predicates: '), 
% 	length(Ps, PN),
% 	write(user_output,PN),
% 	nl(user_output).
% read_clauses(Stream,cl((:- module(_,_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- module(_,_,_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- push_prolog_flag(_,_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- pop_prolog_flag(_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- include(_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- use_module(_,_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- use_module(_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- multifile _),_),Ps,N,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,N,M,Out).
% read_clauses(Stream,cl((:- dynamic(_)),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- dynamic _),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).
% read_clauses(Stream,cl((:- data _),_),Ps,K,M,Out) :-
% 	!,
% 	read_clause(Stream,C1),
% 	read_clauses(Stream,C1,Ps,K,M,Out).

read_clauses(Stream,cl((:- Dir),_),Ps,N,M,Out,[Dir|Dirs]) :- 
	read_clause(Stream,C1),
	!,
	read_clauses(Stream,C1,Ps,N,M,Out, Dirs).
read_clauses(Stream,cl((H :- B),Vs),Ps,N,M,[clause((H :- B),Vs)|Out],Dirs) :- 
	!,
	canonical((H:-B)),
	get_pred_name((H :- B),Pred,Bodypreds),
	each_memb1([Pred|Bodypreds],Ps),
	N1 is N+1,
	read_clause(Stream,C1),
	!,
	read_clauses(Stream,C1,Ps,N1,M,Out,Dirs).
read_clauses(Stream,cl(H,Vs),Ps,N,M,[clause((H :- true),Vs)|Out],Dirs) :- 
	!,
	canonical(H),
	get_pred_name((H :- true),Pred,Bodypreds),
	each_memb1([Pred|Bodypreds],Ps),
	N1 is N+1,
	read_clause(Stream,C1),
	!,
	read_clauses(Stream,C1,Ps,N1,M,Out,Dirs).
read_clauses(_,_,_,_,_,[],_) :-
	write(user_error,'Error reading program.'),
	nl(user_error).



get_pred_name((H :- B),P/N,BPs) :-
	!,
	functor(H,P,N),
	body_preds(B,BPs).
get_pred_name(H ,P/N,[]) :-
	functor(H,P,N).

body_preds(true,[]) :-
	!.
body_preds((\+ B,Bs),[(\+)/1|Ps]) :-
	!,
	body_preds((B,Bs),Ps).
body_preds((B,Bs),[P/N|Ps]) :-
	!,
	functor(B,P,N),
	body_preds(Bs,Ps).
body_preds(\+ B,[(\+)/1|Ps]) :-
	!,
	body_preds(B,Ps).
body_preds(B,[P/N]) :-
	functor(B,P,N).

each_memb1([],_).
each_memb1([P|Ps],S) :-
	memb1(P,S),
	each_memb1(Ps,S).
	
memb1(X,[X|_]) :-
	!.
memb1(X,[_|Xs]) :-
	memb1(X,Xs).


file_suffix([],X,X).
file_suffix([X|Xs],Ys,[X|Zs]) :-
	file_suffix(Xs,Ys,Zs).

close_list([]) :-
	!.
close_list([_|X]) :-
	close_list(X).

read_clause(S,cl(C,Vs)) :-
	read_term(S,C,[variable_names(Vs)]).
% NOTE: read_term/3 does not ready properly (test ->S1;S2) because
% parenthesis are omitted. Neither (S1;S2) for the same reason.

joingoals1(true,Xs,Xs) :-
	!.
joingoals1(Xs,true,Xs) :-
	!.
joingoals1((true,Xs),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((Xs,true),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((X,Xs),Ys,(X,Zs)) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1(X,Xs,(X,Xs)) :-
	X =.. [F|_],
	F \== ','.

sortClauses([predicates(Ps)|Cls], Ps,Procs) :-
	initProcs(Ps,Procs0),
	buildProcs(Cls,Procs0,Procs).

initProcs([],[]).
initProcs([P/N|Ps], [proc(P/N,[])|Procs]) :-
	initProcs(Ps,Procs).
	
buildProcs([],Pr,Pr).
buildProcs([clause((H :- B), Vs)|Cls], Procs0, Procs2) :-
	functor(H,P,N),
	insertClause(Procs0,P/N,H,B,Vs,Procs1),
	buildProcs(Cls, Procs1, Procs2).
	
insertClause([proc(Pred,Cls)|Procs0],Pred,H,B,Vs,[proc(Pred,Cls1)|Procs0]) :-
	!,
	append(Cls,[clause((H :- B), Vs)],Cls1).
insertClause([Proc|Procs0],Pred,H,B,Vs,[Proc|Procs1]) :-
	insertClause(Procs0,Pred,H,B,Vs,Procs1).
	
assertProg([_|Cls]) :-
	assertClauses(Cls).
	
assertClauses([]).
assertClauses([clause((H :- B),Vs)|OutProg]) :-
	assertz_fact(myClause(H,B,Vs)),
	assertClauses(OutProg).

user_clauses([],_,[]).
user_clauses([proc(P/N,Cls)|_],P/N,Cls1) :-
	!,
	returnCls(Cls,Cls1).
user_clauses([_|Prcs],P/N,Cls) :-
	user_clauses(Prcs,P/N,Cls).

returnCls([],[]).
returnCls([clause(C,_)|Cls],[C|Cls1]) :-
	returnCls(Cls,Cls1).

getPreds([clause(Cl,_)|Cls],Qs) :-
	get_pred_name(Cl,P,Ps),
	each_memb1([P|Ps],Qs),
	getPreds(Cls,Qs).
getPreds([],S) :-
	close_list(S).	

getPredClauses([], _ , []).
getPredClauses([predicates(_)|Cls],Key,Cls0):-
	getPredClauses(Cls,Key,Cls0).

getPredClauses([clause( (H :- B), Vs) | Cls], Key,[clause( (H:-B),Vs)|Cls0]):-
	get_pred_name((H :- B),Pred,_Bodypreds),
	Pred == Key,
	!,
	getPredClauses(Cls,Key,Cls0).
getPredClauses([ _ | Cls], Key,Cls0):-
	getPredClauses(Cls,Key,Cls0).

canonical(T) :-
	numbervars(T,0,_).

