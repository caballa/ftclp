% Author: Jorge. A Navas, The University of Melbourne 2012

%=======================================================================%
% Gather type information from user annotations.
%=======================================================================%

:- module(types,
	[
	    clear_types/0,  
	    process_predicates_types/0, 
	    get_predicate_arg_type/3,
	    print_types/0
	]).

%  Ciao libraries
:- use_module(library(format),[format/2]).

%  From the program transformation done by tclp_tr
:- multifile tabled/1.

% '$pred_types'(F/A,Types)
:- data '$pred_types'/2.
clear_types:-
	retractall_fact('$pred_types'(_,_)).

process_predicates_types:-
	tabled(Pred),
	process_predicate_types(Pred),
	fail.
process_predicates_types.

process_predicate_types(Pred):-
	functor(Pred,F,N),
	process_predicate_types0(1,N,Pred,Types),
	T =.. [types|Types],
	asserta_fact('$pred_types'(F/N,T)).

process_predicate_types0(I,N,_,[]):-
	I > N, 
	!.
process_predicate_types0(I,N,Pred, [NTy|Tys]):-
	arg(I,Pred,Ty),
	( valid_type(Ty,NTy) -> true 
	; 
	  format("ERROR ftclp: unrecognized type ~q.\n",[Ty]), 
	  halt
	),
	I1 is I + 1,
	process_predicate_types0(I1,N,Pred,Tys).
	
% get_predicate_arg_type(+F/A,+N,-Ty)
get_predicate_arg_type(F/A,N,Ty):-
	'$pred_types'(F/A,Args),
	!,
	arg(N,Args,Ty).
% here the type by default!
get_predicate_arg_type('.=.'/2 , _, num).
get_predicate_arg_type('.<>.'/2, _, num).
get_predicate_arg_type('.>.'/2 , _, num).
get_predicate_arg_type('.>=.'/2, _, num).
get_predicate_arg_type('.<.'/2 , _, num).
get_predicate_arg_type('.=<.'/2, _, num).
get_predicate_arg_type('+'/2   , _, num).
get_predicate_arg_type('-'/2   , _, num).
get_predicate_arg_type('*'/2   , _, num).
get_predicate_arg_type('/'/2   , _, num).
get_predicate_arg_type(_       ,_ , unk):- !.

% it will ignored by the solver.
valid_type(X,unk)  :- var(X).
% it will be translated to real or integer in the solver depending
% whether -integer-aritmethic is enabled or not.
valid_type(X,num)  :- X == num.
% other types
valid_type(X,real) :- X == real.
valid_type(X,int)  :- X == int.

%--------------------------------------------------------------------------%
%                         Printing utilities
%--------------------------------------------------------------------------%

% print_types/0: display types if available.
print_types:-
	'$pred_types'(F/A,Args),
	format(":- type ~q",[F]),
	( A =:= 0 -> format(".\n",[])
	;
	  format("(",[]),
	  print_pred_arg_types(1,A,Args),
	  format(").\n",[])
	),
        fail.
print_types.

print_pred_arg_types(I,N,_):- 
	I > N, 
	!.
print_pred_arg_types(I,N,Types):- 
	I =:= N, 
	!,
	arg(N,Types,Ty),
	print_valid_type(Ty).	
print_pred_arg_types(I,N,Types):-
	I < N,
	!,
	arg(I,Types,Ty),
	print_valid_type(Ty),
	format(",",[]),
	I1 is I+1,
	print_pred_arg_types(I1,N,Types).

print_valid_type(X):- var(X), format("*",[]).
print_valid_type(X):- X==num, format("num",[]).
