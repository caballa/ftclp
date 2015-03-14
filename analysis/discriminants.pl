% Author: Jorge. A Navas, The University of Melbourne 2012

% This module extracts from annotations which arguments are
% discriminants and which ones not. An argument is discriminant if it
% has impact on the control flow of the logic program.

:- module(discriminants, 
	[
	    clear_discriminants/0,  
	    process_predicates_discriminants/0, 
	    get_predicate_arg_discriminant/3,
	    unify_discriminating_args/2,
	    print_discriminants/0
	]).

%  Ciao libraries
:- use_module(library(format),[format/2]).

%=======================================================================%
% TODO: Ideally discriminant arguments should be inferred
% automatically by e.g., following "Non-discriminating Arguments and
% their Uses" by Christiansen and Gallagher, ICLP'09.
%=======================================================================%

%  From the program transformation done by tclp_tr
:- multifile discriminants/1.

% '$pred_discriminants'(F/A,Discriminants)
:- data '$pred_discriminants'/2.

clear_discriminants:-
	retractall_fact('$pred_discriminants'(_,_)).

process_predicates_discriminants:-
	discriminants(Pred),
	process_predicate_discriminants(Pred),
	fail.
process_predicates_discriminants.

process_predicate_discriminants(Pred):-
	functor(Pred,F,N),
	process_predicate_discriminants0(1,N,Pred,Discriminants),
	asserta_fact('$pred_discriminants'(F/N,Discriminants)).

process_predicate_discriminants0(I,N,_,[]):-
	I > N, 
	!.
process_predicate_discriminants0(I,N,Pred, [NTy|Tys]):-
	arg(I,Pred,Ty),
	valid_discr_arg(Ty,NTy),
	I1 is I + 1,
	process_predicate_discriminants0(I1,N,Pred,Tys).
	
% get_predicate_arg_discriminant(+F/A,+N,-Ty)
get_predicate_arg_discriminant(F/A,N,Ty):-
	'$pred_discriminants'(F/A,Args),
	!,
	arg(N,Args,Ty).

% Conservative: an argument is non-discriminating only user says so.
valid_discr_arg(X,nd)  :- X == nd, !.
valid_discr_arg(_,d) :- !.

%----------------------------------------------------------------------------%
% unify_discriminating_args(+Goal, -NewGoal)
%----------------------------------------------------------------------------%
% NewGoal is Goal but all non-discriminating arguments are replace
% with a free variable.
%----------------------------------------------------------------------------%
unify_discriminating_args(Goal, NewGoal):-
	functor(Goal   , F, N), 
	'$pred_discriminants'(F/N,Args),
	!,
	functor(NewGoal, F, N),
	unify_discriminating_args_aux(Args, 1, Goal,NewGoal).
unify_discriminating_args(Goal, Goal):- !.

unify_discriminating_args_aux([], _I, _, _).
unify_discriminating_args_aux([A|As], I, Goal,NewGoal):-
	A == nd, 
	!,
	NI is I + 1,
	unify_discriminating_args_aux(As, NI, Goal,NewGoal).
unify_discriminating_args_aux([_|As], I, Goal,NewGoal):-
	arg(I, Goal   , X),
	arg(I, NewGoal, X),
	NI is I + 1,
	unify_discriminating_args_aux(As, NI, Goal,NewGoal).
	
% print_discriminants/0: display types if available.
print_discriminants:-
	'$pred_discriminants'(F/A,Args),
	format(":- discriminant ~q",[F]),
	( A =:= 0 -> format(".\n",[])
	;
	  format("(",[]),
	  print_pred_arg_discriminants(1,A,Args),
	  format(").\n",[])
	),
        fail.
print_discriminants.

print_pred_arg_discriminants(I,N,_):- 
	I > N, 
	!.
print_pred_arg_discriminants(I,N,Types):- 
	I =:= N, 
	!,
	arg(N,Types,Ty),
	print_valid_discriminant(Ty).	
print_pred_arg_discriminants(I,N,Types):-
	I < N,
	!,
	arg(I,Types,Ty),
	print_valid_discriminant(Ty),
	format(",",[]),
	I1 is I+1,
	print_pred_arg_discriminants(I1,N,Types).

print_valid_discriminant(X):- X==nd, !, format("nd",[]).
print_valid_discriminant(_):- !, format("d",[]).

