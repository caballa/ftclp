% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%=======================================================================%
%               Operations with attributed variables
%=======================================================================%

:- module(op_attributes,
	[
	  extract_equalities_from_last_unification/4,
	  varset_attributes/2,
	  get_var_attribute/3,
	  % For debugging and/or dot output purposes
	  copy_term_with_attributes/2,
	  print_term_with_attribute/1,
	  print_term_with_attribute/2,
	  create_var_atom/2
	]).

%  Own libraries
:- use_module(debug).
%  Ciao libraries
:- use_module(engine(attributes)).
:- use_module(library(assoc)  , [empty_assoc/1, put_assoc/4, get_assoc/3]).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% '$solver_map'(Var, A, N, VarRef, Eqs )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% - Var is the logical variable
% - A is the identifier assigned to Var as an atom
% - N is the identifier assigned to Var as a number
% - VarRef is the solver variable to which Var is mapped.
% - Eqs contains CLP constraints where variables are of the form VarRef
%   that represent the last unification.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% HOOK: Unification of an attributed variable Var with a non-variable
% term
:- multifile verify_attribute/2.
verify_attribute('$solver_map'(Var, _K, _N, _VarRef, _Eqs), Value):-
	( var(Var) -> detach_attribute(Var) ; true ),
	Var = Value.
% HOOK: Unification of two attributed variables
:- multifile combine_attributes/2.
combine_attributes('$solver_map'(V1, K1, N1, VRef1, Eqs1), 
	           '$solver_map'(V2, _K2, _N2, VRef2, Eqs2)):-
	detach_attribute(V1),
	( V1 == V2 -> true ; detach_attribute(V2)),
 
	V1 = V2,
 
	combine_equalities(VRef1,VRef2,Eqs1,Eqs2,NewEqs),
 
	attach_attribute(V1,'$solver_map'(V1, K1, N1, VRef1, NewEqs)).

% TODO: we should use difference lists for making append constant.
combine_equalities(K1, K2, Eqs1, Eqs2, NewEqs):-
	K1 == K2, 
	!,
	append(Eqs1, Eqs2, NewEqs).
combine_equalities(K1, K2, Eqs1, Eqs2, NewEqs):- 
	!,
	append(Eqs1, [ '.=.'(K1,K2) | Eqs2], NewEqs).

%------------------------------------------------------------------------%
% extract_equalities_from_last_unification(+term, +list(num), -list, -list)
% extract_equalities_from_last_unification(Term, Indx, IndxEqs, RestEqs)
%------------------------------------------------------------------------%
% Split equations from Term into IndxEqs and RestEqs where IndxEqs
% contains the equations corresponding to the term arguments Indx
%------------------------------------------------------------------------%
extract_equalities_from_last_unification(Term, Indx, IndxEqs, RestEqs):-
	functor(Term,_,N),
	extract_equalities_from_args(Term, 1, N, Indx, [], IndxEqs, [], RestEqs).
extract_equalities_from_args(_,I,J,_Indx,IndxEqs,IndxEqs,RestEqs,RestEqs):-
	I > J, !.
extract_equalities_from_args(Term,I,J,Indx,IndxEqs0,IndxEqs2,RestEqs0,RestEqs2):-		
	arg(I,Term,X), var(X),
  	get_attribute(X,'$solver_map'(Y,K,N,VRef,Tmp)),
	!,
	detach_attribute(X),
	attach_attribute(X,'$solver_map'(Y,K,N,VRef,[])),
	( match_index(Indx,I) ->
	  append(IndxEqs0, Tmp, IndxEqs1),
	  RestEqs1 = RestEqs0,
	  pop_index(Indx, NewIndx)
	;
	  IndxEqs1 = IndxEqs0,
	  append(RestEqs0, Tmp, RestEqs1),
	  NewIndx = Indx
	),
	I1 is I + 1,
	extract_equalities_from_args(Term,I1,J,NewIndx,IndxEqs1,IndxEqs2, 
	                             RestEqs1,RestEqs2).
extract_equalities_from_args(Term,I,J,Indx,IndxEqs0,IndxEqs1,RestEqs0,RestEqs1):-	
	I1 is I + 1,
	extract_equalities_from_args(Term,I1,J,Indx,IndxEqs0,IndxEqs1,RestEqs0,RestEqs1).
	
match_index([X|_],I):- X == I, !.
pop_index([],[]). pop_index([_|T],T).

%-----------------------------------------------------------------------%
% varset_attributes(+Term, +Vs)
% Vs are the attributes of the variables in Term
%
% WARNING: this predicate does not traverse the term structure.
%-----------------------------------------------------------------------%
% Vs is in the same order that variables appear in Term's arguments
varset_attributes(Term, Vs):-
	functor(Term,_,N),
	varset_attributes_from_args(Term, N, 1, [], Vs).
varset_attributes_from_args(_, I, J, Vs, Vs):-
	I < J, !.
varset_attributes_from_args(Term, I, J, Vs0, Vs2):-		
	arg(I,Term,X), var(X),
  	get_attribute(X,'$solver_map'(_,_,_,VRef,_)),
	!,
	Vs1 = [VRef|Vs0],
	I1 is I - 1,
	varset_attributes_from_args(Term, I1, J, Vs1, Vs2).
varset_attributes_from_args(Term, I, J, Vs0, Vs1):-	
	I1 is I - 1,
	varset_attributes_from_args(Term, I1, J, Vs0, Vs1).

% get_var_attribute(+,+,-).
get_var_attribute(Term, Index, VRef):-
          arg(Index,Term,X),
          var(X),
          get_attribute(X,'$solver_map'(_,_,_,VRef,_)).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dot output  utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_var_atom(N,A):-
	atom_number(N_atm,N),
	atom_concat('v_',N_atm,A).

%-----------------------------------------------------------------------%
% copy_term_with_attributes(+term,-term)
% copy_term_with_attributes(+term,-term, -assoc)
% copy_term_with_attributes(+term,-term, +assoc, -assoc)
%-----------------------------------------------------------------------%
% Create a new copy of Term called NewTerm where all attributed
% variables are replaced with their corresponding attributes.
%-----------------------------------------------------------------------%
% Note: it is used currently only for dot output purposes
% This is important because it creates many atoms which can raise an
% "ERROR: the atom table is full" is the derivation tree is large.
%-----------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).

copy_term_with_attributes(Term, NewTerm):-
	copy_term_with_attributes(Term, NewTerm, _).

copy_term_with_attributes(Term, NewTerm, Bs1):- 
	empty_assoc(Bs0),
	copy_term_with_attributes(Term, NewTerm, Bs0, Bs1).

copy_term_with_attributes(V, K /*VRef*/, Bs0, Bs1):-
	var(V),
	( get_attribute(V,'$solver_map'(_,_K,N,_VRef/*VRef*/,_)) ->
	  % we create here an atom to represent the variable name on
	  % the fly.
	  create_var_atom(N,K),
	  ( get_assoc(K, Bs0, _) -> 
	    Bs1=Bs0
	  ;
	    put_assoc(K,Bs0,V,Bs1)
	  )
	;
	  % This can happen if we decide for some reason not to keep
	  % track of the variable. 
	  K = _  ,
	  Bs1 = Bs0	  
        ),
	!.
copy_term_with_attributes(A, A, Bs, Bs):- atomic(A), !.
copy_term_with_attributes(Term, Copy, Bs0, Bs1):-
	functor(Term,F,N),
	functor(Copy,F,N),
	copy_term_with_attributes_args(1,N,Term,Copy, Bs0, Bs1).
copy_term_with_attributes_args(I,N,_,_,Bs,Bs):- 
	I > N, 
	!.
copy_term_with_attributes_args(I,N,Term,Copy,Bs,Bs2):-	
	arg(I,Term,X),
	arg(I,Copy,Y),
	copy_term_with_attributes(X,Y,Bs,Bs1),
	J is I+1,
	copy_term_with_attributes_args(J,N,Term,Copy, Bs1, Bs2).
:- pop_prolog_flag(multi_arity_warnings).


%%%%%%%%%%%%%%%%%%%%
% Print utilities
%%%%%%%%%%%%%%%%%%%%
:- push_prolog_flag(multi_arity_warnings,off).
print_term_with_attribute(Term):- 
          current_output(Stream),
          print_term_with_attribute(Stream, Term).
% print a term replacing logical variables with its attribute (if any)
print_term_with_attribute(Stream, V):- 
	var(V),
          !,
	( get_attribute(V,'$solver_map'(_,_K,N,_,_)) ->
	  % we create here an atom to represent the variable name on
	  % the fly.	  
	  create_var_atom(N,K),
	  write(Stream,K)
	; 
	  % It is possible a variable does not have attributes if we
	  % don't want to reason about it.
	  write(Stream,V)
          ).
print_term_with_attribute(Stream, A):- 
	atomic(A), !, 
	write(Stream,A).
print_term_with_attribute(Stream, Term):- 
	functor(Term,F,N), N > 0,
	!, 
	write(Stream,F), write(Stream,'('),
	print_term_args_with_attribute(1,N,Stream,Term),
	write(Stream,')').
:- pop_prolog_flag(multi_arity_warnings).	
print_term_args_with_attribute(I,N,_,_):- I > N,  !.
print_term_args_with_attribute(I,N,Stream,Term):- 
	I = N, !,
	arg(I,Term,A),	  
	print_term_with_attribute(Stream, A).
print_term_args_with_attribute(I,N,Stream, Term):-
	I < N, !,
	arg(I,Term,A),	  
	print_term_with_attribute(Stream, A),
	write(Stream,','),
	J is I+1,
	print_term_args_with_attribute(J,N,Stream, Term).
