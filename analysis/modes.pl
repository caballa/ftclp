% Author: Jorge. A Navas, The University of Melbourne 2012

:- module(modes,
	[
	  clear_modes/0,
	  gather_modes/0,
	  % 
	  get_predicate_modes/3
	]).

%  System libraries
:- use_module(library(sort), [sort/2]).

%=======================================================================%
% Gather information from user annotations.
% TODO: Ideally modes should be inferred automatically.
%=======================================================================%

%  From the program transformation done by tclp_tr
:- multifile mode/1.

:- data '$input'/1.
:- data '$output'/1.

:- data '$input_args'/2.
:- data '$output_args'/2.

clear_modes:-
	retractall_fact('$input'(_)),
	retractall_fact('$output'(_)),
	retractall_fact('$input_args'(_,_)),
	retractall_fact('$output_args'(_,_)).

gather_modes:-
	mode(Goal),
	gather_modes_aux(Goal),
	fail.
gather_modes.

gather_modes_aux(Goal):-
	functor(Goal,F,N),
	Goal =.. [_|Modes],
	gather_modes_aux0(Modes,1,F/N).

gather_modes_aux0([],_,_):-!.
gather_modes_aux0([M|Ms],I,Key):-
	M == in,
	!,
	asserta_fact('$input'(Key/I)),
	I1 is I +1,
	gather_modes_aux0(Ms,I1,Key).
gather_modes_aux0([M|Ms],I,Key):-
	M == out,
	!,
	asserta_fact('$output'(Key/I)),
	I1 is I +1,
	gather_modes_aux0(Ms,I1,Key).
gather_modes_aux0([_M|Ms],I,Key):-
	!,
	I1 is I +1,
	gather_modes_aux0(Ms,I1,Key).

% get_predicate_modes(+Flag,+F/A,-InputIndexes).
get_predicate_modes(in,F/A,S):-
	current_fact('$input_args'(F/A,S)),
	!.
get_predicate_modes(in,F/A,S):-
	findall(I,'$input'(F/A/I),S_u),
	sort(S_u,S),
	asserta_fact('$input_args'(F/A,S)),
	!.
get_predicate_modes(out,F/A,S):-
	current_fact('$output_args'(F/A,S)),
	!.
get_predicate_modes(out,F/A,S):-
	findall(I,'$output'(F/A/I),S_u),
	sort(S_u,S),
	asserta_fact('$output_args'(F/A,S)),
	!.

	
