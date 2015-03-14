% Author: Jorge. A Navas, The University of Melbourne 2013

% This file defines properties that a program must fulfill to be
% considered well-formed.

:- module(well_formed, 
        [
	  is_linear/2,
	  not_redefine_builtins/1
        ]).

% Own libraries
:- use_module('../analysis/scc').
:- use_module(builtins).
% Ciao libraries
:- use_module(library(format), [format/2]).


%--------------------------------------------------------------------------------%
% is_linear(+,+)
%--------------------------------------------------------------------------------%
% Succeed if the program is linear recursive and only the last body
% literal is recursive.
%--------------------------------------------------------------------------------%
is_linear([], _SCCs).
is_linear([proc(F/A, Cls) | Procs], SCCs):-
        % F/A is recursive
        find_scc_pred(SCCs, F/A, SCC),
        !,
        is_linear_clause(Cls, F/A, 1, SCC),
        is_linear(Procs, SCCs).
is_linear([_|Procs], SCCs):-
        % F/A is not recursive
        is_linear(Procs, SCCs).        

is_linear_clause([], _, _, _).
is_linear_clause([clause((_:-B), _Vs) | Cls], F/A, K, SCC):-
        is_linear_body(B, F/A/K, SCC),
        K1 is K + 1,
        is_linear_clause(Cls, F/A, K1, SCC).
        
is_linear_body(true, _ClId, _SCC ).
is_linear_body((B,Bs), F/A/K, SCC):-
        functor(B, BF, BA),  
        ( member(BF/BA, SCC) ->
	format("ERROR ftclp: only last body literal can be recursive.\n",[]),
	format("      ~q in ~q clause ~q\n",[B,F/A,K]),
	halt
        ; 
	% fail here means that B is either nonrecursive or it's
	% defined in a different SCC
          true
        ),
        is_linear_body(Bs, F/A/K, SCC).
is_linear_body(_B, _ClId, _SCCs).

%--------------------------------------------------------------------------------%
% not_redefine_builtins(+)
%--------------------------------------------------------------------------------%
% Succeed if not builtin is redefined in the program.
%--------------------------------------------------------------------------------%
not_redefine_builtins([]).
not_redefine_builtins([proc(F/A,Cls)|_]):-
        sp_builtin_key(F/A),        
        Cls \== [], 
        !,
        format("ERROR ftclp: builtin ~q cannot be redefined.\n",[F/A]),
        halt.        
not_redefine_builtins([_|Procs]):- not_redefine_builtins(Procs).
