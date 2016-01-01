% Author: Jorge. A Navas, The University of Melbourne 2012

%=======================================================================%
%                       Process user options
%=======================================================================%

:- module(options, 
	[ is_enabled_option/1,
	  enable_option/1,
	  process_options/1,
	  cleanup_user_flags/0,
	  % if running from command-line
	  read_and_process_user_options/5
	]).

% Ciao libraries
:- use_module(library(format), [format/2]).
:- use_module(library(system), [delete_file/1]).

% Output derivation tree in dot format
:- data '$dot_output'/0.
% SMT over integer linear arithmetic 
:- data '$integer_arithmetic'/0.
% To reduce the number of interpolation calls
:- data '$minimize_intps'/0.
% Compute interpolants but without considering whether they can be
% out-of-scope.
:- data '$unscoped_interpolation'/0.
% Compute interpolants but ensure variables are in scope.
:- data '$scoped_interpolation'/0.
% To subsume clauses 
:- data '$clause_subsumption'/0.
% To susbume predicates
:- data '$predicate_subsumption'/0.
% To susbume cycles
:- data '$cycle_subsumption'/0.
% Debugging messages
:- data '$debugging'/0.
% No verbose mode
:- data '$no_verbose'/0.
 
% Number of solutions
:- data '$solutions'/1.
% Minimum loop unrolling 
:- data '$min_unroll'/1.
% Maximum loop unrolling 
:- data '$max_unroll'/1.
% Number of interpolants for recursive calls along the same path
:- data '$intp_rec_calls'/1.
% Order for getting the first K interpolants for recursive calls along
% the same path. The parameter K is defined by '$intp_rec_calls'/1.
:- data '$intp_rec_calls_order'/1.
% To show answers
:- data '$print_answers'/0.
% To dump interpolants into a file
:- data '$dump_interpolants'/0.
% To use a disjunctive memo table for subsumption.
:- data '$disj_memo'/0.

cleanup_user_flags:-
	retractall_fact('$debugging'),	
	retractall_fact('$no_verbose'),	
	retractall_fact('$dot_output'),
	retractall_fact('$unscoped_interpolation'),
	retractall_fact('$scoped_interpolation'),
	retractall_fact('$clause_subsumption'),
	retractall_fact('$predicate_subsumption'),
	retractall_fact('$cycle_subsumption'),
 
	retractall_fact('$solutions'(_)),
	retractall_fact('$min_unroll'(_)),
	retractall_fact('$max_unroll'(_)),
	retractall_fact('$intp_rec_calls'(_)),
	retractall_fact('$intp_rec_calls_order'(_)),
	retractall_fact('$disj_memo').

write_if_no_verbose(_Msg,_Args):-
        is_enabled_option(no_verbose),
        !.
write_if_no_verbose(Msg,Args):-
        format(Msg,Args).

display_header:-
	write('  Failure Tabled Constraint Logic Programming by Interpolation'),nl,
	write('  Authors: G. Gange, J.A. Navas, P. Schachte, H. Sondergaard, and P.J. Stuckey.'),nl,
	write('  (C)2013 The University of Melbourne.'),nl,
	write('  Description: CLP interpreter with failure-based tabling.'),nl,nl.

display_options:-
	display_header,
          write('  Usage: cmmd -goal <G> -f  <input_file> [options] '), nl, 
	write('         G must be a single CLP term between single quotes (e.g., \'foo(X)\') '),nl,
	write('         For multiple goals G1,...,Gn wrap them into a predicate '),nl,
	write('         wrap(...) :- G1,...,Gn. and call with -goal \'wrap(...)\''),nl,nl,
	write('  Options: '),nl,
	write('   -help, --help      : display this list of options'),nl,
	write('   -debug             : debug mode'),nl,	
	write('   -dot-output        : display derivation tree in dot format'),nl,
	write('   -show-answers      : show answers'),nl,	
	write('   -dump-interpolants : write interpolants into <input_file>.intp'),nl,	
	write('   -depth     <n>     : explore up to depth n  (default unlimited)'),nl,
	write('   -solutions <n>     : compute n solutions    (default all solutions)'),nl,
	write('   -integer-arithmetic: interpret all constraints over integer arithmetic (default reals)'), nl,
	write('   -clause-pruning    : pruning at the level of clause (default no pruning)'),nl,
	write('   -pred-pruning      : pruning at the level of predicate (default no pruning)'),nl,
	write('   -infinite-pruning  : pruning infinite derivations (default no pruning)'),nl,
	write('\t-min-unroll <n>: minimum number of unrollings for recursive clauses '),nl,
	write('                           before attempting child-parent subsumption (default 1)'),
	nl,
	write('\t-max-unroll <n>: maximum number of unrollings for recursive clauses (default unlimited)'),
	nl,
	write('   -minimize-intp-calls: optimization to minimize the number of interpolation calls'),nl,
          write('                         (only if interpolants are inductive)'),nl,
	write('   -unscoped-intp      : generation of not necessarily well scoped interpolants'), nl,
	write('   -scoped-intp        : generation of well scoped interpolants'), nl,
 
	nl,
	!.

% ArgVs is a list of atoms
read_and_process_user_options(ArgVs, Entry, InputFile, Depth, NewOptions):-
	get_options(ArgVs,Options,_Values),
	( member(help, Options) -> 
	  display_options
	;
	  process_user_options(Options, NewOptions, Entry, InputFile, Depth)
	),
	!.
	
get_options([],[],[]).
get_options([X|T],Options,Args) :-
        (recognised_option(X,Opt,Values) ->
	 ( append(Values, Rest, T),
	   RT = Rest,
	   Options = [Opt|OT], Args = AT
	 )
	;
	   format("ftclp error: unrecognized user option ~q\n",[X]),
	   fail
	   % (
	   %    Options = OT,     Args = [X|AT],
	   %    RT = T
	   % )
	),
	get_options(RT,OT,AT).

recognised_option('-help'               , help                   , []).
recognised_option('--help'              , help                   , []).
recognised_option('-debug'              , debug                  , []).
recognised_option('-no-verbose'         , no_verbose             , []).
recognised_option('-dot-output'         , dot_output             , []).
recognised_option('-depth'              , depth(N)               , [N]).
recognised_option('-solutions'          , solutions(N)           , [N]).
recognised_option('-integer-arithmetic' , integer_arithmetic     , []).
recognised_option('-show-answers'       , print_answers          , []).
recognised_option('-dump-interpolants'  , dump_interpolants      , []).
recognised_option('-f'                  , input_file(F)          , [F]).
recognised_option('-goal'               , entry(G)               , [G]).
recognised_option('-pred-pruning'       , pred_subsumption       , []).
recognised_option('-infinite-pruning'   , cycle_subsumption      , []).
recognised_option('-min-unroll'         , min_unroll(N)          , [N]).
recognised_option('-max-unroll'         , max_unroll(N)          , [N]).
recognised_option('-clause-pruning'     , clause_subsumption     , []).
recognised_option('-unscoped-intp'      , unscoped_interpolation , []).
recognised_option('-scoped-intp'        , scoped_interpolation   , []).
 
recognised_option('-disjunctive-intp'   , disj_memo              , []).
recognised_option('-minimize-intp-calls', minimize_intps          , []).

process_user_options([],[],_,_,_):- !.
process_user_options([depth(A)| Xs], Ys, T, F, N):-
	!,
	atom_number(A,N),
	process_user_options(Xs, Ys, T, F, _).
process_user_options([solutions(A)| Xs], [solutions(N)|Ys], T, F, D):-
	!,
	atom_number(A,N),
	process_user_options(Xs,Ys, T, F, D).
process_user_options([min_unroll(A)| Xs], [min_unroll(N)|Ys], T, F, D):-
	!,
	atom_number(A,N),
	process_user_options(Xs,Ys, T, F, D).
process_user_options([max_unroll(A)| Xs], [max_unroll(N)|Ys], T, F, D):-
	!,
	atom_number(A,N),
	process_user_options(Xs,Ys, T, F, D).
process_user_options([entry(A)| Xs], Ys, T, F, D):-
	!,
	TmpFile = 'tmp___xxx',
	open(TmpFile,write,Out),
	write(Out, A),
	write(Out,'.'),
	nl(Out),
	close(Out),
	open(TmpFile,read,In),
	read(In, T),
	close(In),
	delete_file(TmpFile),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	process_user_options(Xs,Ys,_,F, D).
process_user_options([input_file(F)| Xs], Ys, T,F,D):-
	!,
	process_user_options(Xs,Ys,T,_,D).
process_user_options([X| Xs], [X|Ys], T,F,D):-
	!,
	process_user_options(Xs,Ys,T,F,D).

%-------------------------------------------------------------------------%
is_enabled_option(integer_arithmetic):-
	current_fact('$integer_arithmetic').
is_enabled_option(minimize_intps):-
	current_fact('$minimize_intps').
is_enabled_option(scoped_interpolation) :- 
	current_fact('$scoped_interpolation').
is_enabled_option(unscoped_interpolation) :- 
	current_fact('$unscoped_interpolation').
is_enabled_option(clause_subsumption)  :-
	current_fact('$clause_subsumption').
is_enabled_option(pred_subsumption)  :-
	current_fact('$predicate_subsumption').
is_enabled_option(cycle_subsumption)  :-
	current_fact('$cycle_subsumption').
is_enabled_option(dot_output)      :- 
	current_fact('$dot_output').
is_enabled_option(debug)              :- 
	current_fact('$debugging').
is_enabled_option(no_verbose)              :- 
	current_fact('$no_verbose').
 
is_enabled_option(solutions(N))  :- 
	current_fact('$solutions'(N)).
is_enabled_option(min_unroll(N))  :- 
	current_fact('$min_unroll'(N)).
is_enabled_option(max_unroll(N))  :- 
	current_fact('$max_unroll'(N)).
is_enabled_option(intp_rec_calls(N)):-
	current_fact('$intp_rec_calls'(N)).
is_enabled_option(intp_rec_calls_order(O)):-
	current_fact('$intp_rec_calls_order'(O)).
is_enabled_option(print_answers):-
	current_fact('$print_answers').
is_enabled_option(dump_interpolants):-
	current_fact('$dump_interpolants').
is_enabled_option(disj_memo):-
	current_fact('$disj_memo').

is_enabled_option(interpolation):- 
        is_enabled_option(unscoped_interpolation).
is_enabled_option(interpolation):- 
        is_enabled_option(scoped_interpolation).

enable_option(Flag) :- is_enabled_option(Flag), !.
enable_option(Flag) :- enable_option_(Flag).
          

enable_option_(dot_output)          :- 
	asserta_fact('$dot_output').
enable_option_(debug)               :- 
	asserta_fact('$debugging').
enable_option_(no_verbose)          :- 
	asserta_fact('$no_verbose').
enable_option_(integer_arithmetic) :- 
	asserta_fact('$integer_arithmetic'),
	write_if_no_verbose("ftclp: user selected SMT over integer linear arithmetic.\n",[]).
enable_option_(minimize_intps) :- 
	asserta_fact('$minimize_intps'),
	write_if_no_verbose("ftclp: user selected optimization to minimize number of interpolants.\n",[]).
enable_option_(unscoped_interpolation) :- 
	asserta_fact('$unscoped_interpolation'),
	write_if_no_verbose("ftclp: user selected interpolants that may not be well scoped.\n",[]).
enable_option_(clause_subsumption) :-
	asserta_fact('$clause_subsumption'),
	write_if_no_verbose("ftclp: user selected pruning at the level of clause.\n",[]).
enable_option_(pred_subsumption) :-
	asserta_fact('$predicate_subsumption'),
	write_if_no_verbose("ftclp: user selected pruning at the level of predicate.\n",[]).	
enable_option_(cycle_subsumption) :-
	asserta_fact('$cycle_subsumption'),
	write_if_no_verbose("ftclp: user selected pruning of infinite derivations.\n",[]).	
enable_option_(scoped_interpolation):- 
	asserta_fact('$scoped_interpolation'),
	write_if_no_verbose("ftclp: user selected well scoped interpolants.\n",[]).
 
enable_option_(solutions(N)) :-
	( number(N) ->  true 
	; 
	  format("ERROR ftclp: the argument of solutions must be a number.\n",[]), halt
	),
	asserta_fact('$solutions'(N)),
	write_if_no_verbose("ftclp: user selected only the first ~q solutions.\n",[N]).
enable_option_(min_unroll(N)) :-
	( number(N) ->  true 
	; 
	  format("ERROR ftclp: the argument of min-unroll must be a number.\n",[]), halt
	),
	asserta_fact('$min_unroll'(N)).
	%write_if_no_verbose("ftclp: user selected to start child-parent subsuming after ~q unrolls.\n",[N]).
enable_option_(max_unroll(N)) :-
	( number(N) ->  true 
	; 
	  format("ERROR ftclp: the argument of max-unroll must be a number.\n",[]), halt
	),
	asserta_fact('$max_unroll'(N)).
	%write_if_no_verbose("ftclp: user selected to start child-parent subsuming after ~q unrolls.\n",[N]).
enable_option_(intp_rec_calls(N)) :-
	( number(N) ->  true 
	; 
	  format("ERROR ftclp: the argument must be a number.\n",[]), halt
	),
	asserta_fact('$intp_rec_calls'(N)),
	write_if_no_verbose("ftclp: user selected ~q recursive calls to be interpolated.\n",[N]).
enable_option_(intp_rec_calls_order(O)) :-
	( (O == top ; O == bottom) ->  true 
	; 
	  format("ERROR ftclp: the argument must be either top or bottom.\n",[]), halt
	),
	asserta_fact('$intp_rec_calls_order'(O)),
	write_if_no_verbose("ftclp: user selected ~q to interpolate recursive calls.\n",[O]).
enable_option_(print_answers):- 
	asserta_fact('$print_answers').
enable_option_(dump_interpolants):- 
	asserta_fact('$dump_interpolants').
enable_option_(disj_memo):- 
	asserta_fact('$disj_memo'),
	write_if_no_verbose("ftclp: user selected fully disjunctive interpolants.\n",[]).	


disable_option(dot_output)     :- 
	retract_fact('$dot_output').
disable_option(debug)             :- 
	retract_fact('$debugging').
disable_option(no_verbose)             :- 
	retract_fact('$no_verbose').
disable_option(integer_arithmetic)     :- 
	retract_fact('$integer_arithmetic').
disable_option(minimize_intps)     :- 
	retract_fact('$minimize_intps').
disable_option(unscoped_interpolation)     :- 
	retract_fact('$unscoped_interpolation').
disable_option(scoped_interpolation):- 
	retract_fact('$scoped_interpolation').
disable_option(pred_subsumption):-
	retract_fact('$predicate_subsumption').
disable_option(cycle_subsumption):-
	retract_fact('$cycle_subsumption').
disable_option(clause_subsumption):-
	retract_fact('$clause_subsumption').
 
disable_option(solutions(_)) :-
	retract_fact('$solutions'(_)).
disable_option(min_unroll(_)) :-
	retract_fact('$min_unroll'(_)).
disable_option(max_unroll(_)) :-
	retract_fact('$max_unroll'(_)).
disable_option(intp_rec_calls):-
	retract_fact('$intp_rec_calls'(_)).
disable_option(intp_rec_calls_order):-
	retract_fact('$intp_rec_calls_order'(_)).
disable_option(print_answers):-
	retract_fact('$print_answers').
disable_option(dump_interpolants):-
	retract_fact('$dump_interpolants').
disable_option(disj_memo):-
	retract_fact('$disj_memo').
disable_option(_)                 :- !. 

process_options([]):- !.
process_options(L) :-	
          % inconsistent options
          ( (member(scoped_interpolation, L) , member(unscoped_interpolation, L)) ->
	   format("ftclp error: choose either -scoped-intp or -unscoped-intp but not both.\n",[]),
	   halt
          ;
	  true
          ),
	%%
	( member(no_verbose,         L)      -> enable_option(no_verbose)              ; true),
	( member(scoped_interpolation, L)    -> enable_option(scoped_interpolation)    ; true),
	( member(unscoped_interpolation, L)  -> enable_option(unscoped_interpolation)  ; true),
 
	( member(clause_subsumption, L)      -> enable_option(clause_subsumption)      ; true),
	( member(pred_subsumption, L)        -> enable_option(pred_subsumption)        ; true),
	( member(cycle_subsumption, L)       -> % enable_option(pred_subsumption),
  	                                        enable_option(cycle_subsumption),
					% do not comment this:
                                                  % if we have child-parent subsumption we might
					% not preserve answers so we stop after the first one.
					enable_option(solutions(1))
	                                                                               ; true), 
	( member(disj_memo, L)               -> enable_option(disj_memo)               ; true),
	( member(dot_output, L)              -> enable_option(dot_output)              ; true),
	( member(debug,         L)           -> enable_option(debug)                   ; true),
	( member(print_answers, L)           -> enable_option(print_answers)           ; true),
	( member(dump_interpolants, L)       -> % enable_option(pred_subsumption),
	                                        enable_option(dump_interpolants)       ; true),
	( member(solutions(N1),  L)          -> enable_option(solutions(N1))           ; true),
	( member(intp_rec_calls(N2),  L)     -> enable_option(intp_rec_calls(N2))      ; true),
	( member(min_unroll(N3),  L)         -> enable_option(min_unroll(N3))         ; true),
	( member(max_unroll(N4),  L)         -> enable_option(max_unroll(N4))         ; true),
	( member(intp_rec_calls_order(O), L) -> enable_option(intp_rec_calls_order(O)) ; true),
	( member(integer_arithmetic, L)      -> enable_option(integer_arithmetic)      ; true),
	( member(minimize_intps, L)          -> enable_option(minimize_intps)          ; true),
	!.  

