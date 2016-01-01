% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%=======================================================================%
% This file records answers (succcesful derivations) and allows
% reusing them.
%======================================================================%

 

:- module(answers,
	[ 
	  record_initial_goal/1,
	  initialize_answers/1,
	  end_answers/1,
	  add_answer/2,
	  clear_answers/1,
	  reuse_answers/5,
	  print_answers/1,
	  % for profiling
	  init_answers_profiling_timers/0,
	  print_answers_profiling_timers/0,
	  % for tclp
	  wrapper_run_builtin/2,
	  %% manipulation of path strings
	  atom_concat_with_separator/2,
	  create_str_key/3,
	  init_prefix/1,
	  concat_prefix/4,
	  % consume_goal_from_str_path/4,
	  mk_dash/1,
	  remove_last_functor_from_str_path/4,
	  abstract_str_path/2,
	  convert_str_path_to_term_list/2
	]).

%  Own libraries
:- use_module('trie_C/ciao_trie').
:- use_module(solver).
:- use_module(debug).
:- use_module(counters).
:- use_module(options).
:- use_module(timer).
%  Ciao libraries
:- use_module(library(debugger) , [call_in_module/2]).
:- use_module(library(llists)   , [flatten/2]).
:- use_module(library(format)   , [format/2]).
:- use_module(library(lists)    , [append/3]).

% To use clp_meta/1 from Ciao clpr library
%:-use_package(clpr).
:- use_module(library(clpr/clpr_meta)).

%------------------------------------------------------------------------%
% Notes: 
%
% - We represent derivations as strings (i.e., lists of ASCII codes)
% in order to minimize the number of atoms. Otherwise, Ciao runs out
% of them.
%
% - We would like to simply extend the current solver store with the
% execution of the suffixes to determine if an answer can be reusable
% or not. However, it is quite tricky since we must also consider the
% remaining constraints from the callers.  For now, we re-run the
% whole path each time.
%------------------------------------------------------------------------%

:- multifile rule/3.

init_answers_profiling_timers:-
	mk_timer(add_answer),
	mk_timer(reuse_answer).

print_answers_profiling_timers:-
	get_timer(add_answer,T8),
	get_timer(reuse_answer,T9),
	T17 is T8 + T9,
	format("Answers  \n",[]),
	format("\tTOTAL........................................... ~q ms\n",[T17]),
	format("\t   add ......................................... ~q ms \n",[T8]),
	format("\t   reuse (include sat checks)................... ~q ms \n",[T9]).

% This predicate records the initial state. This is needed to filter
% out answers when reusing. Otherwise, some initial constraints may be
% ignored during reusing incrementing incorrectly the number of
% answers.
:- data '$initial_goal'/1.
record_initial_goal(Goal)   :- 
	asserta_fact('$initial_goal'(Goal)).
	
initialize_answers(TrieRef) :- 
	trie_make(TrieRef).

clear_answers(TrieRef)     :- 
	trie_clear(TrieRef),
	set_counter(num_of_solutions, 0).

end_answers(TrieRef)        :- 
          clear_answers(TrieRef),
	retractall_fact('$initial_goal'(_)).

print_answers(TrieRef)      :- 
	trie_print(TrieRef,"").

add_answer(TrieRef,Stack) :-
	extract_prefix_from_stack(Stack, [] ,PrefixList),
	mk_dash(Separator),
	concat_strings_with_sep(PrefixList, Separator, [], Prefix),
	trie_insert(TrieRef, Prefix).
	
extract_prefix_from_stack([], Ks, Ks).
extract_prefix_from_stack([clause(Goal,Id,_Depth,_ContextId,_,_)| Ss], Ks, NewKs):-
	!,
	create_str_key(Goal,Id,K),
	extract_prefix_from_stack(Ss, [K|Ks], NewKs).
extract_prefix_from_stack([_| Ss], Ks, NewKs):-
	!,
	extract_prefix_from_stack(Ss, Ks, NewKs).

% reuse_answer(+,+,+,+,+)
reuse_answers(_Goal, TrieRef, _, _, _Solver):-
          var(TrieRef),
	!.
reuse_answers(_Goal, TrieRef, StrSubsumerPrefix, StrSubsumedPrefix0, Solver):-
	mk_dash(Dash),
	concat_character(StrSubsumedPrefix0,Dash,StrSubsumedPrefix),
	%%%
	% debugging info
	%%%
	( is_enabled_option(debug) ->
	  format("------------------- REUSING ANSWERS ---------------------\n",[]),
	  atom_codes(AtomSubsumerPrefix,StrSubsumerPrefix),
	  format("SUBSUMER's PREFIX  : ~q\n",[AtomSubsumerPrefix])
	;
	  true
	),
	%%%
	trie_allsuffixed(TrieRef, StrSubsumerPrefix, AllSuffixes),	
	%%%
	% debugging info
	%%%
	( is_enabled_option(debug)  ->
	  map_strings_to_atoms(AllSuffixes,AllAtomSuffixes),
	  format("SUBSUMER's SUFFIXES: ~q\n",[AllAtomSuffixes]),
	  atom_codes(AtomSubsumedPrefix, StrSubsumedPrefix),
	  format("SUBSUMED's PREFIX  : ~q\n",[AtomSubsumedPrefix])
	;
            true
          ),
	%%%
	filter_unsat_suffixes(AllSuffixes, StrSubsumedPrefix, TrieRef, Solver),
	debug_message("---------------------------------------------------------\n",[]),
	!.

filter_unsat_suffixes([],_,_,_):-  !.
filter_unsat_suffixes([StrSuffix|Suffixes], StrPrefix, Trie, Solver):-
	concat_strings([StrPrefix,StrSuffix],Path),
	%%%
	% debugging info
	%%%
	( is_enabled_option(debug) ->
	  atom_codes(AtomPath,Path),
	  format("CHECKING whether ~q is SAT\n",[AtomPath])
	;
	  true
	),
	%% Make sure we start with all initial bindings from the
	%% initial goal.
	( current_fact('$initial_goal'(InitGoal)) ->  true
	;
	  format("ERROR ftclp: record_initial_goal/1 was not called\n",[]),
	  halt
	),
 
	execute_path([InitGoal], Path),
	!,
	( is_enabled_option(print_answers) ->
	  get_counter_value(num_of_solutions,NumOfSol),	  
	  NumOfSol1 is NumOfSol + 1,
	  format("Answer ~q-th: ",[NumOfSol1]),
	  atom_codes(AAPath, Path),
	  format("~q [REUSED]\n",[AAPath])
	;
	  true
	),
	debug_message("is SAT!\n",[]),
	incr_counter(num_of_solutions,_),
	trie_insert(Trie, Path),
	filter_unsat_suffixes(Suffixes, StrPrefix, Trie, Solver).
filter_unsat_suffixes([_|Suffixes], StrPrefix, Trie, Solver):-
	!,
	debug_message("is UNSAT!\n",[]),
	filter_unsat_suffixes(Suffixes, StrPrefix, Trie, Solver).

% for debugging 
map_strings_to_atoms([],[]).
map_strings_to_atoms([Str|Strs],[Atom|Atoms]):-
	atom_codes(Atom,Str),
	map_strings_to_atoms(Strs,Atoms).

%-----------------------------------------------------------------------%
% execute_path(+Goals, +Path)
%-----------------------------------------------------------------------%
% Succeed if the Path starting with query Goals is a successful
% derivation. After we consume the whole Path, Goals must be empty.
%
% Current implementation assumes that all constraints are over reals
% so we can run clpr (much faster) rather than the SMT solver.
%
% NOTES: if we reason about integers we could generate spurious
% answers.  
%-----------------------------------------------------------------------%
execute_path(Goals, Path):-
	% To avoid unifications
          % format("-----------------------------------------\n",[]),
          % format("CALLING execute_path/2\n",[]),
	\+(\+(execute_path_(Goals,Path))),
	!.
execute_path_(Goals,PrefixToExec):- 	               
	% this call can fail if there is not more suffix to execute
	% and Goals is not empty list.
	execute_path_aux(Goals,PrefixToExec,SuffixToExec),	
 
	( var(SuffixToExec) -> % end of the path
	  % format("EXECUTED PATH! REUSE ANSWER \n", []),
	  !
	;
            % still we need to keep visiting the path
	  consume_goal_from_str_path(SuffixToExec, NextGoal, _,_),
	  % format("*** ~q\n",[execute_path_([NextGoal],SuffixToExec)]),
	  execute_path_([NextGoal], SuffixToExec)
          ).
execute_path_aux([], Suffix, Suffix):- 
          !.                              % This cut is important!
execute_path_aux([constraints(Cs,_)| Goals], Suffix, EndSuffix):-
	!,                              % This cut is important

	% format("Executing constraints ~q ... \n",[Cs]),
	% clp_meta(Cs),
	clpr_meta(Cs),
	% format("\tSAT\n",[]),
	execute_path_aux(Goals, Suffix, EndSuffix).
execute_path_aux([builtin(Builtin,_,Module)| Goals], Suffix, EndSuffix):- 
	!,                                % This cut is important
	wrapper_run_builtin(Module, Builtin),
	execute_path_aux(Goals, Suffix, EndSuffix).
% special case: for recursive predicates it's possible this
execute_path_aux([_|_], Suffix, _):-
	var(Suffix),
	!,
	fail.
execute_path_aux([Goal|Goals], Suffix, EndSuffix):-
 
	consume_goal_from_str_path(Suffix, Goal, K, RestSuffix),
 
	% The key is that 1st and 2nd arguments are ground. Thus,
	% there is not non-determinism!
          rule(Goal, K, BodyCl),
 
	!,                                % This cut is important
	% format("bodycl ~q\n",[BodyCl]),
	execute_path_aux(BodyCl, RestSuffix, NextSuffix),  	
 
	execute_path_aux(Goals, NextSuffix, EndSuffix).

wrapper_run_builtin(Module,Builtin):- var(Module), !, call(Builtin).
wrapper_run_builtin(Module,Builtin):- !, call_in_module(Module, Builtin).

 		

% To build prefix paths. 
init_prefix([]).

:- push_prolog_flag(multi_arity_warnings,off).
concat_prefix(Prefix, Suffix, NewPrefix):-
	mk_dash(Separator),
	concat_strings_with_sep([Prefix,Suffix],Separator,[], NewPrefix).
concat_prefix(Prefix, Goal, Id, NewPrefix):-
	create_str_key(Goal, Id, Key),
 	(Prefix == [] ->
 	  NewPrefix = Key
 	;
	  mk_dash(Separator),
 	  concat_strings_with_sep([Prefix,Key], Separator, [], NewPrefix)
	),
 	!.
:- pop_prolog_flag(multi_arity_warnings).	

:- push_prolog_flag(multi_arity_warnings,off).
% create a string from Goal and ClauseId
create_str_key(Goal,ClauseId, Key):-
	functor(Goal,F,N),
	atom_codes(F,FStr),
	number_codes(N,AStr),
	number_codes(ClauseId,ClauseIdStr),
	mk_slash(Slash),
	concat_strings([FStr,Slash,AStr,Slash,ClauseIdStr], Key),
	!.
create_str_key(Goal, Key):-
	functor(Goal,F,N),
	atom_codes(F,FStr),
	number_codes(N,AStr),
	mk_asterisk(Ast),
	mk_slash(Slash),
	concat_strings([FStr,Slash,AStr,Slash,Ast], Key),
	!.
:- pop_prolog_flag(multi_arity_warnings).	


:- push_prolog_flag(multi_arity_warnings,off).
% atom_concat_with_separator(+list(atom),-atom)
atom_concat_with_separator(AtomList, Atom):-
	atom_concat_with_separator(AtomList,'-','',Atom).
atom_concat_with_separator([] ,_,Acc,Acc).
atom_concat_with_separator([A],_,Acc1,Acc2):-
	!,
	atom_concat(Acc1,A,Acc2).
atom_concat_with_separator([A|As],Sep,Acc,Acc3):-
	!,
	atom_concat(Acc,A,Acc1),
	atom_concat(Acc1,Sep,Acc2),
	atom_concat_with_separator(As,Sep,Acc2,Acc3).
:- pop_prolog_flag(multi_arity_warnings).	

% consume_goal_from_str_path(+Suffix, -Goal, ?ClauseId, -RestSuffix)
% from left to right
consume_goal_from_str_path(Suffix, Goal, K, RestSuffix):-
	next_suffix(Suffix, Goal, K, RestSuffix),
	!.
% Note that K may be free 
next_suffix(Suffix, Term, K, RestSuffix2):-
	mk_slash(Slash),
	mk_dash(Dash),
	split(Suffix,Slash,F_Codes,RestSuffix),
	split(RestSuffix,Slash,N_Codes,RestSuffix1),
	split(RestSuffix1,Dash,K_Codes,RestSuffix2),
	( (K_Codes = [Ast], mk_asterisk(Ast)) -> 
	  true
	;
	 number_codes(K,K_Codes)
	),
	create_functor(F_Codes,N_Codes, Term),
	!.
% Note that K may be free 
next_suffix(Suffix, Term, K, []):-
	mk_slash(Slash),
	split(Suffix,Slash,F_Codes,RestSuffix),
	split(RestSuffix,Slash,N_Codes,K_Codes),
	( mk_asterisk(K_Codes) -> 
	  true
	;
	 number_codes(K,K_Codes)
	),
	create_functor(F_Codes, N_Codes, Term),
	!.
% next_suffix(Suffix, _, _, _):- 
% 	format("ERROR ftclp: next_suffix/4 failed ~q\n",[Suffix]),
% 	halt.


create_functor(F_Codes,N_Codes,Term):-
	atom_codes(F,F_Codes),
	number_codes(N, N_Codes),
	functor(Term,F,N).

% last_functor_from_suffix(Suffix, Last):-
% 	consume_goal_from_str_path(Suffix, _, _K, RestSuffix),
% 	( var(RestSuffix) -> 
% 	  Last = Suffix 
% 	;
% 	  last_functor_from_suffix(RestSuffix, Last)
% 	).

% remove_last_functor_from_str_path(+,+,+,-).
remove_last_functor_from_str_path(Goal, ClId, Str, NewStr ):-
	create_str_key(Goal, ClId, LastKey),
          mk_dash(X), 
 	append(NewStr,[X|LastKey], Str).
        
% split(+List,+X,-BeforeX,-AfterX)
split([],_Sentinel, [], _).
split([X|Xs],Sentinel, [], Xs):-
	X == Sentinel,
	!.
split([X|Xs],Sentinel, [X|Ys], Rs):-
	!,
	split(Xs,Sentinel, Ys, Rs).
		
check_match_goal(Goal, F, N0):-
	atom_number(N0,N),
	functor(Goal, F, N), 
	!.
check_match_goal(_Goal, _F, _N):-
	format("ERROR ftclp: some divergence during execution of the suffix\n",[]),
	halt.

% To manipulate strings

concat_strings(CodesLists,CodesList) :- flatten(CodesLists,CodesList).

concat_strings_with_sep([],_C,Acc,Acc):-!.
concat_strings_with_sep([CodesList],_C, Acc, NewAcc):-
	append(Acc,CodesList,NewAcc), !.
concat_strings_with_sep([Xs|Xss],C, Acc, Yss):-
	concat_character(Xs,C,Ys),
	append(Acc,Ys,NewAcc),
	concat_strings_with_sep(Xss, C, NewAcc, Yss).

concat_character(String,C,NewString) :- append(String,[C],NewString).

mk_slash(47).    % ASCII code for "\"
mk_dash(45).     % ASCII code for "-"
mk_asterisk(42). % ASCII code for "*"


% Takes a string that encodes f1/N1/K1-f2/N2/K2-...-fn/Nn/Kn and
% return another string that encodes
% f1/N1/K1-f2/N2/*-...-fn-1/N-1/*-fn/Nn/Kn. That is, replaces the
% clause id with '*' except for the first and last elements
abstract_str_path(Path,AbsPath):-
	convert_str_path_to_term_list(Path, [Goal/K|List]),
	create_str_key(Goal,K,First),
	abstract_path_list(List,  AbsList),
	mk_dash(Separator),	
	concat_strings_with_sep([First|AbsList], Separator, [], AbsPath).
	
abstract_path_list([],[]):- 
	!.
abstract_path_list([Goal/K],[Key]):- 
	!,
	create_str_key(Goal,K, Key).
abstract_path_list([Goal/_|Xs], [Key|Ys]):-
	create_str_key(Goal, Key),
	abstract_path_list(Xs, Ys).

convert_str_path_to_term_list(Path,[Goal/K|AbsPath]):-
	next_suffix(Path, Goal, K, NextPath),
	( var(NextPath) -> AbsPath = [] 
	; 
	  convert_str_path_to_term_list(NextPath, AbsPath)
	).
