% Author: Jorge. A Navas, The University of Melbourne 2012

:- module(ftclp_tr, 
	[
	  ftclp_expansion/2,
	  clear_ftclp_tr/0,
	  % Auxiliary predicates
	  get_num_of_clauses/2,
	  is_counter_inst_constraint/1
	], 
	[assertions, dcg, nortchecks]).	

%=======================================================================%
% Main component of the frontend
%=======================================================================%

% Ciao libraries
:- use_module(library(format)   , [format/2]).
:- use_module(library(sort)     , [sort/2]).
:- use_module(library(terms)    , [atom_concat/2]).
% Own libraries
:- use_module(builtins).	
:- use_module('../counters').
:- use_module('../debug').

:- include(ops).

%---------------------------------------------------------------------%
% This code expansion generates four predicates:
%---------------------------------------------------------------------%
% - rule/3        to represent clauses
% - builtin/3     to represent builtins
% - constraints/2 to represent a sequence of CLP constraints
% - tabled/1      to indicate that we should do caching of the predicate. 
%                 it also contains type information.
% - no_cache/1    
% - discriminants/1
%
%% Deprecated
% - mode/1        to express the predicate argument modes
%
% These predicates must be defined as "multifile" in the files where
% make use of this translation.
%---------------------------------------------------------------------%

clear_ftclp_tr:- 
	delete_all_counters,
	retractall_fact('$counter_inst_constraint'(_)).

% get_num_of_clauses(+,-)
get_num_of_clauses(Goal,N1):-
 	functor_to_atom(Goal,Name),
 	get_counter_value(Name,N),
 	N1 is N-1.

% keep track of which constraints were introduced by the counter
% instrumentation phase.
:- data '$counter_inst_constraint'/1.
is_counter_inst_constraint(Id):-
          ground(Id),
          current_fact('$counter_inst_constraint'(Id)).
	
%----------------------------------------------------------------------%
% Things to do before and after expansion
%----------------------------------------------------------------------%
ftclp_expansion(0,  _ )         :- 
	set_counter('$tclp_tr_builtins', 1),
	set_counter('$tclp_tr_constraints', 1).
ftclp_expansion(end_of_file, _ ):- !.
	%delete_all_counters.
%----------------------------------------------------------------------%
% Expand directives
%----------------------------------------------------------------------%
ftclp_expansion((:- tabled(Pred))         , tabled(Pred)).
%	format("tabled(~q).\n",[Pred]).
ftclp_expansion((:- no_cache(Pred))       , no_cache(Pred)).
%	format("no_cache(~q).\n",[Pred]).
ftclp_expansion((:- discriminants(Pred))  , discriminants(Pred)).
%	format("discriminants(~q).\n",[Pred]).
%% Deprecated mode/1
ftclp_expansion((:- mode(Pred))      , mode(Pred)).
%	format("mode(~q).\n",[Pred]).
ftclp_expansion((:- Dir)             , (:- Dir)).
%	format(":- ~q \n",[Dir]).
%----------------------------------------------------------------------%
% Expand clauses
%----------------------------------------------------------------------%
ftclp_expansion((Head :- Body)      , rule(Head,K,List)):-
	functor_to_atom(Head,Name),
	incr_counter(Name, K),
          flatten_goals(Body,List,[]).
%	print_rule(Head, K, List).
ftclp_expansion((Head)               , rule(Head,K,[])):-
	functor_to_atom(Head,Name),
	incr_counter(Name, K).
%	print_rule(Head, K, []).
flatten_goals((G1 , G2)) --> !,
	flatten_goals(G1),
	flatten_goals(G2).
flatten_goals(true)      --> !, [].
flatten_goals((_ ->_;_)) --> 
	{ 
	  format("ERROR ftclp: (_ -> _ ; _) is not supported \n",[]),
	  halt
	}.
flatten_goals((_ -> _)) --> 
	{ 
	  format("ERROR ftclp: (_ -> _) is not supported \n",[]),
	  halt
	}.
flatten_goals(G)         --> 
	{
	  G == !,  
	  format("ftclp WARNING: metainterpreter does not support ! \n",[]),
	  format("               all cuts will be ignored.\n",[])
	},
	[].
flatten_goals(G)         --> 
	{ 
	  sp_builtin(G, Module),  
	  incr_counter('$tclp_tr_builtins', K)
	}, 	  
	[builtin(G,K,Module)].	 
flatten_goals(G)         --> 
	{ 
	  functor(G,clp_meta,1), 
	  arg(1,G,Ls),
	  Ls = []
	}, 
	[].	 
flatten_goals(G)         --> 
	{ 
	  functor(G,clp_meta,1), 
	  incr_counter('$tclp_tr_constraints', K),
	  arg(1,G,Ls) 
	}, 
	[constraints(Ls,K)].	 
flatten_goals(G)         --> 
	{ 
	  functor(G,counter_inst__clp_meta,1), 
	  incr_counter('$tclp_tr_constraints', K),
	  arg(1,G,Ls) ,
	  asserta_fact('$counter_inst_constraint'(K))
	}, 
	[constraints(Ls,K)].	 
flatten_goals(G)         --> 
	{
	    check_is_not_keyword(G)
	},
	[G].


% functor_to_atom(+Term,-Atom)
functor_to_atom(Term,Atom):-
	functor(Term,F,A),	
	atom_number(A_atm,A),
	atom_concat(['$tclp_tr_',F,'/',A_atm],Atom).

check_is_not_keyword(G):-
	functor(G,F,_),
	( 	
            F \== rule, F \== builtin, F \== constraints, F \== counter_inst__constraints, 
	  F \== tabled, F \== no_cache, F \== mode
	),
	!.
check_is_not_keyword(G):-
	functor(G,F,_),
	format("ERROR ftclp: ~q is a keyword!\n",[F]),
	halt.
	
% Debugging
% print the transformed program if 1st argument unifies with debug_on.
print_rule(Head, K, []):-
	%debug_message("rule(~q,~q,~q).\n",[Head,K,[]]).
	format("rule(~q,~q,~q).\n",[Head,K,[]]).
print_rule(Head, K, List):-
	%debug_message("rule(~q,~q):- ~q . \n",[Head,K,List]).
	format("rule(~q,~q):- ~q . \n",[Head,K,List]).
		
