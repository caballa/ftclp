% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%=======================================================================%
%             Insert and lookup in a memo table. 
%=======================================================================%
% The memo table is split into several regions:
% (1) unscoped-clause area
% (2) scoped-clause area
% (3) predicate area 
%
% (1) and (2) store unscoped and scoped interpolants,
% respectively. They are used for subsuming a clause. Conceptually,
% there is no need for having two different regions. However, in the
% way we conjoin interpolants from different derivations is much
% easier if we do this. However, the price to pay is that we could
% have a lot redundant interpolants.

% (3) is for storing interpolants that can be used to reuse a
% predicate. The interpolants from (3) are formed by combining those
% from (1) and (2).  In general, whenever a new interpolant is stored
% in (3) those used from (1) or (2) are deleted. If (1) and (2) can be
% used to generate the predicate interpolant we create two entries in
% the predicate memo table.
%=======================================================================%
% TODO/FIXME: 
% - We keep two different memo tables for clause interpolants: one for
%   scoped and another for unscoped interpolants.  Otherwise, we would
%   break some important assumptions that allow us easily conjoining
%   interpolants.  However, there can be many duplicates between
%   scoped and unscoped interpolants.
%=======================================================================%

:- module(cache, 
	[ 
	 init_cache/0,
	 clear_cache/0,
	 print_cache_stats/0,
	 init_cache_profiling_timers/0,
	 print_cache_profiling_timers/0,
	 % lookup
	 cache_lookup_clause/7,
	 cache_lookup_pred/6,
	 % store 
	 lazy_update_intp_memotable/14,	 
	 mark_complete_clause/1,
	 mark_complete_predicate/4,
	 cache_gen_disjunctive_interpolants/4,
	 % for ftclp (handle of cycles)
	 lookup_intp_clause_tables/6,
	 lookup_intp_pred_table/4,
	 delete_clause_intp_table/1,
 
	 % for ftclp (user output)
	 dump_interpolants/2,
	 % for debugging in interpolation.pl
	 debug_print_intp_table/1
	]).
%  Own libraries
:- use_module(solver).
:- use_module(answers).
:- use_module(counters).
:- use_module(debug).
:- use_module(options).
:- use_module(op_attributes     , [varset_attributes/2, 
	                         extract_equalities_from_last_unification/4,
	                         print_term_with_attribute/1,
			     print_term_with_attribute/2]).
:- use_module(interpolation).
:- use_module(ftclp             , [get_map_node_to_unique_id/4, is_fake_false_id/1]).
:- use_module('analysis/discriminants').
:- use_module(timer).
:- use_module('frontend/ftclp_tr', [get_num_of_clauses/2]).
%  Ciao libraries
:- use_module(library(terms)    , [atom_concat/2]).
:- use_module(library(format)   , [format/2]).
:- use_module(library(write)).
:- use_module(library(lists)    , [length/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(pathnames), [path_splitext/3]). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% '$intp_table'(+Goal,+ClId,+NodeId,?Itp,?Node,?ClPrefix,?PredPrefix)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% - Goal is the term associated with the tree node.
% - ClId is the identifier for the goal clause.
% - Id is a unique identifier for the tree node.
% - Itp is the interpolant.
% - Node is a complex term for dot output.
% - ClPrefix is an atom that represents the prefix path from the
%   root up to the clause <Goal,Id>.
% - PredPrefix is an atom that represents the prefix path from the
%   root up to the goal Goal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- data '$intp_scoped_table'/7.
:- data '$intp_unscoped_table'/7.
:- data '$intp_pred_table'/4.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  To record that the execution of a clause is complete.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- data '$complete_clause'/1.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              INITIALIZATION, CLEANUP, AND PRINTING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init_cache/0
init_cache:-
          %% Counters for collecting statistics
	set_counter(num_of_entries, 0),
	set_counter(num_of_unchanged_stores, 0),
	set_counter(num_of_changed_stores, 0),
	set_counter(num_of_true_entries, 0),
	set_counter(num_of_merged_entries, 0),
	set_counter(num_of_formulas, 0),
	set_counter(call_success, 0),
	set_counter(call_fail, 0).

% clear_cache/0
clear_cache:-
	retractall_fact('$intp_scoped_table'(_,_,_,_,_,_,_)),
	retractall_fact('$intp_unscoped_table'(_,_,_,_,_,_,_)),
	retractall_fact('$intp_pred_table'(_,_,_,_)),
          retractall_fact('$complete_clause'(_)),
 
	init_cache.

% print_cache_stats/0
print_cache_stats:-
	get_counter_value(call_success,N1),
	get_counter_value(call_fail,N2),
	get_counter_value(num_of_entries,N3),
	get_counter_value(num_of_unchanged_stores, N4),
	get_counter_value(num_of_changed_stores, N5),
	get_counter_value(num_of_merged_entries,N6),

	format("PRUNING STATS\n",[]),
	format("\tNumber of cached goals .......................... ~q\n",[N3]),
	format("\tNumber of cache hits............................. ~q\n",[N1]),
	format("\tNumber of cache fails............................ ~q\n",[N2]),
	format("\tNumber of stores that do not change memo table... ~q\n",[N4]),
	format("\tNumber of stores that change memo table.......... ~q\n",[N5]),
	format("\tNumber of merged entries using disjunction....... ~q\n",[N6]).

init_cache_profiling_timers:-
        	mk_timer(pred_subsumption),
	mk_timer(clause_subsumption),
	mk_timer(subsumption_entailment),
	mk_timer(collapse_memo_entries).

print_cache_profiling_timers:-
        	get_timer(pred_subsumption,T6),
	get_timer(clause_subsumption,T7),
	get_timer(subsumption_entailment,T10),	
	get_timer(collapse_memo_entries,T21),	
	T16 is T6+T7,
	format("Subsumption\n",[]),
	format("\tTOTAL........................................... ~q ms\n",[T16]),
	format("\t   Entailments.................................. ~q ms\n",[T10]),	
	format("\t   predicate-level (lookup+entailment) ......... ~q ms\n",[T6]),
	format("\t   clause-level (lookup+entailment)............. ~q ms\n",[T7]),
	format("\t   Build disjunctive memo entries............... ~q ms\n",[T21]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%                        CACHE INSERTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%----------------------------------------------------------------------%
% lazy_update_intp_memotable(+WhichTable,+Goal,+,+,+,+,+,+Itp,
%                            +CheckInd,+PrevItp,+Cs,+,-CFlag,-IFlag)
%----------------------------------------------------------------------%
% If Goal is not in the memo then it creates a new entry associated
% with Itp.  Otherwise, it conjoins the previous interpolant OldItp
% with Itp. Furthermore, if OldItp implies Itp then CFlag=tt.
% 
% If CheckInd is tt then we perform the inductiveness test: PrevItp and
% Cs implies Itp. If yes, then IFlag=tt. Otherwise, CheckInd=IFlag=ff.
%
% WhichTable = [ unscoped | scoped ]
%----------------------------------------------------------------------%
lazy_update_intp_memotable(WhichTable,
	                 Goal,ClId,Depth,ContextId,Prefix,NewPrefix,Itp,
	                 CheckInd, PrevItp, Cs,
	                 Solver,ContinueFlag,InductiveFlag):-
	% map <Goal,ClId,Depth> to a unique identifier
	get_map_node_to_unique_id(Goal, ClId, Depth, NodeId),

	% In order to apply optimization we need to ensure the
	% sequence interpolant is so far inductive.
	( (is_enabled_option(minimize_intps), CheckInd==tt) ->
	   check_inductiveness(Solver, PrevItp, Cs, Itp, InductiveFlag)
	;
	   InductiveFlag = ff
          ),

	( retract_intp_table(NodeId, WhichTable, OldItp, Node) ->
	  %%%
	  % If the same node has already an interpolant. Then, we
	  % conjoin new and old interpolants. Furthermore it checks if
	  % the old interpolant is more specific than the new one. If
	  % yes ContinueFlag=ff, otherwise ContinueFlag=tt
	  %%%
	  conjoin_formulas(Solver,OldItp,Itp,NewItp),

	  % format("*** Conjoining ",[]),
	  % print_solver_formula(Solver,OldItp),
	  % format(" and ",[]),
	  % print_solver_formula(Solver,Itp),
	  % format("\n",[]),

	  %%%%%%%
	  %%% We can avoid the entailment tests at the expense of
	  %%% computing more interpolants.
	  %%%%%%%
	  ( ( InductiveFlag==tt, 
 	      solver_entailment_formulas(Solver, OldItp, Itp) ) -> 
	      ContinueFlag = ff    % we stop the recursive call
            ;
	      ContinueFlag = tt
	  ),
	  incr_counter(num_of_formulas,_),
	  NewNode = Node	
	;
	  %%%
	  % First time the node is annotated with an interpolant
	  %%%
	  NewItp   = Itp,

	  % format("*** Conjoining true and ",[]),
	  % print_solver_formula(Solver,Itp),
	  % format("\n",[]),

	  NewNode  = node(Goal,ClId,Depth,ContextId),
	  ContinueFlag  = tt,
	  incr_counter(num_of_entries,_),
	  incr_counter(num_of_formulas,_),
	  incr_counter(num_of_changed_stores,_)
	),
	!, % Important cut here: do not allow backtracking
	insert_intp_table(Goal, ClId, NodeId, NewItp, NewNode, 
	                  NewPrefix, Prefix, WhichTable, Solver).
 

% Succeed if PrevItp and Cs implies Itp
check_inductiveness(_,PrevItp,_,_,tt) :-
	% var here is interpreted as false.
	var(PrevItp),
	!,
	incr_counter(inductiveness_checks,_),
	incr_counter(inductive_intp,_).
check_inductiveness(Solver,PrevItp,Cs,Itp,IndFlag) :-
	incr_counter(inductiveness_checks,_),
	solver_sibling_push(Solver),
	solver_mk_not(Solver, Itp, NegItp),
	NewCs = [PrevItp, NegItp | Cs],
	solver_sibling_incremental(Solver, NewCs, SatFlag),
	solver_sibling_reset(Solver),
	!,
	( SatFlag == ff ->
	  IndFlag = tt,
	  incr_counter(inductive_intp,_)
	;
	  IndFlag = ff,
	  incr_counter(noninductive_intp,_)
        ).
%---------------------------------------------------------------------%
% conjoin_formulas(+,+,+,-)
%---------------------------------------------------------------------%
% Conjoin two formulas but remove redundancies if possible
% Note that solver_conjoin_formulas does not simplify if one formula
% is stronger than the other. So we try to do it here.
%---------------------------------------------------------------------%
% For efficiency reasons we might want to comment the first two clauses.
conjoin_formulas(Solver,F1, F2, F1):-
	solver_entailment_formulas(Solver, F1, F2),
	!,
	incr_counter(num_of_unchanged_stores,_).
conjoin_formulas(Solver, F1, F2, F2):-
	solver_entailment_formulas(Solver, F2, F1),
	!,
	incr_counter(num_of_unchanged_stores,_).
conjoin_formulas(Solver, F1, F2, F3):-
	solver_conjoin_formulas(Solver,F1,F2,F3),
	incr_counter(num_of_changed_stores,_).

%---------------------------------------------------------------------%	
% find_intp_table(+,+,-,-)
%---------------------------------------------------------------------%	
% Succeed if there is another entry with exactly the same node id. If
% yes, it means that the same node has been explored before. 
%---------------------------------------------------------------------%	
retract_intp_table(NodeId, WhichTable, Itp, Node):-
	WhichTable == unscoped,
	% We assume that Ciao is fast indexing by 2nd argument
	current_fact('$intp_unscoped_table'(_,_,NodeId,Itp,Node,_,_),Ref),
	!,
	erase(Ref).
retract_intp_table(NodeId, WhichTable, Itp, Node):-
	WhichTable == scoped,
	% We assume that Ciao is fast indexing by 2nd argument
	current_fact('$intp_scoped_table'(_,_,NodeId,Itp,Node,_,_),Ref),
	!,
	erase(Ref).
	
%---------------------------------------------------------------------%	
% insert_intp_table(+,+,+,+,+,+,+,+)
% Assert an entry into the table.
%---------------------------------------------------------------------%	
% ClausePrefix is PredPrefix but with one more key. They are
% different because subsuming a clause is one level deeper than
% subsuming a predicate.
%---------------------------------------------------------------------%	
:- push_prolog_flag(multi_arity_warnings,off).
insert_intp_table(Goal,ClId,NodeId,Itp,Node,ClausePrefix,PredPrefix,
	        WhichTable, Solver):-
 
          ( solver_is_true(Solver, Itp) ->
 
	  true
          ;
	  ( solver_imply_false(Solver, Itp) -> 
 
	    true
            ;
              insert_intp_table(Goal,ClId,NodeId,Itp,Node,ClausePrefix,PredPrefix,
	                      WhichTable)
            )
          ).
insert_intp_table(Goal,ClId,NodeId,Itp,Node,ClausePrefix,PredPrefix,
	        WhichTable):-
	WhichTable == unscoped,!,
	unify_discriminating_args(Goal, NewGoal),
	asserta_fact('$intp_unscoped_table'(NewGoal,ClId,NodeId,Itp,Node,
	                                    ClausePrefix,PredPrefix)).
insert_intp_table(Goal,ClId,NodeId,Itp,Node,ClausePrefix,PredPrefix,WhichTable):-
	WhichTable == scoped,!,
	unify_discriminating_args(Goal, NewGoal),
	asserta_fact('$intp_scoped_table'(NewGoal,ClId,NodeId,Itp,Node,
	                                  ClausePrefix,PredPrefix)).
:- pop_prolog_flag(multi_arity_warnings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% CACHE LOOKUP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%------------------------------------------------------------------------%
% cache_lookup(+term,+num,+num,+Solver,-list(ASCII),-term)
% cache_lookup(Goal,ClauseId,Depth,Solver,SubsumerPrefix,SubsumerNode)
%------------------------------------------------------------------------%
% Succeed if there exists another entry with same goal and **same
% clause** such that current solver store entails entry's interpolant.
%------------------------------------------------------------------------%
:- multifile no_cache/1.
cache_lookup_clause(constraints(_,_),_,_,_,_,_,_):- !,  fail. % not stored 
cache_lookup_clause(builtin(_,_,_)  ,_,_,_,_,_,_):- !,  fail. % not stored
cache_lookup_clause(Goal            ,_,_,_,_,_,_):- no_cache(Goal), !, fail.
cache_lookup_clause(Goal,ClauseId,_Depth,Solver,
	          SubsumerPrefix,SubsumerNode, SubsumedIntpInfo):-
	debug_message("Searching for clause ",[]),
	debug_print_term_with_attribute(Goal),
	debug_message(" ... \n",[]),
	cache_lookup_clause_aux(Goal,ClauseId,Solver,
	                        SubsumerPrefix,SubsumerNode,
			    SubsumedIntpInfo),
	!.

%------------------------------------------------------------------------%
% cache_lookup_clause_aux(+term,+num,+Solver,-list(ASCII),-term,-term)
% cache_lookup_clause_aux(Goal,ClauseId,Solver,SubsumerPrefix,SubsumerNode,...)
%------------------------------------------------------------------------%
% Succeed if there exists an interpolant entailed by the solver state.
% Here only the same Goal and same ClauseId can be subsumed.
% SubsumerGoal is the subsumer goal, SubsumerPrefix is the prefix from
% the root up to SubsumerGoal.
%------------------------------------------------------------------------%
cache_lookup_clause_aux(Goal,ClauseId,Solver,SubsumerPrefix,SubsumerNode, SubsumedIntpInfo):-
	              
	% We match entries with SAME Goal AND ClauseId !	
	is_enabled_option(clause_subsumption),
	\+var(ClauseId),
	start_timer(clause_subsumption),
 
 
	%================================================================%
	% Important: here we do allow backtracking: '$intp_table'/7
	% can have multiple facts even for the same key.
	%================================================================%
	lookup_intp_clause_tables(SubsumerGoal,ClauseId, 
	                          SubsumerNodeId,SubsumerItp,
		                SubsumerNode,SubsumerPrefix),

 
	%================================================================%
	% This is needed for correctness.  We cannot use an
	% interpolant from a node unless its whole subtree below is
	% complete.
	%================================================================%
	is_complete_clause(SubsumerNodeId),

 

	%================================================================%
	% not not to avoid keeping unified Goal and SubsumerGoal.
	%================================================================%
	\+(\+(is_covered(Goal, SubsumerGoal, SubsumerItp, Solver))),
	stop_timer(clause_subsumption,_),	
	%================================================================%
	% To generate the interpolant from the subsumed node
	%================================================================%
	SubsumedIntpInfo = '$subsumed_intp'(Goal, SubsumerGoal, SubsumerItp),
	!.
cache_lookup_clause_aux(_,_,_,_,_,_):-
	stop_timer(clause_subsumption,_),
	!,
	fail.
	
	
lookup_intp_clause_tables(/*in*/ SubsumerGoal,SubsumerClauseId,
	                /*in/out*/SubsumerNodeId,
	                /*out*/SubsumerItp,SubsumerNode,SubsumerPrefix):-
	is_enabled_option(scoped_interpolation),
	is_enabled_option(unscoped_interpolation),
	( current_fact('$intp_scoped_table'(SubsumerGoal,SubsumerClauseId,
	                                    SubsumerNodeId, SubsumerItp, 
				      SubsumerNode, SubsumerPrefix, _))
	
	;
	  current_fact('$intp_unscoped_table'(SubsumerGoal,SubsumerClauseId,
	                                      SubsumerNodeId, SubsumerItp, 
				        SubsumerNode, SubsumerPrefix, _))
          ).
lookup_intp_clause_tables(/*in*/ SubsumerGoal,SubsumerClauseId,
	                /*in/out*/SubsumerNodeId,
	                /*out*/SubsumerItp,SubsumerNode,SubsumerPrefix):-
	is_enabled_option(scoped_interpolation),
	current_fact('$intp_scoped_table'(SubsumerGoal,SubsumerClauseId, 
	                                  SubsumerNodeId, SubsumerItp, 
				    SubsumerNode, SubsumerPrefix, _)).
lookup_intp_clause_tables(/*in*/ SubsumerGoal,SubsumerClauseId,
	               /*out*/SubsumerNodeId,SubsumerItp,SubsumerNode,SubsumerPrefix):-
	is_enabled_option(unscoped_interpolation),
	current_fact('$intp_unscoped_table'(SubsumerGoal,SubsumerClauseId,
	                                    SubsumerNodeId, SubsumerItp, 
				      SubsumerNode, SubsumerPrefix, _)).

% Assume that this operation is constant.
delete_clause_intp_table(NodeId):-
        ( current_fact('$intp_scoped_table'(_,_,NodeId,_,_,_,_), Ref) ->
	erase(Ref)
        ; 
	( current_fact('$intp_unscoped_table'(_,_,NodeId,_,_,_,_), Ref) ->
	  erase(Ref)
	;
	  true
          )
        ).
        
%------------------------------------------------------------------------%
% cache_lookup_pred(+term,+num,+Solver,-list(ASCII),-term,-term)
% cache_lookup_pred(Goal,Depth,Solver,SubsumerPrefix,SubsumerNode,...)
%------------------------------------------------------------------------%
% Succeed if there exists an interpolant in the predicate memo
% entailed by the solver state.  SubsumerGoal is the subsumer goal,
% SubsumerPrefix is the prefix from the root up to SubsumerGoal.
%------------------------------------------------------------------------%
cache_lookup_pred(constraints(_,_),_,_,_,_,_):- !,  fail. % not stored 
cache_lookup_pred(builtin(_,_,_),_,_,_,_,_)  :- !,  fail. % not stored
cache_lookup_pred(Goal          ,_,_,_,_,_):- no_cache(Goal), !, fail.
cache_lookup_pred(Goal, _Depth, Solver, 
	        SubsumerPrefix, SubsumerNode, SubsumedIntpInfo):-
	debug_message("Searching for predicate ",[]),
	debug_print_term_with_attribute(Goal),
	debug_message(" ... \n",[]),
	cache_lookup_pred_aux(Goal, Solver, 
	                      SubsumerPrefix, SubsumerNode, 
                  	            SubsumedIntpInfo),
	!.

cache_lookup_pred_aux(Goal,Solver,SubsumerPrefix,SubsumerNode,SubsumedIntpInfo):-
	is_enabled_option(pred_subsumption),
	start_timer(pred_subsumption),
 
 
	%================================================================%
	% Important: here we do allow backtracking to check all
	% entries until one succeeds.
	%================================================================%
	lookup_intp_pred_table(SubsumerGoal, SubsumerItp, 
	                       SubsumerNode, SubsumerPrefix),
 

 

	%--------------------------------------------------------------%
	% No need to check the interpolant belongs to a complete node
	% since only complete goals are asserted in '$intp_pred_table'.
	%--------------------------------------------------------------%
	\+(\+(is_covered(Goal, SubsumerGoal, SubsumerItp, Solver))),
	%================================================================%
	% To generate the interpolant from the subsumed node
	%================================================================%
	SubsumedIntpInfo = '$subsumed_intp'(Goal, SubsumerGoal, SubsumerItp),
	stop_timer(pred_subsumption,_),
	!.
cache_lookup_pred_aux(_,_,_,_,_):-
	stop_timer(pred_subsumption,_),
	!, 
	fail.

lookup_intp_pred_table(Goal,Itp,Node,Prefix):-
 
	current_fact('$intp_pred_table'(Goal,Itp,Node,Prefix)).
	   
%--------------------------------------------------------------------%
% is_covered(+term, +term, +list, +Solver)
%--------------------------------------------------------------------%
is_covered(Goal, SubsumerGoal, SubsumerItp, Solver):-
 
	Goal = SubsumerGoal,
	extract_equalities_from_last_unification(SubsumerGoal,[],_,MatchingEqs),
	start_timer(subsumption_entailment),
 
 
	
	( solver_entailment(Solver, MatchingEqs, SubsumerItp) ->
	  incr_counter(call_success,_),
	  stop_timer(subsumption_entailment,_)
          ;
	  incr_counter(call_fail   ,_),
	  stop_timer(subsumption_entailment,_),
	  !, 
	  fail
	),
	!.

 

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              EXTRA LOGIC TO MAKE SURE SUBSUMPTION IS CORRECT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%---------------------------------------------------------------------------%
% To mark a clause as explored. This means that we can use its
% interpolant for subsumption. The concept of completeness here can be
% defined easily recursively. A leaf node in the derivation tree is
% complete by definition. Then, a clause is considered complete iff
% all its children are complete.
%---------------------------------------------------------------------------%
mark_complete_clause(NodeId):-
	is_enabled_option(clause_subsumption),
	!,
	% sanity check
	check_nonvar(NodeId),
	debug_message("Marking node id ~q as explored!\n",[NodeId]),
	asserta_fact('$complete_clause'(NodeId)).
mark_complete_clause(_):- 
	!.
%---------------------------------------------------------------------------%
% Succeed if the node is has been marked as explored.
%---------------------------------------------------------------------------%
is_complete_clause(NodeId):-
	is_enabled_option(clause_subsumption),
	!,
	% sanity check
	check_nonvar(NodeId),
	current_fact('$complete_clause'(NodeId)).
is_complete_clause(_):- !, fail.
	
check_nonvar(X):-
	var(X), !,
	format("ERROR ftclp: argument cannot be var.\n",[]), 
	halt.
check_nonvar(_) :- !.

%---------------------------------------------------------------------------%
% mark_complete_predicate(+Goal,+Depth,+Solver,+DoNotErase)
%---------------------------------------------------------------------------%
% Check if all clauses of a given predicate are complete. If so, then
% we conjoin their interpolants and store as a new entry in the
% predicate memo table. 
%
% DoNotErase are node ids that lie on a cycle in the tree. By
% default, once interpolants from the clause memo tables have been
% used to form interpolants in the predicate memo we remove the former
% ones except if it belongs to DoNotErase.
%---------------------------------------------------------------------------%
:- multifile rule/3.
mark_complete_predicate(Goal,Depth,Solver,DoNotErase):-
	% Note that goal is not renamed (i.e., it does not have
	% attribute variables.)
	is_enabled_option(pred_subsumption),
	!,
	% Also, it's tempted to retract facts (call
	% retract_intps_from_clauses/3) inside the findall. However,
	% we cannot do that unless all clauses are completed.
	findall( Goal-NodeId,
		( rule(Goal, ClauseId, _),
		  get_map_node_to_unique_id(Goal, ClauseId, Depth, NodeId),
		  is_complete_clause(NodeId) 
		), 
		Ls),

	length(Ls,NumOfCompletedClauses),
	get_num_of_clauses(Goal, NumOfClauses),
	NumOfClauses =:= NumOfCompletedClauses,
	Ls \== [],
	!,
 
	( is_enabled_option(unscoped_interpolation) ->
	  findall_and_retract_intps_from_clauses(Ls,unscoped,  
	                                         DoNotErase, [E1|Es1]),
	  build_and_store_pred_intp(E1, Es1, unscoped, Solver)
	; 
	  true
	),
	( is_enabled_option(scoped_interpolation) ->
	  findall_and_retract_intps_from_clauses(Ls,scoped,  
	                                         DoNotErase,[E2|Es2]),
	  build_and_store_pred_intp(E2, Es2, scoped, Solver)
	;
	  true
	).	
mark_complete_predicate(_,_,_,_):- !.

 

          

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       CODE TO PRODUCE "PREDICATE" INTERPOLANTS FROM "CLAUSE" INTERPOLANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
% findall_and_retract_intps_from_clauses(+,+,+NodeIds,-)
%-----------------------------------------------------------------------------%
% Here we take all the interpolants from the clauses of the predicate
% and rename in terms of a common set of variables and assert an entry
% (after conjoining all interpolants) in the memo table for
% predicates. As a side effect all the entries corresponding to the
% clauses are deleted only if they are not elements of NodeIds.
%-----------------------------------------------------------------------------%
%%%%%
%% This version keeps some entries
%%%%%
findall_and_retract_intps_from_clauses([], _, _, []):- 
	!.
findall_and_retract_intps_from_clauses(Xs, unscoped, NodeIds, Ys):-
	findall(t(NodeId, Ref, Goal-ClId-Itp-Node-PredPrefix),
	         (member(_-NodeId,Xs), 
		current_fact('$intp_unscoped_table'(Goal,ClId,NodeId,Itp,
                                                        Node,_,PredPrefix),Ref)),
		Zs),
	unzip_and_retract(Zs, NodeIds, Ys),
	!.
findall_and_retract_intps_from_clauses(Xs, scoped, NodeIds, Ys):-
	findall(t(NodeId, Ref, Goal-ClId-Itp-Node-PredPrefix),
	        (member(_-NodeId,Xs),
	         current_fact('$intp_scoped_table'(Goal,ClId,NodeId,Itp,
                                                     Node,_,PredPrefix),Ref)),
	         Zs),
	unzip_and_retract(Zs, NodeIds, Ys),
	!.
			
unzip_and_retract([],_,[]).
unzip_and_retract([t(NodeId, _Ref, Goal-ClId-Itp-Node-PredPrefix)| Xs], 
	        NodeIds, 
	        [Goal-ClId-Itp-Node-PredPrefix | Ys]):-
        member(NodeId, NodeIds),
        !,
        unzip_and_retract(Xs,NodeIds,Ys).
unzip_and_retract([t(_NodeId, Ref, Goal-ClId-Itp-Node-PredPrefix)| Xs], 
	        NodeIds, 
	        [Goal-ClId-Itp-Node-PredPrefix | Ys]):-
        !,
        erase(Ref),
        unzip_and_retract(Xs,NodeIds,Ys).

 

 

build_and_store_pred_intp(Goal-_ClId-InitItp-Node-Prefix, Es, Kind_Of_Intp, Solver):-
	% All prefixes must be the same so we pick the first. Each node
	% refers to each goal clause. We just pick the first one.  We
	% will rename all Itps in @Es in terms of @NewVs.
	varset_attributes(Goal,NewVs),
	rename_and_conjoin_clause_intps(Es, NewVs, Kind_Of_Intp, Solver, InitItp, EndItp),
	Node = node(_,_,Depth,_),
	( is_enabled_option(debug) ->
	  format("ADDING PREDICATE INTERPOLANT at depth ~q ... \n",[Depth]),
	  print_term_with_attribute(Goal),
	  format("\n\t",[]),
	  print_solver_formula(Solver,EndItp),
	  format("\n",[])
	;
	  true
	),	  
 
	asserta_fact('$intp_pred_table'(Goal, EndItp, Node, Prefix)),
 
	!.

rename_and_conjoin_clause_intps([], _, _, _, EndItp, EndItp):- !.
rename_and_conjoin_clause_intps([Goal-_ClId-Itp-_-_|Ts], NewVs, Kind_Of_Intp, Solver, AccItp, EndAccItp):-
	varset_attributes(Goal,OldVs),	
 
	%%%
	% We rename and conjoin only if not existentially quantified
	% variables. Predicate interpolants are used as candidate
	% for child-parent subsumption so we do not want to have
	% existentially quantified variables involved.
	%%%
	( Kind_Of_Intp == unscoped ->
	  ( solver_all_vars_included(Solver, Itp, OldVs) ->
	    true
	  ;
	    debug_message("NOT ADDING PREDICATE INTERPOLANT at for ~q ",[Goal]),
	    debug_message("because has existentially quantified variables\n",[]),
	    !,
	    fail
	  )
	;
	  true
          ),
	%%%
	solver_rename_formula(Solver, Itp, OldVs, NewVs, RenItp),
 
	solver_conjoin_formulas(Solver,AccItp,RenItp,NewAccItp),	
	rename_and_conjoin_clause_intps(Ts, NewVs, Kind_Of_Intp, Solver, NewAccItp, EndAccItp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              CODE TO PRODUCE DISJUNCTIVE INTERPOLANTS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%% TODO: we produce disjunctive interpolants from clauses, but not from
%%       predicates.
%%%

%-----------------------------------------------------------------------------%
% cache_disjunctive_interpolants(+,+,+,+)
%-----------------------------------------------------------------------------%
% Attempt at reducing the number of memo table entries by building
% disjunctive interpolants. This might be helpful for two reasons:
% reduce the number of the memo table and hence, the number of
% entailment tests, and it might increase the likelihood of
% subsumption.
%-----------------------------------------------------------------------------%
cache_gen_disjunctive_interpolants(Goal, ClId, Depth, Solver):-
        ( is_enabled_option(disj_memo) ->
	( is_enabled_option(unscoped_interpolation) ->
	  collapse_memo_entries(Goal,ClId,Depth,unscoped,Solver) 
	; 
	  true
	),
	( is_enabled_option(scoped_interpolation) ->
	  collapse_memo_entries(Goal,ClId,Depth,scoped,Solver) 
	; 
	  true
	)
        ;
	true
        ).
        
%--------------------------------------------------------------------------%
% collapse_memo_entries(+Goal,+ClId,+Depth,+WhichTable,+Solver)
%--------------------------------------------------------------------------%
% Merge memo table entries corresponding to same goal and clause id.
% Merging of two entries with interpolants I1 and I2 is done by
% creating a new entry with interpolant I1 or I2.
%--------------------------------------------------------------------------%
collapse_memo_entries(Goal, ClId, _Depth, WhichTable, Solver):-
	start_timer(collapse_memo_entries),	
	findall( Entry,
		( % Here we retract *complete* clauses
		  get_map_node_to_unique_id(Goal, ClId, _Depth, NodeId),
		  is_complete_clause(NodeId),
		  retract_intp_from_clause(NodeId, WhichTable, Entry)
		), 
		Ls),
	Ls = [InitEntry|Rest],
	!,
	% Here we combine them and assert a new complete clause with a
	% disjunctive interpolant.
	build_and_store_disj_intp(InitEntry, Rest, WhichTable, Solver),
	stop_timer(collapse_memo_entries,_).
collapse_memo_entries(_,_,_,_,_):- 
	stop_timer(collapse_memo_entries,_),
	!.

retract_intp_from_clause(NodeId, unscoped, entry(Goal,ClId,NodeId,Itp,Node,ClPrefix,PredPrefix)):-
	retract_fact('$intp_unscoped_table'(Goal,ClId,NodeId,Itp,Node,ClPrefix,PredPrefix)).
                                              
retract_intp_from_clause(NodeId, scoped, entry(Goal,ClId,NodeId,Itp,Node,ClPrefix,PredPrefix)):-
	retract_fact('$intp_scoped_table'(Goal,ClId,NodeId,Itp,Node,ClPrefix,PredPrefix)).

build_and_store_disj_intp(entry(Goal,ClId,NodeId,InitItp,Node,ClPrefix,PredPrefix),Es,WhichTable,Solver):-
	varset_attributes(Goal,NewVs),
	rename_and_merge_clause_intps(Es, NewVs, Solver, InitItp, EndItp),
	/*
            format("collapsing interpolants for: ",[]),
	  print_term_with_attribute(Goal),
	  format(" ---> ",[]),
	  print_solver_formula(Solver, EndItp), 
	  format("\n----------------------------\n",[]),
	*/
	( WhichTable == unscoped ->
	  asserta_fact('$intp_unscoped_table'(Goal, ClId, NodeId, EndItp, 
	                                      Node, ClPrefix, PredPrefix))
	;
	  asserta_fact('$intp_scoped_table'(Goal, ClId, NodeId, EndItp, 
                                              Node, ClPrefix, PredPrefix))  
	).

% produce a disjunctive interpolant 
rename_and_merge_clause_intps([], _, _, EndItp, EndItp):- !.
rename_and_merge_clause_intps([entry(Goal,_,_,Itp,_,_,_)|Es], NewVs, Solver, AccItp, EndAccItp):-
	incr_counter(num_of_merged_entries,_),
	varset_attributes(Goal,OldVs),	
	solver_rename_formula(Solver, Itp, OldVs, NewVs, RenItp),
	solver_disjoin_formulas(Solver,AccItp,RenItp,NewAccItp),	
	rename_and_merge_clause_intps(Es, NewVs, Solver, NewAccItp, EndAccItp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Print utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% To print formulas
print_solver_formula(Solver, F):-
 	solver_formula_to_str(Solver,F, Str),
	format("~q",[Str]).

debug_print_term_with_attribute(Term):-
	is_enabled_option(debug),
	!,
	print_term_with_attribute(Term).
debug_print_term_with_attribute(_):-!.

% To print memo tables

debug_print_intp_table(Solver):-
	is_enabled_option(debug), !,
	format("UNSCOPED INTERPOLANTS \n",[]),
	print_unscoped_intp_table(Solver),
	format("SCOPED INTERPOLANTS \n",[]),
	print_scoped_intp_table(Solver),
	format("PREDICATE INTERPOLANTS \n",[]),
	current_output(OutStream),
	print_pred_intp_table(OutStream,Solver).
debug_print_intp_table(_):-  !.

print_unscoped_intp_table(Solver):-
	'$intp_unscoped_table'(Goal,_ClauseId,NodeId,Itp,node(_,_,Depth,_),_,_),
	format("~q::",[NodeId]),
	print_term_with_attribute(Goal),
	format(" depth=~q ",[Depth]),
	(is_complete_clause(NodeId) -> 
	 format(" COMPLETE \n\t",[]) ;  format(" INCOMPLETE \n\t",[])),
 	solver_formula_to_str(Solver, Itp, ItpStr),
	format("~q\n",[ItpStr]),
	fail.
print_unscoped_intp_table(_).

:- push_prolog_flag(multi_arity_warnings,off).
print_scoped_intp_table(Solver):-
	'$intp_scoped_table'(Goal,_ClauseId,NodeId,Itp,node(_,_,Depth,_),_,_),
	format("~q::",[NodeId]),
	print_term_with_attribute(Goal),
	format(" depth=~q ",[Depth]),
	(is_complete_clause(NodeId) -> 
	 format(" COMPLETE \n\t",[]) ;  format(" INCOMPLETE \n\t",[])),
	solver_formula_to_str(Solver,Itp,ItpStr),
	format("~q\n",[ItpStr]),
	fail.
print_scoped_intp_table(_).

% This version is more user-friendly
print_scoped_intp_table(Stream, Solver):-
	'$intp_scoped_table'(Goal,_,_,Itp,_,ClPrefix,_),
	print_term_with_attribute(Stream, Goal),
	write(Stream,'   '),
	write(Stream,'call string: '),  write(Stream, ClPrefix), 
	nl(Stream),
	write(Stream,'\t'),
	solver_formula_to_str(Solver,Itp,ItpStr),
	write(Stream, ItpStr),
	nl(Stream), nl(Stream),
	fail.
print_scoped_intp_table(_,_).
:- pop_prolog_flag(multi_arity_warnings).

print_pred_intp_table(Stream, Solver):-
	'$intp_pred_table'(Goal,Itp,_Node,Prefix),
	print_term_with_attribute(Stream, Goal),
	write(Stream,'   '),
	write(Stream,'call string: '),  write(Stream,Prefix), 
	nl(Stream),
	write(Stream,'\t'),
	solver_formula_to_str(Solver,Itp,ItpStr),
	write(Stream, ItpStr),
	nl(Stream), nl(Stream),
	fail.
print_pred_intp_table(_,_).


% dump_interpolants(+atom,+ref)	
dump_interpolants(InFile, Solver):-
          % if pred_subsumption is enabled we only output the
          % "predicate" table although we could also output the
          % "clause" table if needed.
          is_enabled_option(dump_interpolants),
          is_enabled_option(pred_subsumption),
          !,
	path_splitext(InFile,Base,_Extension),	
	atom_concat(Base,'.pred.intp', OutFile),
	open(OutFile,write,Stream),	
	write(Stream, '=================================='), nl(Stream),
	write(Stream, 'Interpolation Cache '), nl(Stream),
	write(Stream, '=================================='), nl(Stream),
	print_pred_intp_table(Stream, Solver), 	
	close(Stream).
dump_interpolants(InFile, Solver):-
          is_enabled_option(dump_interpolants),
          is_enabled_option(scoped_interpolation),
          !,
	% we only output the table of well-scoped interpolants.
	path_splitext(InFile,Base,_Extension),	
	atom_concat(Base,'.clause.intp', OutFile),
	open(OutFile,write,Stream),	
	write(Stream, '=================================='), nl(Stream),
	write(Stream, 'Interpolation Cache '), nl(Stream),
	write(Stream, '=================================='), nl(Stream),
	print_scoped_intp_table(Stream, Solver), 	
	close(Stream).
dump_interpolants(_,_):- !.

 

