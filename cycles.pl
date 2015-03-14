% Jorge. A Navas, The University of Melbourne 2013

%===============================================================================%
%                           TO HANDLE CYCLES
%===============================================================================%

% WARNING: THIS FILE IS TO BE INCLUDED BY tclp.pl
%          DO NOT TRY TO LOAD IT DIRECTLY.

% Here we call a fake infeasible path if the path was infeasible due
% to some counter introduced by the the counter instrumentation used
% to force termination.

 

 


% initialize counters and some other stuff ...	
init_cycles(RecPreds, SCCs):-
        set_counter('$scc_id',0),
        process_scc(SCCs),
        % Very important for termination in case of non-terminating
        % clauses. We start by unrolling once each recursive clause
        % with counter equals to 1 by default.
        ( is_enabled_option(min_unroll(N1)) ->
          InitUnrollDepth=N1
        ;
          InitUnrollDepth=0
        ),
        ( is_enabled_option(max_unroll(N2)) ->
          MaxUnrollDepth=N2
        ;
          MaxUnrollDepth=0  % unlimited
        ),
        init_unroll_depth(RecPreds, InitUnrollDepth, MaxUnrollDepth),
        init_cycle_stats.

% cleanup internal facts and counters
cleanup_cycles:-
        cleanup_unroll_depth_counters,
        retractall_fact('$map_pred_to_scc'(_,_)),
        retractall_fact('$scc_terminating'(_,_)).

% if the interpreter needs to unroll recursive clauses
reset_cycles:-
       retractall_fact('$scc_terminating'(_,_)).
 
init_cycle_stats:-
        set_counter(num_invariant_checks, 0),
        set_counter(num_success_invariant_checks, 0),
        set_counter(num_fail_invariant_checks, 0).
        
print_cycle_stats:-
        get_counter_value(num_invariant_checks, N1),
        get_counter_value(num_success_invariant_checks, N2),
        get_counter_value(num_fail_invariant_checks, N3),
        format("CYCLE STATS\n",[]),
        format("\tNumber of invariant checks................... ~q\n",[N1]),
        format("\t\tNumber of successful invariant checks........ ~q\n",[N2]),
        format("\t\tNumber of failed invariant checks............ ~q\n",[N3]).

        
%------------------------------------------------------------------------------%%
% mark_fake_false(+Stack,+id,+list,+list,+Solver)
%-------------------------------------------------------------------------------%
% Decides which infeasible paths were spurious due to fake constraints
% used to force termination and which not. This is vital for
% correctness. The information has to be asserted because it has to be
% kept during backtracking. 
%-------------------------------------------------------------------------------%
:- data '$fake_false'/1.
mark_fake_false(Stack, Id, RecPreds, RecCls, Solver):-
          mark_fake_false_aux(Stack, Id, RecPreds, RecCls, Solver).
 

% During the counter instrumentation we keep track of which
% constraints were introduced to force termination (i.e, fake
% constraints) and which not. Whenever we detect an infeasible path we
% check whether the id of the last constraint that caused the
% infeasibility corresponds to a fake constraint.
mark_fake_false_aux(Stack, Id, _RecPreds, _RecCls, _Solver):-
	is_enabled_option(cycle_subsumption),
	goal_stack__previous_clause(Stack, Cl, _RestStack), 
 	Cl = clause(Goal,K,Depth,_,_,_), functor(Goal,F,A),
	( is_counter_inst_constraint(Id) ->
 
 	  debug_message("Detected FAKE INFEASIBLE PATH: clause ~q of ~q\n", [K,F/A]), 
	  ( is_enabled_option(no_verbose) -> true ;
  	    format("Execution speculatively stopped w/ clause ~q of ~q at depth ~q.\n", [K,F/A,Depth])), 
	  add_fake_false(F/A/K/Depth)
	;
	  % we fail if actual infeasible path
 	  debug_message("Detected ACTUAL INFEASIBLE PATH: clause ~q of ~q\n",[K,F/A]), 
            !, fail 
          ).
               
% add_fake_false(+F/A/K/Depth)
add_fake_false(Id):-
	retract_fact('$fake_false'(OldSet)),
	insert_set(OldSet, Id, NewSet),
	asserta_fact('$fake_false'(NewSet)).

 

% is_fake_false_id(+F/A/K/Depth)
is_fake_false_id(Id):-
        get_fake_false_ids(Ids),
        !,
        member(Id,Ids).

% get_fake_false_ids(-list)
get_fake_false_ids(Ids):-
	is_enabled_option(cycle_subsumption),
	!,
	current_fact('$fake_false'(Set)),
	set_to_list(Set,List),
	map_ids(List,Ids).
get_fake_false_ids([]):-!.

map_ids([],[]).
map_ids([F/A/K/Depth|Xs], [Y|Ys]):-
	functor(Goal,F,A),
	get_map_node_to_unique_id(Goal,K,Depth,Y),	
	map_ids(Xs,Ys).

% get_depth_from_lower_fake(+,+,-,-)
get_depth_from_lower_fake(F/A, Depth, K , LowerDepth):-
	current_fact('$fake_false'(Set)),
	!,
	lookup_set(Set,F/A/K/LowerDepth), 
	LowerDepth > Depth.

print_fake_nodes([],_Msg).
print_fake_nodes([F/A/K/Depth | Rs], Msg):-
        format("~w clause ~q of ~q at depth ~q.\n",[Msg, K,F/A,Depth]),
        print_fake_nodes(Rs, Msg).

%------------------------------------------------------------------------------%
% promote_fake_infeasible_to_actual.
% ------------------------------------------------------------------------------%
% Try to find a "fake infeasible" node @ChildNode with an ancestor
% @AncestorGoal which subsumes it. If yes, then no further exploration
% of the node is needed.
%
% By "fake infeasible" we mean that we halted the execution of
% @ChildNode by adding fake constraints of the form K>=0 where K is a
% special variable that is decremented whenever a recursive call is
% encountered. Thus, @ChildNode's interpolants might contain
% constraints over K. promote_fake_infeasible_to_actual will attempt
% at getting rid of these fake constraints while preserving the
% infeasibility of all the paths.
%
% Note that we start from an ancestor @AncestorGoal to promote a
% descendant (@ChildGoal) rather than starting directly from the
% descendant. This makes the code much simpler and the number of
% invariant checks are pretty much the same in the end.
%------------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).

% promote_fake_infeasible_to_actual(+,+,+,+).
% This version uses the "predicate" memo table to generate candidates
promote_fake_infeasible_to_actual(AncestorGoal, Depth, Solver, OutStream):-
	is_enabled_option(cycle_subsumption),
	is_enabled_option(pred_subsumption),
	% Try to find an @AncestorNode's descendant which was marked
	% as "fake" infeasible.
	functor(AncestorGoal, F, A),
	get_depth_from_lower_fake(F/A,  Depth, ClId, LowerDepth),
	get_map_node_to_unique_id(AncestorGoal,ClId,LowerDepth,ChildNodeId),
	debug_message("Promoting \"fake\" infeasible nodes using PRED table ... \n", []),
	debug_message("\tAncestor: ~q/~q/~q depth ~q\n",[F,A,ClId,Depth]),
	debug_message("\tChild   : ~q/~q/~q depth ~q\n",[F,A,ClId,LowerDepth]),
	% We use the "clause" memo table just to get the prefix from
	% the root to the descendant.
	lookup_intp_clause_tables(_,_, ChildNodeId, _, ChildNode, ChildPrefix),
	debug_message("\tFound prefix from root to child   :~q\n",[ChildPrefix]),
	AncestorNode = node(_,_,Depth,_),
	% This succeeds if there is available an interpolant (from
	% the "predicate" table) for @AncestorNode at depth @Depth.
	lookup_intp_pred_table(AncestorGoal,AncestorIntp,AncestorNode,AncestorPrefix),
	debug_message("\tFound prefix from root to ancestor:~q\n",[AncestorPrefix]),
	% This succeeds if @AncestorNode and @ChildNode have a
	% **common** prefix in the execution tree.
	get_suffix(AncestorPrefix, ChildPrefix, ChildSuffix),
 
	% Finally, we check if the candidate is indeed invariant
	% along all the paths.
	check_invariant(F/A/ClId, LowerDepth, Depth, 
	                AncestorGoal, ChildSuffix, AncestorIntp, 
		      AncestorNode, ChildNode, Solver, OutStream),
	%%%%
	% An ancestor node could prove the invariance of more than
	% one descendant. We force failure here to check the
	% invariance of all of them.
	fail,
	%%%%
	!.	
promote_fake_infeasible_to_actual(_,_,_,_):- !.

% promote_fake_infeasible_to_actual(+,+,+,+,+,+)
% This version uses the "clause" memo table to generate candidates.
promote_fake_infeasible_to_actual(AncestorGoal, ClId, Depth, AncestorNodeId, Solver, OutStream):-
	is_enabled_option(cycle_subsumption),
	is_enabled_option(clause_subsumption),
	% Try to find an @AncestorNode's descendant which was marked
	% as "fake" infeasible.
	functor(AncestorGoal, F, A),
	get_depth_from_lower_fake(F/A,  Depth, ClId, LowerDepth),
	get_map_node_to_unique_id(AncestorGoal,ClId,LowerDepth,ChildNodeId),
	debug_message("Promoting \"fake\" infeasible nodes using CLAUSE table ... \n", []),
	debug_message("\tAncestor: ~q/~q/~q depth ~q\n\t",[F,A,ClId,Depth]),
	debug_message("\tChild   : ~q/~q/~q depth ~q\n",[F,A,ClId,LowerDepth]),
	% We use the "clause" memo table just to get the prefix from
	% the root to the descendant.
	lookup_intp_clause_tables(_,_, ChildNodeId, _, ChildNode, ChildPrefix),
	debug_message("\tFound prefix from root to child   :~q\n",[ChildPrefix]),
	% This succeeds if there is available an interpolant (from
	% the "clause" table) for @AncestorNodeId. This interpolant
	% is the candidate for the invariant test.
 	lookup_intp_clause_tables(AncestorGoal, _, AncestorNodeId, 
	                          AncestorIntp, AncestorNode, AncestorPrefix0),
	% This succeeds if @AncestorNode and @ChildNode have a
	% **common** prefix in the execution tree.
	remove_last_functor_from_str_path(AncestorGoal, ClId, AncestorPrefix0, AncestorPrefix),
	debug_message("\tFound prefix from root to ancestor:~q\n",[AncestorPrefix]),
	get_suffix(AncestorPrefix, ChildPrefix, ChildSuffix),
	% Finally, we check if the candidate is indeed invariant
	% along all the paths.
	check_invariant(F/A/ClId, LowerDepth, Depth, 
	                AncestorGoal, ChildSuffix, AncestorIntp, 
		      AncestorNode, ChildNode, Solver, OutStream),
	%%%%
	% An ancestor node could prove the invariance of more than
	% one descendant. We force failure here to check the
	% invariance of all of them.
	fail,
	%%%%
	!.	
promote_fake_infeasible_to_actual(_,_,_,_,_,_):- !.
:- pop_prolog_flag(multi_arity_warnings).

%-------------------------------------------------------------------------%
% This predicate always succeeds but it has side-effects. Check
% whether the candidate AncestorIntp is an invariant along the *all*
% paths from an ancestor to its descendant.
%-------------------------------------------------------------------------%
% check_invariant(+,+,+,+,+,+,+,+,+,+)
%-------------------------------------------------------------------------%
check_invariant(F/A/ClId, LowerDepth, Depth, 
                AncestorGoal, ChildSuffix, AncestorIntp, 
	      AncestorNode, ChildNode, Solver, OutStream):-

        debug_message("-------- BEGIN INVARIANT CHECK -----------\n",[]),
        ( check_invariant_incr(AncestorGoal, ChildSuffix, AncestorIntp, Solver) ->
	( is_enabled_option(no_verbose) -> 
            true  
          ;
   	  format("FAILED to subsume clause ~q of ~q at depth ~q.\n", [ClId,F/A,LowerDepth])
	)
        ;
	( is_enabled_option(no_verbose) -> 
            true 
          ;
	  format("PROVED that clause ~q of ~q at depth ~q ", [ClId, F/A, LowerDepth]),
	  format("is subsumed by clause ~q of ~q at depth ~q\n", [ClId, F/A, Depth])
	),
	retract_fact('$fake_false'(OldSet)),
	remove_set(OldSet, F/A/ClId/LowerDepth, NewSet),	  
	asserta_fact('$fake_false'(NewSet)),
	% Remove from the clause memo table the entry
	% corresponding to the subsumed descendant node because we
	% don't want to use this node to subsume others.
	% I think we could replace the descendant's interpolant
	% with its ancestor's interpolant.
	delete_clause_intp_table(ChildNodeId),
	debug_message("Deleted from memo subsumed descendant's id:~q\n",[ChildNodeId]),
	%%%%
	% DOT output
	%%%%
	% FIXME: AncestorNode refers to the first (not necessarily
	% recursive) clause of the predicate that appears in the
	% program. To avoid confusion, we display in the dot file
	% that the current goal is subsumed by another recursive clause.
	arg(4,AncestorNode,CtxId),
	mark_subsumed_trans(ChildNode, node(AncestorGoal,ClId,Depth,CtxId), OutStream)
       ),
       debug_message("-------- END INVARIANT CHECK -----------\n",[]).


rename_and_negate(OldFormula, OldGoal, Solver, NewGoal, NotNewFormula):-
	varset_attributes(OldGoal ,OldVs),
	varset_attributes(NewGoal ,NewVs),
	solver_rename_formula(Solver, OldFormula , OldVs, NewVs, NewFormula0),
	solver_mk_not(Solver, NewFormula0, NotNewFormula).

 

%-------------------------------------------------------------------------------%
% Check if a candidate is invariant incrementally. Note that it may
% cover a large tree. We don't use interpolation so it could be
% expensive. Not sure whether the use of interpolation may make the
% process of getting invariants harder. We could assert a disjunctive
% formula into the solver as an option.
%-------------------------------------------------------------------------------%
% Post: the candidate @AncestorIntp0 is invariant if
% check_invariant_incr/4 fails.
%-------------------------------------------------------------------------------%
check_invariant_incr(AncestorGoal, StrPath, AncestorIntp0, Solver):-
          solver_sibling_reset(Solver),
	abstract_str_path(StrPath, NonDetStrPath),
	% The execution of NonDetPath will produce a tree because some
	% clauses are undetermined.
	convert_str_path_to_term_list(NonDetStrPath, NonDetPath),
	abstract_instr_counter(AncestorIntp0, AncestorGoal, Solver, AncestorIntp),
	solver_sibling_push(Solver),
 
	( solver_is_true(Solver, AncestorIntp) ->
	    % success means it is not invariant   
 
	    solver_sibling_pop(Solver)
	;
	  ( solver_is_false(Solver, AncestorIntp) ->
              % success means it is not invariant   
 
	    solver_sibling_pop(Solver)
            ;                
	    solver_sibling_assert_formula(Solver, AncestorIntp),	  
 
	    ( is_enabled_option(no_verbose) -> true ;
	        format("PATH INVARIANT CANDIDATE ",[]),
	        print_term_with_attribute(AncestorGoal),
	        solver_formula_to_str(Solver, AncestorIntp, AncestorIntp_Str),
	        format("\n\t~q\n",[AncestorIntp_Str])),
	    %%%%%
	    % Important: loop by failure here to check all the paths
	    % between the ancestor and its descendant.
	    %%%%%
	    execute_path([AncestorGoal], NonDetPath,_,Solver, AncestorGoal, DescendantGoal),
	    % We want to prove that I(x) and C(x) and B(x,x') and not I(x') is unsat
	    % but it might be the case that I(x) and C(x) and B(x,x') is unsat
	    % In that case, execute_path/6 will fail which means that the invariant 
	    % trivially holds.
	    rename_and_negate(AncestorIntp,AncestorGoal, Solver, DescendantGoal,NotDescendantIntp0),
	    abstract_instr_counter(NotDescendantIntp0, DescendantGoal,Solver, NotDescendantIntp),
	    solver_sibling_push(Solver),
	    solver_sibling_assert_formula(Solver, NotDescendantIntp),
	    solver_sibling_incremental(Solver,[],SatFlag),
   	    ( is_enabled_option(no_verbose) -> true ;
 	      print_checked_vc(NonDetStrPath, Solver, SatFlag)),
	    %%%
	    % This will fail if the constructed vc is invariant.
	    % this will trigger to check the rest. If succeed then
	    % means the candidate is not invariant.
	    %%%% 
	    continue_check_invariant_incr(SatFlag, Solver)
            )  
          ), 
          !.

% to decide if we need to keep checking paths
continue_check_invariant_incr(SatFlag, Solver):-
        SatFlag == tt,
        incr_counter(num_invariant_checks,_),
        incr_counter(num_fail_invariant_checks,_),
        solver_sibling_reset(Solver).
continue_check_invariant_incr(SatFlag, Solver):-
        SatFlag == ff,
        incr_counter(num_invariant_checks,_),
        incr_counter(num_success_invariant_checks,_),
        solver_sibling_pop(Solver),
        !,
        fail.

% for debugging purposes
print_checked_vc(NonDetStrPath, Solver, SatFlag):-
        is_enabled_option(debug),
        !,
        set_prolog_flag(write_strings,on),
        format("CYCLE TO CHECK: ~q\n",[NonDetStrPath]),
        solver_sibling_print(Solver),
        ( SatFlag == ff -> 
	format("***Path is INVARIANT!\n",[]) 
        ; 
	format("***Path is NOT invariant.\n",[])
        ).
print_checked_vc(_,_,_):- !.

%-------------------------------------------------------------------------------%
% abstract_instr_counter(+,+,+,-)
%-------------------------------------------------------------------------------%
% Pre: last argument is the counter to force termination.
% This is ensured by a program transformation.
%-------------------------------------------------------------------------------%
abstract_instr_counter(Formula, Goal, Solver, NewFormula):-
        	functor(Goal,F,N),
	get_var_attribute(Goal, N, K), 
	% we replace K with 0 - depth
	current_fact('$unroll_depth'(F/N, Val, _MaxVal)),
	NewVal is 0 - Val,
	solver_rename_formula(Solver,  Formula , [K], [NewVal], NewFormula),
	!.

%-------------------------------------------------------------------------------%
% execute_path(+,+,+,+,+,-)	
%-------------------------------------------------------------------------------%
% Assert in the solver all the constraints originated from the
% execution of @Goals guided by @Prefix.
%-------------------------------------------------------------------------------%
execute_path(Goals, Prefix, Suffix, Solver, InitG, LastG) :-
	execute_path_aux(Goals, Prefix, Suffix0, Solver, InitG, LastG0),
	( Suffix0 == [] ->
	  Suffix=Suffix0, LastG=LastG0
	;
	 list_head_tail(Suffix0,NextGoal,_),
	 execute_path([NextGoal],Suffix0,Suffix,Solver,LastG0,LastG)
          ).

execute_path_aux([], Suffix, Suffix,  _, LastG, LastG):- !.
execute_path_aux([constraints(Cs,_)| Goals], Suffix, EndSuffix, Solver,
                 LastG0,LastG1):-
	execute_constraints(Cs, Solver),
	execute_path_aux(Goals, Suffix, EndSuffix, Solver, LastG0, LastG1).
execute_path_aux([constraints(_Cs,_)| _Goals], _Suffix, _EndSuffix, Solver,_,_):-
	solver_sibling_pop(Solver),
	fail.
execute_path_aux([builtin(Builtin,_,Module)| Goals], Suffix, EndSuffix, Solver, 
                 LastG0,LastG1):- 
	wrapper_run_builtin(Module, Builtin),
	execute_path_aux(Goals, Suffix, EndSuffix, Solver, LastG0,LastG1).
execute_path_aux(Goals, [Goal/_], [], _Solver,_,Goal):-
	list_head_tail(Goals,GoalX,_),
	Goal = GoalX.
	%!.
execute_path_aux(Goals, [Goal/K | RestSuffix], EndSuffix, Solver,
                 _LastG0,LastG2):-
	list_head_tail(Goals,GoalX,RestGoals),
	Goal = GoalX,
	% K may be free or ground
          rule(Goal, K, Body),
	execute_clause(Goal, Body, RenamedBody, Solver),
	execute_body(RenamedBody, RestSuffix, NextSuffix, Solver, Goal, 
                       LastG1),  			      
	execute_path_aux(RestGoals, NextSuffix, EndSuffix, Solver, 
	                 LastG1, LastG2).
 
execute_constraints(Cs, Solver):-
          convert_clp_constraints_to_solver(Cs, SolverFormatCs, _OutFormatCs),
	solver_sibling_push(Solver),
	solver_sibling_incremental(Solver, SolverFormatCs, SatFlag),
	SatFlag == tt.
	
execute_body(Body, Prefix, Suffix, Solver, LastG0, LastG1):-
	execute_path_aux(Body, Prefix, Suffix, Solver, LastG0, LastG1).

execute_clause(Goal, BodyCl, BodyClX, Solver):-
 
          extract_equalities_from_last_unification(Goal,[],_,_),
	rename_clause(Goal-BodyCl, Solver, _, GoalX-BodyClX),
	extract_equalities_from_last_unification(GoalX,[],_,HeadGoalCs),
	execute_clause_aux(HeadGoalCs, Solver).
execute_clause_aux([], Solver):-
	solver_sibling_push(Solver).
execute_clause_aux(HeadGoalCs, Solver):-
          HeadGoalCs = [_|_],
	solver_sibling_push(Solver),
	solver_sibling_incremental(Solver, HeadGoalCs, SatFlag),
	SatFlag == tt.
execute_clause_aux(_, Solver):-
	solver_sibling_pop(Solver),
	fail.

list_head_tail([X]   , X,  []) :- !.
list_head_tail([X|Xs], X,  Xs) :- !.
list_head_tail(Xs    ,E, Ys) :- 
	format("ERROR ftclp: list_head_tail(~q,~q,~q)\n",[Xs,E,Ys]),
	halt.

%--------------------------------------------------------------------------%
% get_suffix(+,+,-)
%--------------------------------------------------------------------------%
% @Suffix is a string of the form "F/N/K-...-F/N/K"
% @Suffix is the suffix after the common prefix between
% @AncestorPrefix and @ChildPrefix. 
%--------------------------------------------------------------------------%
get_suffix(AncestorPrefix, ChildPrefix, Suffix):-
	%format("Ancestor: ~q\n",[AncestorPrefix]),
	%format("Child   : ~q\n",[ChildPrefix]),
          mk_dash(X), 
 	append(AncestorPrefix,[X|Suffix],ChildPrefix),
	%format("Suffix  : ~q\n",[Suffix]),
          !,
	% Ensure that Suffix corresponds to a cycle: more than two
	% keys involved and the first and last are the same key.
	% Note that we allow different clauses from the same
	% predicate.
          split(Suffix,[X],ListKeys),    
	ListKeys = [First,_|_],
	last(ListKeys,Last),
	extract_pred_key_from_str(First, PFirst),
	extract_pred_key_from_str(Last , PLast),
	PFirst = PLast.

extract_pred_key_from_str(Str,P/N):-
          Slash=47,  % ASCII code for "/"
          split(Str,[Slash],List),
	List=[X,Y|_],
	atom_codes(P,X),
	number_codes(N,Y).

%--------------------------------------------------------------------------%
% To control the unrolling of each recursive clause.
%
% For each predicate we keep track of the current unrolling depth.  In
% addition, we keep track of the unrolling depth for each SCC by
% taking the maximum value of all of its members. Then, when we
% generate constraints to force termination we use the values from the
% SCCs rather than individual predicates.
% --------------------------------------------------------------------------%
 

% '$unroll_depth'(+F/A,+CurrValue,+MaxValue)
:- data '$unroll_depth'/3.
% '$unroll_depth_scc'(+SCC_Id,+Value)
:- data '$unroll_depth_scc'/2.

cleanup_unroll_depth_counters:-
	retractall_fact('$unroll_depth'(_,_,_)),
	retractall_fact('$unroll_depth_scc'(_,_)).

% init_unroll_depth(+RecPredLs, +InitVal, +MaxVal)
% RecPredLs is a list of F/A terms and InitVal is a number.
init_unroll_depth([],_,_).
init_unroll_depth([F/A|Ps], InitVal, MaxVal):-
          % predicate counter
	asserta_fact('$unroll_depth'(F/A, InitVal, MaxVal)),
	% scc counter
          current_fact('$map_pred_to_scc'(F/A, SCC)),
          ( 
	  current_fact('$unroll_depth_scc'(SCC, _)) -> true ;
	  asserta_fact('$unroll_depth_scc'(SCC, InitVal))
	),
	init_unroll_depth(Ps, InitVal, MaxVal).

% incr_unroll_depth(+,+,-)
% If ReachedMaxFlag > 0 then one the clauses has exceeded the number
% of unrollings.
incr_unroll_depth(Ps, Incr, ReachedMaxFlag):-
          unzip_to_keep_only_pred(Ps, Ks_u),
	sort(Ks_u, Ks), % to remove duplicates
	% predicate counters
	False=0, 
	incr_unroll_depth_pred(Ks, Incr, False, ReachedMaxFlag),
	% scc counters
	update_unroll_depth_scc(Ks).
                
incr_unroll_depth_pred([], _, F0, F0).
incr_unroll_depth_pred([F/A|Xs], Incr, F0, F2):-
	retract_fact('$unroll_depth'(F/A, OldVal, MaxVal)),
	NewVal is OldVal + Incr,
	asserta_fact('$unroll_depth'(F/A, NewVal, MaxVal)),
	( (MaxVal > 0, NewVal > MaxVal) ->
	   F1 = F0 + 1  % maximum unroll has been exceeded.
	;
	   F1 = F0
          ),
	( is_enabled_option(no_verbose) -> true ;
	  format("Increasing unrolling depth of ~q to ~q\n",[F/A, NewVal])),
	incr_unroll_depth_pred(Xs, Incr, F1, F2).

update_unroll_depth_scc([]).
update_unroll_depth_scc([P|Ps]):-
        current_fact('$unroll_depth'(P, P_Val, _MaxVal)),
        current_fact('$map_pred_to_scc'(P, SCC)),
        current_fact('$unroll_depth_scc'(SCC, SCC_Val), Ref),
        ( P_Val > SCC_Val ->
	erase(Ref),
	asserta_fact('$unroll_depth_scc'(SCC, P_Val))
        ;
	true
        ),
        update_unroll_depth_scc(Ps).

unzip_to_keep_only_pred([], []).
unzip_to_keep_only_pred([F/A/_Cl/_Depth|Xs],[F/A|Ys]):- 
        unzip_to_keep_only_pred(Xs,Ys).

%-------------------------------------------------------------------------------%
% process_scc(+list)
%-------------------------------------------------------------------------------%
% Each predicate from a recursive SCC is mapped to the same
% identifier.
%-------------------------------------------------------------------------------%
:- data '$map_pred_to_scc'/2.
process_scc([]).
process_scc([(non_recursive,_) |SCCs]):-
        process_scc(SCCs).
process_scc([(recursive, Ps) |SCCs]):-
        incr_counter('$scc_id' ,Id),
        add_map_pred_to_scc(Ps, Id),
        process_scc(SCCs).

add_map_pred_to_scc([],_).
add_map_pred_to_scc([P|Ps],Id):-
        asserta_fact('$map_pred_to_scc'(P,Id)),
        % debug_message("~q is mapped to SCC id ~q\n",[P,Id]),
        add_map_pred_to_scc(Ps,Id).

%-------------------------------------------------------------------------%
% generate_terminating_constraint(+,+,+,-)
% Fail if no constraint is generated.
%-------------------------------------------------------------------------%
generate_terminating_constraint(Goal, RecPreds, Stack, C):-
	is_enabled_option(cycle_subsumption),
	functor(Goal, F, A),
	% goal must be recursive and first occurrence in the path
	( member(F/A, RecPreds), 
	  \+(is_another_scc_comp_terminating(F/A)),
	  \+(goal_stack__is_variant_member(Stack, F, A))),

	% Pre: last argument is the counter to force termination.
	%      This is ensured by program transformation.
	get_var_attribute(Goal, A, K), 
	%current_fact('$unroll_depth'(F/A, Val, _MaxVal)),
	current_fact('$map_pred_to_scc'(F/A, SCC)),
	current_fact('$unroll_depth_scc'(SCC, SCC_Val)),
	debug_message("Adding constraint K=~q on ~q to force termination\n",[SCC_Val, F/A]),
	C = '.=.'(K, SCC_Val),
	mark_scc_terminating(F/A).

%-------------------------------------------------------------------------------%
% is_another_scc_comp_terminating(+F/A)
%------------------------------------------------------------------------------%
% Succeed if there exists another predicate from the same SCC which
% has been forced to terminate.
%-------------------------------------------------------------------------------%
:- data '$scc_terminating'/2.
is_another_scc_comp_terminating(P):-
	 current_fact('$map_pred_to_scc'(P, SCC_Id)),
           current_fact('$scc_terminating'(Q, SCC_Id)),
	 \+(P=Q).

%------------------------------------------------------------------------------%
% mark_scc_terminating(+F/A)
%------------------------------------------------------------------------------%
mark_scc_terminating(P):-
	 current_fact('$map_pred_to_scc'(P, SCC_Id)),
           asserta_fact('$scc_terminating'(P, SCC_Id)),
	 %format("Forcing ~q to terminate\n",[P]),
	 !.
mark_scc_terminating(P):-
           format("ERROR ftclp: mark_scc_terminating failed for ~q\n",[P]),
	 halt.
	
