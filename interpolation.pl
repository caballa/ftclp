% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%==============================================================================%
%                     Compute interpolants
%==============================================================================%
% This file implements all the machinery to generate both well scoped
% and out-of-scoped interpolants.
%
% An interpolant is well scoped with respect to a head clause H
% (predicate P) if only contains variables of H (P). Otherwise, we say
% it is an out-of-scoped interpolant. Ideally, we would like to have
% scoped interpolants but they are more expensive to compute.  
%==============================================================================%
:- module(interpolation      , [init_interpolation_stats/0,
                                print_interpolation_stats/0,
			  init_interpolation_profiling_timers/0,
			  print_interpolation_profiling_timers/0,
	                      clear_interpolation/0,
			  gen_interpolants/4,
	                      intp_adt__init/1,
			  intp_adt__push/3
			 ],[condcomp]).
%  Ciao libraries
:- use_module(library(sets)  , [insert/3, ord_union/3, 
	                      ord_intersection_diff/4]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(lists) , [append/3]).
:- use_module(library(format), [format/2]).
:- use_module(library(sort ) , [sort/2]).
%  Own libraries
:- use_module(cache          , [lazy_update_intp_memotable/14,
	                      debug_print_intp_table/1]).
:- use_module(counters).
:- use_module('adt/union_find').
:- use_module(options).
:- use_module(solver).
:- use_module(debug).
:- use_module(timer).
:- use_module(op_attributes).

%------------------------------------------------------------------------------%
% IMPORTANT: we compute sequence interpolants but they are not
% necessarily inductive.
%------------------------------------------------------------------------------%
% A priori the idea of generating "sequence" interpolants directly
% from the SMT solver is very attractive. By sequence interpolants we
% mean to obtain all the intermediate interpolants for a given
% unsatisfiable formula from only one satisfiability check. In order
% to do this, the only requirement is to add interpolant groups (also
% called cutpoints) during the forward execution.
%
% It turned out that this is very expensive because current SMT
% technology (at least MathSat) does not perform important
% simplifications across cutpoints. Therefore, satisfiability checks
% are too expensive. Thus, we mimic a n-length sequence interpolant by
% calling the interpolation algorithm n times.
%
% Therefore, it is vital to reduce as much as possible the number of
% interpolants. This is why we implemented a lazy version which starts
% bottom-up along the execution path and stops as soon as certain
% conditions hold (in the paper we show that under those conditions we
% can stop safely).
%------------------------------------------------------------------------------%
% The generation of well-scoped interpolants is more elaborated:
%
% - We say a constraint c1 is **syntactically related** to another
%   constraint c2 if vars(c1) \cap vars(c2) is not empty or there is
%   another constraint c3 such that vars(c1) \cap vars(c3) is not
%   empty and c3 is syntactically related to c2 through transitive
%   closure.
% 
%   This concept is needed for generating properly scoped
%   interpolants.
%
%   It is implemented partially during the forward execution by
%   computing the transitive closure of the primitive constraints
%   during the forward execution of the program. This is done by
%   incr_trans_closure/3.
 
%------------------------------------------------------------------------------%
% For efficiency reasons, we do hashing of CLP constraints so that
% comparisons can be quicker. Also, constraints are sometimes
% represented as a pointer to the internal representation kept by the
% underlying solver. This makes code harder to understand but it
% avoids expensive translations.
%------------------------------------------------------------------------------%

%  From the program transformation done by tclp_tr
:- multifile no_cache/1.

clear_interpolation :-
    retractall_fact('$map_constraint'(_,_,_,_,_)),
    retractall_fact('$cache_list_solver_term_varset'(_,_)),
    retractall_fact('$cache_map_constraints_to_intp_groups'(_,_)).

init_interpolation_stats:-
    %% incremented in cache.pl
    set_counter(inductiveness_checks, 0),
    set_counter(inductive_intp, 0),
    set_counter(noninductive_intp, 0).

print_interpolation_stats:-
    get_counter_value(inductiveness_checks, N1),
    get_counter_value(inductive_intp, N2),
    get_counter_value(noninductive_intp, N3),
    format("INTERPOLATION STATS\n",[]),
    format("\tNumber of inductiveness checks................... ~q\n",[N1]),
    format("\t\tNumber of inductive interpolants......... ~q\n",[N2]),
    format("\t\tNumber of noninductive interpolants...... ~q\n",[N3]).

init_interpolation_profiling_timers:-
    mk_timer(unscoped_interpolation_total),
    mk_timer(unscoped_interpolation_prep),
    mk_timer(unscoped_interpolation_gen_check_unsat), 
    mk_timer(unscoped_interpolation_gen),
    mk_timer(unscoped_interpolation_gen),
    mk_timer(unscoped_interpolation_store),
    mk_timer(scoped_interpolation_prep_1),
    mk_timer(scoped_interpolation_prep_2),
    mk_timer(scoped_interpolation_prep_3),
    mk_timer(scoped_interpolation_prep_4),
    mk_timer(scoped_interpolation_gen),
    mk_timer(scoped_interpolation_store).

print_interpolation_profiling_timers :-
    get_timer(unscoped_interpolation_prep,T0),
    get_timer(unscoped_interpolation_gen,T1),
    get_timer(unscoped_interpolation_store,T2),
    get_timer(unscoped_interpolation_gen_check_unsat,T28),
    get_timer(unscoped_interpolation_total,T15),
    format("Unscoped interpolation\n",[]),
    format("\tTOTAL........................................... ~q ms \n",[T15]),
    format("\t   preprocessing................................ ~q ms \n",[T0]),
    format("\t   checking unsat............................... ~q ms \n",[T28]),
    format("\t   generating interpolant....................... ~q ms \n",[T1]),
    format("\t   store........................................ ~q ms \n",[T2]),
    get_timer(scoped_interpolation_prep_1,T11),
    get_timer(scoped_interpolation_prep_2,T12),
    get_timer(scoped_interpolation_prep_3,T13),
    get_timer(scoped_interpolation_prep_4,T18),
    get_timer(scoped_interpolation_gen,T4),
    get_timer(scoped_interpolation_store,T5),
    T19 is T11 + T18,
    T3 is T19 + T12 + T13,
    T20 is T3 + T4 + T5,
    format("Scoped interpolation\n",[]),
    format("\tTOTAL .......................................... ~q ms \n",[T20]),
    format("\t   preprocessing ............................... ~q ms \n",[T3]),
    format("\t\t   transitive closure................... ~q ms \n",[T19]),	
    format("\t\t\t incremental part during forward execution..... ~q ms \n",[T18]),	
    format("\t\t\t rest during the interpolation call............ ~q ms \n",[T11]),
 
    format("\t\t   form A and B......................... ~q ms \n",[T13]),
    format("\t   interpolation solving ....................... ~q ms \n",[T4]),
    format("\t   store ....................................... ~q ms \n",[T5]).

        
%------------------------------------------------------------------------------%
% gen_interpolants(+StackADT,+list(num),+Solver,+term)
% gen_interpolants(+StackADT,+ItpADT,+Solver,SubsumedItpInfo)
%------------------------------------------------------------------------------%	
% Main wrapper that either calls well-scoped or out-of-scoped
% interpolation.
%
% SubsumedItpInfo is information needed to compute the interpolants
% from a subsumed derivation. 
%------------------------------------------------------------------------------%
 
gen_interpolants(Stack, TrackedItpInfo, Solver, SubsumedItpInfo):-	
	is_enabled_option(unscoped_interpolation),
	!,
	gen_unscoped_interpolants_lazily(Stack, TrackedItpInfo, Solver, SubsumedItpInfo),
	debug_message("##########################################\n",[]),
	debug_message("                Cache                     \n",[]),
	debug_message("##########################################\n",[]),
	debug_print_intp_table(Solver),
	debug_message("##########################################\n",[]).
gen_interpolants(Stack, TrackedItpInfo, Solver, SubsumedItpInfo):-	
	is_enabled_option(scoped_interpolation),
	!,
	gen_scoped_interpolants_lazily(Stack, TrackedItpInfo, Solver,SubsumedItpInfo),
	debug_message("##########################################\n",[]),
	debug_message("                Cache                     \n",[]),
	debug_message("##########################################\n",[]),
	debug_print_intp_table(Solver),
	debug_message("##########################################\n",[]).
gen_interpolants(_,_,_,_):- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  BEGIN UNSCOPED INTERPOLANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------%
% gen_unscoped_interpolants_lazily(+,+,+,+)
%-------------------------------------------------------------------------------%
% The idea is to compute interpolants bottom-up in a lazy manner. If
% for some point p we have already compute an interpolant I_p and the
% new interpolant I_p' is implied by I_p then we can safely stop and
% ignore the rest of the stack. This is a major improvement since it
% reduces significantly the number of interpolants.
%-------------------------------------------------------------------------------%

:- if(defined(incr_assert_cutpoints)).
 
:- else.
% This version mimics n-length sequence interpolants by calling
% interpolation algorithm n times.
gen_unscoped_interpolants_lazily(Stack, TrackedItpInfo, Solver, 
	                       % last argument if the derivation was subsumed
	                       SubsumedIntpInfo):-
 	start_timer(unscoped_interpolation_total),
	intp_adt__get_unscoped(TrackedItpInfo, Css),
	start_timer(unscoped_interpolation_prep),
	solver_intp_sibling_push(Solver),
	solver_intp_sibling_push_intp_group(Solver, _),
	% If the interpolant to be generated is from a subsumed
	% derivation then we need to assert into the solver the
	% negated interpolant.
	assert_subsumed_interpolant(SubsumedIntpInfo, Solver),
	% This asserts all the constraints along the path together
	% with interpolation cutpoints.
	add_intp_cutpoints(Stack,Css,ItpGroups,Solver),
	stop_timer(unscoped_interpolation_prep,_),
	start_timer(unscoped_interpolation_gen_check_unsat),
	solver_intp_sibling_check_sat(Solver,_SatFlag),
	% ( SatFlag == tt -> 
	%   format("Interpolation query must be unsat.\n",[]),
	%   halt
	% ;
	%   true
          % ),
	stop_timer(unscoped_interpolation_gen_check_unsat,_),
	% This predicate also checks the sequence interpolant is
	% inductive.
	gen_store_unscoped_intp(Stack,Css,ItpGroups,_PrevItp,tt,Solver),
	start_timer(unscoped_interpolation_prep),
	% reset is much faster than popping the stack!
	solver_intp_sibling_reset(Solver),
	stop_timer(unscoped_interpolation_prep,_),
 	stop_timer(unscoped_interpolation_total,_),
	!.
gen_unscoped_interpolants_lazily(_,_,_,_):-
          format("ERROR ftclp: gen_unscoped_interpolants_lazily/4 failed.\n",[]),
          halt.

%----------------------------------------------------------------------------%
% add_intp_cutpoints(+Stack,+Css,-ItpGroups,+Solver)
%----------------------------------------------------------------------------%
% Visit Stack and assert all constraints into Solver together with a
% choice point *and* an interpolation cutpoint whenever a clause of
% interest of visited.
%----------------------------------------------------------------------------%
add_intp_cutpoints([], [], [], _).
add_intp_cutpoints([_| RestStack], [[]| RestCs], ItpGroups, Solver):-
	add_intp_cutpoints(RestStack, RestCs, ItpGroups, Solver).
add_intp_cutpoints([clause(Head,_,_,_,_,_)| RestStack], [Cs| RestCss], 
	         ItpGroups, Solver):-
	( term_without_solver_vars(Head) ; no_cache(Head) ),
	solver_intp_sibling_assert(Solver, Cs),
	add_intp_cutpoints(RestStack, RestCss, ItpGroups, Solver).
add_intp_cutpoints([clause(_,_,_,_,_,_)|RestStack], [Cs|RestCss],
	         [ItpGroup | ItpGroups], Solver):-
	solver_intp_sibling_push(Solver),
	solver_intp_sibling_push_intp_group(Solver, ItpGroup),
	solver_intp_sibling_assert(Solver,Cs),
	add_intp_cutpoints(RestStack, RestCss, ItpGroups, Solver).
add_intp_cutpoints([_| RestStack], [Cs|RestCss], ItpGroups, Solver):-
	solver_intp_sibling_assert(Solver,  Cs),
	add_intp_cutpoints(RestStack, RestCss, ItpGroups, Solver).

%----------------------------------------------------------------------------%
% gen_store_unscoped_intp(+Stack,+Css,+ItpGroups,+PrevItp,+CheckInd,+Solver)
%----------------------------------------------------------------------------%
% The solver status is assumed to be unsat.
%
% Visit Stack extracting one interpolant for each member of
% ItpGroups. Then, store them in the memo.
%
% PrevItp is the interpolant from the previous (immediate lower level)
% cutpoint. It is used to check whether the sequence interpolant is
% inductive or not.
%
% CheckInd is a flag. If tt it performs inductive check.
%----------------------------------------------------------------------------%
gen_store_unscoped_intp([],[],_,_,_,_).
gen_store_unscoped_intp([_| RestStack], [[]| RestCs], ItpGroups, PrevItp, 
                        CheckInd, Solver) :-
	gen_store_unscoped_intp(RestStack, RestCs, ItpGroups, PrevItp, 
                                  CheckInd, Solver).
gen_store_unscoped_intp([clause(Head,_Id,_Depth,_,_,_)| RestStack],
	              [_| RestCs], ItpGroups, PrevItp, CheckInd, Solver):-
	( term_without_solver_vars(Head) ; no_cache(Head) ),
	gen_store_unscoped_intp(RestStack, RestCs, ItpGroups, PrevItp, 
                                  CheckInd, Solver).
gen_store_unscoped_intp([clause(Head,ClId,Depth,ContextId,Prefix,NewPrefix)|RestStack],
	              [Cs|RestCss],[ItpGroup|ItpGroups], PrevItp, 
                        CheckInd, Solver):-

	%% GENERATE INTP(A,B)
	start_timer(unscoped_interpolation_gen),
	solver_intp_sibling_interpolate(Solver, [ ItpGroup | ItpGroups], Itp),
	stop_timer(unscoped_interpolation_gen,_),
	%% STORE INTP(A,B)
	%% This predicate will also report (via ContinueFlag) if we
	%% need to keep going up or not.
	start_timer(unscoped_interpolation_store),     
          lazy_update_intp_memotable(unscoped,
	                           Head,ClId,Depth,ContextId,Prefix,NewPrefix,
	                           Itp, 
		                 /*these three arguments to check inductiveness*/
			       CheckInd,PrevItp,Cs, 
			       Solver,ContinueFlag,InductiveFlag),
	stop_timer(unscoped_interpolation_store,_),
	( ContinueFlag == ff ->  true
	; gen_store_unscoped_intp(RestStack,RestCss,ItpGroups,Itp,
                                    InductiveFlag,Solver)).
gen_store_unscoped_intp([_| RestStack], [_|RestCss], ItpGroups, PrevItp, 
                        CheckInd, Solver):-
	gen_store_unscoped_intp(RestStack, RestCss, ItpGroups, PrevItp, 
	                        CheckInd, Solver).
:- endif.
		
:- push_prolog_flag(multi_arity_warnings,off).
assert_subsumed_interpolant(SubsumedIntpInfo,_Solver):-
	% If SubsumedIntpInfo is a free var then the interpolant is
	% from an failed derivation.
	var(SubsumedIntpInfo),
	!.
assert_subsumed_interpolant('$subsumed_intp'(Goal,SubsumerGoal,SubsumerItp), 
                            Solver):-
          % Implemented as a "not not" to undo unification between Goal and
          % SubsumerGoal.
          assert_subsumed_interpolant(Goal,SubsumerGoal,SubsumerItp,Solver).
assert_subsumed_interpolant(Goal,SubsumerGoal,SubsumerItp,Solver):-
	% Here the interpolant to be generated is from a subsumed
	% derivation.
          Goal = SubsumerGoal,
 	extract_equalities_from_last_unification(SubsumerGoal,[],_,MatchingEqs),
	solver_mk_not(Solver,SubsumerItp,NotSubsumerItp),
 	solver_intp_sibling_assert(Solver, [NotSubsumerItp | MatchingEqs]),
	fail.
assert_subsumed_interpolant(_,_,_,_).
:- pop_prolog_flag(multi_arity_warnings).

%------------------------------------------------------------------------------%
% term_without_solver_vars(+Term)
%------------------------------------------------------------------------------%
% Succeed if Term does not have any variable tracked by the solver.
%------------------------------------------------------------------------------%
term_without_solver_vars(Term):- 
        varset_attributes(Term,Vs), 
        !, 
        Vs = [].


 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END UNSCOPED INTERPOLANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BEGIN SCOPED INTERPOLANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------%
% gen_scoped_interpolants_lazily(+Stack,+,+,+)
%-------------------------------------------------------------------------------%
% Generate "well-scoped" interpolants from Stack and store them in the
% memo table. The generation is lazy in order to stop as soon as
% possible in a similar manner than in gen_unscoped_interpolants_lazily/3.
%-------------------------------------------------------------------------------%
gen_scoped_interpolants_lazily(StackIn, TrackedItpInfoIn, Solver, SubsumedItpInfo):-
          % if the current path is a subsumed one we need to push
          % first the negation of the subsumer's interpolant.
          update_goal_stack_and_intp_adt(StackIn, TrackedItpInfoIn, 
	                               SubsumedItpInfo, Solver,
                                         Stack, TrackedItpInfo), 			        
 	intp_adt__get_scoped(TrackedItpInfo, Path, AllFlatCs_s, 
                               UserDefCs_s, SynRelBodyLitVs),
	% We need Css to check interpolants are inductive
 	intp_adt__get_unscoped(TrackedItpInfo, Css),
  
 	gen_store_scoped_intp(Stack, Path,  
			  AllFlatCs_s,UserDefCs_s, SynRelBodyLitVs, 
			  % to check inductiveness
			  Css,_PrevItp,tt, 
			  Solver),
	!.
gen_scoped_interpolants_lazily(_,_,_,_):-
          format("ERROR ftclp: gen_scoped_interpolants_lazily/4 failed.\n",[]),
          halt.

%%----------------------------------------------------------------------------%%
% gen_store_scoped_intp
%%----------------------------------------------------------------------------%%
% - UserDefCs_s contains all user-defined constraints along the
%   path.
%
% - SynRelBodyLitVs is an union-find that contains all syntactically
%   related constraints (see definition at the beginning of this file)
%   with respect to the body literal (L) variables but without using
%   the constraints from the unification between the body literal and
%   the clause head (H) during the computation of the
%   "syntactically-related" relationship.
% 
 
% Notation: Goal refers to the body literal that matched with the
% clause head.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We will form A as the constraints from SynRelBodyLitVs plus those form
% the unification between L and H.  The rest of constraints go to B.
% By doing this, the only common variables between A and B must be
% ONLY variables from H which is exactly what we want.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_store_scoped_intp([],[],_,_,_,_,_,_,_). 
gen_store_scoped_intp([clause(Head,_,_,_,_,_)|FullStack], 
	            [user_defined(_,_,_-_)|Pss], 
                      AllCs_s,UserDefCs_s,SynRelBodyLitVs,
	            /* these three to check inductiveness */
	            [_|Css], PrevItp,CheckInd, 
                      Solver):-
          /* The user says explicitly forget about this predicate */
          no_cache(Head),
	!,
	gen_store_scoped_intp(FullStack, Pss, AllCs_s,
	                      UserDefCs_s,SynRelBodyLitVs,
	                      Css, PrevItp,CheckInd,
			  Solver).
gen_store_scoped_intp([Top|Stack], [user_defined(Top, GoalHeadCs_s,_-_)|Pss],  
                      AllCs_s,UserDefCs_s,SynRelBodyLitVs,
	            [_|Css],PrevItp,CheckInd,Solver):-
          /* The predicate does not have variables (i.e., arity 0) */
          GoalHeadCs_s == [],
	gen_store_scoped_intp(Stack, Pss, AllCs_s,UserDefCs_s,
		            SynRelBodyLitVs,
		            Css,PrevItp,CheckInd,
			  Solver).
gen_store_scoped_intp([Top|Stack],
	            [user_defined(Top,GoalHeadCs_s,_HeadVs-BodyLitVs)|Pss], 
	            AllCs_s,UserDefCs_s,SynRelBodyLitVs,
                      /* these three to check inductiveness */
	            [Cs|Css],PrevItp,CheckInd,
		  Solver):-		   
          GoalHeadCs_s \= [],
	Top = clause(Head,ClId,Depth,ContextId,Prefix,NewPrefix),
 
	start_timer(scoped_interpolation_prep_1),
 
	%%---------------------------------------------------------------%%
          %% NonGoalHeadCs are all constraints except all primitive
          %% constraints and those from the unification between the
          %% current body literal and the clause head.
	%%---------------------------------------------------------------%%
	ord_difference(UserDefCs_s, GoalHeadCs_s, NonGoalHeadCs),
	%%===== THIS IS ONE OF THE TWO BOTTLENECKS ======================%%
	%% NextSynRelBodyLitVs contains the equivalence classes
	%% after computing the transitive closure of the constraints
	%% in NonGoalHeadCs.
 	incr_trans_closure(NonGoalHeadCs,SynRelBodyLitVs,NextSynRelBodyLitVs),
	%%===============================================================%%
	stop_timer(scoped_interpolation_prep_1,_),
	start_timer(scoped_interpolation_prep_2),
 
	NewBodyLitVs = BodyLitVs,
	stop_timer(scoped_interpolation_prep_2,_),
	( NewBodyLitVs = [] -> 
	  ContinueFlag = tt,
            NewCheckInd  = tt
          ; 
	  start_timer(scoped_interpolation_prep_3),
 
            %% 
            % Merge in the union-find all variables from the body
            % literal.
            %% 
	  ( merge_vars_unionfind(NewBodyLitVs,NextSynRelBodyLitVs,A0) ->
 	    
	    %% GoalHeadCs_s, A0, A, B: sorted lists of constraints identifiers.
	    constraint_intersection_diff(GoalHeadCs_s,NewBodyLitVs,
                                           NewGoalHeadCs_s,_),
	    ord_union(NewGoalHeadCs_s,A0, A),
	    ord_difference(AllCs_s, A, B),
	    rev_map_constraints(A,A_Cs),
	    rev_map_constraints(B,B_Cs),
	    stop_timer(scoped_interpolation_prep_3,_),
	    %%===== THIS IS THE OTHER BOTTLENECK ===========================%%
	    start_timer(scoped_interpolation_gen),
	    solver_interpolate_formulas(Solver, A_Cs, B_Cs, Itp),
 
	    stop_timer(scoped_interpolation_gen,_),
	    %%==============================================================%%
	    %% This predicate will also report (via ContinueFlag) if we
	    %% need to keep going up or not.
	    %%==============================================================%%
	    start_timer(scoped_interpolation_store),     
	    lazy_update_intp_memotable(scoped,
	                               Head,ClId,Depth,ContextId,
	                               Prefix,NewPrefix, Itp,
				 /* to check inductiveness */
				 CheckInd,PrevItp,Cs, 
				 Solver,ContinueFlag,InductiveFlag),
	    stop_timer(scoped_interpolation_store,_),
	    NewCheckInd  = InductiveFlag
	    %%==============================================================%		 
	  ;
	    ContinueFlag = tt,
	    NewCheckInd  = tt,
	    stop_timer(scoped_interpolation_prep_3,_)
	  )
	),
	( ContinueFlag == ff ->  true
	; gen_store_scoped_intp(Stack,Pss, AllCs_s, UserDefCs_s,
	                        SynRelBodyLitVs, Css,Itp,NewCheckInd, Solver)).
gen_store_scoped_intp([_|Stack],[primitive(_Cs)|Pss], AllCs_s,UserDefCs_s,
                      SynRelBodyLitVs, [_|Css],PrevItp,CheckInd,Solver):-
	start_timer(scoped_interpolation_prep_1),
 
	stop_timer(scoped_interpolation_prep_1,_),
	%%---------------------------------------------------------------%%
	gen_store_scoped_intp(Stack,Pss,AllCs_s,UserDefCs_s,SynRelBodyLitVs,
		            Css,PrevItp,CheckInd,Solver).


 

%-------------------------------------------------------------------------------% 
% update_goal_stack_and_intp_adt(+,+,+,+,-,-)
%-------------------------------------------------------------------------------% 
% Used only for scoped interpolants when there is a subsumed
% derivation.
%-------------------------------------------------------------------------------% 
update_goal_stack_and_intp_adt(Stack,TrackedItp,SubsumedItp,_,Stack,TrackedItp):-
          var(SubsumedItp),!.
update_goal_stack_and_intp_adt(Stack, TrackedItp, 
                               '$subsumed_intp'(Subsumed,Subsumer,SubsumerItp), 
			 Solver,
			 NewStack, NewTrackedItp):-
	varset_attributes(Subsumer, SubsumerVs),
	varset_attributes(Subsumed, SubsumedVs),
	solver_rename_formula(Solver, 
	                      SubsumerItp,SubsumerVs,SubsumedVs,SubsumedItp),
 	solver_mk_not(Solver,SubsumedItp,NotSubsumedItp),	
          intp_adt__push_subsumed(TrackedItp, 
	                        NotSubsumedItp, SubsumedVs,
                                  NewTrackedItp),
 
          NewStack = [_|Stack],
	!.

% special case for subsumed derivation
intp_adt__push_subsumed(TrackedItp, NotSubsumedItp, SubsumedVs, NewTrackedItp):-
	TrackedItp = 
             '$intp_adt'(unscoped(UnscopedPrefix), 
                         scoped(ScopePrefix,AllFlatCs,FlatUserDefCs,UF0)),
	start_timer(scoped_interpolation_prep_4),
	map_and_sort_segment(primitive([NotSubsumedItp]), _, Encoded_Cs),
	ord_insert_list(Encoded_Cs, AllFlatCs, AllFlatCs_Out),
          ScopePrefix_Out = [primitive(Encoded_Cs)| ScopePrefix],
	Encoded_Cs = [Encoded_C],
	add_vars_unionfind(SubsumedVs, Encoded_C, UF0, UF1),
	merge_vars_unionfind_(SubsumedVs, UF1, UF2),
          stop_timer(scoped_interpolation_prep_4,_),
          NewTrackedItp = 
             '$intp_adt'(unscoped([[NotSubsumedItp]|UnscopedPrefix]), 
	               scoped(ScopePrefix_Out,AllFlatCs_Out,FlatUserDefCs,UF2)).

 

 

%%------------------------------------------------------------------------%
%% transitive_closure(+Ks,-UF)
%% -----------------------------------------------------------------------%
%% Use an union-find to compute efficiently the transitive closure of
%% the "syntactically related" property of a given set of constraints. 
%%------------------------------------------------------------------------%
incr_trans_closure(Ks, UF0, UF1):-
	transitive_closure_aux(Ks, UF0, UF1).

transitive_closure_aux([], UF, UF):- !.
transitive_closure_aux([K|Ks], UF0, UF2):-
	add_constraint_unionfind(K ,UF0, UF1),
	transitive_closure_aux(Ks, UF1, UF2).

add_constraint_unionfind(K, UF0, UF2):-
	% Avoid recompute the variable set of the constraint.
	map_constraint__get_vars(K,Vs),
	( Vs = [] -> UF2=UF0 
	;
	  add_vars_unionfind(Vs, K, UF0, UF1),
	  merge_vars_unionfind_(Vs, UF1, UF2)
	).	
add_vars_unionfind([], _, UF, UF).
add_vars_unionfind([V|Vs], K, UF0, UF2):-
	% This can fail if V is not found
	unionfind_add_and_merge(V,[K],UF0,UF1),
	!,
	add_vars_unionfind(Vs, K, UF1, UF2).
add_vars_unionfind([V|Vs], K, UF0, UF2):-
	unionfind_make(V, [K], UF0,UF1),
	add_vars_unionfind(Vs, K, UF1, UF2).

merge_vars_unionfind_([] ,UF,UF):- !.
merge_vars_unionfind_([_],UF,UF):- !.
merge_vars_unionfind_([V1,V2|Vs],UF0,UF2):-
	unionfind_merge(V1,V2,UF0,UF1,_), 
	!,
	merge_vars_unionfind_([V2|Vs],UF1,UF2).
merge_vars_unionfind_([_V1,V2|Vs],UF0,UF1):-
	!, % V1 is not in the union-find
	merge_vars_unionfind_([V2|Vs],UF0,UF1).
	
%----------------------------------------------------------------------------%
% merge_vars_unionfind(-Vs,-UF,+MergedVal)
%----------------------------------------------------------------------------%
% MergedVal is the final value after merging all Vs in UF.
%----------------------------------------------------------------------------%
merge_vars_unionfind([],_,_):-
	format("ERROR ftclp: (merge_vars_unionfind/3) ",[]),
	format("1st argument cannot be empty list.\n" ,[]),
	halt.
merge_vars_unionfind([V],UF,Val):-
	unionfind_findSimple(_,V,UF,data(_,Val)),
	!.
 
merge_vars_unionfind([V1,V2|Vs],UF0,Val):-
	% V1 is in UF0
	unionfind_findSimple(_,V1,UF0,_),
	!,
	merge_vars_unionfind_aux([V2|Vs],V1,UF0,UF1),
	% This cannot fail because V1 is for sure in UF1
	unionfind_findSimple(_,V1,UF1,data(_,Val)).
merge_vars_unionfind([V1,V2|Vs],UF0,Val):-
	% V1 is not in UF0
	!,
	unionfind_make(V1, [], UF0, UF1),
	merge_vars_unionfind_aux([V2|Vs],V1,UF1,UF2),
	% This cannot fail because V1 is for sure in UF2
	unionfind_findSimple(_,V1,UF2,data(_,Val)).

merge_vars_unionfind_aux([],_Y,UF,UF):- !.
merge_vars_unionfind_aux([X|Xs],Y,UF0,UF2):-
	% X and Y are in UF0
 	unionfind_merge(X,Y,UF0,UF1,_), 
	!,
	merge_vars_unionfind_aux(Xs,Y,UF1,UF2).
merge_vars_unionfind_aux([X|Xs],Y,UF0,UF3):-
	!,
	% X is not in UF0 (not that Y is always in UF0)
	unionfind_make(X, [], UF0, UF1),
 	unionfind_merge(X,Y,UF1,UF2,_), 
	merge_vars_unionfind_aux(Xs,Y,UF2,UF3).

 

 


%===============================================================================%
%   Pseudo-argname for encapsulate information needed by interpolation
%===============================================================================%
% - unscoped(Css) where Css is a list of lists with all the constraints
%   from the path. Each list is an interpolation group.
%
% - scoped(Path,AllCs,UserDefCs,UF) where Path is an encoding of the
%   full path, AllCs is a list of constraints from the whole path,
%   UserDefCs is a list of constraints containing only user-defined
%   constraints, and UF is an union find.
%===============================================================================%
intp_adt__init('$intp_adt'(unscoped([]),scoped([],[],[],UF0))):-
	unionfind_make(UF0).
intp_adt__push(TrackedItp, _, TrackedItp):-
	( \+(is_enabled_option(scoped_interpolation)), 
	  \+(is_enabled_option(unscoped_interpolation))),
	!.
intp_adt__push(TrackedItp, Segment, NewTrackedItp):-
	TrackedItp = 
             '$intp_adt'(unscoped(UnscopedPrefix), 
                         scoped(ScopePrefix,AllFlatCs,FlatUserDefCs,UF0)),
 
	(  ( Segment = user_defined(_,Cs,_-_) ; Segment = primitive(Cs) ) -> 
	   NewUnscopedPrefix = [Cs|UnscopedPrefix]
	; 
	   NewUnscopedPrefix = UnscopedPrefix
	),
	( is_enabled_option(scoped_interpolation) ->
	  start_timer(scoped_interpolation_prep_4),
	  map_and_sort_segment(Segment, MappedSegment_s, New_Cs_s),
	  %%%
	  % - FlatUserDefCs, NewFlatUserDefCs constain all user-defined
	  %   constrains along the path.
	  % - AllFlatCs contains all flattened constraints along the path.
	  % - UF0, UF1 contain all "syntactically related" primitive
	  %  constraints
	  %%%
	  ord_insert_list(New_Cs_s, AllFlatCs, NewAllFlatCs),
	  ( Segment = user_defined(_,_, _-_) ->
	    % To build incrementally the path with all user-defined
	    % constraints
	    ord_insert_list(New_Cs_s, FlatUserDefCs, NewFlatUserDefCs),
	    % To build incrementally the transition closure of all
	    % "syntactically related" PRIMITIVE constraints.
	    UF1 = UF0
	  ; % else Segment = primitive(_)
	    % To build incrementally the path with all user-defined
	    % constraints
  	    NewFlatUserDefCs   = FlatUserDefCs,
	    % To build incrementally the transition closure of all
	    % "syntactically related" PRIMITIVE constraints.
	    incr_trans_closure(New_Cs_s, UF0, UF1)
	  ),
	  NewScopePrefix = [MappedSegment_s| ScopePrefix],
	  stop_timer(scoped_interpolation_prep_4,_)
	;
            % do nothing: new values are old values
	  NewScopePrefix   = ScopePrefix,
	  NewFlatUserDefCs = FlatUserDefCs,	  
	  UF1              = UF0,
	  NewAllFlatCs     = AllFlatCs
	),
          NewTrackedItp = 
               '$intp_adt'(unscoped(NewUnscopedPrefix), 
	                 scoped(NewScopePrefix,NewAllFlatCs,NewFlatUserDefCs,UF1)).
             
%-------------------------------------------------------------------------------%
% intp_adt__get_unscoped(+,-)
%-------------------------------------------------------------------------------%
% Provide the information needed to produce out-of-the-scope
% interpolants.
% Css is a list of lists that represents the constraints along a path.
%-------------------------------------------------------------------------------%
intp_adt__get_unscoped(TrackedItp,Css):- TrackedItp='$intp_adt'(unscoped(Css),_).
	
%-------------------------------------------------------------------------------%
% intp_adt__get_scoped(+,-,-,-)
%-------------------------------------------------------------------------------%
% Provide the information needed to produce scoped interpolants.
% Path_s is an encoding of the path in the correct order.
% AllFlatCs is a list of constraints along the path 
% FlatUserDefCs is a list of constraints containing only
% user-defined constraints
% UF is an union find with all primitive constraints along the path.
%-------------------------------------------------------------------------------%
intp_adt__get_scoped(TrackedItp, Path, AllFlatCs, FlatUserDefCs, UF):-
	TrackedItp = '$intp_adt'(_,scoped(Path,AllFlatCs,FlatUserDefCs,UF)).
             
% Sort everything inside user_defined/3 and primitive/1 terms.
map_and_sort_segment(user_defined(H,Cs,A-B), 
	           user_defined(H,Ks_s,A_s-B_s), Ks_s):-
	map_constraints(Cs, Ks),
	sort(Ks,Ks_s),
 
	A_s=A,
	sort(B,B_s).
map_and_sort_segment(primitive(Cs), primitive(Ks_s), Ks_s):-
	map_constraints(Cs, Ks),
	sort(Ks,Ks_s).
%===============================================================================%

%%-----------------------------------------------------------------------------%%
%                      Hashing constraints
%%-----------------------------------------------------------------------------%%
% '$map_constraint'(Constraint, Id , Vs , IntpGroup, EqualityFlag)
% '$map_constraint'(term , num, list(atom), num , atom)
:-data '$map_constraint'/5.
%%-----------------------------------------------------------------------------%%
map_constraint__new(C, Id):-
	current_fact('$map_constraint'(C, Id, _, _, _)),
        !.
map_constraint__new(C, Id):-
	incr_counter('$counter_map_constraint', Id, 1),
	solver_term_varset(C, [], Vs),
	( (C = '.=.'(X,Y), is_solver_var(X), is_solver_var(Y)) ->
	   EqFlag = tt
	;
	   EqFlag = ff
	), 
	asserta_fact('$map_constraint'(C, Id, Vs, _, EqFlag)),
        !.
map_constraint__add_intp_group(C, K):-
	retract_fact('$map_constraint'(C, Id, Vs, _, EqFlag)),
	asserta_fact('$map_constraint'(C, Id, Vs, K, EqFlag)),
	!.
map_constraint__add_intp_group(C,_):-
	format("ERROR ftclp: constraint ~q is not in the map!\n",[C]),
	halt.

map_constraint__get_vars(K, Vs):-
	%% Assume indexing by 2nd argument is cheap. Otherwise, we
	%% need another map here.
	current_fact('$map_constraint'(_, K, Vs, _, _)),	
	!.
map_constraint__get_vars(_, _):-
	format("ERROR ftclp: id constraint is not in the map!\n",[]),
	halt.
map_constraint__get_intp_group(K, IntpG):-
	%% Assume indexing by 2nd argument is cheap. Otherwise, we
	%% need another map here.
	current_fact('$map_constraint'(_, K, _, IntpG, _)),	
	!.
map_constraint__get_intp_group(_, _):-
	format("ERROR ftclp: id constraint is not in the map!\n",[]),
	halt.

map_constraint__is_equality(K):-
	current_fact('$map_constraint'(_, K, _, _, EqFlag)),
	!,
	EqFlag == tt.

% Map a list of constraints into a list of ids
map_constraints([],[]).
map_constraints([C|Cs], [K|Ks]):-
	map_constraint__new(C, K),
	map_constraints(Cs, Ks).

rev_map_constraints([],[]).
rev_map_constraints([K|Ks], [C|Cs]):-
	map_constraint__new(C, K),
	rev_map_constraints(Ks, Cs).

:- data '$cache_map_constraints_to_intp_groups'/2.
map_constraints_to_intp_groups(Cs,Is):-
	current_fact('$cache_map_constraints_to_intp_groups'(Cs,Is)),
	!.
map_constraints_to_intp_groups(Cs,Is):-
	map_constraints_to_intp_groups__(Cs,Is),
	asserta_fact('$cache_map_constraints_to_intp_groups'(Cs,Is)),
	!.

map_constraints_to_intp_groups__(Cs,Is):-
	map_constraints_to_intp_groups_aux(Cs,Is_u),
	sort(Is_u,Is).
map_constraints_to_intp_groups_aux([],[]).
map_constraints_to_intp_groups_aux([K|Ks],[I|Is]):-
	map_constraint__get_intp_group(K,I),
	map_constraints_to_intp_groups_aux(Ks,Is).


:- data '$cache_list_solver_term_varset'/2.
list_solver_term_varset(Cs,Vs):-
	current_fact('$cache_list_solver_term_varset'(Cs,Vs)),
	!.
list_solver_term_varset(Cs,Vs):-
	list_solver_term_varset_aux(Cs,[],Vs),
	asserta_fact('$cache_list_solver_term_varset'(Cs,Vs)),
	!.

list_solver_term_varset(Cs,Vs):-
	list_solver_term_varset_aux(Cs,[],Vs).
list_solver_term_varset_aux([], Vs, Vs):-
	!.
list_solver_term_varset_aux([K|Ks], Vs0, Vs2):-
 	map_constraint__get_vars(K, Vs),
	ord_insert_list(Vs0,Vs,Vs1),
 	list_solver_term_varset_aux(Ks,Vs1,Vs2).

%------------------------------------------------------------------------------%
% constraint_intersection_diff(+Cs,+Vs,-IntCs,-DiffCs)
%------------------------------------------------------------------------------%
% Intersection and difference between a constraint a list of
% variables. A constraint C intersects with a list of variables Vs if
% the variables of C intersect with Vs. Similarly with difference.
%------------------------------------------------------------------------------%
constraint_intersection_diff([],_,[],[]).
constraint_intersection_diff([K|Ks],Ys,[K|Zs],Ws):-
 	map_constraint__get_vars(K, Xs),
	ord_intersect(Xs,Ys),
	!,
	constraint_intersection_diff(Ks,Ys,Zs,Ws).
constraint_intersection_diff([K|Ks],Ys,Zs,[K|Ws]):-
	!,
	constraint_intersection_diff(Ks,Ys,Zs,Ws).
	
 

%%-----------------------------------------------------------------------------%%
%% Auxiliary predicates
%%-----------------------------------------------------------------------------%%

% ord_insert_list(+list,+list,-list)
% ord_insert_list(+L1,+L2,-L3)
% L1 is an unsorted list, L2 is a sorted list.  L3 is the result of
% inserting all elements of L1 in L2 in a sorted manner.
ord_insert_list([],Ls,Ls).
ord_insert_list([X|Xs],Ls0,Ls2):-
	insert(Ls0,X,Ls1),
	ord_insert_list(Xs,Ls1,Ls2).

ord_difference(S1,S2,S3):-
	ord_intersection_diff(S1,S2,_,S3).
ord_intersection(S1,S2,S3):-
	ord_intersection_diff(S1,S2,S3,_).
ord_intersect(S1,S2):-
	ord_intersection_diff(S1,S2,S3,_),
	S3 \== [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Print utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For debugging
% display_intp_groups:-
% 	current_fact('$map_intp_group'(C,K)),
% 	format("~q --> ~q \n",[C,K]),
% 	fail.
% display_intp_groups.

% For debugging 
debug_print_interpolants(Itps,Solver):-
	is_enabled_option(debug), !,
	print_interpolants(Itps,Solver).
debug_print_interpolants(_,_):-  !.

print_interpolants([],_).
print_interpolants([I|Is],Solver):-
 	solver_formula_to_str(Solver,I, Str),
	format("~q\n",[Str]),
	print_interpolants(Is,Solver).

% For debugging 
print_unionfind(UF0,Solver):-
	unionfind_to_list(UF0,L),
	print_unionfind_aux(L,Solver).

print_unionfind_aux([],_Solver).
print_unionfind_aux([rec(V,data(Rep,Cs))|Ls], Solver):-
	V == Rep,
	!,
	decode_constraints(Cs,Solver,FormattedCs),
	decode_var(Rep,Solver,FormattedRep),
	format("~q --> ~q\n", [FormattedRep, FormattedCs]),
	print_unionfind_aux(Ls,Solver).
print_unionfind_aux([rec(V,data(Rep,_))|Ls],Solver):-
	!,
	decode_var(V,Solver,FormattedV),
	decode_var(Rep,Solver,FormattedRep),
	format("~q --> ~q\n", [FormattedV,FormattedRep]),
	print_unionfind_aux(Ls,Solver).

% For debugging
decode_constraints([],_,[]).
decode_constraints([K|Ks],Solver,[C1|Cs]):-
	current_fact('$map_constraint'(C, K, _, _,_)),
	replace_solver_vars_with_str([C],Solver,[C1]),
	decode_constraints(Ks,Solver,Cs).
decode_constraints([K|_],_,_):-
	format("ERROR ftclp: decode_constraints/3 failed with ~q\n",[K]),
	halt.

% For debugging
decode_vars([],_S,[]).
decode_vars([V|Vs],Solver,[NewV|NewVs]):-
	decode_var(V,Solver,NewV),
	decode_vars(Vs,Solver,NewVs).

decode_var(V0,Solver,V1):-
	replace_solver_vars_with_str([V0],Solver,[V1]).

 

