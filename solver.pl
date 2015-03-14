% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%=======================================================================%
% Wrapper to communicate with underlying solver.
%=======================================================================%

:- module(solver, 
	[ 
	  clear_solver/0,
	  print_solver_stats/0,
	  solver_init/2,
	  solver_stop/1,
	  solver_push_with_cut_point/2,
	  solver_cut_point/2,
	  solver_push/1,
	  solver_pop/1,
	  solver_declare_variable/4,
	  solver_assert/2,
	  solver_assert_with_cutpoint/3,
	  solver_incremental/3,
	  solver_check_sat/2,
	  convert_clp_constraints_to_solver/3,
	  solver_interpolate/3,
	  solver_interpolate_formulas/4,
	  % This one if the solver entails a formula Q
	  solver_entailment/3,
	  solver_entailment_formulas/3,
	  solver_rename_formula/5,
	  % To build/test some formulae
	  solver_conjoin_formulas/4,
	  solver_disjoin_formulas/4,
	  solver_mk_true/2,
	  solver_is_true/2,
	  solver_is_false/2,
	  solver_imply_false/2,
	  solver_mk_not/3,
	  solver_all_vars_included/3,
	  % for debugging
	  solver_formula_to_str/3,
	  solver_print/1,
	  replace_solver_vars_with_str/3,
	  % Operations in "sibling" solvers
	  solver_sibling_push/1,
	  solver_sibling_pop/1,
	  solver_sibling_reset/1,
	  solver_sibling_assert_formula/2,
	  solver_sibling_incremental/3,
	  solver_sibling_print/1,
	  % 
	  solver_intp_sibling_push/1,
	  solver_intp_sibling_push_intp_group/2,
	  solver_intp_sibling_pop/1,
	  solver_intp_sibling_assert/2,
	  solver_intp_sibling_check_sat/2,
	  solver_intp_sibling_interpolate/3,
	  solver_intp_sibling_print/1,
	  solver_intp_sibling_reset/1,
	  solver_intp_sibling_set_intp_group/2,
	  %
	  solver_term_varset/3,
	  is_solver_var/1
	]).

%  Own libraries
:- use_module('solver/ciao_prolog_smt').
:- use_module('op_attributes', [create_var_atom/2, print_term_with_attribute/1]).
:- use_module(options).
:- use_module(debug).
:- use_module(counters).
%  Ciao libraries
:- use_module(timer).
:- use_module(library(terms) , [atom_concat/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(sets)  , [ord_union/3 ]).

clear_solver:-
        unregister_environments,
        % For caching
        retractall_fact('$var_map'(_,_)), % to avoid redeclaration of variables
        retractall_fact('$cache_solver_term_varset'(_,_)),
        % Counters for statistics
        % satisfiability checks (true or false)
        delete_counter('$solver_num_of_sat_checks'),   
        % sat checks that are false
        delete_counter('$solver_num_of_unsat_queries'),
        delete_counter('$solver_num_of_sibling_sat_queries'),
        delete_counter('$solver_num_of_entailments'),
        delete_counter('$solver_num_of_seq_interpolants'),
        delete_counter('$solver_num_of_interpolants'),
        delete_counter('$solver_num_of_assrt_formulas').

init_stat_counters:-
        set_counter('$solver_num_of_sat_checks',0),
        set_counter('$solver_num_of_unsat_queries',0),
        set_counter('$solver_num_of_entailments',0),
        set_counter('$solver_num_of_seq_interpolants',0),
        set_counter('$solver_num_of_interpolants',0),
        set_counter('$solver_num_of_sibling_sat_queries',0),
        set_counter('$solver_num_of_assrt_formulas',0).

print_solver_stats:-
        get_counter_value('$solver_num_of_sat_checks', N1),
        get_counter_value('$solver_num_of_unsat_queries', N8),
        get_counter_value('$solver_num_of_entailments', N2),
        get_counter_value('$solver_num_of_interpolants', N3),
        get_counter_value('$solver_num_of_sibling_sat_queries', N5),
        N11 is N1 - N8,        
        format("SOLVER STATS\n",[]),
        format("\tNumber of satisfiability checks............. ~q\n",[N1]),
        format("\t\tNumber of sat queries............... ~q\n",[N11]),
        format("\t\tNumber of unsat queries............. ~q\n",[N8]),
        format("\tNumber of entailment tests.................. ~q\n",[N2]),
        format("\tNumber of interpolant queries............... ~q\n",[N3]),
        format("\tNumber of sat checks to reuse answers....... ~q\n",[N5]).

% The underlying solver must support simultaneously several
% environments: a parent and multiple siblings. An environment is like
% a stack where all the constraints are pushed/popped. The reason why
% we want to have sibling environments rather than independent solvers
% is because underlying solvers often share terms across siblings.
%
% Here all the environments:
:- data '$parent_env'/1.
:- data '$sibling_env'/1.
:- data '$intp_sibling_env'/1.
register_environments(NumOfEnv):-
        asserta_fact('$parent_env'(0)),
        asserta_fact('$sibling_env'(1)),
        asserta_fact('$intp_sibling_env'(2)),
        NumOfEnv = 3.
unregister_environments:-
        retractall_fact('$parent_env'(_)),
        retractall_fact('$sibling_env'(_)),
        retractall_fact('$intp_sibling_env'(_)).

% solver_init(+atm(Name),-Solver)
solver_init(SolverName, smt(SolverRef)) :- 
        register_environments(NumOfEnv),
        smt_init(SolverName, NumOfEnv, SolverRef),        
        init_stat_counters.

solver_push(smt(SolverRef)) :- 
        start_timer(push),
        current_fact('$parent_env'(Env)),
        smt_push(SolverRef,Env),
        stop_timer(push,_),
        debug_message("Push a choice point.\n",[]).

solver_push_with_cut_point(smt(SolverRef), IntpGroup) :- 
        start_timer(push),
        current_fact('$parent_env'(Env)),
        smt_push(SolverRef, Env),
        stop_timer(push,_),
        smt_push_intp_group(SolverRef, Env, IntpGroup),
        debug_message("Push a choice point and added cutpoint ~q\n",[IntpGroup]).

solver_sibling_push(smt(SolverRef)) :- 
        current_fact('$sibling_env'(Env)),
        % FIXME: this will slow down if many constraints are asserted between
        % pushes
        smt_push_intp_group(SolverRef, Env, _), % otherwise crash!
        smt_push(SolverRef, Env),
        debug_message("Push a choice point.\n",[]).

solver_intp_sibling_push(smt(SolverRef)) :- 
        current_fact('$intp_sibling_env'(Env)),
        smt_push(SolverRef, Env),
        debug_message("Push a choice point.\n",[]).

solver_intp_sibling_push_intp_group(smt(SolverRef),IntpGroup) :- 
        current_fact('$intp_sibling_env'(Env)),
        smt_push_intp_group(SolverRef,Env,IntpGroup),
        debug_message("Push an intp group ~q.\n",[IntpGroup]).

solver_cut_point(smt(SolverRef), IntpGroup):-
        current_fact('$parent_env'(Env)),
        smt_push_intp_group(SolverRef,Env,IntpGroup),
        debug_message("Added a cutpoint ~q\n",[IntpGroup]).

solver_intp_sibling_set_intp_group(smt(SolverRef), IntpGroup) :- 
        current_fact('$intp_sibling_env'(Env)),
        smt_set_intp_group(SolverRef,Env,IntpGroup),
        debug_message("Set interpolation group in sibling ~q\n",[IntpGroup]).

solver_pop(smt(SolverRef)) :- 
        start_timer(pop),
        current_fact('$parent_env'(Env)),
        smt_pop(SolverRef,Env),
        stop_timer(pop,_),
        debug_message("Popped a choice point.\n",[]).

solver_sibling_pop(smt(SolverRef)) :- 
        current_fact('$sibling_env'(Env)),
        smt_pop(SolverRef,Env),
        debug_message("Popped a choice point.\n",[]).

solver_intp_sibling_pop(smt(SolverRef)) :- 
        current_fact('$intp_sibling_env'(Env)),
        smt_pop(SolverRef,Env),
        debug_message("Popped a choice point.\n",[]).

solver_sibling_reset(smt(SolverRef)):-
        current_fact('$sibling_env'(Env)),
        smt_reset(SolverRef,Env).

solver_intp_sibling_reset(smt(SolverRef)):-
        current_fact('$intp_sibling_env'(Env)),
        smt_reset(SolverRef,Env).

solver_stop(smt(SolverRef)) :- 
        smt_stop(SolverRef).

:- data '$var_map'/2.
solver_declare_variable(smt(_SolverRef), Id, VarRef, _Ty):-
        current_fact('$var_map'(Id,VarRef)),
        !.
solver_declare_variable(smt(SolverRef), Id, VarRef, Ty):-
        Ty == int,
        !,
        smt_declare_int_var(SolverRef, Id, VarRef),
        asserta_fact('$var_map'(Id,VarRef)).
solver_declare_variable(smt(SolverRef), Id, VarRef, Ty):-
        Ty == real,
        !,
        smt_declare_real_var(SolverRef, Id, VarRef),
        asserta_fact('$var_map'(Id,VarRef)).
solver_declare_variable(_, _,_, Ty):-
        format("solver_declare_variable/4 unsupported type ~q\n",[Ty]), 
        halt.

% Without no conversion. The conversion must have been done as
% precondition.
solver_assert(smt(_), []):- !.
solver_assert(smt(SolverRef), Cs):-
        current_fact('$parent_env'(Env)),
        smt_assert(SolverRef,Env,Cs).

solver_assert_with_cutpoint(smt(SolverRef), Cs, IntpGroup):-
        current_fact('$parent_env'(Env)),
        smt_push_intp_group(SolverRef, Env, IntpGroup),
        smt_assert(SolverRef, Env, Cs).

solver_intp_sibling_assert(smt(_),[]):- !.
solver_intp_sibling_assert(smt(SolverRef), Cs):-
        current_fact('$intp_sibling_env'(Env)),
        smt_assert(SolverRef,Env,Cs).

% Without no conversion. The conversion must have been done as
% precondition.
solver_incremental(smt(SolverRef), Cs, SatFlag):-
        start_timer(assert_constraints),
        current_fact('$parent_env'(Env)),
        smt_assert(SolverRef,Env,Cs),
        stop_timer(assert_constraints,_),
        incr_counter('$solver_num_of_sat_checks', _),
        start_timer(check_sat),
        ( smt_check_sat(SolverRef,Env) ->
	SatFlag = tt
        ;
	SatFlag = ff,
	incr_counter('$solver_num_of_unsat_queries',_)
        ),
        stop_timer(check_sat,_).

solver_check_sat(smt(SolverRef), SatFlag):-
        incr_counter('$solver_num_of_sat_checks', _),
        current_fact('$parent_env'(Env)),
        ( smt_check_sat(SolverRef,Env) ->
	SatFlag = tt
        ;
	SatFlag = ff,
	incr_counter('$solver_num_of_unsat_queries',_)
        ).

solver_intp_sibling_check_sat(smt(SolverRef), SatFlag):-
        current_fact('$intp_sibling_env'(Env)),
        ( smt_check_sat(SolverRef,Env) ->
	SatFlag = tt
        ;
	SatFlag = ff
        ).
		
solver_sibling_assert_formula(smt(SolverRef), F):-
        current_fact('$sibling_env'(Env)),
        smt_assert_formula(SolverRef, Env, F).

solver_sibling_incremental(smt(SolverRef), Cs, SatFlag):-
        current_fact('$sibling_env'(Env)),
        smt_assert(SolverRef, Env, Cs),
        incr_counter('$solver_num_of_sibling_sat_queries',_),
        ( smt_check_sat(SolverRef, Env) ->
	SatFlag = tt
        ;
	SatFlag = ff
        ).

solver_interpolate(smt(SolverRef), ItpGroups, Intp):-
        incr_counter('$solver_num_of_interpolants',_),
        current_fact('$parent_env'(Env)),
        smt_interpolant(SolverRef, Env, ItpGroups, Intp).

solver_intp_sibling_interpolate(smt(SolverRef), ItpGroups, Intp):-
        incr_counter('$solver_num_of_interpolants',_),
        current_fact('$intp_sibling_env'(Env)),
        smt_interpolant(SolverRef, Env, ItpGroups, Intp).

solver_interpolate_formulas(smt(SolverRef), As, Bs, Intp):-
        incr_counter('$solver_num_of_interpolants',_),
        current_fact('$intp_sibling_env'(Env)),
        smt_interpolant_formulas(SolverRef, Env, As, Bs, Intp).
	
% Check if the solver store entails QRef (it's not a list). Cs are
% optional constraints that will be asserted into the solver before
% the entailment test.
solver_entailment(smt(SolverRef), Cs, QRef):-
        incr_counter('$solver_num_of_entailments', _),
        current_fact('$parent_env'(Env)),
        smt_check_entailment(SolverRef, Env, Cs, QRef),
        !.

% Check if P entails Q.
% Ensure: smt_check_entailment_formulas/3 pops the solver.
solver_entailment_formulas(smt(SolverRef), P, Q):-
        incr_counter('$solver_num_of_entailments', _),
        current_fact('$sibling_env'(Env)),
        smt_push_intp_group(SolverRef, Env, _), % otherwise crash!
        smt_check_entailment_formulas(SolverRef, Env, P, Q),
        !.

% Rename all variables in OldVs with NewVs in F producing a new
% renamed formula NF.
solver_rename_formula(smt(Solver), F, OldVs, NewVs, NF):-
        smt_rename_formula(Solver, F, OldVs, NewVs, NF).

solver_conjoin_formulas(smt(Solver),F1,F2,F3):-
        smt_conjoin_formulas(Solver,F1,F2,F3).

solver_disjoin_formulas(smt(Solver),F1,F2,F3):-
        smt_disjoin_formulas(Solver,F1,F2,F3).

solver_mk_true(smt(Solver),True):-
        smt_mk_true(Solver,True).

% Succeed if F is the "true" term
solver_is_true(smt(Solver),F):-
        smt_is_true(Solver,F).

solver_mk_not(smt(Solver), F, NotF):-
        smt_mk_not(Solver, F, NotF).

% Succeed if F is the "false" term
solver_is_false(smt(Solver),F):-
        smt_mk_not(Solver, F, NotF),
        smt_is_true(Solver,NotF).

% Succeed if F implies false
solver_imply_false(Solver,F):-
        solver_sibling_push(Solver),
        solver_sibling_incremental(Solver,[F], SatFlag),
        solver_sibling_pop(Solver), 
        !,
        SatFlag == ff.

% Succeed if all variables of F are in Vs
solver_all_vars_included(smt(Solver), F, Vs):-
        smt_all_vars_included(Solver, F, Vs).

solver_print_formula(smt(Solver), F):-
        smt_formula_to_string(Solver,F,F_Str),
        format("~q",[F_Str]).

% solver_formula_to_str(dummy, V, V).
solver_formula_to_str(smt(Solver), V, V_Str):-
        smt_formula_to_string(Solver,V , V_Str).


solver_print(smt(Solver)):-
        current_fact('$parent_env'(Env)),
        smt_print(Solver,Env).

solver_sibling_print(smt(Solver)):-
        current_fact('$sibling_env'(Env)),
        smt_print(Solver,Env).

solver_intp_sibling_print(smt(Solver)):-
        current_fact('$intp_sibling_env'(Env)),
        smt_print(Solver,Env).

%------------------------------------------------------------------------%
% Convert a list of clp constraints to a list of constraints in the
% format of the solver
%------------------------------------------------------------------------%
% TODO: we do not do any caching of previous translations!
%------------------------------------------------------------------------%
convert_clp_constraints_to_solver(Xs,Ys, Ws):-
        convert_clp_constraints_to_solver_aux(Xs, Ys, Ws).
convert_clp_constraints_to_solver_aux([], [], []).
convert_clp_constraints_to_solver_aux([X|Xs], [Y|Ys], [Z|Zs]):-
        convert_clp_constraint_to_solver(X, Y, Z),
        convert_clp_constraints_to_solver_aux(Xs, Ys, Zs).
convert_clp_constraint_to_solver(V, VRef, K):-
        var(V), 
        !,
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Key step: translate a logical variable into an atom which
        % must have been declared previously in the solver
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % See comments in op_attributes.pl to see the meaning of the
        % arguments of '$solver_map'.
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
        ( get_attribute(V,'$solver_map'(_,_K,N,VRef,_Eqs)) -> true
        ;
	  format("ERROR ftclp: unrecognized variable by the solver.\n",[]),
	  format("Possible reasons: \n",[]),
	  format("\t-check that predicates are properly annotated.   \n",[]),
	  format("\t-check that no free variables in clp_meta/1. \n",[]),
	  halt
        ),
        %(is_enabled_option(debug) -> create_var_atom(N,K) ; true).
        create_var_atom(N,K).
convert_clp_constraint_to_solver(N, N, A):-
        num(N),	
        atom_number(A,N),
        !.
convert_clp_constraint_to_solver(A, A, A):-
        atm(A),	
        !.
convert_clp_constraint_to_solver(F1,F2,F3):-
        functor(F1,N,A),
        functor(F2,N,A),
        functor(F3,N,A),
        convert_clp_constraint_to_solver_args(1,A,F1,F2,F3).
convert_clp_constraint_to_solver_args(I,N,_,_,_)   :- I > N, !.
convert_clp_constraint_to_solver_args(I,N,F1,F2,F3):-
        I =< N,
        arg(I,F1,T1),
        arg(I,F2,T2),
        arg(I,F3,T3),
        convert_clp_constraint_to_solver(T1,T2,T3),
        NI is I + 1,
        convert_clp_constraint_to_solver_args(NI,N,F1,F2,F3).

:- data '$cache_solver_term_varset'/2.
% solver_term_varset(+Term,+Vs1,-Vs2)
% pre : Vs1 is sorted
% post: Vs2 is sorted
solver_term_varset(Term,Vs1,Vs2):-
        current_fact('$cache_solver_term_varset'(Term, Vs0)),
        !,
        % Vs0,Vs1 are always sorted
        ord_union(Vs0,Vs1,Vs2).
solver_term_varset(Term,Vs1,Vs2):-
        solver_term_varset_aux(Term,[],Vs),
        sort(Vs,Vs_s),
        asserta_fact('$cache_solver_term_varset'(Term, Vs_s)),
        ord_union(Vs_s,Vs1,Vs2).	

% Return variables from a term
solver_term_varset_aux(V, Vs, Vs):-
        var(V),
        !.
solver_term_varset_aux(V, Vs, [V|Vs]):-
        is_solver_var(V),
        !.
solver_term_varset_aux(A, Vs, Vs):-  
        atomic(A), !. 
solver_term_varset_aux(Term, Vs0, Vs1):-
        functor(Term,_F,N),
        solver_term_varset_aux_args(1,N,Term, Vs0, Vs1).
solver_term_varset_aux_args(I,N,_,Vs,Vs):-  I > N,  !.
solver_term_varset_aux_args(I,N,Term,Vs,Vs2):-	
        arg(I,Term,X),
        solver_term_varset_aux(X,Vs,Vs1),
        J is I+1,
        solver_term_varset_aux_args(J,N,Term,Vs1, Vs2).
is_solver_var('$address'(_)). % Succeed if term is a variable in solver format.

% For debugging
%---------------------------------------------------------------------%
% replace_solver_vars_with_str(+list,+Solver,-list)
% replace_solver_vars_with_str(+Cs,+Solver,-NewCs)
%---------------------------------------------------------------------%
% Take a list of terms Cs with '$address(_)' atoms and return the same
% list NewCs but replace those atoms with strings that show the
% context of them. '$address(_)' is supposed to hold the address of
% some solver expression.
% ---------------------------------------------------------------------%

replace_solver_vars_with_str([], _, []).
replace_solver_vars_with_str([X|Xs], Solver,  [Y|Ys]):-
        replace_solver_vars_with_str_aux(X, Solver, Y),
        replace_solver_vars_with_str(Xs, Solver, Ys).
replace_solver_vars_with_str_aux(V, _Solver, V):-
        var(V),
        !.
replace_solver_vars_with_str_aux(V, Solver, V_Str):-
        V = '$address'(_),
        !,
        solver_formula_to_str(Solver, V, V_Str).
replace_solver_vars_with_str_aux(A, _Solver, A):-
        atomic(A),
        !.
replace_solver_vars_with_str_aux(F1, Solver, F2):-
        functor(F1,N,A),
        functor(F2,N,A),
        replace_solver_vars_with_str_aux_args(1,A,Solver,F1,F2).
replace_solver_vars_with_str_aux_args(I,N,_,_,_)   :- I > N, !.
replace_solver_vars_with_str_aux_args(I,N,Solver,F1,F2):-
        I =< N,
        arg(I,F1,T1),
        arg(I,F2,T2),
        replace_solver_vars_with_str_aux(T1,Solver,T2),
        NI is I + 1,
        replace_solver_vars_with_str_aux_args(NI,N,Solver,F1,F2).

