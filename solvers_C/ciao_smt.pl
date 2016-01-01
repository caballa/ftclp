%%============================================================================
%% Ciao interface to external SMT solvers
%% Author: Jorge. A Navas, The University of Melbourne 2012-2013
%%============================================================================
%
% FIXME: this interface exposes references (denoted by the type ref)
% to internal C++ pointers (e.g., solver::Solver and solver::Exp) to
% the Prolog side. This is to avoid unnecessary marshal/unmarshal
% operations.
%
%%============================================================================

:- module(ciao_smt,
	[ 
	  smt_init/3,
	  smt_stop/1,
	  smt_push/2,
	  smt_push_intp_group/3,
	  smt_set_intp_group/3,
	  smt_pop/2,
	  smt_reset/2,
	  smt_declare_int_var/3,
	  smt_declare_real_var/3,
	  smt_assert/3,
	  smt_assert_formula/3,
	  smt_check_sat/2,
	  smt_check_entailment/4,
	  smt_check_entailment_formulas/4,
	  smt_interpolant/4,
	  smt_interpolant_formulas/5,
	  smt_conjoin_formulas/4,
	  smt_disjoin_formulas/4,
	  smt_rename_formula/5,
	  smt_mk_true/2,
	  smt_is_true/2,
	  smt_all_vars_included/3,
	  smt_mk_not/3,
	  smt_formula_to_string/3,
	  smt_print/2
	],
	[assertions, foreign_interface]).

:- include(linker_opts). % include automatically generated file
:- use_foreign_library(['ciao_smt', 'mathsat', 'gmpxx', 'gmp', 'stdc++']).
%:- use_foreign_library(['z3', 'iz3', 'foci']).


%------------------------------------------------------------------------------%
% smt_init(+atom, +int, -ref)
%------------------------------------------------------------------------------%
:- true pred smt_init_C(in(_), in(_), out(_), go(Success)) :: 
        atom * int * any_term * int + (returns(Success), foreign(smt_create)).
smt_init(SolverName, NumOfEnv, SolverRef) :- 
	smt_init_C(SolverName, NumOfEnv, SolverRef, 1).

%------------------------------------------------------------------------------%
% smt_stop(+ref)
%------------------------------------------------------------------------------%
:- true pred smt_stop_C(in(_), go(Success)) :: 
        any_term * int + (returns(Success), foreign(smt_stop)).
smt_stop(SolverRef) :- 
	smt_stop_C(SolverRef, 1).

%------------------------------------------------------------------------------%
% smt_push(+ref,+int)
%------------------------------------------------------------------------------%
:- true pred smt_push_C(in(_),in(_),go(Success)) :: 
        any_term * int * int + (returns(Success), foreign(smt_push)).
smt_push(SolverRef, Env) :- 
	smt_push_C(SolverRef, Env, 1).

%------------------------------------------------------------------------------%
% smt_pop(+ref,+int)
%------------------------------------------------------------------------------%
:- true pred smt_pop_C(in(_),in(_),go(Success)) ::
	 any_term * int * int + (returns(Success), foreign(smt_pop)).
smt_pop(SolverRef, Env) :- 
	smt_pop_C(SolverRef, Env, 1).

%------------------------------------------------------------------------------%
% smt_reset(+ref,+int)
%------------------------------------------------------------------------------%
:- true pred smt_reset_C(in(_),in(_),go(Success)) :: 
        any_term * int * int + (returns(Success), foreign(smt_reset)).
smt_reset(SolverRef, Env) :- 
	smt_reset_C(SolverRef, Env, 1).

%------------------------------------------------------------------------------%
% smt_push_intp_group(+ref,+int,-int)
%------------------------------------------------------------------------------%
:- true pred smt_push_intp_group_C(in(_), in(_), out(_), go(Success)) ::
	any_term * int * int * int + 
          (returns(Success), foreign(smt_push_intp_group)).
smt_push_intp_group(SolverRef, Env, IntpGroup) :- 
	smt_push_intp_group_C(SolverRef, Env, IntpGroup, 1).

%------------------------------------------------------------------------------%
% smt_set_intp_group(+ref,+int,+int)
%------------------------------------------------------------------------------%
:- true pred smt_set_intp_group_C(in(_), in(_), in(_), go(Success)) ::
	any_term * int * int * int + 
          (returns(Success), foreign(smt_set_intp_group)).
smt_set_intp_group(SolverRef, Env, IntpGroup) :- 
	smt_set_intp_group_C(SolverRef, Env, IntpGroup, 1).

%------------------------------------------------------------------------------%
% smt_declare_real_var(+ref,+int,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_declare_real_var_C(in(_), in(_), out(_), go(Success))
	:: any_term * int * any_term * int + (returns(Success),
	foreign(smt_declare_real_var)).
smt_declare_real_var(SolverRef, Id, VarRef) :- 
	smt_declare_real_var_C(SolverRef, Id, VarRef, 1).

%------------------------------------------------------------------------------%
% smt_declare_int_var(+ref,+int,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_declare_int_var_C(in(_), in(_), out(_), go(Success))
	:: any_term * int * any_term * int + (returns(Success),
	foreign(smt_declare_int_var)).
smt_declare_int_var(SolverRef, Id, VarRef) :- 
	smt_declare_int_var_C(SolverRef, Id, VarRef, 1).

%------------------------------------------------------------------------------%
% stm_assert(+ref,+int,+list(clp_constraints))
%------------------------------------------------------------------------------%
:- true pred smt_assert_C(in(_), in(_), in(_), go(Success)) :: 
        any_term * int * any_term * int + (returns(Success),foreign(smt_assert)).
smt_assert(SolverRef, Env, Cs):- 
	smt_assert_C(SolverRef, Env, Cs, 1).

%------------------------------------------------------------------------------%
% smt_assert_formula(+ref,+int,+ref)
%------------------------------------------------------------------------------%
:- true pred smt_assert_formula_C(in(_), in(_), in(_), go(Success)) ::
	any_term * int * any_term * int + 
          (returns(Success), foreign(smt_assert_formula)).
smt_assert_formula(SolverRef, Env, FRef):- 
	smt_assert_formula_C(SolverRef, Env, FRef, 1).

%------------------------------------------------------------------------------%
% smt_check_sat(+ref,+int)
%------------------------------------------------------------------------------%
:- true pred smt_check_sat_C(in(_), in(_), go(Success)) ::
	any_term * int * int + (returns(Success), foreign(smt_check_sat)).
smt_check_sat(SolverRef, Env):- 
	smt_check_sat_C(SolverRef, Env, 1).

%------------------------------------------------------------------------------%
% smt_check_entailment(+ref,+int,+list(clp_constraints),+ref)
%------------------------------------------------------------------------------%
:- true pred smt_check_entailment_C(in(_), in(_), in(_), in(_), go(Success))
	:: any_term * int * any_term * any_term * int + 
           (returns(Success), foreign(smt_check_entailment)).
smt_check_entailment(SolverRef, Env, Cs, Q):- 
	smt_check_entailment_C(SolverRef, Env, Cs, Q, 1).

%------------------------------------------------------------------------------%
% smt_check_entailment_formulas(+ref,+int,+ref,+ref)
%------------------------------------------------------------------------------%
:- true pred smt_check_entailment_formulas_C(in(_),in(_),in(_),in(_),go(Success)) 
	:: any_term * int * any_term * any_term * int +
	(returns(Success), foreign(smt_check_entailment_formulas)).
smt_check_entailment_formulas(SolverRef, Env, P, Q):- 
	smt_check_entailment_formulas_C(SolverRef, Env, P, Q, 1).

%------------------------------------------------------------------------------%
% smt_interpolant_formulas(+ref,+int,+ref,+ref,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_interpolant_formulas_C(in(_), in(_), in(_), in(_), out(_),
	go(Success)) :: any_term * int * any_term * any_term * any_term * int +
          (returns(Success), foreign(smt_interpolant_formulas)).
smt_interpolant_formulas(SolverRef, Env, ARef, BRef, IntpRef):-
	smt_interpolant_formulas_C(SolverRef, Env, ARef, BRef, IntpRef, 1).

%------------------------------------------------------------------------------%
% smt_interpolant(+ref,+int,+list(int),-ref)
%------------------------------------------------------------------------------%
:- true pred smt_interpolant_C(in(_), in(_), in(_), out(_), go(Success)) ::
	any_term * int * any_term * any_term * int + 
          (returns(Success), foreign(smt_interpolant)).
smt_interpolant(SolverRef, Env, Groups, IntpRef):-
	smt_interpolant_C(SolverRef, Env, Groups, IntpRef, 1).

%------------------------------------------------------------------------------%
% smt_conjoin_formulas(+ref,+ref,+ref,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_conjoin_formulas_C(in(_), in(_), in(_), out(_),
	go(Success)) :: any_term * any_term * any_term * any_term *
	int + (returns(Success), foreign(smt_conjoin_formulas)).
smt_conjoin_formulas(SolverRef, F1Ref, F2Ref, F3Ref):- 
	smt_conjoin_formulas_C(SolverRef, F1Ref, F2Ref, F3Ref, 1).

%------------------------------------------------------------------------------%
% smt_disjoin_formulas(+ref,+ref,+ref,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_disjoin_formulas_C(in(_), in(_), in(_), out(_),
	go(Success)) :: any_term * any_term * any_term * any_term *
	int + (returns(Success), foreign(smt_disjoin_formulas)).
smt_disjoin_formulas(SolverRef, F1Ref, F2Ref, F3Ref):- 
	smt_disjoin_formulas_C(SolverRef, F1Ref, F2Ref, F3Ref, 1).

%------------------------------------------------------------------------------%
% smt_rename_formula(+ref,+ref,+ref,+ref,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_rename_formula_C(in(_), in(_), in(_), in(_), out(_),
	go(Success)) :: any_term * any_term * any_term * any_term *
	any_term * int + (returns(Success),
	foreign(smt_rename_formula)).
smt_rename_formula(SolverRef, FRef, OldVsRef, NewVsRef, NewFRef):- 
	smt_rename_formula_C(SolverRef, FRef, OldVsRef, NewVsRef, NewFRef, 1).

%------------------------------------------------------------------------------%
% smt_mk_true(+ref,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_mk_true_C(in(_), out(_), go(Success)) 
	:: any_term * any_term * int + (returns(Success), foreign(smt_mk_true)).
smt_mk_true(SolverRef, TrueRef):- 
	smt_mk_true_C(SolverRef, TrueRef, 1).

%------------------------------------------------------------------------------%
% smt_is_true(+ref,+ref)
%------------------------------------------------------------------------------%
:- true pred smt_is_true_C(in(_), in(_), go(Success)) :: any_term *
	any_term * int + (returns(Success), foreign(smt_is_true)).
smt_is_true(SolverRef, ExprRef):- 
	smt_is_true_C(SolverRef, ExprRef, 1).

%------------------------------------------------------------------------------%
% smt_all_vars_included(+ref,+ref,+ref)
%------------------------------------------------------------------------------%
:- true pred smt_all_vars_included_C(in(_), in(_), in(_), go(Success)) :: any_term *
	any_term * any_term * int + (returns(Success), foreign(smt_all_vars_included)).
smt_all_vars_included(SolverRef, ExprRef, VsRef):- 
	smt_all_vars_included_C(SolverRef, ExprRef, VsRef, 1).

%------------------------------------------------------------------------------%
% smt_mk_not(+ref,+ref,-ref)
%------------------------------------------------------------------------------%
:- true pred smt_mk_not_C(in(_), in(_), out(_), go(Success)) ::
	any_term * any_term * any_term * int + 
          (returns(Success), foreign(smt_mk_not)).
smt_mk_not(SolverRef, FRef, NotFRef):- 
	smt_mk_not_C(SolverRef, FRef, NotFRef, 1).

%------------------------------------------------------------------------------%
% smt_print(+ref,+int)
%------------------------------------------------------------------------------%
:- true pred smt_print_C(in(_), in(_), go(Success)) 
	:: any_term * int * int + (returns(Success), foreign(smt_print)).
smt_print(SolverRef,Env):- 
	smt_print_C(SolverRef, Env, 1).

%------------------------------------------------------------------------------%
% smt_formula_to_string(+ref,+ref,-string)
%------------------------------------------------------------------------------%
:- true pred smt_formula_to_string_C(in(_), in(_), out(_),
	go(Success)) :: any_term * any_term * string * int +
	(returns(Success), foreign(smt_formula_to_string)).
smt_formula_to_string(SolverRef, ExpRef, Str):-
	smt_formula_to_string_C(SolverRef, ExpRef, Str, 1).




	
