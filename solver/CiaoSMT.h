/* Author: Jorge. A Navas, The University of Melbourne 2012-2013  */

#ifndef __CIAOSMT__H
#define __CIAOSMT__H

#include <include/Ciao.h>
#include <solver/Common.h>
#include <solver/SMT_solvers/util/cache.h>
#include <solver/SMT_solvers/Solver.h>
// Here we include all the SMT implementations we want to use
#include <solver/SMT_solvers/MSat.h>

namespace Ciao {

  typedef NilC<solver::Exp>::cache CiaoTermMap;
  static CiaoTermMap ciao_term_map;

  extern "C" Prolog_foreign_return_type 
  smt_create(Prolog_term_ref, int env, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_stop(Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_push(Prolog_term_ref, int env);
  extern "C" Prolog_foreign_return_type 
  smt_pop(Prolog_term_ref, int env);
  extern "C" Prolog_foreign_return_type 
  smt_reset(Prolog_term_ref,int env);
  // Add interpolation cutpoints
  extern "C" Prolog_foreign_return_type 
  smt_push_intp_group(Prolog_term_ref, int env, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_set_intp_group(Prolog_term_ref, int env, int);
  // Declaration of variables
  extern "C" Prolog_foreign_return_type 
  smt_declare_int_var(Prolog_term_ref, int, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_declare_real_var(Prolog_term_ref, int, Prolog_term_ref);
  // Assert variants
  extern "C" Prolog_foreign_return_type 
  smt_assert(Prolog_term_ref, int env, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_assert_formula(Prolog_term_ref, int env, Prolog_term_ref);
  // Satisfiability check
  extern "C" Prolog_foreign_return_type 
  smt_check_sat(Prolog_term_ref, int env);
  // Entailment test variants
  extern "C" Prolog_foreign_return_type
  smt_check_entailment(Prolog_term_ref, int env,
                       Prolog_term_ref,Prolog_term_ref);
  extern "C" Prolog_foreign_return_type
  smt_check_entailment_formulas(Prolog_term_ref,int env,
                                Prolog_term_ref,Prolog_term_ref);
  // Interpolation variants
  extern "C" Prolog_foreign_return_type 
  smt_interpolant_formulas(Prolog_term_ref, int env, Prolog_term_ref, 
                           Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_interpolant(Prolog_term_ref, int env, Prolog_term_ref, Prolog_term_ref);
  // Build formulae
  extern "C" Prolog_foreign_return_type 
  smt_conjoin_formulas(Prolog_term_ref, Prolog_term_ref, 
                       Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_disjoin_formulas(Prolog_term_ref, Prolog_term_ref, 
                       Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_rename_formula(Prolog_term_ref, Prolog_term_ref, Prolog_term_ref, 
                     Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_mk_true(Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_is_true(Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_is_false(Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_mk_not(Prolog_term_ref, Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_all_vars_included(Prolog_term_ref, Prolog_term_ref, Prolog_term_ref);
  // Pretty-printer
  extern "C" Prolog_foreign_return_type 
  smt_formula_to_string(Prolog_term_ref, Prolog_term_ref, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  smt_print(Prolog_term_ref, int env);

} // end namespace Ciao

#endif /* __CIAOSMT__H */
