/* Author: Jorge. A Navas, The University of Melbourne 2012-2013    */

#include <solver/CiaoSMT.h>

using namespace Ciao;
using namespace std;
using namespace solver;

void HALT(error e){
  /*
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom_chars(et, e.what());
  Prolog_raise_exception(et);
  */
  cerr << e << endl;
  exit(EXIT_FAILURE);
}

vector<Exp> convert_ref_to_Exp_vector(Prolog_term_ref l, Solver *solver);

/*
  unsigned RSHash(const char* str){
  unsigned b = 378551;
  unsigned a = 63689;
  unsigned hash = 0;
  
  while (*str){
  hash = hash * a + (*str++);
  a *= b;
  }
  return (hash & 0x7FFFFFFF);
  }
*/

// Convert a ciao term into Exp. 
// Prolog_term_ref are not unique so we need to create a hash value
// traversing the prolog term. The 3rd argument is just for hashing.
Exp convertCiaoTermToSMT(Solver *solver, Prolog_term_ref t, string &t_str){
  if (Prolog_is_variable(t))
    throw error("problems during conversion from ciao term to smt formulae.");

  // CiaoTermMap::iterator it(ciao_term_map.find(t));
  // if(it != ciao_term_map.end())
  //   return (*it).second;

  stringstream ss;

  Exp res;
  if  (Prolog_is_address(t)){
    Exp *v = term_to_handle<Exp>(t);
    res = *v;
    ss << v->exp;
    t_str = ss.str();
  }
  else if (Prolog_is_integer(t)){
    int n;
    int ok = Prolog_get_int(t,&n);
    if (!ok)
      throw error("problems during conversion from ciao term to smt formulae.");
    res = solver->MkNumber(n);
    ss << n;
    t_str = ss.str();
  }
  else if (Prolog_is_compound(t)){
    Prolog_atom name;
    int arity;
    Prolog_get_compound_name_arity(t, &name, &arity);
    if (!(arity > 0 && arity <= 2))
      throw error("problems during conversion from ciao term to smt formulae.");

    if (arity == 1){
      Prolog_term_ref arg;
      int ok = Prolog_get_arg(1, t, arg);
      if (!ok)
        throw error("problems during conversion from ciao term to smt formulae.");
      string arg_str;
      Exp ExpArg = convertCiaoTermToSMT(solver,arg, arg_str);
      if (strcmp(name, "not") == 0){
        res = ~ExpArg;
        t_str = "not(" + arg_str + ")";
      }
      else if (strcmp(name, "-") == 0){    
        res = solver->MkNumber(0)-ExpArg;
        t_str = "-(" + arg_str + ")";
      }
      else
        throw error("unsupported unary operator");
    }
    else if (arity == 2){
      Prolog_term_ref arg1;
      Prolog_term_ref arg2;
      int ok = Prolog_get_arg(1, t, arg1);
      if (!ok)
        throw error("problems during conversion from ciao term to smt formulae.");
      ok = Prolog_get_arg(2, t, arg2);
      if (!ok)
        throw error("problems during conversion from ciao term to smt formulae.");
      string arg1_str;
      string arg2_str;
      Exp ExpArg1 = convertCiaoTermToSMT(solver,arg1,arg1_str);
      Exp ExpArg2 = convertCiaoTermToSMT(solver,arg2,arg2_str);
      if (strcmp(name, ".=.") == 0){
        res = ExpArg1==ExpArg2; 
        t_str = "=(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, ".<>.") == 0){
        res = ~(ExpArg1==ExpArg2); 
        t_str = "<>(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, ".>.") == 0){
        res = ExpArg1>ExpArg2; 
        t_str = ">(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, ".>=.") == 0){
        res = ExpArg1>=ExpArg2;
        t_str = ">=(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, ".<.") == 0){
        res = ExpArg1<ExpArg2; 
        t_str = "<(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, ".=<.") == 0){
        res = ExpArg1<=ExpArg2;
        t_str = "=<(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, "+") == 0){
        res = ExpArg1+ExpArg2;
        t_str = "+(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, "-") == 0){
        res = ExpArg1-ExpArg2;
        t_str = "-(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, "*") == 0){
        res = ExpArg1*ExpArg2;
        t_str = "*(" + arg1_str + "," + arg2_str + ")";
      }
      else if (strcmp(name, "/") == 0){
        res = ExpArg1/ExpArg2;
        t_str = "/" + arg1_str + "," + arg2_str + ")";
      }
      else
        cout << "Unsupported binary operator" << endl;
    }
  }
  //  ciao_term_map[RSHash(t_str.c_str())] = res;
  return res;
}

Exp convertCiaoTermToSMT(Solver *solver, Prolog_term_ref t){
  string t_str;
  return convertCiaoTermToSMT(solver,t,t_str);
}


// Convert a list of ciao terms into solver:Exp
Exp convertCiaoTermListToSMT(Solver *solver, Prolog_term_ref c){
  if (!(Prolog_is_cons(c) || Prolog_empty_cons(c)))
    throw error("problems during conversion from ciao list to smt formulae.");
  Exp f = solver->BoolConst(true);
  if (Prolog_empty_cons(c))
    return f;

  while (!Prolog_empty_cons(c)){
    Prolog_term_ref h;
    Prolog_term_ref t;    
    Prolog_get_cons(c, h, t);
    f = f&convertCiaoTermToSMT(solver, h);
    Prolog_put_term(c,t);
  }
  return f;
}

extern "C" Prolog_foreign_return_type
smt_create(Prolog_term_ref ref_solver_name,int num_env,Prolog_term_ref ref_solver) {
  try {

    Prolog_atom solver_name;
    int ok = Prolog_get_atom_name(ref_solver_name, &solver_name);
    if(!ok)
      throw error("problems in smt_create.");

    Solver *solver;
    if (strcmp(solver_name, "mathsat") == 0){
      solver = new MSat(num_env);
    }
    // else if (strcmp(solver_name,"z3") == 0){
    //   solver = new Z3();
    // }
    else
      throw error("unsupported smt solver");

    Prolog_term_ref term = Prolog_new_term_ref(); 
    Prolog_put_address(term, solver);
    return Prolog_unify(ref_solver, term) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
smt_stop(Prolog_term_ref ref_solver) {
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    delete solver;
    ciao_term_map.clear();
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
smt_push(Prolog_term_ref ref_solver, int env) {
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    solver->Push(env);
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
smt_pop(Prolog_term_ref ref_solver, int env) {
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    solver->Pop(env);
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
smt_reset(Prolog_term_ref ref_solver, int env) {
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    solver->Reset(env);
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}


extern "C" Prolog_foreign_return_type
smt_push_intp_group(Prolog_term_ref ref_solver, int env, Prolog_term_ref g) {
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_long(tmp,  solver->PushIntpCut(env));
    return Prolog_unify(g, tmp) ? PROLOG_SUCCESS : PROLOG_FAILURE;    
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
smt_set_intp_group(Prolog_term_ref ref_solver, int env, int g) {
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    solver->SetIntpGroup(g,env);
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_declare_int_var(Prolog_term_ref ref_solver, int id, Prolog_term_ref term){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp  v = solver->DeclareIntVariable(id);
    Prolog_term_ref tmp = Prolog_new_term_ref(); 
    Prolog_put_address(tmp, new Exp((Solver*) solver, v.exp));
    return Prolog_unify(tmp, term) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_declare_real_var(Prolog_term_ref ref_solver, int id, Prolog_term_ref term){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp  v = solver->DeclareRealVariable(id);
    Prolog_term_ref tmp = Prolog_new_term_ref(); 
    Prolog_put_address(tmp, new Exp((Solver*) solver, v.exp));
    return Prolog_unify(tmp, term) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_assert(Prolog_term_ref ref_solver, int env, Prolog_term_ref constraints){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp e = convertCiaoTermListToSMT(solver, constraints);
    solver->AssertFormula(e, env);
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_assert_formula(Prolog_term_ref ref_solver, int env, Prolog_term_ref ref_p){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *p = term_to_handle<Exp>(ref_p);
    solver->AssertFormula(*p, env);
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_check_sat(Prolog_term_ref ref_solver, int env){

  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    return solver->Check_Sat(env) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

// p is a list of ciao constraints but q is already a reference to
// Exp
extern "C" Prolog_foreign_return_type 
smt_check_entailment(Prolog_term_ref ref_solver, int env,
                     Prolog_term_ref ref_ps, Prolog_term_ref ref_q){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp  p = convertCiaoTermListToSMT(solver, ref_ps);
    Exp *q = term_to_handle<Exp>(ref_q);
    return solver->Check_Entailment(p, *q, true, env) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

// both ref_p and ref_q are pointers to Exp
extern "C" Prolog_foreign_return_type 
smt_check_entailment_formulas(Prolog_term_ref ref_solver, int env,
                              Prolog_term_ref ref_p, Prolog_term_ref ref_q){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *p = term_to_handle<Exp>(ref_p);
    Exp *q = term_to_handle<Exp>(ref_q);
    return solver->Check_Entailment(*p,*q,true,env) ? 
        PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}


vector<int> convert_ref_to_int_vector(Prolog_term_ref l){
  vector<int> v;
  Prolog_term_ref c = Prolog_new_term_ref();   
  Prolog_put_term(c, l);
  while (!Prolog_empty_cons(c)){
    Prolog_term_ref h;
    Prolog_term_ref t;    
    Prolog_get_cons(c, h, t);
    int n;
    int ok = Prolog_get_int(h,&n);
    if (!ok)
      throw error("problems during coversion of references.");
    v.push_back(n);
    Prolog_put_term(c,t);
  }
  return v;
}

extern "C" Prolog_foreign_return_type 
smt_interpolant_formulas(Prolog_term_ref ref_solver, int env,
                         Prolog_term_ref A_ref, Prolog_term_ref B_ref, 
                         Prolog_term_ref intp_ref){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    // Convert to a vector of Exp
    // vector<Exp> A = convertCiaoTermListToSMTVector(solver,A_ref);
    // vector<Exp> B = convertCiaoTermListToSMTVector(solver,B_ref);
    // Convert to a Exp
    Exp  A = convertCiaoTermListToSMT(solver, A_ref);
    Exp  B = convertCiaoTermListToSMT(solver, B_ref);

    Prolog_term_ref  t = Prolog_new_term_ref();
    Prolog_put_address(t, new Exp((Solver*) solver, 
                                  solver->GenerateInterpolant(A,B,env).exp));
    return Prolog_unify(intp_ref, t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_interpolant(Prolog_term_ref ref_solver, int env, Prolog_term_ref groups_ref, 
                Prolog_term_ref intp_ref){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    SeqIntpGroups groups;
    if (Prolog_empty_cons(groups_ref))
      throw error("interpolant group cannot be empty.");
    Prolog_term_ref c = Prolog_new_term_ref();   
    Prolog_put_term(c,groups_ref);
    while (!Prolog_empty_cons(c)){
      Prolog_term_ref h;
      Prolog_term_ref t;    
      Prolog_get_cons(c, h, t);
      int n;
      int ok = Prolog_get_int(h,&n);
      if (!ok)
        throw error("problems in smt_interpolant.");
      groups.push_back(n);
      Prolog_put_term(c,t);
    }
    Prolog_term_ref  t = Prolog_new_term_ref();
    Prolog_put_address(t, new Exp((Solver*) solver, 
                                  solver->GenerateInterpolant(groups, env).exp));
    return Prolog_unify(intp_ref, t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_conjoin_formulas(Prolog_term_ref ref_solver, 
                     Prolog_term_ref ref_f1, Prolog_term_ref ref_f2, 
                     Prolog_term_ref ref_f3){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *e1 = term_to_handle<Exp>(ref_f1);
    Exp *e2 = term_to_handle<Exp>(ref_f2);
    Prolog_term_ref t = Prolog_new_term_ref();
    Exp * r_ref = new Exp((Solver*) solver, ((*e1)&(*e2)).exp);
    Prolog_put_address(t, r_ref);
    return Prolog_unify(ref_f3,t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;

}

extern "C" Prolog_foreign_return_type 
smt_disjoin_formulas(Prolog_term_ref ref_solver, 
                     Prolog_term_ref ref_f1, Prolog_term_ref ref_f2, 
                     Prolog_term_ref ref_f3){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *e1 = term_to_handle<Exp>(ref_f1);
    Exp *e2 = term_to_handle<Exp>(ref_f2);

    Prolog_term_ref t = Prolog_new_term_ref();
    Exp * r_ref = new Exp((Solver*) solver, ((*e1)|(*e2)).exp);
    Prolog_put_address(t, r_ref);
    return Prolog_unify(ref_f3,t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

vector<Exp> 
convert_ref_to_Exp_vector(Prolog_term_ref l, Solver *solver){
  vector<Exp> v;
  Prolog_term_ref c = Prolog_new_term_ref();   
  Prolog_put_term(c, l);
  while (!Prolog_empty_cons(c)){
    Prolog_term_ref h;
    Prolog_term_ref t;    
    Prolog_get_cons(c, h, t);
    if (Prolog_is_address(h)){
      Exp *e = term_to_handle<Exp>(h);      
      v.push_back(*e);
    }
    else if  (Prolog_is_integer(h)){
      int n;
      int ok = Prolog_get_int(h,&n);
      if (!ok)
        throw error("problems during conversion of references.");
      Exp e = solver->MkNumber(n);
      v.push_back(e);
    }
    else
      throw error("while converting a list of references expected either variables or numbers");
      
    Prolog_put_term(c,t);
  }
  return v;
}

extern "C" Prolog_foreign_return_type 
smt_rename_formula(Prolog_term_ref ref_solver, Prolog_term_ref ref_f, 
                   Prolog_term_ref ref_old_vs, Prolog_term_ref ref_new_vs,  
                   Prolog_term_ref ref_new_f){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp  *old_e = term_to_handle<Exp>(ref_f);
    
    vector<Exp> old_vs = convert_ref_to_Exp_vector(ref_old_vs, solver);
    vector<Exp> new_vs = convert_ref_to_Exp_vector(ref_new_vs, solver);
    
    if (old_vs.size() != new_vs.size())
      throw error("when renaming a formula the number of old and new vars must be the same");
    Exp new_e = solver->rename(*old_e,old_vs,new_vs);
    Prolog_term_ref t = Prolog_new_term_ref();
    Exp * r_ref = new Exp((Solver*) solver, new_e.exp);
    Prolog_put_address(t, r_ref);
    return Prolog_unify(t,ref_new_f) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_mk_true(Prolog_term_ref ref_solver, Prolog_term_ref ref_f){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Prolog_term_ref t = Prolog_new_term_ref();
    Exp * e = new Exp((Solver*) solver, (solver->BoolConst(true)).exp);
    Prolog_put_address(t, e);
    return Prolog_unify(ref_f,t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_is_true(Prolog_term_ref ref_solver, Prolog_term_ref ref_exp){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *e       = term_to_handle<Exp>(ref_exp);
    return solver->IsTrue(*e) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}


extern "C" Prolog_foreign_return_type 
smt_is_false(Prolog_term_ref ref_solver, Prolog_term_ref ref_exp){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *e       = term_to_handle<Exp>(ref_exp);
    return solver->IsFalse(*e) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}


extern "C" Prolog_foreign_return_type 
smt_all_vars_included(Prolog_term_ref ref_solver,
                      Prolog_term_ref ref_exp, Prolog_term_ref ref_vs){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *e         = term_to_handle<Exp>(ref_exp);
    vector<Exp> vs = convert_ref_to_Exp_vector(ref_vs, solver);
    return solver->all_vars_included(*e, vs) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}


extern "C" Prolog_foreign_return_type 
smt_mk_not(Prolog_term_ref ref_solver, Prolog_term_ref ref_f, 
           Prolog_term_ref not_ref_f){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *f         = term_to_handle<Exp>(ref_f);
    Exp * e = new Exp((Solver*) solver, solver->apply(OP_NOT,(*f).exp));
    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_put_address(t, e);
    return Prolog_unify(not_ref_f, t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}


extern "C" Prolog_foreign_return_type 
smt_formula_to_string(Prolog_term_ref ref_solver, Prolog_term_ref ref_exp, 
                      Prolog_term_ref str){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    Exp *e = term_to_handle<Exp>(ref_exp);
    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_put_atom_chars(t, solver->ExpString(*e).c_str());
    return Prolog_unify(str,t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type 
smt_print(Prolog_term_ref ref_solver, int env){
  try {
    Solver *solver = term_to_handle<Solver>(ref_solver);
    solver->PrintAssertedFormulas(env);
    return PROLOG_SUCCESS;
  }
  catch (error e){
    HALT(e);
  }
  return PROLOG_FAILURE;
}



