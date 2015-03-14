#include <solver/SMT_solvers/MSat.h>

////////////////////////////////////////////////////////////////////////////
/// \file   MSat.cpp
////////////////////////////////////////////////////////////////////////////

using namespace solver;
using namespace Ciao;

/// Print the asserted formulas in the solver.
void MSat::PrintAssertedFormulas(ENV_KIND env_id){
  msat_env env = _env[env_id];
  size_t num_of_asserted;
  msat_term * asserted_formulas = 
      msat_get_asserted_formulas(env,&num_of_asserted);
  cout << "solver(\n\n";
  for (unsigned i=0; i< num_of_asserted; i++){
    char *s = msat_term_repr(asserted_formulas[i]);    
    cout << "\t" << i+1 << ":"<< s << "\n";
    msat_free(s);
  }
  cout << ")\n";
  msat_free(asserted_formulas);
}

// Public methods of the class
void MSat::Push(ENV_KIND env_id){
  msat_env env = _env[env_id];
  int ok = msat_push_backtrack_point(env);
  if (ok)
    throw error("mathsat push " +  string(msat_last_error_message(env)));
#ifdef  DEBUG_MATHSAT
  cout << "Mathsat pushed a choice point.\n";      
#endif 
}
  
void MSat::Pop(ENV_KIND env_id){
  msat_env env = _env[env_id];
  int ok = msat_pop_backtrack_point(env);
  if (ok)
    throw error("mathsat pop " +  string(msat_last_error_message(env)));
#ifdef  DEBUG_MATHSAT
  cout << "Mathsat popped a choice point!\n";
#endif
}

void MSat::Reset(ENV_KIND env_id){
  msat_env env = _env[env_id];
  int ok = msat_reset_env(env);
  if (ok)
    throw error("mathsat reset " +  string(msat_last_error_message(env)));
#ifdef  DEBUG_MATHSAT
  cout << "Mathsat deleted the assertion stack!\n";
#endif
}

bool MSat::IsTrue(const Exp& expr){
  msat_term t = terms[expr.exp];
  return msat_term_is_true(_env[0],t);
}

bool MSat::IsFalse(const Exp& expr){
  msat_term t = terms[expr.exp];
  return msat_term_is_false(_env[0],t);
}

Exp MSat::BoolConst(bool val){
  msat_term term = val ? msat_make_true(_env[0]) : msat_make_false(_env[0]);
  return Exp(this, term_to_bexp(term));
}

Exp MSat::MkNumber(int n){
  MkNumberMap::iterator it(mknumber_map.find(n));
  if(it != mknumber_map.end())
    return (*it).second;

  ostringstream convert; 
  convert << n;      
  msat_term term = msat_make_number(_env[0], convert.str().c_str());
  Exp res(this, term_to_bexp(term));
  mknumber_map[n] = res;
  return res;
}

void MSat::AssertFormula(const Exp& expr, ENV_KIND env_id){
  msat_env env = _env[env_id];

  msat_term t = terms[expr.exp];
  // returns nonzero on error
  int ok = msat_assert_formula(env,t);
  if (ok)
    throw error("mathsat assert " + string(msat_last_error_message(env)));
  
  // PrintAssertedFormulas(env);
}

int MSat::PushIntpCut(ENV_KIND env_id){

  msat_env env = _env[env_id];
  int g = msat_create_itp_group(env);   
  if (g < 0)
    throw error("mathsat create_itp_group " + string(msat_last_error_message(env)));
  // nonzero on error
  if (msat_set_itp_group(env, g))
    throw error("mathsat set_itp_group " + string(msat_last_error_message(env)));
#ifdef  DEBUG_MATHSAT
  cout << "Mathsat added an interpolation cutpoint (internal id= " << g << ")\n";
#endif 
  return g;
}

void  MSat::SetIntpGroup(int g, ENV_KIND env_id){

  msat_env env = _env[env_id];
  if (msat_set_itp_group(env, g))
    throw error("mathsat set_itp_group " + string(msat_last_error_message(env)));

#ifdef  DEBUG_MATHSAT
  cout << "Mathsat switched interpolation group (internal id= " << g << ")\n";
#endif 
}

bool MSat::Check_Sat(ENV_KIND env_id){
  msat_env env = _env[env_id];
  msat_result status = msat_solve(env);
  bool sat_flag = true;
  switch (status){
  case MSAT_SAT:
    break;
  case MSAT_UNSAT:
    sat_flag=false;
    break;
  default:
    throw error("mathsat check_sat " + string(msat_last_error_message(env)));
  } 
  return sat_flag;
}

// Assert both P and Q in env_id environment.
bool MSat::
Check_Entailment(const Exp& P, const Exp& Q, bool Pop, ENV_KIND env_id){  

  msat_env env = _env[env_id];
  msat_term P_term      = terms[P.exp]; 
  // not Q
  msat_term Q_term      = terms[Q.exp]; 
  msat_term Not_Q_term  = msat_make_not(env, Q_term);
  if (MSAT_ERROR_TERM(Not_Q_term))
    throw error("mathsat entailment "  + string(msat_last_error_message(env)));

#ifdef DEBUG_MATHSAT
  PrintAssertedFormulas();
  cout << "Checking if " << msat_term_repr(P_term) << " entails " 
       << msat_term_repr(Q_term) << endl;
#endif 
  ///
  int ok = msat_push_backtrack_point(env);  
  SetIntpGroup(_root[env_id]);
  ///
  ok = msat_assert_formula(env, P_term);
  if (ok){
    throw error("mathsat entailment " + string(msat_last_error_message(env)));
    cout << msat_last_error_message(env) << endl;
  }
  ok = msat_assert_formula(env, Not_Q_term);
  if (ok)
    throw error("mathsat entailment " + string(msat_last_error_message(env)));

  msat_result status = msat_solve(env);
  bool entailment_flag;
  switch (status){
  case MSAT_SAT:
    entailment_flag = false;    
    break;
  case MSAT_UNSAT:
    entailment_flag = true;    
    break;
  default:  
    throw error("mathsat entailment " + string(msat_last_error_message(env)));
  }
  
  if (Pop){
   ok = msat_pop_backtrack_point(env);
   if (ok)
     throw error("mathsat entailment " + string(msat_last_error_message(env)));
  }
  return entailment_flag;
}

Exp MSat::
GenerateInterpolant(const vector<Exp> &A, const vector<Exp> &B, ENV_KIND env_id){
                    
  msat_env env = _env[env_id];
  int ok = msat_push_backtrack_point(env);
  if (ok)
    throw error("mathsat gen interpolant "  + string(msat_last_error_message(env)));

  int group_a = msat_create_itp_group(env); 
  int group_b = msat_create_itp_group(env); 
  if (!(group_a != -1 && group_b != -1))
    throw error("mathsat problems during interpolation generation.");

  // A
  ok = msat_set_itp_group(env, group_a);
  if (ok)
    throw error("mathsat gen interpolant "  + string(msat_last_error_message(env))); 
  for(unsigned i=0; i<A.size(); i++){
    msat_term t_a = terms[A[i].exp];
    ok = msat_assert_formula(env,t_a);
    if (ok)
      throw error("mathsat gen interpolant "  + string(msat_last_error_message(env)));
  }

  // B
  ok = msat_set_itp_group(env, group_b);
  if (ok)
    throw error("mathsat gen interpolant "  + string(msat_last_error_message(env)));
  for(unsigned j=0; j<B.size(); j++){
    msat_term t_b = terms[B[j].exp];
    int ok = msat_assert_formula(env,t_b);
    if (ok)
      throw error("mathsat gen interpolant " + string(msat_last_error_message(env)));
  }

  //  Extract interpolant
  if (!(msat_solve(env) == MSAT_UNSAT))    
    throw error("mathsat interpolation generation: A and B must be unsat!");

  int groups_of_a[1] = { group_a };
  msat_term itp = msat_get_interpolant(env, groups_of_a, 1);
  if (MSAT_ERROR_TERM(itp))
    throw error("mathsat gen interpolant " + string(msat_last_error_message(env)));

  ok = msat_pop_backtrack_point(env);
  if (ok)
    throw error("mathsat gen interpolant " + string(msat_last_error_message(env)));
  
  Exp res(Exp((Solver*) this, term_to_bexp(itp)));
  return res; 
}


// Return a single interpolant (A,B) where the vector groups_of_a is A
// and B is the rest of groups.
Exp MSat::GenerateInterpolant(SeqIntpGroups groups_of_a, ENV_KIND env_id) {
  msat_env env = _env[env_id];
  //PrintAssertedFormulas();    
  msat_term itp = msat_get_interpolant(env, &groups_of_a[0], groups_of_a.size());
  if (MSAT_ERROR_TERM(itp))
    throw error("mathsat gen interpolant "  + string(msat_last_error_message(env)));  
  // cout << "-----------------------------\n";
  // for(unsigned i=0; i<= groups.size(); i++){
  //   cout << "Intp group " << groups[i] << ": " 
  // 	      << msat_term_repr(msat_get_interpolant(env, groups_of_a, i)) << "\n";
  // }
  // cout << "-----------------------------\n";
  return Exp((Solver*) this, term_to_bexp(itp));
}

// Return a sequence interpolant.
// groups is A and selgroups only those relevant cutpoints.
// pre: selgroups must be a subset of groups!
SeqIntp MSat::
GenerateSeqInterpolant(SeqIntpGroups groups,  SeqIntpGroups selgroups, 
                       ENV_KIND env_id) {
  // This procedure is linear on the size of groups.
                       
  msat_env env = _env[env_id];
  //PrintAssertedFormulas();
  SeqIntp Intps;
  while( !selgroups.empty()){
    const int last = selgroups.back();
    // throw away those groups that are not in selgroups
    while (groups.back() != last){ 
      groups.pop_back(); 
    }
    msat_term itp = msat_get_interpolant(env, &groups[0], groups.size());
    if (MSAT_ERROR_TERM(itp))
      throw error("mathsat gen interpolant " + string(msat_last_error_message(env)));

    Intps.push_front(Exp((Solver*) this, term_to_bexp(itp)));
    groups.pop_back();
    selgroups.pop_back();
  }
  return Intps;
}

_Exp MSat::apply(Op op, const _Exp& x, const _Exp& y)
{
  msat_term term;
  switch(op)
  {
    case OP_AND:
      term = msat_make_and(_env[0], terms[x], terms[y]);
      break;
    case OP_OR:
      term = msat_make_or(_env[0], terms[x], terms[y]);
      break;  
    case OP_XOR:
      term = msat_make_not(_env[0], msat_make_iff(_env[0], terms[x], terms[y]));
      break;
    case OP_IFF:
      term = msat_make_iff(_env[0], terms[x], terms[y]);
      break;
    case OP_EQ:
      term = msat_make_equal(_env[0], terms[x], terms[y]);
      break;
    case OP_GEQ:
      term = msat_make_leq(_env[0], terms[y], terms[x]);
      break;
    case OP_GT:
      term = msat_make_not(_env[0], msat_make_leq(_env[0], terms[x], terms[y]));
      break;
    case OP_LEQ:
      term = msat_make_leq(_env[0], terms[x], terms[y]);
      break;
    case OP_LT:
      term = msat_make_not(_env[0], msat_make_leq(_env[0], terms[y], terms[x]));
      break;
    case OP_PLUS:
      term = msat_make_plus(_env[0], terms[x], terms[y]);
      break;
    case OP_MINUS:
      { 
        ostringstream s;
        s << "(- 0  ";
        s << msat_to_smtlib2_term(_env[0], terms[y]);
        s << ")";
        msat_term minus_y = msat_from_string(_env[0], s.str().c_str());

        if (MSAT_ERROR_TERM(minus_y))
          throw error("mathsat error term "  + string(msat_last_error_message(_env[0])));

        term = msat_make_plus(_env[0], terms[x], minus_y);
      }
      break;
    case OP_TIMES:
      term = msat_make_times(_env[0], terms[x], terms[y]);
      break;
    case OP_DIV:
      {
        ostringstream s;
        s << "(/ ";
        s << msat_to_smtlib2_term(_env[0], terms[x]);
        s << " ";
        s << msat_to_smtlib2_term(_env[0], terms[y]);
        s << ")";
        term = msat_from_string(_env[0], s.str().c_str());
      }
      break;
    default:
      throw error("mathsat invalid operator");
  }
  if (MSAT_ERROR_TERM(term))
    throw error("mathsat "  + string(msat_last_error_message(_env[0])));
  
  return term_to_bexp(term);
}

_Exp MSat::apply(Op op, const _Exp& x)
{
  msat_term term;
  switch(op){
    case OP_NOT:
      term = msat_make_not(_env[0], terms[x]);
      break;
    default:
      throw error("mathsat invalid operator");
  }
  return term_to_bexp(term);
}

MSat::RewriteMap 
MSat::make_rename_map(const vector<Exp> &OldVs, const vector<Exp> &NewVs){
  // FIXME: we do not cache the mappings.
  if (!(OldVs.size() == NewVs.size()))
    throw error("renaming with different length of old and new variables.");
  MSat::RewriteMap m;
  for(unsigned i=0; i<OldVs.size();i++){
    msat_term term = terms[OldVs[i].exp];
    MSat::RewriteMap::iterator I = m.find(msat_term_id(term));
    if (I != m.end()) continue;
    m.insert(make_pair(msat_term_id(term),terms[NewVs[i].exp]));
  }
  return m;
}

Exp MSat::rename(const Exp& phi, 
                 const vector<Exp> &old_vars, const vector<Exp> &new_vars){

  RewriteMap  m = make_rename_map(old_vars, new_vars);
  msat_term term = rename(terms[phi.exp], m);
  return Exp((Solver*) this, term_to_bexp(term));
}

msat_term MSat::rename(msat_term term, RewriteMap rewrite_map){

  // TODO: we do not cache the partial renamed terms.
  RewriteMap::iterator it(rewrite_map.find(msat_term_id(term)));
  if(it != rewrite_map.end())
    return (*it).second;

  size_t arity = msat_term_arity(term);
  // It's possible that some terms does not have mapping
  if(arity == 0)
    return term;
  
  if (arity > 2)
    throw error("renaming an unrecognized operator.");

  msat_term ret;
  if(msat_term_is_and(_env[0], term)){
    ret = msat_make_and(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map),
                        rename(msat_term_get_arg(term, 1), rewrite_map));
  } 
  else if(msat_term_is_or(_env[0], term)) {
    ret = msat_make_or(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map),
                       rename(msat_term_get_arg(term, 1), rewrite_map));
  } 
  else if(msat_term_is_iff(_env[0], term)) {
    ret = msat_make_iff(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map),
                        rename(msat_term_get_arg(term, 1), rewrite_map));
  } 
  else if(msat_term_is_not(_env[0], term)) {
    ret = msat_make_not(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map));
  } 
  else if(msat_term_is_equal(_env[0], term)) {
    ret = msat_make_equal(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map),
                          rename(msat_term_get_arg(term, 1), rewrite_map));
  }
  else if(msat_term_is_leq(_env[0], term)) {
    ret = msat_make_leq(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map),
                        rename(msat_term_get_arg(term, 1), rewrite_map));
  }
  else if(msat_term_is_plus(_env[0], term)) {
    ret = msat_make_plus(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map),
                         rename(msat_term_get_arg(term, 1), rewrite_map));
  }
  else if(msat_term_is_times(_env[0], term)) {
    ret = msat_make_times(_env[0], rename(msat_term_get_arg(term, 0), rewrite_map),
                          rename(msat_term_get_arg(term, 1), rewrite_map));
  }
  else{ 
    throw error("mathsat unsupported node type");
  }
  
  return ret;
}

bool MSat::all_vars_included(msat_term term , const set<size_t> &ids){
  if (msat_term_is_constant(_env[0], term) != 0){ 
    // term is a var
    return (ids.count(msat_term_id(term)) > 0);
  }
  else{
    size_t arity = msat_term_arity(term);
    if (arity == 0){
      return true;
    }
    else if (arity == 1){
      return (all_vars_included(msat_term_get_arg(term, 0), ids));
    }
    else if (arity == 2){
      return ( all_vars_included(msat_term_get_arg(term, 0), ids) && 
               all_vars_included(msat_term_get_arg(term, 1), ids));
    }
    else 
      throw error("unrecognized operator.");
  }
}

bool MSat::all_vars_included(const Exp &formula, const vector<Exp>&vars){
  msat_term term = terms[formula.exp];
  set<size_t> ids;
  for(unsigned int i=0;i<vars.size();i++){
    ids.insert(msat_term_id(terms[vars[i].exp]));
  }
  return all_vars_included(term, ids);
}
