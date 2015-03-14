#ifndef __MSAT_H__
#define __MSAT_H__
////////////////////////////////////////////////////////////////////////////
/// \file   MSat.h
///         Interface to Mathsat SMT solver
////////////////////////////////////////////////////////////////////////////
#include <map>
#include <sstream>
#include <set>
#include <mathsat.h>
#include <solver/SMT_solvers/util/cache.h>
#include <solver/SMT_solvers/Solver.h>
#include <solver/Common.h>

//#define DEBUG_MATHSAT
//#define PRINT_STATS
//#define EXIT_LEAK

using namespace std;
using namespace Ciao;

namespace solver {

class MSat : public Solver {

  typedef NilC<_Exp>::cache         TermMap;
  typedef NilC<msat_term>::cache RewriteMap;
  typedef NilC<Exp>::cache      MkNumberMap;

 public:
  inline msat_config CreateSolverConfig(string debugFileName){
    msat_config   cfg;
    cfg = msat_create_config();
    ///
    // For debugging of msat
    // msat_set_option(cfg, "debug.api_call_trace", "1");
    // msat_set_option(cfg, "debug.api_call_trace_filename", 
    //                 debugFileName.c_str());
    ///
    msat_set_option(cfg, "random_seed"        , "123456789" );      
    msat_set_option(cfg, "interpolation"      , "true" );
    msat_set_option(cfg, "theory.euf.enabled" , "false" );      
    msat_set_option(cfg, "theory.bv.enabled"  , "false" );      
    msat_set_option(cfg, "theory.fp.enabled"  , "false" );      
    msat_set_option(cfg, "theory.arr.enabled" , "false" );      
    return cfg;
  }
  
  /// Constructor of the class
  MSat(unsigned int num_env): Solver() {

    // create main environment
    string filename("/tmp/env-0.smt2");
    msat_config cfg = CreateSolverConfig(filename);
    msat_env main_env = msat_create_env(cfg);
    if (MSAT_ERROR_ENV(main_env)){
      ostringstream buf;
      buf << "Mathsat error [msat_create_env]: " 
          << msat_last_error_message(main_env);
      throw error(buf.str());
    }
    msat_destroy_config(cfg);
    _env.push_back(main_env);
    _root.push_back(PushIntpCut(0));

    // create sibling environments
    for (unsigned int i=1; i < num_env; i++){
      ostringstream sstream;
      sstream << "/tmp/env-" << i << ".smt2" ;
      msat_config  cfg = CreateSolverConfig(sstream.str());
      msat_env sib_env = msat_create_shared_env(cfg, _env[0]);
      if (MSAT_ERROR_ENV(sib_env)){
        ostringstream buf;
        buf << "Mathsat error [msat_create_env]: " 
            << msat_last_error_message(sib_env);
        throw error(buf.str());
      }
      msat_destroy_config(cfg);
      _env.push_back(sib_env);
      _root.push_back(PushIntpCut(i));
    } // enfor

    // Allocate the type tags
    b_type = msat_get_bool_type(_env[0]);
    i_type = msat_get_integer_type(_env[0]);
    r_type = msat_get_rational_type(_env[0]);

#ifdef DEBUG_MATHSAT
    cout << msat_get_version() << "\n";
    cout << "Mathsat initialized environment successfully.\n";
#endif 
    
  }
  
  /// Virtual destructor of the class
  ~MSat(){
#ifdef PRINT_STATS
    const char * stats = msat_get_search_stats(_env[0]);
    cout << stats << endl;
#endif
#ifndef EXIT_LEAK
    for (int i=_env.size()-1; i >= 0 ; i--){
      Reset(i);
      msat_destroy_env(_env[i]);
    }
#ifdef DEBUG_MATHSAT
    cout << "Mathsat destroyed environment successfully.\n";
#endif 
#else 
    cout << "Warning: leaked Mathsat environment.\n";
#endif
    _env.clear();
    _root.clear();
  }

  /// Add a choice point.
  void Push(ENV_KIND env=0);
  /// Delete a choice point.
  void Pop(ENV_KIND env=0);
  
  /// Reset an enviroment
  void Reset(ENV_KIND env);
  
  /// Add an interpolation cut in the solver.
  int  PushIntpCut(ENV_KIND env=0);
  void SetIntpGroup(int g, ENV_KIND env=0);
    
  // Make a "true" or "false" term
  Exp BoolConst(bool val);
  // Make a number
  Exp MkNumber(int n);

  // Declare an integer variable using an internal id
  inline Exp DeclareIntVariable(){
    int id = ids.size();
    return DeclareIntOrRealVariable(true, id);
  }

  // Declare a real variable using an internal id  
  inline Exp DeclareRealVariable(){
    int id = ids.size();
    return DeclareIntOrRealVariable(false, id);
  }

  // Declare an integer variable using an external id  
  inline Exp DeclareIntVariable(int id){
    return DeclareIntOrRealVariable(true, id); 
  }
  
  // Declare a real variable using an external id
  inline Exp DeclareRealVariable(int id){
    return DeclareIntOrRealVariable(false, id);
  }

  // Return true if "true" term
  bool IsTrue(const Exp& e);
  // Return false if "false" term
  bool IsFalse(const Exp& e);
  
  // Assert a formula into the solver.
  void AssertFormula(const Exp& e, ENV_KIND env=0) ;
  
  //  Return true if the current state of the solver is satisfiable
  bool Check_Sat(ENV_KIND env=0);
  
  inline string ExpString(const Exp& e){
    // To print in internal Mathsat format
    // char* c = msat_term_repr(terms[e.exp]);
    // string ret(c);
    // msat_free(c);
    // return ret;
    // To print in SMTLIB2 format
    ostringstream s;
    s << msat_to_smtlib2_term(_env[0], terms[e.exp]);
    return s.str();
  }
  
  // Check if P entails Q. 
  // Pre: variables of P and Q must be declared first.
  bool Check_Entailment(const Exp& P, const Exp& Q, bool Pop, ENV_KIND env=0);

  /// Interpolations methods:
  // Generate a single interpolant from two formulas
  Exp GenerateInterpolant(const vector<Exp> &A, const vector<Exp> &B, ENV_KIND env);
  // Generate a single interpolant from interpolation groups.
  Exp GenerateInterpolant(SeqIntpGroups groups, ENV_KIND env=0);
  // Generate a sequence interpolant from some selected groups.
  SeqIntp GenerateSeqInterpolant(SeqIntpGroups groups, SeqIntpGroups sel_groups, ENV_KIND env=0); 

  /// 
  bool all_vars_included(const Exp &formula, const vector<Exp> &vars);
  bool all_vars_included(msat_term term, const set<size_t> &ids);

  /// To rename formulas
  RewriteMap make_rename_map(const vector<Exp>&, const vector<Exp>&);
  Exp rename(const Exp&, const vector<Exp>&, const vector<Exp>&);
  msat_term rename(msat_term, const RewriteMap);
  
  /// To build formulae
  _Exp apply(Op op, const _Exp& x, const _Exp& y);
  _Exp apply(Op op, const _Exp& x);
  
  inline _Exp term_to_bexp(const msat_term& term){
    TermMap::iterator it(term_map.find(msat_term_id(term)));
    if(it != term_map.end())
      return (*it).second;
    else {
      int t_id = terms.size();
      terms.push_back(term);
      term_map[msat_term_id(term)] = t_id;
      return t_id;
    }
  }

  /// debugging
  void PrintAssertedFormulas(ENV_KIND env=0);
  
 private:

  // all variables must be declare in _env[0], the main environment so
  // all the sibling can share those declarations.
  inline Exp DeclareIntOrRealVariable(bool IsInt, int id){
    ids.push_back(concat("v_",id));
    msat_decl d ;
    if (IsInt)
      d = msat_declare_function(_env[0], ids.back().c_str(), i_type);
    else
      d = msat_declare_function(_env[0], ids.back().c_str(), r_type);
    
    if (MSAT_ERROR_DECL(d)){
      throw error("[declare_var]" + string(msat_last_error_message(_env[0])));
    }
    
    msat_term term = msat_make_constant(_env[0], d);
    if (MSAT_ERROR_TERM(term)){
      throw error("[make_term]" + string(msat_last_error_message(_env[0])));
    }
    
    return Exp((Solver*) this, term_to_bexp(term));
  }
  
  // We allow an arbitrary number of environments. env[0] is the main
  // environment and the rest are sibling environments.  This mean
  // that all variables and terms should be created in env[0] so all
  // sibling share them.
  vector<msat_env> _env;
  vector<int> _root;
  
  msat_type     b_type; // Boolean type
  msat_type     i_type; // Integer type
  msat_type     r_type; // Real type
  
  TermMap term_map;
  MkNumberMap mknumber_map;
  vector<string>      ids;
  vector<msat_term> terms;

}; //end class

}//end namespace solver

#endif /* __MSAT_H__ */
