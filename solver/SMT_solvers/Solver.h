#ifndef __SOLVER_H__
#define __SOLVER_H__

////////////////////////////////////////////////////////////////////////////
/// \file   Solver.h
///         Generic interface for external solvers.
////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <sstream>
#include <cassert>

using namespace std;

namespace solver{

typedef unsigned int ENV_KIND; // solver enviroment id

typedef int _Exp;
enum Op  { OP_INT, OP_REAL, 
           OP_NOT, OP_AND, OP_OR, OP_IFF, OP_XOR, 
           OP_LEQ, OP_LT, OP_GEQ, OP_GT, OP_EQ, 
           OP_PLUS, OP_TIMES, OP_MINUS, OP_DIV};

class Solver;
class Exp {
 public:
  Exp(Solver* _env, _Exp _exp)
      : env( _env ), exp( _exp )
  {  }

  Exp(void)
      : env( NULL ), exp( -1 )
  {  }  

  Solver* env;
  _Exp exp;
};

typedef vector<int> SeqIntpGroups;
typedef list<Exp>        SeqIntp;

class Solver{
 protected:
  /// Constructor of the class
  Solver(){}
 public:

  /// Virtual destructor of the class
  virtual ~Solver(){}

  /// Add a choice point.
  virtual void Push(ENV_KIND env=0) = 0;
  /// Delete a choice point.
  virtual void Pop(ENV_KIND env=0) = 0;

  /// Delete a choice point.
  /// Clears the assertion stack of WhichEnv. However, terms created
  /// in WhichEnv are still valid.
  virtual void Reset(ENV_KIND env) = 0;

  /// Add an interpolation cut in the solver.
  virtual int PushIntpCut(ENV_KIND env=0) = 0;
  virtual void SetIntpGroup(int g, ENV_KIND env=0) = 0;

  /// Declare variables
  virtual Exp BoolConst(bool val) = 0;
  virtual Exp MkNumber(int n) = 0;
  virtual Exp DeclareRealVariable() = 0;
  virtual Exp DeclareIntVariable() = 0;
  virtual Exp DeclareRealVariable(int) = 0;
  virtual Exp DeclareIntVariable(int) = 0;
    
  /// Assert a formula into the solver.
  virtual void AssertFormula(const Exp& e, ENV_KIND env=0) = 0;

  /// Succeed if the conjunction of the asserted formulas are
  /// satisfiable.
  virtual bool Check_Sat(ENV_KIND env=0) = 0;

  /// Check if P entails Q. 
  /// Variables of P and Q must be declared first. 
  virtual bool 
  Check_Entailment(const Exp& P, const Exp& Q, bool Pop, ENV_KIND env=0) = 0;

  // Return a single interpolant (A,B) where A and B are formulas. 
  Exp GenerateInterpolant(const Exp &A, const Exp &B, ENV_KIND env) {
    std::vector<Exp> A_seq;
    std::vector<Exp> B_seq;
    A_seq.push_back(A);
    B_seq.push_back(B);
    return GenerateInterpolant(A_seq, B_seq, env);
  }
  /// Generate a single interpolant 
  virtual Exp 
  GenerateInterpolant(const vector<Exp>&A,const vector<Exp>&B,ENV_KIND env)=0;
  /// Generate a single interpolant
  virtual Exp 
  GenerateInterpolant(SeqIntpGroups groups, ENV_KIND env=0) = 0;
  /// Generate a sequence interpolant
  virtual SeqIntp 
  GenerateSeqInterpolant(SeqIntpGroups, SeqIntpGroups, ENV_KIND env=0) = 0;
                         
  /// To rename a formula
  virtual Exp rename(const Exp&, const vector<Exp>&, const vector<Exp>&) =0;

  /// To build formulae
  virtual _Exp apply(Op op, const _Exp& x, const _Exp& y) = 0;
  virtual _Exp apply(Op op, const _Exp& x) = 0;

  virtual bool IsTrue(const Exp&) = 0;
  virtual bool IsFalse(const Exp&) = 0;

  virtual std::string ExpString(const Exp& e) = 0;

  virtual bool all_vars_included(const Exp &formula, const vector<Exp> &vars) = 0;

  virtual void PrintAssertedFormulas(ENV_KIND env=0) = 0;

  // Convenience function for producing strings.
  inline string concat(const char* prefix, int id){
    ostringstream s;
    s << prefix;
    s << id;
    return s.str();
  }
};

////
// Operators used for constructing formulae.
////
inline Exp operator|(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_OR, a.exp, b.exp));
}
inline Exp operator&(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_AND, a.exp, b.exp));
}
inline Exp operator^(const Exp& a, const Exp& b)
{
  return Exp(a.env, a.env->apply(OP_XOR, a.exp, b.exp));
}
inline Exp operator~(const Exp& a)
{
  return Exp(a.env, a.env->apply(OP_NOT, a.exp));
}
inline Exp operator<(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_LT, a.exp, b.exp));
}
inline Exp operator>(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_GT, a.exp, b.exp));
}
inline Exp operator<=(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_LEQ, a.exp, b.exp));
}
inline Exp operator>=(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_GEQ, a.exp, b.exp));
}
inline Exp operator==(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_EQ, a.exp, b.exp));
}
inline Exp operator+(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_PLUS, a.exp, b.exp));
}
inline Exp operator-(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_MINUS, a.exp, b.exp));
}
inline Exp operator*(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_TIMES, a.exp, b.exp));
}
inline Exp operator/(const Exp& a, const Exp& b)
{
  assert(a.env == b.env);
  return Exp(a.env, a.env->apply(OP_DIV, a.exp, b.exp));
}

} //end namespace

#endif  /* __SOLVER_H__ */
