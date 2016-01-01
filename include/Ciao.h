// Common code to implement interfaces between Ciao and C++
// The code was wrote by R. Bagnara for the interface Ciao with PPL.

#ifndef __CIAO__H
#define __CIAO__H
#include <ciao_prolog.h>
#include <cassert>
#include <sstream>
#include <iostream>
#include <cstring>

namespace Ciao {

  typedef ciao_term Prolog_term_ref;
  typedef const char* Prolog_atom;
  typedef ciao_bool Prolog_foreign_return_type;
  
  const Prolog_foreign_return_type PROLOG_SUCCESS = 1;
  const Prolog_foreign_return_type PROLOG_FAILURE = 0;
  
  /*!
    Return a new term reference.
  */
  inline Prolog_term_ref
    Prolog_new_term_ref() {
    return 0;
  }
  
  /*!
    Make \p t be a reference to the same term referenced by \p u,
    i.e., assign \p u to \p t.
  */
  inline int
    Prolog_put_term(Prolog_term_ref& t, Prolog_term_ref u) {
    t = u;
    return 1;
  }
  
  /*!
    Assign to \p t a Prolog integer with value \p l.
  */
  inline int
    Prolog_put_long(Prolog_term_ref& t, long l) {
    t = ciao_integer(l);
  return 1;
  }
  
  
  /*!
    Assign to \p t an atom whose name is given
    by the null-terminated string \p s.
  */
  inline int
    Prolog_put_atom_chars(Prolog_term_ref& t, const char* s) {
    t = ciao_atom(s);
    return 1;
  }
  
  /*!
    Assign to \p t the Prolog atom \p a.
  */
  inline int
    Prolog_put_atom(Prolog_term_ref& t, Prolog_atom a) {
    t = ciao_atom(a);
  return 1;
  }
  
  /*!
    Assign to \p t a term representing the address contained in \p p.
  */
  inline int
    Prolog_put_address(Prolog_term_ref& t, void* p) {
    t = ciao_pointer_to_address(ciao_implicit_state, p);
    return 1;
  }
  
  /*!
    Return an atom whose name is given by the null-terminated string \p s.
  */
  inline Prolog_atom
    Prolog_atom_from_string(const char* s) {
    return ciao_atom_name(ciao_atom(s));
  }
  
  /*!
    Assign to \p t a compound term whose principal functor is \p f
    of arity 1 with argument \p a1.
  */
  inline int
    Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			      Prolog_term_ref a1) {
    Prolog_term_ref args[1];
    args[0] = a1;
    t = ciao_structure_a(f, 1, args);
    return 1;
  }
  
  /*!
    Assign to \p t a compound term whose principal functor is \p f
    of arity 2 with arguments \p a1 and \p a2.
  */
  inline int
    Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			      Prolog_term_ref a1, Prolog_term_ref a2) {
    Prolog_term_ref args[2];
    args[0] = a1;
    args[1] = a2;
    t = ciao_structure_a(f, 2, args);
    return 1;
  }

  /*!
    Assign to \p t a compound term whose principal functor is \p f
    of arity 3 with arguments \p a1, \p a2 and \p a3.
  */
  inline int
    Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			      Prolog_term_ref a1, Prolog_term_ref a2,
			      Prolog_term_ref a3) {
    Prolog_term_ref args[3];
    args[0] = a1;
    args[1] = a2;
    args[2] = a3;
    t = ciao_structure_a(f, 3, args);
    return 1;
  }
  
  /*!
    Assign to \p t a compound term whose principal functor is \p f
    of arity 4 with arguments \p a1, \p a2, \p a3 and \p a4.
  */
  inline int
    Prolog_construct_compound(Prolog_term_ref& t, Prolog_atom f,
			      Prolog_term_ref a1, Prolog_term_ref a2,
			      Prolog_term_ref a3, Prolog_term_ref a4) {
    Prolog_term_ref args[4];
    args[0] = a1;
    args[1] = a2;
    args[2] = a3;
    args[3] = a4;
    t = ciao_structure_a(f, 4, args);
    return 1;
  }
  
  /*!
    Assign to \p c a Prolog list whose head is \p h and tail is \p t.
  */
  inline int
    Prolog_construct_cons(Prolog_term_ref& c,
			  Prolog_term_ref h, Prolog_term_ref t) {
    c = ciao_list(h, t);
    return 1;
  }
  
  inline int
    Prolog_construct_empty_cons(Prolog_term_ref& c){
    c = ciao_empty_list();
    return 1;
  }
  
  
  /*!
    Raise a Prolog exception with \p t as the exception term.
  */
  inline void
    Prolog_raise_exception(Prolog_term_ref t) {
    ciao_raise_exception(t);
  }
  
  /*!
    Return true if \p t is a Prolog variable, false otherwise.
  */
  inline int
    Prolog_is_variable(Prolog_term_ref t) {
    return ciao_is_variable(t);
  }
  
  /*!
    Return true if \p t is a Prolog atom, false otherwise.
  */
  inline int
    Prolog_is_atom(Prolog_term_ref t) {
    return ciao_is_atom(t);
  }
  
  /*!
    Return true if \p t is a Prolog integer, false otherwise.
  */
  inline int
    Prolog_is_integer(Prolog_term_ref t) {
    return ciao_is_integer(t);
  }
  
  /*!
    Return true if \p t is the representation of an address, false otherwise.
  */
  inline int
    Prolog_is_address(Prolog_term_ref t) {
    return ciao_is_address(ciao_implicit_state, t);
  }
  
  /*!
    Return true if \p t is a Prolog compound term, false otherwise.
  */
  inline int
    Prolog_is_compound(Prolog_term_ref t) {
    return ciao_is_structure(t);
  }
  
  /*!
    Return true if \p t is a Prolog cons (list constructor), false otherwise.
  */
  inline int
    Prolog_is_cons(Prolog_term_ref t) {
    return ciao_is_list(t);
  }
  
  inline int
    Prolog_get_int(Prolog_term_ref t, int *lp){
    assert(ciao_is_integer(t));
    if (ciao_fits_in_int(t)) {
      *lp = ciao_to_integer(t);
      return 1;
    }
    else
      return 0;
  }
  
  /*!
    If \p t is the Prolog representation for a memory address, return
    true and store that address into \p v; return false otherwise.
    The behavior is undefined if \p t is not an address.
  */
  inline int
    Prolog_get_address(Prolog_term_ref t, void** vpp) {
    assert(Prolog_is_address(t));
    *vpp = ciao_address_to_pointer(ciao_implicit_state, t);
    return 1;
  }

  /*!
    If \p t is a Prolog atom, return true and store its name into \p name.
    The behavior is undefined if \p t is not a Prolog atom.
  */
  inline int
    Prolog_get_atom_name(Prolog_term_ref t, Prolog_atom* ap) {
    assert(Prolog_is_atom(t));
    *ap = ciao_atom_name(t);
    return 1;
  }

  /*!
    If \p t is a Prolog compound term, return true and store its name
    and arity into \p name and \p arity, respectively.
    The behavior is undefined if \p t is not a Prolog compound term.
  */
  inline int
    Prolog_get_compound_name_arity(Prolog_term_ref t, Prolog_atom* ap, int* ip) {
    assert(Prolog_is_compound(t));
    *ap = ciao_structure_name(t);
    *ip = ciao_structure_arity(t);
    return 1;
  }

  /*!
    If \p t is a Prolog compound term and \p i is a positive integer
    less than or equal to its arity, return true and assign to \p a the
    i-th (principal) argument of \p t.
    The behavior is undefined if \p t is not a Prolog compound term.
  */
  inline int
    Prolog_get_arg(int i, Prolog_term_ref t, Prolog_term_ref& a) {
    assert(Prolog_is_compound(t));
    a = ciao_structure_arg(t, i);
    return 1;
  }

  /*!
    If \p c is a Prolog cons (list constructor), assign its head and
    tail to \p h and \p t, respectively.
    The behavior is undefined if \p c is not a Prolog cons.
  */
  inline int
    Prolog_get_cons(Prolog_term_ref c, Prolog_term_ref& h, Prolog_term_ref& t) {
    assert(Prolog_is_cons(c));
    h = ciao_list_head(c);
    t = ciao_list_tail(c);
    return 1;
  }

  inline int
    Prolog_empty_cons(Prolog_term_ref c){
    return ciao_is_empty_list(c);
  }

  /*!
    Unify the terms referenced by \p t and \p u and return true
    if the unification is successful; return false otherwise.
  */
  inline int
    Prolog_unify(Prolog_term_ref t, Prolog_term_ref u) {
    return ciao_unify(t, u);
  }

  template <typename T>
    T* term_to_handle(Prolog_term_ref t) {
    if (Prolog_is_address(t)) {
      void* p;
      if (Prolog_get_address(t, &p))
	return static_cast<T*>(p);
    }
    assert(false); // FIXME: capture properly the exception
    return 0;
  }
  
};
#endif
