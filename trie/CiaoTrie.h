/* Author: Jorge. A Navas, The University of Melbourne 2012-2013    */

#ifndef __CIAOTRIE__H
#define __CIAOTRIE__H

#include <include/Ciao.h>
#include <trie/critbit/critbit.h>

namespace Ciao {

  extern "C" Prolog_foreign_return_type 
  trie_make_c(Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  trie_clear_c(Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  trie_insert_c(Prolog_term_ref, const char *);
  extern "C" Prolog_foreign_return_type 
  trie_print_c(Prolog_term_ref, const char *);
  extern "C" Prolog_foreign_return_type 
  trie_allprefixed_c(Prolog_term_ref, const char *, Prolog_term_ref);
  extern "C" Prolog_foreign_return_type 
  trie_allsuffixed_c(Prolog_term_ref, const char *, Prolog_term_ref);

};
#endif
