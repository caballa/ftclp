/* Author: Jorge. A Navas, The University of Melbourne 2012-2013 */

#include "CiaoTrie.h"
#include <stdlib.h> // for malloc
#include <set>
#include <iostream>

using namespace Ciao;

extern "C" Prolog_foreign_return_type
trie_make_c(Prolog_term_ref ref_trie) {
  try {
    cb_tree_t * t = (cb_tree_t *) malloc(sizeof(cb_tree_t));
    *t = cb_tree_make();
    Prolog_term_ref term = Prolog_new_term_ref(); 
    Prolog_put_address(term, t);
    return Prolog_unify(ref_trie, term) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (const std::exception &e){
    Prolog_term_ref et = Prolog_new_term_ref();
    Prolog_put_atom_chars(et, e.what());
    Prolog_raise_exception(et);
  }
  return PROLOG_FAILURE;
}

cb_tree_t * term_ref_to_tree(Prolog_term_ref ref_trie){
  void *p; 
  if (!Prolog_get_address(ref_trie, &p))
    return 0;
  cb_tree_t * tree = (cb_tree_t *) p;
  return tree;  
}

extern "C" Prolog_foreign_return_type
trie_clear_c(Prolog_term_ref ref_trie) {
  try {
    cb_tree_t * tree = term_ref_to_tree(ref_trie);
    if (!tree)
      return PROLOG_FAILURE;

    cb_tree_clear(tree);
    return PROLOG_SUCCESS;
  }
  catch (const std::exception &e){
    Prolog_term_ref et = Prolog_new_term_ref();
    Prolog_put_atom_chars(et, e.what());
    Prolog_raise_exception(et);
  }
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
trie_insert_c(Prolog_term_ref ref_trie, const char * str) {
  try {
    cb_tree_t * tree = term_ref_to_tree(ref_trie);
    if (!tree)
      return PROLOG_FAILURE;
    if (cb_tree_insert(tree, str) != 0) {
      fprintf(stderr, "Insertion failed with %s \n", str);
      return PROLOG_FAILURE;
    }
    return PROLOG_SUCCESS;
  }
  catch (const std::exception &e){
    Prolog_term_ref et = Prolog_new_term_ref();
    Prolog_put_atom_chars(et, e.what());
    Prolog_raise_exception(et);
  }
  return PROLOG_FAILURE;
}

static int collect_strings_cb(const char *s, void *x) {
  std::set<std::string> * elements = (std::set<std::string> *) x;
  std::string str(s);
  elements->insert(str);
  return 0;
}

extern "C" Prolog_foreign_return_type
trie_print_c(Prolog_term_ref ref_trie, const char * p) {
  try {
    cb_tree_t * tree = term_ref_to_tree(ref_trie);
    if (!tree)
      return PROLOG_FAILURE;

    std::set<std::string> strings;
    if (cb_tree_walk_prefixed(tree, p, collect_strings_cb, &strings) != 0) {
      std::cout << "Trie = empty" << std::endl;
      return PROLOG_SUCCESS;
    }

    std::cout << "Trie = {";
    for(std::set<std::string>::iterator I = strings.begin() , E = strings.end(); I!=E; ++I){
      std::string x = *I;
      std::cout << x << ";";
    }
    std::cout << "}" << std::endl;
    return PROLOG_SUCCESS;
  }
  catch (const std::exception &e){
    Prolog_term_ref et = Prolog_new_term_ref();
    Prolog_put_atom_chars(et, e.what());
    Prolog_raise_exception(et);
  }
  return PROLOG_FAILURE;

}

extern "C" Prolog_foreign_return_type 
trie_allprefixed_c(Prolog_term_ref ref_trie, const char * prefix, Prolog_term_ref allprefixed){
  try {
    cb_tree_t * tree = term_ref_to_tree(ref_trie);
    if (!tree)
      return PROLOG_FAILURE;

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_construct_empty_cons(tmp);

    std::set<std::string> strings;
    if (cb_tree_walk_prefixed(tree, prefix, collect_strings_cb, &strings) != 0) {
      return Prolog_unify(allprefixed, tmp) ? PROLOG_SUCCESS : PROLOG_FAILURE;
    }
    for(std::set<std::string>::iterator I = strings.begin() , E = strings.end(); I!=E; ++I){
      std::string x = *I;
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_put_atom_chars(t, x.c_str());
      Prolog_construct_cons(tmp, t, tmp);
    }
    return Prolog_unify(allprefixed, tmp) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (const std::exception &e){
    Prolog_term_ref et = Prolog_new_term_ref();
    Prolog_put_atom_chars(et, e.what());
    Prolog_raise_exception(et);
  }
  return PROLOG_FAILURE;  
}

extern "C" Prolog_foreign_return_type 
trie_allsuffixed_c(Prolog_term_ref ref_trie, const char * prefix, Prolog_term_ref allsuffixed){
  try {
    cb_tree_t * tree = term_ref_to_tree(ref_trie);
    if (!tree)
      return PROLOG_FAILURE;

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_construct_empty_cons(tmp);

    std::set<std::string> strings;
    if (cb_tree_walk_prefixed(tree, prefix, collect_strings_cb, &strings) != 0) {
      return Prolog_unify(allsuffixed, tmp) ? PROLOG_SUCCESS : PROLOG_FAILURE;
    }
    std::string p(prefix);
    size_t n = p.size();
    for(std::set<std::string>::iterator I = strings.begin() , E = strings.end(); I!=E; ++I){
      std::string x = *I;
      std::string suffix = x.substr(n+1);
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_put_atom_chars(t, suffix.c_str());
      Prolog_construct_cons(tmp, t, tmp);
    }
    return Prolog_unify(allsuffixed, tmp) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  catch (const std::exception &e){
    Prolog_term_ref et = Prolog_new_term_ref();
    Prolog_put_atom_chars(et, e.what());
    Prolog_raise_exception(et);
  }
  return PROLOG_FAILURE;  
}

