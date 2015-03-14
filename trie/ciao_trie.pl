%%============================================================================
%% Ciao interface to a crit-bit implementation for strings 
%% Author: Jorge. A Navas, The University of Melbourne 2012-2013
%%============================================================================

:- module(ciao_trie,
	[ 
	  trie_make/1,
	  trie_clear/1,
	  trie_insert/2,
	  trie_allprefixed/3,
	  trie_allsuffixed/3,
	  trie_print/2	
	],
	[assertions, foreign_interface]).

:- extra_linker_opts(['-L${FTCLP_INSTALL}/lib']).
:- use_foreign_library(['ciao_trie', 'stdc++']).

:- true pred trie_make_2(out(_), go(Success)) 
	:: any_term * int + (returns(Success), foreign(trie_make_c)).
trie_make(TrieRef) :- 
	trie_make_2(TrieRef, 1).

:- true pred trie_clear_2(in(_), go(Success)) 
	:: any_term * int + (returns(Success), foreign(trie_clear_c)).
trie_clear(TrieRef) :- 
	trie_clear_2(TrieRef, 1).

% Insert string in the trie
:- true pred trie_insert_3(in(_), in(_), go(Success)) 
	:: any_term * string * int + (returns(Success),foreign(trie_insert_c)).
trie_insert(TrieRef, Str):- 
	trie_insert_3(TrieRef, Str, 1).

:- true pred trie_print_3(in(_), in(_), go(Success)) 
	:: any_term * string * int + (returns(Success),foreign(trie_print_c)).
trie_print(TrieRef, Prefix):- 
	trie_print_3(TrieRef, Prefix, 1).

% Return all strings (list of ascii codes) that match Prefix
:- true pred trie_allprefixed_4(in(_), in(_), out(_), go(Success)) 
	:: any_term * string * any_term * int + 
             (returns(Success), foreign(trie_allprefixed_c)).
trie_allprefixed(TrieRef, Prefix, Strings):- 
	trie_allprefixed_4(TrieRef, Prefix, AtomList, 1),
	convert_atm_list_to_ascii_list(AtomList,Strings).
	
% Return all suffixes (list of ascii codes) that match Prefix
:- true pred trie_allsuffixed_4(in(_), in(_), out(_), go(Success)) 
	:: any_term * string * any_term * int + 
             (returns(Success),foreign(trie_allsuffixed_c)).
trie_allsuffixed(TrieRef, Prefix, Strings):- 
	trie_allsuffixed_4(TrieRef, Prefix, AtomList, 1),
	convert_atm_list_to_ascii_list(AtomList,Strings).

convert_atm_list_to_ascii_list([],[]).
convert_atm_list_to_ascii_list([A|As],[B|Bs]):-
	atom_codes(A,B),
	convert_atm_list_to_ascii_list(As,Bs).
