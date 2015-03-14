:- module(mysets, 
	[ mk_set/1, 
	  insert_set/3, 
	  find_set/2, 
	  lookup_set/2, 
	  remove_set/3,
	  set_to_list/2 ]).

% Ciao libraries
:- use_module(library(sets)).

%-----------------------------------------------------------------------%
%                   Generic api for a set
%-----------------------------------------------------------------------%
% We wrap the predicates from the library "sets" in case we decide
% later on a more efficient datastructure with logarithmic complexity.
%-----------------------------------------------------------------------%

mk_set([]).

insert_set(Set,X,NewSet):- 
	ord_union(Set,[X],NewSet).

find_set(Set,X):- 
	ord_member(X, Set).

lookup_set(Set,X):- 
	member(X, Set).

set_to_list(Set, Set).

remove_set(Set,X,NewSet):-
	ord_delete(Set,X,NewSet).