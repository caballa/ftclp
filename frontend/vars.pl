% Author: J. Gallagher 31/03/2006. (c) Roskilde University.
 
:- module(vars, [vars/2]).

vars(T,Vs) :-
	vars3(T,[],Vs).

vars3(X,Vs,Vs1) :-
	var(X),
	insertvar(X,Vs,Vs1).
vars3(X,Vs,Vs) :-
	atomic(X).
vars3(X,Vs,Vs1) :-
	nonvar(X),
	X =.. [_|Args],
	argvars(Args,Vs,Vs1).
 
argvars([],Q,Q).
argvars([X|Xs],Vs,Vs2) :-
	vars3(X,Vs,Vs1),
	argvars(Xs,Vs1,Vs2).
 
insertvar(X,[],[X]).
insertvar(X,[Y|Vs],[Y|Vs]) :-
	X == Y.
insertvar(X,[Y|Vs],[Y|Vs1]) :-
	X \== Y,
	insertvar(X,Vs,Vs1).

