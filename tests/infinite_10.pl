% SAFE
% The number of invariant checks must be 5
:- use_package(clpq).

prove :- 
	clp_meta([ X .=. 0]), 
	l(X).

:- tabled(l(num)).
l(X):- body(X,X1), l(X1).
l(X):- err(X).

:- tabled(body(num,num)).
body(X,X1):- clp_meta([ X1 .=. X + 1]).
body(X,X1):- clp_meta([ X1 .=. X + 2]).
body(X,X1):- clp_meta([ X1 .=. X + 3]).
body(X,X1):- clp_meta([ X1 .=. X + 4]).
body(X,X1):- clp_meta([ X1 .=. X + 5]).

:- tabled(err(num)).
err(X):- clp_meta([X .<. 0  ]).

	
