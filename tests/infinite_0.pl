:- use_package(clpq).

% x=0; 
% while(*){ 
%  if(*) x= x+3; else x=x+1;
% }
% if (x<0) error();

% The model of this program is empty which means that the program is
% safe. 

prove :- 
	clp_meta([ X .=. 0 ]), 
	l(X,X1),
	err(X1).

:- tabled(l(num,num)).

l(X,X1):- clp_meta([X1 .=. X]).
l(X,X2):- 
	clp_meta([X1 .=. X+3]), 
	l(X1,X2).
l(X,X2):- 
	clp_meta([X1 .=. X+1]), 
	l(X1,X2).

:- tabled(err(num)).
err(X):- clp_meta([X .<. 0  ]).
