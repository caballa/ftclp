:- use_package(clpq).

% x=0; 
% while(*){ 
%  if(*) x= x+3; else x=x+1;
% }
% if (x<0) error();

% The model of this program is empty which means that the program is
% safe. 

entry_goal :- 
	clp_meta([ X .=. 0 ]), l(X,K).

:- tabled(l(num,num)).
l(X,K):- 
	clp_meta([ K .>=. 0, X1 .=. X+3, K1 .=. K - 1]), 
	l(X1,K1).
l(X,K):- 
	clp_meta([ K .>=. 0, X1 .=. X+1, K1 .=. K - 1]), 
	l(X1,K1).
l(X,_K):- 
	error_c(X).

:- tabled(error_c(num)).
error_c(X):- clp_meta([X .<. 0  ]).
