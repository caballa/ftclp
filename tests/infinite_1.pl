:- use_package(clpq).

% From "VERIFYING INFINITE STATE SYSTEMS BY SPECIALIZING CONSTRAINT
% LOGIC PROGRAMS" by F. Fioravanti, A. Pettorossi, M. Proietti.  

% x=1; // also works with assume(x>=1);
% y=0;
% while(*){ x=x+y, y++;}
% if (y>x) error();

% The model of this program is empty which means that the program is
% safe. 

% This example requires to infer that y>=0.  This invariant is pretty
% easy to get with abstract interpreration but with
% interpolation-based methods not.

entry_goal :- 
	clp_meta([ X .=. 1, Y .=.0 ]), l(X,Y,K).

:- tabled(l(num,num,num)).
l(X,Y,K):- 
	clp_meta([K .>=. 0, X1 .=. X+Y, Y1 .=. Y+1, K1 .=. K - 1]), 
	l(X1,Y1,K1).
l(X,Y,_K):- error_c(X,Y).

:- tabled(error_c(num,num)).
error_c(X,Y):- clp_meta([Y .>. X  ]).

	
