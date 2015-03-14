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

prove :- 
	clp_meta([ X .=. 1, Y .=.0 ]), 
	l(X,Y).

:- tabled(l(num,num)).
l(X,Y):- 
	clp_meta([X1 .=. X+Y, Y1 .=. Y+1]), 
	l(X1,Y1).
l(X,Y):- err(X,Y).

:- tabled(err(num,num)).
err(X,Y):- clp_meta([Y .>. X  ]).

	
