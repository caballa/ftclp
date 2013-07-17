:- use_package(clpq).

% This program is from http://map.uniroma2.it/smc/
% It is called rel

/*
int x=0;
int y=0;
int n;

INIT: n>=0

while (x < n) {
   x = x + 1;
   y = y + 1;
}

while (x > 0) {
   x = x - 1;
   y = y - 1;
}

if (y > x)
   ERROR:;
*/


entry_goal :- 
	clp_meta([ 
		   X .=. 0, Y .=. 0, 
		   N .>=. 0
		  ]),
	l1(X,Y,N,X1,Y1,K1),
	l2(X1,Y1,N,X2,Y2,K2),
	error_c(X2,Y2).

:- tabled(l1(num,num,num,num,num,num)).
l1(X,Y,N,X2,Y2,K):- 
	clp_meta([ 
		   K .>=. 0, 
		   X .<. N,
		   X1 .=. X + 1,
		   Y1 .=. Y + 1,
		   K1 .=. K - 1
		 ]), 
	l1(X1,Y1,N,X2,Y2,K1).
l1(X,Y,N,X1,Y1,K):-
	clp_meta([X1 .=. X, Y1 .=. Y]).

:- tabled(l2(num,num,num,num,num,num)).
l2(X,Y,N,X2,Y2,K):- 
	clp_meta([ 
		   K .>=. 0, 
		   X .>. 0,
		   X1 .=. X - 1,
		   Y1 .=. Y - 1,
		   K1 .=. K - 1
		 ]), 
	l2(X1,Y1,N,X2,Y2,K1).
l2(X,Y,N,X1,Y1,K):-
	clp_meta([X1 .=. X, Y1 .=. Y]).

:- tabled(error_c(num,num)).
error_c(X,Y):- clp_meta([Y .>. X]).
