:- use_package(clpq).

% This program is from http://map.uniroma2.it/smc/
% It is called interp 

/*
int main(){

	int x=0;
	int y=0;

	while (__VERIFIER_nondet_int()) {
		if (__VERIFIER_nondet_int()) {
			x = x+1; 
			y = y+2;
		} else if (__VERIFIER_nondet_int()) {
			if (x >= 4) {
			    x = x+1; 
			    y = y+3; 
			}
		} 
	}

    if(3*x < y)
		goto ERROR;
	
	return 0;
ERROR:
	return -1;
}

*/

entry_goal :- 
	clp_meta([ X .=. 0 ,Y .=. 0 ]), 
	l(X,Y,K).


:- tabled(l(num,num,num)).
l(X,Y,K):- 
	clp_meta([ K .>=. 0]),
	l_body(X,Y,X1,Y1),
	clp_meta([ K1 .=. K - 1]), 
	l(X1,Y1,K1).
l(X,Y,_K):- 
	error_c(X,Y).

:- tabled(l_body(num,num,num,num)).
l_body(X0,Y0,X1,Y1):-
	clp_meta([X1 .=. X0 + 1, Y1 .=. Y0 + 2]).
l_body(X0,Y0,X1,Y1):-
	clp_meta([X1 .=. X0 + 1, Y1 .=. Y0 + 3]).

:- tabled(error_c(num,num)).
error_c(X,Y):- clp_meta([ 3 * X .<. Y]).
