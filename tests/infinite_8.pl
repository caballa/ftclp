:- use_package(clpq).
/*
This program is from http://map.uniroma2.it/smc/
It is called interp 

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

prove :- 
	clp_meta([ X .=. 0 ,Y .=. 0 ]), 
	l(X,Y).


:- tabled(l(num,num)).
l(X,Y):- 
	l_body(X,Y,X1,Y1),
	l(X1,Y1).
l(X,Y):- 
	err(X,Y).

:- tabled(l_body(num,num,num,num)).
l_body(X0,Y0,X1,Y1):-
	clp_meta([X1 .=. X0 + 1, Y1 .=. Y0 + 2]).
l_body(X0,Y0,X1,Y1):-
	clp_meta([X1 .=. X0 + 1, Y1 .=. Y0 + 3]).

:- tabled(err(num,num)).
err(X,Y):- clp_meta([ 3 * X .<. Y]).
