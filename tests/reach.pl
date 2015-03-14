:- use_package(clpq).

entry_goal(C):- 
	clp_meta([C0 .=. 0, R0  .=. 0]), 
	r(a,z,C0,R0,C,R,Path),
	clp_meta([R .=<. 10]).


:- tabled(e(_,_,num,num)).
:- no_cache(e(_,_,_,_)).
e(a,b,C,R):- clp_meta([C .=. 3, R .=. 2]).
e(a,d,C,R):- clp_meta([C .=. 2, R .=. 5]).
e(a,c,C,R):- clp_meta([C .=. 5, R .=. 3]).

e(b,d,C,R):- clp_meta([C .=. 2, R .=. 2]).
e(c,d,C,R):- clp_meta([C .=. 1, R .=. 4]).

e(d,z,C,R):- clp_meta([C .=. 1, R .=. 7]).
e(d,e,C,R):- clp_meta([C .=. 1, R .=. 2]).
e(d,f,C,R):- clp_meta([C .=. 1, R .=. 1]).
e(d,g,C,R):- clp_meta([C .=. 3, R .=. 2]).

e(e,z,C,R):- clp_meta([C .=. 1, R .=. 4]).
e(f,z,C,R):- clp_meta([C .=. 2, R .=. 3]).
e(g,z,C,R):- clp_meta([C .=. 1, R .=. 1]).

:- tabled(r(_,_,num,num,num,num,_)).
r(X,Y,C,R,C2,R2,[(X-Z)|Path]) :-
	e(X,Z,C1,R1),
	clp_meta([C0 .=. C1 + C, R0 .=. R1 + R]),
	r(Z,Y,C0,R0,C2,R2,Path).
r(X,Y,C,R,C0,R0,[(X-Y)]) :-
	clp_meta([C0 .=. C + C1, R0 .=. R + R1]),
	e(X,Y,C1,R1).

/*

?- entry_goal([C,R,Path]).

C = 7,
Path = [a-b,b-d,d-e,e-z],
R = 10 ? ;

C = 8,
Path = [a-b,b-d,d-f,f-z],
R = 8 ? ;

C = 9,
Path = [a-b,b-d,d-g,g-z],
R = 7 ? ;

C = 5,
Path = [a-d,d-f,f-z],
R = 9 ? ;

C = 6,
Path = [a-d,d-g,g-z],
R = 8 ? ;

C = 10,
Path = [a-c,c-d,d-g,g-z],
R = 10 ? ;

*/

	

