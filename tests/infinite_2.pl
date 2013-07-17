:- use_package(clpq).

% From "A Practical and Complete Approach to Predicate Refinement" by
% Jhala and McMillan:
%
% x=i; y=j;
% while (x!=0){
%   x--;
%   y--;
% }
% if (i==j)
%    assert(y<=0);

% The model of this program is empty which means that the program is
% safe. 

% We need the SAFE INDUCTIVE INVARIANT y + i <= x + j

entry_goal :- 
	clp_meta([ X .=. I, Y .=.J ]), l(X,Y,I,J,K).

:- tabled(l(num,num,num,num,num)).
l(X,Y,I,J,K):- 
	clp_meta([ X .<>.0,  K .>=. 0, 
	           X1 .=. X-1, Y1 .=. Y-1, 
		   K1 .=. K - 1]), 
	l(X1,Y1,I,J,K1).
l(X,Y,I,J,K):- 
	clp_meta([X .=. 0]), 
	error_c(X,Y,I,J).

:- tabled(error_c(num,num,num,num)).
error_c(_X,Y,I,J):- clp_meta([I .=. J, Y .>. 0]).



	
