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

prove :- 
	clp_meta([ X .=. I, Y .=.J ]), 
	l(X,Y,I,J).

:- tabled(l(num,num,num,num)).
l(X,Y,I,J):- 
	clp_meta([ X .<>.0,  
	           X1 .=. X-1, Y1 .=. Y-1]),
          l(X1,Y1,I,J).
	
l(X,Y,I,J):- 
	clp_meta([X .=. 0]), 
	err(X,Y,I,J).

:- tabled(err(num,num,num,num)).
err(_X,Y,I,J):- clp_meta([I .=. J, Y .>. 0]).



	
