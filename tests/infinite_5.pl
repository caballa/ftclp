:- use_package(clpq).

% This example to show that the second copy of the recursive clause
% can not be subsumed. There is an answer which means the program is
% unsafe.
% void main(){
%   int NONDET;
%   int i,N;
%   int a;
%   int x;
%   if (NONDET > 0) x=1; else x=2;
%   while (i<N){    
%     i=i+1;
%   }
%   assert (x <=1) ;
%   return;
% }

prove :- 
          clp_meta([ N .>=. 0]),
	p(X,I,N,X1,I1), 
	l(X1,I1,N).

:- tabled(p(num,num,num,num,num)).
p(_X,I,N,X1,I1):- clp_meta([ X1 .=. 1, I1 .=. I]).
p(_X,I,N,X1,I1):- clp_meta([ X1 .=. 2, I1 .=. I]).

:- tabled(l(num,num,num)).
l(X,I,N):- 
	clp_meta([ I .<. N, I1 .=. I+1]), 
	l(X,I1,N).
l(X,I,N):- 
	clp_meta([I .>=.N]), 
	err(X).

:- tabled(err(num)).
err(X):- clp_meta([X .>. 1]).



