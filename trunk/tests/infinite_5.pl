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
%   _TRACER_abort(x > 1);
%   return;
% }

entry_goal :- 
	p(X,I,N,X1,I1,N1), 
	l(X1,I1,N1,K).

:- tabled(p(num,num,num,num,num,num)).
p(_X,I,N,X1,I1,N1):- clp_meta([ X1 .=. 1, I1 .=. I, N1 .=. N]).
p(_X,I,N,X1,I1,N1):- clp_meta([ X1 .=. 2, I1 .=. I, N1 .=. N]).

:- tabled(l(num,num,num,num)).
l(X,I,N,K):- 
	clp_meta([ K .>=. 0, I .<. N, I1 .=. I+1, K1 .=. K - 1]), 
	l(X,I1,N,K1).
l(X,I,N,K):- 
	clp_meta([I .>=.N]), error_c(X).

:- tabled(error_c(num)).
error_c(X):- clp_meta([X .>. 1]).



