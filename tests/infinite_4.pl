:- use_package(clpq).

% From the "Path-Invariants" paper. The model of this program is empty
% which means that the program is safe.
%
% main(){
%   int i, n;
%   int a, b;
%   i=0; 
%   a=0;
%   b=0;
%   while (i < n){
%     if (*){     	
%       a=a+1;
%       b=b+2;
%       }
%     else{
%       a=a+2;
%       b=b+1;
%     }
%     i++;
%   }
%   if (a+b != 3*n) error;
% }

% IMPORTANT: the model is empty if we reason about integer arithmetic.
% We reason by default using reals. For reasoning about integer we
% need to change the code. Search for "HERE TO CHANGE TO int or real."
% in tclp.pl. We need the invariant A + B <= 3 * I

entry_goal :- 
	clp_meta([ N .>. 0, I .=. 0, A .=. 0, B .=. 0]), 
	l(I,A,B,N,K).

:- tabled(l(num,num,num,num,num)).
l(I,A,B,N,K):- 
	clp_meta([ K .>=. 0, I .<. N]),
	l_body(A,B,A1,B1),
	clp_meta([ I1 .=. I+1, K1 .=. K - 1]), 
	l(I1,A1,B1,N,K1).
l(I,A,B,N,_K):- 
	clp_meta([I .>=.N]), error_c(A,B,N).

:- tabled(l_body(num,num,num,num)).
l_body(A0,B0,A1,B1):-
	clp_meta([A1 .=. A0 + 1, B1 .=. B0 + 2]).
l_body(A0,B0,A1,B1):-
	clp_meta([A1 .=. A0 + 2, B1 .=. B0 + 1]).

:- tabled(error_c(num,num,num)).
error_c(A,B,N):- clp_meta([ A + B .<>. 3 * N]).

% FIXME: this causes an infinite loop 
% error_c(A,B,N):- clp_meta([ Z .=. A + B, Z .>. 3 * N]).
% error_c(A,B,N):- clp_meta([ Z .=. A + B, Z .<. 3 * N]).

