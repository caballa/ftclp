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
% We reason by default using reals. Run with -integer-arithmetic
% option
%
%  We need the invariant A + B <= 3 * I

prove :- 
	clp_meta([ N .>. 0, I .=. 0, A .=. 0, B .=. 0]), 
	l(I,A,B,N).

:- tabled(l(num,num,num,num)).
l(I,A,B,N):- 
	clp_meta([ I .<. N]),
	l_body(A,B,A1,B1),
	clp_meta([ I1 .=. I+1]), 
	l(I1,A1,B1,N).
l(I,A,B,N):- 
	clp_meta([I .>=.N]), err(A,B,N).

:- tabled(l_body(num,num,num,num)).
l_body(A0,B0,A1,B1):-
	clp_meta([A1 .=. A0 + 1, B1 .=. B0 + 2]).
l_body(A0,B0,A1,B1):-
	clp_meta([A1 .=. A0 + 2, B1 .=. B0 + 1]).

:- tabled(err(num,num,num)).
err(A,B,N):- clp_meta([ A + B .<>. 3 * N]).


