:- use_package(clpq).

% This program is a simplified version of the ssh program commonly
% used in the software verification community.
% void main()
% {
%   int e, s;
%   e=0;
%   s=2;  
%   while (*) {
%     if (s == 2){
%       if (e ==0) e=1;
%       s = 3;
%     }
%     else if (s == 3){
%       if (e ==1) e=2;
%       s=4;
%     }
%     else if (s == 4){
%       if (e == 3) error();
%       s=5;
%     }
%   }
%   return;
% }

% Check that it does not work with K<=1
entry_goal :- 
	clp_meta([ E .=. 0, S .=. 2]), 
	l(E,S,K).

:- tabled(l(num,num,num)).
l(E0,S0,K0):- 
	clp_meta([ K0 .>=. 0]), 
	l_body(E0,S0,E1,S1),
	clp_meta([ K1 .=. K0 - 1]), 
	l(E1,S1,K1).
l(E,S,_K):-
	error_c(E,S).

:- tabled(error_c(num,num)).
% No answer
error_c(E,S):- clp_meta([E  .=. 3, S .=. 4]).
% With this there is an answer
%error_c(E,S):- clp_meta([E  .=. 2, S .=. 4]).

	
:- tabled(l_body(num,num,num,num)).
l_body(E0,S0,E1,S1):-
	clp_meta([ S0 .=. 2]),
	l_body_1(E0,S0,E1,S1).
l_body(E0,S0,E1,S1):-
	clp_meta([ S0 .=. 3]),
	l_body_2(E0,S0,E1,S1).
l_body(E0,S0,E1,S1):-
	clp_meta([ S0 .=. 4]),
	l_body_3(E0,S0,E1,S1).
l_body(E0,S0,E1,S1):-
	clp_meta([ S0 .>. 4,
	           E1 .=. E0, S1 .=. S0]).

:- tabled(l_body_1(num,num,num,num)).
l_body_1(E0,_S0,E2,S2):-
	l_body_1_1(E0,E1),
	clp_meta([ S2 .=. 3, E2 .=. E1]).

:- tabled(l_body_2(num,num,num,num)).
l_body_2(E0,_S0,E2,S2):-
	l_body_2_1(E0,E1),
	clp_meta([ S2 .=. 4, E2 .=. E1]).

:- tabled(l_body_3(num,num,num,num)).
l_body_3(E0,S0,E1,S1):-
	l_body_3_1(E0,E1,S0,S1).

:- tabled(l_body_1_1(num,num)).
l_body_1_1(E0,E1):- clp_meta([ E0 .=.  0, E1 .=. 1]).
l_body_1_1(E0,E1):- clp_meta([ E0 .<>. 0, E1 .=. E0]).

:- tabled(l_body_2_1(num,num)).
l_body_2_1(E0,E1):- clp_meta([ E0 .=.  1, E1 .=. 2]).
l_body_2_1(E0,E1):- clp_meta([ E0 .<>. 1, E1 .=. E0]).

:- tabled(l_body_3_1(num,num,num,num)).
% l_body_3_1(E0,E1,S0,S1):- 
% 	clp_meta([ S0 .=. 4, E0 .=. 3, 
% 	           E1 .=. E0, S1 .=. S0]).
l_body_3_1(E0,E1,S0,S1):- 
 	clp_meta([ S0 .=. 4, E0 .=. 2, 
 	           E1 .=. E0, S1 .=. S0]).
l_body_3_1(E0,E1,S0,S1):- 
	clp_meta([ S0 .<>.4, 
	           S1 .=. 5, E1 .=. E0]).
% l_body_3_1(E0,E1,S0,S1):- 
% 	clp_meta([ E0 .<>.3, 
% 	           S1 .=. 5, E1 .=. E0]).
l_body_3_1(E0,E1,S0,S1):- 
	clp_meta([ E0 .<>.2, 
	           S1 .=. 5, E1 .=. E0]).








