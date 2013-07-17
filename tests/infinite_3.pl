:- use_package(clpq).

%% From one of the  SLAM papers (it's a classic)
% main()
% {
%   int lock, old, new;

%   old=0;
%   lock=0;
%   new=old+1;

%   while (new != old) {
%     lock = 1;
%     old = new;
%     if (*) {
%       lock = 0;
%       new++;
%     }
%   }
%   if (lock==0) error;
% }

% The model of this program is empty which means that the program is
% safe. We need the invariant Old + 1 <= New

entry_goal :- 
	clp_meta([ Lock .=. 0, /*Old .=. 0,*/ New .=. Old + 1]), 
	l(Lock,Old,New,K).

:- tabled(l(num,num,num,num)).
l(Lock,Old,New,K):- 
	clp_meta([ K .>=. 0, New .<>. Old ]), 
	l_body(Lock,Old,New,Lock1,Old1,New1),
	clp_meta([ K1 .=. K - 1]), 
	l(Lock1,Old1,New1,K1).
l(Lock,Old,New,_K):- 
	clp_meta([ New .=. Old]), 
	error_c(Lock).

:- tabled(l_body(num,num,num,num,num,num)).
l_body(_Lock,Old,New,Lock1,Old1,New1):-
	clp_meta([ Lock1 .=. 0, New1 .=. New + 1, Old1 .=. Old ]).
l_body(Lock,Old,New,Lock1,Old1,New1):-
	clp_meta([ Lock1 .=. Lock, New1 .=. New, Old1 .=. Old ]).
	
:- tabled(error_c(num)).
error_c(Lock):- clp_meta([Lock .=. 0]).

