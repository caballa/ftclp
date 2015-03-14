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

prove :- 
	clp_meta([ Lock .=. 0, Old .=. 0, New .=. Old + 1]), 
	l(Lock,Old,New).

:- tabled(l(num,num,num)).
l(Lock,Old,New):- 
	clp_meta([ New .<>. Old ]), 
	clp_meta([ Lock1 .=. 1, 
	           Old1  .=. New, 
		 New1  .=. New]),
	l_body(Lock1, Old1, New1, Lock2, Old2, New2),
	l(Lock2,Old2,New2).
l(Lock,Old,New):- 
	clp_meta([ New .=. Old]), 
	err(Lock).

:- tabled(l_body(num,num,num,num,num,num)).
l_body(_Lock,Old,New,Lock1,Old1,New1):-
	clp_meta([ Lock1 .=. 0, 
	           Old1  .=. Old, 
		 New1  .=. New + 1 ]).
l_body(Lock,Old,New,Lock,Old,New).
	
:- tabled(err(num)).
err(Lock):- clp_meta([Lock .=. 0]).

