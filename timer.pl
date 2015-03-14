% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%======================================================================%
%                To define timers for profiling 
%======================================================================%

:- module(timer, 
	[
	    clear_timers/0,
	    mk_timer/1,
	    start_timer/1,
	    stop_timer/2,
	    get_timer/2
	]).

%  System libraries
:- use_module(library(prolog_sys)).  

clear_timers:-
	retractall_fact('$time'(_)),
	retractall_fact('$timer'(_,_)),
	retractall_fact('$acc_time'(_,_)).

:- data '$time'/1.
start_time :-
	statistics(runtime,[T1,_]),
	asserta_fact('$time'(T1)).

end_time(Time) :-	
	statistics(runtime,[T1,_]),
	retract_fact('$time'(T0)),
	Time is T1 - T0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FOR PROFILING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data '$timer'/2.
:- data '$acc_time'/2.
mk_timer(Name):-
	asserta_fact('$timer'(Name, 0)).
delete_all_timers:-
	retractall_fact('$timer'(_,_)).

start_timer(Name):-
	current_fact('$acc_time'(Name,_)),
	!,
	format("ERROR ftclp: timer ~q was not stopped before.\n",[Name]),
	format("This is an indication that something unexpectedly failed.\n",[]),
	halt.
start_timer(Name):-
	statistics(runtime,[T1,_]),	
	asserta_fact('$acc_time'(Name,T1)).

stop_timer(Name, Delta):-
	statistics(runtime,[T1,_]),	
	( retract_fact('$acc_time'(Name,T0)) -> 
            true
	; 
            format("ERROR ftclp: stop_timer/2 ~q without calling first start_timer/1\n",
                   [Name]), 
            halt
          ),
	Delta is T1 - T0,
	( retract_fact('$timer'(Name,AccT0)) -> true
	; format("ERROR ftclp: mk_timer/1 must be called first\n",[]), halt),
	AccT1 is AccT0 + Delta,
	asserta_fact('$timer'(Name,AccT1)).

% Return the value for the timer Name in miliseconds!
get_timer(Name,T):-
	current_fact('$timer'(Name,T)).
	


