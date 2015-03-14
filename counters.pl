% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%=======================================================================%
% Define counters
%=======================================================================%

:- module(counters, 
	[ 
	  set_counter/2,
	  get_counter_value/2,
	  incr_counter/2,
	  incr_counter/3,
	  delete_all_counters/0,
	  delete_counter/1
	]).

:- data counter/2.

% set_counter(+atm, +num)
% initialize the counter
set_counter(CounterName, K):-
	asserta_fact(counter(CounterName,K)).	

:- push_prolog_flag(multi_arity_warnings,off).
% incr_counter(+atm, -num)
% Return in OldVal the old value and increment its value by 1
incr_counter(CounterName, OldVal):-
	retract_fact(counter(CounterName,OldVal)),
	!,
	NewVal is OldVal + 1,
	asserta_fact(counter(CounterName,NewVal)).
incr_counter(CounterName, 1):-
	asserta_fact(counter(CounterName,2)).

% incr_counter(+atm, -num, +num)
% Return in OldVal the old value and increment its value by Incr
incr_counter(CounterName, OldVal, Incr):-
	retract_fact(counter(CounterName, OldVal)),
	!,
	NewVal is OldVal + Incr,
	asserta_fact(counter(CounterName,NewVal)).
incr_counter(CounterName, 1, Incr):-
	NewVal is 1+Incr,
	asserta_fact(counter(CounterName,NewVal)).
:- pop_prolog_flag(multi_arity_warnings).
% get_counter_value(+atm, -num):-
get_counter_value(CounterName, Val):-
	current_fact(counter(CounterName, Val)),
	!.
get_counter_value(_, 1):- !.

% delete_all_counters
delete_all_counters:-
	retractall_fact(counter(_,_)).

% delete_counter(+atm)
delete_counter(Name):-
	retractall_fact(counter(Name,_)).


