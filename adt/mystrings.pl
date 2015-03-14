% Author: Jorge. A Navas, The University of Melbourne 2013

:- module(mystrings, [starts_with/3, split/3]).

%  Ciao libraries
:- use_module(library(lists)).

% set_prolog_flag(write_strings,on).

%-------------------------------------------------------------------------------%
% starts_with(OldString,Pattern,Rest)
%-------------------------------------------------------------------------------%
starts_with(OldString,[],OldString):- 
        !.
starts_with([H|TOldString],[H|T],Rest):- 
        !, 
        starts_with(TOldString,T,Rest).

%-------------------------------------------------------------------------------%
% split(+OldString,+Pattern,-ListStrings)
%-------------------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).

split(OldString,Pattern,ListStrings):- 
        split(OldString,Pattern,[],ListStrings).

% split(+OldString,+Pattern,+PartialStart,-ListStrings).
split([],_Pattern,[],[]):- 
        !.
split([],_Pattern,PartialStart,[PartialStart]):- 
        !.
split(OldString,Pattern,[],[RestStrings]):-
        starts_with(OldString,Pattern,Rest),
        !,
        split(Rest,Pattern,[],RestStrings).
split(OldString,Pattern,PartialStart,[PartialStart|RestStrings]):-
        starts_with(OldString,Pattern,Rest),
        !,
        split(Rest,Pattern,[],RestStrings).
split([H|T],Pattern,PartialStart,RestStrings):-
        !,
        append(PartialStart,[H],PartialStartTemp),
        split(T,Pattern,PartialStartTemp,RestStrings).
:- pop_prolog_flag(multi_arity_warnings).	
