% Author: Jorge. A Navas, The University of Melbourne 2013

:- module(writeprog, [ writeprog/3 ]).

%  Ciao libraries
:- use_module(library(write)).

% writeprog(+Clauses,+Directives,+Stream)
writeprog(Clauses,Directives,Stream):-
	writeDirectives(Directives,Stream),
	writeClauses(Clauses,Stream).

% writeDirectives(+Dirs,+Stream)
writeDirectives([],_).
writeDirectives([Dir|Dirs],S):-
	write(S,':- '),
	write(S, Dir),
	write(S,'.'),
	nl(S),
	writeDirectives(Dirs,S).

% writeClauses(+Clauses,+Stream)
writeClauses([],_).
writeClauses([predicates(_)|Cls],S) :-
	writeClauses(Cls,S).
writeClauses([clause((A :- B), _Ws)|BCls],Stream) :-
	%applyNames(Ws),
	writeq(Stream,A),
	write(Stream,' :-'),
	nl(Stream),
	writeBody(Stream,B),
	write(Stream,'.'),
	nl(Stream),
	writeClauses(BCls,Stream).
	
writeBody(S,(B,Bs)) :-
	!,
	write(S,'      '),
	writeLiteral(S,B),
	write(S,','),
	nl(S),
	writeBody(S,Bs).
writeBody(S,(B;Bs)) :-
	!,
	write(S,'      '),
	write(S,'('),
	writeLiteral(S,B),
	write(S,';'),
	nl(S),
	writeBody(S,Bs),
	write(S,')').
writeBody(S,(B->Bs)) :-
	!,
	write(S,'      '),
	write(S,'('),
	writeLiteral(S,B),
	write(S,'->'),
	nl(S),
	writeBody(S,Bs),
	write(S,')').
writeBody(S,B) :-
	write(S,'      '),
	writeLiteral(S,B).

writeLiteral(S,L):-
	!,
	writeq(S,L).
	
applyNames([]).
applynames([X=X|Ws]) :-
	applyNames(Ws).
	
writeTerms([],_).
writeTerms([T|Ts],Stream) :-
	writeq(Stream,T),
	write(Stream,'.'),
	nl(Stream),
	writeTerms(Ts,Stream).


