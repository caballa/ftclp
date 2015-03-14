% Author: J. Gallagher 31/03/2006. (c) Roskilde University.
 
:- module(clauseTermElim, [bodyTermElim/2, clauseTermElim/2]).

%  Own libraries
:- use_module(canonical).
:- use_module(myterms).
:- use_module(vars).
%  Ciao libraries
:- use_module(library(lists)).

% To understand clpr operators
:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

% Here predicates that we do not want to normalize.
skip_normalization(clp_meta/1).
skip_normalization(counter_inst__clp_meta/1).

% Eliminates non-variable terms (except atoms) in clause bodies,
% introducing new predicates instead. E.g., 
%    .    p(..) :- q(...,f(g(X,Y)),...),...
% becomes p(..) :- q(...,U,...),new(U,X,Y),...
%         new(f(g(X,Y)),X,Y) :- true.

%%/* Added by J.A. Navas
clauseTermElim([predicates(Ps)|Cls], [predicates(Qs)|BCls]) :-
	elimClauseTerms(Cls,0,_,NewPs,[],BCls,[]),
	append(Ps,NewPs,Qs).
	
elimClauseTerms([clause((H :- B),Ws)|Cls], K0,K3, Ps0,Ps3,
                [clause((H2 :- B3),Ws1)|BCls],BCls0) :-
	melt(clause((H :- B),Ws),clause((H1 :- B1),Ws1)),	
	transformHead(H1,K0,K1,Ps0,Ps1,BCls0,BCls1,H2,Bs),
	transformBody(B1,K1,K2,Ps1,Ps2,B2,BCls1,BCls2),
	joingoals1(Bs,B2,B3),
	canonical(clause((H2 :- B3),Ws1)),
	elimClauseTerms(Cls,K2,K3,Ps2,Ps3,BCls,BCls2).
elimClauseTerms([],K,K,Ps,Ps,BCls,BCls).

transformHead(H1,K0,K1,Ps0,Ps1,BCls0,BCls1,H2,Bs):-
	transformBody(H1,K0,K1,Ps0,Ps1,(H2,Bs),BCls0,BCls1).
%%*/
	
bodyTermElim([predicates(Ps)|Cls], [predicates(Qs)|BCls]) :-
	elimBodyTerms(Cls,0,_,NewPs,[],BCls,[]),
	append(Ps,NewPs,Qs).
	
elimBodyTerms([clause((H :- B),Ws)|Cls], K0,K2, Ps0,Ps2,
              [clause((H1 :- B2),Ws1)|BCls],BCls0) :-
	melt(clause((H :- B),Ws),clause((H1 :- B1),Ws1)),	
	transformBody(B1,K0,K1,Ps0,Ps1,B2,BCls0,BCls1),
	canonical(clause((H1 :- B2),Ws1)),
	elimBodyTerms(Cls,K1,K2,Ps1,Ps2,BCls,BCls1).
elimBodyTerms([],K,K,Ps,Ps,BCls,BCls).
	
transformBody(true,K,K,Ps,Ps,true,BCls,BCls).
transformBody((B,Bs),K0,K2,Ps0,Ps2,Bs2,BCls0,BCls2) :-
	!,
	transformAtom(B,K0,K1,Ps0,Ps1,B1,BCls0,BCls1),
	transformBody(Bs,K1,K2,Ps1,Ps2,Bs1,BCls1,BCls2),
	joingoals1(B1,Bs1,Bs2).
transformBody(B,K0,K1,Ps0,Ps1,B1,BCls0,BCls2) :-
	transformAtom(B,K0,K1,Ps0,Ps1,B1,BCls0,BCls2).

transformAtom(B,K0,K0,Ps0,Ps0,B,BCls0,BCls0) :-
	functor(B,F,A),	
	skip_normalization(F/A),
	!.
transformAtom(B,K0,K1,Ps0,Ps1,BDs,BCls0,BCls1) :-
	B=..[P|Xs],
	transformArgs(Xs,K0,K1,Ps0,Ps1,Ys,Ds,BCls0,BCls1),
	B1=..[P|Ys],
	joingoals1(B1,Ds,BDs).

transformArgs([],K,K,Ps,Ps,[],true,BCls,BCls).
transformArgs([X|Xs],K0,K1,Ps0,Ps1,[X|Ys],Ds,BCls1,BCls2) :-
%	var(X),
% Added by J.A. Navas
	(var(X) ; atomic(X)),
	!,
	transformArgs(Xs,K0,K1,Ps0,Ps1,Ys,Ds,BCls1,BCls2).
transformArgs([X|Xs],K0,K2,Ps0,Ps2,[U|Ys],(D,Ds),BCls1,BCls2) :-
	newpred(K0,K1,X,U,Ps0,Ps1,D,D1),
	transformArgs(Xs,K1,K2,Ps1,Ps2,Ys,Ds,
                        [clause((D1:-true),[])|BCls1],BCls2).

newpred(K0,K1,X,U,[BK/M|Ps0],Ps0,D,D2) :-
	K1 is K0+1,
	name(K0,KN),
	name(bodyTerm,BN),
	append(BN,KN,BKN),
	name(BK,BKN),
	vars(X,Vs),
	D=..[BK,U|Vs],
	D1=..[BK,X|Vs],
	functor(D,BK,M),
	copyterm(D1,D2),
	canonical(D2).
	
joingoals1(true,Xs,Xs) :-
	!.
joingoals1(Xs,true,Xs) :-
	!.
joingoals1((true,Xs),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((Xs,true),Ys,Zs) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1((X,Xs),Ys,(X,Zs)) :-
	!,
	joingoals1(Xs,Ys,Zs).
joingoals1(X,Xs,(X,Xs)) :-
	X =.. [F|_],
	F \== ','.
