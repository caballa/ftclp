% TRIVIALLY SAFE (x is not changed)

prove:-
        clp_meta([X .=. 0, I .=. 0, N .>. 0]),
        l(I,N,X).

:- tabled(l(num,num,num)).
l(I,N,X):-
        clp_meta([ I .<. N, X .>. 0]).
l(I,N,X):-
        clp_meta([ I .<. N, X .=<. 0, I1 .=. I + 1]),
        l(I1,N,X).        
l(I,N,X):-
        clp_meta([ I .<. N, X .=<. 0, I1 .=. I + 1]),
        l(I1,N,X).        


