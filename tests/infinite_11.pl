% UNSAFE

/*
main(){
  int i,n,x;
  
  x=0;
  i=0;
  while (i<n) {
    if (*){
      _abort(x>0); 
    } 
    else{ 
      x = 1;
    }
    i++;
  }
}
*/

prove:-
        clp_meta([X .=. 0, I .=. 0, N .>. 0]),
        l(I,N,X).

:- tabled(l(num,num,num)).
l(I,N,X):-
        clp_meta([ I .<. N]),
        l_body_1(X,X1), 
        clp_meta([ X1 .>. 0 ]).
l(I,N,X):-
        clp_meta([ I .<. N]),
        l_body_1(X,X1),
        clp_meta([ X1 .=<. 0 ]),
        clp_meta([ I1 .=. I + 1]),
        l(I1,N,X1).        
% If comment this clause then program is correct with invariant X <= 0
l(I,N,X):-
        clp_meta([ I .<. N]),
        l_body_2(X,X1),
        clp_meta([ I1 .=. I + 1]),
        l(I1,N,X1).


:- tabled(l_body_1(num,num)).
l_body_1(X,X1) :- clp_meta([ X1 .=. X]).

:- tabled(l_body_2(num,num)).
l_body_2(_X,X1):- clp_meta([ X1 .=. 1]).

