/*
main(){
  int i,x;

  i=0;x=0;
  while (i < 1){
    x++;
    if (*) x--;
    i++;
  }
  abort(x !=0);
}
*/

prove_unsafe:-
        clp_meta([X1 .=. 0, I .=. 0]),
        l(I,X1,X2),
        clp_meta([ X2 .<>. 0]).

prove_safe:-
        clp_meta([X1 .=. 0, I .=. 0]),
        l(I,X1,X2),
        clp_meta([ X2 .>. 1]).

:- tabled(l(num,num,num)).
l(I,X1,X2):-
        clp_meta([ I .>=. 1, X2 .=. X1]).
l(I,X1,X4):-
        clp_meta([ I .<. 1, X2 .=. X1 + 1]),
        l_body(X2,X3), 
        clp_meta([ I1 .=. I + 1]),
        l(I1,X3,X4).        

:- tabled(l_body(num,num)).
l_body(X,X1) :- clp_meta([ X1 .=. X]).
l_body(X,X1) :- clp_meta([ X1 .=. X - 1]).


