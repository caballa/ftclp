/*
int main() {
  int i, sn=0;
  for(i=1; i<=8; i++) {
    if (i<4)
    sn = sn + (2);
  }
  abort((sn != 8*(2)) && (sn != 0)); 	 
}
*/

prove_safe:-
        clp_meta([Sn_1 .=. 0, I .=. 0]),
        l(I,Sn_1,Sn_2),
        clp_meta([ Sn_2 .=. 6]).

prove_unsafe:-
        clp_meta([Sn_1 .=. 0, I .=. 0]),
        l(I,Sn_1,Sn_2),
        clp_meta([ Sn_2 .<>. 0, Sn_2 .<>. 8*2]).

:- tabled(l(num,num,num)).
l(I,Sn_1,Sn_2):-
        clp_meta([ I .>=. 9, Sn_2 .=. Sn_1]).
l(I,Sn_1,Sn_3):-
        clp_meta([ I .=<. 8]),
        l_body(I, Sn_1, Sn_2), 
        clp_meta([ I1 .=. I + 1]),
        l(I1,Sn_2,Sn_3).        

:- tabled(l_body(num,num,num)).
l_body(I,Sn_1,Sn_2) :- clp_meta([ I .<. 4 , Sn_2 .=. Sn_1 + 2]).
l_body(I,Sn_1,Sn_2) :- clp_meta([ I .>=. 4, Sn_2 .=. Sn_1]).

