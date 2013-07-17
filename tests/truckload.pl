 %% - truckload(I,W,D,T): truckload problem where I is the number of packets, 
 %%   W is the load, D is the final city and T is the time restrictions.

% Usage: truckload(60,50,chicago,T)
% By incrementing 2nd argument the search space is larger.

:- tabled(truckload(_,num,_,num)).

truckload(0,0,_,_). 
truckload(I,W,D,T) :-
	I > 0,
	I1 is I - 1,
        truckload(I1,W,D,T).
truckload(I,W,D,T) :-
	I > 0,
        pack(I,Wi,D,T),
	W1 is W - Wi,
	W1 >= 0,
	I1 is I - 1,
        truckload(I1,W1,D,T).

:- tabled(pack(num,num,_,num)).

pack(60,29,chicago,T) :- clp_meta([ T .>=. 19, T .=<. 30]).
pack(59,82,chicago,T) :- clp_meta([ T .>=. 20, T .=<. 30]).
pack(58,24,chicago,T) :- clp_meta([ T .>=. 8,  T .=<. 13]).
pack(57,11,chicago,T) :- clp_meta([ T .>=. 21, T .=<. 23]).
pack(56,57,chicago,T) :- clp_meta([ T .>=. 8,  T .=<. 29]).
pack(55,30,chicago,T) :- clp_meta([ T .>=. 14, T .=<. 19]).
pack(54,71,chicago,T) :- clp_meta([ T .>=. 11, T .=<. 15]).
pack(53,31,chicago,T) :- clp_meta([ T .>=. 10, T .=<. 26]).
pack(52,34,chicago,T) :- clp_meta([ T .>=. 19, T .=<. 21]).
pack(51,73,chicago,T) :- clp_meta([ T .>=. 0,  T .=<. 20]).
pack(50,83,chicago,T) :- clp_meta([ T .>=. 21, T .=<. 25]).
pack(49,86,chicago,T) :- clp_meta([ T .>=. 10, T .=<. 13]).
pack(48,79,chicago,T) :- clp_meta([ T .>=. 22, T .=<. 25]).
pack(47,50,chicago,T) :- clp_meta([ T .>=. 18, T .=<. 29]).
pack(46,58,chicago,T) :- clp_meta([ T .>=. 13, T .=<. 15]).
pack(45,69,chicago,T) :- clp_meta([ T .>=. 4,  T .=<. 15]).
pack(44,77,chicago,T) :- clp_meta([ T .>=. 22, T .=<. 29]).
pack(43,74,chicago,T) :- clp_meta([ T .>=. 27, T .=<. 31]).
pack(42,65,chicago,T) :- clp_meta([ T .>=. 12, T .=<. 25]).
pack(41,26,chicago,T) :- clp_meta([ T .>=. 12, T .=<. 27]).
pack(40,56,chicago,T) :- clp_meta([ T .>=. 15, T .=<. 17]).
pack(39,15,chicago,T) :- clp_meta([ T .>=. 29, T .=<. 31]).
pack(38,81,chicago,T) :- clp_meta([ T .>=. 24, T .=<. 27]).
pack(37,45,chicago,T) :- clp_meta([ T .>=. 2,  T .=<. 8 ]).
pack(36,40,chicago,T) :- clp_meta([ T .>=. 5,  T .=<. 22]).
pack(35,43,chicago,T) :- clp_meta([ T .>=. 4,  T .=<. 11]).
pack(34,22,chicago,T) :- clp_meta([ T .>=. 23, T .=<. 30]).
pack(33,60,chicago,T) :- clp_meta([ T .>=. 4,  T .=<. 30]).
pack(32,82,chicago,T) :- clp_meta([ T .>=. 28, T .=<. 30]).
pack(31,41,chicago,T) :- clp_meta([ T .>=. 27, T .=<. 29]).
pack(30,29,chicago,T) :- clp_meta([ T .>=. 19, T .=<. 29]).
pack(29,82,chicago,T) :- clp_meta([ T .>=. 20, T .=<. 29]).
pack(28,24,chicago,T) :- clp_meta([ T .>=. 8,  T .=<. 12]).
pack(27,11,chicago,T) :- clp_meta([ T .>=. 21, T .=<. 22]).
pack(26,57,chicago,T) :- clp_meta([ T .>=. 8,  T .=<. 28]).
pack(25,30,chicago,T) :- clp_meta([ T .>=. 14, T .=<. 18]).
pack(24,71,chicago,T) :- clp_meta([ T .>=. 11, T .=<. 14]).
pack(23,31,chicago,T) :- clp_meta([ T .>=. 10, T .=<. 27]).
pack(22,34,chicago,T) :- clp_meta([ T .>=. 19, T .=<. 20]).
pack(21,73,chicago,T) :- clp_meta([ T .>=. 0,  T .=<. 19]).
pack(20,83,chicago,T) :- clp_meta([ T .>=. 21, T .=<. 24]).
pack(19,86,chicago,T) :- clp_meta([ T .>=. 10, T .=<. 12]).
pack(18,79,chicago,T) :- clp_meta([ T .>=. 22, T .=<. 24]).
pack(17,50,chicago,T) :- clp_meta([ T .>=. 18, T .=<. 28]).
pack(16,58,chicago,T) :- clp_meta([ T .>=. 13, T .=<. 14]).
pack(15,69,chicago,T) :- clp_meta([ T .>=. 4,  T .=<. 14]).
pack(14,77,chicago,T) :- clp_meta([ T .>=. 22, T .=<. 28]).
pack(13,74,chicago,T) :- clp_meta([ T .>=. 27, T .=<. 30]).
pack(12,65,chicago,T) :- clp_meta([ T .>=. 12, T .=<. 24]).
pack(11,26,chicago,T) :- clp_meta([ T .>=. 12, T .=<. 26]).
pack(10,56,chicago,T) :- clp_meta([ T .>=. 15, T .=<. 16]).
pack(9, 15,chicago,T) :- clp_meta([ T .>=. 29, T .=<. 30]).
pack(8, 81,chicago,T) :- clp_meta([ T .>=. 24, T .=<. 26]).
pack(7, 45,chicago,T) :- clp_meta([ T .>=. 2,  T .=<. 7]).
pack(6, 40,chicago,T) :- clp_meta([ T .>=. 5,  T .=<. 21]).
pack(5, 43,chicago,T) :- clp_meta([ T .>=. 4,  T .=<. 10]).
pack(4, 22,chicago,T) :- clp_meta([ T .>=. 23, T .=<. 29]).
pack(3, 60,chicago,T) :- clp_meta([ T .>=. 4,  T .=<. 29]).
pack(2, 82,chicago,T) :- clp_meta([ T .>=. 28, T .=<. 29]).
pack(1, 41,chicago,T) :- clp_meta([ T .>=. 27, T .=<. 28]).

% pack(60,D,chicago,T) :- clp_meta([D.=. 29, T .>=. 19, T .=<. 30]).
% pack(59,D,chicago,T) :- clp_meta([D.=. 82, T .>=. 20, T .=<. 30]).
% pack(58,D,chicago,T) :- clp_meta([D.=. 24, T .>=. 8,  T .=<. 13]).
% pack(57,D,chicago,T) :- clp_meta([D.=. 11, T .>=. 21, T .=<. 23]).
% pack(56,D,chicago,T) :- clp_meta([D.=. 57, T .>=. 8,  T .=<. 29]).
% pack(55,D,chicago,T) :- clp_meta([D.=. 30, T .>=. 14, T .=<. 19]).
% pack(54,D,chicago,T) :- clp_meta([D.=. 71, T .>=. 11, T .=<. 15]).
% pack(53,D,chicago,T) :- clp_meta([D.=. 31, T .>=. 10, T .=<. 26]).
% pack(52,D,chicago,T) :- clp_meta([D.=. 34, T .>=. 19, T .=<. 21]).
% pack(51,D,chicago,T) :- clp_meta([D.=. 73, T .>=. 0,  T .=<. 20]).
% pack(50,D,chicago,T) :- clp_meta([D.=. 83, T .>=. 21, T .=<. 25]).
% pack(49,D,chicago,T) :- clp_meta([D.=. 86, T .>=. 10, T .=<. 13]).
% pack(48,D,chicago,T) :- clp_meta([D.=. 79, T .>=. 22, T .=<. 25]).
% pack(47,D,chicago,T) :- clp_meta([D.=. 50, T .>=. 18, T .=<. 29]).
% pack(46,D,chicago,T) :- clp_meta([D.=. 58, T .>=. 13, T .=<. 15]).
% pack(45,D,chicago,T) :- clp_meta([D.=. 69, T .>=. 4,  T .=<. 15]).
% pack(44,D,chicago,T) :- clp_meta([D.=. 77, T .>=. 22, T .=<. 29]).
% pack(43,D,chicago,T) :- clp_meta([D.=. 74, T .>=. 27, T .=<. 31]).
% pack(42,D,chicago,T) :- clp_meta([D.=. 65, T .>=. 12, T .=<. 25]).
% pack(41,D,chicago,T) :- clp_meta([D.=. 26, T .>=. 12, T .=<. 27]).
% pack(40,D,chicago,T) :- clp_meta([D.=. 56, T .>=. 15, T .=<. 17]).
% pack(39,D,chicago,T) :- clp_meta([D.=. 15, T .>=. 29, T .=<. 31]).
% pack(38,D,chicago,T) :- clp_meta([D.=. 81, T .>=. 24, T .=<. 27]).
% pack(37,D,chicago,T) :- clp_meta([D.=. 45, T .>=. 2,  T .=<. 8 ]).
% pack(36,D,chicago,T) :- clp_meta([D.=. 40, T .>=. 5,  T .=<. 22]).
% pack(35,D,chicago,T) :- clp_meta([D.=. 43, T .>=. 4,  T .=<. 11]).
% pack(34,D,chicago,T) :- clp_meta([D.=. 22, T .>=. 23, T .=<. 30]).
% pack(33,D,chicago,T) :- clp_meta([D.=. 60, T .>=. 4,  T .=<. 30]).
% pack(32,D,chicago,T) :- clp_meta([D.=. 82, T .>=. 28, T .=<. 30]).
% pack(31,D,chicago,T) :- clp_meta([D.=. 41, T .>=. 27, T .=<. 29]).
% pack(30,D,chicago,T) :- clp_meta([D.=. 29, T .>=. 19, T .=<. 29]).
% pack(29,D,chicago,T) :- clp_meta([D.=. 82, T .>=. 20, T .=<. 29]).
% pack(28,D,chicago,T) :- clp_meta([D.=. 24, T .>=. 8,  T .=<. 12]).
% pack(27,D,chicago,T) :- clp_meta([D.=. 11, T .>=. 21, T .=<. 22]).
% pack(26,D,chicago,T) :- clp_meta([D.=. 57, T .>=. 8,  T .=<. 28]).
% pack(25,D,chicago,T) :- clp_meta([D.=. 30, T .>=. 14, T .=<. 18]).
% pack(24,D,chicago,T) :- clp_meta([D.=. 71, T .>=. 11, T .=<. 14]).
% pack(23,D,chicago,T) :- clp_meta([D.=. 31, T .>=. 10, T .=<. 27]).
% pack(22,D,chicago,T) :- clp_meta([D.=. 34, T .>=. 19, T .=<. 20]).
% pack(21,D,chicago,T) :- clp_meta([D.=. 73, T .>=. 0,  T .=<. 19]).
% pack(20,D,chicago,T) :- clp_meta([D.=. 83, T .>=. 21, T .=<. 24]).
% pack(19,D,chicago,T) :- clp_meta([D.=. 86, T .>=. 10, T .=<. 12]).
% pack(18,D,chicago,T) :- clp_meta([ D.=. 79, T .>=. 22, T .=<. 24]).
% pack(17,D,chicago,T) :- clp_meta([D.=. 50, T .>=. 18, T .=<. 28]).
% pack(16,D,chicago,T) :- clp_meta([D.=. 58, T .>=. 13, T .=<. 14]).
% pack(15,D,chicago,T) :- clp_meta([D.=. 69, T .>=. 4,  T .=<. 14]).
% pack(14,D,chicago,T) :- clp_meta([D.=. 77, T .>=. 22, T .=<. 28]).
% pack(13,D,chicago,T) :- clp_meta([D.=. 74, T .>=. 27, T .=<. 30]).
% pack(12,D,chicago,T) :- clp_meta([D.=. 65, T .>=. 12, T .=<. 24]).
% pack(11,D,chicago,T) :- clp_meta([D.=. 26, T .>=. 12, T .=<. 26]).
% pack(10,D,chicago,T) :- clp_meta([D.=. 56, T .>=. 15, T .=<. 16]).
% pack(9,D,chicago,T) :- clp_meta([D.=. 15, T .>=. 29, T .=<. 30]).
% pack(8,D,chicago,T) :- clp_meta([D.=. 81, T .>=. 24, T .=<. 26]).
% pack(7,D,chicago,T) :- clp_meta([D.=. 45, T .>=. 2,  T .=<. 7]).
% pack(6,D,chicago,T) :- clp_meta([D.=. 40, T .>=. 5,  T .=<. 21]).
% pack(5,D,chicago,T) :- clp_meta([D.=. 43, T .>=. 4,  T .=<. 10]).
% pack(4,D,chicago,T) :- clp_meta([D.=. 22, T .>=. 23, T .=<. 29]).
% pack(3,D,chicago,T) :- clp_meta([D.=. 60, T .>=. 4,  T .=<. 29]).
% pack(2,D,chicago,T) :- clp_meta([D.=. 82, T .>=. 28, T .=<. 29]).
% pack(1,D,chicago,T) :- clp_meta([D.=. 41, T .>=. 27, T .=<. 28]).

% pack(I,D,chicago,T) :- clp_meta([I .=. 60, D.=. 29, T .>=. 19, T .=<. 30]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 59, D.=. 82, T .>=. 20, T .=<. 30]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 58, D.=. 24, T .>=. 8,  T .=<. 13]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 57, D.=. 11, T .>=. 21, T .=<. 23]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 56, D.=. 57, T .>=. 8,  T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 55, D.=. 30, T .>=. 14, T .=<. 19]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 54, D.=. 71, T .>=. 11, T .=<. 15]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 53, D.=. 31, T .>=. 10, T .=<. 26]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 52, D.=. 34, T .>=. 19, T .=<. 21]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 51, D.=. 73, T .>=. 0,  T .=<. 20]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 50, D.=. 83, T .>=. 21, T .=<. 25]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 49, D.=. 86, T .>=. 10, T .=<. 13]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 48, D.=. 79, T .>=. 22, T .=<. 25]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 47, D.=. 50, T .>=. 18, T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 46, D.=. 58, T .>=. 13, T .=<. 15]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 45, D.=. 69, T .>=. 4,  T .=<. 15]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 44, D.=. 77, T .>=. 22, T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 43, D.=. 74, T .>=. 27, T .=<. 31]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 42, D.=. 65, T .>=. 12, T .=<. 25]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 41, D.=. 26, T .>=. 12, T .=<. 27]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 40, D.=. 56, T .>=. 15, T .=<. 17]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 39, D.=. 15, T .>=. 29, T .=<. 31]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 38, D.=. 81, T .>=. 24, T .=<. 27]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 37, D.=. 45, T .>=. 2,  T .=<. 8 ]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 36, D.=. 40, T .>=. 5,  T .=<. 22]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 35, D.=. 43, T .>=. 4,  T .=<. 11]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 34, D.=. 22, T .>=. 23, T .=<. 30]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 33, D.=. 60, T .>=. 4,  T .=<. 30]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 32, D.=. 82, T .>=. 28, T .=<. 30]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 31, D.=. 41, T .>=. 27, T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 30, D.=. 29, T .>=. 19, T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 29, D.=. 82, T .>=. 20, T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 28, D.=. 24, T .>=. 8,  T .=<. 12]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 27, D.=. 11, T .>=. 21, T .=<. 22]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 26, D.=. 57, T .>=. 8,  T .=<. 28]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 25, D.=. 30, T .>=. 14, T .=<. 18]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 24, D.=. 71, T .>=. 11, T .=<. 14]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 23, D.=. 31, T .>=. 10, T .=<. 27]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 22, D.=. 34, T .>=. 19, T .=<. 20]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 21, D.=. 73, T .>=. 0,  T .=<. 19]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 20, D.=. 83, T .>=. 21, T .=<. 24]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 19, D.=. 86, T .>=. 10, T .=<. 12]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 18, D.=. 79, T .>=. 22, T .=<. 24]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 17, D.=. 50, T .>=. 18, T .=<. 28]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 16, D.=. 58, T .>=. 13, T .=<. 14]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 15, D.=. 69, T .>=. 4,  T .=<. 14]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 14, D.=. 77, T .>=. 22, T .=<. 28]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 13, D.=. 74, T .>=. 27, T .=<. 30]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 12, D.=. 65, T .>=. 12, T .=<. 24]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 11, D.=. 26, T .>=. 12, T .=<. 26]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 10, D.=. 56, T .>=. 15, T .=<. 16]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 9, D.=. 15, T .>=. 29, T .=<. 30]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 8, D.=. 81, T .>=. 24, T .=<. 26]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 7, D.=. 45, T .>=. 2,  T .=<. 7]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 6, D.=. 40, T .>=. 5,  T .=<. 21]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 5, D.=. 43, T .>=. 4,  T .=<. 10]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 4, D.=. 22, T .>=. 23, T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 3, D.=. 60, T .>=. 4,  T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 2, D.=. 82, T .>=. 28, T .=<. 29]).
% pack(I,D,chicago,T) :- clp_meta([I .=. 1, D.=. 41, T .>=. 27, T .=<. 28]).
