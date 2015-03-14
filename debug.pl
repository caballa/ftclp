% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%=======================================================================%
%                  Print debugging messages
%=======================================================================%

:- module(debug, [ debug_message/2 ]).

% Own libraries
:- use_module(options).
% Ciao libraries
:- use_module(library(format), [format/2]).

debug_message(Msg,ArgList):- 
	is_enabled_option(debug),
	format(Msg,ArgList),
	!.
debug_message(_,_):- !.
	% format_to_string(Msg,ArgList,Msg0),
	% string_add_color(Msg0,green,Msg1),
	% format("~q",[Msg1]).

% color_code(black,0).
% color_code(red,1).
% color_code(green,2).
% color_code(yellow,3).
% color_code(blue,4).
% color_code(magenta,5).
% color_code(cyan,6).
% color_code(white,7).

% string_add_color(String,Color,NString):-
% 	Color == yellow,
% 	!,
% 	% foreground
% 	FG0 = 33,
% 	% background always black
% 	BG0 = 40, 
% 	format_to_string("\033[0;~q;~qm~q\033[m",[BG0,FG0,String], NString).
% string_add_color(String,Color,NString):-
% 	color_code(Color,FG),
% 	% foreground
% 	FG0 is 30+FG,
% 	% background always white
% 	BG  = 7, BG0 is 40 + BG, 
% 	format_to_string("\033[0;~q;~qm~q\033[m",[BG0,FG0,String],NString).
