% Author: Jorge. A Navas, The University of Melbourne 2012

:- module(tclp_loadprogram, [ consult_input_file/2 ]).

%  Ciao libraries
:- use_module(library(filenames), [file_name_extension/3, no_path_file_name/2]).
:- use_module(library(terms)    , [atom_concat/2]).
:- use_module(library(system)   , [getenvstr/2]).

:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

%--------------------------------------------------------------------------------%
% Load program to execute
%--------------------------------------------------------------------------------%

consult_input_file(FileName,TmpFileName):-
	file_name_extension(FileName,Base,Extension),
	atom_concat(Base,'__tr',Base0),
	atom_concat(Base0,Extension,TmpFileName),	
	open(FileName,read,In),
	open(TmpFileName,write,Out),
	no_path_file_name(Base0,ModuleName),
	write_module_dir(Out,ModuleName,_),
	write_code_expansion_dir(Out),	
	% To avoid warnings
	write(Out, ':- push_prolog_flag(discontiguous_warnings,off).'),
	nl(Out),
	copyfile(In,Out), 
	write(Out, ':- pop_prolog_flag(discontiguous_warnings).'),
	nl(Out),
	close(In), 
	close(Out),
	use_module(TmpFileName).

write_module_dir(Stream, ModuleName, _ExportList):-
	atom_concat([':- ', 'module(\'', ModuleName,'\',','_).'], ModuleDirective),
	write(Stream,ModuleDirective),	
	nl(Stream).

frontend_path(Frontend_Path):-
          getenvstr('FTCLP_INSTALL',FTCLP_Path_str),
          atom_codes(FTCLP_Path, FTCLP_Path_str),
          atom_concat(FTCLP_Path,'/frontend/', Frontend_Path),
          !.
frontend_path(_):-
	format("ERROR ftclp: $FTCLP_INSTALL environment variable not found\n",[]),
	!,
	fail.

write_code_expansion_dir(Stream):-
	frontend_path(BasePath),
	atom_concat([':- include(\'',BasePath,'tclp_package.pl','\').'], Path),
	write(Stream,Path),
	nl(Stream).

copyfile(In,Out) :- 
          repeat, 
	read(In,Data),       
          process(Data,Out),    
          !.        
process(end_of_file,_) :- !. 
process(Data,Out) :-  
	write(Out,Data), write(Out,'.'), nl(Out), 
	fail. 
