% Author: Jorge. A Navas, The University of Melbourne 2012

:- module(ftclp_loadprogram, [ consult_input_file/2 ]).

% include automatically generated file
:- include('../ftclp_paths.pl').
%  Ciao libraries
:- use_module(library(pathnames), [path_split/3, path_splitext/3]).
:- use_module(library(terms)    , [atom_concat/2]).

:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

%--------------------------------------------------------------------------------%
% Load program to execute
%--------------------------------------------------------------------------------%

consult_input_file(FileName,TmpFileName):-
	path_splitext(FileName,Base,Extension),
	atom_concat(Base,'__tr',Base0),
	atom_concat(Base0,Extension,TmpFileName),	
	open(FileName,read,In),
	open(TmpFileName,write,Out),
	path_split(Base0,_,ModuleName),
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

write_code_expansion_dir(Stream):-
	frontend_path(BasePath),
	atom_concat([':- include(\'',BasePath,'/', 'ftclp_package.pl','\').'], Path),
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
