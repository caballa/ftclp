% Author: Jorge. A Navas, The University of Melbourne 2013

:- module(normalize, [normalize/2, normalize/4]).

%  Own libraries
:- use_module(readprog).
:- use_module(writeprog).
:- use_module(clauseTermElim).
%  Ciao libraries
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(filenames), [file_name_extension/3]).

%------------------------------------------------------------------%
% normalize(+InFile, -OutFile)
% normalize(+Prog,+Dirs,+InFile,-OutFile)
%------------------------------------------------------------------%
% Execute all the program transformations needed to obtain a normal
% form.
%------------------------------------------------------------------%
:- push_prolog_flag(multi_arity_warnings,off).
normalize(InFile, OutFile):-
	readprog(InFile, Prog, Dirs),
	normalize(Prog,Dirs,InFile,OutFile).
normalize(Prog, Dirs, InFile, OutFile):-
          % Here the program transformations
	clauseTermElim(Prog, NormProg),
	% Write to a file the transformed program
	file_name_extension(InFile,Base,Extension),	
	atom_concat(Base,'__norm', Base0),
	atom_concat(Base0,Extension, OutFile),
	open(OutFile,write,Stream),	
	writeprog(NormProg, Dirs, Stream),
	close(Stream).
:- pop_prolog_flag(multi_arity_warnings).	