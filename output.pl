% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%=======================================================================%
%                Display execution tree in DOT format
%=======================================================================%

:- module(output,
	[open_dotfile/2,
	 close_dotfile/1,
	 write_dotfile_trans/6,
	 write_dotfile_mark_node/3,
	 gen_label/2
	]).

%  Ciao libraries
:- use_module(library(filenames), [file_name_extension/3]).
:- use_module(library(terms)    , [atom_concat/2]).

open_dotfile(FileName,Stream):-
	file_name_extension(FileName,Base,_Extension),	
	atom_concat(Base,'.dot',DotFileName),
	open(DotFileName,write,Stream),
	write_dotfile_header(Stream).
	
close_dotfile(S):- 
          write_dotfile_end(S),
	close(S).

write_dotfile_header(S):-
	write(S,'strict digraph G { '),nl(S),
	write(S,'\t size = \"10,10\"; '),nl(S),
	write(S,'\t node [shape=plaintext,width=.1,height=.1]; '),nl(S),
	write(S,'\t compound=true; '), nl(S).

write_dotfile_end(S):-
	write(S,'} '), nl(S).

write_dotfile_trans(T,N1,N2,LabelForN2,LabelForEdge,S):-
	write(S,'\t'), 
	write(S,'\"'),
	write(S,N1), 
	write(S,'\"'),
	write(S,'->'), 
	write(S,'\"'),
	write(S,N2), 
	write(S,'\"'),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% decoration on edges
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	( LabelForEdge == [] -> true % to avoid noise
	; write(S,' [label= \"'),
	  write(S, LabelForEdge),
	  write(S,'\"]')
          ),
	!,
	( T == feasible ->
 	  write(S,' [style=filled]')
          ; 
	    ( T == infeasible ->
	      write(S,' [arrowhead=dot,style=filled,color=red]')
	    ;
	       ( T == fake_infeasible ->
	         write(S,' [arrowhead=dot,style=filled,color=brown]')
	       ;
	         ( T == subsumed ->		
		 write(S,' [style=filled,color=green]')
	         ;
		 format("ERROR ftclp: write_dotfile_trans/3 failed.\n",[]),
		 halt
	         )
                 )
	    )
          ),
	write(S,';'),
	nl(S),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% decoration on nodes
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( T == subsumed ->
	  %  write_dotfile_two_nodes_same_rank(N1,N2,S)
	  write(S,'\t\"'),	    
	  write(S,N1), 
	  write(S,'\"'),
	  write(S, ' [style=filled, color=green, shape=ellipse];'),
	  nl(S)
	;
	  ( T == infeasible ->
	    write(S,'\t\"'),	    
	    write(S,N2), 
	    write(S,'\"'),
	    write(S, ' [label= \"false\"];'),
	    nl(S)
	  ;  
	      ( T == fake_infeasible ->
		write(S,'\t\"'),	    
		write(S,N2), 
		write(S,'\"'),
		write(S, ' [label= \"fake false\"];'),
		nl(S)
	      ;  
		  ( T == feasible ->
		    write(S,'\t\"'),	    
		    write(S,N2), 
		    write(S,'\"'),
		    write(S,' [label= \"'),
		    write(S, LabelForN2),
		    write(S,'\"];'),
		    nl(S)
		  ;
		      format("ERROR(write_dotfile_trans): unsupported flag\n",[]),
		      halt
		  )
	      )
	  )
       ).

write_dotfile_mark_node(T,N,S):-
	write(S,'\t'), 
	write(S,'\"'),
	write(S,N), 
	write(S,'\"'),
	( T == success ->	
	   write(S,'[shape=box,style=bold,color=blue];')
	;
	  ( T == failure ->  
	    write(S,'[shape=box,style=bold,color=red];')   
	  ;
              format("ERROR(write_dotfile_mark_node): unsupported 1st arg ~q\n",[T]),
	    halt
         )		
	),
	nl(S).

write_dotfile_two_nodes_same_rank(N1,N2,S):-	
	write(S, '\t { rank =same; '),
	write(S,'\"'),
 	write(S,N1),
	write(S,'\"'),
	write(S, '; '),
	write(S,'\"'),
 	write(S,N2),
	write(S,'\"'),
	write(S, ';};\n ').

% gen_label(+term,-atom)
% A label of the form PredName/Arity/ClauseId#Depth#Context
gen_label(node(root,_,_,_), root):- !.	
gen_label(node(Goal, K, Depth, Context), Label):-
	functor(Goal,F,A),
	atom_number(K_atm,K),
	atom_number(A_atm,A),
	atom_number(D_atm,Depth),	
	atom_concat('Depth:',D_atm,D_atm1),
	atom_number(C_atm,Context),	
	atom_concat([F,'/',A_atm,'/',K_atm,'#',D_atm1,'#',C_atm], Label).






	