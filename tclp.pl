% Author: Jorge. A Navas, The University of Melbourne 2012-2013

%======================================================================%
% CLP interpreter that uses interpolants for pruning redundant failed
% derivations. The interpreter can handle recursive clauses and it can
% terminate for some programs even if they have some infinite
% derivations.
%
% This implementation follows the paper "Failure Tabled Constraint
% Logic Programming by Interpolation" written by Graeme Gange, Jorge
% A. Navas, Peter Schachte, Harald Sondergaard, and Peter J. Stuckey
% and published in ICLP'13.
%======================================================================%

:- module(tclp, [ tclp/5, main/1, get_map_node_to_unique_id/4, is_fake_false_id/1]).

%  Ciao libraries
:- use_module(library(compiler) , [unload/1]). 
:- use_module(library(terms)    , [atom_concat/2]).
:- use_module(library(write)    , [numbervars/3]).
:- use_module(library(lists)    , [append/3, last/2, length/2, reverse/2]).
:- use_module(library(format)   , [format/2]).
:- use_module(library(prolog_sys)).
:- use_module(library(assoc)    , [get_assoc/3,assoc_to_list/2]).
:- use_module(library(system)   , [file_exists/1,getenvstr/2]).
:- use_module('frontend/readprog').
%  Own libraries
:- use_module('frontend/tclp_loadprogram').
:- use_module('frontend/tclp_tr').
:- use_module('frontend/normalize').
:- use_module('frontend/counter_instrument').
:- use_module('frontend/remove_useless_clauses').
:- use_module('frontend/well_formed').
:- use_module('analysis/modes').
:- use_module('analysis/types').
:- use_module('analysis/discriminants').
:- use_module('analysis/scc').
:- use_module('adt/mysets').
:- use_module('adt/mystrings').
:- use_module(options).
:- use_module(solver).
:- use_module(cache).
:- use_module(answers).
:- use_module(interpolation).
:- use_module(op_attributes).
:- use_module(timer).
:- use_module(debug).
:- use_module(output).
:- use_module(counters).
:- include(cycles).

 


%---------------------------------------------------------------------%
% main/1: run the metainterpreter as an executable.
%---------------------------------------------------------------------%
main(ArgVs):-
	% For now, mathsat is the only solver.
	SolverName      = mathsat,
	read_and_process_user_options(ArgVs, 
	                              Entry, InputFile, Depth, Options),
	( (var(Entry) ; var(InputFile)) -> 
	   true
	; 
	  ( var(Depth) -> 
   	    tclp(InputFile,Entry,SolverName,0,Options)
	  ;
	    tclp(InputFile,Entry,SolverName,Depth,Options)  
	  )
	),
	!.
main(_):- !.

%---------------------------------------------------------------------%
%  From the program transformation done by tclp_tr
%---------------------------------------------------------------------%
:- multifile rule/3.
:- multifile builtin/3.
:- multifile constraints/2.
 
init_stats:-
	set_counter(num_of_nodes           , 0),
	set_counter(num_of_infeasible_paths, 0),
	set_counter(num_of_solutions       , 0),
	init_interpolation_stats.

print_stats(EnableCaching):-
	get_counter_value(num_of_nodes,N1),
	get_counter_value(num_of_infeasible_paths,N2),
	get_counter_value(num_of_solutions,N3),
	format("\nEXECUTION STATS \n",[]),
	format("\tNumber of states ..................................... ~q\n",[N1]),
	format("\tNumber of failed derivations detected by the solver... ~q\n",[N2]),
	format("\tNumber of answers..................................... ~q\n",[N3]),
	( EnableCaching==tt -> print_cache_stats ;  true),
	print_solver_stats,
	print_interpolation_stats,
	( is_enabled_option(cycle_subsumption) -> print_cycle_stats ; true).

init_timers:-
          mk_timer(load_file),
	mk_timer(compute_scc),
	mk_timer(execution),
	mk_timer(push),
	mk_timer(pop),
	mk_timer(check_sat),
	mk_timer(assert_constraints),
	init_interpolation_profiling_timers,
	init_cache_profiling_timers,
	init_answers_profiling_timers.

print_profiling_timers:-
	format("\nPROFILING INFORMATION \n",[]),
	get_timer(check_sat,T1),
	get_timer(assert_constraints,T2),
	get_timer(push,T3),
	get_timer(pop ,T4),
	format("Push.................................................... ~q ms \n",[T3]),
	format("Pop..................................................... ~q ms \n",[T4]),
	format("Assert constraints...................................... ~q ms \n",[T2]),
	format("Satisfiability checks................................... ~q ms \n",[T1]),
	print_interpolation_profiling_timers,
	print_cache_profiling_timers,
	print_answers_profiling_timers.


cleanup:-
	retractall_fact('$maximum_depth'(_)),
	retractall_fact('$incomplete_search'),
	retractall_fact('$tracked_node'(_,_,_,_)),
 		
	retractall_fact('$fake_false'(_)),
	delete_all_counters,
	clear_timers,
	clear_tclp_tr,
	clear_cache,
	clear_modes,
	clear_types,
	clear_discriminants,
	clear_solver,
	clear_interpolation,
	cleanup_user_flags,
	cleanup_cycles.

% Succeed if we need to run the machinery to reuse answers.
is_reusing_answers:-
 	is_enabled_option(clause_subsumption).
is_reusing_answers:-
 	is_enabled_option(pred_subsumption).
	
% This is needed for fixing the path for load_compilation_module/1.
:- multifile library_directory/1. :- dynamic library_directory/1.
:- multifile file_search_path/2.  :- dynamic file_search_path/2.
add_ftclp_in_ciao_libpaths :-
          getenvstr('FTCLP_INSTALL',FTCLP_Path_str),
          atom_codes(FTCLP_Path, FTCLP_Path_str),
          atom_concat(FTCLP_Path,'//ftclp', FTCLP_Lib),
          asserta_fact(library_directory(FTCLP_Lib)),
          asserta_fact(file_search_path(ftclp, FTCLP_Path)).
        
%---------------------------------------------------------------%
% tclp(+atm,+term,+atm,+num,+list(atm))
%---------------------------------------------------------------%
tclp(InFile, Goal, SolverName, MaxDepth, Opts):-
	cleanup,
	init_timers,
	% process user options
	process_options(Opts),
	% load program		
	add_ftclp_in_ciao_libpaths,
	check_input_file(InFile,InFile0),
	load_file_and_get_sccs(InFile,InFile0,InFile2,RecPreds,RecCls,SCCs),
	% for dot output
	(is_enabled_option(dot_output)->open_dotfile(InFile, OutStream);true),
	set_prolog_flag(write_strings,on),
	% Gather mode information 
	% gather_modes,	
	% Gather type information 
	process_predicates_types, 
	% Gather discriminant information 
	process_predicates_discriminants, 
	% Solver initialization
	initialize_solver(SolverName, Solver),
	% Initialize data structure to store efficiently all answers
	(is_reusing_answers -> initialize_answers(Answers) ; true),
	% Cache initialization
	( (is_enabled_option(clause_subsumption) ;
	   is_enabled_option(pred_subsumption)) -> 
	   EnableCaching=tt ; EnableCaching=ff),
	( EnableCaching==tt -> init_cache ;  true),
	% Stats initialization for the execution
	init_stats,
	% Initialize counter for variable names:
	% we must name variables using a counter that is not affected
	% by backtracking.
	set_counter(variable_names,1),
	% EXECUTION OF THE PROGRAM
	record_maximum_depth(MaxDepth),
	init_cycles(RecPreds, SCCs),
	start_timer(execution),
	execute_all_sol(Goal, Answers, RecPreds, RecCls, Solver, InFile, OutStream),
	stop_timer(execution, T1),
	format("ftclp: ~q executed in ~q ms.\n",[InFile, T1]),	
	dump_interpolants(InFile, Solver),
	debug_message("##########################################\n",[]),
	debug_message("        Final Interpolation Cache         \n",[]),
	debug_message("##########################################\n",[]),
	debug_print_intp_table(Solver),
	debug_message("##########################################\n",[]),
	( (get_counter_value(num_of_solutions,NumOfSols), NumOfSols == 0) ->
	   ( current_fact('$incomplete_search') ->
	     format("RESULT: INCOMPLETE. No solutions found.\n",[]) 
	   ;
	     format("RESULT: NO SOLUTIONS FOUND!\n",[])
	   )
	; 
	  ( is_enabled_option(solutions(N))->
	    format("RESULT: SOLUTIONS FOUND. Execution stopped after ~q solutions.\n",[N]) 
	  ; 
	    get_counter_value(num_of_solutions,NumOfSols),
	    format("RESULT: SOLUTIONS FOUND. Found ~q solutions.\n",[NumOfSols])
            )
          ),
	( is_enabled_option(no_verbose) -> true
          ;
	  % For printing stats
	  print_stats(EnableCaching),
	  % For printing profiling information
	  print_profiling_timers
          ),
	solver_stop(Solver),	
	(is_reusing_answers -> end_answers(Answers) ; true),
	% we force the unloading of the files to avoid problems if
	% further calls to tclp/2.
	unload_program(InFile2),
	!.
tclp(_,_,_,_,_):- !. % Here if input filename is not found.

% load_file_and_get_sccs(+, +, -, -, -, -)
% InFile0 is InFile including the absolute path.
load_file_and_get_sccs(InFile, InFile0, InFile2, RecPreds, RecCls, SCCs):-
	start_timer(load_file),
	transform_program(InFile0, InFile1),
	consult_input_file(InFile1,InFile2),
	stop_timer(load_file, T0),
	format("ftclp: loaded ~q in ~q ms.\n",[InFile,T0]),
	start_timer(compute_scc),
	% recursive_preds(InFile1, RecPreds, _),
	readprog(InFile1, Prog), 	
	sortClauses(Prog,Ps,Procs),
	not_redefine_builtins(Procs), % well-formed condition
	recursive_preds(Ps, Procs, RecPreds, SCCs),
	recursive_clauses(Ps, Procs, RecCls),
	stop_timer(compute_scc,T2),
	format("ftclp: computed sccs in ~q ms.\n",[T2]).

unload_program(InFile):- unload(InFile).

%--------------------------------------------------------------------------%
% transform_program(+atm,+atm)
%--------------------------------------------------------------------------%
% Source-to-source program transformations
%--------------------------------------------------------------------------%
transform_program(InFile, OutFile):-
        % readprog(InFile, Prog, Dirs), 	
        % sortClauses(Prog,Ps,Procs),
        %% After each transformation make sure that Ps, Procs, and Dirs
        %% are not invalidated. If yes, they must be updated.
        % normalize(Prog, Dirs, InFile, OutFile).
        
        ( is_enabled_option(cycle_subsumption) ->
	( is_enabled_option(interpolation) ->
	  counter_instrument(InFile, TmpFile),
	  remove_useless_clauses(TmpFile, OutFile)
 
	;
	  format("ERROR ftclp: interpolation must be enabled.\n",[])
	)
        ;
	OutFile=InFile
        ).
        
% check_input_file(+,-)
% Utility to check file exists.
check_input_file(FileName,AbsoluteFileName):-
	absolute_file_name(FileName,AbsoluteFileName),
	file_exists(AbsoluteFileName),!.
check_input_file(FileName,_):-
	format("ERROR ftclp: ~q not found \n",[FileName]),
	!,
	fail.

%================= BEGIN METAINTERPRETER ==================================%

%--------------------------------------------------------------------------%
% execute_all_sol(Goal, Answers, RecPreds, RecCls, Solver, InFile, OutStream)
% execute_all_sol(+term, +Answers, +list, +list, +Solver, +atom , +atom)
%--------------------------------------------------------------------------%
% Execute Goal and returns all its solutions
% - Answers contains all answers.
% - RecPreds identifies which are the recursive predicates.
% - RecCls identifies which are the recursive clauses.
% - Solver is the solver used for satisfiability checks and
%   generation of interpolants.
% - OutStream a stream where redirect the DOT commands for displaying
%   the tree.
%--------------------------------------------------------------------------%
execute_all_sol(Goal, Answers, RecPreds, RecCls, Solver, _InFile, OutStream):-
	incr_counter(num_of_nodes,_),
	Predec      = node(root,_,0,0),
	goal_stack__init(InitStack),
	Depth       = 1,
	Goals       = [Goal],
	intp_adt__init(TrackedItpInfo),
	mk_set(Set),
	asserta_fact('$fake_false'(Set)),
	( is_reusing_answers -> 
	  init_prefix(InitPrefix),
	  record_initial_goal(Goal) 
	; 
	  true
	),
	execute_one_sol(Goals, Predec, Succ, InitStack, EndStack, 
	                Depth, _LastDepth, TrackedItpInfo, _ , 
	                InitPrefix, _EndPrefix, Answers,
		      RecPreds, RecCls, Solver, OutStream), 	
	%%%%%
	%% Here we obtained already one solution for the entry Goal.
	%% We force backtracking for seeking more solutions
	%% TODO: this should be driven by the user by typing ";"
	%%%%%
	start_timer(add_answer),
	(is_reusing_answers -> add_answer(Answers, EndStack) ; true),
	stop_timer(add_answer,_),
	( is_enabled_option(print_answers) ->
	  % length(EndStack, StackLength),
	  % format("Length: ~q ",[StackLength]),
	  % goal_stack__print(EndStack) 
	  get_counter_value(num_of_solutions,NumOfSol),	  
	  NumOfSol1 is NumOfSol + 1,
	  ( (is_enabled_option(solutions(N)), N ==1) ->
	    format("Solution:",[])
	  ;
	    format("Solution ~q-th: ",[NumOfSol1])
            ),
	  goal_stack__print_only_functors(EndStack)
	; 
	  true
	),
	( is_enabled_option(dot_output) ->
	  gen_label(Succ,Label),
	  write_dotfile_mark_node(success,Label,OutStream) 
	; 
	  true
	),
	incr_counter(num_of_solutions,_),
	get_counter_value(num_of_solutions,NumOfSolutions),
	( ( is_enabled_option(solutions(N)), (NumOfSolutions >= N)) ->
	  % For just one solution
	  !,
	  (is_enabled_option(dot_output)->close_dotfile(OutStream);true)
          ;
	  % For more solutions
	  fail
          ).
execute_all_sol(_,_,_,_,_,_,OutStream):-
 	is_enabled_option(cycle_subsumption),
 	get_counter_value(num_of_solutions,NumOfSols),	  
	( ( is_enabled_option(solutions(N)), (NumOfSols >= N)) ->
	  !,
	  % produced all answers required by the user
	  (is_enabled_option(dot_output)->close_dotfile(OutStream);true)
          ;
	  fail
          ).          
execute_all_sol(Goal, Answers, RecPreds,  RecCls, Solver, InFile, OutStream):-
	is_enabled_option(cycle_subsumption),
	% Next, we check if we succeed to get rid of all fake
	% infeasible paths
	current_fact('$fake_false'(FakeSet)),
	( FakeSet == [] -> 
	  % Yes, our search is complete!
	  (is_enabled_option(dot_output)->close_dotfile(OutStream);true)
	; 
	  ( is_enabled_option(no_verbose) -> true ;
	    format("INCOMPLETE EXECUTION TREE:\n",[]),
	    print_fake_nodes(FakeSet,'\tFailed to subsume')), 
	    incr_unroll_depth(FakeSet, 1, ReachedMaxFlag),  % we increase by one!
	  ( is_enabled_option(no_verbose) -> true ;
  	    format("--------------------------------------------------\n",[])),
            ( ReachedMaxFlag > 0 -> % we have exceeded the maximum of unrollings
	    asserta_fact('$incomplete_search'),
	    debug_message("FORCING EXECUTION TO STOP AFTER ~w UNROLLS.\n\n",[ReachedMaxFlag]),
 	    (is_enabled_option(dot_output)->close_dotfile(OutStream);true)	    
            ; 
  	    %%
	    %% Completely naive restart from the root 
	    %%
	    %% TODO: we should restart only the execution of those
	    %% predicates whose termination is not ensured.
	    %%
	    %% Make sure that cleanup everything:
	    retractall_fact('$fake_false'(_)),
	    clear_cache,
	    clear_answers(Answers),
	    reset_cycles,
	    % for dot output: discard the tree by previous unrollings.
	    ( is_enabled_option(dot_output) ->
	      close_dotfile(OutStream),
	      open_dotfile(InFile, NewOutStream)
	    ; 
	        true
	    ),
	    %% Run again
	    execute_all_sol(Goal,Answers,RecPreds,RecCls,Solver,InFile,NewOutStream)
	  )
	),
	!.
execute_all_sol(_,_,_,_,_,_,OutStream):-
        (is_enabled_option(dot_output)->close_dotfile(OutStream);true).

%--------------------------------------------------------------------------%
% execute_one_sol: main predicate that executes a predicate
%--------------------------------------------------------------------------%
% - Predec, Succ are previous and next nodes in the current path
%   (for DOT purposes)
% - Stack contains the executed path (StackADT)
% - Depth is the current depth of the path.
% - TrackedItpInfo encapsulates some information (e.g, interpolation cuts)
%   used for generating interpolants  (ItpADT).
% - Prefix is just an atom with the traversed predicates along the path.
%--------------------------------------------------------------------------%
execute_one_sol(_, _, _, _, _, Depth, _, _, _, _, _, _, _, _,_,_):-
	% Maximum depth reached
	reached_maximum_depth(Depth),
	!,
	asserta_fact('$incomplete_search'),
	debug_message("FORCING EXECUTION TO STOP AFTER DEPTH ~w \n\n",[Depth]),
	fail.
execute_one_sol([], Predec, Predec, Stack, Stack, Depth, Depth,
	      TrackedItpInfo, TrackedItpInfo, Prefix, Prefix,_,_,_,_Solver,_).
execute_one_sol([constraints(Cs,Cs_Id)| Goals], Predec, Last, Stack, EndStack, 
	      Depth, EndDepth, TrackedItpInfo, EndTrackedItpInfo, 
	      Prefix, EndPrefix, Answers, RecPreds, RecCls, Solver, OutStream):-	       
	incr_counter(num_of_nodes,_),
	goal_stack__push(Stack,constraints(Cs,_CsSig,Cs_Id)-Depth, NewStack),
 		
	execute_clp_constraints(Cs, Cs_Id, NewStack, Predec, Succ, Depth, 
	                        TrackedItpInfo, NewTrackedItpInfo, 
			    RecPreds, RecCls, Solver, OutStream),
	NewDepth is Depth+1,
	execute_one_sol(Goals, Succ, Last, NewStack, EndStack, 
	                NewDepth, EndDepth, 
		      NewTrackedItpInfo, EndTrackedItpInfo,
		      Prefix, EndPrefix, Answers, RecPreds, RecCls, 
                          Solver, OutStream).
execute_one_sol([constraints(_,_)| _],_Predec,_,_,_,_PredecDepth,
                _,_,_,_,_,_,_,_,Solver,_):-
	solver_pop(Solver),
 	fail.
execute_one_sol([builtin(Builtin, Builtin_Id, Module)| Goals], Predec, Last, 
	       Stack, EndStack, Depth, EndDepth, 
	       TrackedItpInfo, EndTrackedItpInfo, Prefix, EndPrefix, Answers, 
	       RecPreds, RecCls, Solver, OutStream):-	 
          NewStack = Stack,
	incr_counter(num_of_nodes,_),
	execute_builtin(Builtin, Builtin_Id, Module, Predec, Succ, Depth, OutStream),
	NewDepth is Depth+1,
	execute_one_sol(Goals, Succ, Last, NewStack, EndStack, 
	                NewDepth, EndDepth, TrackedItpInfo, EndTrackedItpInfo,
		      Prefix, EndPrefix, Answers, RecPreds, RecCls, 
                          Solver, OutStream).
execute_one_sol([Goal|_], Predec,_,Stack,_,Depth,_,TrackedItpInfo,_,Prefix,_,	
                Answers, _RecPreds, _RecCls, Solver, OutStream):-
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% SUBSUMPTION CHECK
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	is_enabled_option(pred_subsumption),
	cache_lookup_pred(Goal, Depth, Solver, 
	                  SubsumerPrefix, SubsumerNode, SubsumedIntpInfo),
	!,
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% REUSING ANSWERS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	start_timer(reuse_answer),
	reuse_answers(Goal, Answers, SubsumerPrefix, Prefix, Solver),
	stop_timer(reuse_answer,_),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% GENERATION OF INTERPOLANTS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	debug_message("Generating interpolant from subsumed derivation ...\n",[]),
 		
	gen_interpolants(Stack, TrackedItpInfo, Solver, SubsumedIntpInfo),
 		
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% DOT output
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% FIXME: SubsumerNode refers to the first clause of the
	% predicate that appears in the program. We will display in
	% the dot file that the current node is subsumed by this
	% first clause although in reality it is subsumed the *all*
	% the clauses, not by just this one.
	mark_subsumed_trans(Predec, Goal, 0 ,Depth, SubsumerNode, OutStream),
	debug_message("**** SUBSUMED PREDICATE!\n",[]),
	!,
	% Failure here stops the exploration of the rest of the path
	fail.
execute_one_sol([Goal|Goals], Predec, Last, Stack, EndStack, 
	      Depth, EndDepth, TrackedItpInfo, EndTrackedItpInfo, 
	      Prefix, EndPrefix, Answers, RecPreds, RecCls, Solver, OutStream):-
          %==================================================================%
          % Here we obtain all clauses by failure
          %==================================================================%
	rule(Goal, ClauseId, BodyCl),	
	( is_reusing_answers ->  
	  concat_prefix(Prefix, Goal, ClauseId, NewPrefix) 
	;
	  NewPrefix=Prefix 
          ),
          clause_resolution(Goal-BodyCl, ClauseId, Predec, _GoalX-BodyClX, Next,
 	                  Stack, NewStack, Depth, NewDepth,
	                  TrackedItpInfo, NewTrackedItpInfo,
	                  Prefix, NewPrefix, Answers, RecPreds, RecCls, 
                            Solver, OutStream),

	execute_clause_body(BodyClX, Next, Succ, NewStack, TmpStack, 
	                    NewDepth, NextDepth, 
			NewTrackedItpInfo, NewTrackedItpInfo1, 
			NewPrefix, NewPrefix1, Answers, RecPreds, RecCls, 
                              Solver, OutStream),

	execute_one_sol(Goals, Succ, Last, TmpStack, EndStack, NextDepth, EndDepth,
	                NewTrackedItpInfo1, EndTrackedItpInfo, 
	  	      NewPrefix1, EndPrefix, Answers, RecPreds, RecCls, 
                          Solver, OutStream).
execute_one_sol([Goal|_],_,_,_,_,Depth,_,_,_,_,_,_,_,_,Solver,OutStream):-
	 get_fake_false_ids(IncompleteNodes),
	 % The execution of all the clauses of the predicate has
	 % been completed. After now on, its interpolant can be used
	 % for subsumption, included child-parent subsumption.
           mark_complete_predicate(Goal, Depth, Solver, IncompleteNodes),
	 promote_fake_infeasible_to_actual(Goal, Depth, Solver, OutStream),
	 fail.

% Execution of the clause body 
execute_clause_body(Body, Predec, Last, Stack, EndStack, Depth, EndDepth,
	          TrackedItpInfo, EndTrackedItpInfo,
	          Prefix, EndPrefix, Answers, RecPreds, RecCls, 
                    Solver, OutStream):-	
  	 execute_one_sol(Body, Predec, Last, Stack, EndStack, Depth, EndDepth, 
	                 TrackedItpInfo, EndTrackedItpInfo,
	                 Prefix, EndPrefix, Answers, RecPreds, RecCls,
		       Solver, OutStream).

%%%
% HERE NON-RECURSIVE PREDICATES
%%%

% Perform clause resolution: unification between a clause head and a
% body literal.
clause_resolution(Goal-_BodyCl, ClauseId, Predec,_,_,Stack,_,Depth,_,TrackedItpInfo,_,
	        _Prefix,NewPrefix,Answers,_RecPreds,_RecCls,Solver,OutStream):-
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% SUBSUMPTION CHECK
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          is_enabled_option(clause_subsumption),
	cache_lookup_clause(Goal, ClauseId, Depth, Solver, 
	                    SubsumerPrefix, SubsumerNode, SubsumedIntpInfo),
	!,
	% cut+fail stops the exploration of the rest of the path
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% REUSING ANSWERS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	start_timer(reuse_answer),
	reuse_answers(Goal, Answers, SubsumerPrefix, NewPrefix, Solver),
	stop_timer(reuse_answer,_),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% GENERATION OF INTERPOLANTS
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	debug_message("Generating interpolant from subsumed derivation ...\n",[]),
 
 		
	gen_interpolants(Stack, TrackedItpInfo, Solver, SubsumedIntpInfo),
 		
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% DOT output
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	mark_subsumed_trans(Predec, Goal, ClauseId ,Depth, SubsumerNode, OutStream),
	debug_message("**** SUBSUMED CLAUSE!\n",[]),
	!,
	fail.
clause_resolution(Goal-BodyCl, ClauseId, Predec, GoalX-BodyClX, Next,
	        Stack, NewStack, Depth, NewDepth,
	        TrackedItpInfo, NewTrackedItpInfo,
	        Prefix, NewPrefix,_Answers,RecPreds,RecCls,Solver,OutStream):-
 
       update_goal_stack_and_intp_adt(/*in*/Goal,BodyCl,ClauseId,Context,Depth,
                                      /*in*/Solver, RecPreds, 
		                  GoalX, BodyClX, HeadGoalCs,/*out*/ 
		                  /*in*/Prefix, NewPrefix,/*out*/
			        /*in*/Stack , NewStack ,/*out*/
			        /*in*/TrackedItpInfo,NewTrackedItpInfo/*out*/),
       incr_counter(num_of_nodes,_),
       %-------------------------------------------------------------%
       % We map the current goal to a unique identifier. The
       % identifier is asserted. Note we only map user-defined
       % predicates.
       %-------------------------------------------------------------%
       add_map_node_to_unique_id(Goal, ClauseId, Depth),
       % Note that this predicate will fail if unsatisfiability is
       % detected.
       project_call(HeadGoalCs, GoalX, ClauseId, NewStack, Predec, Depth, 
		NewTrackedItpInfo, RecPreds, RecCls, Solver, OutStream),
       % These predicate only if debugging is on.
       debug_print_matching_constraints(Goal, HeadGoalCs, Solver),
       % These predicates only if dot_output option is enabled.	     
       convert_constraints_for_dot_output(HeadGoalCs, Solver, DotHeadGoalCs),
       mark_feasible_trans(Predec, GoalX, ClauseId, Depth, Context, 
	                 DotHeadGoalCs, OutStream),	
       Next = node(Goal, ClauseId, Depth, Context),
       %%%------------------------------------------------------------%%
       %%% IMPORTANT: perform prolog unification of any argument from
       %%% Goal and GoalX that is not handled by the solver.
       unify_variables_without_attributes(Goal,GoalX),
       %%%------------------------------------------------------------%%
 		
       NewDepth is Depth+1.       
clause_resolution(Goal-_,ClId,_,_,_,_,_,Depth,_,_,_,_,_,_,_,_,Solver, OutStream):- 
       % Once we have executed the clause we can mark it as
       % "complete". This means that its corresponding interpolant (if
       % any) can be used for subsumption, included child-parent
       % subsumption.
       get_map_node_to_unique_id(Goal,ClId,Depth,NodeId),
       mark_complete_clause(NodeId),
       promote_fake_infeasible_to_actual(Goal, ClId, Depth, NodeId, Solver, OutStream),
       cache_gen_disjunctive_interpolants(Goal,ClId, Depth, Solver),
       !, % important cut
       fail.

% Execution of clp (primitive) constraints
execute_clp_constraints(Cs, Cs_Id, Stack, Predec, 
	              node(constraints(Cs), Cs_Id, Depth, Context), 
	              Depth, TrackedItpInfo, NewTrackedItpInfo, 
		    RecPreds, RecCls, Solver, OutStream):- 	                            
        convert_clp_constraints_to_solver(Cs, SolverFormatCs, UserFormatCs),
        solver_push(Solver),
 		
        intp_adt__push(TrackedItpInfo,primitive(SolverFormatCs),NewTrackedItpInfo),
        %%% assert_constraints(SolverFormatCs, Solver, SatFlag, ItpGroups),
        assert_constraints(SolverFormatCs, Solver, SatFlag),
        debug_message("\tAdding constraints: ~q\n",[UserFormatCs]),
        ( SatFlag == tt ->
	  mark_feasible_trans(Predec, constraints(Cs), Cs_Id, Depth, Context, 
	                      '', OutStream)
        ;	    
            debug_message("Detected infeasible execution\n",[]),
	  incr_counter(num_of_infeasible_paths,_),
	  gen_interpolants(Stack, NewTrackedItpInfo, Solver, _NoSubsumedIntpInfo),
	  %%%
	  % This will record if the infeasibility is spuriuos due to
	  % the fake constraints used to force termination or not.
	  %%%
 		
	  ( mark_fake_false(Stack,Cs_Id,RecPreds,RecCls,Solver) ->
	    true
	  ;
	    mark_infeasible_trans(Predec, constraints(Cs), Cs_Id, Depth, 
	                          Context, UserFormatCs, OutStream, infeasible)
	  ),
	  !,
	  fail
        ),
        !.		
% Execution of builtins
execute_builtin(Builtin, Builtin_Id, Module, Predec, 
	      node(Builtin,Builtin_Id,Depth,Context), Depth, OutStream):- 
        ( wrapper_run_builtin(Module,Builtin) ->
 	debug_message("Call to builtin ~q succeed.\n",[Builtin]),
	mark_feasible_trans(Predec, Builtin, Builtin_Id, Depth, Context,
	                    '', OutStream)
        ;
	% Note that here we could generate interpolants if we could
	% reason about Herbrand terms.
	debug_message("Call to builtin ~q failed.\n",[Builtin]),
	mark_infeasible_trans(Predec, Builtin, Builtin_Id, Depth, Context, 
	                      '', OutStream, infeasible),
	!,
	fail
        ),
        !.

%---------------------------------------------------------------------------------%
% project_call(+list,+term,+num,+StackADT,+term,+num,+ItpADT,+list,+list,+Solver,+atom)
%---------------------------------------------------------------------------------%
project_call([], _, _, _, _, _, _, _, _, Solver, _):-
	solver_push(Solver).
project_call(HeadGoalCs, Goal, ClauseId, Stack, Predec, Depth, 
	   TrackedItpInfo, RecPreds, RecCls, Solver, OutStream) :-
          HeadGoalCs = [_|_],
	solver_push(Solver),
	project_call_aux(Goal, HeadGoalCs, ClauseId, Stack, Predec, Depth, 
	                 TrackedItpInfo, RecPreds, RecCls, Solver, OutStream).
project_call(_, _, _, _, _, _, _, _, _, Solver, _) :-
	solver_pop(Solver),
	fail.	                

project_call_aux(Goal, Cs, Clause_Id, Stack, Predec, Depth, 
	       TrackedItpInfo, RecPreds, RecCls, Solver, OutStream):-  
          %%% Cs is a list of CLP constraints but variables are already
          %%% in solver format.       
          assert_constraints(Cs, Solver, SatFlag),
	( SatFlag == tt -> 
	  true
	;	  
            debug_message("Detected infeasible execution.\n",[]),
	  incr_counter(num_of_infeasible_paths,_),
	  gen_interpolants(Stack, TrackedItpInfo, Solver, _NoSubsumedIntpInfo),
	  %%%
	  % This will record if the infeasibility is spuriuos due to
	  % the fake constraints used to force termination or not.
	  %%%
 		
	  ( mark_fake_false(Stack,Clause_Id,RecPreds,RecCls,Solver) -> 
	    true
	  ; 
	    ( is_enabled_option(dot_output) ->
	      convert_clp_constraints_to_solver(Cs, _, UserFormatCs),
	      mark_infeasible_trans(Predec, Goal, Clause_Id, Depth, _Context, 
	                            UserFormatCs, OutStream, infeasible)
	    ; 
	      true
	    )
	  ),
	  !, 
	  fail
          ),
	!.


% code that updates the goal stack and the interpolation adt.
update_goal_stack_and_intp_adt(Goal,BodyCl,ClauseId,Context,Depth,
	                     Solver, RecPreds,
			 Head, BodyClX, NewHeadGoalCs,
			 Prefix,NewPrefix,
	                     Stack,NewStack,
			 TrackedItpInfo,NewTrackedItpInfo):-
       %-------------------------------------------------------------%
       % Push in the stack
       %-------------------------------------------------------------%
       rename_clause(Goal-BodyCl, Solver, _, Head-BodyClX),
       Clause = clause(Head,ClauseId,Depth,Context,Prefix,NewPrefix),
       goal_stack__push(Stack, Clause, NewStack),
       %-------------------------------------------------------------%
       % Add constraints from the goal-head unification. HeadGoalCs
       % is a list of CLP constraints with variables already in
       % solver format solver.
       %-------------------------------------------------------------%
       extract_equalities_from_last_unification(Head,[],_,HeadGoalCs),
       ( is_enabled_option(scoped_interpolation) ->
	 varset_attributes(Goal , NonHeadVs),
	 varset_attributes(Head , HeadVs) 
       ; 
	 true
       ),
       %-------------------------------------------------------------%
       % Add constraint in case the predicate is recursive and we need
       % to force termination
       %-------------------------------------------------------------%
       ( generate_terminating_constraint(Goal, RecPreds, Stack, C) ->
	 NewHeadGoalCs = [ C | HeadGoalCs ]
       ;
	 NewHeadGoalCs = HeadGoalCs
       ),  
       intp_adt__push(TrackedItpInfo, 
		  user_defined(Clause,NewHeadGoalCs,HeadVs-NonHeadVs), 
	            NewTrackedItpInfo),
       !.

% Assert constraints and check satisfiability.
% No interpolation cutpoint is added.
assert_constraints(Cs, Solver, SatFlag):- 
        solver_incremental(Solver, Cs, SatFlag),
        !.

%-----------------------------------------------------------------------%
% rename_clause(+term,+Solver,-term,-term)
% rename_clause(Clause,Solver,Sig,ClauseX)
%-----------------------------------------------------------------------%
% ClauseX is a fresh copy of Clause. Moreover, declare variables of
% ClauseX in the solver.
%-----------------------------------------------------------------------%
rename_clause(Goal-Body, Solver, _Sig, GoalX-BodyX):-
	copy_term(Goal-Body,GndGoal-GndBody),
	copy_term(Goal-Body,GoalX-BodyX),
	get_counter_value(variable_names,V0),
	numbervars(GndGoal-GndBody, V0, V1),
	V2 is V1+1,
	set_counter(variable_names, V2),
	declare_solver_variables([GoalX|BodyX], [GndGoal|GndBody] , Solver),
	debug_print_rule(GoalX,BodyX),
	!.

%------------------------------------------------------------------------%
% unify_variables_without_attributes(?Goal,?GoalX)
%------------------------------------------------------------------------%
% Any variable that is not handled by the solver must be unified via
% Prolog unification.
%------------------------------------------------------------------------%
unify_variables_without_attributes(Goal,GoalX):-
	functor(Goal,F,N),
	functor(GoalX,F,N),
	unify_variables_without_attributes_aux(1,N,F, Goal,GoalX),
	!.
unify_variables_without_attributes_aux(I,N,_,_,_):- 
	I > N, !.
unify_variables_without_attributes_aux(I,N,F,Goal,GoalX):-
	arg(I,Goal ,X),
	arg(I,GoalX,Y),
	% if one of the arguments has an attribute is handled by the
	% underlying solver. Otherwise, we use prolog unification.
	( ( get_attribute(X,'$solver_map'(_,_,_,_,_)) 
	  , 
	    get_attribute(Y,'$solver_map'(_,_,_,_,_))
	  ) -> true
	;
	  X=Y
	),
	NI is I+1,
	unify_variables_without_attributes_aux(NI,N,F,Goal,GoalX).
	
%======================== END METAINTERPRETER ============================

%============== BEGIN DECLARATION OF VARIABLES IN THE SOLVER =============

%-------------------------------------------------------------------------%
% We map each variable that appears in the head of a clause or a goal
% literal to a solver variable and keep the mapping using the
% technique of attribute variables.
%
% Currently, we only support real or integer arithmetic. However, we
% don't allow to mix integer with real arithmetic.
%-------------------------------------------------------------------------%
declare_solver_variables([], [], _Solver):- !.
declare_solver_variables([constraints(Cs,K)|Bs], 
	               [constraints(GndCs,K)|GndBs], Solver):-
        ( is_enabled_option(integer_arithmetic) -> Ty=int ; Ty=real),
	declare_vars_from_constraints(Cs,GndCs,Ty,Solver),
	declare_solver_variables(Bs, GndBs, Solver).
declare_solver_variables([B|Bs], [GndB|GndBs], Solver):-
	declare_vars_from_functor(B, GndB, Solver),
	declare_solver_variables(Bs, GndBs, Solver).

declare_vars_from_functor(Goal, GndGoal, Solver):-
	functor(Goal,F,N),
	functor(GndGoal,F,N),
	declare_vars_from_functor_arg(1, N, Goal, F, Solver, GndGoal).
declare_vars_from_functor_arg(I, N, _, _, _,_):-  I > N, !.
declare_vars_from_functor_arg(I, N, Goal, F, Solver, GndGoal):- 
	arg(I,Goal,X),
	arg(I,GndGoal,Y),	
	( (var(X), solver_valid_type(F/N,I,Ty)) ->
	   % Here we ask user for the type. 
	   declare_var(X,Y,Ty,Solver)
	;
	   % Here user said to ignore the argument 
	   true
	),	
	NI is I + 1,
	declare_vars_from_functor_arg(NI, N, Goal, F, Solver, GndGoal).

declare_var(V, '$VAR'(N), Ty, Solver):-
	%%%%%%%%%
	% If we create atoms at front we will run out of atoms and get a Ciao
	% error. However, with integers there is no limit.
	%%%%%%%%%
	% atom_number(N_atm,N),
	% atom_concat('v_',N_atm,A),	
	%---------------------------------------------------------------%
	% Here we declare solver variable
	solver_declare_variable(Solver, N, VarRef, Ty),
	% Here we store the map
	attach_attribute(Fresh, '$solver_map'(Fresh, _A, N, VarRef, [] )),
	V = Fresh.
	%---------------------------------------------------------------%

% RULES to decide the types of the variables:
%
% - If the argument is typed with 'num' the type will be real, unless
%   '-integer-arithmetic' is enabled in which case it will be int.
% - If the argument is typed with 'int' the type will be always int.
% - If the argument is typed with 'real' the type will be real, unless
%   '-integer-arithmetic' is enabled in which case it will be int.
solver_valid_type(F/N,I,int):- 
	get_predicate_arg_type(F/N,I,int), 
	!.
solver_valid_type(F/N,I,Ty):- 
	get_predicate_arg_type(F/N,I,real), 
	!,
	( is_enabled_option(integer_arithmetic) -> 
	  Ty=int 
	; 
	  Ty=real
	).

solver_valid_type(F/N,I,Ty):- 
	get_predicate_arg_type(F/N,I,num), 
	!,
	( is_enabled_option(integer_arithmetic) -> 
	  Ty=int 
	; 
	  Ty=real
	).

% Even with type information we need to go inside the constraints to
% declare all variables because there may be existentially quantified
% variables that do not appear in any clause head or literal goal.
declare_vars_from_constraints([],[],_,_).
declare_vars_from_constraints([C|Cs], [GndC|GndCs], Ty, Solver):-
	declare_vars_from_constraint(C, GndC, Ty, Solver),
	declare_vars_from_constraints(Cs, GndCs, Ty, Solver).

declare_vars_from_constraint('.=.'(A,B), '.=.'(GndA,GndB), Ty, Solver):-
	declare_var_from_expr(A, GndA, Ty, Solver),
	declare_var_from_expr(B, GndB, Ty, Solver).
declare_vars_from_constraint('.<>.'(A,B), '.<>.'(GndA,GndB), Ty, Solver ):-
	declare_var_from_expr(A, GndA, Ty, Solver),
	declare_var_from_expr(B, GndB, Ty, Solver).

declare_var_from_expr(V, GndV, Ty, Solver):-
	var(V), !, declare_var(V, GndV, Ty, Solver).
declare_var_from_expr(A, _,_,_):- atomic(A), !.
declare_var_from_expr('.>.'(A,B), '.>.'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).
declare_var_from_expr('.>=.'(A,B), '.>=.'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).
declare_var_from_expr('.<.'(A,B), '.<.'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).
declare_var_from_expr('.=<.'(A,B), '.=<.'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).
declare_var_from_expr('+'(A,B), '+'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).
declare_var_from_expr('-'(A), '-'(GndA), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver).
declare_var_from_expr('-'(A,B), '-'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).
declare_var_from_expr('*'(A,B), '*'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).
declare_var_from_expr('/'(A,B), '/'(GndA,GndB), Ty, Solver):-	
	declare_var_from_expr(A, GndA, Ty,Solver),
	declare_var_from_expr(B, GndB, Ty,Solver).

%================= END DECLARATION OF VARIABLES IN THE SOLVER ===========%
		
%========================================================================%
%                        Auxiliary predicates
%========================================================================%

% Check whether we are using a supported solver. If yes call
% initialization of the solver.
initialize_solver(mathsat, smt(Solver)):- 
	solver_init(mathsat, smt(Solver)), 
	!.
% initialize_solver(z3     , smt(Solver)):- 
% 	solver_init(z3, smt(Solver)), 
% 	!.
initialize_solver(Solver,  _):- 
	format("ERROR ftclp: unknown solver ~q\n",[Solver]),
	halt.
    
% If asserted then the search is incomplete even if the interpreter
% successfully finished.
:- data '$incomplete_search'/0.

% Stop the execution if a certain depth is hit.
:- data '$maximum_depth'/1.
record_maximum_depth(0):- !. % 0 means no bound
record_maximum_depth(Depth):-
	asserta_fact('$maximum_depth'(Depth)),
	!.
reached_maximum_depth(Depth):-
	current_fact('$maximum_depth'(MaxDepth)),
	Depth > MaxDepth,
	!.

%-----------------------------------------------------------------------%
%             Pseudo-argname for encapsulate the call stack
%-----------------------------------------------------------------------%
% goal_stack__init(-Stack)
goal_stack__init([]).

% goal_stack__push(+Stack,+Elem,-NewStack)
goal_stack__push(Stack,X,[X|Stack]).	

% goal_stack__is_member(+Stack,+Goal)
goal_stack__is_member(Stack, Goal):-
	\+(\+( member(clause(Goal,_ClId,_,_,_,_), Stack))).

% goal_stack__is_variant_member(+Stack,+Functor,+Arity)
goal_stack__is_variant_member([clause(Goal,_,_,_,_,_)|_], F, A):-
        functor(Goal, F, A), 
        !.
goal_stack__is_variant_member([_|Cls], F, A):-
        goal_stack__is_variant_member(Cls, F, A).

% goal_stack__previous_clause(+Stack,-Cl,-RestStack)
goal_stack__previous_clause([clause(Goal,Id,Depth,ContextId,Prefix,NewPrefix)|RestStack],
	                   clause(Goal,Id,Depth,ContextId,Prefix,NewPrefix),RestStack):- 
        !.
goal_stack__previous_clause([_|Stack], Cl, RestStack):-
	goal_stack__previous_clause(Stack, Cl, RestStack).	

% goal_stack__print_only_functors(+Stack)
goal_stack__print_only_functors(S):- 
	goal_stack__print_only_functors_aux(S,[],AtomList),
	atom_concat_with_separator(AtomList,Atom),
	format("~q\n",[Atom]),
	!.

goal_stack__print_only_functors_aux([],Acc,Acc).
goal_stack__print_only_functors_aux([clause(Goal,Id,_,_,_,_)|Ss], Xs, Ys):-
	create_str_key(Goal,Id,Key),
	atom_codes(AKey, Key),
	goal_stack__print_only_functors_aux(Ss, [AKey|Xs], Ys).
goal_stack__print_only_functors_aux([_|Ss], Xs, Ys):-
	goal_stack__print_only_functors_aux(Ss, Xs, Ys).

% goal_stack__print(+Stack)
goal_stack__print(S):- 
	format("{",[]), 
	goal_stack__print_aux(S),
	format("}\n",[]).  
goal_stack__print_aux([]).
goal_stack__print_aux([constraints(Cs,_,_Id)-_Depth|Ss]):-
	format("~q ;",[Cs]),
	goal_stack__print_aux(Ss).
goal_stack__print_aux([builtin(Goal,_Id)-_Depth|Ss]):-
	format("~q ;",[Goal]),
	goal_stack__print_aux(Ss).
goal_stack__print_aux([return(_,_,_)-_-_|Ss]):-
	goal_stack__print_aux(Ss).
goal_stack__print_aux([clause(Goal,Id,_Depth,_ContextId,_Prefix,_NewPrefix)|Ss]):-
	format("~q_clid=~q ;",[Goal,Id]),
	goal_stack__print_aux(Ss).

%-----------------------------------------------------------------------%
% add_map_node_to_unique_id(+term, +num, +num)
%-----------------------------------------------------------------------%
% Map a node in the tree to a unique identifier.
%-----------------------------------------------------------------------%
:- data '$tracked_node'/4.
add_map_node_to_unique_id(Goal, K, Depth):-
	functor(Goal,F,A),
	incr_counter('$node_to_unique_id', Id),
	debug_message("Asserting mapping for ~q/~q/~q::~q\n", [ F,A,K,Depth]),
	asserta_fact('$tracked_node'(F/A,K,Depth,Id)),
	!.
%-----------------------------------------------------------------------%
% get_map_node_to_unique_id(+term, +num, +num, -num)
%-----------------------------------------------------------------------%
% Get the unique identifier associated with the tree node.
%-----------------------------------------------------------------------%
get_map_node_to_unique_id(Goal, K, Depth, Id):-
        \+var(Goal), \+var(K), \+var(Depth),
        functor(Goal,F,A),
        current_fact('$tracked_node'(F/A,K,Depth,Id)),
        !.
% get_map_node_to_unique_id(-term, -num, -num, +num)
get_map_node_to_unique_id(Goal, K, Depth, Id):-
        var(Goal), var(K), var(Depth), \+var(Id),
        current_fact('$tracked_node'(F/A,K,Depth,Id)),
        functor(Goal, F, A),
        !.

%%------------------------------------------------------------------------%%
%%                         Dot output 
%%------------------------------------------------------------------------%%

% convert_constraints_for_dot_output(+,+,-)
% Cs is a list of constraints where variables are in Solver format.
convert_constraints_for_dot_output(Cs, Solver, NewCs):-
	is_enabled_option(dot_output), !,
	replace_solver_vars_with_str(Cs, Solver , NewCs).
convert_constraints_for_dot_output(_,_,_):-  !.

% Record transitions using dot
% All arguments are input except Context
mark_feasible_trans(Predec, Goal, ClauseId, Depth, Context, 
	          LabelForEdge, OutStream):-
	is_enabled_option(dot_output),
	!,
	gen_label(Predec,From),
	map_node_to_context(Goal, ClauseId, Depth, Context),
	gen_label(node(Goal,ClauseId, Depth, Context),To),
	copy_term_with_attributes(Goal,Sig),	
	( get_map_node_to_unique_id(Goal,ClauseId,Depth,NodeId) ->
	  atom_number(ANodeId  ,NodeId),
	  atom_number(AClauseId,ClauseId),
	  atom_concat(['MemoId:',ANodeId], PreLabel),
	  atom_concat(['Clause:',AClauseId], PostLabel),
	  LabelForNode = PreLabel-Sig-PostLabel
	;
	  LabelForNode = Sig-ClauseId   
	),
	write_dotfile_trans(feasible , From, To, LabelForNode, 
	                     LabelForEdge, OutStream).
mark_feasible_trans(_,_,_,_,_,_,_):- 
	!.
% All arguments are input except Context
mark_infeasible_trans(Predec, Goal, ClauseId, Depth, Context, 
	            LabelForEdge, OutStream, TypeOfInfeas):-
	%% Here we do not need to call replace_solver_vars_with_str
	%% because LabelForEdge has already that form. Here it is ok
	%% but it was obtained without extra cost.
	is_enabled_option(dot_output),
	!,
	(TypeOfInfeas == fake_infeasible ; TypeOfInfeas == infeasible),
	gen_label(Predec,From),
	map_node_to_context(Goal,ClauseId, Depth, Context),
	gen_label(node(Goal,ClauseId, Depth, Context),To),
	copy_term_with_attributes(Goal,Sig),
	write_dotfile_trans(TypeOfInfeas , From, To, Sig, LabelForEdge, 
	                    OutStream).
mark_infeasible_trans(_,_,_,_,_,_,_,_):-
	!.

:- push_prolog_flag(multi_arity_warnings,off).
% All arguments are input except Context
mark_subsumed_trans(Predec, Goal, ClauseId, Depth, Subsumer, OutStream):-
	is_enabled_option(dot_output),
	!,
	mark_feasible_trans(Predec, Goal, ClauseId, Depth, Context, 
	                    '', OutStream),
	Subsumed = node(Goal,ClauseId, Depth, Context),
	gen_label(Subsumed, From),
	gen_label(Subsumer, To),
	write_dotfile_trans(subsumed , From, To, '', '', OutStream).
mark_subsumed_trans(_,_,_,_,_,_):- 
	!.
% Subsumed=Subsumer=node(Goal,ClauseId,Depth,Context)
mark_subsumed_trans(Subsumed, Subsumer, OutStream):-
	is_enabled_option(dot_output),
	!,
	gen_label(Subsumed, From),
	gen_label(Subsumer, To),
	write_dotfile_trans(subsumed , From, To, '', '', OutStream).
mark_subsumed_trans(_,_,_):-
	!.
:- pop_prolog_flag(multi_arity_warnings).
         
map_node_to_context(Goal, K, Depth, Id):-
	functor(Goal,F,A),
	atom_number(K_atm,K),
	atom_number(A_atm,A),
	atom_number(D_atm,Depth),	
	atom_concat([F,'/',A_atm,'/',K_atm,'#',D_atm],ContextCnt),
	incr_counter(ContextCnt,Id).


%%---------------------------------------------------------------------%%
%%                            Debugging info
%%---------------------------------------------------------------------%%
%% print_term_with_attribute/1 display the term but replacing logical
%% variable with one of its attributes. Check definition of
%% print_term_with_attribute/1 to see which one.

debug_print_rule(Head,Body):-
	is_enabled_option(debug),	
	!,
	print_term_with_attribute(Head),
	format(" :- ",[]),
	debug_print_body(Body),
        format("\n",[]).
debug_print_rule(_,_):-!.

debug_print_body(B):-
	is_enabled_option(debug),!,	
	print_body(B).
debug_print_body(_):- !.
	
print_constraints([]).
print_constraints([C]):-
	 print_term_with_attribute(C).
print_constraints([C|Cs]):-
	 print_term_with_attribute(C),
	 format(", ",[]),
	 print_constraints(Cs).
	
print_body([]):- !.
print_body([L]):-
	( L = constraints(Cs,_) ->
	  print_constraints(Cs)
	;
	  print_term_with_attribute(L)
	),
	!,
	format(".",[]).
print_body([L|Ls]):-
	( L = constraints(Cs,_) ->
	  print_constraints(Cs)
	;
	  print_term_with_attribute(L)
	),
	format(", ",[]),
	print_body(Ls).

debug_print_matching_constraints(Goal, HeadGoalCs, Solver):-
	is_enabled_option(debug),	
	!,
	functor(Goal,F,A),
	replace_solver_vars_with_str(HeadGoalCs,Solver,UserFriendHeadGoalCs),
	format("Matching with a clause of ~q\n\tAdding ~q \n",
	       [ F/A, UserFriendHeadGoalCs]).
debug_print_matching_constraints(_,_,_):-
	!.

 		

 

