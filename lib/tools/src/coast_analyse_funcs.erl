%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%%*********************************************************************
%%%
%%%   DEFINITIONS
%%%   ===========
%%%  
%%%   In this program we count, for modules, functions, and function clauses,
%%%   the following:  - the total number of calls, denoted TotN
%%%                   - the number of calls from another module, denoted ExtN
%%%                   - the number of *internal* calls that are *non-recursive*,
%%%                     denoted IntNonRecN
%%%  
%%%  
%%%   These three quantities mean different things, depending on whether we 
%%%   talk about modules, functions or function clauses. Let us define it as 
%%%   follows:
%%%  
%%%   For modules:
%%%   ------------
%%%   - By the total number of calls we mean the number of times a specific
%%%     module (i.e., the functions in the module) have been called, both from 
%%%     other modules (i.e., from functions residing in other modules) and from 
%%%     the same module (i.e., from functions internal to the module).
%%%  
%%%   - The term "number of calls from another module" denotes the number of
%%%     times a module (i.e., the functions in the module) has been called from 
%%%     other modules (i.e., from functions residing in other modules).
%%%  
%%%   - A recursive module call means here that a module calls itself (i.e., 
%%%     a function in the module calls a function (the same function or another) 
%%%     in the very same module)).
%%%     By definition, all such calls have to be internal. Consequently, the 
%%%     number of "internal non-recursive calls" always equals to 0 (zero), since
%%%     all internal calls, according to the definition, also are recursive.  :-/
%%%  
%%%   The following relation holds true: 
%%%   number of internal *recursive* calls = TotN - ExtN
%%%  
%%%  
%%%   For functions:
%%%   --------------
%%%   - By the total number of calls we mean the number of times a specific 
%%%     function has been called (including the number of times the function
%%%     calls itself as well as function calls from the same and other modules).
%%%  
%%%   - "Number of calls from another module" denotes the number of times a
%%%     specific function has been called from functions in other modules.
%%%  
%%%   - Internal non-recursive calls means the number of times the function in
%%%     question has been called from functions, other than itself, in the very
%%%     same module.
%%%  
%%%   The following relation holds true: 
%%%   number of internal *recursive* calls = TotN - ExtN - IntNonRecN
%%%  
%%%  
%%%   For function clauses:
%%%   ---------------------
%%%   - By the total number of calls we mean the number of times a specific 
%%%     function clause has been called (including the number of times the 
%%%     function clause calls itself as well as function calls from all other 
%%%     function clauses in the same and other modules).
%%%  
%%%   - "Number of calls from another module" denotes the number of times a
%%%     specific function clause has been called from functions in other modules.
%%%  
%%%   - Internal non-recursive calls means the number of times the function in
%%%     question has been called from functions, other than itself, in the very
%%%     same module.
%%%  
%%%   The following relation holds true: 
%%%   number of internal *recursive* calls = TotN - ExtN - IntNonRecN
%%%  
%%%
%%%*********************************************************************



-module(coast_analyse_funcs).



-export([mod_calls/1, 
	 func_calls/1, 
	 clause_calls/1, 
	 mod_coverage/1, 
	 func_coverage/1, 
	 clause_coverage/1,
	 analyse_to_file/1,
	 clear_module/1,
	 get_known_modules/1
	]).



-include("coast_server.hrl").





%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   Returns a sorted list of the number of times the available 
%%                functions have been called.
%%
%% Parameters:    
%%======================================================================


%% {{Module, Func, Arity, Index, Clause, ClauseLine, CodeLine}, 
%%  TotN, ExtModN, IntNonRecN}




mod_calls(Modules) ->
    {function_calls, FuncList} = func_calls(Modules),
    mod_calls_int(FuncList, []).




func_calls(Modules) ->
    {clause_calls, ClauseList} = clause_calls(Modules),
    func_calls_int(ClauseList, []).




clause_calls(Modules) ->
    clause_calls_int(Modules, []).




mod_coverage(Modules) ->
    {function_coverage, FuncList} = func_coverage(Modules),
    mod_coverage_int(FuncList, []).




func_coverage(Modules) ->
    {clause_coverage, ClauseList} = clause_coverage(Modules),
    func_coverage_int(ClauseList, []).




clause_coverage(Modules) ->
    clause_coverage_int(Modules, []).





analyse_to_file(Modules) ->
    case ok_to_analyse(Modules) of
	ok ->
	    analyse_to_file2(Modules, []);
	Other ->
	    Other
    end.






clear_module([Mod | T]) ->
    coast_comm_funcs:remove_from_database(Mod),
    clear_module(T);
clear_module([]) ->
    ok.






get_known_modules(CompiledModules) ->
    AllModules = 
	CompiledModules ++ 
	lists:flatten(ets:match(?TABLE_NAME, 
				{{'$1', '_', '_', '_', '_',  '_', '_'},
				 '_', '_', '_'})),
    uniq(lists:sort(AllModules)).
    



    



%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************




analyse_loop(InFd, OutFd) ->
    analyse_loop2(epp:parse_erl_form(InFd), InFd, OutFd).





analyse_loop2({ok,Form}, InFd, OutFd) ->
    {Flag, Form1} = merge_bump_data(Form, false),
    case Flag of
        true ->
            io:format(OutFd, "~s~n", [erl_pp:form(Form1)]);
        false ->
            done
    end,
    analyse_loop(InFd, OutFd);
analyse_loop2({eof, _}, InFd, OutFd) ->
    epp:close(InFd),
    file:close(OutFd),
    ok;
analyse_loop2({error, {Line, Mod, Reason}}, InFd, OutFd) ->
    {error, {Line, Mod, Reason}}.





mod_calls_int([{{M,_,_},TN1,EM1,INR1}, {{M,F,A},TN2,EM2,INR2} | T], Acc) ->
    mod_calls_int([{{M,F,A},TN1 + TN2,EM1 + EM2,0} | T], Acc);
mod_calls_int([{{M,_,_},TN,EM,INR} | T], Acc) ->
    mod_calls_int(T, [{M,TN,EM,INR} | Acc]);
mod_calls_int([], Acc) ->
    {module_calls, lists:keysort(1, Acc)}.






func_calls_int([{{M,F,A,C1},TN1,EM1,INR1}, {{M,F,A,C2},TN2,EM2,INR2} | T], Acc) ->
    func_calls_int([{{M,F,A,C2},TN1 + TN2,EM1 + EM2,INR1 + INR2} | T], Acc);
func_calls_int([{{M,F,A,_},TN,EM,INR} | T], Acc) ->
    func_calls_int(T, [{{M,F,A},TN,EM,INR} | Acc]);
func_calls_int([], Acc) ->
    {function_calls, lists:keysort(1, Acc)}.




clause_calls_int([Module | T], Acc) ->
    L     = get_call_data_list(Module),
    L1    = merge_clause_calls(lists:sort(L), []),
    clause_calls_int(T, Acc ++ L1);
clause_calls_int([], Acc) ->
    {clause_calls, lists:keysort(1, Acc)}.



mod_coverage_int([{{M, _, _}, Cov1, NotCov1}, 
	      {{M, F, A}, Cov2, NotCov2} | T], Acc) ->
    mod_coverage_int([{{M, F, A}, Cov1 + Cov2, NotCov1 + NotCov2} | T], Acc);
mod_coverage_int([{{M, _, _}, Cov, NotCov} | T], Acc) ->
    mod_coverage_int(T, [{M, Cov, NotCov} | Acc]);
mod_coverage_int([], Acc) ->
    {module_coverage, lists:keysort(1, Acc)}.





func_coverage_int([{{M, F, A, C1}, Cov1, NotCov1}, 
	       {{M, F, A, C2}, Cov2, NotCov2} | T], Acc) ->
    func_coverage_int([{{M, F, A, C2}, Cov1 + Cov2, NotCov1 + NotCov2} | T], Acc);
func_coverage_int([{{M, F, A, C}, Cov, NotCov} | T], Acc) ->
    func_coverage_int(T, [{{M, F, A}, Cov, NotCov} | Acc]);
func_coverage_int([], Acc) ->
    {function_coverage, lists:keysort(1, Acc)}.



clause_coverage_int([Module | T], Acc) ->
    L     = get_call_data_list(Module),
    L1    = merge_clause_coverage(lists:sort(L), 0, 0, []),
    clause_coverage_int(T, Acc ++ L1);
clause_coverage_int([], Acc) ->
    {clause_coverage, lists:keysort(1, Acc)}.





get_call_data_list(Module) ->
       % Check if the table has been 'initialised' for the current module!
    case ets:lookup(?TABLE_NAME, {module_initialised, Module}) of
	[{{module_initialised, Module}, true}] ->
	    ok;
	_Other ->
	    case catch initialise_table(apply(Module, 
					      'coast_init_info_$$_private', [])) 
		of
		{'EXIT', {undef, {Module, 'coast_init_info_$$_private', []}}} ->
		    exit({not_coast_compiled, Module});
		Other ->
		    done
	    end,
	    ets:insert(?TABLE_NAME, {{module_initialised, Module}, true})
    end,
       %% We want a list of lists, where each sublist has the format 
       %% [Module, Function, Arity, Clause, StatementIndex, 
       %%  TotalNumberOfTimesCalled, NumberOfTimesCalledFromOtherModules, 
       %%  NumberOfTimesCalledNonRecursivelyFromTheSameModule
       %% ]
       %% (The StatementIndex shall not be confused with the LineNumber!!!)
       %% The reason for keeping the StatementIndex is that we want to keep
       %% the original relative order of statements, since the first
       %% bump-call for each clause tells how many times that specific clause 
       %% has been called. The other bump-calls for that clause may be uncertain, 
       %% since, e.g., there may be statements that never are entered! 
    L = ets:match(?TABLE_NAME, {{Module, '$2','$3','$5', '$4', '_', '_'}, 
				'$6', 
				'$7', 
				'$8'
			       }),
    lists:map(fun(DataList) ->
		      [Module] ++ DataList
	      end, L).






initialise_table([Index | T]) ->
    case ets:lookup(?TABLE_NAME, Index) of
	[] ->
	    ets:insert(?TABLE_NAME, {Index, 0, 0, 0});
	_Other ->
	    done
    end,
    initialise_table(T);
initialise_table([]) ->
    done.
    
    




%% The indices in the source code file will be in rising order,
%% meaning that we obtain the same order after the previous sorting.
%% This means that the first data list for a function clause will correspond
%% to the first executable line in that clause!


merge_clause_calls([[M,F,A,C,I,TotN,ExtModN,IntNonRecN], [M,F,A,C,_,_,_,_] | T], Acc) ->
    merge_clause_calls([[M,F,A,C,I,TotN,ExtModN,IntNonRecN] | T], Acc);
merge_clause_calls([[M,F,A,C,_,TotN,ExtModN,IntNonRecN] | T], Acc) ->
    merge_clause_calls(T, [{{M,F,A,C}, TotN, ExtModN, IntNonRecN} | Acc]);
merge_clause_calls([], Acc) ->
    Acc.





merge_clause_coverage([[M,F,A,C,_,0,_,_], [M,F,A,C,I,TotN2,EM,INR] | T], CovN, NotCovN, Acc) ->
       %% Here we get statistics from each clause.
    merge_clause_coverage([[M,F,A,C,I,TotN2,EM,INR] | T], 
			  CovN, NotCovN + 1, Acc);
merge_clause_coverage([[M,F,A,C,_,TotN1,_,_], [M,F,A,C,I,TotN2,EM,INR] | T], CovN, NotCovN, Acc) ->
       % Here we get statistics from each clause.
    merge_clause_coverage([[M,F,A,C,I,TotN2,EM,INR] | T], 
			  CovN + 1, NotCovN, Acc);
merge_clause_coverage([[M,F,A,C,I,0,EM,INR] | T], CovN, NotCovN, Acc) ->
    merge_clause_coverage(T, 0, 0, 
			  [{{M,F,A,C}, CovN, NotCovN + 1} | Acc]);
merge_clause_coverage([[M,F,A,C,I,TotN,EM,INR] | T], CovN, NotCovN, Acc) ->
    merge_clause_coverage(T, 0, 0, 
			  [{{M,F,A,C}, CovN + 1, NotCovN} | Acc]);
merge_clause_coverage([],_,_,Acc) ->
    Acc.








ok_to_analyse([Module | T]) ->
    case catch apply(Module, 'coast_init_info_$$_private', []) of
	{'EXIT', Reason} ->
	    {error, {not_coast_compiled, Module}};
	{error, Reason} ->
	    {error, Reason};
	Other ->
	    ModuleInfo  = Module:module_info(attributes),
	    ObjDir = element(2, 
			     element(2, 
				     lists:keysearch(coast_obj_dir, 
						     1, 
						     ModuleInfo))),
	    CoastSrcDir = filename:join(ObjDir, ?COAST_SRC_DIR),
	    FileName = filename:join(CoastSrcDir, 
				     atom_to_list(Module) ++ ?COAST_SOURCE_SUFFIX),
	    case file:open(FileName, read) of
		{error, Reason} ->
		    {error, {cannot_find_file, FileName}};
		{ok, _Fd} ->
		    ok_to_analyse(T)
	    end
    end;
ok_to_analyse([]) ->
    ok.




analyse_to_file2([Module | T], Acc) ->
    case catch analyse_one_module(Module) of
	{error, Reason} ->
	    {error, Reason};
	{'EXIT', Reason} ->
	    {error, Reason};
	{ok, FileName} ->
	    analyse_to_file2(T, [FileName | Acc])
    end;
analyse_to_file2([], Acc) ->
    {ok, lists:reverse(Acc)}.
    
					     



analyse_one_module(Mod) ->
       % Check if the table has been 'initialised' for the current module!
    case ets:lookup(?TABLE_NAME, {module_initialised, Mod}) of
	{{module_initialised, Mod}, true} ->
	    ok;
	_Other ->
	    case catch initialise_table(
			 apply(Mod,'coast_init_info_$$_private',[])) of
		{'EXIT', {undef,{Mod,'coast_init_info_$$_private',[]}}} ->
		    exit({not_coast_compiled,Mod});
		Other ->
		    done
	    end,
	    ets:insert(?TABLE_NAME, {{module_initialised, Mod}, true})
    end,
    ModuleInfo  = Mod:module_info(attributes),
    ObjDir = element(2, 
		     element(2, 
			     lists:keysearch(coast_obj_dir, 
					     1, 
					     ModuleInfo))),
    CoastSrcDir = filename:join(ObjDir, ?COAST_SRC_DIR),
    ModStr      = atom_to_list(Mod),
    InFileName  = filename:join(CoastSrcDir, ModStr ++ ?COAST_PRETTY_SUFFIX),
    OutFileName = filename:join(filename:join(ObjDir, ?COAST_DIR),
				ModStr ++ ?COAST_ANALYSE_SUFFIX),
    {ok, InFd}  = file:open(InFileName, read),
    {ok, OutFd} = file:open(OutFileName, write),
    io:format(OutFd, "~n", []),

    [SrcFile] = coast_comm_funcs:get_source_files([Mod]),
    io:format(OutFd, "   SOURCE FILE:  ~s~n~n~n~n", [SrcFile]),
    print_call_statistics(OutFd, ?MODULE:func_calls([Mod])),
    print_func_cover_data(OutFd, ?MODULE:func_coverage([Mod])),

    io:format(OutFd, 
	      "   NUMBER OF TIMES EACH~n"
	      "   LINE HAS BEEN EXECUTED:~n", []),
    print_separator(OutFd),
    case lists:keysearch(vsn, 1, ModuleInfo) of
	false ->
	    done;
	{value, {vsn, Version}} ->
	    io:format(OutFd, "   Version:      ~p~n~n~n~n~n", [Version])
    end,

    case ets:match(?TABLE_NAME, {{Mod,'_','_',0,'_','_','$1'}, '_', '_', '_'}) of
	[] ->
	       %% This is the pathological case of a source code file 
	       %% with no functions...
	    done;
	[[FirstFcnLine]] -> 
	    ets:insert(?TABLE_NAME, {{first_code_line, Mod}, FirstFcnLine}),
	    MaxNofCalls = hd(lists:reverse(
			       lists:sort(
				 lists:flatten(
				   ets:match(?TABLE_NAME, 
					     {{'_','_','_','_','_','_','_'}, 
					      '$1', '_',  '_'}
					    ))))),
	    MaxNofCallsStrLength = length(integer_to_list(MaxNofCalls)),
	    scan_to_line(FirstFcnLine, 1, InFd),
	    read_file(InFd, io:get_line(InFd, ''), FirstFcnLine, OutFd, Mod, undefined, 
		      undefined, MaxNofCallsStrLength)
    end,
    file:close(InFd),
    file:close(OutFd),
    {ok, OutFileName}.

    




max(Curr, Max) when Curr > Max ->
    Curr;
max(_Curr, Max) ->
    Max.


    
    

print_generic(Fd, MapFun, LenFun, LenFunStartTuple, 
	      ForeachFun, IoStrFun, HeaderIoArgs, DataList) ->
    FcdL2    = lists:map(MapFun, DataList),
    MaxTuple = lists:foldl(LenFun, LenFunStartTuple, FcdL2),
    IoStr    = IoStrFun(MaxTuple),
    print_separator(Fd),
    io:format(Fd, IoStr ++ "~n", HeaderIoArgs),
    lists:foreach(ForeachFun(IoStr, Fd), FcdL2),
    print_separator(Fd).
    
			   
    
    


print_call_statistics(_OutFd, {_,[]}) ->
    done;
print_call_statistics(OutFd, {_,FcdL}) ->
    MapFun = 
	fun({{M,F,A},Tot,Ext,NonRec}) ->
		{atom_to_list(M)++":"++atom_to_list(F)++"/"++integer_to_list(A),
		 integer_to_list(Tot)++"  ", 
		 integer_to_list(Ext)++"   ",
		 integer_to_list(NonRec)++"           "}
	end,
    LenFun = fun({S,T,E,NR}, {M1,M2,M3,M4}) ->
		     {max(length(S),M1), max(length(T),M2), max(length(E),M3), 
		      max(length(NR),M4)
		     }
	     end,
    LenFunStartTuple = {length("Function"),length("Total"),length("External"),
			length("Internal non-recursive")
		       }, 
    IoStrFun = fun({M1,M2,M3,M4}) ->
		       "   ~-" ++ integer_to_list(M1) ++ "s" ++ "     ~" ++ 
			   integer_to_list(M2) ++ "s     ~" ++ 
			   integer_to_list(M3) ++ "s    ~" ++ 
			   integer_to_list(M4) ++ "s~n"
	       end,
    HeaderIoArgs = ["Function","Total","External","Internal non-recursive"],
    ForeachFun = fun(IoStr, Fd) ->
			 fun({MFA,Tot,Ext,NonRec}) ->
				 io:format(Fd, 
					   IoStr,
					   [MFA,Tot,Ext,NonRec]);
			    (X) ->
				 done
			 end
		 end,

    io:format(OutFd, "   NUMBER OF CALLS TO EACH FUNCTION:~n", []),
    print_generic(OutFd, MapFun, LenFun, LenFunStartTuple, ForeachFun, 
		  IoStrFun, HeaderIoArgs, FcdL),
   
    io:format(OutFd, "~n~n~n", []).



print_func_cover_data(_OutFd, {_,[]}) ->
    done;
print_func_cover_data(OutFd, {_,FcdL}) ->
    MapFun = 
	fun({{M,F,A},Cov,UnCov}) ->
		{atom_to_list(M)++":"++atom_to_list(F)++"/"++integer_to_list(A),
		 integer_to_list(Cov) ++ "     ", 
		 integer_to_list(UnCov) ++ "     "}
	end,
    LenFun = 
	fun({S,C,UC}, {M1,M2,M3}) ->
		{max(length(S),M1), max(length(C),M2), max(length(UC),M3)}
	end,
    LenFunStartTuple = {length("Function"),
			length("Covered lines"),
			length("Uncovered lines")
		       },
    IoStrFun = 
	fun({M1,M2,M3}) ->
		"   ~-" ++ integer_to_list(M1) ++ "s" ++ "        ~" ++ 
		    integer_to_list(M2) ++ "s        ~" ++ integer_to_list(M3) ++ 
		    "s~n"
	end,
    ForeachFun = 
	fun(IoStr, Fd) ->
		fun({MFA,Cov,UnCov}) ->
			io:format(Fd, IoStr, [MFA,Cov,UnCov]);
		   (X) ->
			done
		end
	end,
    HeaderIoArgs = ["Function", "Covered lines", "Uncovered lines"],
    io:format(OutFd, "   COVERAGE DATA:~n", []),
    print_generic(OutFd, MapFun, LenFun, LenFunStartTuple, ForeachFun, 
		  IoStrFun, HeaderIoArgs, FcdL),
    io:format(OutFd, "~n~n~n~n", []).

    




print_separator(OutFd) ->
        io:format(OutFd, 
		  "   ********************************************"
		  "********************************~n", []).
    


scan_to_line(NofLines, CurrentLine, InFd) when CurrentLine < NofLines ->
    io:get_line(InFd, ''),
    scan_to_line(NofLines, CurrentLine + 1, InFd);
scan_to_line(_NofLines, _CurrentLine, _InFd) ->
    ok.

     



read_file(InFd, eof, LineNo, OutFd, Mod, Func, Arity, MaxLengthN) ->
    io:format(OutFd, "~n", []);
read_file(InFd, Line, LineNo, OutFd, Mod, Func, Arity, MaxLengthN) ->
    Tab1 = 3,
    Tab2 = MaxLengthN + 2,
    Tab3 = 7,
    Tab4 = 2,

    {NewFunc, NewArity} = 
	case ets:match(?TABLE_NAME, 
		       {{Mod,'$1','$2','_','_','_',LineNo}, 
			'$3', 
			'_', 
			'_'}) 
	    of
	    [[F, A, N]] ->
		io:format(OutFd, "~s", 
			  [lists:duplicate(Tab1 + Tab2 + Tab3 + Tab4, " ") ++ 
			   "|  " ++ Line]),
		NStr = integer_to_list(N),
		io:format(OutFd, "~s|~n", [lists:duplicate(Tab1, " ") ++ NStr ++ 
					   lists:duplicate(Tab2 - length(NStr), 
							   " ") ++ 
					   lists:duplicate(Tab3 + Tab4, ".")]),
		{F, A};
	    [] ->
		case regexp:match(Line, "%%%%%--> End of function ") of
		    {match, _, _} ->
			ets:insert(?TABLE_NAME, {{end_of_function, Mod, 
						  Func, Arity}, 
						 LineNo - 2}),
			io:format(OutFd, "~s~n", 
				  [lists:duplicate(Tab1 + Tab2 + Tab3 + 
						   Tab4, " ") ++ "|  "]),
			io:format(OutFd, "~s~n", 
				  [lists:duplicate(Tab1 + Tab2 + Tab3 + 
						   Tab4, " ") ++ "|  "]),
			{Func, Arity};
		    _Other ->
			io:format(OutFd, "~s", 
				  [lists:duplicate(Tab1 + Tab2 + Tab3 + 
						   Tab4, " ") ++ 
				   "|  " ++ Line]),
			{Func, Arity}
		end
	end,
    read_file(InFd, io:get_line(InFd, ''), LineNo + 1, OutFd, 
	      Mod, NewFunc, NewArity, MaxLengthN).

					      
		    
		
	    
	    






merge_bump_data({attribute, Line, module, Mod}, Flag) ->
    put(module, Mod),
    {Flag, {attribute, Line, module, Mod}};
merge_bump_data({function, Line, Name, Arity, Clauses}, Flag) ->
    {Flag1, Clauses1} = merge_bump_data1(Clauses, Flag),
    {Flag1, {function, Line, Name, Arity, Clauses1}};
merge_bump_data(X, Flag) ->
    {Flag, X}.



merge_bump_data1([H|T], Flag) ->
    {Flag1, H1} = merge_bump_data1(H, Flag),
    {Flag2, T1} = merge_bump_data1(T, Flag1),
    {Flag2, [H1 | T1]};
merge_bump_data1({call, Line, {remote, _, {atom, _, coast}, {atom, _, bump}}, Args}, Flag) ->
    [{atom, L1, Mod}, {atom, L2, Func}, {integer, L3, Arity},
     {integer, L4, Index}, {integer, L5, ClauseNo}, 
     {integer, L6, ClauseLine}, {integer, L7, SrcLine}, {atom, L8, TraceMode}] = Args,
    Key = {Mod, Func, Arity, Index, ClauseNo, ClauseLine, SrcLine},
    case ets:lookup(?TABLE_NAME, Key) of
	[{Key, 0, _, _}] ->
            %% if there is a single zero change the flag to true
            {true, {string, Line, "#### Not Covered!"}};
	[{Key, 1, _, _}] ->
            {true, {string, Line, 
		    lists:flatten(io_lib:format("---- ~w pass", [1]))}};
	[{Key, N, _, _}] ->
            {true, {string, Line, 
		    lists:flatten(io_lib:format("---- ~w passes", [N]))}};
	[] ->
	    {true, {string, Line, 
		    lists:flatten(io_lib:format("???? No information found!!!"))}}
    end;
merge_bump_data1(X, Flag) when tuple(X) ->
    {Flag1, L} = merge_bump_data1(tuple_to_list(X), Flag),
    {Flag1, list_to_tuple(L)};
merge_bump_data1(X, Flag) ->
    {Flag, X}.




uniq([H1, H2 | T]) when H1 == H2 ->
    uniq([H1 | T]);
uniq([H | T]) ->
    [H | uniq(T)];
uniq([]) ->
    [].






