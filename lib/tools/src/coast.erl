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
%%%   Description:      This is the user interface module for the COAST tool.
%%%
%%%*********************************************************************


-module(coast).



-export([compile/1, 
	 compile/2, 
	 compile_all/0, 
	 compile_all/1, 
	 compile_all/2, 
	 run/3,
	 mod_calls/1, 
	 func_calls/1, 
	 clause_calls/1, 
	 mod_coverage/1, 
	 func_coverage/1, 
	 clause_coverage/1,
	 analyse_to_file/1,
	 known_modules/0, 
	 source_files/1, 
	 clear/1, 
	 clear_all/0, 
	 quit/0	 
	]).

-export([bump/9,
	 set_last_call/4
	]).



-include("coast.hrl").
-include("coast_server.hrl").





%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




compile(File) ->
    compile_common(File, coastify, [], no_trace, []).



compile(File, Options) ->
    compile_common(File, coastify, Options, no_trace, []).



compile_all() ->
    case  file:get_cwd() of
	{ok, Dir} ->
	    case compile_all_common(Dir, [], no_trace) of
		{ok, ListOfFilesS} ->
		    {ok, lists:map(fun(X) ->
					   filename:basename(X)
				   end, ListOfFilesS)};
		Error ->
		    Error
	    end;
	Other ->
	    Other
    end.




compile_all(Dir) ->
    compile_all_common(Dir, [], no_trace).




compile_all(Dir, Options) ->
    compile_all_common(Dir, Options, no_trace).




mod_calls(Modules) ->
    send_msg_to_server(mod_calls, Modules).




func_calls(Modules) ->
    send_msg_to_server(func_calls, Modules).




clause_calls(Modules) ->
    send_msg_to_server(clause_calls, Modules).




mod_coverage(Modules) ->
    send_msg_to_server(mod_coverage, Modules).



func_coverage(Modules) ->
    send_msg_to_server(func_coverage, Modules).




clause_coverage(Modules) ->
    send_msg_to_server(clause_coverage, Modules).




analyse_to_file(Modules) ->
    send_msg_to_server(analyse_to_file, Modules).





%%======================================================================
%% Function:      bump/8
%%
%% Return Value:  
%%
%% Description:   'bump' is called by the modules compiled for coast analysis.
%%
%% Parameters:    
%%======================================================================


bump(Mod, Func, Arity, Index, Clause, ClauseLine, CodeLine, CallDirection, Trace) ->
    Self = self(),
    {LastMod, LastFunc, LastArity, LastCallDirection} = 
	check_if_allowed_to_run(Self, Mod),
    AddTuple = check_kind_of_call({LastMod,LastFunc,LastArity,LastCallDirection}, 
				  {Mod,Func,Arity}),
    coast_server:start(),
    ?COAST_SERVER_NAME ! {bump, self(), 
			  {Mod,Func,Arity,Index,Clause,ClauseLine,CodeLine},
			  AddTuple},
    ets:insert(?TABLE_NAME, {{last_call,Self}, {Mod,Func,Arity,CallDirection}}).
%    handle_trace(Trace, Mod, Index).




%%======================================================================
%% Function:      set_last_call
%%
%% Return Value:  
%%
%% Description:   This function is used to get back to a correct state 
%%                after having executed nested functions (i.e., function
%%                calls having other function calls as parameters).
%%
%% Parameters:    
%%======================================================================


set_last_call(Mod, Func, Arity, CallDirection) ->
    Self = self(),
    check_if_allowed_to_run(Self, Mod),
    ets:insert(?TABLE_NAME, {{last_call,Self}, {Mod,Func,Arity,CallDirection}}).




quit() ->
    send_msg_to_server(quit, []).




clear(Modules) ->
    send_msg_to_server(clear, Modules).




clear_all() ->
    send_msg_to_server(clear_all, []).




known_modules() ->
    send_msg_to_server(known_modules, []).



source_files(Modules) ->
    send_msg_to_server(source_files, Modules).




run(Mod, Func, Args) when list(Args) ->
    case send_msg_to_server(run, [Mod, Func, Args]) of
	{'EXIT', Reason} ->
	    exit(Reason);
	Other ->
	    Other
    end;
run(Mod, Func, Args) ->
    run(Mod, Func, [Args]).
    
    




%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************




send_msg_to_server(Msg, Args) when list(Args) ->
    coast_server:start(),
    ?COAST_SERVER_NAME ! {Msg, self(), Args},
    receive
	{Msg, ?COAST_SERVER_NAME, Data} ->
	    Data
    end;
send_msg_to_server(Msg, Arg) ->
    send_msg_to_server(Msg, [Arg]).

    



check_if_allowed_to_run(Self, Mod) ->
       %% We have to check two things:
       %%  1. That the program has been started from coast, NOT from the shell.
       %%  2. That the coast server is running.
    case catch ets:lookup(?TABLE_NAME, {last_call,Self}) of
	[{{last_call,Self}, ValueTuple}] ->
	       %% The program has been started (previously) from within the 
	       %% coast program, OK! Since the table exists, we know the coast
	       %% server is running.
	    ValueTuple;
	[] ->
	       %% We don't know whether the program has been started from the 
	       %% shell or the coast program - have to check this! (But we know
	       %% that the coast server is running, since the table exists.)
	    exit_if_shell_process(Mod),
	       %% If we are still alive, return initial value.  ;-)
	    {'$$_undefined','$$_undefined','$$_undefined','$$_undefined'};
	{'EXIT', Reason} ->
	       %% The table doesn't exist, meaning that the coast server not  
	       %% yet has been started! Since the coast server hasn't been
	       %% started, we also know that we must have been started from 
	       %% the shell! Terminate immediately! :-)
	    terminate(Mod)
    end.






check_kind_of_call({LastMod,LastFunc,LastArity,LastCallDirection}, {Mod,Func,Arity}) ->
    case LastMod of
	Mod ->
	    case {LastFunc, LastArity} of
		   %% Watch out for the case when we have called a not coast-compiled
		   %% module, which in turn calls us! In that case {LastFunc,LastArity}
		   %% will be {Func,Arity}!!! Better check the LastCallDirection variable!
		{Func, Arity} when LastCallDirection == external ->
		    {1, 1, 0};
		{Func, Arity} ->
		    {1, 0, 0};
		_OtherTuple ->
		       %% Here we may actually count returning from
		       %% a function call as a non-recursive call.
		       %% However, this doesn't matter, since we only
		       %% are interested in those values when it comes
		       %% to the first line in a function clause! (And
		       %% we cannot reach that line from a function call...)
		       %% For a normal line we are only interested in the 
		       %% total number of times that line has been passed.  :-)
		    {1, 0, 1}
	    end;
	_OtherMod ->
	    {1, 1, 0}
    end.
    


exit_if_shell_process(Module) ->
    case process_info(self(), initial_call) of
	{initial_call, {shell, evaluator, 3}} ->
	    terminate(Module);
	Other ->
	    done
    end.
	    
    



terminate(Module) ->
    {YY,MM,DD,H,M,S} = get_curr_date_and_time(),
    io:format("~n=ERROR REPORT==== ~p-~s-~p::~s:~s:~s ===~n"
	      "Module ~p is currently compiled for coverage~n"
	      "analysis and cannot be called from the shell.~n"
	      "Execution terminated!~n~n", [DD,MM,YY,H,M,S,Module]),
    exit({coast_compiled,Module}).




get_curr_date_and_time() ->
    Months = {"Jan","Feb","Mar","Apr","May","Jun",
	      "Jul","Aug","Sep","Oct","Nov","Dec"},
    NumStr = fun(N) when N < 10 ->
		     [$0 | integer_to_list(N)];
		(N) ->
		     integer_to_list(N)
	     end,
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    {Year, element(Month, Months), Day, NumStr(Hour), NumStr(Min), NumStr(Day)}.
     
    
    





compile_common([H | T], CoastifyMode, Options, TraceMode, Acc) when atom(H) ->
       % This is the case ['file1' | T] 
    case catch compile_common2(H, CoastifyMode, Options, TraceMode) of
	{ok, File} ->
	    compile_common(T, CoastifyMode, Options, TraceMode, [File | Acc]);
	{error, Reason} ->
	    {error, Reason};
	{'EXIT', Reason} ->
	    {error, {bad_format, [H | T]}}
    end;
compile_common([H | T], CoastifyMode, Options, TraceMode, Acc) when list(H) ->
       % This is the case ["file1" | T]
    case catch compile_common2(H, CoastifyMode, Options, TraceMode) of
	{ok, File} ->
	    compile_common(T, CoastifyMode, Options, TraceMode, 
			   [filename:rootname(File) | Acc]);
	{error, Reason} ->
	    {error, Reason};
	{'EXIT', Reason} ->
	    {error, {bad_format, [H | T]}}
    end;
compile_common([H | T], CoastifyMode, Options, TraceMode, Acc) when integer(H) ->
       % This is the case "file". (We test for integer above because 
       % a string is represented as a list of integers, remember?)
    case catch compile_common2([H | T], CoastifyMode, Options, TraceMode) of
	{ok, File} ->
	    {ok, filename:rootname(File)};
	{error, Reason} ->
	    {error, Reason};
	{'EXIT', Reason} ->
	    {error, {bad_format, [H | T]}}
    end;
compile_common(Module, CoastifyMode, Options, TraceMode, Acc) when atom(Module) ->
       % This is the case 'file'
    case catch compile_common2(Module, CoastifyMode, Options, TraceMode) of
	{ok, File} ->
	    {ok, File};
	{error, Reason} ->
	    {error, Reason};
	{'EXIT', Reason} ->
	    {error, {bad_format, Module}}
    end;
compile_common([], CoastifyMode, Options, TraceMode, Acc) ->
    {ok, lists:reverse(Acc)}.
    





compile_common2(File, CoastifyMode, Options, TraceMode) ->   
    coast_server:start(),
    ?COAST_SERVER_NAME ! {file, self(), File, CoastifyMode, Options, TraceMode},
    receive
	{error, ?COAST_SERVER_NAME, Reason} ->
	    {error, Reason};
	{file_compiled, ?COAST_SERVER_NAME} ->
	    {ok, File}
    end.
	    




compile_all_common(Dir, Options, TraceMode) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            compile_all_common(Dir, lists:sort(Files), Options, TraceMode, []);
        {error, enoent} ->
            {error, "No such directory"};

	OtherError ->
	    {error, "Cannot read directory"}
    end.





compile_all_common(Dir, [H | T], Options, TraceMode, CompiledFiles) ->
       %% Don't run coast on the coast program files, or on files that have been
       %% generated by the coast program! Ignore file names that ends with 
       %% '.COAST.erl', '.COAST.pretty.erl', and the files 'coast.erl', 
       %% 'coast_server.erl', 'coast_analyse_funcs.erl', 'coast_comm_funcs.erl',
       %% and 'coast_compile_funcs.erl'.
    FilesToAvoid = ["coast.erl", "coast_server.erl", "coast_analyse_funcs.erl", 
		    "coast_comm_funcs.erl", "coast_compile_funcs.erl"],

    case lists:reverse(H) of
	   % Beware of ...'.COAST.erl'!
        [$l, $r, $e, $., $T, $S, $A, $O, $C, $. | Z] ->
            compile_all_common(Dir, T, Options, TraceMode, CompiledFiles);
	   % Beware of ...'.COAST.pretty.erl'!
	[$l, $r, $e, $., $y, $t, $t, $e, $r, $p, $., $T, $S, $A, $O, $C, $. | Z] ->
	    compile_all_common(Dir, T, Options, TraceMode, CompiledFiles);
        [$l, $r, $e, $. | Z] ->
	    case lists:member(H, FilesToAvoid) of 
		true ->
		    compile_all_common(Dir, T, Options, TraceMode, CompiledFiles);
		false ->
		    FileName = filename:join(Dir, H),
		    case compile_common(FileName, coastify, 
					Options, TraceMode, []) 
			of
			{ok, File} ->
			    compile_all_common(Dir, T, Options, TraceMode, 
					       [File | CompiledFiles]);
			Other ->
			    Other
		    end
	    end;
	_OtherFileNameExtension ->
	    compile_all_common(Dir, T, Options, TraceMode, CompiledFiles)
    end;
compile_all_common(Dir, [], _Options, TraceMode, CompiledFiles) -> 
    {ok, lists:reverse(CompiledFiles)}.
