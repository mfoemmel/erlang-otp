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

-module(coast_server).



-export([start/0]).

-export([init/1, 
	 code_runner/5, 
	 garbage_collector/0
	]).



-include("coast.hrl").
-include("coast_server.hrl").




%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************



%%======================================================================
%% Function:      start/0
%%
%% Return Value:  
%%
%% Description:   Starts a process named 'coast_server'. This process 
%%                owns the coverage data base, i.e., the ETS table named 
%%                'coast_internal_data_table'.
%%
%% Parameters:    None.
%%======================================================================


start() ->
    case whereis(?COAST_SERVER_NAME) of
	undefined ->
	    ServerPid = spawn(?MODULE, init, [self()]),
	       %% Make sure the server has created the ETS table before 
	       %% control is returned.
	    receive 
		{server_started, ServerPid} ->
		    done
	    end,
	       %% Normally we just have to register the server, but there may
	       %% be a situation where another process simultaneously tries to
	       %% start the coast server...
	    case catch register(?COAST_SERVER_NAME, ServerPid) of
		true ->
		    ok;
		{'EXIT', Reason} ->
		       %% A race condition, try again, but kill the server
		       %% we started first...
		    exit(ServerPid, kill),
		    timer:sleep(500),
		    start()
	    end;
	Pid ->
	    ok
    end.





%%======================================================================
%% Function:      init/1
%%
%% Return Value:  
%%
%% Description:   Initialisation code of the coast_server process.
%%
%% Parameters:    None.
%%======================================================================


init(Creator) ->
    ets:new(?TABLE_NAME, [set, public, named_table]),
    spawn_link(?MODULE, garbage_collector, []),
    Creator ! {server_started, self()},
    loop([]).








%%======================================================================
%% Function:      code_runner/5
%%
%% Return Value:  
%%
%% Description:   Executes the function Mod:Func(Args), and returns the 
%%                result to the requesting process.
%%
%% Parameters:    
%%======================================================================


code_runner(ParentPid, RequestingPid, Mod, Func, Args) ->
    case catch apply(Mod, Func, Args) of
	Result ->
	       %% Has to be ParentPid in the result tuple, otherwise the 
	       %% requesting process won't recognize the message!
	    RequestingPid ! {run, ParentPid, Result}
    end.
    







%%======================================================================
%% Function:      garbage_collector/0
%%
%% Return Value:  
%%
%% Description:   Process that checks the ETS table regularly, removing
%%                no longer used data.
%%
%% Parameters:    
%%======================================================================


garbage_collector() ->
    timer:sleep(30000),
    case catch lists:flatten(ets:match(?TABLE_NAME, {{last_call, '$1'}, '_'})) of
	{'EXIT', Reason} ->
	       % Table doesn't exist any more!
	    done;
	ProcessList ->
	    lists:foreach(fun(Pid) ->
				  case erlang:is_process_alive(Pid) of
				      false ->
					  ets:delete(?TABLE_NAME, 
						     {last_call, Pid});
				      true ->
					  done
				  end
			  end, ProcessList),
	    garbage_collector()
    end.






%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************



%%======================================================================
%% Function:      loop/2
%%
%% Return Value:  
%%
%% Description:   Loop part of the coast_server process. Keeps track of
%%                the coast database/table, and the files that have
%%                been compiled for coverage analysis.
%%
%% Parameters:    
%%======================================================================


loop(CompiledModules) ->
    receive 
	{bump, _Sender, BumpLocation, AddTuple} ->
	    bump_code_counters(BumpLocation, AddTuple),
	    loop(CompiledModules);


	{mod_calls,Sender,Modules} ->
	    Sender ! {mod_calls,?COAST_SERVER_NAME, 
		      get_req_res(coast_analyse_funcs,mod_calls,[Modules])},
	    loop(CompiledModules);


	{func_calls,Sender,Modules} ->
	    Sender ! {func_calls,?COAST_SERVER_NAME, 
		      get_req_res(coast_analyse_funcs,func_calls,[Modules])},
	    loop(CompiledModules);


	{clause_calls,Sender,Modules} ->
	    Sender ! {clause_calls,?COAST_SERVER_NAME, 
		      get_req_res(coast_analyse_funcs,clause_calls,[Modules])},
	    loop(CompiledModules);

	
	{mod_coverage,Sender,Modules} ->
	    Sender ! {mod_coverage,?COAST_SERVER_NAME, 
		      get_req_res(coast_analyse_funcs,mod_coverage,[Modules])},
	    loop(CompiledModules);

	
	{func_coverage,Sender,Modules} ->
	    Sender ! {func_coverage,?COAST_SERVER_NAME, 
		      get_req_res(coast_analyse_funcs,func_coverage,[Modules])},
	    loop(CompiledModules);

	
	{clause_coverage,Sender,Modules} ->
	    Sender ! {clause_coverage,?COAST_SERVER_NAME, 
		      get_req_res(coast_analyse_funcs,clause_coverage,[Modules])},
	    loop(CompiledModules);

	
	{analyse_to_file,Sender,Modules} ->
	    Sender ! {analyse_to_file,?COAST_SERVER_NAME, 
		      get_req_res(coast_analyse_funcs,analyse_to_file,[Modules])},
	    loop(CompiledModules);

	
	{file, Sender, File, CoastMode, Opts, TraceMode} ->
	    NewCompMods = 
		case get_req_res(coast_compile_funcs, compile, 
				 [File, CoastMode, Opts, TraceMode]) 
		    of
		    {error, Reason} ->
			Sender ! {error,?COAST_SERVER_NAME,Reason},
			CompiledModules;
		    NewModule ->
			Sender ! {file_compiled,?COAST_SERVER_NAME},
			[NewModule | lists:delete(NewModule,CompiledModules)]
		end,
	    loop(NewCompMods);

	
	{run,Sender,[Mod,Func,Args]} ->
	    spawn(?MODULE,code_runner,[?COAST_SERVER_NAME,Sender,Mod,Func,Args]),
	    loop(CompiledModules);

	
	{known_modules,Sender, _} ->
	    {Result,NewCompiledModules} = 
		case get_req_res(coast_analyse_funcs,
				 get_known_modules,[CompiledModules]) 
		    of
		    {error,Reason} ->
			{{error,Reason},CompiledModules};
		    KnownModules ->
			{KnownModules,KnownModules}
		end,
	    Sender ! {known_modules,?COAST_SERVER_NAME,Result},
	    loop(NewCompiledModules);

	
	{source_files,Sender,Modules} ->
	    Sender ! {source_files,?COAST_SERVER_NAME,
		      get_req_res(coast_comm_funcs, get_source_files,[Modules])},
	    loop(CompiledModules);

	
	{clear,Sender,Modules} ->
	    Sender ! {clear,?COAST_SERVER_NAME,
		      get_req_res(coast_analyse_funcs,clear_module,[Modules])},
	    loop(CompiledModules);

	
	{clear_all,Sender,Modules} ->
	       %% Easiest to just kill the table and create a new!
	    ets:delete(?TABLE_NAME),
	    ets:new(?TABLE_NAME,[set,public,named_table]),
	    Sender ! {clear_all,?COAST_SERVER_NAME,ok},
	    loop(CompiledModules);
	
	
	{quit,Sender,_} ->
	    Sender ! {quit,?COAST_SERVER_NAME,ok};

	
	_Other ->
	    io:format("Unknown message: ~p~n", [_Other]),
	    loop(CompiledModules)
    end.









get_req_res(CoastMod,Func,Args) ->
    case catch apply(CoastMod,Func,Args) of
	{'EXIT', {not_coast_compiled,Module}} ->
	    {error, {not_coast_compiled, Module}};
	{'EXIT', Reason} ->
	    {error, {'EXIT', Reason}};
	Other ->
	    Other
    end.





bump_code_counters({M,F,A,Idx,Clause,ClauseLine,CodeLine}, {TotAdd,ExtModAdd,NonRecAdd}) ->
    {TotN, ExtModN, IntNonRecN} = 
	lookup(M, F, A, Idx, Clause, ClauseLine, CodeLine),
    ets:insert(?TABLE_NAME, {{M, F, A, Idx, Clause, ClauseLine, CodeLine}, 
			     TotN + TotAdd, 
			     ExtModN + ExtModAdd,
			     IntNonRecN + NonRecAdd
			    }).
    




lookup(Module, Func, Arity, Index, Clause, ClauseLine, CodeLine) ->
    case ets:lookup(?TABLE_NAME, 
		    {Module, Func, Arity, Index, Clause, ClauseLine, CodeLine}
		   ) of
	[{_, TotN, ExtModN, NonRecN}] when integer(TotN), 
					   integer(ExtModN),
					   integer(NonRecN) -> 
	    {TotN, ExtModN, NonRecN};
	_Other ->
	    {0, 0, 0}
    end.
	    





