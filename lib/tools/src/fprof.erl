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

%%%----------------------------------------------------------------------
%%% File    : fprof.erl
%%% Author  : Raimo Niskanen <raimo@erix.ericsson.se>
%%% Purpose : File tracing profiling tool wich accumulated times.
%%% Created : 18 Jun 2001 by Raimo Niskanen <raimo@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(fprof).
-author('raimo@erix.ericsson.se').

%% External exports
-export([
	 apply/2, apply/3, apply/4,
	 start/0, stop/0, stop/1,
	 trace/1, trace/2,
	 profile/0, profile/1, profile/2,
	 analyse/0, analyse/1, analyse/2]).
%% Debug functions
-export([get_state/0,
	 save_profile/0, save_profile/1, save_profile/2,
	 load_profile/0, load_profile/1, load_profile/2,
	 code_change/0]).

%% Imports
-import(lists, [flatten/1, flatten/2, reverse/1, reverse/2, sort/1, foldl/3]).

%% Debug exports
-export([call/1, just_call/1, reply/2]).
-export([trace_off/0, trace_on/3]).
-export([getopts/2, setopts/1]).
-export([println/5, print_callers/2, print_func/2, print_called/2]).
-export([trace_call_collapse/1]).
-export([parsify/1]).

%% Internal exports
-export(['$code_change'/1]).



-define(FNAME_WIDTH, 72).
-define(NR_WIDTH, 15).

-define(TRACE_FILE, "fprof.trace").
-define(DUMP_FILE, "fprof.dump").
-define(PROFILE_FILE, "fprof.profile").
-define(ANALYSIS_FILE, "fprof.analysis").

-define(FPROF_SERVER, fprof_server).
-define(FPROF_SERVER_TIMEOUT, infinity).



-define(debug, 9).

-ifdef(debug).
-define(dbg(Level, F, A), 
	if Level >= ?debug -> io:format(F, A), ok;
	   true -> ok
	end).
-else.
-define(dbg(Level, F, A), ok).
-endif.



%%%----------------------------------------------------------------------
%%% Higher order API functions
%%%----------------------------------------------------------------------



apply({M, F} = Function, Args) 
  when atom(M), atom(F), list(Args) ->
    apply_1(Function, Args, []);
apply(Fun, Args) 
  when function(Fun), list(Args) ->
    apply_1(Fun, Args, []);
apply(A, B) ->
    erlang:fault(badarg, [A, B]).

apply(M, F, Args) when atom(M), atom(F), list(Args) ->
    apply_1({M, F}, Args, []);
apply({M, F} = Function, Args, Options) 
  when atom(M), atom(F), list(Args), list(Options) ->
    apply_1(Function, Args, Options);
apply(Fun, Args, Options) 
  when function(Fun), list(Args), list(Options) ->
    apply_1(Fun, Args, Options);
apply(A, B, C) ->
    erlang:fault(badarg, [A, B, C]).

apply(Module, Function, Args, Options) 
  when atom(Module), atom(Function), list(Args), list(Options) ->
    apply_1({Module, Function}, Args, Options);
apply(A, B, C, D) ->
    erlang:fault(badarg, [A, B, C, D]).


apply_1(Function, Args, Options) ->        
    {[_, Procs, Continue], Options_1} =
	getopts(Options, [start, procs, continue]),
    Procs_1 = case Procs of
		  [{procs, P}] when list(P) ->
		      P;
		  _ ->
		      []
	      end,
    case Continue of
	[] ->
	    apply_start_stop(Function, Args, Procs_1, Options_1);
	[continue] ->
	    apply_continue(Function, Args, Procs_1, Options_1);
	_ ->
	    erlang:fault(badarg, [Function, Args, Options])
    end.



apply_start_stop(Function, Args, Procs, Options) ->
    Ref = make_ref(),
    Parent = self(),
    Child = 
	spawn(
	  fun() ->
		  MRef = erlang:monitor(process, Parent),
		  receive
		      {Parent, Ref, start_trace} ->
			  case trace([start, 
				      {procs, [Parent | Procs]} 
				      | Options]) of
			      ok ->
				  catch Parent ! {self(), Ref, trace_started},
				  receive
				      {Parent, Ref, stop_trace} ->
					  trace([stop]),
					  catch Parent 
					      ! {self(), Ref, trace_stopped},
					  done;
				      {'DOWN', MRef, _, _, _} ->
					  trace([stop])
				  end;
			      {error, Reason} ->
				  exit(Reason)
			  end;
		      {'DOWN', MRef, _, _, _} ->
			  done
		  end
	  end),
    MRef = erlang:monitor(process, Child),
    catch Child ! {self(), Ref, start_trace},
    receive
	{Child, Ref, trace_started} ->
	    Try = (catch {Ref, erlang:apply(Function, Args)}),
	    catch Child ! {self(), Ref, stop_trace},
	    receive
		{Child, Ref, trace_stopped} ->
		    receive
			{'DOWN', MRef, _, _, _} ->
			    ok
		    end;
		{'DOWN', MRef, _, _, _} ->
		    trace([stop])
	    end,
	    case Try of
		{Ref, Result} ->
		    Result;
		{'EXIT', Reason} ->
		    exit(Reason);
		Exception ->
		    throw(Exception)
	    end;
	{'DOWN', MRef, _, _, Reason} ->
	    exit(Reason)
    end.

apply_continue(Function, Args, Procs, Options) ->
    Ref = make_ref(),
    Parent = self(),
    Child = 
	spawn(
	  fun() ->
		  MRef = erlang:monitor(process, Parent),
		  receive
		      {Parent, Ref, start_trace} ->
			  case trace([start, 
				      {procs, [Parent | Procs]} 
				      | Options]) of
			      ok ->
				  exit({Ref, trace_started});
			      {error, Reason} ->
				  exit(Reason)
			  end;
		      {'DOWN', MRef, _, _, _} ->
			  done
		  end
	  end),
    MRef = erlang:monitor(process, Child),
    catch Child ! {self(), Ref, start_trace},
    receive
	{'DOWN', MRef, _, _, {Ref, trace_started}} ->
	    case catch {Ref, erlang:apply(Function, Args)} of
		{Ref, Result} ->
		    Result;
		{'EXIT', Reason} ->
		    exit(Reason);
		Exception ->
		    throw(Exception)
	    end;
	{'DOWN', MRef, _, _, Reason} ->
	    exit(Reason)
    end.



%%%----------------------------------------------------------------------
%%% Requests to ?FPROF_SERVER
%%%----------------------------------------------------------------------

-record(trace_start, {procs,  % List of processes
		      mode,   % normal | verbose
		      type,   % file | tracer
		      dest}). % Filename | Pid/Port

-record(trace_stop, {}).

% -record(open_out, {file}).

% -record(close_out, {}).

-record(profile, {src,          % Filename
		  group_leader, % IoPid
		  dump,         % Filename | IoPid
		  flags}).      % List

-record(profile_start, {group_leader, % IoPid
			dump,         % Filename | IoPid
			flags}).      % List

-record(profile_stop, {}).

-record(analyse, {group_leader, % IoPid
		  dest,         % Filename | IoPid
		  flags,        % List
		  cols,         % Integer
		  callers,      % Boolean
		  sort,         % acc_r | own_r
		  totals,       % Boolean
		  details}).    % Boolean

-record(stop, {
	 reason}).



%%---------------
%% Debug requests
%%---------------

-record(get_state, {}).

-record(save_profile, {file}).

-record(load_profile, {file}).



%%%----------------------------------------------------------------------
%%% Basic API functions
%%%----------------------------------------------------------------------



trace(start, Filename) ->
    trace([start, {file, Filename}]);
trace(verbose, Filename) ->
    trace([start, verbose, {file, Filename}]);
trace(Option, Value) when atom(Option) ->
    trace([{Option, Value}]);
trace(Option, Value) ->
    erlang:fault(badarg, [Option, Value]).

trace(stop) ->
    %% This shortcut is present to minimize the number of undesired
    %% function calls at the end of the trace.
    call(#trace_stop{});
trace(verbose) ->
    trace([start, verbose]);
trace([stop]) ->
    %% This shortcut is present to minimize the number of undesired
    %% function calls at the end of the trace.
    call(#trace_stop{});
trace({Opt, _Val} = Option) when atom(Opt) ->
    trace([Option]);
trace(Option) when atom(Option) ->
    trace([Option]);
trace(Options) when list(Options) ->
    case getopts(Options, 
		 [start, stop, procs, verbose, file, tracer, cpu_time]) of
	{[[], [stop], [], [], [], [], []], []} ->
	    call(#trace_stop{});
	{[[start], [], Procs, Verbose, File, Tracer, CpuTime], []} ->
	    {Type, Dest} = case {File, Tracer} of
			       {[], [{tracer, Pid} = T]} 
			       when pid(Pid); port(Pid) ->
				   T;
			       {[file], []} ->
				   {file, ?TRACE_FILE};
			       {[{file, []}], []} ->
				   {file, ?TRACE_FILE};
			       {[{file, _} = F], []} ->
				   F;
			       {[], []} ->
				   {file, ?TRACE_FILE};
			       _ ->
				   erlang:fault(badarg, [Options])
			   end,
	    V = case Verbose of
		       [] -> normal;
		       [verbose] -> verbose;
		       [{verbose, true}] -> verbose;
		       [{verbose, false}] -> normal;
		       _ -> erlang:fault(badarg, [Options])
		   end,
	    CT = case CpuTime of
		     [] -> wallclock;
		     [cpu_time] -> cpu_time;
		     [{cpu_time, true}] -> cpu_time;
		     [{cpu_time, false}] -> wallclock;
		     _ -> erlang:fault(badarg, [Options])
		 end,
	    call(#trace_start{procs = case Procs of
					  [] ->
					      [self()];
					  [{procs, P}] when list(P) ->
					      P;
					  [{procs, P}] ->
					      [P];
					  _ ->
					      erlang:fault(badarg, [Options])
				      end,
			      mode = {V, CT},
			      type = Type,
			      dest = Dest});
	_ ->
	    erlang:fault(badarg, [Options])
    end;
trace(Options) ->
    erlang:fault(badarg, [Options]).



profile() ->
    profile([]).

profile(Option, Value) when atom(Option) ->
    profile([{Option, Value}]);
profile(Option, Value) ->
    erlang:fault(badarg, [Option, Value]).

profile(Option) when atom(Option) ->
    profile([Option]);
profile({Opt, _Val} = Option) when atom(Opt) ->
    profile([Option]);
profile(Options) when list(Options) ->
    case getopts(Options, [start, stop, file, dump, append]) of
	{[Start, [], File, Dump, Append], []} ->
	    {Target, Flags} = 
		case {Dump, Append} of
		    {[], []} ->
			{[], []};
		    {[dump], []} ->
			{group_leader(), []};
		    {[{dump, []}], []} ->
			{?DUMP_FILE, []};
		    {[{dump, []}], [append]} ->
			{?DUMP_FILE, [append]};
		    {[{dump, D}], [append]} when pid(D) ->
			erlang:fault(badarg, [Options]);
		    {[{dump, D}], [append]} ->
			{D, [append]};
		    {[{dump, D}], []} ->
			{D, []};
		    _ ->
			erlang:fault(badarg, [Options])
		end,
	    case {Start, File} of
		{[start], []} ->
		    call(#profile_start{group_leader = group_leader(),
					dump = Target,
					flags = Flags});
		{[], _} ->
		    Src = 
			case File of
			    [] ->
				?TRACE_FILE;
			    [file] ->
				?TRACE_FILE;
			    [{file, []}] ->
				?TRACE_FILE;
			    [{file, F}] ->
				F;
			    _ ->
				erlang:fault(badarg, [Options])
			end,
		    call(#profile{src = Src,
				  group_leader = group_leader(),
				  dump = Target,
				  flags = Flags});
		_ ->
		    erlang:fault(badarg, [Options])
	    end;
	{[[], [stop], [], [], []], []} ->
	    call(#profile_stop{});
	_ ->
	    erlang:fault(badarg, [Options])
    end;
profile(Options) ->
    erlang:fault(badarg, [Options]).



analyse() ->
    analyse([]).

analyse(Option, Value) when atom(Option) ->
    analyse([{Option, Value}]);
analyse(Option, Value) ->
    erlang:fault(badarg, [Option, Value]).

analyse(Option) when atom(Option) ->
    analyse([Option]);
analyse({Opt, _Val} = Option) when atom(Opt) ->
    analyse([Option]);
analyse(Options) when list(Options) ->
    case getopts(Options, 
		 [dest, append, cols, callers, no_callers, 
		  sort, totals, details, no_details]) of
	{[Dest, Append, Cols, Callers, NoCallers,
	  Sort, Totals, Details, NoDetails], []} ->
	    {Target, Flags} = 
		case {Dest, Append} of
		    {[], []} ->
			{group_leader(), []};
		    {[dest], []} ->
			{group_leader(), []};
		    {[{dest, []}], []} ->
			{?ANALYSIS_FILE, []};
		    {[{dest, []}], [append]} ->
			{?ANALYSIS_FILE, [append]};
		    {[{dest, F}], [append]} when pid(F) ->
			erlang:fault(badarg, [Options]);
		    {[{dest, F}], [append]} ->
			{F, [append]};
		    {[{dest, F}], []} ->
			{F, []};
		    _ ->
			erlang:fault(badarg, [Options])
		end,
	    call(#analyse{group_leader = group_leader(),
			  dest = Target,
			  flags = Flags,
			  cols = case Cols of
				     [] ->
					 80;
				     [{cols, C}] when integer(C), C > 0 ->
					 C;
				     _ ->
					 erlang:fault(badarg, [Options])
				 end,
			  callers = case {Callers, NoCallers} of
					{[], []} -> 
					    true;
					{[callers], []} ->
					    true;
					{[{callers, true}], []} ->
					    true;
					{[{callers, false}], []} ->
					    false;
					{[], [no_callers]} ->
					    false;
					_ ->
					    erlang:fault(badarg, [Options])
				    end,
			  sort = case Sort of
				     [] -> 
					 acc;
				     [{sort, acc}] ->
					 acc;
				     [{sort, own}] ->
					 own;
				     _ ->
					 erlang:fault(badarg, [Options])
				 end,
			  totals = case Totals of
				       [] -> 
					   false;
				       [totals] ->
					   true;
				       [{totals, true}] ->
					   true;
				       [{totals, false}] ->
					   false;
				       _ ->
					   erlang:fault(badarg, [Options])
				   end,
			  details = case {Details, NoDetails} of
					{[], []} ->
					    true;
					{[details], []} ->
					    true;
					{[{details, true}], []} ->
					    true;
					{[{details, false}], []} ->
					    false;
					{[], [no_details]} ->
					    false;
				       _ ->
					   erlang:fault(badarg, [Options])
				    end});
  	_ ->
	    erlang:fault(badarg, [Options])
    end;
analyse(Options) ->
    erlang:fault(badarg, [Options]).



%%----------------
%% Debug functions
%%----------------



get_state() ->
    just_call(#get_state{}).



save_profile() ->
    save_profile([]).

save_profile(Option, Value) when atom(Option) ->
    save_profile([{Option, Value}]);
save_profile(Option, Value) ->
    erlang:fault(badarg, [Option, Value]).

save_profile(Option) when atom(Option) ->
    save_profile([Option]);
save_profile(Options) when list(Options) ->
    case getopts(Options, [file]) of
	{[File], []} ->
	    call(#save_profile{file = case File of
					  [] -> 
					      ?PROFILE_FILE;
					  [{file, F}] ->
					      F;
					  _ ->
					      erlang:fault(badarg, [Options])
				      end});
  	_ ->
	    erlang:fault(badarg, [Options])
    end;
save_profile(Options) ->
    erlang:fault(badarg, [Options]).



load_profile() ->
    load_profile([]).

load_profile(Option, Value) when atom(Option) ->
    load_profile([{Option, Value}]);
load_profile(Option, Value) ->
    erlang:fault(badarg, [Option, Value]).

load_profile(Option) when atom(Option) ->
    load_profile([Option]);
load_profile(Options) when list(Options) ->
    case getopts(Options, [file]) of
	{[File], []} ->
	    call(#load_profile{file = case File of
					  [] -> 
					      ?PROFILE_FILE;
					  [{file, F}] ->
					      F;
					  _ ->
					      erlang:fault(badarg, [Options])
				      end});
  	_ ->
	    erlang:fault(badarg, [Options])
    end;
load_profile(Options) ->
    erlang:fault(badarg, [Options]).



code_change() ->
    just_call('$code_change').



%%%----------------------------------------------------------------------
%%% ETS table record definitions
%%% The field 'id' must be first in these records;
%%% it is the common ets table index field.
%%%----------------------------------------------------------------------

-record(clocks, {
	  id,
	  cnt = 0,   % Number of calls
	  own = 0,   % Own time (wall clock)
	  acc = 0}). % Accumulated time : own + subfunctions (wall clock)

-record(proc, {
	  id,
	  parent,
	  spawned_as,     % Spawned MFArgs
	  init_log = [],  % List of first calls, head is newest
	  init_cnt = 2}). % First calls counter, counts down to 0

-record(misc, {id, 
	       data}).



%% Analysis summary record
-record(funcstat, {
	  callers_sum,   % #clocks{id = {Pid, Caller, Func}}
	  called_sum,    % #clocks{id = {Pid, Caller, Func}}
	  callers = [],  % [#clocks{}, ...]
	  called = []}). % [#clocks{}, ...]



%%%----------------------------------------------------------------------
%%% ?FPROF_SERVER
%%%----------------------------------------------------------------------

%%%-------------------
%%% Exported functions
%%%-------------------

%% Start server process
start() ->
    case catch spawn_3step(
	   fun (_Parent) ->
		   register(?FPROF_SERVER, self()),
		   process_flag(trap_exit, true),
		   ready
	   end,
	   fun(_Server, ready) ->
		   go
	   end,
	   fun(_Parent, go) ->
		   put(trace_state, idle),
		   put(profile_state, {idle, {error, no_profile}}),
		   put(pending_stop, []),
		   server_loop([])
	   end) of
	{ok, Server, ready, go} ->
	    {ok, Server};
	{'EXIT', _} ->
	    {error, {already_started, whereis(?FPROF_SERVER)}}
    end.



%% Stop server process

stop() ->
    stop(normal).

stop(kill) ->
    case whereis(?FPROF_SERVER) of
	undefined ->
	    ok;
	Pid ->
	    exit(Pid, kill),
	    ok
    end;
stop(Reason) ->
    just_call(#stop{reason = Reason}),
    ok.



%%%------------------------
%%% Client helper functions
%%%------------------------

%% Send request to server process and return the server's reply.
%% First start server if it ain't started.
call(Request) ->
    case whereis(?FPROF_SERVER) of
	undefined ->
	    start(),
	    just_call(Request);
	Server ->
	    just_call(Server, Request)
    end.

%% Send request to server process, and return the server's reply.
%% Returns {'EXIT', Pid, Reason} if the server dies during the
%% call, or if it wasn't started.
just_call(Request) ->
    just_call(whereis(?FPROF_SERVER), Request).

just_call(undefined, _) ->
    {'EXIT', ?FPROF_SERVER, noproc};
just_call(Pid, Request) ->
    Mref = erlang:monitor(process, Pid),
    receive
	{'DOWN', Mref, _, _, Reason} ->
	    {'EXIT', Pid, Reason}
    after 0 ->
	    Tag = {Mref, self()},
	    T = case Request of
		    #stop{} ->
			?FPROF_SERVER_TIMEOUT;
		    _ ->
			0
		end,
	    %% io:format("~p request: ~p~n", [?MODULE, Request]),
	    catch Pid ! {?FPROF_SERVER, Tag, Request},
	    receive
		{?FPROF_SERVER, Mref, Reply} ->
		    erlang:demonitor(Mref),
		    receive {'DOWN', Mref, _, _, _} -> ok after T -> ok end,
		    Reply;
		{'DOWN', Mref, _, _, Reason} ->
		    receive {?FPROF_SERVER, Mref, _} -> ok after T -> ok end,
		    {'EXIT', Pid, Reason}
	    after ?FPROF_SERVER_TIMEOUT ->
		    timeout
	    end
    end.



%%%------------------------
%%% Server helper functions
%%%------------------------

%% Return the reply to the client's request.
reply({Mref, Pid}, Reply) when reference(Mref), pid(Pid) ->
    catch Pid ! {?FPROF_SERVER, Mref, Reply},
    ok.



server_loop(State) ->    
    receive 
	{?FPROF_SERVER, {Mref, Pid} = Tag, '$code_change'} 
	when reference(Mref), pid(Pid) ->
	    reply(Tag, ok),
	    ?MODULE:'$code_change'(State);
	{?FPROF_SERVER, {Mref, Pid} = Tag, Request} 
	when reference(Mref), pid(Pid) ->
	    server_loop(handle_req(Request, Tag, State));
	Other ->
	    server_loop(handle_other(Other, State))
    end.

%-export.
'$code_change'(State) ->
    case lists:keysearch(time, 1, module_info(compile)) of
	{value, {time, {Y, M, D, HH, MM, SS}}} ->
	    io:format("~n~w: code change to compile time "
		      ++"~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w~n",
		      [?MODULE, Y, M, D, HH, MM, SS]);
	false ->
	    ok
    end,
    server_loop(State).



%% Server help function that stops the server iff the
%% sub state machines are in proper states. Sends the reply
%% to all waiting clients.
try_pending_stop(State) ->
    case {get(trace_state), get(profile_state), get(pending_stop)} of
	{idle, {idle, _}, [_|_] = PendingStop} ->
	    Reason = get(stop_reason),
	    Reply = result(Reason),
	    lists:foreach(
	      fun (Tag) ->
		      reply(Tag, Reply)
	      end,
	      PendingStop),
	    exit(Reason);
	_ ->
	    State
    end.

%%------------------
%% Server handle_req			    
%%------------------

handle_req(#trace_start{procs = Procs,
			mode = Mode,
			type = file,
			dest = Filename}, Tag, State) ->
    case {get(trace_state), get(pending_stop)} of
	{idle, []} ->
	    trace_off(),
	    Port = open_dbg_trace_port(file, Filename),
	    case trace_on(Procs, Port, Mode) of
		ok ->
		    put(trace_state, running),
		    put(trace_type, file),
		    put(trace_pid, Port),
		    reply(Tag, ok),
		    State;
		Error ->
		    reply(Tag, Error),
		    State
	    end;
	_ ->
	    reply(Tag, {error, already_tracing}),
	    State
    end;
handle_req(#trace_start{procs = Procs,
			mode = Mode,
			type = tracer,
			dest = Tracer}, Tag, State) ->
    case {get(trace_state), get(pending_stop)} of
	{idle, []} ->
	    trace_off(),
	    case trace_on(Procs, Tracer, Mode) of
		ok ->
		    put(trace_state, running),
		    put(trace_type, tracer),
		    put(trace_pid, Tracer),
		    reply(Tag, ok),
		    State;
		Error ->
		    reply(Tag, Error),
		    State
	    end;
	_ ->
	    reply(Tag, {error, already_tracing}),
	    State
    end;

handle_req(#trace_stop{}, Tag, State) ->
    case get(trace_state) of
	running ->
	    TracePid = get(trace_pid),
	    trace_off(),
	    case erase(trace_type) of
		file ->
		    catch erlang:port_close(TracePid),
		    put(trace_state, stopping),
		    put(trace_tag, Tag),
		    State;
		tracer ->
		    erase(trace_pid),
		    put(trace_state, idle),
		    case {get(profile_state), get(profile_type), 
			  get(profile_pid)} of
			{running, tracer, TracePid} ->
			    exit(TracePid, normal),
			    put(profile_tag, Tag),
			    State;
			_ ->
			    reply(Tag, ok),
			    try_pending_stop(State)
		    end
	    end;
	_ ->
	    reply(Tag, {error, not_tracing}),
	    State
    end;

handle_req(#profile{src = Filename,
		    group_leader = GroupLeader,
		    dump = Dump,
		    flags = Flags}, Tag, State) ->
    case {get(profile_state), get(pending_stop)} of
	{{idle, _}, []} ->
	    case ensure_open(Dump, [write | Flags]) of
		{already_open, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, false);
		{ok, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, true);
		{error, _} = Error ->
		    reply(Tag, Error),
		    State
	    end,
	    Table = ets:new(?MODULE, [set, public, {keypos, #clocks.id}]),
	    Pid = spawn_link_dbg_trace_client(Filename, Table, 
					      GroupLeader, 
					      get(profile_dump)),
	    put(profile_state, running),
	    put(profile_type, file),
	    put(profile_pid, Pid),
	    put(profile_tag, Tag),
	    put(profile_table, Table),
	    State;
	_ ->
	    reply(Tag, {error, already_profiling}),
	    State
    end;
	    
handle_req(#profile_start{group_leader = GroupLeader,
			  dump = Dump,
			  flags = Flags}, Tag, State) ->
    case {get(profile_state), get(pending_stop)} of
	{{idle, _}, []} ->
	    case ensure_open(Dump, [write | Flags]) of
		{already_open, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, false);
		{ok, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, true);
		{error, _} = Error ->
		    reply(Tag, Error),
		    State
	    end,
	    Table = ets:new(?MODULE, [set, public, {keypos, #clocks.id}]),
	    Pid = spawn_link_trace_client(Table, GroupLeader, 
					  get(profile_dump)),
	    put(profile_state, running),
	    put(profile_type, tracer),
	    put(profile_pid, Pid),
	    put(profile_table, Table),
	    reply(Tag, {ok, Pid}),
	    State;
	_ ->
	    reply(Tag, {error, already_profiling}),
	    State
    end;

handle_req(#profile_stop{}, Tag, State) ->
    case {get(profile_state), get(profile_type)} of
	{running, tracer} ->
	    ProfilePid = get(profile_pid),
	    case {get(trace_state), get(trace_type), get(trace_pid)} of
		{running, tracer, ProfilePid} ->
		    trace_off(),
		    erase(trace_type),
		    erase(trace_pid),
		    put(trace_state, idle);
		_ ->
		    ok
	    end,
	    exit(ProfilePid, normal),
	    put(profile_tag, Tag),
	    State;
	{running, file} ->
	    reply(Tag, {error, profiling_file}),
	    State;
	{_, _} ->
	    reply(Tag, {error, not_profiling}),
	    State
    end;

handle_req(#analyse{dest = Dest,
		    flags = Flags} = Request, Tag, State) ->
    case get(profile_state) of
	{idle, ok} ->
	    case ensure_open(Dest, [write | Flags]) of
		{error, _} = Error ->
		    reply(Tag, Error),
		    State;
		{DestState, DestPid} ->
		    ProfileTable = get(profile_table),
		    case catch spawn_3step(
			   fun(_Server) ->
				   do_analyse(ProfileTable, 
					      Request#analyse{dest = DestPid})
			   end,
			   fun(_Worker, _Result) ->
				   finish
			   end,
			   fun(_Server, finish) ->
				   ok
			   end) of
			{ok, _Worker, Result, finish} ->
			    reply(Tag, Result),
			    State;
			{'EXIT', Reason} ->
			    reply(Tag, {error, Reason}),
			    State
		    end,
		    case DestState of
			already_open ->
			    ok;
			ok ->
			    file:close(DestPid)
		    end
	    end;
	{idle, Error} ->
	    reply(Tag, Error),
	    State;
	_ ->
	    reply(Tag, {error, profiling}),
	    State
    end;

handle_req(#stop{reason = Reason}, Tag, State) ->
    PendingStop = get(pending_stop),
    case PendingStop of
	[] ->
	    put(stop_reason, Reason);
	_ ->
	    ok
    end,
    put(pending_stop, [Tag | PendingStop]),
    try_pending_stop(State);

%%----------------------
%% Server debug requests
%%----------------------

handle_req(#get_state{}, Tag, State) ->
    reply(Tag, {ok, get()}),
    State;

handle_req(#save_profile{file = File}, Tag, State) ->
    case get(profile_state) of
	{idle, ok} ->
	    reply(Tag, ets:tab2file(get(profile_table), File)),
	    State;
	{idle, Error} ->
	    reply(Tag, Error),
	    State;
	_ ->
	    reply(Tag, {error, profiling}),
	    State
    end;

handle_req(#load_profile{file = File}, Tag, State) ->
    case get(profile_state) of
	{idle, Result} ->
	    case ets:file2tab(File) of
		{ok, Table} ->
		    put(profile_state, {idle, ok}),
		    case Result of
			{error, no_profile} ->
			    ets:delete(put(profile_table, Table));
			_ ->
			    put(profile_table, Table)
		    end,
		    reply(Tag, ok),
		    State;
		Error ->
		    reply(Tag, Error),
		    State
	    end;
	_ ->
	    reply(Tag, {error, profiling}),
	    State
    end;

	    

handle_req(Request, Tag, State) ->
    io:format("~n~p:handle_req, unknown request - ~p~n", 
	      [?MODULE, Request]),
    reply(Tag, {error, unknown_request}), 
    State.

%%--------------------
%% Server handle_other
%%--------------------

handle_other({'EXIT', Pid, Reason} = Other, State) when pid(Pid); port(Pid) ->
    case {get(trace_state), get(trace_pid)} of
	{running, Pid} ->
	    trace_off(),
	    io:format("~n~p:handle_other, unexpected ~p (trace_pid)~n",
		      [?MODULE, Other]),
	    put(trace_state, idle),
	    erase(trace_type),
	    erase(trace_pid),
	    try_pending_stop(State);
	{stopping, Pid} ->
	    put(trace_state, idle),
	    erase(trace_pid),
	    reply(erase(trace_tag), result(Reason)),
	    try_pending_stop(State);
	_ ->
	    case {get(profile_state), get(profile_pid)} of
		{running, Pid} ->
		    Result = result(Reason),
		    put(profile_state, {idle, Result}),
		    erase(profile_type),
		    erase(profile_pid),
		    case erase(profile_close_dump) of
			true ->
			    file:close(erase(profile_dump));
			false ->
			    erase(profile_dump)
		    end,
		    reply(erase(profile_tag), Result),
		    try_pending_stop(State);
		_ ->
		    io:format("~n~p:handle_other, unexpected ~p~n",
			      [?MODULE, Other]),
		    State
	    end
    end;

handle_other(Other, State) ->
    io:format("~p:handle_other, unknown - ~p", 
			  [?MODULE, Other]),
    State.



%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

result(normal) ->
    ok;
result(Reason) ->
    {error, Reason}.

ensure_open(Pid, _Options) when pid(Pid) ->
    {already_open, Pid};
ensure_open([], _Options) ->
    {already_open, undefined};
ensure_open(Filename, Options) when atom(Filename); list(Filename) ->
    file:open(Filename, Options).

%%%---------------------------------
%%% Fairly generic utility functions
%%%---------------------------------



%% getopts(List, Options)) -> {DecodedOptions, RestOptions}
%%
%% List           = [Option]
%% Options        = [OptionTag]
%% Option         = OptionTag | OptionTuple
%% OptionTuple    = tuple(), element(1, OptionTuple) == OptionTag
%% OptionTag      = term()
%% OptionValue    = term()
%% DecodedOptions = [OptionList]
%% OptionList     = [Option]
%% RestOptions    = [Option]
%%
%% Searches List for options with tags defined in Options.
%% Returns DecodedOptions containing one OptionList per
%% OptionTag in Options, and RestOptions which contains
%% all terms from List not matching any OptionTag.
%%
%% All returned lists preserve the order from Options and List.
%%
%% An example:
%%     getopts([{f, 1}, e, {d, 2}, {c, 3, 4}, {b, 5}, a, b],
%%             [a, b, c, d]) ->
%%         {[[a], [{b, 5}, b],[{c, 3, 4}], [{d, 2}]], 
%%          [{f, 1}, e]}
%%
getopts(List, Options) when list(List), list(Options) ->
    getopts_1(Options, List, []).

getopts_1([], List, Result) ->
    {lists:reverse(Result), List};
getopts_1([Option | Options], List, Result) ->
    {Optvals, Remaining} = getopts_2(List, Option, [], []),
    getopts_1(Options, Remaining, [Optvals | Result]).

getopts_2([], _Option, Result, Remaining) ->
    {lists:reverse(Result), lists:reverse(Remaining)};
getopts_2([Option | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, [Option | Result], Remaining);
getopts_2([Optval | Tail], Option, Result, Remaining) 
  when element(1, Optval) == Option ->
    getopts_2(Tail, Option, [Optval | Result], Remaining);
getopts_2([Other | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, Result, [Other | Remaining]).

%% setopts(Options) -> List
%%
%% The reverse of getopts, almost.
%% Re-creates (approximately) List from DecodedOptions in 
%% getopts/2 above. The original order is not preserved, 
%% but rather the order from Options.
%% 
%% An example:
%%     setopts([[a], [{b,5}, b], [{c, 3, 4}], [{d,2}]]) ->
%%         [a, {b, 5}, b, {c, 3, 4}, {d, 2}]
%%
%% And a more generic example:
%%     {D, R} = getopts(L, O),
%%     L2 = setopts(D) ++ R
%% L2 will contain exactly the same terms as L, but not in the same order.
%%
setopts(Options) when list(Options) ->
    lists:append(Options).



spawn_3step(FunPrelude, FunAck, FunBody) ->
    spawn_3step(spawn, FunPrelude, FunAck, FunBody).

spawn_link_3step(FunPrelude, FunAck, FunBody) ->
    spawn_3step(spawn_link, FunPrelude, FunAck, FunBody).

spawn_3step(Spawn, FunPrelude, FunAck, FunBody) 
  when Spawn == spawn; Spawn == spawn_link ->
    Parent = self(),
    Ref = make_ref(),
    Child = 
	erlang:Spawn(
	  fun() ->
		  Ack = FunPrelude(Parent),
		  catch Parent ! {self(), Ref, Ack},
		  MRef = erlang:monitor(process, Parent),
		  receive
		      {Parent, Ref, Go} ->
			  erlang:demonitor(MRef),
			  receive {'DOWN', MRef, _, _, _} -> ok 
			  after 0 -> ok
			  end,
			  FunBody(Parent, Go);
		      {'DOWN', MRef, _, _, _} ->
			  ok
		  end
	  end),
    MRef = erlang:monitor(process, Child),
    receive
	{Child, Ref, Ack} ->
	    erlang:demonitor(MRef),
	    case catch {Ref, FunAck(Child, Ack)} of
		{Ref, Go} ->
		    receive {'DOWN', MRef, _, _, _} -> ok after 0 -> ok end,
		    catch Child ! {Parent, Ref, Go},
		    {ok, Child, Ack, Go};
		{'EXIT', Reason} ->
 		    exit(Child, kill),
		    receive 
			{'DOWN', MRef, _, _, _} ->
			    exit(Reason)
		    end;
		Exception ->
 		    exit(Child, kill),
		    receive 
			{'DOWN', MRef, _, _, _} ->
			    throw(Exception)
		    end
	    end;
	{'DOWN', MRef, _, _, Reason} ->
	    receive {Child, Ref, _Ack} -> ok after 0 -> ok end,
	    case Spawn of 
		spawn_link ->
		    receive {'EXIT', Reason} -> ok after 0 -> ok end;
		spawn ->
		    ok
	    end,
	    exit(Reason)
    end.



%%%---------------------------------
%%% Trace message handling functions
%%%---------------------------------

trace_off() ->
    case catch erlang:trace(all, false, [all, cpu_timestamp]) of
	{'EXIT', {badarg, _}} ->
	    erlang:trace(all, false, [all]);
	_ ->
	    ok
    end,
    erlang:trace_pattern(on_load, false, [local]),
    erlang:trace_pattern({'_', '_', '_'}, false, [local]),
    ok.



trace_on(Procs, Tracer, {V, CT}) ->
    case case CT of
	     cpu_time ->
		 case catch erlang:trace(all, true, [cpu_timestamp]) of
		     {'EXIT', {badarg, _}} ->
			 {error, not_supported};
		     _ ->
			 ok
		 end;
	     wallclock ->
		 ok
	 end
	of ok ->
	    MatchSpec = [{'_', [], [{message, {{cp, {caller}}}}]}],
	    erlang:trace_pattern(on_load, MatchSpec, [local]),
	    erlang:trace_pattern({'_', '_', '_'}, MatchSpec, [local]),
	    lists:foreach(
	      fun (P) ->
		      erlang:trace(P, true, [{tracer, Tracer} | trace_flags(V)])
	      end,
	      Procs),
	    ok;
	Error ->
	    Error
    end.



trace_flags(normal) ->
    [call, return_to, 
     running, procs, garbage_collection, 
     arity, timestamp, set_on_spawn];
trace_flags(verbose) ->
    [call, return_to, 
     send, 'receive',
     running, procs, garbage_collection, 
     timestamp, set_on_spawn].



%%%-------------------------------------
%%% Tracer process functions, for
%%% the 'dbg' tracer and for a lookalike 
%%%-------------------------------------

open_dbg_trace_port(Type, Spec) ->
    Fun = dbg:trace_port(Type, Spec),
    Fun().



spawn_link_dbg_trace_client(File, Table, GroupLeader, Dump) ->
    case dbg:trace_client(file, File, 
			  {fun handler/2, 
			   {init, GroupLeader, Table, Dump}}) of
	Pid when pid(Pid) ->
	    link(Pid),
	    Pid;
	Other ->
	    exit(Other)
    end.
			  



spawn_link_trace_client(Table, GroupLeader, Dump) ->
    {ok, Child, ready, go} = 
	spawn_link_3step(
	  fun(_Parent) ->
		  process_flag(trap_exit, true),
		  ready
	  end,
	  fun(_Child, ready) ->
		  go
	  end,
	  fun(Parent, go) ->
		  Init = {init, GroupLeader, Table, Dump},
		  tracer_loop(Parent, fun handler/2, Init)
	  end),
    Child.

tracer_loop(Parent, Handler, State) ->
    receive
	Trace when element(1, Trace) == trace ->
	    tracer_loop(Parent, Handler, Handler(Trace, State));
	Trace when element(1, Trace) == trace_ts ->
	    tracer_loop(Parent, Handler, Handler(Trace, State));
	{'EXIT', Parent, Reason} ->
	    handler(end_of_trace, State),
	    exit(Reason);
	_ ->
	    tracer_loop(Parent, Handler, State)
    end.



%%%---------------------------------
%%% Trace message handling functions
%%%---------------------------------

handler(end_of_trace, {init, GroupLeader, Table, Dump}) ->
    dump(Dump, start_of_trace),
    dump(Dump, end_of_trace),
    info(GroupLeader, Dump, "Empty trace!~n", []),
    end_of_trace(Table, undefined),
    done;
handler(end_of_trace, {_, TS, GroupLeader, Table, Dump}) ->
    dump(Dump, end_of_trace),
    info(GroupLeader, Dump, "End of trace!~n", []),
    end_of_trace(Table, TS),
    done;
handler(Trace, {init, GroupLeader, Table, Dump}) ->
    dump(Dump, start_of_trace),
    info(GroupLeader, Dump, "Reading trace data...~n", []),
    TS = trace_handler(Trace, Table, Dump),
    ets:insert(Table, #misc{id = first_ts, data = TS}),
    ets:insert(Table, #misc{id = last_ts_n, data = {TS, 1}}),
    {1, TS, GroupLeader, Table, Dump};
handler(Trace, {M, _, GroupLeader, Table, Dump}) ->
    N = M+1,
    info_dots(GroupLeader, Dump, N),
    TS = trace_handler(Trace, Table, Dump),
    ets:insert(Table, #misc{id = last_ts_n, data = {TS, N}}),
    {N, TS, GroupLeader, Table, Dump}.



end_of_trace(Table, TS) ->
    %%
    %% Close all process stacks, as if the processes exited.
    %%
    Procs = get(),
    put(table, Table),
    ?dbg(2, "get() -> ~p~n", [Procs]),
    lists:map(
      fun ({Pid, _}) when pid(Pid) ->
	      trace_exit(Table, Pid, TS)
      end,
      Procs),
    erase(),
    end_of_trace.



info_dots(GroupLeader, GroupLeader, _) ->
    ok;
info_dots(GroupLeader, _, N) ->
    if (N rem 100000) == 0 ->
	    io:format(GroupLeader, ",~n", []);
       (N rem 50000) == 0 ->
	    io:format(GroupLeader, ".~n", []);
       (N rem 1000) == 0 ->
	    io:put_chars(GroupLeader, ".");
       true ->
	    ok
    end.

info(GroupLeader, GroupLeader, _, _) ->
    ok;
info(GroupLeader, _, Format, List) ->
    io:format(GroupLeader, Format, List).

dump_stack(undefined, _, Term) ->
    Term;
dump_stack(Dump, Stack, Term) ->
    {Depth, _D} = 
	case Stack of
	    undefined ->
		{0, 0};
	    _ ->
		case length(Stack) of
		    0 ->
			{0, 0};
		    N ->
			{N, length(hd(Stack))}
		end
	end,
     io:format(Dump, "~s~p.~n", [lists:duplicate(Depth, "  "), parsify(Term)]),
    Term.

dump(undefined, Term) ->
    Term;
dump(Dump, Term) ->
    io:format(Dump, "~p.~n", [parsify(Term)]),
    Term.



%%%----------------------------------
%%% Profiling state machine functions
%%%----------------------------------



trace_handler({trace_ts, Pid, call, _MFA, _TS} = Trace, _Table, Dump) ->
    Stack = get(Pid),
    dump_stack(Dump, Stack, Trace),
    exit({incorrect_trace_data, ?MODULE, ?LINE,
	  [Trace, Stack]});
trace_handler({trace_ts, Pid, call, {_M, _F, Arity} = Func, 
	       {cp, CP}, TS} = Trace,
	      Table, Dump)
  when integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_call(Table, Pid, Func, TS, CP),
    TS;
trace_handler({trace_ts, Pid, call, {_M, _F, Args} = MFArgs, 
	       {cp, CP}, TS} = Trace,
	      Table, Dump)
  when list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_call(Table, Pid, Func, TS, CP),
    TS;
%%
%% return_to
trace_handler({trace_ts, Pid, return_to, undefined, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_return_to(Table, Pid, undefined, TS),
    TS;
trace_handler({trace_ts, Pid, return_to, {_M, _F, Arity} = Func, TS} = Trace,
	      Table, Dump)
  when integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_return_to(Table, Pid, Func, TS),
    TS;
trace_handler({trace_ts, Pid, return_to, {_M, _F, Args} = MFArgs, TS} = Trace,
	      Table, Dump)
  when list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_return_to(Table, Pid, Func, TS),
    TS;
%%
%% spawn
trace_handler({trace_ts, Pid, spawn, Child, MFArgs, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_spawn(Table, Child, MFArgs, TS, Pid),
    TS;
trace_handler({trace_ts, Pid, spawn, Child, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_spawn(Table, Child, undefined, TS, Pid),
    TS;
%%
%% exit
trace_handler({trace_ts, Pid, exit, _Reason, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_exit(Table, Pid, TS),
    TS;
%%
%% out
trace_handler({trace_ts, Pid, out, 0, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_out(Table, Pid, undefined, TS),
    TS;
trace_handler({trace_ts, Pid, out, {_M, _F, Arity} = Func, TS} = Trace,
	      Table, Dump)
  when integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_out(Table, Pid, Func, TS),
    TS;
trace_handler({trace_ts, Pid, out, {_M, _F, Args} = MFArgs, TS} = Trace,
	      Table, Dump)
  when list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_out(Table, Pid, Func, TS),
    TS;
%%
%% in
trace_handler({trace_ts, Pid, in, 0, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_in(Table, Pid, undefined, TS),
    TS;
trace_handler({trace_ts, Pid, in, {_M, _F, Arity} = Func, TS} = Trace,
	      Table, Dump)
  when integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_in(Table, Pid, Func, TS),
    TS;
trace_handler({trace_ts, Pid, in, {_M, _F, Args} = MFArgs, TS} = Trace,
	      Table, Dump)
  when list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_in(Table, Pid, Func, TS),
    TS;
%%
%% gc_start
trace_handler({trace_ts, Pid, gc_start, _Func, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_start(Table, Pid, TS),
    TS;
%%
%% gc_end
trace_handler({trace_ts, Pid, gc_end, _Func, TS} = Trace,
	      Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_end(Table, Pid, TS),
    TS;
%%
%% link
trace_handler({trace_ts, Pid, link, _OtherPid, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% unlink
trace_handler({trace_ts, Pid, unlink, _OtherPid, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% getting_linked
trace_handler({trace_ts, Pid, getting_linked, _OtherPid, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% getting_unlinked
trace_handler({trace_ts, Pid, getting_unlinked, _OtherPid, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% register
trace_handler({trace_ts, Pid, register, _Name, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% unregister
trace_handler({trace_ts, Pid, unregister, _Name, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% send
trace_handler({trace_ts, Pid, send, _OtherPid, _Msg, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% 'receive'
trace_handler({trace_ts, Pid, 'receive', _Msg, TS} = Trace,
	      _Table, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% Others
trace_handler(Trace, _Table, Dump) ->
    dump(Dump, Trace),
    exit({incorrect_trace_data, ?MODULE, ?LINE, [Trace]}).



%% The call stack
%% --------------
%%
%% The call stack can be modeled as a tree, with each level in the tree
%% corresponding to a real (non-tail recursive) stack entry, 
%% and the nodes within a level corresponding to tail recursive
%% calls on that real stack depth.
%%
%% Example:
%% a() ->
%%     b().
%% b() ->
%%     c(),
%%     d().
%% c() -> ok.
%% d() ->
%%     e(),
%%     c().
%% e() ->
%%     f().
%% f() -> ok.
%%
%% During the execution the call tree would be, for each call and return_to:
%%
%% a()    b()    c()    ->b    d()    e()    f()    ->d    c()    ->a
%%
%%     a      a      a      a      a      a      a      a      a      a
%%            |      |      |      |\     |\     |\     |\    /|\
%%            b      b      b      b d    b d    b d    b d  b d c
%%                   |                      |     /|
%%                   c                      e    e f
%%
%% The call tree is in this code represented as a two level list, 
%% which for the biggest tree (5 nodes) in the example above would be:
%%     [[{f, _}, {e, _}], [{d, _}, {b, _}], [{a, _}]]
%% where the undefined fields are timestamps of the calls to the
%% functions, and the function name fields are really 
%% {Module, Function, Arity} tuples.
%%
%% Since tail recursive calls can form an infinite loop, cycles 
%% within a tail recursive level must be collapsed or else the
%% stack (tree) size may grow towards infinity.



trace_call(Table, Pid, Func, TS, CP) ->
    Stack = get_stack(Pid),
    ?dbg(0, "trace_call(~p, ~p, ~p, ~p)~n~p~n", 
	 [Pid, Func, TS, CP, Stack]),
    case Stack of
	[] ->
	    init_log(Table, Pid, Func),
	    OldStack = 
		if CP == undefined ->
			Stack;
		   true ->
			[[{CP, TS}]]
		end,
	    put(Pid, trace_call_push(Table, Pid, Func, TS, OldStack));
	[[{suspend, _} | _] | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, CP, Stack]});
	[[{garbage_collect, _} | _] | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, CP, Stack]});
	[[{CP, _} | _], [{CP, _} | _] | _] ->
	    %% This is a difficult case - current function becomes
	    %% new stack top but is already pushed. It might be that
	    %% this call is actually tail recursive, or maybe not.
	    %% Assume tail recursive to not build the stack infinitely
	    %% and fix the problem at the next call after a return to
	    %% this level.
	    %%
	    %% This can be viewed as collapsing a very short stack
	    %% recursive stack cykle.
	    init_log(Table, Pid, Func),
	    put(Pid, trace_call_shove(Table, Pid, Func, TS, Stack));
	[[{CP, _} | _] | _] ->
	    %% Current function becomes new stack top -> stack push
	    init_log(Table, Pid, Func),
	    put(Pid, trace_call_push(Table, Pid, Func, TS, Stack));
	[_, [{CP, _} | _] | _] ->
	    %% Stack top unchanged -> no push == tail recursive call
	    init_log(Table, Pid, Func),
	    put(Pid, trace_call_shove(Table, Pid, Func, TS, Stack));
	[[{Func0, _} | _], [{Func0, _} | _], [{CP, _} | _] | _] ->
	    %% Artificial case that only should happen when 
	    %% stack recursive short cycle collapsing has been done,
	    %% otherwise CP should not occur so far from the stack front.
	    %%
	    %% It is a tail recursive call but fix the stack first.
	    init_log(Table, Pid, Func),
	    put(Pid, 
		trace_call_shove(Table, Pid, Func, TS,
				 trace_return_to_int(Table, Pid, Func0, TS,
						     Stack)));
	[[{_, TS0} | _] = Level0] ->
	    %% Current function known, but not stack top
	    %% -> assume tail recursive call
	    init_log(Table, Pid, Func),
	    OldStack =
		if CP == undefined ->
			Stack;
		   true ->
			[Level0, [{CP, TS0}]]
		end,
	    put(Pid, trace_call_shove(Table, Pid, Func, TS, OldStack));
	[_ | _] ->
	    %% Weird case when the stack is seriously f***ed up.
	    %% CP is not at stack top nor at previous stack top, 
	    %% which is impossible, if we had a correct stack view.
	    OldStack = 
		if CP == undefined ->
			%% Assume that CP is unknown because it is
			%% the stack bottom for the process, and that 
			%% the whole call stack is invalid. Waste it.
			trace_return_to_int(Table, Pid, CP, TS, Stack);
		   true ->
			%% Assume that we have collapsed a tail recursive
			%% call stack cykle too many. Introduce CP in
			%% the current tail recursive level so it at least
			%% gets charged for something.
			init_log(Table, Pid, CP),
			trace_call_shove(Table, Pid, CP, TS, Stack)
		end,
	    %% Regard this call as a stack push.
	    init_log(Table, Pid, Func),
	    put(Pid, trace_call_push(Table, Pid, Func, TS, OldStack));
	_ ->
	    %% Should not happen since all cases are covered above. (?)
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, CP, Stack]})
    end,
    ok.

%% Normal stack push
trace_call_push(Table, Pid, Func, TS, Stack) ->
    case Stack of
	[] ->
	    ok;
	_ ->
	    trace_clock(Table, Pid, TS, Stack, #clocks.own)
    end,
    NewStack = [[{Func, TS}] | Stack],
    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
    NewStack.

%% Tail recursive stack push
trace_call_shove(Table, Pid, Func, TS, Stack) ->
    trace_clock(Table, Pid, TS, Stack, #clocks.own),
    [[_ | NewLevel0] | NewStack1] = 
	case Stack of
	    [] ->
		[[{Func, TS}]];
	    [Level0 | Stack1] ->
		[trace_call_collapse([{Func, TS} | Level0]) | Stack1]
	end,
    NewStack = [[{Func, TS} | NewLevel0] | NewStack1],
    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
    NewStack.

%% Collapse tail recursive call stack cycles to prevent them from
%% growing to infinite length.
trace_call_collapse([]) ->
    [];
trace_call_collapse([_] = Stack) ->
    Stack;
trace_call_collapse([_, _] = Stack) ->
    Stack;
trace_call_collapse([_ | Stack1] = Stack) ->
    trace_call_collapse_1(Stack, Stack1, 1).

%% Find some other instance of the current function in the call stack
%% and try if that instance may be used as stack top instead.
trace_call_collapse_1(Stack, [], _) ->
    Stack;
trace_call_collapse_1([{Func0, _} | _] = Stack, [{Func0, _} | S1] = S, N) ->
    case trace_call_collapse_2(Stack, S, N) of
	true ->
	    S;
	false ->
	    trace_call_collapse_1(Stack, S1, N+1)
    end;
trace_call_collapse_1(Stack, [_ | S1], N) ->
    trace_call_collapse_1(Stack, S1, N+1).

%% Check if all caller/called pairs in the perhaps to be collapsed
%% stack segment (at the front) are present in the rest of the stack, 
%% and also in the same order.
trace_call_collapse_2(_, _, 0) ->
    true;
trace_call_collapse_2([{Func1, _} | [{Func2, _} | _] = Stack2],
	   [{Func1, _} | [{Func2, _} | _] = S2],
	   N) ->
    trace_call_collapse_2(Stack2, S2, N-1);
trace_call_collapse_2([{Func1, _} | _], [{Func1, _} | _], _N) ->
    false;
trace_call_collapse_2(_Stack, [_], _N) ->
    false;
trace_call_collapse_2(Stack, [_ | S], N) ->
    trace_call_collapse_2(Stack, S, N);
trace_call_collapse_2(_Stack, [], _N) ->
    false.



trace_return_to(Table, Pid, Func, TS) ->
    Stack = get_stack(Pid),
    ?dbg(0, "trace_return_to(~p, ~p, ~p)~n~p~n", 
	 [Pid, Func, TS, Stack]),
    case Stack of
	[[{suspend, _} | _] | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]});
	[[{garbage_collect, _} | _] | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]});
	[_ | _] ->
	    put(Pid, trace_return_to_int(Table, Pid, Func, TS, Stack));
	[] ->
	    put(Pid, trace_return_to_int(Table, Pid, Func, TS, Stack));
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]})
    end,
    ok.

trace_return_to_int(Table, Pid, Func, TS, Stack) ->
    case trace_return_to_2(Table, Pid, Func, TS, Stack) of
	{undefined, _} ->
	    [[{Func, TS}] | Stack];
	{[[{Func, _} | Level0] | Stack1] = NewStack, _} ->
	    trace_clock(Table, Pid, TS, NewStack, #clocks.own),
	    [[{Func, TS} | Level0] | Stack1];
	{NewStack, _} ->
	    trace_clock(Table, Pid, TS, NewStack, #clocks.own),
	    NewStack
    end.

%% A list of charged functions is passed around to assure that 
%% any function is charged with ACC time only once - the first time
%% it is encountered. The function trace_return_to_1 is called only
%% for the front of a tail recursive level, and if the front 
%% does not match the returned-to function, trace_return_to_2
%% is called for all functions within the tail recursive level.
%%
%% Charging is done in reverse order, i.e from stack rear to front.

%% Search the call stack until the returned-to function is found at
%% a tail recursive level's front, and charge it with ACC time.
trace_return_to_1(_, _, undefined, _, []) ->
    {[], []};
trace_return_to_1(_, _, _, _, []) ->
    {undefined, []};
trace_return_to_1(Table, Pid, Func, TS, 
		  [[{Func, _} | Level0] | Stack1] = Stack) ->
    %% Match at front of tail recursive level
    Charged = trace_return_to_3([Level0 | Stack1], []),
    case lists:member(Func, Charged) of
	false ->
	    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
	    {Stack, [Func | Charged]};
	true ->
	    {Stack, Charged}
    end;
trace_return_to_1(Table, Pid, Func, TS, Stack) ->
    trace_return_to_2(Table, Pid, Func, TS, Stack).

%% Charge all functions within one tail recursive level, 
%% from rear to front, with ACC time.
trace_return_to_2(Table, Pid, Func, TS, [] = Stack) ->
    trace_return_to_1(Table, Pid, Func, TS, Stack);
trace_return_to_2(Table, Pid, Func, TS, [[] | Stack1]) ->
    trace_return_to_1(Table, Pid, Func, TS, Stack1);
trace_return_to_2(Table, Pid, Func, TS,
		  [[{Func0, _} | Level1] | Stack1] = Stack) ->
    case trace_return_to_2(Table, Pid, Func, TS, [Level1 | Stack1]) of
	{undefined, _} = R ->
	    R;
	{NewStack, Charged} = R ->
	    case lists:member(Func0, Charged) of
		false ->
		    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
		    {NewStack, [Func0 | Charged]};
		true ->
		    R
	    end
    end.

%% Return a flat list of all function names in the given stack
trace_return_to_3([], R) ->
    R;
trace_return_to_3([[] | Stack1], R) ->
    trace_return_to_3(Stack1, R);
trace_return_to_3([[{Func0, _} | Level0] | Stack1], R) ->
    trace_return_to_3([Level0 | Stack1], [Func0 | R]).



trace_spawn(Table, Pid, MFArgs, TS, Parent) ->
    Stack = get(Pid),
    ?dbg(0, "trace_spawn(~p, ~p, ~p, ~p)~n~p~n", 
	 [Pid, MFArgs, TS, Parent, Stack]),
    case Stack of
	undefined ->
	    put(Pid, trace_call_push(Table, Pid, suspend, TS, [])),
	    ets:insert(Table, #proc{id = Pid, parent = Parent,
				    spawned_as = MFArgs});
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, MFArgs, TS, Parent, Stack]})
    end.



trace_exit(Table, Pid, TS) ->
    Stack = erase(Pid),
    ?dbg(0, "trace_exit(~p, ~p)~n~p~n", [Pid, TS, Stack]),
    case Stack of
	undefined ->
	    ok;
	[] ->
	    ok;
	[_ | _] = Stack ->
	    trace_return_to_int(Table, Pid, undefined, TS, Stack),
	    ok;
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, TS, Stack]})
    end,
    ok.



trace_out(Table, Pid, Func, TS) ->    
    Stack = get_stack(Pid),
    ?dbg(0, "trace_out(~p, ~p, ~p)~n~p~n", [Pid, Func, TS, Stack]),
    case Stack of
	[] ->
	    put(Pid, trace_call_push(Table, Pid, suspend, TS, Stack));
	[[{suspend, _} | _] | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]});
% 	[[{Func, _} | _] | _] ->
% 	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
% 	    NewStack = push(suspend, TS, Stack),
% 	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
% 	    put(Pid, NewStack);
% 	[[{garbage_collect, _} | _] | _] ->
% 	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
% 	    NewStack = push(suspend, TS, Stack),
% 	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
% 	    put(Pid, NewStack);
	[_ | _] ->
	    put(Pid, trace_call_push(Table, Pid, suspend, TS, Stack));
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]})
    end.



trace_in(Table, Pid, Func, TS) ->	    
    Stack = get(Pid),
    ?dbg(0, "trace_in(~p, ~p, ~p)~n~p~n", [Pid, Func, TS, Stack]),
    case Stack of
	undefined ->
	    put(Pid, []);
	[] ->
	    ok;
	[[{suspend, _}]] ->
	    put(Pid, trace_return_to_int(Table, Pid, undefined, TS, Stack));
	[[{suspend, _}] | [[{Func1, _} | _] | _]] ->
	    put(Pid, trace_return_to_int(Table, Pid, Func1, TS, Stack));
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]})
    end.



trace_gc_start(Table, Pid, TS) ->    
    Stack = get_stack(Pid),
    ?dbg(0, "trace_gc_start(~p, ~p)~n~p~n", [Pid, TS, Stack]),
    case Stack of
	_ when list(Stack) ->
	    put(Pid, trace_call_push(Table, Pid, garbage_collect, TS, Stack));
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, TS, Stack]})
    end.



trace_gc_end(Table, Pid, TS) ->	    
    Stack = get(Pid),
    ?dbg(0, "trace_gc_end(~p, ~p)~n~p~n", [Pid, TS, Stack]),
    case Stack of
	undefined ->
	    put(Pid, []);
	[] ->
	    ok;
	[[{garbage_collect, _}]] ->
	    put(Pid, trace_return_to_int(Table, Pid, undefined, TS, Stack));
	[[{garbage_collect, _}], [{Func1, _} | _] | _] ->
	    put(Pid, trace_return_to_int(Table, Pid, Func1, TS, Stack));
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, TS, Stack]})
    end.



%%%-----------------------------------------
%%% Statistics calculating support functions
%%%-----------------------------------------



get_stack(Id) ->
    case get(Id) of
	undefined ->
	    [];
	Stack ->
	    Stack
    end.



mfarity({M, F, Args}) when list(Args) ->
    {M, F, length(Args)};
mfarity(MFA) ->
    MFA.



init_log(_Table, _Id, suspend) ->
    ok;
init_log(_Table, _Id, void) ->
    ok;
init_log(Table, Id, Entry) ->
    case ets:lookup(Table, Id) of
	[#proc{init_cnt = 0}] ->
	    ok;
	[#proc{init_cnt = N, init_log = L} = Proc] ->
	    ets:insert(Table, 
		       Proc#proc{init_cnt = N-1, init_log = [Entry | L]});
	[] ->
	    ok
    end.


trace_clock(Table, Pid, T, 
	    [[{garbage_collect, TS0}], [{suspend, _}]], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, undefined, garbage_collect, Clock);
trace_clock(Table, Pid, T, 
	    [[{garbage_collect, TS0}], [{suspend, _}], [{Func2, _} | _] | _],
	    Clock) ->
    trace_clock_1(Table, Pid, T, TS0, Func2, garbage_collect, Clock);
trace_clock(Table, Pid, T, [[{Func0, TS0}, {Func1, _} | _] | _], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, Func1, Func0, Clock);
trace_clock(Table, Pid, T, [[{Func0, TS0}], [{Func1, _} | _] | _], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, Func1, Func0, Clock);
trace_clock(Table, Pid, T, [[{Func0, TS0}]], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, undefined, Func0, Clock);
trace_clock(_, _, _, [], _) ->
    void.

trace_clock_1(Table, Pid, _, _, Caller, suspend, #clocks.own) ->
    clock_add(Table, {Pid, Caller, suspend}, #clocks.own, 0);
trace_clock_1(Table, Pid, T, TS, Caller, Func, Clock) ->
    clock_add(Table, {Pid, Caller, Func}, Clock,
	      if integer(T) ->
		      T;
		 true ->
		      ts_sub(T, TS)
	      end).

clock_add(Table, Id, Clock, T) ->
    ?dbg(1, "clock_add(Table, ~w, ~w, ~w)~n", [Id, Clock, T]),
    case (catch ets:update_counter(Table, Id, {Clock, T})) of
	{'EXIT', _} ->
	    ets:insert(Table, #clocks{id = Id}),
	    case ets:update_counter(Table, Id, {Clock, T}) of
		X when X >= 0 -> ok;
		X -> ?dbg(0, "Negative counter value ~p ~p ~p ~p~n",
			  [X, Id, Clock, T])
	    end;
	V ->
	    V
    end.

clocks_add(Table, #clocks{id = Id} = Clocks) ->
    ?dbg(1, "clocks_add(Table, ~w)~n", [Clocks]),
    case ets:lookup(Table, Id) of
	[Clocks0] ->
	    ets:insert(Table, clocks_sum(Clocks, Clocks0, Id));
	[] ->
	    ets:insert(Table, Clocks)
    end.



clocks_sum(#clocks{id = _Id1, 
		   cnt = Cnt1, 
		   own = Own1, 
		   acc = Acc1}, 
	   #clocks{id = _Id2, 
		   cnt = Cnt2, 
		   own = Own2, 
		   acc = Acc2}, 
	   Id) ->
    #clocks{id = Id,
	    cnt = Cnt1 + Cnt2,
	    own = Own1 + Own2,
	    acc = Acc1 + Acc2}.



ts_sub({A, B, C} = T, {A0, B0, C0} = T0) ->
    X = ((((A-A0)*1000000) + (B-B0))*1000000) + C - C0,
    if X >= 0 -> ok;
       true -> ?dbg(9, "Negative counter value ~p ~p ~p~n",
		    [X, T, T0])
    end,
    X;
ts_sub(_, _) ->
    undefined.



%%%--------------------------------
%%% Profile data analysis functions
%%%--------------------------------



do_analyse(Table, 
	   #analyse{group_leader = GroupLeader,
		    dest = Io,
		    cols = Cols0,
		    callers = PrintCallers,
		    sort = Sort,
		    totals = PrintTotals,
		    details = PrintDetails} = Analyse) ->
    ?dbg(5, "do_analyse(~p, ~p)~n", [Table, Analyse]),
    Waste = 11,
    MinCols = Waste + 12, %% We need Width >= 1
    Cols = if Cols0 < MinCols -> MinCols; true -> Cols0 end,
    Width = (Cols-Waste) div 12,
    FnameWidth = Cols - Waste - 5*Width,
    Dest = {Io, [FnameWidth, Width, 2*Width, 2*Width]},
    SortElement = case Sort of
		      own ->
			  #clocks.own;
		      acc ->
			  #clocks.acc
		  end,
    %%
    %% Clean out the process dictionary before the next step
    %%
    Erase = erase(),
    ?dbg(2, "erase() -> ~p~n", [Erase]),
    %%
    %% Process the collected data and spread it to 3 places:
    %% * Per {process, caller, func}. Stored in the process dictionary.
    %% * Sum per process. Stored in an ets table.
    %% * Extra info per process. Stored in another ets table.
    %%
    io:format(GroupLeader, "Processing data...~n", []),
    PidTable = ets:new(?MODULE, [set, private, {keypos, #clocks.id}]),
    ProcTable = ets:new(?MODULE, [set, private, {keypos, #proc.id}]),
    ets_select_foreach(
      Table, [{'_', [], ['$_']}], 100,
      fun (#clocks{id = {Pid, Caller, Func}} = Clocks) ->
	      case PrintDetails of
		  true ->
		      funcstat_pd(Pid, Caller, Func, Clocks),
		      clocks_add(PidTable, Clocks#clocks{id = Pid});
		  false ->
		      ok
	      end,
	      clocks_add(PidTable, Clocks#clocks{id = totals}),
	      case PrintTotals of
		  true ->
		      funcstat_pd(totals, Caller, Func, Clocks);
		  false ->
		      ok
	      end;
	  (#proc{} = Proc) ->
	      ets:insert(ProcTable, Proc);
	  (#misc{} = Misc) ->
	      ets:insert(ProcTable, Misc)
      end),
    ?dbg(3, "get() -> ~p~n", [get()]),
    {FirstTS, LastTS, _TraceCnt} = 
	case {ets:lookup(ProcTable, first_ts), 
	      ets:lookup(ProcTable, last_ts_n)} of
	    {[#misc{data = FTS}], [#misc{data = {LTS, TC}}]} 
	    when FTS /= undefined, LTS /= undefined ->
		{FTS, LTS, TC};
	    _ ->
		exit(empty_trace)
	end,
    Totals0 = 
	case ets:lookup(PidTable, totals) of
	    [T0] ->
		ets:delete(PidTable, totals),
		T0;
	    _ ->
		exit(empty_trace)
	end,
    Totals = Totals0#clocks{acc = ts_sub(LastTS, FirstTS)},
    ?dbg(3, "Totals0 =  ~p~n", [Totals0]),
    ?dbg(3, "PidTable =  ~p~n", [ets:tab2list(PidTable)]),
    ?dbg(3, "ProcTable =  ~p~n", [ets:tab2list(ProcTable)]),
    ?dbg(4, "Totals = ~p~n", [Totals]),
    %%
    %% Reorganize the process dictionary by Pid.
    %%
    lists:foreach(
      fun ({{Pid, _Func}, Funcstat}) ->
	      put(Pid, [Funcstat | case get(Pid) of
				       undefined -> [];
				       Other -> Other
				   end])
      end,
      erase()),
    ?dbg(4, "get() -> ~p~n", [get()]),
    %%
    %% Sort the processes
    %%
    PidSorted = 
	postsort_r(
	  lists:sort(
	    ets:select(PidTable, 
		       [{'_', [], [[{element, #clocks.own, '$_'} | '$_']]}]))),
    ?dbg(4, "PidSorted = ~p~n", [PidSorted]),
    %%
    %% Print the functions per process
    %%
    io:format(GroupLeader, "Creating output...~n", []),
    println(Dest, "%% ", [], "Analysis results:", ""),
    println(Dest, "{  ", analysis_options, ",", ""),
    println(Dest, " [{", {callers, PrintCallers}, "},", ""),
    println(Dest, "  {", {sort, Sort}, "},", ""),
    println(Dest, "  {", {totals, PrintTotals}, "},", ""),
    println(Dest, "  {", {details, PrintDetails}, "}]}.", ""),
    println(Dest),
    lists:foreach(
      fun ({#clocks{} = Clocks, ProcOrPid, FuncstatList}) ->
	      println(Dest, "%  ", head, "", ""),
	      case ProcOrPid of
		  #proc{} ->
		      println(Dest, "[{ ", Clocks, "},", "%%"),
		      print_proc(Dest, ProcOrPid);
		  totals ->
		      println(Dest, "[{ ", Clocks, "}].", "%%%");
		  _ when pid(ProcOrPid) ->
		      println(Dest, "[{ ", Clocks, "}].", "%%")
	      end,
	      println(Dest),
	      lists:foreach(
		fun (#funcstat{callers_sum = CallersSum, 
%			       called_sum = CalledSum, 
			       callers = Callers, 
			       called = Called}) ->
			case {PrintCallers, Callers} of
%			    {true, []} ->
%				ok;
			    {true, _} ->
				print_callers(Dest, Callers),
				println(Dest, " { ", CallersSum, "},", "%"),
				print_called(Dest, Called),
				println(Dest);
			    {false, _} ->
				println(Dest, "{  ", CallersSum, "}.", "")
			end,
			ok
		end,
		%% Sort the functions within the process, 
		%% and the callers and called within the function.
		funcstat_sort_r(FuncstatList, SortElement)),
	      println(Dest)
      end,
      %% Look up the processes in sorted order
      lists:map(
	fun (#clocks{id = Pid} = Clocks) -> 
		Proc = case ets:lookup(ProcTable, Pid) of
			   [] -> Pid;
			   [ProcX] -> ProcX
		       end,
		FuncstatList = 
		    case get(Pid) of
			undefined ->
			    [];
			FL ->
			    FL
		    end,
		{Clocks, Proc, FuncstatList}
	end,
	case PrintDetails of
	    true ->
		[Totals | PidSorted];
	    false ->
		[Totals]
	end)),
    %%
    %% Cleanup
    %%
    ets:delete(PidTable),
    ets:delete(ProcTable),
    io:format(GroupLeader, "Done!~n", []),
    ok.



%%----------------------------
%% Analysis printout functions
%%----------------------------



print_proc({undefined, _}, _) ->
    ok;
print_proc(Dest, 
	   #proc{id = _Pid, 
		 parent = Parent, 
		 spawned_as = SpawnedAs,
		 init_log = InitLog}) ->
    case {Parent, SpawnedAs, InitLog} of
	{undefined, undefined, []} ->
	    println(Dest, "   ", [], "].", "");
	{_, undefined, []} ->
	    println(Dest, " { ", {spawned_by, parsify(Parent)}, "}].", "");
	_ ->
	    println(Dest, " { ", {spawned_by, parsify(Parent)}, "},", ""),
	    case {SpawnedAs, InitLog} of
		{_, []} ->
		    println(Dest, " { ",
			    {spawned_as, SpawnedAs},
			    "}].", "");
		{undefined, _} ->
		    println(Dest, " { ", 
			    {initial_calls, lists:reverse(InitLog)},
			    "}].", "");
		_ ->
		    println(Dest, " { ",
			    {spawned_as, SpawnedAs},
			    "},", ""),
		    println(Dest, " { ",
			    {initial_calls, lists:reverse(InitLog)},
			    "}].", "")
	    end
    end.



print_callers(Dest, []) ->
    println(Dest, "{[", [], "],", "");
print_callers(Dest, [Clocks]) ->
    println(Dest, "{[{", Clocks, "}],", "");
print_callers(Dest, [Clocks | Tail]) ->
    println(Dest, "{[{", Clocks, "},", ""),
    print_callers_1(Dest, Tail).

print_callers_1(Dest, [Clocks]) ->
    println(Dest, "  {", Clocks, "}],", "");
print_callers_1(Dest, [Clocks | Tail]) ->
    println(Dest, "  {", Clocks, "},", ""),
    print_callers_1(Dest, Tail).



print_func(Dest, Clocks) ->
    println(Dest, " { ", Clocks, "},", "%").



print_called(Dest, []) ->
    println(Dest, " [", [], "]}.", "");
print_called(Dest, [Clocks]) ->
    println(Dest, " [{", Clocks, "}]}.", "");
print_called(Dest, [Clocks | Tail]) ->
    println(Dest, " [{", Clocks, "},", ""),
    print_called_1(Dest, Tail).

print_called_1(Dest, [Clocks]) ->
    println(Dest, "  {", Clocks, "}]}.", "");
print_called_1(Dest, [Clocks | Tail]) ->
    println(Dest, "  {", Clocks, "},", ""),
    print_called_1(Dest, Tail).



println({undefined, _}) ->
    ok;
println({Io, _}) ->
    io:nl(Io).

println({undefined, _}, _Head,
	_, 
	_Tail, _Comment) ->
    ok;
println({Io, [W1, W2, W3, W4]}, Head,
	#clocks{id = Pid, cnt = Cnt, acc = _, own = Own},
	Tail, Comment) when pid(Pid) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  flat_format(parsify(Pid), $,, W1),
		  flat_format(Cnt, $,, W2, right),
		  flat_format(undefined, $,, W3, right),
		  flat_format(Own*0.001, [], W4-1, right),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, [W1, W2, W3, W4]}, Head,
	#clocks{id = {_M, _F, _A} = Func, cnt = Cnt, acc = Acc, own = Own},
	Tail, Comment) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  flat_format(Func, $,, W1),
		  flat_format(Cnt, $,, W2, right),
		  flat_format(Acc*0.001, $,, W3, right),
		  flat_format(Own*0.001, [], W4-1, right),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, [W1, W2, W3, W4]}, Head,
	#clocks{id = Id, cnt = Cnt, acc = Acc, own = Own},
	Tail, Comment) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  flat_format(parsify(Id), $,, W1),
		  flat_format(Cnt, $,, W2, right),
		  flat_format(Acc*0.001, $,, W3, right),
		  flat_format(Own*0.001, [], W4-1, right),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, [W1, W2, W3, W4]}, Head,
	head,
	Tail, Comment) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  pad(" ", $ , W1),
		  pad($ , " CNT ", W2),
		  pad($ , " ACC ", W3),
		  pad($ , " OWN", W4-1),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, _}, Head,
	[],
	Tail, Comment) ->
    io:format(Io, "~s~s~s~n",
	      [pad(Head, $ , 3), Tail, Comment]);
println({Io, _}, Head,
	{Tag, Term},
	Tail, Comment) ->
    io:format(Io, "~s~p, ~p~s~s~n",
	      [pad(Head, $ , 3), parsify(Tag), parsify(Term), Tail, Comment]);
println({Io, _}, Head,
	Term,
	Tail, Comment) ->
    io:format(Io, "~s~p~s~s~n",
	      [pad(Head, $ , 3), parsify(Term), Tail, Comment]).



%%%--------------------------
%%% Sorting support functions
%%%--------------------------


%% Add a Clocks record to the callers and called funcstat records
%% in the process dictionary.
%% 
funcstat_pd(Pid, Func1, Func0, Clocks) ->
    put({Pid, Func0},
	case get({Pid, Func0}) of
	    undefined ->
		#funcstat{callers_sum = Clocks#clocks{id = Func0}, 
			  called_sum = #clocks{id = Func0},
			  callers = [Clocks#clocks{id = Func1}]};
	    #funcstat{callers_sum = CallersSum,
		      callers = Callers} = FuncstatCallers ->
		FuncstatCallers#funcstat{
		  callers_sum = clocks_sum(CallersSum, Clocks, Func0),
		  callers = [Clocks#clocks{id = Func1} | Callers]}
	end),
    put({Pid, Func1},
        case get({Pid, Func1}) of
            undefined ->
                #funcstat{callers_sum = #clocks{id = Func1}, 
                          called_sum = Clocks#clocks{id = Func1},
                          called = [Clocks#clocks{id = Func0}]};
            #funcstat{called_sum = CalledSum,
                      called = Called} = FuncstatCalled ->
                FuncstatCalled#funcstat{
                  called_sum = clocks_sum(CalledSum, Clocks, Func1),
                  called = [Clocks#clocks{id = Func0} | Called]}
        end).



%% Sort a list of funcstat records,
%% and sort the callers and called lists within the funcstat record.
funcstat_sort_r(FuncstatList, Element) ->
    funcstat_sort_r_1(FuncstatList, Element, []).

funcstat_sort_r_1([], _, R) ->
    postsort_r(sort(R));
funcstat_sort_r_1([#funcstat{callers_sum = #clocks{} = Clocks,
			     callers = Callers,
			     called = Called} = Funcstat
		   | L], 
		  Element,
		  R) ->
    funcstat_sort_r_1(L, 
		      Element, 
		      [[element(Element, Clocks)
			|Funcstat#funcstat{
			   callers = clocks_sort_r(Callers, Element),
			   called = clocks_sort_r(Called, Element)}]
		       | R]).



%% Sort a list of clocks records.
clocks_sort_r(L, E) ->
    clocks_sort_r_1(L, E, []).

clocks_sort_r_1([], _, R) ->
    postsort_r(sort(R));
clocks_sort_r_1([#clocks{} = C | L], E, R) ->
    clocks_sort_r_1(L, E, [[element(E, C)|C] | R]).


%% Take a list of terms with sort headers and strip the headers.
postsort_r(L) ->
    postsort_r(L, []).

postsort_r([], R) ->
    R;
postsort_r([[_|C] | L], R) ->
    postsort_r(L, [C | R]).



%%%----------------------------------------------------------------------
%%% Fairly generic support functions
%%%

%% Standard format and flatten.
flat_format(F, Trailer) when float(F) ->
    flatten([io_lib:format("~.3f", [F]), Trailer]);
flat_format(W, Trailer) ->
    flatten([io_lib:format("~p", [W]), Trailer]).

%% Format, flatten, and pad.
flat_format(Term, Trailer, Width) ->
    flat_format(Term, Trailer, Width, left).

flat_format(Term, Trailer, Width, left) ->
    flat_format(Term, Trailer, Width, {left, $ });
flat_format(Term, Trailer, Width, {left, Filler}) ->
    pad(flat_format(Term, Trailer), Filler, Width);
flat_format(Term, Trailer, Width, right) ->
    flat_format(Term, Trailer, Width, {right, $ });
flat_format(Term, Trailer, Width, {right, Filler}) ->
    pad(Filler, flat_format(Term, Trailer), Width).



%% Left pad a string using a given char.
pad(Char, L, Size) when integer(Char), list(L), integer(Size) ->
    List = lists:flatten(L),
    Length = length(List),
    if Length >= Size ->
	    List;
       true ->
	    lists:append(lists:duplicate(Size - Length, Char), List)
    end;
%% Right pad a string using a given char.
pad(L, Char, Size) when list(L), integer(Char), integer(Size) ->
    List = lists:flatten(L),
    Length = length(List),
    if Length >= Size ->
	    List;
       true ->
	    lists:append(List, lists:duplicate(Size - Length, Char))
    end.



ets_select_foreach(Table, MatchSpec, Limit, Fun) ->
    ets:safe_fixtable(Table, true),
    ets_select_foreach_1(ets:select(Table, MatchSpec, Limit), Fun).

ets_select_foreach_1('$end_of_table', _) ->
    ok;
ets_select_foreach_1({Matches, Continuation}, Fun) ->
    ?dbg(2, "Matches = ~p~n", [Matches]),
    lists:foreach(Fun, Matches),
    ets_select_foreach_1(ets:select(Continuation), Fun).



%% Converts the parts of a deep term that are not parasable when printed
%% with io:format() into their string representation.
parsify([]) ->
    [];
parsify([Hd | Tl]) ->
    [parsify(Hd) | parsify(Tl)];
parsify({A, B}) ->
    {parsify(A), parsify(B)};
parsify({A, B, C}) ->
    {parsify(A), parsify(B), parsify(C)};
parsify(Tuple) when tuple(Tuple) ->
    list_to_tuple(parsify(tuple_to_list(Tuple)));
parsify(Pid) when pid(Pid) ->
    erlang:pid_to_list(Pid);
parsify(Port) when port(Port) ->
    erlang:port_to_list(Port);
parsify(Ref) when reference(Ref) -> 
    erlang:ref_to_list(Ref);
parsify(Fun) when function(Fun) ->
    erlang:fun_to_list(Fun);
parsify(Term) ->
    Term.



%% A simple loop construct.
%%
%% Calls 'Fun' with argument 'Start' first and then repeatedly with
%% its returned value (state) until 'Fun' returns 'Stop'. Then
%% the last state value that was not 'Stop' is returned.

% iterate(Start, Done, Fun) when function(Fun) ->
%     iterate(Start, Done, Fun, Start).

% iterate(Done, Done, Fun, I) ->
%     I;
% iterate(I, Done, Fun, _) ->
%     iterate(Fun(I), Done, Fun, I).
