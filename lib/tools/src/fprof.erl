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
% 	 spawn/1, spawn/2, spawn/3, spawn/4,
% 	 spawn_link/1, spawn_link/2, spawn_link/3, spawn_link/4,
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

% spawn(Fun) when function(Fun) ->
%     spawn(Fun, []);
% spawn(A) ->
%     erlang:fault(badarg, [A]).

% spawn(Fun, Options) when function(Fun), list(Options) ->
%     spawn_1(spawn, Fun, []);
% spawn(A, B) ->
%     erlang:fault(badarg, [A, B]).

% spawn(M, F, Args) when atom(M), atom(F), list(Args) ->
%     spawn(M, F, Args, []);
% spawn(A, B, C) ->
%     erlang:fault(badarg, [A, B, C]).

% spawn(M, F, Args, Options) when atom(M), atom(F), list(Args), list(Options) ->
%     spawn_1(spawn, {M, F, Args}, Options);
% spawn(A, B, C, D) ->
%     erlang:fault(badarg, [A, B, C, D]).


% spawn_link(Fun) when function(Fun) ->
%     spawn_link(Fun, []);
% spawn_link(A) ->
%     erlang:fault(badarg, [A]).

% spawn_link(Fun, Options) when function(Fun), list(Options) ->
%     spawn_1(spawn_link, Fun, []);
% spawn_link(A, B) ->
%     erlang:fault(badarg, [A, B]).

% spawn_link(M, F, Args) when atom(M), atom(F), list(Args) ->
%     spawn_link(M, F, Args, []);
% spawn_link(A, B, C) ->
%     erlang:fault(badarg, [A, B, C]).

% spawn_link(M, F, Args, Options) when atom(M), atom(F), list(Args), list(Options) ->
%     spawn_1(spawn_link, {M, F, Args}, Options);
% spawn_link(A, B, C, D) ->
%     erlang:fault(badarg, [A, B, C, D]).


% spawn_1(Spawn, Function, Options) ->
%     {[{start, _}, {procs, Procs}], Options_1} = 
% 	getopts(Options, [start, procs]),
%     Procs_1 = case Procs of
% 		  [[P]] when list(P) ->
% 		      P;
% 		  _ ->
% 		      []
% 	      end,
%     {ok, Child, ready, go} = 
% 	spawn_3step(
% 	  Spawn,
% 	  fun(_Parent) ->
% 		  ready
% 	  end,
% 	  fun(Child, ready) ->
% 		  trace([start, {procs, [Child | Procs_1]} | Options_1]),
% 		  go
% 	  end,
% 	  fun(_Parent, go) ->
% 		  case Function of
% 		      {M, F, Args} ->
% 			  erlang:apply(M, F, Args);
% 		      Fun ->
% 			  erlang:apply(Fun, [])
% 		  end
% 	  end),
%     Child.



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
    {[{start, _}, {procs, Procs}, {continue, Continue}], Options_1} = 
	getopts(Options, [start, procs, continue]),
    Procs_1 = case Procs of
		  [[P]] when list(P) ->
		      P;
		  _ ->
		      []
	      end,
    case Continue of
	[] ->
	    apply_start_stop(Function, Args, Procs_1, Options_1);
	[[]] ->
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
		  cols}).       % Integer

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
    call(#trace_stop{});
trace(verbose) ->
    trace([start, verbose]);
trace(Option) when atom(Option) ->
    trace([Option]);
trace([stop]) ->
    call(#trace_stop{});
trace(Options) when list(Options) ->
    case getopts(Options, [start, stop, 
			   procs, verbose, 
			   file, tracer]) of
	{[{start, []}, {stop, [[]]},
	  {procs, []}, {verbose, []},
	  {file, []}, {tracer, []}],
	 []} ->
	    call(#trace_stop{});
	{[{start, Start}, {stop, []},
	  {procs, Procs}, {verbose, Verbose},
	  {file, File}, {tracer, Tracer}],
	 []} ->
	    case Start of
		[[]] ->
		    ok;
		_ ->
		    erlang:fault(badarg, [Options])
	    end,
	    {Type, Dest} = case {File, Tracer} of
			       {[], [[Pid]]} when pid(Pid); port(Pid) ->
				   {tracer, Pid};
			       {[[]], []} ->
				   {file, ?TRACE_FILE};
			       {[[F]], []} ->
				   {file, F};
			       {[], []} ->
				   {file, ?TRACE_FILE};
			       _ ->
				   erlang:fault([badarg, [Options]])
			   end,
	    call(#trace_start{procs = case Procs of
					  [] ->
					      [self()];
					  [[P]] when list(P) ->
					      P;
					  [[P]] ->
					      [P];
					  _ ->
					      erlang:fault(badarg, [Options])
				      end,
			      mode = case Verbose of
					 [] ->
					     normal;
					 [[]] -> 
					     verbose;
					 _ ->
					     erlang:fault(badarg, [Options])
				     end,
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
profile(Options) when list(Options) ->
    case getopts(Options, [start, stop, file, 
			   dump, append]) of
	{[{start, [[]]}, {stop, []}, {file, []}, 
	  {dump, Dump}, {append, Append}],
	 []} ->
	    call(#profile_start{group_leader = group_leader(),
				dump = case Dump of
					   [] ->
					       [];
					   [[]] ->
					       group_leader();
					   [[[]]] ->
					       ?DUMP_FILE;
					   [[F]] ->
					       F;
					   _ ->
					       erlang:fault(badarg, [Options])
				       end,
				flags = case {Append, Dump} of
					    {[], _} ->
						[];
					    {[[]], [[F]]} when pid(F) ->
						erlang:fault(badarg, [Options]);
       					    {[[]], [[_]]} ->
						[append];
					    _ ->
						erlang:fault(badarg, [Options])
					end});
	{[{start, []}, {stop, [[]]}, {file, []}, 
	  {dump, []}, {append, []}],
	 []} ->
	    call(#profile_stop{});
	{[{start, []}, {stop, []}, {file, File}, 
	  {dump, Dump}, {append, Append}],
	 []}->
	    call(#profile{src = case File of
				    [] ->
					?TRACE_FILE;
				    [[]] ->
					?TRACE_FILE;
				    [[F]] ->
					F;
				    _ ->
					erlang:fault(badarg, [Options])
				end,
			  group_leader = group_leader(),
			  dump = case Dump of
				     [] ->
					 [];
				     [[]] ->
					 group_leader();
				     [[[]]] ->
					 ?DUMP_FILE;
				     [[F]] ->
					 F;
				     _ ->
					 erlang:fault(badarg, [Options])
				 end,
			  flags = case {Append, Dump} of
				      {[], _} ->
					  [];
				      {[[]], [[F]]} when pid(F) ->
					  erlang:fault(badarg, [Options]);
				      {[[]], [[_]]}  ->
					  [append];
				      _ ->
					  erlang:fault(badarg, [Options])
				  end});
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
analyse(Options) when list(Options) ->
    case getopts(Options, [dest, append, cols]) of
	{[{dest, Dest}, {append, Append}, {cols, Cols}],
	 []} ->
	    call(#analyse{group_leader = group_leader(),
			  dest = case Dest of
				     [] ->
					 group_leader();
				     [[]] ->
					 group_leader();
				     [[[]]] ->
					 ?ANALYSIS_FILE;
				     [[F]] ->
					 F;
				     _ ->
					erlang:fault(badarg, [Options])
				end,
			  flags = case {Append, Dest} of
				      {[], _} ->
					  [];
				      {[[]], [[]]}  ->
					  [append];
				      {[[]], [[F]]} when pid(F) ->
					  erlang:fault(badarg, [Options]);
				      {[[]], [[_]]}  ->
					  [append];
				      _ ->
					  erlang:fault(badarg, [Options])
				  end,
			  cols = case Cols of
				     [] ->
					 80;
				     [[C]] when integer(C), C >= 80 ->
					 C;
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
	{[{file, File}], []} ->
	    call(#save_profile{file = case File of
					  [] -> 
					      ?PROFILE_FILE;
					  [[F]] ->
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
	{[{file, File}], []} ->
	    call(#load_profile{file = case File of
					  [] -> 
					      ?PROFILE_FILE;
					  [[F]] ->
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
	    stopped;
	Pid ->
	    exit(Pid, kill),
	    stopped
    end;
stop(Reason) ->
    just_call(#stop{reason = Reason}),
    stopped.



%%%------------------------
%%% Client helper functions
%%%------------------------

%% Send request to server process and return the server's reply.
%% First start server if it aint started.
call(Request) ->
    case whereis(?FPROF_SERVER) of
	undefined ->
	    start();
	_Server ->
	    ok
    end,
    just_call(Request).

%% Send request to server process, and return the server's reply.
%% Returns {'EXIT', Pid, Reason} if the server dies during the
%% call, or if it wasn't started.
just_call(Request) ->		      
    Mref = erlang:monitor(process, ?FPROF_SERVER),
    Tag = {Mref, self()},
    T = case Request of
	    #stop{} ->
		?FPROF_SERVER_TIMEOUT;
	    _ ->
		0
	end,
    catch ?FPROF_SERVER ! {?FPROF_SERVER, Tag, Request},
    receive
	{?FPROF_SERVER, Mref, Reply} ->
	    erlang:demonitor(Mref),
	    receive {'DOWN', Mref, _, _, _} -> ok after T -> ok end,
	    Reply;
	{'DOWN', Mref, _, Pid, Reason} ->
	    receive {?FPROF_SERVER, Mref, _} -> ok after T -> ok end,
	    {'EXIT', Pid, Reason}
    after ?FPROF_SERVER_TIMEOUT ->
	    timeout
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
			dest = Filename} = Request, Tag, State) ->
    case {get(trace_state), get(pending_stop)} of
	{idle, []} ->
	    trace_off(),
	    Port = open_dbg_trace_port(file, Filename),
	    trace_on(Procs, Port, Mode),
	    put(trace_state, running),
	    put(trace_type, file),
	    put(trace_pid, Port),
	    reply(Tag, ok),
	    State;
	_ ->
	    reply(Tag, {error, already_tracing}),
	    State
    end;
handle_req(#trace_start{procs = Procs,
			mode = Mode,
			type = tracer,
			dest = Tracer} = Request, Tag, State) ->
    case {get(trace_state), get(pending_stop)} of
	{idle, []} ->
	    trace_off(),
	    trace_on(Procs, Tracer, Mode),
	    put(trace_state, running),
	    put(trace_type, tracer),
	    put(trace_pid, Tracer),
	    reply(Tag, ok),
	    State;
	_ ->
	    reply(Tag, {error, already_tracing}),
	    State
    end;

handle_req(#trace_stop{} = Request, Tag, State) ->
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
		    flags = Flags} = Request, Tag, State) ->
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
	    reply(Tag, {error, already_analysing}),
	    State
    end;
	    
handle_req(#profile_start{group_leader = GroupLeader,
			  dump = Dump,
			  flags = Flags} = Request, Tag, State) ->
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
	    reply(Tag, {error, already_analysing}),
	    State
    end;

handle_req(#profile_stop{} = Request, Tag, State) ->
    case {get(profile_state), get(profile_type)} of
	{running, tracer} ->
	    ProfilePid = get(profile_pid),
	    case {get(trace_state), get(trace_type), get(trace_pid)} of
		{running, tracer, ProfilePid} ->
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
	    reply(Tag, {error, analysing_file}),
	    State;
	{_, _} ->
	    reply(Tag, {error, not_analysing}),
	    State
    end;
	    
handle_req(#analyse{group_leader = GroupLeader,
		    dest = Dest,
		    flags = Flags,
		    cols = Cols} = Request, Tag, State) ->
    case get(profile_state) of
	{idle, ok} ->
	    case ensure_open(Dest, [write | Flags]) of
		{error, _} = Error ->
		    reply(Tag, Error),
		    State;
		{DestState, DestPid} ->
		    ProfileTable = get(profile_table),
		    case catch spawn_3step(
			   fun(Server) ->
				   Result = do_analyse(ProfileTable, 
						       GroupLeader, 
						       DestPid, 
						       Cols),
				   Result
			   end,
			   fun(Worker, Result) ->
				   finish
			   end,
			   fun(Server, finish) ->
				   ok
			   end) of
			{ok, Worker, Result, finish} ->
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
	    reply(Tag, {error, analysing}),
	    State
    end;

handle_req(#stop{reason = Reason} = Request, Tag, State) ->
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
%% Options        = [OptionName]
%% Option         = OptionName | {OptionName, OptionValue}
%% OptionName     = atom()
%% OptionValue    = term()
%% DecodedOptions = [{OptionName, [TaggedValue]}]
%% RestOptions    = [Option]
%% TaggedValue    = NIL | cons(Value, NIL)
%%
%% Searches List for options with names defined in Options.
%% Returns DecodedOptions containing all options from List
%% that had names from Options, and RestOptions which contains
%% all other terms from List.
%%
%% All returned lists preservs the order from Options and List.
%%
%% An example:
%%     getopts([{f, 1}, e, {d, 2}, {c, 3, 4}, {b, 5}, a, b],
%%             [a, b, c, d]) ->
%%         {[{a, [[]]}, {b, [[5], []}, {c, []}, {d, [[2]]}],
%%          [{f, 1}, e, {c, 3, 4}}
%%
getopts(List, Options) when list(List), list(Options) ->
    getopts_1(Options, List, []).

getopts_1([], List, Result) ->
    {lists:reverse(Result), List};
getopts_1([Option | Options], List, Result) ->
    {Optvals, Remaining} = getopts_2(List, Option, [], []),
    getopts_1(Options, Remaining, [{Option, Optvals} | Result]).

getopts_2([], _Option, Result, Remaining) ->
    {lists:reverse(Result), lists:reverse(Remaining)};
getopts_2([{Option, Value} | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, [[Value] | Result], Remaining);
getopts_2([Option | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, [[] | Result], Remaining);
getopts_2([Other | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, Result, [Other | Remaining]).

%% The reverse of setopts, almost.
%% Re-creates (approximately) List from DecodedOptions in 
%% getopts/2 above. The original order is not preserved, 
%% but rather the order from Options.
%% 
%% An example:
%%     setopts([{a, [[]]}, {b, [[5], []}, {c, []}, {d, [[2]]}]) ->
%%         [a, {b, 5}, b, {d, 2}]
%%
%% And a more generic example:
%%     {D, R} = getopts(L, O),
%%     L2 = setopts(D) ++ R
%% L2 will contain exactly the same terms as L, but not in the same order.
%%
setopts(Options) when list(Options) ->
    lists:reverse(
      lists:foldl(
	fun ({Option, Values}, Accumulator) ->
		lists:foldl(
		  fun ([], Acc) ->
			  [Option | Acc];
		      ([Val], Acc) ->
			  [{Option, Val} | Acc]
		  end,
		  Accumulator,
		  Values)
	end,
	[],
	Options)).



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
	    receive {Chile, Ref, _Ack} -> ok after 0 -> ok end,
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
    trace_off(new),
    lists:foreach(
      fun(Pid) ->
	      case is_process_alive(Pid) of
		  true ->
		      catch trace_off(Pid);
		  false ->
		      ok
	      end
      end,
      processes()),
    erlang:trace_pattern(on_load, false, []),
    erlang:trace_pattern({'_', '_', '_'}, false, []),
    ok.
    


trace_off(P) ->    
    case erlang:trace_info(P, flags) of
	{flags, undefined} ->
	    0;
	{flags, []} ->
	    1;
	{flags, Flags} ->
	    case erlang:trace_info(P, tracer) of
		{tracer, undefined} ->
		    0;
		{tracer, _} = TracerFlag ->
		    erlang:trace(P, false, [TracerFlag | Flags])
	    end
    end.



trace_on(Procs, Tracer, Mode) ->
    erlang:trace_pattern(on_load, true, [local]),
    erlang:trace_pattern({'_', '_', '_'}, true, [local]),
    lists:foreach(
      fun (P) ->
	      erlang:trace(P, true, [{tracer, Tracer} | trace_flags(Mode)])
      end,
      Procs).



trace_flags(normal) ->
    [call, return_to, 
     running, procs, garbage_collection, 
     arity, timestamp, set_on_spawn];
trace_flags(verbose) ->
    [call, return_to, send, 'receive',
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
    Handler = 
	case Dump of
	    undefined ->
		fun handler/2;
	    _ ->
		fun dump_handler/2
	end,
    case dbg:trace_client(file, File, 
			  {Handler, 
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
		  case Dump of
		      undefined ->
			  tracer_loop(Parent, fun handler/2, Init);
		      _ ->
			  tracer_loop(Parent, fun dump_handler/2, Init)
		  end
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

handler(end_of_trace, {init, GroupLeader, Table, _Dump}) ->
    io:format(GroupLeader, "Empty trace!~n", []),
    end_of_trace(Table, undefined, 0, undefined),
    done;
handler(end_of_trace, {N, FirstTS, LastTS, _S}) ->
    GroupLeader = erase(group_leader),
    Table = erase(table),
    Dump = erase(dump),
    io:format(GroupLeader, "End of trace!~n", []),
    end_of_trace(Table, FirstTS, N, LastTS),
    done;
handler(Trace, {init, GroupLeader, Table, Dump}) ->
    put(group_leader, GroupLeader),
    put(table, Table),
    put(dump, Dump),
    io:format(GroupLeader, "Reading trace data...~n", []),
    {TS, S} = trace_handler(Trace, undefined, init),
    {1, TS, TS, S};
handler(Trace, {M, FirstTS, LastTS, S}) ->
    N = M+1,
    handler_format_dots(get(group_leader), N),
    case trace_handler(Trace, LastTS, S) of
	{undefined, S} ->
	    {N, FirstTS, LastTS, S};
	{TS, NewS} ->
	    {N, 
	     case FirstTS of
		 undefined ->
		     TS;
		 _ ->
		     FirstTS
	     end,
	     TS,
	     NewS}
    end.



dump_handler(end_of_trace, {init, GroupLeader, Table, Dump}) ->
    io:format(Dump, "Trace data dump: empty!~n", []),
    if GroupLeader /= Dump ->
	    io:put_chars(GroupLeader, "Empty trace!~n");
       true ->
	    ok
    end,
    end_of_trace(Table, undefined, 0, undefined),
    done;
dump_handler(end_of_trace, {N, FirstTS, LastTS, _S}) ->
    GroupLeader = erase(group_leader),
    Table = erase(table),
    Dump = erase(dump),
    io:format(Dump, "Trace data dump: end!~n", []),
    if GroupLeader /= Dump ->
	    io:format(GroupLeader, "End of trace!~n", []);
       true ->
	    ok
    end,
    end_of_trace(Table, FirstTS, N, LastTS),
    done;
dump_handler(Trace, {init, GroupLeader, Table, Dump}) ->
    put(group_leader, GroupLeader),
    put(table, Table),
    put(dump, Dump),
    io:format(Dump, "Trace data dump:~n~p~n", [Trace]),
    if GroupLeader /= Dump ->
	    io:format(GroupLeader, "Reading trace data...~n", []);
       true ->
	    ok
    end,
    {TS, S} = trace_handler(Trace, undefined, init),
    {1, TS, TS, S};
dump_handler(Trace, {M, FirstTS, LastTS, S}) ->
    N = M+1,
    io:format(get(dump), "~p~n", [Trace]),
    GroupLeader = get(group_leader),
    case get(dump) of
	GroupLeader ->
	    ok;
	_ ->
	    handler_format_dots(GroupLeader, N)
    end,
    case trace_handler(Trace, LastTS, S) of
	{undefined, S} ->
	    {N, FirstTS, LastTS, S};
	{TS, NewS} ->
	    {N, 
	     case FirstTS of
		 undefined ->
		     TS;
		 _ ->
		     FirstTS
	     end,
	     TS,
	     NewS}
    end.



handler_format_dots(Io, N) ->
    if (N rem 100000) == 0 ->
	    io:format(Io, ",~n", []);
       (N rem 50000) == 0 ->
	    io:format(Io, ".~n", []);
       (N rem 1000) == 0 ->
	    io:put_chars(Io, ".");
       true ->
	    ok
    end.



end_of_trace(Table, FirstTS, N, LastTS) ->
    %%
    %% Close all process stacks, as if the processes exited.
    %%
    Procs = get(),
    put(table, Table),
    ?dbg(2, "get() -> ~p~n", [Procs]),
    lists:map(
      fun ({Pid, _}) when pid(Pid) ->
	      trace_exit(Pid, LastTS)
      end,
      Procs),
    erase(),
    ets:insert(Table, #misc{id = first_ts, data = FirstTS}),
    ets:insert(Table, #misc{id = trace_cnt, data = N}),
    ets:insert(Table, #misc{id = last_ts, data = LastTS}),
    ets:insert(Table, #misc{id = end_of_trace, data = []}),
    end_of_trace.



%%%----------------------------------
%%% Profiling state machine functions
%%%----------------------------------



%% State machine init
trace_handler(Trace, PrevTS, init) ->
    trace_handler(Trace, PrevTS, {false, undefined});
%%
%% call
trace_handler({trace_ts, Pid, call, {M, F, Arity} = Func, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when integer(Arity) ->
    trace_call(Pid, {Func, TS}),
    {TS, {false, Pid}};
trace_handler({trace_ts, Pid, call, {M, F, Args} = MFArgs, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when list(Args) ->
    Func = mfarity(MFArgs),
    trace_call(Pid, {Func, TS}),
    {TS, {false, Pid}};
%%
%% return_to
trace_handler({trace_ts, Pid, return_to, undefined, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    trace_return_to(Pid, {undefined, TS}),
    {TS, {false, Pid}};
trace_handler({trace_ts, Pid, return_to, {M, F, Arity} = Func, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when integer(Arity) ->
    trace_return_to(Pid, {Func, TS}),
    {TS, {false, Pid}};
trace_handler({trace_ts, Pid, return_to, {M, F, Args} = MFArgs, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when list(Args) ->
    Func = mfarity(MFArgs),
    trace_return_to(Pid, {Func, TS}),
    {TS, {false, Pid}};
%%
%% spawn
trace_handler({trace_ts, Pid, spawn, Child, MFArgs, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    trace_spawn(Child, {MFArgs, TS}, Pid),
    {TS, {false, Pid}};
trace_handler({trace_ts, Pid, spawn, Child, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    trace_spawn(Child, {undefined, TS}, Pid),
    {TS, {false, Pid}};
%%
%% exit
trace_handler({trace_ts, Pid, exit, _Reason, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    trace_exit(Pid, TS),
    {TS, {false, Pid}};
%%
%% out
trace_handler({trace_ts, Pid, out, {M, F, Arity} = Func, PrevTS} = Trace,
	      PrevTS, {false, PrevPid})
  when integer(Arity) ->
    trace_out(PrevPid, {Func, PrevTS}),
    {PrevTS, {true, PrevPid}};
trace_handler({trace_ts, Pid, out, {M, F, Args} = MFArgs, PrevTS} = Trace,
	      PrevTS, {false, PrevPid})
  when list(Args) ->
    Func = mfarity(MFArgs),
    trace_out(PrevPid, {Func, PrevTS}),
    {PrevTS, {true, PrevPid}};
trace_handler({trace_ts, Pid, out, {M, F, Arity} = Func, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when integer(Arity) ->
    trace_out(Pid, {Func, TS}),
    {TS, {false, Pid}};
trace_handler({trace_ts, Pid, out, {M, F, Args} = MFArgs, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when list(Args) ->
    Func = mfarity(MFArgs),
    trace_out(Pid, {Func, TS}),
    {TS, {false, Pid}};
%%
%% in
trace_handler({trace_ts, Pid, in, {M, F, Arity} = Func, TS} = Trace,
	      _PrevTS, {true, PrevPid})
  when integer(Arity) ->
    trace_in(PrevPid, {Func, TS}),
    {TS, {false, PrevPid}};
trace_handler({trace_ts, Pid, in, {M, F, Args} = MFArgs, TS} = Trace,
	      _PrevTS, {true, PrevPid})
  when list(Args) ->
    Func = mfarity(MFArgs),
    trace_in(PrevPid, {Func, TS}),
    {TS, {false, PrevPid}};
trace_handler({trace_ts, Pid, in, {M, F, Arity} = Func, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when integer(Arity) ->
    trace_in(Pid, {Func, TS}),
    {TS, {false, Pid}};
trace_handler({trace_ts, Pid, in, {M, F, Args} = MFArgs, TS} = Trace,
	      _PrevTS, {false, _PrevPid})
  when list(Args) ->
    Func = mfarity(MFArgs),
    trace_in(Pid, {Func, TS}),
    {TS, {false, Pid}};
%%
%% gc_start
trace_handler({trace_ts, Pid, gc_start, _Info = Func, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    trace_gc_start(Pid, TS),
    {TS, {false, Pid}};
%%
%% gc_end
trace_handler({trace_ts, Pid, gc_end, _Info = Func, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    trace_gc_end(Pid, TS),
    {TS, {false, Pid}};
%%
%% getting_linked
trace_handler({trace_ts, Pid, getting_linked, OtherPid, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    {TS, {false, OtherPid}};
%%
%% getting_unlinked
trace_handler({trace_ts, Pid, getting_unlinked, OtherPid, TS} = Trace,
	      _PrevTS, {false, _PrevPid}) ->
    {TS, {false, OtherPid}};
%%
%% register
trace_handler({trace_ts, Pid, register, _Name, TS} = Trace,
	      _PrevTS, {false, PrevPid}) ->
    {TS, {false, PrevPid}};
%%
%% unregister
trace_handler({trace_ts, Pid, unregister, _Name, TS} = Trace,
	      _PrevTS, {false, PrevPid}) ->
    {TS, {false, PrevPid}};
%%
%% Any trace message with timestamp
trace_handler(Trace, _PrevTS, {false, _PrevPid}) 
  when tuple(Trace), element(1, Trace) == trace_ts ->
    {element(size(Trace), Trace), {false, element(2, Trace)}};
trace_handler(Trace, PrevTS, {true, PrevPid}) 
  when tuple(Trace), element(1, Trace) == trace_ts ->
    Stack = get(PrevPid),
    exit({inconsistent_trace_data, ?MODULE, ?LINE,
	  [Stack, PrevPid, Trace, PrevTS]});
%%
%% Others
trace_handler(_Trace, _PrevTS, S) ->
    S.



trace_call(Pid, {Func, TS} = Call) ->
    Stack = get(Pid),
    ?dbg(0, "trace_call(~p, ~p) ~p~n", [Pid, Call, Stack]),
    case Stack of
	undefined ->
	    Table = get(table),
	    init_log(Table, Pid, Func),
	    trace_clock(Table, Pid, 1, [Call], #clocks.cnt),
	    put(Pid, [Call]);
	[] ->
	    Table = get(table),
	    init_log(Table, Pid, Func),
	    trace_clock(Table, Pid, 1, [Call], #clocks.cnt),
	    put(Pid, [Call]);
	[{suspend, _TS0} | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]});
	[{garbage_collect, _TS0} | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]});
	[{Func, _TS0} | _] ->
	    ok;
	[_Call0 | _] ->
	    Table = get(table),
	    init_log(Table, Pid, Func),
	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
	    NewStack = trace_call_1(Stack, Call, Stack, []),
	    trace_clock(Table, Pid, 1, [Call | NewStack], #clocks.cnt),
	    put(Pid, [Call | NewStack]);
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]})
    end,
    ok.

%% Search down the call stack to find a call to Func, 
%% save the calls passed.
trace_call_1([], _Call, Stack, _R) ->
    Stack;
trace_call_1([{Func, _TS0} = Call0 | Stack1], {Func, TS}, Stack, R) ->
    trace_call_2(Stack1, reverse(R), Call0, Stack, Stack1);
trace_call_1([Call0 | Stack1], Call, Stack, R) ->
    trace_call_1(Stack1, Call, Stack, [Call0 | R]).

%% Found a call to Func. Go further down the call stack as long as 
%% the saved calls from the stack top matches the calls below
%% Func. If Func is found again at the end of the saved call stack,
%% we have a call cycle and collaps it.
trace_call_2([{Func, _TS0A} | _RestA], [], 
	     {Func, _TS0}, _Stack, Stack1) ->
    Stack1;
trace_call_2([{Func, _TS0A} | RestA], [{Func, _TS0B} | RestB], 
	     Call0, Stack, Stack1) ->
    trace_call_2(RestA, RestB, Call0, Stack, Stack1);
trace_call_2(_StackA, _StackB,
	     _Call0, Stack, _Stack1) ->
    Stack.



trace_return_to(Pid, {Func, TS} = Call) ->
    Stack = get(Pid),
    ?dbg(0, "trace_return_to(~p, ~p) ~p~n", [Pid, Call, Stack]),
    case Stack of
	undefined ->
	    NewStack = [Call],
	    trace_clock(get(table), Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack);
	[] ->
	    case Func of
		undefined ->
		    put(Pid, []);
		_ ->
		    NewStack = [Call],
		    trace_clock(get(table), Pid, 1, NewStack, #clocks.cnt),
		    put(Pid, NewStack)
	    end;
	[{suspend, _TS0} | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]});
	[{garbage_collect, _TS0} | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]});
	[{Func, TS0} | _] ->
	    ok;
	[{Func0, TS0} | _] = Stack ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
	    case Func of
		undefined ->
		    put(Pid, trace_return_to_1(Table, Pid, Call, Stack));
		_ ->
		    put(Pid, 
			[Call | trace_return_to_1(Table, Pid, Call, Stack)])
	    end;
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Call]})
    end,
    ok.

trace_return_to_1(_Table, _Pid, _Call, []) ->
    [];
trace_return_to_1(Table, Pid, {Func, TS}, [{Func, _TS0} | Rest] = Stack) ->
    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
    Rest;
trace_return_to_1(Table, Pid, {_Func, TS} = Call, [Call0 | Rest] = Stack) ->
    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
    trace_return_to_1(Table, Pid, Call, Rest).



trace_spawn(Pid, {undefined, TS} = Call, Parent) ->
    case get(Pid) of
	undefined ->
	    Table = get(table),
	    NewStack = [{suspend, TS}],
	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack),
	    ets:insert(Table, #proc{id = Pid, parent = Parent});
	Stack ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]})
    end;
trace_spawn(Pid, {MFArgs, TS} = Call, Parent) ->
    Func = mfarity(MFArgs),
    case get(Pid) of
	undefined ->
	    Table = get(table),
	    NewStack = [{Func, TS}],
	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
	    NewerStack = [{suspend, TS} | NewStack],
	    trace_clock(Table, Pid, 1, NewerStack, #clocks.cnt),
	    put(Pid, NewerStack),
	    ets:insert(Table, #proc{id = Pid, parent = Parent,
				    spawned_as = MFArgs});
	Stack ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]})
    end.



trace_exit(Pid, TS) ->
    Stack = get(Pid),
    case Stack of
	undefined ->
	    ok;
	[] ->
	    ok;
	[{garbage_collect, TS0} = Call0 
	 | [{suspend, TS1} | [_Call2 | _] = Stack2] = Stack1] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack1, #clocks.acc),
	    FixStack = [Call0 | Stack2],
	    trace_clock(Table, Pid, TS, FixStack, #clocks.own),
	    trace_exit_1(Table, Pid, TS, FixStack);
	[{suspend, TS0} | _] ->
	    trace_exit_1(get(table), Pid, TS, Stack);
	[_Call0 | _] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
	    trace_exit_1(Table, Pid, TS, Stack)
    end,
    put(Pid, []).

trace_exit_1(Table, Pid, TS, []) ->
    ok;
trace_exit_1(Table, Pid, TS, [_Call0 | Rest] = Stack) ->
    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
    trace_exit_1(Table, Pid, TS, Rest).



trace_out(Pid, {_Func, TS} = Call) ->    
    Stack = get(Pid),
    ?dbg(0, "trace_out(~p, ~p) ~p~n", [Pid, Call, Stack]),
    case Stack of
	undefined ->
	    NewStack = [{suspend, TS}],
	    trace_clock(get(table), Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack);
	[] ->
	    NewStack = [{suspend, TS}],
	    trace_clock(get(table), Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack);
	[{suspend, _TS0} | _] ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]});
% 	[{Func, TS0} | _] ->
% 	    Table = get(table),
% 	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
% 	    NewStack = [{suspend, TS} | Stack],
% 	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
% 	    put(Pid, NewStack);
% 	[{garbage_collect, TS0} | _] ->
% 	    Table = get(table),
% 	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
% 	    NewStack = [{suspend, TS} | Stack],
% 	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
% 	    put(Pid, NewStack);
	[{Func0, TS0} = Call0 | _] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
	    NewStack = [{suspend, TS} | Stack],
	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack);
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]})
    end.
    


trace_in(Pid, {_Func, TS} = Call) ->	    
    Stack = get(Pid),
    ?dbg(0, "trace_in(~p, ~p) ~p~n", [Pid, Call, Stack]),
    case Stack of
	undefined ->
	    put(Pid, []);
	[] ->
	    put(Pid, []);
% 	[{suspend, TS0} | [{garbage_collect, TS1} | Stack2] = Stack1] ->
% 	    Table = get(table),
% 	    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
% 	    trace_clock(Table, Pid, TS, Stack1, #clocks.acc),
% 	    put(Pid, [{garbage_collect, TS} | Stack2]);
	[{suspend, TS0}] ->
	    trace_clock(get(table), Pid, TS, Stack, #clocks.acc),
	    put(Pid, []);
	[{suspend, TS0} | [{Func1, _TS1} | Stack2] = Stack1] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
	    trace_clock(Table, Pid, TS, Stack1, #clocks.acc),
	    put(Pid, [{Func1, TS} | Stack2]);
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, Call]})
    end.



trace_gc_start(Pid, TS) ->    
    Stack = get(Pid),
    ?dbg(0, "trace_gc_start(~p, ~p) ~p~n", [Pid, TS, Stack]),
    case Stack of
	undefined ->
	    NewStack = [{garbage_collect, TS}],
	    trace_clock(get(table), Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack);
	[] ->
	    NewStack = [{garbage_collect, TS}],
	    trace_clock(get(table), Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack);
	[{suspend, TS0} | [_Call1 | _] = Stack1] ->
	    Call = {garbage_collect, TS},
	    trace_clock(get(table), Pid, 1, [Call | Stack1], #clocks.cnt),
	    put(Pid, [Call | Stack]);
	[{Func0, TS0} | _] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
	    NewStack = [{garbage_collect, TS} | Stack],
	    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
	    put(Pid, NewStack);
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, TS]})
    end.



trace_gc_end(Pid, TS) ->	    
    Stack = get(Pid),
    ?dbg(0, "trace_gc_end(~p, ~p) ~p~n", [Pid, TS, Stack]),
    case Stack of
	undefined ->
	    put(Pid, []);
	[] ->
	    put(Pid, []);
	[{garbage_collect, TS0}] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
	    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
	    put(Pid, []);
	[{garbage_collect, TS0} = Call0 | [{suspend, TS1} 
					   | Stack2] = Stack1] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, [Call0 | Stack2], #clocks.own),
	    trace_clock(Table, Pid, TS, [Call0 | Stack2], #clocks.acc),
	    trace_clock(Table, Pid, TS, Stack1, #clocks.acc),
	    put(Pid, [{suspend, TS} | Stack2]);
	[{garbage_collect, TS0} | [{Func1, TS1} | Stack2] = Stack1] ->
	    Table = get(table),
	    trace_clock(Table, Pid, TS, Stack, #clocks.own),
	    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
	    trace_clock(Table, Pid, TS, Stack1, #clocks.acc),
	    put(Pid, [{Func1, TS} | Stack2]);
	_ ->
	    exit({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Stack, Pid, TS]})
    end.



%%%-----------------------------------------
%%% Statistics calculating support functions
%%%-----------------------------------------



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



trace_clock(Table, Pid, TS, [{_Func0, TS0} = Call0], Clock) ->
    trace_clock(Table, Pid, TS, [Call0, {undefined, TS0}], Clock);
trace_clock(Table, Pid, T, [{Func0, TS0}, {Func1, TS1} | _], Clock)
  when integer(T) ->
    clock_add(Table, {Pid, Func1, Func0}, Clock, T);
trace_clock(Table, Pid, TS, [{Func0, TS0}, {Func1, TS1} | _], Clock) ->
    clock_add(Table, {Pid, Func1, Func0}, Clock, ts_sub(TS, TS0)).

clock_add(Table, Id, Clock, T) ->
    ?dbg(0, "        clock_add(Table, ~w, ~w, ~w)~n", [Id, Clock, T]),
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



do_analyse(Table, GroupLeader, Io, Cols) ->
    NrWidth = (Cols-3) div 7,
    FnameWidth = Cols - 3*NrWidth - 3,
    Dest = {Io, FnameWidth, NrWidth},
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
    Data = ets:tab2list(Table),
    ?dbg(2, "Data = ~p~n", [Data]),
    PidTable = ets:new(?MODULE, [set, private, {keypos, #clocks.id}]),
    ProcTable = ets:new(?MODULE, [set, private, {keypos, #proc.id}]),
    lists:foreach(
      fun (#clocks{id = {Pid, Caller, Func}} = Clocks) ->
	      funcstat_pd_pid_func(Pid, Caller, Func, Clocks),
	      funcstat_pd_pid_caller(Pid, Caller, Func, Clocks),
	      clocks_add(PidTable, Clocks#clocks{id = Pid});
	  (#proc{} = Proc) ->
	      ets:insert(ProcTable, Proc);
	  (#misc{} = Misc) ->
	      ets:insert(ProcTable, Misc)
      end,
      Data),
    [#misc{id = end_of_trace, data = Nil}] = 
	ets:lookup(ProcTable, end_of_trace),
    [#misc{id = first_ts, data = FirstTS}] = 
	ets:lookup(ProcTable, first_ts),
    [#misc{id = trace_cnt, data = TraceCnt}] = 
	ets:lookup(ProcTable, trace_cnt),
    [#misc{id = last_ts, data = LastTS}] = 
	ets:lookup(ProcTable, last_ts),
    if Nil == [], FirstTS /= undefined, LastTS /= undefined ->
	    ok;
       true ->
	    exit(incomplete_trace)
    end,
    ?dbg(3, "get() -> ~p~n", [get()]),
    PidList = ets:tab2list(PidTable),
    ProcList = ets:tab2list(ProcTable),
    ?dbg(3, "PidList =  ~p~n", [PidList]),
    ?dbg(3, "ProcList =  ~p~n", [ProcList]),
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
    %% Sort the processes.
    %%
    io:format(GroupLeader, "Calculating totals...~n", []),
    PidSorted = clocks_sort_ownR(PidList),
    ?dbg(4, "PidSorted = ~p~n", [PidSorted]),
    %%
    %% Calculate totals
    %%
    Totals0 = lists:foldl(fun(A, B) -> clocks_sum(A, B, totals) end,
			 #clocks{id = totals},
			 PidList),
    Totals = Totals0#clocks{acc = ts_sub(LastTS, FirstTS)},
    ?dbg(4, "Totals = ~p~n", [Totals]),
    %%
    %% Print the functions per process
    %%
    io:format(GroupLeader, "Creating output...~n", []),
    case Io of
	undefined ->
	    ok;
	_ ->
	    io:format(Io, "~nDetailed statistics:~n", [])
    end,
    print_head(Dest),
    print_totals(Dest, Totals),
    lists:foreach(
      fun ({#clocks{} = Clocks, ProcOrPid, FuncstatList}) ->
	      print_head(Dest),
	      print_pid(Dest, Clocks),
	      print_proc(Dest, ProcOrPid),
	      lists:foreach(
		fun (#funcstat{callers_sum = CallersSum, 
%			       called_sum = CalledSum, 
			       callers = Callers, 
			       called = Called}) ->
			lists:foreach(
			  fun(C) ->
				  print_caller(Dest, C)
			  end,
			  Callers),
			print_func(Dest, CallersSum),
%			print_func(Dest, CalledSum),
			lists:foreach(
			  fun(C) ->
				  print_called(Dest, C)
			  end,
			  Called),
			print_nl(Dest),
			ok
		end,
		%% Sort the functions within the process, 
		%% and the callers and called within the function.
		funcstat_sort_accR(FuncstatList, fun clocks_sort_accR/1))
      end,
      %% Look up the processes in sorted order
      lists:map(
	fun (#clocks{id = Pid} = Clocks) -> 
		Proc = case ets:lookup(ProcTable, Pid) of
			   [] -> Pid;
			   [ProcX] -> ProcX
		       end,
		FuncstatList = get(Pid),
		{Clocks, Proc, FuncstatList}
	end, 
	PidSorted)),
%     %%
%     %% Sort and print the functions per process
%     %%
%     io:format(GroupLeader, "Creating output...~n", []),
%     Dest = group_leader(),
%     io:format(Io, "~nDetailed statistics:~n", []),
%     lists:foreach(
%       fun (Times) ->
% 	      print_head(Dest),
% 	      Pid = times_id(Times),
% 	      print_entry(Dest, Times, Totals),
% 	      case ets:lookup(ProcTable, Pid) of
% 		  [#proc{} = Proc] ->
% 		      print_entry(Dest, Proc);
% 		  [] ->
% 		      ok
% 	      end,
% 	      lists:foreach(
% 		fun(Times1) ->
% 			print_entry(Dest, Times1, Totals)
% 		end,
% 		lists:reverse(
% 		  lists:sort(
% 		    lists:map(fun clocks_pid_mfarity_presort/1,
% 			      getl(Pid)))))
%       end,
%       PidSorted),
%     %%
%     %% Sort and print the per process data
%     %%
%     FuncList = ets:tab2list(FuncTable),
%     ets:delete(FuncTable),
%     ?dbg(2, "FuncList = ~p~n", [FuncList]),
%     io:format(Io, "~n** Processes summarized~n", []),
%     print_head(Dest),
%     lists:foreach(
%       fun (Times) ->
% 	      Func = times_id(Times),
% 	      print_entry(Dest, Times, Totals)
%       end,
%       lists:reverse(
% 	lists:sort(
% 	  lists:map(fun clocks_mfarity_presort/1,
% 		    FuncList)))),
%     %%
%     %% Print the total totals
%     %%
%     print_head(Dest),
%     print_entry(Dest, Totals, Totals),
%     %%
%     %% Finish
%     %%
    ets:delete(PidTable),
    ets:delete(ProcTable),
    io:format(GroupLeader, "Done!~n", []),
    ok.



%%----------------------------
%% Analysis printout functions
%%----------------------------



print_head({Io, FnameWidth, NrWidth}) ->
    case Io of
	undefined ->
	    ok;
	_ ->
	    io:format(Io, "~s~s~s~s  ~n~n",
		      [pad(" ", $ , FnameWidth),
		       pad($ , " CALLS", NrWidth),
		       pad($ , " ACC", NrWidth), 
		       pad($ , " OWN",  NrWidth)])
    end.

print_totals({Io, FnameWidth, NrWidth}, 
	     #clocks{cnt = Cnt, own = Own, acc = Acc}) ->
    case Io of
	undefined ->
	    ok;
	_ ->
	    io:format(Io, "~s~s~s~s  ~n~n",
		      [pad(flat_format("** Totals ", []), 
			   $ , FnameWidth),
		       pad($ , flat_format(" ~w",   [Cnt]),       NrWidth),
		       pad($ , flat_format(" ~.3f", [Acc*0.001]), NrWidth),
		       pad($ , flat_format(" ~.3f", [Own*0.001]), NrWidth)])
    end.

print_pid({Io, FnameWidth, NrWidth}, 
	  #clocks{id = Pid, cnt = Cnt, own = Own}) ->
    case Io of
	undefined ->
	    ok;
	_ ->
	    io:format(Io, "~s~s~s~s  ~n",
		      [pad(flat_format("** Process ~w ", [Pid]), 
			   $ , FnameWidth),
		       pad($ , flat_format(" ~w",   [Cnt]),       NrWidth),
		       pad($ , " ",                               NrWidth),
		       pad($ , flat_format(" ~.3f", [Own*0.001]), NrWidth)])
    end.

print_caller(Dest, Clocks) ->
    print_func(Dest, Clocks, "   ~s ", "  ").

print_called(Dest, Clocks) ->
    print_func(Dest, Clocks, "   ~s ", "  ").

print_func(Dest, Clocks) ->
    print_func(Dest, Clocks, "*  ~s ", " *").

print_func({Io, FnameWidth, NrWidth}, 
	   #clocks{id = Func, cnt = Cnt, own = Own, acc = Acc}, 
	   Format, Tail) ->
    case Io of
	undefined ->
	    ok;
	_ ->
	    io:format(Io, "~s~s~s~s~s~n",
		      [pad(flat_format(Format,
				       [format_func(Func)]), 
			   $ , FnameWidth),
		       pad($ , flat_format(" ~w",   [Cnt]),       NrWidth),
		       pad($ , flat_format(" ~.3f", [Acc*0.001]), NrWidth),
		       pad($ , flat_format(" ~.3f", [Own*0.001]), NrWidth),
		       Tail])
    end.

print_proc(Dest,
	   #proc{id = Pid, parent = undefined, init_log = []}) ->
    print_nl(Dest);
print_proc({Io, FnameWidth, NrWidth} = Dest, 
	   #proc{id = Pid, 
		 parent = Parent, 
		 spawned_as = SpawnedAs,
		 init_log = InitLog}) ->
    case Io of
	undefined ->
	    ok;
	_ ->
	    io:put_chars(Io, 
			 ["** ",
			  case Parent of 
			      undefined ->
				  [];
			      _ ->
				  io_lib:format("Spawned by ~w~s.~n", 
						[Parent,
						 case SpawnedAs of
						     undefined ->
							 "";
						     _ ->
							 [" as ",
							  format_func(SpawnedAs)]
						 end])
			  end,
			  case InitLog of
			      [] ->
				  io_lib:nl();
			      _ ->
				  ["** Initial calls: " | io_lib:nl()]
			  end])
    end,
    print_init_log(Dest, InitLog),
    print_nl(Dest);
print_proc(Dest, Pid) ->
    print_nl(Dest).

print_init_log({undefined, FnameWidth, NrWidth}, 
	       _) ->
    ok;
print_init_log(_Dest, 
	       []) ->
    ok;
print_init_log({Io, FnameWidth, NrWidth} = Dest, 
	       [Func | Tail]) ->
    print_init_log(Dest, Tail),
    io:format(Io, "**    ~s~n", [format_func(Func)]).

print_nl({undefined, FnameWidth, NrWidth}) ->
    ok;
print_nl({Io, FnameWidth, NrWidth}) ->
    io:nl(Io).



format_func({M, F, Arity}) when integer(Arity) ->
    io_lib:format("~w:~w/~w", [M, F, Arity]);
format_func({M, F, Args}) when list(Args) ->
    io_lib:format("~w:~w(~s)", 
		  [M, F, format_func_args(Args)]);
format_func(Func) ->
    io_lib:format("~w", [Func]).

format_func_args([A | [_|_] = T]) ->
    [io_lib:format("~p,", [A]) | format_func_args(T)];
format_func_args([A]) ->
    io_lib:format("~p", [A]);
format_func_args([]) ->
    [].



flat_format(Format, Args) ->
    flatten(io_lib:format(Format, Args)).



pad(Char, L, Size) when integer(Char), list(L), integer(Size) ->
    List = lists:flatten(L),
    Length = length(List),
    if Length >= Size ->
	    List;
       true ->
	    lists:append(lists:duplicate(Size - Length, Char), List)
    end;
pad(L, Char, Size) when list(L), integer(Char), integer(Size) ->
    List = lists:flatten(L),
    Length = length(List),
    if Length >= Size ->
	    List;
       true ->
	    lists:append(List, lists:duplicate(Size - Length, Char))
    end.



%%%--------------------------
%%% Sorting support functions
%%%--------------------------



funcstat_pd_pid_func(Pid, Caller, Func, Clocks) ->
    put({Pid, Func},
	case get({Pid, Func}) of
	    undefined ->
		#funcstat{callers_sum = Clocks#clocks{id = Func}, 
			  called_sum = #clocks{id = Func},
			  callers = [Clocks#clocks{id = Caller}]};
	    #funcstat{callers_sum = CallersSum,
		      callers = Callers} = Funcstat ->
		Funcstat#funcstat{
		  callers_sum = clocks_sum(CallersSum, Clocks, Func),
		  callers = [Clocks#clocks{id = Caller} | Callers]}
	end).

funcstat_pd_pid_caller(Pid, Caller, Func, Clocks) ->
    put({Pid, Caller},
	case get({Pid, Caller}) of
	    undefined ->
		#funcstat{callers_sum = #clocks{id = Caller}, 
			  called_sum = Clocks#clocks{id = Caller},
			  called = [Clocks#clocks{id = Func}]};
	    #funcstat{called_sum = CalledSum,
		      called = Called} = Funcstat ->
		Funcstat#funcstat{
		  called_sum = clocks_sum(CalledSum, Clocks, Caller),
		  called = [Clocks#clocks{id = Func} | Called]}
	end).



funcstat_sort_accR(FuncstatList, ClocksSort) ->
    funcstat_sort_accR_1(FuncstatList, ClocksSort, []).

funcstat_sort_accR_1([], _ClocksSort, R) ->
    postsortR(sort(R), []);
funcstat_sort_accR_1([#funcstat{callers_sum = #clocks{acc = K},
				callers = Callers,
				called = Called} = Funcstat
		      | L], 
		     ClocksSort,
		     R) ->
    funcstat_sort_accR_1(L, 
			 ClocksSort,
			 [[K | Funcstat#funcstat{callers = ClocksSort(Callers),
						 called = ClocksSort(Called)}]
			  | R]).



clocks_sort_accR(ClocksList) ->
    clocks_sort_accR_1(ClocksList, []).

clocks_sort_accR_1([], R) ->
    postsortR(sort(R), []);
clocks_sort_accR_1([#clocks{acc = K} = C | L], R) ->
    clocks_sort_accR_1(L, [[K | C] | R]).



clocks_sort_ownR(ClocksList) ->
    clocks_sort_ownR_1(ClocksList, []).

clocks_sort_ownR_1([], R) ->
    postsortR(sort(R), []);
clocks_sort_ownR_1([#clocks{own = K} = C | L], R) ->
    clocks_sort_ownR_1(L, [[K | C] | R]).



postsortR([], R) ->
    R;
postsortR([[_ | C] | L], R) ->
    postsortR(L, [C | R]).
