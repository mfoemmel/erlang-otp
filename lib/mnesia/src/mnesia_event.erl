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
-module(mnesia_event).

-behaviour(gen_event).
-behaviour(mnesia_event).

%% gen_event callback interface
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {nodes = [], 
		dumped_core = false,  %% only dump fatal core once
		args}).

%%%----------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------

%%-----------------------------------------------------------------
%% init(Args) ->
%%     {ok, State} | Error
%%-----------------------------------------------------------------

init(Args) ->
    {ok, #state{args = Args}}.

%%-----------------------------------------------------------------
%% handle_event(Event, State) -> 
%%    {ok, NewState} | remove_handler |
%%    {swap_handler, Args1, State1, Mod2, Args2}
%%-----------------------------------------------------------------

handle_event(Event, State) ->
    handle_any_event(Event, State).

%%-----------------------------------------------------------------
%% handle_info(Msg, State) ->
%%    {ok, NewState} | remove_handler |
%%    {swap_handler, Args1, State1, Mod2, Args2}
%%-----------------------------------------------------------------

handle_info(Msg, State) ->
    handle_any_event(Msg, State),
    {ok, State}.

%%-----------------------------------------------------------------
%% handle_call(Event, State) -> 
%%    {ok, Reply, NewState} | {remove_handler, Reply} | 
%%    {swap_handler, Reply, Args1, State1, Mod2, Args2}
%%-----------------------------------------------------------------

handle_call(Msg, State) ->
    Reply = ok,
    case handle_any_event(Msg, State) of
	{ok, NewState} ->
	    {ok, Reply, NewState};
	remove_handler ->
	    {remove_handler, Reply};
	{swap_handler,Args1, State1, Mod2, Args2} ->
	    {swap_handler, Reply, Args1, State1, Mod2, Args2}
    end.

%%-----------------------------------------------------------------
%% terminate(Reason, State) ->
%%     AnyVal
%%-----------------------------------------------------------------

terminate(Reason, State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Upgrade process when its code is to be changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    exit(not_supported).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

handle_any_event({mnesia_system_event, Event}, State) ->
    handle_system_event(Event, State);
handle_any_event({mnesia_table_event, Event}, State) ->
    handle_table_event(Event, State);
handle_any_event(Msg, State) ->
    report_error("~p got unexpected event: ~p~n", [?MODULE, Msg]),
    {ok, State}.

handle_table_event({Oper, Record, TransId}, State) ->
    report_info("~p performed by ~p on record:~n\t~p~n",
		[Oper, TransId, Record]),
    {ok, State}.  

handle_system_event({mnesia_checkpoint_activated, Checkpoint}, State) ->
    {ok, State};

handle_system_event({mnesia_checkpoint_deactivated, Checkpoint}, State) ->
    {ok, State};

handle_system_event({mnesia_up, Node}, State) ->
    Nodes = [Node | State#state.nodes],
    {ok, State#state{nodes = Nodes}}; 

handle_system_event({mnesia_down, Node}, State) ->
    case mnesia:system_info(fallback_activated) of
	true ->
	    Msg = "A fallback is installed and Mnesia "
		  "must be restarted. Forcing shutdown "
		  "after mnesia_down from ~p...~n",
	    report_fatal(Msg, [Node], nocore, State#state.dumped_core),
	    mnesia:lkill(),
	    exit(fatal);
	false ->
	    Nodes = lists:delete(Node, State#state.nodes),
	    {ok, State#state{nodes = Nodes}}
    end;

handle_system_event({mnesia_overload, Details}, State) ->
    report_error("Mnesia is overloaded: ~p~n", [Details]),
    {ok, State}; 

handle_system_event({mnesia_info, Format, Args}, State) ->
    report_info(Format, Args),
    {ok, State}; 

handle_system_event({mnesia_error, Format, Args}, State) ->
    report_error(Format, Args),
    {ok, State}; 

handle_system_event({mnesia_fatal, Format, Args, BinaryCore}, State) ->
    report_fatal(Format, Args, BinaryCore, State#state.dumped_core),
    {ok, State#state{dumped_core = true}};

handle_system_event({inconsistent_database, Reason, Node}, State) ->
    report_error("mnesia_event got {inconsistent_database, ~w, ~w}~n",
		 [Reason, Node]),
    {ok, State}; 

handle_system_event({mnesia_user, Event}, State) ->
    report_info("User event: ~p~n", [Event]),
    {ok, State}; 

handle_system_event(Msg, State) ->
    report_error("mnesia_event got unexpected system event: ~p~n", [Msg]),
    {ok, State}.

report_info(Format0, Args0) ->
    Format = "Mnesia(~p): " ++ Format0,
    Args = [node() | Args0],
    case global:whereis_name(mnesia_global_logger) of
	undefined ->
	    io:format(Format, Args);
	Pid ->
	    io:format(Pid, Format, Args)
    end.

report_error(Format0, Args0) ->
    Format = "Mnesia(~p): ** ERROR ** " ++ Format0,
    Args = [node() | Args0],
    error_logger:format(Format, Args),
    case global:whereis_name(mnesia_global_logger) of
	undefined ->
	    ok;
	Pid ->
	    io:format(Pid, Format, Args)
    end.

report_fatal(Format, Args, BinaryCore, CoreDumped) ->
    UseDir = mnesia_monitor:use_dir(),
    if
	UseDir == true,
	CoreDumped == false,
	binary(BinaryCore) ->
	    CoreFile = core_file(),
	    report_error("(core dumped to file: ~p)~n ** FATAL ** " ++ Format,
			 [CoreFile] ++ Args),
	    file:write_file(CoreFile, BinaryCore);
	true ->
	    report_error("(ignoring core) ** FATAL ** " ++ Format, Args)
    end.

core_file() ->
    %% Integers = tuple_to_list(date()) ++ tuple_to_list(time()),
    Integers = tuple_to_list(now()),
    Fun = fun(I) when I < 10 -> ["_0",I];
	     (I) -> ["_",I]
	  end,
    List = lists:append([Fun(I) || I <- Integers]),
    filename:absname(lists:concat(["MnesiaCore.", node()] ++ List)).

	
