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
-module(dets_server).

%% Disk based linear hashing lookup dictionary. Server part.

-behaviour(gen_server).

%% External exports.
-export([all/0, close/1, get_pid/1, open_file/1, open_file/2, pid2name/1,
         users/1, verbose/1]).

%% Internal.
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

%% state for the dets server
-record(state, {store, parent}).

-include("dets.hrl").

-define(REGISTRY, dets_registry).  % {Table, NoUsers, TablePid}
-define(OWNERS, dets_owners).      % {TablePid, Table}
-define(STORE, dets).              % {User, Table} and {{links,User}, NoLinks}

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Internal.
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, dets_server, [self()], []).

start() -> 
    ensure_started().

stop() ->
    case whereis(?SERVER_NAME) of
	undefined ->
	    stopped;
	_Pid ->
            gen_server:call(?SERVER_NAME, stop, infinity)
    end.

all() ->
    call(all).

close(Tab) ->
    call({close, Tab}).

get_pid(Tab) ->
    ets:lookup_element(?REGISTRY, Tab, 3).

open_file(File) ->
    call({open, File}).

open_file(Tab, OpenArgs) ->
    call({open, Tab, OpenArgs}).

pid2name(Pid) ->
    ensure_started(),
    case ets:lookup(?OWNERS, Pid) of
        [] -> undefined;
        [{_Pid,Tab}] -> {ok, Tab}
    end.

users(Tab) ->
    call({users, Tab}).

verbose(What) ->
    call({set_verbose, What}).

call(Message) ->
    ensure_started(),
    gen_server:call(?SERVER_NAME, Message, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(Parent) ->
    Store = init(),
    {ok, #state{store=Store, parent=Parent}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(all, _From, State) ->
    F = fun(X, A) -> [element(1, X) | A] end,
    {reply, ets:foldl(F, [], ?REGISTRY), State};
handle_call({close, Tab}, {From, _Tag}, State) ->
    Res = handle_close(State, From, Tab, normal),
    {reply, Res, State};
handle_call(echo, _From, State) ->
    {reply, ok, State};
handle_call({open, File}, {From, _Tag}, State) ->
    Pid = spawn(dets, do_open_file, [File, get(verbose)]),
    receive
        {Pid, {ok, Tab}} ->
            Store = State#state.store,
            do_link(Store, From),
            true = ets:insert(Store, {From, Tab}),
            true = ets:insert(?REGISTRY, {Tab, 1, Pid}),
            true = ets:insert(?OWNERS, {Pid, Tab}),
            {reply, {ok, Tab}, State};
        {Pid, Error} ->
            {reply, Error, State}
    end;
handle_call({open, Tab, OpenArgs}, {From, _Tag}, State) ->
    Store = State#state.store,
    case ets:lookup(?REGISTRY, Tab) of
        [] -> 
            Pid = spawn(dets, do_open_file, [Tab, OpenArgs, get(verbose)]),
            receive
                {Pid, {ok, Result}} ->
                    do_link(Store, From),
                    true = ets:insert(Store, {From, Tab}),
                    true = ets:insert(?REGISTRY, {Tab, 1, Pid}),
                    true = ets:insert(?OWNERS, {Pid, Tab}),
                    {reply, {ok, Result}, State};
                {Pid, Error} ->
                    {reply, Error, State}
            end;
        [{Tab, _Counter, Pid}] ->
            Pid ! {self(), {add_user, Tab, OpenArgs}},
            receive
                {Pid, {ok, Result}} ->
                    do_link(Store, From),
                    true = ets:insert(Store, {From, Tab}),
                    ets:update_counter(?REGISTRY, Tab, 1),
                    {reply, {ok, Result}, State};
                {Pid, Error} ->
                    {reply, Error, State}
	    end
    end;
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({set_verbose, What}, _From, State) ->
    set_verbose(What),
    {reply, ok, State};
handle_call({users, Tab}, _From, State) ->
    Users = ets:select(State#state.store, [{{'$1', Tab}, [], ['$1']}]),
    {reply, Users, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State) ->
    %% First we need to figure out which tables that Pid are using.
    Store = State#state.store,
    All = ets:lookup(Store, Pid),
    handle_all(State, All),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    stop_all(Reason, State).

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

ensure_started() ->
    case whereis(?SERVER_NAME) of
	undefined -> 
	    DetsServer = {?SERVER_NAME, {?MODULE, start_link, []},
			  permanent, 2000, worker, [?MODULE]},
            supervisor:start_child(kernel_safe_sup, DetsServer),
            ok = gen_server:call(?SERVER_NAME, echo, infinity);
	_ -> ok
    end.

init() ->
    set_verbose(verbose_flag()),
    process_flag(trap_exit, true),
    ets:new(?REGISTRY, [set, named_table]),
    ets:new(?OWNERS, [set, named_table]),
    ets:new(?STORE, [duplicate_bag]).

verbose_flag() ->
    case init:get_argument(dets) of
	{ok, Args} ->
	    lists:member(["verbose"], Args);
	_ ->
	    false
    end.

set_verbose(true) ->
    put(verbose, yes);
set_verbose(_) ->
    erase(verbose).

stop_all(How, S) ->
    F = fun({{links, _}, _}, _) -> 
                ignore;
           ({Pid, Tab}, _) -> 
                handle_close(S, Pid, Tab, How)
        end,
    Store = S#state.store,
    ets:foldl(F, foo, Store),
    0 = ets:info(Store, size). %% assertion

handle_all(_S, []) ->
    done;
handle_all(S, [{From, Tab} | Tail]) ->
    handle_close(S, From, Tab, normal),
    handle_all(S, Tail).

handle_close(S, From, Tab, How) ->
    Store = S#state.store,
    case ets:match_object(Store, {From, Tab}) of
	[] -> 
	    ?DEBUGF("DETS: Table ~w close attempt by non-owner~w~n",
		    [Tab, From]),
	    {error, not_owner};
	[_ | Keep] ->
	    case ets:lookup(?REGISTRY, Tab) of
		[] -> 
		    {error, not_open};
		[{Tab, 1, Pid}] ->
		    do_unlink(Store, From),
		    true = ets:delete(?REGISTRY, Tab),
		    true = ets:delete(?OWNERS, Pid),
		    true = ets:match_delete(Store, {From, Tab}),
		    Pid ! {self(), close},
		    receive {Pid, {closed, Res}} -> Res end;
		[{Tab, _Counter, Pid}] ->
		    do_unlink(Store, From),
		    true = ets:match_delete(Store, {From, Tab}),
		    [true = ets:insert(Store, K) || K <- Keep],
		    ets:update_counter(?REGISTRY, Tab, -1),
		    if 
			How == shutdown ->
			    ok;
			true ->
			    Pid ! {self(), {close, From}},
			    receive {Pid, {closed, Res}} -> Res end
		    end
	    end
    end.

%% Links with counters
do_link(Store, Pid) ->
    Key = {links, Pid},
    case ets:lookup(Store, Key) of
	[] ->
	    true = ets:insert(Store, {Key, 1}),
	    link(Pid);
	[{_, C}] ->
	    true = ets:delete(Store, Key),
	    true = ets:insert(Store, {Key, C+1})
    end.

do_unlink(Store, Pid) ->
    Key = {links, Pid},
    case ets:lookup(Store, Key) of
	[{_, C}] when C > 1 ->
	    true = ets:delete(Store, Key),
	    true = ets:insert(Store, {Key, C-1});
	_ ->
	    true = ets:delete(Store, Key),
	    unlink(Pid)

    end.

