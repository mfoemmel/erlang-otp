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
%%----------------------------------------------------------------------
%% Purpose: Monitor connections and timers
%%----------------------------------------------------------------------

-module(megaco_monitor).

-behaviour(gen_server).

%% Application internal exports
-export([
	 start_link/0,
	 apply_after/4,
	 cancel_apply_after/1,

	 lookup_request/1,
	 match_requests/1,
	 insert_request/1,
	 delete_request/1, 

	 lookup_reply/1,
	 match_replies/1,
	 insert_reply/1,
	 delete_reply/1,

	 apply_at_exit/4,
	 cancel_apply_at_exit/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {parent_pid}).
-record(apply_at_exit, {ref, pid, module, function, arguments}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

lookup_request(Key) ->
    ets:lookup(megaco_requests, Key).

match_requests(Pat) ->
    ets:match_object(megaco_requests, Pat).

insert_request(Rec) ->
    ets:insert(megaco_requests, Rec).

delete_request(Key) ->
    ets:delete(megaco_requests, Key).

lookup_reply(Key) ->
    ets:lookup(megaco_replies, Key).

match_replies(Pat) ->
    ets:match_object(megaco_replies, Pat).

insert_reply(Rec) ->
    ets:insert(megaco_replies, Rec).

delete_reply(Key) ->
    ets:delete(megaco_replies, Key).

apply_after(M, F, A, Time) when atom(M), atom(F), list(A) ->
    if
	Time == infinity ->
	    apply_after_infinity;
	integer(Time) ->
	    Msg = {apply_after, M, F, A},
	    Ref = erlang:send_after(Time, whereis(?SERVER), Msg),
	    {apply_after, Ref}
    end.

cancel_apply_after({apply_after, Ref}) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_apply_after(apply_after_infinity) ->
    ok;
cancel_apply_after(BadRef) ->
    {error, {bad_ref, BadRef}}.

%% Performs apply(M, F, [Reason | A]) when process Pid dies
apply_at_exit(M, F, A, Pid) 
  when atom(M), atom(F), list(A), pid(Pid) ->
    Ref = call({apply_at_exit, M, F, A, Pid}),
    {apply_at_exit, Ref}.

cancel_apply_at_exit({apply_at_exit, Ref}) ->
    cast({cancel_apply_at_exit, Ref});
cancel_apply_at_exit(BadRef) ->
    {error, {bad_ref, BadRef}}.
    
call(Request) ->
    gen_server:call(?SERVER, Request, infinity).

cast(Msg) ->
    ?SERVER ! Msg, ok.

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

init([Parent]) ->
    process_flag(trap_exit, true),
    ets:new(megaco_requests,     [public, named_table, {keypos, 2}]),
    ets:new(megaco_replies,      [public, named_table, {keypos, 2}]),
    {ok, #state{parent_pid = Parent}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({apply_at_exit, M, F, A, Pid}, _From, S) ->
    Ref = erlang:monitor(process, Pid),
    AAE = #apply_at_exit{ref       = Ref,
			 pid       = Pid,
			 module    = M,
			 function  = F,
			 arguments = A},
    put({?MODULE, Ref}, AAE),
    Reply = Ref,
    {reply, Reply, S};
handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    {reply, Reply, S}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~p, ~p)~n",
                             [?MODULE, self(), Msg, S]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({cancel_apply_at_exit, Ref}, S) ->
    case erase({?MODULE, Ref}) of
	undefined ->
	    %% Reply = {error, {already_cancelled, {apply_at_exit, Ref}}},
	    {noreply, S};
	_AAE ->
	    erlang:demonitor(Ref),
	    {noreply, S}
    end;

handle_info({apply_after, M, F, A}, S) ->
    handle_apply(M, F, A, apply_after),
    {noreply, S};

handle_info({'DOWN', Ref, process, _Pid, Reason}, S) ->
    case erase({?MODULE, Ref}) of
	undefined ->
	    {noreply, S};
	AAE ->
	    M = AAE#apply_at_exit.module,
	    F = AAE#apply_at_exit.function,
	    A = AAE#apply_at_exit.arguments,
	    handle_apply(M, F, [Reason | A], apply_at_exit),
	    {noreply, S}
    end;

handle_info({'EXIT', Pid, Reason}, S) when Pid == S#state.parent_pid ->
    %% [megaco_messenger:disconnect(CH, {stopped, Reason})
    %% 	|| CH <- megaco:lookup_system_info(connections)],
    {stop, Reason, S};

handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
                             [?MODULE, self(), Info, S]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

handle_apply(M, F, A, _ErrorTag) ->
    spawn(M, F, A).
