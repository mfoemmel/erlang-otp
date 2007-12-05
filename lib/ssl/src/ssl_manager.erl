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
%%----------------------------------------------------------------------
%% Purpose: Manages ssl sessions and trusted certifacates
%%----------------------------------------------------------------------

-module(ssl_manager).
-behaviour(gen_server).

%% Internal application API
-export([start_link/0, register_trusted_certs/1, 
	 lookup_trusted_cert/3, client_session_id/3, server_session_id/2,
	 register_session/2, register_session/3, invalidate_session/2,
	 invalidate_session/3]).

% Spawn export
-export([init_session_validator/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").

-record(state, {
	  session_cache,
	  certificate_db,
	  session_validation_timer
	 }).

-define('24H_in_sec', 86400).
-define('24H_in_msec', 8640000).
-define(SESSION_VALIDATION_INTERVAL, 60000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: 
%% Description: 
%%--------------------------------------------------------------------
register_trusted_certs(File) ->
    call({trusted_certs, File}).

%%--------------------------------------------------------------------
%% Function: 
%% Description: 
%%--------------------------------------------------------------------
lookup_trusted_cert(SerialNumber, Issuer, Ref) ->
    ssl_certificate_db:lookup_trusted_cert(Ref, SerialNumber, Issuer).

%%--------------------------------------------------------------------
%% Function: 
%% Description: 
%%--------------------------------------------------------------------
client_session_id(Host, Port, SslOpts) ->
    call({client_session_id, Host, Port, SslOpts}).
   
%%--------------------------------------------------------------------
%% Function: 
%% Description: 
%%--------------------------------------------------------------------
server_session_id(Port, SuggestedSessionId) ->
    call({server_session_id, Port, SuggestedSessionId}).

%%--------------------------------------------------------------------
%% Function: 
%% Description: 
%%--------------------------------------------------------------------
register_session(Host, Port, Session) ->
    cast({register_session, Host, Port, Session}).

register_session(Port, Session) ->
    cast({register_session, Port, Session}).

%%--------------------------------------------------------------------
%% Function: 
%% Description: 
%%--------------------------------------------------------------------
invalidate_session(Host, Port, Session) ->
    cast({invalidate_session, Host, Port, Session}).

invalidate_session(Port, Session) ->
    cast({invalidate_session, Port, Session}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    CertDb = ssl_certificate_db:create(),
    SessionCache = ssl_session:create_cache(),
    Timer = erlang:send_after(?'24H_in_msec', self(), validate_sessions),
    {ok, #state{certificate_db = CertDb,
		session_cache = SessionCache,
		session_validation_timer = Timer}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({{trusted_certs, File}, Pid}, _From, 
	    State = #state{certificate_db = Db}) ->
    erlang:monitor(process, Pid),
    Result = 
	case (catch ssl_certificate_db:add_trusted_certs(Pid, File, Db)) of
	    {ok, Ref} ->
		{ok, Ref};
	    Error ->
		{error, Error}
	end,
    {reply, Result, State};

handle_call({{client_session_id, Host, Port, SslOpts}, _}, _, State) ->
    Id = ssl_session:id({Host, Port, SslOpts}),
    {reply, Id, State};

handle_call({{server_session_id, Port, SuggestedSessionId}, _}, _, State) ->
    Id = ssl_session:id(Port, SuggestedSessionId),
    {reply, Id, State};

handle_call(_,_, State) ->
    {reply, ok, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_session, Host, Port, Session}, State) ->
    TimeStamp = calendar:datetime_to_gregorian_seconds({date(), time()}),
    NewSession = Session#session{time_stamp = TimeStamp},
    ssl_session:cache_update(Host, Port, NewSession),
    {noreply, State};

handle_cast({register_session, Port, Session}, State) ->    
    TimeStamp = calendar:datetime_to_gregorian_seconds({date(), time()}),
    NewSession = Session#session{time_stamp = TimeStamp},
    ssl_session:cache_update(Port, NewSession),
    {noreply, State};

handle_cast({invalidate_session, Host, Port, Session}, State) ->
    ssl_session:cache_delete(Host, Port, Session),
    {noreply, State};

handle_cast({invalidate_session, Port, Session}, State) ->
    ssl_session:cache_delete(Port, Session),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%-------------------------------------------------------------------- 
handle_info(validate_sessions, State) ->
    Timer = erlang:send_after(?SESSION_VALIDATION_INTERVAL, 
			      self(), validate_sessions),
    start_session_validator(),
    {noreply, State#state{session_validation_timer = Timer}};

handle_info({'EXIT', _, _}, State) ->
    %% Session validator died!! Do we need to take any action?
    %% maybe error log
    {noreply, State};

handle_info({'DOWN', _Ref, _Type, _Pid, ecacertfile}, State) ->
    {noreply, State};

handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, 
	    State = #state{certificate_db = Db}) ->
    ssl_certificate_db:remove_trusted_certs(Pid, Db),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{certificate_db = Db,
			  session_cache = SessionCache,
			  session_validation_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    ssl_certificate_db:remove(Db),
    ssl_session:remove_cache(SessionCache),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(Msg) ->
    gen_server:call(?MODULE, {Msg, self()}, infinity).

cast(Msg) ->
    gen_server:cast(?MODULE, Msg).
 
validate_session(Host, Port, Session) ->
    case valid_session(Session) of
	true ->
	    ok;
	false ->
	    invalidate_session(Host, Port, Session)
    end.

validate_session(Port, Session) ->
    case valid_session(Session) of
	true ->
	    ok;
	false ->
	    invalidate_session(Port, Session)
    end.

valid_session(#session{time_stamp = TimeStamp}) ->
    Now =  calendar:datetime_to_gregorian_seconds({date(), time()}),
    Now - TimeStamp < ?'24H_in_sec'.
		    
start_session_validator() ->
    spawn_link(ssl_manager, init_session_validator, []).

init_session_validator() ->
    ssl_session:safe_fix_cache(true),
    Key = ssl_session:cache_first(),
    session_validation(Key).

session_validation(cache_end) ->
    ssl_session:safe_fix_cache(false),
    ok;
session_validation(Key = {Host, Port, SessionId}) ->
    Session = ssl_session:cache_lookup(Host, Port, SessionId),
    validate_session(Host, Port, Session),
    session_validation(ssl_session:cache_next(Key));
session_validation(Key = {Port, SessionId}) ->
    Session = ssl_session:cache_lookup(Port, SessionId),
    validate_session(Port, Session),
    session_validation(ssl_session:cache_next(Key)).
    
