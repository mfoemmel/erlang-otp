% ``The contents of this file are subject to the Erlang Public License,
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

-module(httpc_manager).

-behaviour(gen_server).

-include("http.hrl").

%% Application API
-export([start_link/0, request/1, cancel_request/1,
	 request_canceled/1, retry_request/1, insert_session/1, 
	 delete_session/1, set_options/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {
	  cancel = [],	 % [{RequestId, HandlerPid, ClientPid}]  
	  handler_db,    % ets() - Entry: {Requestid, HandlerPid, ClientPid}
	  options = #options{}
	 }).

-record(options, {
	  proxy =  {undefined, []}, % {{ProxyHost, ProxyPort}, [NoProxy]},
	  max_pipeline_size = ?TCP_PIPELINE_LENGTH,
	  max_sessions =  ?MAX_TCP_SESSIONS
	 }).

%%====================================================================
%% Application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid}
%%
%% Description: Starts the http request manger process. (Started by
%% the intes supervisor.)
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []). 

%%--------------------------------------------------------------------
%% Function: request() -> {ok, Requestid} | {error, Reason}
%%	Request = #request{}
%%
%% Description: Sends a request to the httpc manager process.
%%--------------------------------------------------------------------
request(Request) ->
    call({request, Request}, infinity).

%%--------------------------------------------------------------------
%% Function: retry_request(Request) -> _
%%	Request = #request{}
%%
%% Description: Resends a request to the httpc manager process, intended
%% to be called by the httpc handler process if it has to terminate with
%% a non empty pipeline.
%%--------------------------------------------------------------------
retry_request(Request) ->
    cast({retry_request, Request}).

%%--------------------------------------------------------------------
%% Function: cancel_request(RequestId) -> ok
%%	RequestId - ref()
%%
%% Description: Cancels the request with <RequestId>.
%%--------------------------------------------------------------------
cancel_request(RequestId) ->
    call({cancel_request, RequestId}, infinity).

%%--------------------------------------------------------------------
%% Function: request_canceled(RequestId) -> ok
%%	RequestId - ref()
%%
%% Description: Confirms that a request has been canceld. Intended to
%% be called by the httpc handler process.
%%--------------------------------------------------------------------
request_canceled(RequestId) ->
    cast({request_canceled, RequestId}).

%%--------------------------------------------------------------------
%% Function: insert_session(Session) -> _
%%	Session - #tcp_session{}
%%
%% Description: Inserts session information into the httpc manager table
%% httpc_manager_session_db. Intended to be called by the httpc request
%% handler process.
%%--------------------------------------------------------------------
insert_session(Session) ->
    ets:insert(httpc_manager_session_db, Session).

%%--------------------------------------------------------------------
%% Function: delete_session(SessionId) -> _
%%	SessionId -  {{Host, Port}, HandlerPid}
%% 
%% Description: Deletes session information from the httpc manager table
%% httpc_manager_session_db. Intended to be called by the httpc request
%% handler process.
%%--------------------------------------------------------------------
delete_session(SessionId) ->
    ets:delete(httpc_manager_session_db, SessionId).

%%--------------------------------------------------------------------
%% Function: set_options(Options) -> ok
%%
%% Options = [Option]
%% Option = {proxy, {Proxy, [NoProxy]}} | {max_pipeline_size, Max} |
%%          {max_sessions, Max}
%% Proxy = {Host, Port}
%% NoProxy - [Domain | HostName | IPAddress]     
%% Max - integer() 
%% 
%% Description: Sets the options to be used by the client.
%%--------------------------------------------------------------------
set_options(Options) ->
    cast({set_options, Options}).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Session]) -> {ok, State} | 
%%                       {ok, State, Timeout} | ignore |{stop, Reason}
%% Description: Initiates the httpc_manger process
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    ets:new(httpc_manager_session_db, 
	    [public, set, named_table, {keypos, #tcp_session.id}]),
    {ok, #state{handler_db = ets:new(handler_db, [protected, set])}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({request, Request}, _, State) ->
    handle_request(Request, State);
handle_call({cancel_request, RequestId}, From, State) ->
    case ets:lookup(State#state.handler_db, RequestId) of
	[] ->
	    ok, %% Nothing to cancel
	      {reply, ok, State};
	[{_, Pid, _}] ->
	    httpc_handler:cancel(RequestId, Pid),
	    {noreply, State#state{cancel = 
				  [{RequestId, Pid, From} |
				   State#state.cancel]}}
    end;
handle_call(Msg, From, State) ->
    error_logger:error_report("HTTPC_MANAGER recived unkown call: ~p"
			      "from: ~p~n", [Msg, From]),
    {reply, {error, 'API_violation'}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({retry_request, Request}, State) ->
    case handle_request(Request, State) of
	{reply, {ok, _}, NewState} ->
	    {noreply, NewState};
	{reply, Error, NewState} ->
	    http_response:error(Request, Error),
	    {noreply, NewState}
    end;
handle_cast({request_canceled, RequestId}, State) ->
    ets:delete(State#state.handler_db, RequestId),
    case lists:keysearch(RequestId, 1, State#state.cancel) of
	{value, Entry = {RequestId, _, From}} ->
	    gen_server:reply(From, ok),
	    {noreply, 
	     State#state{cancel = lists:delete(Entry, State#state.cancel)}};
	_ ->
	   {noreply, State}
    end;
handle_cast({set_options, Options}, State) ->
    NewOptions = 
	#options{proxy = 
		 httpd_util:key1search(Options, proxy, {undefined, []}),
		 max_pipeline_size =
		 httpd_util:key1search(Options, max_pipeline_size, 
				       ?TCP_PIPELINE_LENGTH),
		 max_sessions = 
		 httpd_util:key1search(Options, max_pipeline_size, 
				       ?MAX_TCP_SESSIONS)
		}, 
    {noreply, State#state{options = NewOptions}};
handle_cast(Msg, State) ->
    error_logger:error_report("HTTPC_MANAGER recived unkown cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling all non call/cast messages
%%---------------------------------------------------------
handle_info({'DOWN', _, _, Pid, _}, State) ->
    ets:match_delete(State#state.handler_db, {'_', Pid, '_'}),

    %% If there where any canceled request, handled by the
    %% the process that now has terminated, the
    %% cancelation can be viewed as sucessfull!
    NewCanceldList = 
	lists:foldl(fun(Entry = {_, HandlerPid, From}, Acc)  ->
			    case HandlerPid of
				Pid ->
				    gen_server:reply(From, ok),
				    lists:delete(Entry, Acc);
				_ ->
				    Acc
			    end 
		    end, State#state.cancel, State#state.cancel),
    {noreply, State#state{cancel = NewCanceldList}};    
handle_info(Info, State) ->
    error_logger:error_report("Unknown message in "
			      "httpc_manager:handle_info ~p~n", [Info]),
    {noreply, State}. 
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> _  (ignored by gen_server)
%% Description: Shutdown the httpc_handler
%%--------------------------------------------------------------------
terminate(_, State) ->
    ets:delete(httpc_manager_session_db),
    ets:delete(State#state.handler_db).

%%--------------------------------------------------------------------
%% Func: code_change(_OldVsn, State, Extra) -> {ok, NewState}
%% Purpose: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
handle_request(Request, 
	       State = #state{options = #options{proxy = ProxyOptions}}) ->
    case select_session(Request#request.address, State) of
	{ok, HandlerPid} ->
	    NewRequest = generate_request_id(Request),
	    Reply = pipeline(NewRequest, HandlerPid, State),
	    {reply, Reply, State};
	no_connection ->
	    NewRequest = generate_request_id(Request),
	    {ok, Pid} = httpc_handler:start(NewRequest, ProxyOptions),
	    ets:insert(State#state.handler_db, {NewRequest#request.id, 
						Pid, NewRequest#request.from}),
	    erlang:monitor(process, Pid),
	    {reply, {ok, NewRequest#request.id}, State};
	{no_session,  OpenSessions} when OpenSessions 
	< State#options.max_sessions ->
	    NewRequest = generate_request_id(Request),
	    {ok, Pid} = httpc_handler:start(NewRequest, ProxyOptions),
	    erlang:monitor(process, Pid),
	    {reply, {ok, NewRequest#request.id}, State};
	{no_session, _} ->
	    {reply, {error, too_many_sessions}, State}
    end.

select_session(HostPort, #state{options = 
				#options{max_pipeline_size = Max}}) ->
    Candidates = ets:match(httpc_manager_session_db,
			   {'_', {HostPort, '$1'}, false, '_', '_', '$2'}),
    
    case Candidates of 
	[] ->
	  no_connection; 
	_ ->
	    NewCandidates = 
		lists:foldl(
		  fun([Pid, PipelineLength], Acc) when 
			    PipelineLength =< Max ->
			  [{Pid, PipelineLength} | Acc];
		     (_, Acc) ->
			  Acc
		  end, [], Candidates),
	    
	    case lists:keysort(2, NewCandidates) of
		[] ->
		    {no_session, length(Candidates)};
		[{HandlerPid, _} | _] ->
		    {ok, HandlerPid}
	    end
    end.
	    
pipeline(Request, HandlerPid, State = 
	 #state{options = #options{proxy = ProxyOptions}}) ->
    case (catch httpc_handler:send(Request, HandlerPid)) of
	ok ->
	    ets:insert(State#state.handler_db, {Request#request.id, 
						HandlerPid,
						Request#request.from}),
	    {ok, Request#request.id};
	_  -> %timeout pipelining failed 
	    {ok, Pid} = httpc_handler:start(Request, ProxyOptions),
	    ets:insert(State#state.handler_db, {Request#request.id, 
						Pid, Request#request.from}),
	    {ok, Request#request.id}
    end.

generate_request_id(Request) ->
    case Request#request.id of
	undefined ->
	    RequestId = make_ref(),
	    Request#request{id = RequestId};
	_ ->
	    %% This is an automatic redirect or a retryed pipelined
	    %% request keep the old id.
	    Request
    end.

call(Msg, Timeout) ->
    gen_server:call(?MODULE, Msg, Timeout).

cast(Msg) ->
   gen_server:cast(?MODULE, Msg).
