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

-module(httpc_handler).

-behaviour(gen_server).

-include("http.hrl").

%%--------------------------------------------------------------------
%% Application API
-export([start/2, send/2, cancel/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {request,        % #request{}
                session,        % #tcp_session{} 
                status_line,    % {Version, StatusCode, ReasonPharse}
                headers,        % #http_response_h{}
                body,           % binary()
                mfa,            % {Moduel, Function, Args}
                pipeline = queue:new(),  % queue() 
                busy = true,             % true | false
		canceled = [],	         % [RequestId]
                max_header_size = nolimit, % noimit | integer() 
                max_body_size = nolimit    % noimit | integer() 
               }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid}
%%
%% Description: Starts a http-request handler process. Intended to be
%% called by the httpc manager process.
%% %%--------------------------------------------------------------------
start(Request, ProxyOptions) ->
    %% Note will be monitored by the httpc_manager process
    %% The link functionality is not desired in this case.
    gen_server:start(?MODULE, [Request, ProxyOptions], []).

%%--------------------------------------------------------------------
%% Function: send(Request, Pid) -> ok 
%%	Request = #request{}
%%      Pid = pid() - the pid of the http-request handler process.
%%
%% Description: Uses this handlers session to send a request. Intended
%% to be called by the httpc manager process.
%%--------------------------------------------------------------------
send(Request, Pid) ->
    call(Request, Pid, 5000).

%%--------------------------------------------------------------------
%% Function: cancel(RequestId, Pid) -> ok
%%	RequestId = ref()
%%      Pid = pid() -  the pid of the http-request handler process.
%%
%% Description: Cancels a request. Intended to be called by the httpc
%% manager process.
%%--------------------------------------------------------------------
cancel(RequestId, Pid) ->
    cast({cancel, RequestId}, Pid).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Session]) -> {ok, State} | 
%%                       {ok, State, Timeout} | ignore |{stop, Reason}
%%
%% Description: Initiates the httpc_handler process 
%%
%% Note: The init function may not fail, that will kill the
%% httpc_manager process. We could make the httpc_manager more comlex
%% but we do not want that so errors will be handled by the process
%% sending an init_error message to itself.
%% 
%%--------------------------------------------------------------------
init([Request, ProxyOptions]) ->
    case http_transport:connect(Request, ProxyOptions) of
        {ok, Socket} ->
            case http_request:send(Request, Socket) of
                ok ->
		    ClientClose = client_close_value(Request),
		    Session = 
			#tcp_session{id = {Request#request.address, self()},
				     scheme = Request#request.scheme,
				     socket = Socket,
				     client_close = ClientClose},
                    State = #state{request = Request, 
				   session = Session},
		    NewState = State#state{mfa = 
					   {http_response, parse,
					    [State#state.max_header_size]}},
                    case ClientClose of
			true -> %% Not a presistent connection
			    ok;
			false  ->
			    httpc_manager:insert_session(Session)
		    end,
		    http_transport:setopts(Session#tcp_session.scheme, 
					   Socket, [{active, once}]),
                    activate_request_timeout(Request),
                    {ok, NewState};
                {error, Reason} -> 
		    self() ! {init_error, error_sending, 
			      http_response:error(Request, Reason)},
		    {ok, #state{request = Request,
				session = #tcp_session{socket = Socket}}}
            end;
        {error, Reason} -> 
            self() ! {init_error, error_connecting,
		      http_response:error(Request, Reason)},
            {ok, #state{request = Request}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _, State = #state{session = Session =
					   #tcp_session{socket = Socket}}) ->
    case http_request:send(Request, Socket) of
        ok ->
            case State#state.busy of
                true ->
                    NewPipeline = queue:in(Request, State#state.pipeline),
		    NewSession = 
			Session#tcp_session{pipeline_length = 
					    queue:len(NewPipeline)},
		    httpc_manager:insert_session(NewSession),
                    {reply, {ok, NewSession}, 
		     State#state{pipeline = NewPipeline,
				 session = NewSession}};
		false ->
		    activate_request_timeout(Request),
		    http_transport:setopts(Session#tcp_session.scheme, 
					   Session#tcp_session.socket, 
                                           [{active, once}]),
		    NewSession = Session#tcp_session{pipeline_length = 1},
		    NewState = 
			#state{request = Request, 
			       mfa = {http_response, parse, 
				      [State#state.max_header_size]},
			       session = NewSession},
		    httpc_manager:insert_session(NewSession),
		    {reply, ok, NewState}
	    end;
	{error, Reason} ->
	    http_response:send(Request#request.from, 
			       http_response:error(Request,Reason)), 
	    {stop, normal, 
	     State#state{request = Request#request{from = answer_sent}}}
    end.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({cancel, RequestId}, State) ->
    httpc_manager:request_canceled(RequestId),
    {noreply, State#state{canceled = [RequestId | State#state.canceled]}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Proto, _Socket, Data}, State = 
	    #state{mfa = {Module, Function, Args}, 
		   request = #request{method = Method}, session = Session}) 
  when Proto == tcp; Proto == ssl ->
    
    case Module:Function([Data | Args]) of
        {ok, Result} ->
            handle_http_msg(Result, State); 
        {_, whole_body, _} when Method == head ->
	    handle_response(State#state{body = <<>>}); 
	NewMFA ->
	    http_transport:setopts(Session#tcp_session.scheme, 
                                   Session#tcp_session.socket, 
				   [{active, once}]),
            {noreply, State#state{mfa = NewMFA}}
    end;
%% The Server may close the connection too indicate that the
%% whole body is now sent instead of sending an lengh
%% indicator.
handle_info({tcp_closed, _}, State = #state{mfa = {_, whole_body, Args}}) ->
    handle_response(State#state{body = hd(Args)}); 
handle_info({ssl_closed, _}, State = #state{mfa = {_, whole_body, Args}}) ->
    handle_response(State#state{body = hd(Args)}); 
%% Error cases
handle_info({tcp_closed, _}, State) ->
    {stop, session_remotly_closed, State};
handle_info({ssl_closed, _}, State) ->
    {stop, session_remotly_closed, State};
handle_info({tcp_error, _, _} = Reason, State) ->
    {stop, Reason, State};
handle_info({ssl_error, _, _} = Reason, State) ->
    {stop, Reason, State};
%% Timeouts
handle_info({timeout, Id}, State =  #state{request = Request = 
					   #request{id = Id}}) ->
    http_response:send(Request#request.from, 
		       http_response:error(Request, session_local_timeout)),
    {stop, normal, State#state{request = Request#request{from = answer_sent}}};
handle_info({timeout, _}, State) -> %% Response has been received so ignore
    {noreply, State};
%% Setting up the connection to the server somehow failed. 
handle_info({init_error, _, ClientErrMsg},
	    State = #state{request = Request}) ->
    http_response:send(Request#request.from, ClientErrMsg),
    {stop, normal, State#state{request = Request#request{from = answer_sent}}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> _  (ignored by gen_server)
%% Description: Shutdown the httpc_handler
%%--------------------------------------------------------------------
terminate(normal, #state{session = undefined}) ->
    ok;  %% Init error there is no socket to be closed.
terminate(normal, #state{request = Request, 
		    session = #tcp_session{id = undefined,
					   socket = Socket}}) ->  
    %% Init error sending, no session information has been setup but
    %% there is a socket that needs closing.
    http_transport:close(Request#request.scheme, Socket);

terminate(Reason, State = #state{session = Session, request = Request}) -> 
    case Request#request.from of
	answer_sent ->
	    ok;
	Pid ->
	    http_response:send(Pid, http_response:error(Request, Reason))
    end,
    catch httpc_manager:delete_session(Session#tcp_session.id),
    case queue:is_empty(State#state.pipeline) of 
	false ->
	    catch lists:foreach(fun(NewRequest) ->
					httpc_manager:retry_request(NewRequest)
				end, queue:to_list(State#state.pipeline));
	true ->
	    ok
    end,
    http_transport:close(Session#tcp_session.scheme, 
			 Session#tcp_session.socket).

%%--------------------------------------------------------------------
%% Func: code_change(_OldVsn, State, Extra) -> {ok, NewState}
%% Purpose: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_http_msg({Version, StatusCode, ReasonPharse, Headers, Body}, 
		State) ->
    
    case Headers#http_response_h.content_type of
        "multipart/byteranges" ++ _Param ->
            exit(not_yet_implemented);
        _ ->
            handle_http_body(Body, 
			     State#state{status_line = {Version, 
							StatusCode,
							ReasonPharse},
					 headers = Headers})
    end;
handle_http_msg({ChunkedHeaders, Body}, 
		State = #state{headers = Headers}) ->
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    handle_response(State#state{headers = NewHeaders, body = Body});
handle_http_msg(Body, State) ->
    handle_response(State#state{body = Body}).

handle_http_body(<<>>, State = #state{request = #request{method = head}}) ->
    handle_response(State#state{body = <<>>});

handle_http_body(Body, State = #state{headers = Headers, session = Session,
				      max_body_size = MaxBodySize,
				      request = Request}) ->
    case Headers#http_response_h.transfer_encoding of
        "chunked" ->
	    case http_chunk:decode(Body, State#state.max_body_size, 
				   State#state.max_header_size) of
		{Module, Function, Args} ->
		    http_transport:setopts(Session#tcp_session.scheme, 
					   Session#tcp_session.socket, 
					   [{active, once}]),
		    {noreply, State#state{mfa = 
					  {Module, Function, Args}}};
		{ok, {ChunkedHeaders, NewBody}} ->
		    NewHeaders = http_chunk:handle_headers(Headers, 
							   ChunkedHeaders),
		    handle_response(State#state{headers = NewHeaders, 
						body = NewBody})
	    end;
        Encoding when list(Encoding) ->
	    http_response:send(Request#request.from, 
			       http_response:error(Request, unknown_encoding)),
	    {stop, normal, 
	     State#state{request = Request#request{from = answer_sent}}};
        _ ->
            Length =
                list_to_integer(Headers#http_response_h.content_length),
            case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
                true ->
                    case http_response:whole_body(Body, Length) of
                        {ok, Body} ->
			    handle_response(State#state{body = Body});
                        MFA ->
                            http_transport:setopts(
			      Session#tcp_session.scheme, 
			      Session#tcp_session.socket, 
			      [{active, once}]),
			    {noreply, State#state{mfa = MFA}}
		    end;
                false ->
		    http_response:send(Request#request.from,
				       http_response:error(Request, 
							   body_too_big)),
                    {stop, normal, 
		     State#state{request = Request#request{from = answer_sent}}}
            end
    end.

handle_response(State = #state{request = Request = #request{id = ID,
							    from = Client},
			       session = Session, 
			       status_line = StatusLine,
			       headers = Headers, body = Body}) ->
    
    case lists:member(ID, State#state.canceled) of
	true ->
	    handle_pipeline(
	      State#state{canceled = lists:delete(ID, 
						  State#state.canceled)});
	false ->
	    case http_response:result({StatusLine, Headers, Body}, 
				       Request, Session) of
		{ok, ""} -> % redirect
		    handle_pipeline(State);
		{ok, Msg} ->
		    http_response:send(Client, Msg),
		    handle_pipeline(State);
		{stop, Msg} ->
		    http_response:send(Client, Msg),
		    {stop, normal, 
		     State#state{request = Request#request{from = answer_sent}}}
	    end
    end.

handle_pipeline(State = #state{session = Session}) ->
    case queue:out(State#state.pipeline) of
	{empty, _} ->
	    {noreply, 
	     State#state{busy = false, 
			 mfa = {http_response, parse,
				[State#state.max_header_size]}}};
	{{value, NextRequest}, Pipeline} ->
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of
		true ->
		    handle_pipeline(
		      State#state{canceled = 
				  lists:delete(NextRequest#request.id, 
					       State#state.canceled),
				  pipeline = Pipeline}); 
		false ->
		    http_transport:setopts(Session#tcp_session.scheme, 
					   Session#tcp_session.socket, 
                                           [{active, once}]),
		    {noreply, 
		     State#state{pipeline = Pipeline,
				 request = NextRequest,
				 mfa = {http_response, parse,
					[State#state.max_header_size]}}}
	    end
    end.

call(Msg, Pid, Timeout) ->
    gen_server:call(Pid, Msg, Timeout).

cast(Msg, Pid) ->
    gen_server:cast(Pid, Msg).

activate_request_timeout(Request) ->
    Time = (Request#request.settings)#http_options.timeout,
    case Time of
	infinity ->
	    no_timer;
	_ ->
	    erlang:send_after(Time, self(), {timeout, Request#request.id})
    end.

client_close_value(Request) ->
    case (Request#request.headers)#http_request_h.connection of
	"close" ->
	    true;
	 _ ->
	    false
    end.
