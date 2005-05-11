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
-module(httpd_request_handler).

%% app internal api
-export([start/2, synchronize/3]).

%% module internal api
-export([init/2, handle_next_request/3]). 

-include("httpd.hrl").
-include("httpd_verbosity.hrl").
-include("http.hrl").

%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid}
%%
%% Description: Starts a httpd-request handler process. Intended to be
%% called by the httpd acceptor process.
%% %%--------------------------------------------------------------------
start(Manager, ConfigDB) ->
    Pid = proc_lib:spawn(?MODULE, init, [Manager, ConfigDB]),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% Function: synchronize(Pid, SocketType, Socket) -> void()
%%
%% Pid = pid()
%% SocketType = ip_comm | ssl 
%% Socket = socket()
%% Description: Send a synchronize message. Intended to be called by
%% the http acceptor process when it has transfered the socket ownership
%% to this process. 
%%--------------------------------------------------------------------
synchronize(Pid, SocketType, Socket) ->
    Pid ! {synchronize, SocketType, Socket}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
init(Manager, ConfigDB) ->
    {SocketType, Socket, {Status, Verbosity}} = await_synchronize(Manager),
    put(sname,self()),
    put(verbosity,?vvalidate(Verbosity)),
    Resolve     = http_transport:resolve(),
    Peername    = httpd_socket:peername(SocketType, Socket),
    InitData    = #init_data{peername=Peername, resolve=Resolve},
    handle_connection(Status, Manager, #mod{config_db = ConfigDB, 
					    socket_type = SocketType, 
					    socket = Socket,
					    init_data = InitData}).
handle_connection({reject, busy}, Manager, 
		  #mod{config_db = ConfigDB} = ModData) ->
    MaxHeaderSize =
	httpd_util:lookup(ConfigDB, max_header_size, ?HTTP_MAX_HEADER_SIZE),
    TimeOut = httpd_util:lookup(ConfigDB, keep_alive_timeout, 150000),
    case (catch receive_http_msg({httpd_request, parse, [MaxHeaderSize]},
				 ModData, TimeOut)) of
	{_, _, Version, _, _} ->
	    handle_busy(Manager, ModData#mod{http_version = Version}); 
	{error, Reason, NewModData} ->
	    handle_request_receive_error(Reason, NewModData)
    end;

handle_connection({reject, blocked}, Manager, 
	    #mod{config_db = ConfigDB} = ModData) ->
    MaxHeaderSize =
	httpd_util:lookup(ConfigDB, max_header_size, ?HTTP_MAX_HEADER_SIZE),
    TimeOut = httpd_util:lookup(ConfigDB, keep_alive_timeout, 150000),
    case (catch receive_http_msg({httpd_request, parse, [MaxHeaderSize]},
				 ModData, TimeOut)) of
	{_, _, Version, _, _} ->
	    handle_blocked(Manager, ModData#mod{http_version = Version}); 
	{error, Reason, NewModData} ->
	    handle_request_receive_error(Reason, NewModData)
    end;

handle_connection(accept, Manager, ModData) ->
    handle_connection(Manager, ModData).


await_synchronize(Manager) ->
    %% Make sure this process terminates if the httpd manager process
    %% should die!
    link(Manager), 
    receive
	{synchronize, SocketType, Socket} ->
	    ?vlog("received syncronize: "
		  "~n   SocketType: ~p"
		  "~n   Socket:     ~p", [SocketType, Socket]),
	    {SocketType, Socket, httpd_manager:new_connection(Manager)}
    after 15000 ->
	    exit(synchronize_timeout)
    end.

handle_busy(Manager, #mod{config_db = ConfigDB} = ModData) ->
    MaxClients = httpd_util:lookup(ConfigDB, max_clients, 150),
    Reason = io_lib:format("heavy load (>~w processes)", [MaxClients]),
    reject_connection(Manager, ModData, Reason).

handle_blocked(Manager, ModData) ->
    String = "Server maintenance performed, try again later",
    reject_connection(Manager, ModData, String).

reject_connection(_Manager, #mod{socket_type = SocketType, socket = Socket} =
		  ModData, Reason) ->
    ReasonStr = lists:flatten(Reason),
    ?vtrace("send status (503) message", []),
    httpd_response:send_status(ModData, 503, ReasonStr),
    close_sleep(SocketType, 1000),  
    ?vtrace("close the socket", []),
    httpd_socket:close(SocketType, Socket).

handle_connection(Manager, #mod{config_db = ConfigDB, 
				socket_type = SocketType, socket = Socket} 
		  = ModData) ->
    ?vlog("handle connection: ~p", [Socket]),
    TimeOut     = httpd_util:lookup(ConfigDB, keep_alive_timeout, 150000),
    NrOfRequest = httpd_util:lookup(ConfigDB, 
				    max_keep_alive_request, forever),
    ?MODULE:handle_next_request(ModData, NrOfRequest, TimeOut),
    ?vlog("handle connection: done", []),
    httpd_manager:done_connection(Manager),
    ?vlog("handle connection: close socket", []),
    close_sleep(SocketType, 1000),  
    httpd_socket:close(SocketType, Socket).

handle_next_request(_, NrOfRequests, _) when NrOfRequests < 1 -> 
    ?vtrace("handle_next_request: done", []),
    ok;
handle_next_request(#mod{socket = Socket, config_db = ConfigDB} 
		   = ModData, NrOfRequests, Timeout) ->

    case (catch handle_request(ModData, Timeout)) of
        {'EXIT', Reason} ->
            ?vlog("exit reading from socket: ~p",[Reason]),
            error_logger:error_report({'EXIT',Reason}),
	    Reason = 
		io_lib:format("exit reading from socket: ~p => ~n~p~n",
			      [Socket, Reason]),
	    error_log(Reason, ModData);
	{error, Reason, NewModData} ->
            handle_request_receive_error(Reason, NewModData);
        NewModData when record(NewModData, mod) ->
            case NewModData#mod.connection of
                true ->
                    ReqTimeout = 
			httpd_util:lookup(ConfigDB, 
					  keep_alive_timeout, 150000),
		    ?MODULE:handle_next_request(NewModData,
					       dec(NrOfRequests),
					       ReqTimeout);
		_ ->
                    ok
            end;
        _ ->
            ok
    end.

handle_request(#mod{config_db = ConfigDB} = ModData, Timeout) ->
    MaxHeaderSize =
	httpd_util:lookup(ConfigDB, max_header_size, ?HTTP_MAX_HEADER_SIZE),
    {Method, Uri, Version, {RecordHeaders, Headers}, Body} =
	receive_http_msg({httpd_request, parse, [MaxHeaderSize]},
			 ModData, Timeout),
    
    case httpd_request:validate(Method, Uri, Version) of
	ok  ->
	    {ok, NewModData} = 
		httpd_request:update_mod_data(ModData, Method, Uri,
					      Version, Headers),
	    
	    case is_host_specified_if_required(NewModData#mod.absolute_uri,
					       RecordHeaders, Version) of
		true ->
		    case handle_body(RecordHeaders, MaxHeaderSize,
				     Body, NewModData, Timeout) of
			{ok, NewBody} ->
			    respond(NewBody, [], NewModData);
			Other ->
			    Other
		    end;
		false ->
		    httpd_response:send_status(ModData#mod{http_version = 
							   Version}, 
					       400, none),
		    {error,"No host specified"}
	    end;
	{error, {not_supported, What}} ->
	    error_logger:error_report("httpd_request_handler: "  
 				      "Not supported ~p~n", [What]),
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       501, {Method, Uri, Version}),
	    {error, "Not supported"};
	{error, {bad_request, {forbidden, URI}}} ->
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       403, URI),
	    {error,"Forbidden Request"}
    
    end.

receive_http_msg({Module, Function, Args},
		 #mod{socket_type  = SocketType, socket = Socket} = ModData,
		 Timeout) ->
    http_transport:setopts(SocketType, Socket, [binary,{packet, 0},
						{active, once}]),
    receive
	{Proto, Socket, Data} when Proto == tcp; Proto == ssl ->
	    case Module:Function([Data | Args]) of
		{ok, Result} ->
		    Result;
		{error, Reason, Version} ->
		    throw({error, Reason, 
			   ModData#mod{http_version = Version}});
		NewMFA ->
		    receive_http_msg(NewMFA, ModData, Timeout)
	    end;	
	{tcp_closed, Socket} ->
	    throw({error, session_remotely_closed, ModData});
	{tcp_error, Socket, Reason} ->
	    throw({error, Reason, ModData});
	{ssl_closed, Socket} ->
	    throw({error, session_remotely_closed, ModData});
	{ssl_error, Socket, Reason, ModData} ->
	    throw({error, Reason})
    after Timeout ->
	    Version = case (catch lists:nth(3, lists:last(Args))) of
			  Ver when is_list(Ver) ->
			      Ver;
			  _  ->
			      unknown
		      end,
	    throw({error, session_local_timeout, 
		   ModData#mod{http_version = Version}})
    end.

handle_body(RecordHeaders, MaxHdrSz, Body, #mod{config_db = ConfigDB,
						socket = Socket,
						socket_type = SocketType}
	    = ModData, Timeout) ->
    
    MaxBodySize = httpd_util:lookup(ConfigDB, max_body_size, nolimit),
    
    case handle_expect(RecordHeaders, ModData, MaxBodySize) of 
	ok ->
	    case (catch handle_body(RecordHeaders, MaxHdrSz, 
				    Body, MaxBodySize, 
				    ModData, Timeout)) of
		
		{NewHeaders, NewBody} when is_record(NewHeaders, 
						     http_request_h), 
					   is_list(NewBody) ->  
		    {ok, NewBody};
		{error, unknown_coding} ->
		    httpd_response:send_status(ModData, 501, 
					       "Unknown Transfer-Encoding"),
		    httpd_socket:close(SocketType, Socket),
		    {socket_closed,"Expect conditions was not fullfilled"};
		{error, body_too_big} ->
		    httpd_response:send_status(ModData, 417, "Body too big"),
		    httpd_socket:close(SocketType, Socket),
		    {socket_closed,"Expect denied according to size"};
		{error, session_local_timeout} ->
		    httpd_response:send_status(ModData, 408, 
					       "Request timeout"),
		    httpd_socket:close(SocketType, Socket),
		    {socket_closed, "Local timeout"};
		Other ->
		    error_logger:error_report("Reason for internal" 
					      "server error: ~p~n", [Other]),
		    httpd_response:send_status(ModData, 500, none),
		    httpd_socket:close(SocketType, Socket),
		    {socket_closed, Other}
	    end;
	Other ->
	    Other

    end.
	
handle_body(Headers, MaxHeaderSize, Body, MaxBodySize, ModData, Timeout)->
    case Headers#http_request_h.'transfer-encoding' of
	"chunked" ->
	    {ChunkedHeaders, NewBody} =
		case http_chunk:decode(Body, MaxBodySize, MaxHeaderSize) of
		    {Module, Function, Args} ->
			receive_http_msg({Module, Function, Args},
					 ModData, Timeout);
		    {ok, Result} ->
			Result
		end,
	    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
	    {NewHeaders, binary_to_list(NewBody)}; 
	Encoding when list(Encoding) ->
	    throw({error, unknown_coding, ModData});
	_ -> 
	    Length = 
		list_to_integer(Headers#http_request_h.'content-length'),
	    case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
		true ->
		    case httpd_request:whole_body(Body, Length) of 
			{Module, Function, Args} ->
			    NewBody = 
				receive_http_msg({Module, Function, Args}, 
						 ModData, Timeout),
			    {Headers, binary_to_list(NewBody)};
			{ok, NewBody} ->
			    {Headers, binary_to_list(NewBody)}
		    end;
		false ->
		    throw({error, body_too_big, ModData}) 
	    end
    end.

is_host_specified_if_required(nohost, #http_request_h{host = undefined}, 
			      "HTTP/1.1") ->
    false;
is_host_specified_if_required(_, _, _) ->
    true.

handle_expect(Headers, #mod{socket = Socket, socket_type = SocketType,
			    config_db = ConfigDB} = ModData, MaxBodySize) ->
    Length = Headers#http_request_h.'content-length',
    case expect(Headers, ModData#mod.http_version, ConfigDB) of
	continue when MaxBodySize > Length; MaxBodySize == nolimit ->
	    httpd_response:send_status(ModData, 100, ""),
	    ok;
	continue when MaxBodySize < Length ->
	    httpd_response:send_status(ModData, 417, "Body too big"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Expect denied according to size"};
	break ->
	    httpd_response:send_status(ModData, 417, "Method not allowed"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Expect conditions was not fullfilled"};
	no_expect_header ->
	    ok;
	http_1_0_expect_header ->
	    httpd_response:send_status(ModData, 400, 
				       "Only HTTP/1.1 Clients "
				       "may use the Expect Header"),
	    httpd_socket:close(SocketType, Socket),
	    {socket_closed,"Due to a HTTP/1.0 expect header"}
    end.

%% The request is read in send it forward to the module that 
%% generates the response
respond(EntityBody, ExtraHeader, 
	#mod{parsed_header = ParsedHeader, init_data = InitData,
	     socket = Socket, socket_type = SocketType,
	     config_db = ConfigDB} = ModData) ->
    ?DEBUG("respond -> ~n"
	      "    EntityBody:   ~p~n"
	      "    ExtraHeader:  ~p~n"
	      "    ParsedHeader: ~p~n",
	      [EntityBody, ExtraHeader, ParsedHeader]),
    ok = httpd_response:generate_and_send_response(ModData#mod{parsed_header = 
							       ParsedHeader ++ 
							       ExtraHeader,
							       entity_body = 
							       EntityBody}),
    #mod{socket_type = SocketType, socket = Socket, 
	 config_db = ConfigDB, init_data = InitData}.


expect(Headers, "HTTP/1.1", _) ->
    case Headers#http_request_h.expect of
	"100-continue" ->
	    continue; 
	undefined ->
	    no_expect_header;
	_ ->
	    break
    end;

expect(Headers, _, ConfigDB) ->
    case Headers#http_request_h.expect of
	undefined ->
	    no_expect_header;
	_ ->
	    case httpd_util:lookup(ConfigDB, expect, continue) of
		continue->
		    no_expect_header;
		_ ->
		    http_1_0_expect_header
	    end
    end.

handle_request_receive_error({header_too_long, Max, Rem}, ModData) ->
    String = io_lib:format("header too long: ~p : ~p",[Max, Rem]),
    handle_request_receive_error(413, String, ModData);
handle_request_receive_error({body_too_long, Max, Actual}, ModData) ->
    String = io_lib:format("body too long: ~p : ~p", [Max, Actual]),
    handle_request_receive_error(413, String, ModData);
handle_request_receive_error(session_local_timeout, ModData) ->
    handle_request_receive_error(408, "Request timeout", ModData);
handle_request_receive_error(_, _) ->
    ok.

handle_request_receive_error(ReasonCode, ReasonString, ModData) ->
    ?vlog("error reading request: ~s",[ReasonString]),
    error_log(ReasonString, ModData),
    httpd_response:send_status(ModData, ReasonCode, ReasonString).

error_log(ReasonString, #mod{socket = Socket, socket_type = SocketType,
			     config_db = ConfigDB, 
			     init_data = #init_data{peername = Peername}}) ->
    Error = lists:flatten(
	      io_lib:format("Error reading request: ~s",[ReasonString])),
    error_log(mod_log, SocketType, Socket, ConfigDB, Peername, Error),
    error_log(mod_disk_log, SocketType, Socket, ConfigDB, Peername, Error).

error_log(Mod, SocketType, Socket, ConfigDB, Peername, String) ->
    Modules = httpd_util:lookup(ConfigDB, modules,
				[mod_get, mod_head, mod_log]),
    case lists:member(Mod, Modules) of
	true ->
	    Mod:error_log(SocketType, Socket, ConfigDB, Peername, String);
	_ ->
	    ok
    end.

%% This ugly thing is to make ssl deliver the message, before the
%% close...
close_sleep({ssl, _}, Time) ->
    sleep(Time);
close_sleep(_, _) ->
    ok.

sleep(T) -> receive after T -> ok end.

dec(N) when integer(N)->
    N-1;
dec(N) ->
    N.
