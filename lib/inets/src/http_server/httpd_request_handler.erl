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
-export([start_link/2, synchronize/3]).

%% module internal api
-export([connection/2, do_next_connection/6]). 

-include("httpd.hrl").
-include("httpd_verbosity.hrl").
-include("http.hrl").

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid}
%%
%% Description: Starts a httpd-request handler process. Intended to be
%% called by the httpd acceptor process.
%% %%--------------------------------------------------------------------
start_link(Manager, ConfigDB) ->
    Pid = proc_lib:spawn(?MODULE, connection, [Manager, ConfigDB]),
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
connection(Manager, ConfigDB) ->
    {SocketType, Socket, {Status, Verbosity}} = await_synchronize(Manager),
    put(sname,self()),
    put(verbosity,?vvalidate(Verbosity)),
    connection1(Status, Manager, ConfigDB, SocketType, Socket).


connection1({reject, busy}, Manager, ConfigDB, SocketType, Socket) ->
    handle_busy(Manager, ConfigDB, SocketType, Socket);

connection1({reject, blocked}, Manager, ConfigDB, SocketType, Socket) ->
    handle_blocked(Manager, ConfigDB, SocketType, Socket);

connection1(accept, Manager, ConfigDB, SocketType, Socket) ->
    handle_connection(Manager, ConfigDB, SocketType, Socket).

await_synchronize(Manager) ->
    receive
	{synchronize, SocketType, Socket} ->
	    ?vlog("received syncronize: "
		  "~n   SocketType: ~p"
		  "~n   Socket:     ~p", [SocketType, Socket]),
	    {SocketType, Socket, httpd_manager:new_connection(Manager)}
    after 5000 ->
	    exit(synchronize_timeout)
    end.

handle_busy(Manager, ConfigDB, SocketType, Socket) ->
    ?vlog("handle busy: ~p", [Socket]),
    MaxClients = httpd_util:lookup(ConfigDB, max_clients, 150),
    String = io_lib:format("heavy load (>~w processes)", [MaxClients]),
    reject_connection(Manager, ConfigDB, SocketType, Socket, String).

handle_blocked(Manager, ConfigDB, SocketType, Socket) ->
    ?vlog("handle blocked: ~p", [Socket]),
    String = "Server maintenance performed, try again later",
    reject_connection(Manager, ConfigDB, SocketType, Socket, String).

reject_connection(_Manager, ConfigDB, SocketType, Socket, Info) ->
    String = lists:flatten(Info),
    ?vtrace("send status (503) message", []),
    httpd_response:send_status(SocketType, Socket, 503, String, ConfigDB),
    %% This ugly thing is to make ssl deliver the message, before the
    %% close...
    close_sleep(SocketType, 1000),  
    ?vtrace("close the socket", []),
    httpd_socket:close(SocketType, Socket).

handle_connection(Manager, ConfigDB, SocketType, Socket) ->
    ?vlog("handle connection: ~p", [Socket]),
    Resolve     = http_transport:resolve(),
    Peername    = httpd_socket:peername(SocketType, Socket),
    InitData    = #init_data{peername=Peername, resolve=Resolve},
    TimeOut     = httpd_util:lookup(ConfigDB, keep_alive_timeout, 150000),
    NrOfRequest = httpd_util:lookup(ConfigDB, 
				    max_keep_alive_request, forever),
    ?MODULE:do_next_connection(ConfigDB, InitData, 
			       SocketType, Socket,NrOfRequest,TimeOut),
    ?vlog("handle connection: done", []),
    httpd_manager:done_connection(Manager),
    ?vlog("handle connection: close socket", []),
    httpd_socket:close(SocketType, Socket).

do_next_connection(_ConfigDB, _InitData, _SocketType, _Socket, NrOfRequests, 
		   _Timeout) when NrOfRequests < 1 -> 
    ?vtrace("do_next_connection: done", []),
    ok;
do_next_connection(ConfigDB, InitData, SocketType, Socket, NrOfRequests, 
		   Timeout) ->
    Peername = InitData#init_data.peername,
    case (catch receive_request(ConfigDB, SocketType, Socket, 
				InitData, Timeout)) of
        {'EXIT', Reason} ->
            ?vlog("exit reading from socket: ~p",[Reason]),
            error_logger:error_report({'EXIT',Reason}),
	    String = 
		lists:flatten(
		  io_lib:format("exit reading from socket: ~p => ~n~p~n",
				[Socket, Reason])),
	    error_log(mod_log, 
		      SocketType, Socket, ConfigDB, Peername, String),
	    error_log(mod_disk_log, 
		      SocketType, Socket, ConfigDB, Peername, String);
        {error, Reason} ->
            handle_request_receive_error(Reason,SocketType,Socket,
					 ConfigDB,Peername);
        ModData when record(ModData, mod) ->
            case ModData#mod.connection of
                true ->
                    ReqTimeout = 
			httpd_util:lookup(ConfigDB, 
					  keep_alive_timeout, 150000),
		    ?MODULE:do_next_connection(ConfigDB, InitData,
					       SocketType, Socket,
					       dec(NrOfRequests),
					       ReqTimeout);
		_ ->
                    ok
            end;
        _ ->
            ok
    end.

receive_request(ConfigDB, SocketType, Socket, InitData, Timeout) ->
    MaxHeaderSize =
	httpd_util:lookup(ConfigDB, max_header_size, ?HTTP_MAX_HEADER_SIZE),
    {Method, Uri, Version, {RecordHeaders, Headers}, Body} =
	receive_http_msg({httpd_request, parse, [MaxHeaderSize]},
	  {SocketType, Socket, Timeout}),
    
    case httpd_request:validate(Method, Uri, Version) of
	ok  ->
	    {ok, ModData} = httpd_request:mod_data(Socket, SocketType, 
						ConfigDB,
						Method, Uri,
						Version,
						Method ++ " " ++
						Uri ++ " " ++
						Version, 
						Headers,
					       [], InitData),
	    
	    case is_host_specified_if_required(ModData#mod.absolute_uri,
					       RecordHeaders, Version) of
		true ->
		    case handle_body(RecordHeaders, MaxHeaderSize, ConfigDB,
				     Socket, SocketType, Body, ModData,
				     Timeout) of
			{ok, NewBody} ->
			    finish_request(NewBody, [], ModData);
			Other ->
			    Other
		    end;
		false ->
		    httpd_response:send_status(ModData, 400, none),
		    {error,"No host specified"}
	    end;
	{error, {not_supported, What}} ->
	    error_logger:error_report("httpd_request_handler: "  
 				      "Not supported ~p~n", [What]),
	    httpd_response:send_status(SocketType, Socket, 501,
				       {Method, Uri, Version},
				       ConfigDB),
	    {error, "Not supported"};
	{error, {bad_request, {forbidden, URI}}} ->
	    httpd_response:send_status(SocketType, Socket, 
				       403, URI, ConfigDB),
	    {error,"Forbidden Request"}
    
    end.

receive_http_msg({Module, Function, Args},
		 SocketInfo = {SocketType, Socket, Timeout}) ->
    http_transport:setopts(SocketType, Socket, [binary,{packet, 0},
						{active, once}]),
    receive
	{Proto, Socket, Data} when Proto == tcp; Proto == ssl ->
	    case Module:Function([Data | Args]) of
		{ok, Result} ->
		    Result;
		NewMFA ->
		    receive_http_msg(NewMFA, SocketInfo)
	    end;	
	{tcp_closed, Socket} ->
	    throw({error, session_remotely_closed});
	{tcp_error, Socket, Reason} ->
	    throw({error, Reason});
	{ssl_closed, Socket} ->
	    throw({error, session_remotely_closed});
	{ssl_error, Socket, Reason} ->
	    throw({error, Reason})
    after Timeout ->
	    throw({error, session_local_timeout})
    end.

handle_body(RecordHeaders, MaxHdrSz, ConfigDB, Socket, SocketType, Body, 
	    ModData, Timeout) ->
    
    MaxBodySize = httpd_util:lookup(ConfigDB, max_body_size, nolimit),
    
    case handle_expect(RecordHeaders, ConfigDB, Socket, SocketType,
		       ModData, MaxBodySize) of 
	ok ->
	    case (catch handle_body(RecordHeaders, MaxHdrSz, 
				    Body, MaxBodySize, 
				    {SocketType, Socket, Timeout})) of
		
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
	
handle_body(Headers, MaxHeaderSize, Body, MaxBodySize, SocketInfo)->
    case Headers#http_request_h.'transfer-encoding' of
	"chunked" ->
	    {ChunkedHeaders, NewBody} =
		case http_chunk:decode(Body, MaxBodySize, MaxHeaderSize) of
		    {Module, Function, Args} ->
			receive_http_msg({Module, Function, Args},
					 SocketInfo);
		    {ok, Result} ->
			Result
		end,
	    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
	    {NewHeaders, binary_to_list(NewBody)}; 
	Encoding when list(Encoding) ->
	    throw({error, unknown_coding});
	_ -> 
	    Length = 
		list_to_integer(Headers#http_request_h.'content-length'),
	    case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
		true ->
		    case httpd_request:whole_body(Body, Length) of 
			{Module, Function, Args} ->
			    NewBody = 
				receive_http_msg({Module, Function, Args}, 
						 SocketInfo),
			    {Headers, binary_to_list(NewBody)};
			{ok, NewBody} ->
			    {Headers, binary_to_list(NewBody)}
		    end;
		false ->
		    throw({error, body_too_big}) 
	    end
    end.

is_host_specified_if_required(nohost, #http_request_h{host = undefined}, 
			      "HTTP/1.1") ->
    false;
is_host_specified_if_required(_, _, _) ->
    true.

handle_expect(Headers, ConfigDB, Socket, SocketType,ModData, MaxBodySize) ->
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

finish_request(EntityBody, ExtraHeader, 
	       #mod{parsed_header = ParsedHeader} = ModData) ->
    ?DEBUG("finish_request -> ~n"
	      "    EntityBody:   ~p~n"
	      "    ExtraHeader:  ~p~n"
	      "    ParsedHeader: ~p~n",
	      [EntityBody, ExtraHeader, ParsedHeader]),
    httpd_response:send(ModData#mod{parsed_header = 
				    ParsedHeader ++ ExtraHeader,
				    entity_body = EntityBody}).

%%----------------------------------------------------------------------
%% If the user sends an expect header-field with the value
%% 100-continue We must send a 100 status message if he is a HTTP/1.1
%% client.  If it is an HTTP/1.0 client it's little more difficult.
%% If expect is not defined it is easy but in the other case shall we
%% Break or end the transmission or let it continue the standard is
%% not clear if to break connection or wait for data.
%% ----------------------------------------------------------------------

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

handle_request_receive_error({header_too_long, Max, Rem},
		     SocketType, Socket, ConfigDB, Peername) ->
    String = io_lib:format("header too long: ~p : ~p",[Max, Rem]),
    handle_request_receive_error(ConfigDB, 413, String, SocketType, 
			 Socket, Peername);
handle_request_receive_error({body_too_long, Max, Actual},
                  SocketType, Socket, ConfigDB, Peername) ->
    String = io_lib:format("body too long: ~p : ~p", [Max, Actual]),
    handle_request_receive_error(ConfigDB, 413, String, SocketType, 
			 Socket, Peername);
handle_request_receive_error(session_local_timeout,
		     SocketType, Socket, ConfigDB, Peername) ->
    handle_request_receive_error(ConfigDB, 408, "Request timeout", 
			 SocketType, Socket, Peername);
handle_request_receive_error(_, _, _, _, _) ->
    ok.

handle_request_receive_error(ConfigDB, ReasonCode, 
		     ReasonString, SocketType, Socket, Peername) ->
    ?vlog("error reading request: ~s",[ReasonString]),
    E = lists:flatten(
          io_lib:format("Error reading request: ~s",[ReasonString])),
    error_log(mod_log, SocketType, Socket, ConfigDB, Peername, E),
    error_log(mod_disk_log, SocketType, Socket, ConfigDB, Peername, E),
    httpd_response:send_status(SocketType, Socket, ReasonCode, 
			       ReasonString, ConfigDB).
    
error_log(Mod, SocketType, Socket, ConfigDB, Peername, String) ->
    Modules = httpd_util:lookup(ConfigDB, modules,
				[mod_get, mod_head, mod_log]),
    case lists:member(Mod, Modules) of
	true ->
	    Mod:error_log(SocketType, Socket, ConfigDB, Peername, String);
	_ ->
	    ok
    end.

close_sleep({ssl, _}, Time) ->
    sleep(Time);
close_sleep(_, _) ->
    ok.

sleep(T) -> receive after T -> ok end.

dec(N) when integer(N)->
    N-1;
dec(N) ->
    N.
