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
-module(httpd_listener).

-include("httpd.hrl").
-include("httpd_verbosity.hrl").


%% External API
-export([start_link/3, start_link/4]).

%% Other exports (for spawn's etc.)
-export([connection/5,connection/6]).


%% ----
%% This (and the report_error/4 function) is just temporary
%% and should be removed eventually.

%%-define(httpd_verbose,true).
-ifdef(httpd_verbose).
-define(REPORT_ERROR(Db,FS,A),report_error(Db,FS,A,?LINE)).
-else.
-define(REPORT_ERROR(Db,FS,A),ok).
-endif.


-define(USE_ASSERT,true).      %% false | true
-define(ASSERT_ACTION,report). %% no_action | print | report | exit
-define(ASSERT(When,Expected,Actual,Cdb),
	assert(?USE_ASSERT,?LINE,When,Expected,Actual,Cdb)).


%%
%% External API
%%

%% start_link

start_link(SocketType,ListenSocket,ConfigDB) ->
    start_link(idle,SocketType,ListenSocket,ConfigDB).

start_link(UsageState,SocketType,ListenSocket,ConfigDB) ->
    ?DEBUG("start_link(~p) -> ~n"
	   "      SocketType:   ~p~n"
	   "      ListenSocket: ~p~n",
	   [UsageState,SocketType,ListenSocket]),
    proc_lib:spawn_link(?MODULE,connection,
			[UsageState,self(),SocketType,ListenSocket,ConfigDB,
			 get_verbosity()]).


connection(Us,Manager,SocketType,ListenSocket,ConfigDB,Verbosity) ->
    put(sname,self()),
    put(verbosity,Verbosity),
    connection(Us,Manager,SocketType,ListenSocket,ConfigDB).


%% connection (busy)

%% The 30000 ms timeout in connection/5 serves the purpose of
%% making old code not stick around indefinitely when upgrading.
%%
%% This process was created when max_client connections had been 
%% accepted (was in busy usage state). The assumption is that the 
%% next connection will follow shortly. But that is not necessarily 
%% the case. Theoretically all the connections active at the time 
%% of the creation could very well all be handled and done with. 
%% When the next connection is received the only active process is 
%% this "busy" connection process. I.e. there is no heavy load 
%% situation at all. To handle this situation, the first thing done 
%% after a successful accept is to check if the server is actually 
%% still busy (still at max_client), which is done in the handle_busy 
%% function.

connection(busy,Manager,SocketType,ListenSocket,ConfigDB) ->
    ?LOG("connection(busy) -> ListenSocket: ~p~n",[ListenSocket]),
    ?vlog("starting when busy",[]),
    case (catch httpd_socket:accept(SocketType,ListenSocket,30000)) of
	{error, timeout} ->
	    ?DEBUG("connection(busy) -> error:  timeout",[]),
	    ?vdebug("Accept timeout",[]),
	    ?REPORT_ERROR(ConfigDB," accept timeout",[]),
	    ?MODULE:connection(busy,Manager,SocketType,ListenSocket,ConfigDB);
	{error, {enfile, _}} ->
	    ?LOG("connection(busy) -> error: enfile",[]),
	    ?vinfo("Accept error: enfile",[]),
	    ?REPORT_ERROR(ConfigDB,"connection accept error: enfile",[]),
	    %% Out of sockets...
	    receive after 200 -> ok end,
	    ?MODULE:connection(busy,Manager,SocketType,ListenSocket,ConfigDB);
	{error, emfile} ->
	    ?LOG("connection(busy) -> error: emfile",[]),
	    ?vinfo("Accept error: emfile",[]),
	    ?REPORT_ERROR(ConfigDB,"connection accept error: emfile",[]),
	    %% Too many open files -> Out of sockets...
	    receive after 200 -> ok end,
	    ?MODULE:connection(busy,Manager,SocketType,ListenSocket,ConfigDB);
	{error, closed} ->
	    ?LOG("connection(busy) -> error: closed",[]),
	    ?vlog("Accept error: closed",[]),
	    %% This propably only means that our manager is stopping
	    ?REPORT_ERROR(ConfigDB,"connection accept error: closed",[]),
	    exit(normal);
	{error, Reason} ->
	    ?LOG("connection(busy) -> error: ~p",[Reason]),
	    ?vinfo("Accept error:~n   ~p",[Reason]),
	    accept_failed(SocketType, ConfigDB, Reason);
	{'EXIT', Reason} ->
	    ?LOG("connection(busy) -> exit: ~p",[Reason]),
	    ?vinfo("Accept exit:~n   ~p",[Reason]),
	    accept_failed(SocketType, ConfigDB, Reason);
	Socket ->
	    ?DEBUG("connection(busy) -> accepted: ~p",[Socket]),
	    ?vlog("accepted",[]),
	    handle_busy(Manager,SocketType,Socket,ConfigDB)
    end;


%% connection (non-busy, i.e. idle or active)

connection(Us,Manager,SocketType,ListenSocket,ConfigDB) ->
    ?LOG("connection(~p) -> ListenSocket: ~p",[Us,ListenSocket]),
    ?vlog("starting when ~p",[Us]),
    case (catch httpd_socket:accept(SocketType,ListenSocket, 30000)) of
	{error, timeout} ->
	    ?DEBUG("connection(~p) -> error: timeout",[Us]),
	    ?vdebug("Accept timeout",[]),
	    ?REPORT_ERROR(ConfigDB,"connection accept timeout",[]),
	    ?MODULE:connection(Us,Manager,SocketType,ListenSocket,ConfigDB);
	{error, {enfile, _}} ->
	    ?LOG("connection(~p) -> error: enfile",[Us]),
	    ?vinfo("Accept error: enfile",[]),
	    ?REPORT_ERROR(ConfigDB,"connection accept error: enfile",[]),
	    %% Out of sockets...
	    receive after 200 -> ok end,
	    ?MODULE:connection(Us,Manager,SocketType,ListenSocket,ConfigDB);
	{error, emfile} ->
	    ?LOG("connection(~p) -> error: emfile",[Us]),
	    ?vinfo("Accept error: emfile",[]),
	    ?REPORT_ERROR(ConfigDB,"connection accept error: emfile",[]),
	    %% Too many open files -> Out of sockets...
	    receive after 200 -> ok end,
	    ?MODULE:connection(Us,Manager,SocketType,ListenSocket,ConfigDB);
	{error, closed} ->
	    ?LOG("connection(~p) -> error: closed",[Us]),
	    ?vlog("Accept error: closed",[]),
	    %% This propably only means that our manager is stopping
	    ?REPORT_ERROR(ConfigDB,"connection accept error: closed",[]),
	    exit(normal);
	{error, Reason} ->
	    ?LOG("connection(~p) -> error: ~p",[Us,Reason]),
	    ?vinfo("Accept error:~n   ~p",[Reason]),
	    accept_failed(SocketType, ConfigDB, Reason);
	{'EXIT', Reason} ->
	    ?LOG("connection(~p) -> exit: ~p",[Us,Reason]),
	    ?vinfo("Accept exit:~n   ~p",[Reason]),
	    accept_failed(SocketType, ConfigDB, Reason);
	Socket ->
	    ?DEBUG("connection(~p) -> accepted: ~p",[Us,Socket]),
	    ?vlog("accepted(~p)",[Us]),
	    handle_connection(Manager, SocketType, Socket, ConfigDB)
    end.


handle_busy(Manager,SocketType,Socket,ConfigDB) ->
    %% This process was created when we hade reached max connections
    %% (busy), but are we still? Check if it really is heavy load (busy)
    case httpd_manager:is_busy(Manager) of
	true -> %% busy state
	    ?LOG("handle_busy -> still busy, so reject",[]),
	    ?vlog("still busy usage state => reject",[]),
	    reject_connection(Manager, SocketType, Socket, ConfigDB);

	false -> %% not busy state
	    ?LOG("handle_busy -> no longer busy, so handle",[]),
	    ?vlog("no longer busy usage state => handle",[]),
	    handle_connection(Manager, SocketType, Socket, ConfigDB)
    end.


reject_connection(Manager,SocketType,Socket,ConfigDB) ->
    httpd_manager:new_connection(Manager,reject),
    MaxClients = httpd_util:lookup(ConfigDB,max_clients,150),
    String = io_lib:format("heavy load (>~w processes)",[MaxClients]),
    httpd_response:send_status(SocketType,Socket,503,String,ConfigDB),
    httpd_manager:done_connection(Manager,reject),
    close(SocketType,Socket,ConfigDB).


handle_connection(Manager, SocketType, Socket, ConfigDB) ->
    Resolve  = httpd_socket:resolve(SocketType),
    Peername = httpd_socket:peername(SocketType, Socket),
    InitData = #init_data{peername=Peername, resolve=Resolve},
    httpd_manager:new_connection(Manager,accept),
    MaxRequests = httpd_util:lookup(ConfigDB, keep_alive, 1),
    do_next_connection(InitData, SocketType, Socket, ConfigDB, 
		       MaxRequests, 60000), % XXX Was infinity
    httpd_manager:done_connection(Manager,accept),
    close(SocketType,Socket,ConfigDB).


do_next_connection(_InitData, _SocketType, _Socket, _ConfigDB, 0, _Timeout) ->
    ok;
do_next_connection(InitData,SocketType,Socket,ConfigDB,MaxRequests,Timeout) ->
    Peername = InitData#init_data.peername,
    case catch httpd_request:read(SocketType,
				  Socket,
				  ConfigDB,
				  InitData, 
				  Timeout) of
	{'EXIT',Reason} ->
	    ?LOG("do_next_connection -> exit: ~p",[Reason]),
	    ?vlog("exit reading request: ~p",[Reason]),
	    error_logger:error_report({'EXIT',Reason}),
	    mod_log:error_log(SocketType,Socket,ConfigDB,Peername,Reason),
	    mod_disk_log:error_log(SocketType,Socket,ConfigDB,Peername,Reason);
	{error,Reason} ->
	    handle_read_error(Reason,SocketType,Socket,ConfigDB,Peername);
	Info when record(Info, mod) ->
	    case Info#mod.connection of
		keep_alive ->
		    RequestTimeout = httpd_util:lookup(ConfigDB, 
						      keep_alive_timeout, 
						      15000),
		    do_next_connection(InitData,
				       SocketType,
				       Socket, 
				       ConfigDB, 
				       MaxRequests-1, 
				       RequestTimeout);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

handle_read_error({header_too_long,Max,Rem},
		  SocketType,Socket,ConfigDB,Peername) ->
    ?LOG("handle_read_error(header_too_long) -> entry with"
	 "~n   Max: ~p"
	 "~n   Rem: ~p",[Max,Rem]),
    String = io_lib:format("header too long: ~p + ~p",[Max,Rem]),
    handle_read_error(ConfigDB,String,SocketType,Socket,Peername,
		      max_header_action,close);
handle_read_error({body_too_long,Max,Actual},
		  SocketType,Socket,ConfigDB,Peername) ->
    ?LOG("handle_read_error(body_too_long) -> entry with"
	 "~n   Max:    ~p"
	 "~n   Actual: ~p",[Max,Actual]),
    String = io_lib:format("body too long: ~p : ~p",[Max,Actual]),
    handle_read_error(ConfigDB,String,SocketType,Socket,Peername,
		      max_body_action,close);
handle_read_error(Error,SocketType,Socket,ConfigDB,Peername) ->
    ok.

handle_read_error(ConfigDB,ReasonString,SocketType,Socket,Peername,
		  Item,Default) ->
    ?vlog("error reading request: ~s",[ReasonString]),
    E = io_lib:format("Error reading request: ~s",[ReasonString]),
    mod_log:error_log(SocketType,Socket,ConfigDB,Peername,E),
    mod_disk_log:error_log(SocketType,Socket,ConfigDB,Peername,E),
    case httpd_util:lookup(ConfigDB,Item,Default) of
	reply414 ->
	    send_read_status(SocketType,Socket,414,ReasonString,ConfigDB);
	_ ->
	    ok
    end.
    
send_read_status(SocketType,Socket,Code,ReasonString,ConfigDB) ->
    httpd_response:send_status(SocketType,Socket,Code,ReasonString,ConfigDB).

    
accept_failed(SocketType, ConfigDB, Error) ->
    String = lists:flatten(io_lib:format("Accept failed: ~p", [Error])),
    error_logger:error_report(String),
    mod_log:error_log(SocketType, undefined, ConfigDB, {0, "unknown"}, String),
    mod_disk_log:error_log(SocketType, undefined, ConfigDB, {0, "unknown"}, String),
    exit({accept_failed, String}).


-ifdef(httpd_verbose).
report_error(ConfigDB,FStr,Args,Line) ->
    String = lists:flatten(io_lib:format("Error at line ~w: " ++ FStr, 
					 [Line|Args])),
    error_logger:error_report(String),
    mod_log:report_error(ConfigDB,String),
    mod_disk_log:report_error(ConfigDB,String).
-endif.


%% Socket utility functions:

close(SocketType,Socket,ConfigDB) ->
    case httpd_socket:close(SocketType,Socket) of
	ok ->
	    ok;
	{error,closed} ->
	    ?REPORT_ERROR(ConfigDB,"Socket ~p already closed",[Socket]);
	{error,Reason} ->
	    ?REPORT_ERROR(ConfigDB,"Error while closing socket: ~p",[Reason])
    end.

-ifdef(httpd_verbose).
assert(false,_Line,_Res,_Res,_Cdb) ->
    ok;
assert(true,_Line,Res,Res,_Cdb) ->
    ok;
assert(true,Line,Expected,Actual,Cdb) ->
    assert_action(?ASSERT_ACTION,Line,Expected,Actual,Cdb).

assert_action(no_action,_Line,_Expected,_Actual,_Cdb) ->
    ok;
assert_action(print,Line,Expected,Actual,_Cdb) ->
    io:format("(~p:~p:~p) Assert failed: ~n\tExpected: ~p~n\tActual:   ~p",
	      [self(),?MODULE,Line,Expected,Actual]);
assert_action(report,Line,Expected,Actual,Cdb) ->
    report_error(Cdb,"Assert failed: ~n\tExpected: ~p~n\tActual:   ~p",
		 [self(),Expected,Actual],Line);
assert_action(exit,Line,Expected,Actual,_Cdb) ->
    exit({assert_failed,{?MODULE,Line,Expected,Actual}});
assert_action(_Action,_Line,_Expected,_Actual,_Cdb) ->
    ok.
-endif.



get_verbosity() ->
    get_verbosity(get(listener_verbosity)).

get_verbosity(undefined) ->
    ?default_verbosity;
get_verbosity(V) ->
    ?vvalidate(V).
