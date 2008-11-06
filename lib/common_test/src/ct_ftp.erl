%%<copyright>
%% <year>2003-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

%%% @doc Common Test specific layer on top of OTP ftp clinet ftp.erl
%%%
%%% @type connection() = handle() | ct:target_name()
%%% @type handle() = ct_gen_conn:handle(). Handle for a specific
%%% ftp connection.

-module(ct_ftp).

%% API
-export([get/3,put/3, open/1,close/1, send/2,send/3, 
	 recv/2,recv/3, cd/2, ls/2, type/2, delete/2]).

%% Callbacks
-export([init/3,handle_msg/2,reconnect/2,terminate/2]).

-include("ct_util.hrl").

-record(state,{ftp_pid,target_name}).

-define(DEFAULT_PORT,21).

%%%=================================================================
%%% API

%%%-----------------------------------------------------------------
%%% @spec put(Name,LocalFile,RemoteFile) -> ok | {error,Reason}
%%%      Name = target_name()
%%%      LocaFile = string()
%%%      RemoteFile = string()
%%%
%%% @doc Open a ftp connection and send a file to the remote host.
%%%
%%% <p><code>LocalFile</code> and <code>RemoteFile</code> must be
%%% absolute paths.</p>
%%%
%%% <p>If the target host is a "special" node, the ftp address must be
%%% specified in the config file like this:</p>
%%% <pre>
%%% {node,[{ftp,IpAddr}]}.</pre>
%%%
%%% <p>If the target host is something else, e.g. a unix host, the
%%% config file must also include the username and password (both
%%% strings):</p>
%%% <pre>
%%% {unix,[{ftp,IpAddr},
%%%        {username,Username},
%%%        {password,Password}]}.</pre>
put(Name,LocalFile,RemoteFile) ->
    Fun = fun(Ftp) -> send(Ftp,LocalFile,RemoteFile) end,
    open_and_do(Name,Fun).

%%%-----------------------------------------------------------------
%%% @spec get(Name,RemoteFile,LocalFile) -> ok | {error,Reason}
%%%      Name = target_name()
%%%      RemoteFile = string()
%%%      LocaFile = string()
%%%
%%% @doc Open a ftp connection and fetch a file from the remote host.
%%%
%%% <p><code>RemoteFile</code> and <code>LocalFile</code> must be
%%% absolute paths.</p>
%%%
%%% <p>The config file must be as for put/3.</p>
%%% @see put/3
get(Name,RemoteFile,LocalFile) ->
    Fun = fun(Ftp) -> recv(Ftp,RemoteFile,LocalFile) end,
    open_and_do(Name,Fun).


%%%-----------------------------------------------------------------
%%% @spec open(Name) -> {ok,Handle} | {error,Reason}
%%%      Name = ct:target_name()
%%%      Handle = handle()
%%% 
%%% @doc Open an FTP connection to the specified node.
open(Name) ->
    case ct_util:get_key_from_name(Name) of
	{ok,node} ->
	    open(Name,"erlang","x");
	{ok,_Key} -> % any other, e.g. unix
	    case ct:get_config({Name,username}) of
		undefined ->
		    log(heading(open,Name),"Failed: ~p\n",
			[{not_available,{Name,username}}]),
		    {error,{not_available,{Name,username}}};
		Username ->
		    case ct:get_config({Name,password}) of
			undefined ->
			    log(heading(open,Name),"Failed: ~p\n",
				[{not_available,{Name,password}}]),
			    {error,{not_available,{Name,password}}};
			Password ->
			    open(Name,Username,Password)
		    end
	    end;
	Error ->
	    Error
    end.

open(Name,Username,Password) ->
    log(heading(open,Name),"",[]),
    case ct:get_config({Name,ftp}) of
	undefined ->
	    log(heading(open,Name),"Failed: ~p\n",
		[{not_available,{Name,ftp}}]),
	    {error,{not_available,{Name,ftp}}};
	Addr ->
	    ct_gen_conn:start(Name,full_addr(Addr),{Username,Password},?MODULE)
    end.


%%%-----------------------------------------------------------------
%%% @spec send(Connection,LocalFile) -> ok | {error,Reason}
%%%
%%% @doc Send a file over FTP.
%%% <p>The file will get the same name on the remote host.</p>
%%% @see send/3
send(Connection,LocalFile) ->
    send(Connection,LocalFile,filename:basename(LocalFile)).

%%%-----------------------------------------------------------------
%%% @spec send(Connection,LocalFile,RemoteFile) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      LocaFile = string()
%%%      RemoteFile = string()
%%%
%%% @doc Send a file over FTP.
%%%
%%% <p>The file will be named <code>RemoteFile</code> on the remote host.</p>
send(Connection,LocalFile,RemoteFile) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{send,LocalFile,RemoteFile});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec recv(Connection,RemoteFile) -> ok | {error,Reason}
%%%
%%% @doc Fetch a file over FTP.
%%% <p>The file will get the same name on the local host.</p>
%%% @see recv/3
recv(Connection,RemoteFile) ->
    recv(Connection,RemoteFile,filename:basename(RemoteFile)).

%%%-----------------------------------------------------------------
%%% @spec recv(Connection,RemoteFile,LocalFile) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      RemoteFile = string()
%%%      LocaFile = string()
%%%
%%% @doc Fetch a file over FTP.
%%%
%%% <p>The file will be named <code>LocalFile</code> on the local host.</p>
recv(Connection,RemoteFile,LocalFile) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{recv,RemoteFile,LocalFile});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec cd(Connection,Dir) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      Dir = string()
%%%
%%% @doc Change directory on remote host.
cd(Connection,Dir) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{cd,Dir});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec ls(Connection,Dir) -> {ok,Listing} | {error,Reason}
%%%      Connection = connection()
%%%      Dir = string()
%%%      Listing = string()
%%%
%%% @doc List the directory Dir.
ls(Connection,Dir) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{ls,Dir});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec type(Connection,Type) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      Type = ascii | binary
%%%
%%% @doc Change file transfer type
type(Connection,Type) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{type,Type});
	Error ->
	    Error
    end.
    
%%%-----------------------------------------------------------------
%%% @spec delete(Connection,File) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      File = string()
%%%
%%% @doc Delete a file on remote host
delete(Connection,File) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{delete,File});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec close(Connection) -> ok | {error,Reason}
%%%      Connection = connection()
%%%
%%% @doc Close the FTP connection.
close(Connection) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    ct_gen_conn:stop(Pid);
	Error ->
	    Error
    end.


%%%=================================================================
%%% Callback functions

%% @hidden
init(Name,{IP,Port},{Username,Password}) ->
    case ftp_connect(IP,Port,Username,Password) of
	{ok,FtpPid} ->
	    log(heading(init,Name), 
		"Opened ftp connection:\nIP: ~p\nUsername: ~p\nPassword: ~p\n",
		[IP,Username,lists:duplicate(length(Password),$*)]),
	    {ok,FtpPid,#state{ftp_pid=FtpPid,target_name=Name}};
	Error ->
	    Error
    end.
	    

ftp_connect(IP,Port,Username,Password) ->
    case ftp:open(IP,Port) of
	{ok,FtpPid} ->
	    case ftp:user(FtpPid,Username,Password) of
		ok ->
		    {ok,FtpPid};
		{error,Reason} ->
		    {error,{user,Reason}}
	    end;
	{error,Reason} ->
	    {error,{open,Reason}}
    end.

%% @hidden
handle_msg({send,LocalFile,RemoteFile},State) ->
    log(heading(send,State#state.target_name),
	"LocalFile: ~p\nRemoteFile: ~p\n",[LocalFile,RemoteFile]),
    Result = ftp:send(State#state.ftp_pid,LocalFile,RemoteFile),
    {Result,State};
handle_msg({recv,RemoteFile,LocalFile},State) ->
    log(heading(recv,State#state.target_name),
	"RemoteFile: ~p\nLocalFile: ~p\n",[RemoteFile,LocalFile]),
    Result = ftp:recv(State#state.ftp_pid,RemoteFile,LocalFile),
    {Result,State};
handle_msg({cd,Dir},State) ->
    log(heading(cd,State#state.target_name),"Dir: ~p\n",[Dir]),
    Result = ftp:cd(State#state.ftp_pid,Dir),
    {Result,State};
handle_msg({ls,Dir},State) ->
    log(heading(ls,State#state.target_name),"Dir: ~p\n",[Dir]),
    Result = ftp:ls(State#state.ftp_pid,Dir),
    {Result,State};
handle_msg({type,Type},State) ->
    log(heading(type,State#state.target_name),"Type: ~p\n",[Type]),
    Result = ftp:type(State#state.ftp_pid,Type),
    {Result,State};
handle_msg({delete,File},State) ->
    log(heading(delete,State#state.target_name),"Delete file: ~p\n",[File]),
    Result = ftp:delete(State#state.ftp_pid,File),
    {Result,State}.

%% @hidden
reconnect(_Addr,_State) ->
    {error,no_reconnection_of_ftp}.

%% @hidden
terminate(FtpPid,State) ->
    log(heading(terminate,State#state.target_name),
	"Closing FTP connection.\nHandle: ~p\n",[FtpPid]),
    ftp:close(FtpPid).


%%%=================================================================
%%% Internal function
get_handle(Pid) when pid(Pid) ->
    {ok,Pid};
get_handle(Name) ->
    case ct_util:get_connections(Name,?MODULE) of
	{ok,[{Pid,_}]} ->
	    {ok,Pid};
	{ok,[]} ->
	    open(Name);
	Error ->
	    Error
    end.

full_addr({Ip,Port}) ->
    {Ip,Port};
full_addr(Ip) ->
    {Ip,?DEFAULT_PORT}.

call(Pid,Msg) ->
    ct_gen_conn:call(Pid,Msg).


heading(Function,Name) ->
    io_lib:format("ct_ftp:~w ~p",[Function,Name]).

log(Heading,Str,Args) ->
    ct_gen_conn:log(Heading,Str,Args).


open_and_do(Name,Fun) ->
    case open(Name) of
	{ok,Ftp} ->
	    R = Fun(Ftp),
	    close(Ftp),
	    R;
	Error ->
	    Error
    end.
    
    
