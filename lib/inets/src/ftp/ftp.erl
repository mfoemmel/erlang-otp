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
%% Description: This module implements an ftp client, RFC 959. 
%% It also supports ipv6 RFC 2428.

-module(ftp).

-behaviour(gen_server).

%%  API - Client interface
-export([cd/2, close/1, delete/2, formaterror/1, 
	 lcd/2, lpwd/1, ls/1, ls/2, 
	 mkdir/2, nlist/1, nlist/2, 
	 open/1, open/2, open/3, force_active/1,
	 pwd/1, quote/2,
	 recv/2, recv/3, recv_bin/2, 
	 recv_chunk_start/2, recv_chunk/1, 
	 rename/3, rmdir/2, 
	 send/2, send/3, send_bin/3, 
	 send_chunk_start/2, send_chunk/2, send_chunk_end/1, 
	 type/2, user/3,user/4,account/2,
	 append/3, append/2, append_bin/3,
	 append_chunk/2, append_chunk_end/1, append_chunk_start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

%% supervisor callbacks
-export([start_link_sup/1]).

%% Constante used in internal state definition
-define(CONNECTION_TIMEOUT, 60*1000).
%% Internal state
-record(state, {
	  csock = undefined, % socket() - Control connection socket 
	  dsock = undefined, % socket() - Data connection socket 
	  verbose = false,   % boolean() 
	  ldir = undefined,  % string() - Current local directory
	  type = ftp_server_default,  % atom() - binary | ascci 
	  chunk = false,     % boolean() - Receiving data chunks 
	  mode = passive,    % passive | active
	  timeout = ?CONNECTION_TIMEOUT, % integer()
	  %% Data received so far on the data connection
	  data = <<>>,   % binary()
	  %% Data received so far on the control connection
	  %% {BinStream, AccLines}. If a binary sequence
	  %% ends with ?CR then keep it in the binary to
	  %% be able to detect if the next received byte is ?LF
	  %% and hence the end of the response is reached!
	  ctrl_data = {<<>>, [], start},  % {binary(), [bytes()], LineStatus}
	  client = undefined,  % pid() - Client pid
	  %% Function that activated a connection and maybe some
	  %% data needed further on.
	  caller = undefined, % term()     
	  ip_v6_disabled      % boolean()
	 }).

%% Internal Constants
-define(BYTE_TIMEOUT, 1000).   % Timeout for _ONE_ byte to arrive. (ms)
-define(FTP_PORT, 21).
-define(DOUBLE_QUOTE, 34).
-define(LEFT_PAREN, $().
-define(RIGHT_PAREN, $)).
%% First group of reply code digits
-define(POS_PREL, 1).
-define(POS_COMPL, 2).
-define(POS_INTERM, 3).
-define(TRANS_NEG_COMPL, 4).
-define(PERM_NEG_COMPL, 5).
%% Second group of reply code digits
-define(SYNTAX,0).
-define(INFORMATION,1).
-define(CONNECTION,2).
-define(AUTH_ACC,3).
-define(UNSPEC,4).
-define(FILE_SYSTEM,5).

-define(FILE_BUFSIZE, 4096).

-define(CR, 13).
-define(LF, 10).
-define(WHITE_SPACE, 32).
%%%=========================================================================
%%%  API - CLIENT FUNCTIONS
%%%=========================================================================
%%--------------------------------------------------------------------------
%% open(Host, <Port>, <Flags>) -> {ok, Pid} | {error, ehost}
%%	Host = string(), 
%%      Port = integer(), 
%%      Flags = [Flag], 
%%      Flag = verbose | debug
%%
%% Description:  Start an ftp client and connect to a host.
%%--------------------------------------------------------------------------
%%The only option was the host in textual form
open({option_list, Options})->
    ensure_started(),
    Flags = key_search(flags, Options, []),
    {ok, Pid} =  ftp_sup:start_child([[[Flags], Options]]),
    call(Pid, {open, ip_comm, Options}, pid);
	 
%%The only option was the tuple form of the ip-number
open(Host) when tuple(Host) ->
    open(Host, ?FTP_PORT, []);

%%Host is the string form of the hostname 
open(Host)->
    open(Host,?FTP_PORT,[]).

open(Host, Port) when integer(Port) ->
    open(Host,Port,[]);

open(Host, Flags) when list(Flags) ->
    open(Host,?FTP_PORT, Flags).

open(Host,Port,Flags) when integer(Port), list(Flags) ->
    ensure_started(),
    {ok, Pid} = ftp_sup:start_child([[[Flags], []]]), 
    call(Pid, {open, ip_comm, Host, Port}, pid).
%%--------------------------------------------------------------------------
%% user(Pid, User, Pass, <Acc>) -> ok | {error, euser} | {error, econn} 
%%                                    | {error, eacct}
%%	Pid = pid(), 
%%      User = Pass =  Acc = string()
%%
%% Description:  Login with or without a supplied account name.
%%--------------------------------------------------------------------------
user(Pid, User, Pass) ->
    call(Pid, {user, User, Pass}, atom).

user(Pid, User, Pass, Acc) ->
    call(Pid, {user, User, Pass, Acc}, atom).
%%--------------------------------------------------------------------------
%% account(Pid, Acc)  -> ok | {error, eacct}
%%	Pid = pid()
%%	Acc= string()
%%
%% Description:  Set a user Account.
%%--------------------------------------------------------------------------
account(Pid,Acc) ->
    call(Pid, {account, Acc}, atom).
%%--------------------------------------------------------------------------
%% pwd(Pid) -> {ok, Dir} | {error, elogin} | {error, econn} 
%%	Pid = pid()
%%      Dir = string()
%%
%% Description:  Get the current working directory at remote server.
%%--------------------------------------------------------------------------
pwd(Pid) ->
    call(Pid, pwd, ctrl).
%%--------------------------------------------------------------------------
%% lpwd(Pid) ->  {ok, Dir} | {error, elogin} 
%%	Pid = pid()
%%      Dir = string()
%%
%% Description:  Get the current working directory at local server.
%%--------------------------------------------------------------------------
lpwd(Pid) ->
    call(Pid, lpwd, local_string).
%%--------------------------------------------------------------------------
%% cd(Pid, Dir) ->  ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  Change current working directory at remote server.
%%--------------------------------------------------------------------------
cd(Pid, Dir) ->
    call(Pid, {cd, Dir}, atom).
%%--------------------------------------------------------------------------
%% lcd(Pid, Dir) ->  ok | {error, epath}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  Change current working directory for the local client.
%%--------------------------------------------------------------------------
lcd(Pid, Dir) ->
    call(Pid, {lcd, Dir}, string).
%%--------------------------------------------------------------------------
%% ls(Pid, <Dir>) -> {ok, Listing} | {error, epath} | {error, elogin} | 
%%                   {error, econn}
%%	Pid = pid()
%%	Dir = string()
%%      Listing = string()
%%
%% Description: List the contents of current directory (ls/1) or
%% directory Dir (ls/2) at remote server.
%%--------------------------------------------------------------------------
ls(Pid) ->
  ls(Pid, "").
ls(Pid, Dir) ->
    call(Pid, {dir, long, Dir}, string).
%%--------------------------------------------------------------------------
%% nlist(Pid, <Dir>) -> {ok, Listing} | {error, epath} | {error, elogin} | 
%%                      {error, econn}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  List the contents of current directory (ls/1) or directory
%%           Dir (ls/2) at remote server. The returned list is a stream
%%           of file names.
%%--------------------------------------------------------------------------
nlist(Pid) ->
  nlist(Pid, "").
nlist(Pid, Dir) ->
    call(Pid, {dir, short, Dir}, string).

%%--------------------------------------------------------------------------
%% rename(Pid, CurrFile, NewFile) ->  ok | {error, epath} | {error, elogin} 
%%                                    | {error, econn}
%%	Pid = pid()
%%	CurrFile = NewFile = string()
%%
%% Description:  Rename a file at remote server.
%%--------------------------------------------------------------------------
rename(Pid, CurrFile, NewFile) ->
    call(Pid, {rename, CurrFile, NewFile}, string).
%%--------------------------------------------------------------------------
%% delete(Pid, File) ->  ok | {error, epath} | {error, elogin} | 
%%                       {error, econn}
%%	Pid = pid()
%%	File = string()
%%
%% Description:  Remove file at remote server.
%%--------------------------------------------------------------------------
delete(Pid, File) ->
    call(Pid, {delete, File}, string).
%%--------------------------------------------------------------------------
%% mkdir(Pid, Dir) -> ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid(), 
%%	Dir = string()
%%
%% Description:  Make directory at remote server.
%%--------------------------------------------------------------------------
mkdir(Pid, Dir) ->
    call(Pid, {mkdir, Dir}, atom).
%%--------------------------------------------------------------------------
%% rmdir(Pid, Dir) -> ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid(), 
%%	Dir = string()
%%
%% Description:  Remove directory at remote server.
%%--------------------------------------------------------------------------
rmdir(Pid, Dir) ->
    call(Pid, {rmdir, Dir}, atom).
%%--------------------------------------------------------------------------
%% type(Pid, Type) -> ok | {error, etype} | {error, elogin} | {error, econn}
%%	Pid = pid() 
%%	Type = ascii | binary
%%
%% Description:  Set transfer type.
%%--------------------------------------------------------------------------
type(Pid, Type) ->
    call(Pid, {type, Type}, atom).
%%--------------------------------------------------------------------------
%% recv(Pid, RemoteFileName <LocalFileName>) -> ok | {error, epath} |
%%                                          {error, elogin} | {error, econn}
%%	Pid = pid()
%%	RemoteFileName = LocalFileName = string()
%%
%% Description:  Transfer file from remote server.
%%--------------------------------------------------------------------------
recv(Pid, RemotFileName) ->
  recv(Pid, RemotFileName, RemotFileName).

recv(Pid, RemotFileName, LocalFileName) ->
    call(Pid, {recv, RemotFileName, LocalFileName}, atom).
%%--------------------------------------------------------------------------
%% recv_bin(Pid, RFile) -> {ok, Bin} | {error, epath} | {error, elogin} 
%%			   | {error, econn}
%%	Pid = pid()
%%	RFile = string()
%%      Bin = binary()
%%
%% Purpose:  Transfer file from remote server into binary.
%%--------------------------------------------------------------------------
recv_bin(Pid, RFile) ->
    call(Pid, {recv_bin, RFile}, bin).

%%--------------------------------------------------------------------------
%% recv_chunk_start(Pid, RFile) -> ok | {error, elogin} | {error, epath} 
%%                                 | {error, econn}
%%	Pid = pid()
%%	RFile = string()
%%
%% Description:  Start receive of chunks of remote file.
%%--------------------------------------------------------------------------
recv_chunk_start(Pid, RFile) ->
    call(Pid, {recv_chunk_start, RFile}, atom).

%%--------------------------------------------------------------------------
%% recv_chunk(Pid, RFile) ->  ok | {ok, Bin} | {error, Reason}
%%	Pid = pid()
%%	RFile = string()
%%
%% Description:  Transfer file from remote server into binary in chunks
%%--------------------------------------------------------------------------
recv_chunk(Pid) ->
    call(Pid, recv_chunk, atom).
%%--------------------------------------------------------------------------
%% send(Pid, LocalFileName <RemotFileName>) -> ok | {error, epath} 
%%                                                | {error, elogin} 
%%                             | {error, econn}
%%	Pid = pid()
%%	LocalFileName = RemotFileName = string()
%%
%% Description:  Transfer file to remote server.
%%--------------------------------------------------------------------------
send(Pid, LocalFileName) ->
  send(Pid, LocalFileName, LocalFileName).

send(Pid, LocalFileName, RemotFileName) ->
    call(Pid, {send, LocalFileName, RemotFileName}, atom).
%%--------------------------------------------------------------------------
%% send_bin(Pid, Bin, RFile) -> ok | {error, epath} | {error, elogin} 
%%                             | {error, enotbinary} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%	RFile = string()
%%
%% Description:  Transfer a binary to a remote file.
%%--------------------------------------------------------------------------
send_bin(Pid, Bin, RFile) when binary(Bin) ->
    call(Pid, {send_bin, Bin, RFile}, atom);
send_bin(_Pid, _Bin, _RFile) ->
  {error, enotbinary}.

%%--------------------------------------------------------------------------
%% send_chunk_start(Pid, RFile) -> ok | {error, elogin} | {error, epath} 
%%                                 | {error, econn}
%%	Pid = pid()
%%	RFile = string()
%%
%% Description:  Start transfer of chunks to remote file.
%%--------------------------------------------------------------------------
send_chunk_start(Pid, RFile) ->
    call(Pid, {send_chunk_start, RFile}, atom).
%%--------------------------------------------------------------------------
%% append_chunk_start(Pid, RFile) -> ok | {error, elogin} | {error, epath} 
%%				     | {error, econn}
%%	Pid = pid()
%%	RFile = string()
%%
%% Description:  Start append chunks of data to remote file.
%%--------------------------------------------------------------------------
append_chunk_start(Pid, RFile) ->
    call(Pid, {append_chunk_start, RFile}, atom).
%%--------------------------------------------------------------------------
%% send_chunk(Pid, Bin) -> ok | {error, elogin} | {error, enotbinary} 
%%                       | {error, echunk} | {error, econn
%%      Pid = pid()
%%	Bin = binary().
%%
%% Purpose:  Send chunk to remote file.
%%--------------------------------------------------------------------------
send_chunk(Pid, Bin) when binary(Bin) ->
    call(Pid, {transfer_chunk, Bin}, atom);
send_chunk(_Pid, _Bin) ->
  {error, enotbinary}.
%%--------------------------------------------------------------------------
%% append_chunk(Pid, Bin) -> ok | {error, elogin} | {error, enotbinary} 
%%			     | {error, echunk} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%
%% Description:  Append chunk to remote file.
%%--------------------------------------------------------------------------
append_chunk(Pid, Bin) when binary(Bin) ->
    call(Pid, {transfer_chunk, Bin}, atom);
append_chunk(_Pid, _Bin) ->
  {error, enotbinary}.
%%--------------------------------------------------------------------------
%% send_chunk_end(Pid) -> ok | {error, elogin} | {error, echunk} 
%%			  | {error, econn}
%%	Pid = pid()
%%
%% Description:  End sending of chunks to remote file.
%%--------------------------------------------------------------------------
send_chunk_end(Pid) ->
    call(Pid, chunk_end, atom).
%%--------------------------------------------------------------------------
%% append_chunk_end(Pid) ->  ok | {error, elogin} | {error, echunk} 
%%			     | {error, econn}
%%	Pid = pid()
%%
%% Description:  End appending of chunks to remote file.
%%--------------------------------------------------------------------------
append_chunk_end(Pid) ->
    call(Pid, chunk_end, atom).

%%--------------------------------------------------------------------------
%% append(Pid, LocalFileName, RemotFileName) -> ok | {error, epath} 
%%                                          | {error, elogin} | {error, econn}
%%	Pid = pid()
%%	LocalFileName = RemotFileName = string()
%%
%% Description:  Append the local file to the remote file
%%--------------------------------------------------------------------------
append(Pid, LocalFileName) ->
    append(Pid, LocalFileName, LocalFileName).

append(Pid, LocalFileName, RemotFileName) ->
    call(Pid, {append, LocalFileName, RemotFileName}, atom).
%%--------------------------------------------------------------------------
%% append_bin(Pid, Bin, RFile) -> ok | {error, epath} | {error, elogin} 
%%				  | {error, enotbinary} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%	RFile = string()
%%
%% Purpose:  Append a binary to a remote file.
%%--------------------------------------------------------------------------
append_bin(Pid, Bin, RFile) when binary(Bin) ->
    call(Pid, {append_bin, Bin, RFile}, atom);
append_bin(_Pid, _Bin, _RFile) ->
    {error, enotbinary}.

%%--------------------------------------------------------------------------
%% quote(Pid, Cmd) -> ok
%%	Pid = pid()
%%	Cmd = string()
%%
%% Description: Send arbitrary ftp command.
%%--------------------------------------------------------------------------
quote(Pid, Cmd) when list(Cmd) ->
    call(Pid, {quote, Cmd}, atom).

%%--------------------------------------------------------------------------
%% close(Pid) -> ok
%%	Pid = pid()
%%
%% Description:  End the ftp session.
%%--------------------------------------------------------------------------
close(Pid) ->
    cast(Pid, close),
    ok.

%%--------------------------------------------------------------------------
%% force_active(Pid) -> ok
%%	Pid = pid()
%%
%% Description: Force connection to use active mode. 
%%--------------------------------------------------------------------------
force_active(Pid) ->
    call(Pid, force_active, atom).

%%--------------------------------------------------------------------------
%% formaterror(Tag) -> string()
%%	Tag = atom() | {error, atom()}
%%
%% Description:  Return diagnostics.
%%--------------------------------------------------------------------------
formaterror(Tag) ->
  errstr(Tag).

%%%========================================================================
%%% gen_server callback functions 
%%%========================================================================

%%-------------------------------------------------------------------------
%% init(Args) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
%% Description: Initiates the erlang process that manages a ftp connection.
%%-------------------------------------------------------------------------
init([Flags]) ->
    inet_db:start(),
    {ok, LDir} = file:get_cwd(),
    State = case is_debug(Flags) of
		true ->
		    dbg:tracer(),
		    dbg:p(all, [call]),
		    dbg:tpl(ftp, [{'_', [], [{return_trace}]}]),
		    #state{ldir = LDir};
		false ->
		    case is_verbose(Flags) of
			true ->
			    #state{verbose = true, ldir = LDir};
			false ->
			    #state{ldir = LDir}  
		    end
	    end,
    process_flag(priority, low), 
    {ok, State#state{ip_v6_disabled = is_ipv6_disabled(Flags)}}.

%%--------------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State}               |
%%                                      {noreply, State, Timeout}      |
%%                                      {stop, Reason, Reply, State}   |
%%                                      {stop, Reason, Reply, State}     
%% Description: Handle incoming requests. 
%%-------------------------------------------------------------------------
handle_call({open, ip_comm, ConnectionData}, From, State) ->
    case key_search(host, ConnectionData, undefined) of
	undefined ->
	    {stop, normal, {error, ehost}, State};
	Host ->
	    Port = key_search(port, ConnectionData, ?FTP_PORT),
	    Timeout = key_search(timeout, ConnectionData, ?CONNECTION_TIMEOUT),
	    setup_ctrl_connection(Host, Port, Timeout, 
				  State#state{client = From})
    end;	
handle_call({open, ip_comm, Host, Port}, From, State) ->
    setup_ctrl_connection(Host, Port, ?CONNECTION_TIMEOUT, 
			  State#state{client = From}); 

handle_call(force_active, _, State) ->
    {reply, ok, State#state{mode = active}};

handle_call({user, User, Password}, From, State) ->
    handle_user(User, Password, "", State#state{client = From});

handle_call({user, User, Password, Acc}, From, State) ->
    handle_user(User, Password, Acc, State#state{client = From});
   
handle_call({account, Acc}, From, State)->
    handle_user_account(Acc, State#state{client = From});

handle_call(pwd, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("PWD", [])), 
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = pwd}};

handle_call(lpwd, From,  #state{ldir = LDir} = State) ->
    {reply, {ok, LDir}, State#state{client = From}};

handle_call({cd, Dir}, From,  #state{chunk = false} 
	    = State) ->
    send_ctrl_message(State, mk_cmd("CWD ~s", [Dir])), 
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = cd}};

handle_call({lcd, Dir}, _From, #state{ldir = LDir0} = State) ->
    LDir = filename:absname(Dir, LDir0),
    case file:read_file_info(LDir) of
	{ok, _ } ->
	    {reply, ok, State#state{ldir = LDir}};
	_  ->
	    {reply, {error, epath}, State}
    end;

handle_call({dir, Len, Dir}, From, #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {dir, Dir, Len},
				      client = From});
handle_call({rename, CurrFile, NewFile}, From,
	    #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("RNFR ~s", [CurrFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {rename, NewFile}, client = From}};

handle_call({delete, File}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("DELE ~s", [File])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({mkdir, Dir}, From,  #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("MKD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({rmdir, Dir}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("RMD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({type, Type}, From,  #state{chunk = false} 
	    = State) ->  
    case Type of
	ascii ->
	    send_ctrl_message(State, mk_cmd("TYPE A", [])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = type, type = ascii, client = From}};
	binary ->
	    send_ctrl_message(State, mk_cmd("TYPE I", [])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = type, type = binary, 
				  client = From}};
	_ ->
	    {reply, {error, etype}, State}
    end;

handle_call({recv, RemoteFile, LocalFile}, From, 
	    #state{chunk = false, ldir = LocalDir} = State) ->
    NewLocalFile = filename:absname(LocalFile, LocalDir),
    
    case file_open(NewLocalFile, write) of
	{ok, Fd} ->
	    setup_data_connection(State#state{client = From,
					      caller = 
					      {recv_file, 
					       RemoteFile, Fd}});
	{error, _What} ->
	    {reply, {error, epath}, State}
    end;
    
handle_call({recv_bin, RFile}, From, #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {recv_bin, RFile},
				      client = From});

handle_call({recv_chunk_start, RFile}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"RETR", RFile},
				      client = From});

handle_call(recv_chunk, _, #state{chunk = false} = State) ->
    {reply, {error, "ftp:recv_chunk_start/2 not called"}, State}; 

handle_call(recv_chunk, From, #state{chunk = true} = State) ->
    activate_data_connection(State),
    {noreply, State#state{client = From, caller = recv_chunk}};
    
handle_call({send, LFile, RFile}, From, #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_file,
						{"STOR", LFile, RFile}},
				      client = From});
handle_call({append, LFile, RFile}, From, #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_file,
						{"APPE", LFile, RFile}},
				      client = From});
handle_call({send_bin, Bin, RFile}, From, #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_data,
					       {"STOR", Bin, RFile}},
				      client = From});
handle_call({append_bin, Bin, RFile}, From, #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_data,
						{"APPE", Bin, RFile}},
				      client = From});
handle_call({send_chunk_start, RFile}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"STOR", RFile},
				      client = From});
handle_call({append_chunk_start, RFile}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"APPE", RFile},
				      client = From});
handle_call({transfer_chunk, Bin}, _, #state{chunk = true} = State) ->
    send_data_message(State, Bin),
    {reply, ok, State};

handle_call(chunk_end, From, #state{chunk = true} = State) ->
    close_data_connection(State),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, dsock = undefined, 
			  caller = end_chunk_transfer, chunk = false}};

handle_call({quote, Cmd}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd(Cmd, [])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = quote}};

handle_call(_, _, #state{chunk = true} = State) ->
    {reply, {error, echunk}, State}.

%%--------------------------------------------------------------------------
%% handle_cast(Request, State) -> {noreply, State} | 
%%                                {noreply, State, Timeout} |
%%                                {stop, Reason, State} 
%% Description: Handles cast messages.         
%%-------------------------------------------------------------------------
handle_cast(close, State) ->
    send_ctrl_message(State, mk_cmd("QUIT", [])),
    close_ctrl_connection(State),
    close_data_connection(State),
    {stop, normal, State#state{csock = undefined, dsock = undefined}}.

%%--------------------------------------------------------------------------
%% handle_info(Msg, State) -> {noreply, State} | {noreply, State, Timeout} |
%%			      {stop, Reason, State}
%% Description: Handles tcp messages from the ftp-server.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------

%%% Data socket messages %%%
handle_info({tcp, Socket, Data}, #state{dsock = Socket, 
					caller = {recv_file, Fd}} 
	    = State) ->    
    file_write(binary_to_list(Data), Fd),
    activate_data_connection(State),
    {noreply, State};

handle_info({tcp, Socket, Data}, #state{dsock = Socket, client = From,	
					caller = recv_chunk} 
	    = State)  ->    
    gen_server:reply(From, {ok, Data}),
    {noreply, State#state{client = undefined, data = <<>>}};

handle_info({tcp, Socket, Data}, #state{dsock = Socket} = State) ->
    activate_data_connection(State),
    {noreply, State#state{data = <<(State#state.data)/binary,
				  Data/binary>>}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, client = From,
					 caller = {recv_file, Fd}} 
	    = State) ->
    file_close(Fd),
    gen_server:reply(From, ok),
    {noreply, State#state{dsock = undefined, client = undefined,
			  data = <<>>, caller = undefined}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, client = From,
					 caller = recv_chunk} 
	    = State) ->
    gen_server:reply(From, ok),
    {noreply, State#state{dsock = undefined, client = undefined,
			  data = <<>>, caller = undefined,
			  chunk = false}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, caller = recv_bin, 
					 data = Data} = State) ->
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, data = <<>>, 
			  caller = {recv_bin, Data}}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, data = Data,
					 caller = {handle_dir_result, Dir}} 
	    = State) ->
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, 
			  caller = {handle_dir_result, Dir, Data},
			  data = <<>>}};
	    
handle_info({tcp_error, Socket, Reason}, #state{dsock = Socket,
						client = From} = State) ->
    gen_server:reply(From, {error, Reason}),
    close_data_connection(State),
    {noreply, State#state{dsock = undefined, client = undefined,
			  data = <<>>, caller = undefined, chunk = false}};

%%% Ctrl socket messages %%%
%% Make sure we received the first 4 bytes so we know how to parse
%% the FTP server response e.i. is the response composed of one
%% or multiple lines.
handle_info({tcp, Socket, Data}, #state{csock = Socket, 
					ctrl_data = {<<>>, [], start}} 
	    = State)
  when size(Data) < 4 -> 
    activate_ctrl_connection(State),
    {noreply, State#state{ctrl_data =  {Data, [], start}}};

handle_info({tcp, Socket, Data}, #state{csock = Socket, verbose = Verbose,
					ctrl_data = {CtrlData, AccLines, 
						     LineStatus}} 
	    = State) ->    
    case parse_ftp_lines(<<CtrlData/binary, Data/binary>>, 
			 AccLines, LineStatus) of
	{ok, [ResCode1, ResCode2, ResCode3 | _] = Lines} ->
	    verbose(Lines, Verbose),
	    CtrlResult =  
		result(ResCode1 ,ResCode2, ResCode3, Lines),
	    handle_ctrl_result(CtrlResult,
			       State#state{ctrl_data = {<<>>, [], start}});
	{continue, NewCtrlData} ->
	    activate_ctrl_connection(State),
	    {noreply, State#state{ctrl_data = NewCtrlData}}
    end;

handle_info({tcp_closed, Socket}, #state{csock = Socket} = State) ->    
    {stop, ftp_server_close, State#state{csock = undefined}};

handle_info({tcp_error, Socket, Reason}, _) ->
    error_logger:error_report("tcp_error on socket: ~p  for reason: ~p~n", 
			      [Socket, Reason]),
    exit(normal); %% User will get error message from terminate/2

%%% Other messages %%%
handle_info(Info, State) ->
    error_logger:info_msg("ftp : ~w : Unexpected message: ~w\n", 
			  [self(),Info]),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------
terminate(normal, State) ->
    do_termiante({error, econn}, State);
terminate(Reason, State) ->
    do_termiante({error, Reason}, State).

do_termiante(ErrorMsg, State) ->
    close_data_connection(State),
    close_ctrl_connection(State),
    case State#state.client of
	undefined ->
	    ok;
	From ->
	    gen_server:reply(From, ErrorMsg)
    end,
    ok. 

code_change(_, _, []) ->
    ok.

%%%=========================================================================
%% Start/stop
%%%=========================================================================
%%--------------------------------------------------------------------------
%% start_link_sup(Args) -> {ok, Pid} | {error, Reason} 
%%                                    
%% Description: Callback function for the ftp supervisor. It is called 
%%            : when open/[1,3] calls ftp_sup:start_child/1 to start an 
%%            : instance of the ftp process.
%%--------------------------------------------------------------------------
start_link_sup([Args, Options]) ->
    gen_server:start_link(?MODULE, Args, Options).

%%% Stop functionality is handled by close/1

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%--------------------------------------------------------------------------
%%% Help function to handle_info
%%--------------------------------------------------------------------------
%%
%%      "A reply is defined to contain the 3-digit code, followed by Space
%%      <SP>, followed by one line of text (where some maximum line length
%%      has been specified), and terminated by the Telnet end-of-line
%%      code, or a so called multilined reply for example:
%%
%%                                123-First line
%%                                Second line
%%                                  234 A line beginning with numbers
%%                                123 The last line
%%
%%         The user-process then simply needs to search for the second
%%         occurrence of the same reply code, followed by <SP> (Space), at
%%         the beginning of a line, and ignore all intermediary lines.  If
%%         an intermediary line begins with a 3-digit number, the Server
%%         will pad the front to avoid confusion.
%% Multiple lines exist
parse_ftp_lines(<<C1, C2, C3, $-, Rest/binary>>, Lines, start) ->
    parse_ftp_lines(Rest, [$-, C3, C2, C1 | Lines], {C1, C2, C3});
%% Only one line exists
parse_ftp_lines(<<C1, C2, C3, ?WHITE_SPACE, Bin/binary>>, Lines, start) ->
    parse_ftp_lines(Bin, [?WHITE_SPACE, C3, C2, C1 | Lines], finish);

%% Last line found
parse_ftp_lines(<<C1, C2, C3, Rest/binary>>, Lines, {C1, C2, C3}) ->
    parse_ftp_lines(Rest, [C3, C2, C1 | Lines], finish);
%% Potential last line wait for more data
parse_ftp_lines(<<C1, C2>> = Data, Lines, {C1, C2, _} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
parse_ftp_lines(<<C1>> = Data, Lines, {C1, _, _} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
parse_ftp_lines(<<>> = Data, Lines, {_,_,_} = StatusCode) ->
    {continue, {Data, Lines, StatusCode}};
%% Part of the multiple lines
parse_ftp_lines(<<Octet, Rest/binary>>, Lines, {_,_, _} = StatusCode) ->
    parse_ftp_lines(Rest, [Octet | Lines], StatusCode);

%% End of FTP server response found
parse_ftp_lines(<<?CR, ?LF>>, Lines, finish) ->
    {ok, lists:reverse([?LF, ?CR | Lines])}; 
parse_ftp_lines(<<?CR, ?LF, Rest/binary>>, Lines, finish) ->
    error_logger:error_report("Recevied unexpected data ~p~n", [Rest]),
    {ok, lists:reverse([?LF, ?CR | Lines])}; 
%% Potential end found  wait for more data 
parse_ftp_lines(<<?CR>> = Data, Lines, finish) ->
    {continue, {Data, Lines, finish}};
parse_ftp_lines(<<>> = Data, Lines, finish) ->
    {continue, {Data, Lines, finish}};
%% Part of last line
parse_ftp_lines(<<Octet, Rest/binary>>, Lines, finish) ->
    parse_ftp_lines(Rest, [Octet | Lines], finish).

%%--------------------------------------------------------------------------
%%% Help functions to handle_call and/or handle_ctrl_result
%%--------------------------------------------------------------------------
%% User handling 
handle_user(User, Password, Acc, State) ->
    send_ctrl_message(State, mk_cmd("USER ~s", [User])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_user, Password, Acc}}}.

handle_user_passwd(Password, Acc, State) ->
    send_ctrl_message(State, mk_cmd("PASS ~s", [Password])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_user_passwd, Acc}}}.

handle_user_account(Acc, State) ->
    send_ctrl_message(State, mk_cmd("ACCT ~s", [Acc])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = handle_user_account}}.

%%--------------------------------------------------------------------------
%% handle_ctrl_result 
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Handling of control connection setup
handle_ctrl_result({pos_compl, _}, #state{caller = open,
					  client = From} 
		   = State) ->
    gen_server:reply(From,  {ok, self()}),
    {noreply, State#state{client = undefined, 
			  caller = undefined }};
handle_ctrl_result({_, Lines}, #state{caller = open} = State) ->
    ctrl_result_response(econn, State, {error, Lines});

%%--------------------------------------------------------------------------
%% Data connection setup active mode 
handle_ctrl_result({pos_compl, _}, #state{mode = active,
					  caller = {setup_data_connection,
						    {LSock, Caller}}} 
		   = State) ->
    handle_caller(State#state{caller = Caller, dsock = {lsock, LSock}});

handle_ctrl_result({Status, Lines}, #state{mode = active, caller = 
				  {setup_data_connection, {LSock, _}}} 
		   = State) ->
    close_connection(LSock),
    ctrl_result_response(Status, State, {error, Lines});

%% Data connection setup passive mode 
handle_ctrl_result({pos_compl, Lines}, #state{mode = passive,
					      ip_v6_disabled = false,
					      caller = 
					      {setup_data_connection, 
					       Caller},
					      csock = CSock,
					      timeout = Timeout} 
		   = State) ->
    [_, PortStr | _] = lists:reverse(string:tokens(Lines, "|")),
    {ok, {IP, _}} = inet:peername(CSock),
    {ok, Socket} = connect(IP, list_to_integer(PortStr),  
				    Timeout, State),
    handle_caller(State#state{caller = Caller, dsock = Socket});


handle_ctrl_result({pos_compl, Lines}, 
		   #state{mode = passive, ip_v6_disabled = true,
			  caller = {setup_data_connection, Caller}, 
			  timeout = Timeout} = State) ->
    
    {_, [?LEFT_PAREN | Rest]} = 
	lists:splitwith(fun(?LEFT_PAREN) -> false; (_) -> true end, Lines),
    {NewPortAddr, _} =
	lists:splitwith(fun(?RIGHT_PAREN) -> false; (_) -> true end, Rest),
    [A1, A2, A3, A4, P1, P2] = lists:map(fun(X) -> list_to_integer(X) end,
					 string:tokens(NewPortAddr, [$,])),
    {ok, Socket} = connect({A1, A2, A3, A4}, (P1 * 256) + P2,
			   Timeout, State),
    handle_caller(State#state{caller = Caller, dsock = Socket});

%% FTP server does not support passive mode try to fallback on active mode
handle_ctrl_result(_, #state{mode = passive, caller = {setup_data_connection, 
						       Caller}} = State) ->
    setup_data_connection(State#state{mode = active, caller = Caller});
    
%%--------------------------------------------------------------------------
%% User handling 
handle_ctrl_result({pos_interm, _}, #state{caller =
					   {handle_user, PassWord, Acc}}
		   = State) ->
    handle_user_passwd(PassWord, Acc, State);
handle_ctrl_result({Status, _}, 
		   #state{caller = {handle_user, _, _}} = State) ->
    ctrl_result_response(Status, State, {error, euser});

%% Accounts 
handle_ctrl_result({pos_interm_acct, _}, #state{caller = 
						{handle_user_passwd, Acc}} = 
		   State) when Acc =/= "" ->
    handle_user_account(Acc, State);
handle_ctrl_result({Status, _},
		   #state{caller = {handle_user_passwd, _}} = State) ->
    ctrl_result_response(Status, State, {error, euser});

%%--------------------------------------------------------------------------
handle_ctrl_result({pos_compl, Lines}, #state{caller = pwd, 
					      client = From} = State) ->
    Dir = pwd_result(Lines),
    gen_server:reply(From, {ok, Dir}),
    {noreply, State#state{client = undefined, caller = undefined}};

%%--------------------------------------------------------------------------
%% Directory listing handling 
handle_ctrl_result({pos_prel, _}, #state{caller = {dir, Dir}} = State) ->
    NewState = accept_data_connection(State),
    activate_data_connection(NewState),
    {noreply, NewState#state{caller = {handle_dir_result, Dir}}};

handle_ctrl_result({pos_compl, _}, #state{caller = {handle_dir_result, Dir,
						    Data}, client = From} 
		   = State) ->
    case Dir of
	"" -> % Current directory
	    gen_server:reply(From, {ok, Data}),
	    {noreply, State#state{client = undefined, caller = undefined}};
	_ ->
	    %% If there is only one line it might be a directory with on
	    %% file but it might be an error message that the directory
	    %% was not found. So in this case we have to endure a little
	    %% overhead to be able to give a good return value. Alas not
	    %% all ftp implementations behave the same and returning
	    %% an error string is allowed by the FTP RFC. 
	    case lists:dropwhile(fun(?CR) -> false;(_) -> true end, 
				 binary_to_list(Data)) of
		[?CR, ?LF] ->	
		    send_ctrl_message(State, mk_cmd("PWD", [])),
		    activate_ctrl_connection(State),
		    {noreply, 
		     State#state{caller = {handle_dir_data, Dir, Data}}};
		_ ->
		    gen_server:reply(From, {ok, Data}),
		    {noreply, State#state{client = undefined,
					  caller = undefined}}
	    end
    end;

handle_ctrl_result({pos_compl, Lines}, 
		   #state{caller = {handle_dir_data, Dir, DirData}} = State) ->
    OldDir = pwd_result(Lines),    
    send_ctrl_message(State, mk_cmd("CWD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_dir_data_second_phase, OldDir,
				    DirData}}};
handle_ctrl_result({Status, _},
		   #state{caller = {handle_dir_data, _, _}} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result({pos_compl, _},
		   #state{caller = {handle_dir_data_second_phase, OldDir, 
				    DirData}} = State) ->
    send_ctrl_message(State, mk_cmd("CWD ~s", [OldDir])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_dir_data_third_phase, DirData}}};
handle_ctrl_result({Status, _}, 
		   #state{caller = {handle_dir_data_second_phase, _, _}} 
		   = State) ->
    ctrl_result_response(Status, State, {error, epath});
handle_ctrl_result(_, #state{caller = {handle_dir_data_third_phase, DirData},
			     client = From} = State) ->
    gen_server:reply(From, {ok, DirData}),
    {noreply, State#state{client = undefined, caller = undefined}};

handle_ctrl_result({Status, _}, #state{caller = cd} = State) ->
    ctrl_result_response(Status, State, {error, epath});

%%--------------------------------------------------------------------------
handle_ctrl_result({pos_interm, _}, #state{caller = {rename, NewFile}} 
		   = State) ->
    send_ctrl_message(State, mk_cmd("RNTO ~s", [NewFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = rename_second_phase}}; 

handle_ctrl_result({Status, _}, 
		   #state{caller = {rename, _}} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result({Status, _},
		   #state{caller = rename_second_phase} = State) ->
    ctrl_result_response(Status, State, {error, epath});

%%--------------------------------------------------------------------------
handle_ctrl_result({pos_prel, _}, #state{caller = recv_bin} = State) ->
    NewState = accept_data_connection(State),
    activate_data_connection(NewState),
    {noreply, NewState};

handle_ctrl_result({pos_compl, _}, #state{caller = {recv_bin, Data},
					  client = From} = State) ->
    gen_server:reply(From, {ok, Data}),
    {noreply, State#state{client = undefined, caller = undefined}};

%%--------------------------------------------------------------------------
handle_ctrl_result({pos_prel, _}, #state{client = From,
					 caller = start_chunk_transfer}
		   = State) ->
    NewState = accept_data_connection(State),
    gen_server:reply(From, ok),
    {noreply, NewState#state{chunk = true, client = undefined,
			     caller = undefined}};
%%--------------------------------------------------------------------------
handle_ctrl_result({pos_prel, _}, #state{caller = {recv_file, _}} = State) ->
    NewState = accept_data_connection(State),
    activate_data_connection(NewState),
    {noreply, NewState};

handle_ctrl_result({Status, _}, #state{caller = {recv_file, Fd}} = State) ->
    file_close(Fd),
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined}, 
			 {error, epath});
%%--------------------------------------------------------------------------
handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_file, Fd}} 
		   = State) ->
    NewState = accept_data_connection(State),
    send_file(Fd, NewState); 

%%--------------------------------------------------------------------------
handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_data, Bin}} 
		   = State) ->
    NewState = accept_data_connection(State),
    send_data_message(NewState, Bin),
    close_data_connection(NewState),
    activate_ctrl_connection(NewState),
    {noreply, NewState#state{caller = transfer_data_second_phase,
			     dsock = undefined}};

%%--------------------------------------------------------------------------
handle_ctrl_result({_, Lines}, #state{caller = quote, client = From} =
		   State) ->
    gen_server:reply(From, string:tokens(Lines, [?CR, ?LF])),
    {noreply, State#state{client = undefined, caller = undefined}};

%%--------------------------------------------------------------------------
handle_ctrl_result({Status, Lines}, #state{client = From} = State) 
  when From =/= undefined ->
    ctrl_result_response(Status, State, {error, Lines}).

%%--------------------------------------------------------------------------
%% Help function to handle_ctrl_result
ctrl_result_response(pos_compl, #state{client = From} = State, _)  ->
    gen_server:reply(From, ok),
    {noreply, State#state{client = undefined, caller = undefined}};

ctrl_result_response(Status, #state{client = From} = State, _) when
Status == etnospc; Status == epnospc; Status == efnamena; Status == econn ->
    gen_server:reply(From, {error, Status}),
    {stop, normal, {error, Status}, State#state{client = undefined}};

ctrl_result_response(_, #state{client = From} = State, ErrorMsg) ->
    gen_server:reply(From, ErrorMsg),
    {noreply, State#state{client = undefined, caller = undefined}}.

%%--------------------------------------------------------------------------
%% Help functions to handle_ctrl_result
%%--------------------------------------------------------------------------
handle_caller(#state{caller = {dir, Dir, Len}} = State) ->
    Cmd = case Len of
	      short -> "NLST";
	      long -> "LIST"
	  end,
    case Dir of 
	"" ->
	    send_ctrl_message(State, mk_cmd(Cmd, ""));
	_ ->
	    send_ctrl_message(State, mk_cmd(Cmd ++ " ~s", [Dir]))
    end,
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {dir, Dir}}};
     
handle_caller(#state{caller = {recv_bin, RFile}} = State) ->
    send_ctrl_message(State, mk_cmd("RETR ~s", [RFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = recv_bin}};

handle_caller(#state{caller = {start_chunk_transfer, Cmd, RFile}} = State) ->
    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = start_chunk_transfer}};

handle_caller(#state{caller = {recv_file, RFile, Fd}} = State) ->
    send_ctrl_message(State, mk_cmd("RETR ~s", [RFile])), 
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {recv_file, Fd}}};

handle_caller(#state{caller = {transfer_file, {Cmd, LocalFile, RemoteFile}},
		     ldir = LocalDir, client = From} = State) ->

    case file_open(filename:absname(LocalFile, LocalDir), read) of
	{ok, Fd} ->
	    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RemoteFile])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = {transfer_file, Fd}}};
	{error, _} ->
	    gen_server:reply(From, {error, epath}),
	    {noreply, State#state{client = undefined, caller = undefined,
				  dsock = undefined}} 
    end;

handle_caller(#state{caller = {transfer_data, {Cmd, Bin, RFile}}} = State) ->
    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {transfer_data, Bin}}}.

%%  ----------- FTP SERVER COMMUNICATION  ------------------------- 

%% Connect to FTP server at Host (default is TCP port 21) 
%% in order to establish a control connection.
setup_ctrl_connection(Host, Port, Timeout, State)->
    case connect(Host, Port, Timeout, State) of
	{ok, CSock} ->
	    NewState = State#state{csock = CSock},
	    activate_ctrl_connection(NewState),
	    {noreply, NewState#state{caller = open}};
	{error, _} ->
	    gen_server:reply(State#state.client, {error, ehost}),
	    {stop, normal, State#state{client = undefined}}
    end.

setup_data_connection(#state{mode = active, caller = Caller, csock = CSock}
		      = State) ->    
    
    IntToString = fun(Element) -> integer_to_list(Element) end,
    
    case (catch inet:sockname(CSock)) of
	{ok, {{_, _, _, _, _, _, _, _} = IP, _}} ->
	    {ok, LSock} = 
		gen_tcp:listen(0, [{ip, IP}, {active, false},
				   inet6, binary, {packet, 0}]),
	    {ok, Port} = inet:port(LSock),
	    Cmd = mk_cmd("EPRT |2|~s:~s:~s:~s:~s:~s:~s:~s|~s|", 
			 lists:map(IntToString, 
				   tuple_to_list(IP) ++ [Port])),
	    send_ctrl_message(State, Cmd),
	    activate_ctrl_connection(State),  
	    {noreply, State#state{caller = {setup_data_connection, 
					    {LSock, Caller}}}};
	{ok, {{_,_,_,_} = IP, _}} ->	    
	    {ok, LSock} = gen_tcp:listen(0, [{ip, IP}, {active, false},
                                     binary, {packet, 0}]),
	    {ok, Port} = inet:port(LSock),
	    {IP1, IP2, IP3, IP4} = IP,
	    {Port1, Port2} = {Port div 256, Port rem 256},
	    send_ctrl_message(State, 
			      mk_cmd("PORT ~w,~w,~w,~w,~w,~w",
				     [IP1, IP2, IP3, IP4, Port1, Port2])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = {setup_data_connection, 
					    {LSock, Caller}}}}
    end;

setup_data_connection(#state{mode = passive, ip_v6_disabled = false,
			     caller = Caller} = State) ->
    send_ctrl_message(State, mk_cmd("EPSV", [])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {setup_data_connection, Caller}}};

setup_data_connection(#state{mode = passive, ip_v6_disabled = true,
			     caller = Caller} = State) ->
    send_ctrl_message(State, mk_cmd("PASV", [])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {setup_data_connection, Caller}}}.


connect(Host, Port, TimeOut, #state{ip_v6_disabled = false}) ->
    {Opts, NewHost} = 
	case catch (inet:getaddr(Host, inet6)) of
	    %% If an ipv4-ipv6-compatible address is returned 
	    %% use ipv4 directly as some ftp-servers does not
	    %% handle "ip4-ipv6-compatiblity" mode well!
	    {ok, {0, 0, 0, 0, _, _, _, _}} ->
		{ok, NewIP} = inet:getaddr(Host, inet),
		{[binary, {packet, 0}, {active, false}], NewIP};
	    {ok, IP} ->
		{[binary, {packet, 0}, {active, false}, inet6], IP};
	    {error, _} ->
		{[binary, {packet, 0}, {active, false}], Host}
	end,
    gen_tcp:connect(NewHost, Port, Opts, TimeOut);

connect(Host, Port, TimeOut, #state{ip_v6_disabled = true}) -> 
    Opts = [binary, {packet, 0}, {active, false}],
    gen_tcp:connect(Host, Port, Opts, TimeOut).

accept_data_connection(#state{mode = active,
			      dsock = {lsock, LSock}} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    State#state{dsock = Socket};

accept_data_connection(#state{mode = passive} = State) ->
    State.

send_ctrl_message(#state{csock = Socket}, Message) ->
    send_message(Socket, Message).

send_data_message(#state{dsock = Socket}, Message) ->
    send_message(Socket, Message).

send_message(Socket, Message) ->
    case gen_tcp:send(Socket, Message) of
	ok ->
	    ok;
	{error, Reason} ->
	    error_logger:error_report("gen_tcp:send/2 failed for reason ~p~n", 
				      [Reason]),
	    exit(normal) %% User will get error message from terminate/2
    end.

activate_ctrl_connection(#state{csock = Socket}) ->
    activate_connection(Socket).

activate_data_connection(#state{dsock = Socket}) ->
    activate_connection(Socket).

activate_connection(Socket) ->
    inet:setopts(Socket, [{active, once}]).

close_ctrl_connection(#state{csock = undefined}) ->
    ok;
close_ctrl_connection(#state{csock = Socket}) ->
    close_connection(Socket).

close_data_connection(#state{dsock = undefined}) ->
    ok;
close_data_connection(#state{dsock = Socket}) ->
    close_connection(Socket).

close_connection(Socket) ->
    gen_tcp:close(Socket).

%%  ------------ FILE HANDELING  ----------------------------------------   

send_file(Fd, State) ->
    case file_read(Fd) of
	{ok, N, Bin} when N > 0->
	    send_data_message(State, Bin),
	    send_file(Fd, State);
	{ok, _, _} ->
	    file_close(Fd),
	    close_data_connection(State),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = transfer_file_second_phase,
				  dsock = undefined}};
        {error, Reason} ->
	    gen_server:reply(State#state.client, {error, Reason}),
	    {stop, normal, State#state{client = undefined}}
    end.

file_open(File, Option) ->
  file:open(File, [raw, binary, Option]).

file_close(Fd) ->
  file:close(Fd).

file_read(Fd) ->				
    case file:read(Fd, ?FILE_BUFSIZE) of
	{ok, Bytes} ->
	    {ok, size(Bytes), Bytes};
	eof ->
	    {ok, 0, []};
	Other ->
	    Other
    end.

file_write(Bytes, Fd) ->
    file:write(Fd, Bytes).

%% --------------  MISC ---------------------------------------------- 

call(GenServer, Msg, Format) ->
    call(GenServer, Msg, Format, infinity).
call(GenServer, Msg, Format, Timeout) ->
    case gen_server:call(GenServer, Msg, Timeout) of
	{ok, Bin} when Format == string ->
	    {ok, binary_to_list(Bin)};
	Result ->
	    Result
    end.

cast(GenServer, Msg) ->
    gen_server:cast(GenServer, Msg).

key_search(Key, List, Default)->	     
    case lists:keysearch(Key, 1, List) of
	{value, {_,Val}} ->
	    Val;
	false ->
	    Default
    end.

mk_cmd(Fmt, Args) ->
    [io_lib:format(Fmt, Args)| [?CR, ?LF]].		% Deep list ok.

result(D1, _D2 ,_D3, Lines) when D1 - $0 > 10;  D1 - $0 < 0 ->
    {error,{invalid_server_response,Lines}};
result(D1, D2, D3, Lines) ->
    Res1 = D1 - $0,
    Res2 = D2 - $0,
    Res3 = D3 - $0,
    {rescode(Res1,Res2,Res3), Lines}.

%% Positive Preleminary Reply
rescode(?POS_PREL,_,_)                   -> pos_prel; 
%% Positive Completion Reply
rescode(?POS_COMPL,_,_)                  -> pos_compl;
%% Positive Intermediate Reply nedd account
rescode(?POS_INTERM,?AUTH_ACC,2)         -> pos_interm_acct;
%% Positive Intermediate Reply
rescode(?POS_INTERM,_,_)                 -> pos_interm; 
%% No storage area no action taken
rescode(?TRANS_NEG_COMPL,?FILE_SYSTEM,2) -> etnospc;
%% Temporary Error, no action taken
rescode(?TRANS_NEG_COMPL,_,_)            -> trans_neg_compl;
%% Permanent disk space error, the user shall not try again
rescode(?PERM_NEG_COMPL,?FILE_SYSTEM,2)  -> epnospc;
rescode(?PERM_NEG_COMPL,?FILE_SYSTEM,3)  -> efnamena; 
%%Directory was not empty
rescode(?PERM_NEG_COMPL,?FILE_SYSTEM,0)  -> perm_dir_not_empty;
rescode(?PERM_NEG_COMPL,_,_)             -> perm_neg_compl.

pwd_result(Lines) ->
    {_, [?DOUBLE_QUOTE | Rest]} = 
	lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Lines),
    {Dir, _} =
	lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Rest),
    Dir.

is_verbose(Params) -> 
    check_param(verbose, Params).

is_debug(Flags) -> 
    check_param(debug, Flags).

is_ipv6_disabled(Flags) -> 
    check_param(ip_v6_disabled, Flags).

check_param(Param, Params) -> 
    lists:member(Param, Params).

verbose(Lines, true) ->
    erlang:display(string:strip(string:strip(Lines, right, ?LF), right, ?CR));
verbose(_, false) ->
    ok.

ensure_started() ->
    %% Start of the inets application should really be handled by the 
    %% application using inets. 
    case application:start(inets) of
	{error,{already_started,inets}} ->
	    ok;
	ok ->
	    error_logger:info_report("The inets application was not started."
				     " Has now been started as a temporary" 
				     " application.")
    end.

%%  ------------ ERROR STRINGS -------------------------- 

errstr({error, Reason}) ->
    errstr(Reason);

errstr(echunk) -> "Synchronisation error during chunk sending.";
errstr(eclosed) -> "Session has been closed.";
errstr(econn) ->  "Connection to remote server prematurely closed.";
errstr(eexists) ->"File or directory already exists.";
errstr(ehost) ->  "Host not found, FTP server not found, "
		      "or connection rejected.";
errstr(elogin) -> "User not logged in.";
errstr(enotbinary) -> "Term is not a binary.";
errstr(epath) ->  "No such file or directory, already exists, "
		      "or permission denied.";
errstr(etype) ->  "No such type.";
errstr(euser) ->  "User name or password not valid.";
errstr(etnospc) -> "Insufficient storage space in system.";
errstr(epnospc) -> "Exceeded storage allocation "
		       "(for current directory or dataset).";
errstr(efnamena) -> "File name not allowed.";
errstr(edirnotempty) -> "Directory not empty.";
errstr(Reason) -> 
    lists:flatten(io_lib:format("Unknown error: ~w", [Reason])).
