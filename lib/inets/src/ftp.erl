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
-module(ftp).

-behaviour(gen_server).

%% This module implements an ftp client based on socket(3)/gen_tcp(3),
%% file(3) and filename(3).
%%

%% This define only apply to `sockets' mode.
-define(OPEN_TIMEOUT,60*1000).

%% Theese defines only apply to `ip_comm' mode.
-define(BYTE_TIMEOUT, 1000).   % Timeout for _ONE_ byte to arrive. (ms)
-define(OPER_TIMEOUT, 300).    % Operation timeout (seconds)

%% Client interface
-export([cd/2, close/1, delete/2, formaterror/1, help/0, 
	 lcd/2, lpwd/1, ls/1, ls/2, mkdir/2, nlist/1, 
	 nlist/2, open/1, open/2, pwd/1, recv/2, recv/3,
	 recv_bin/2, rename/3, rmdir/2, send/2, send/3, 
	 send_bin/3, send_chunk/2, send_chunk_end/1,
	 send_chunk_start/2, type/2, user/3]).

%% Internal
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, sock_connect/3]).


%%
%% CLIENT FUNCTIONS
%%

%% open(Host)
%% open(Host, Flags)
%%
%% Purpose:  Start an ftp client and connect to a host.
%% Args:     Host = string(), Flags = [Flag], Flag = verbose | debug
%% Returns:  {ok, Pid} | {error, ehost}
open(Host) ->
  open(Host, []).


open(Host, Flags) ->
  %% Dbg = {debug,[trace,log,statistics]},
  %% Options = [Dbg],
  Options = [],
  {ok, Pid} = gen_server:start_link(ftp, [Flags], Options),
  gen_server:call(Pid, {open, ip_comm, Host}, infinity).


%% user(Pid, User, Pass)
%% Purpose:  Login.
%% Args:     Pid = pid(), User = Pass = string()
%% Returns:  ok | {error, euser} | {error, econn}
user(Pid, User, Pass) ->
  gen_server:call(Pid, {user, User, Pass}, infinity).

%% pwd(Pid)
%%
%% Purpose:  Get the current working directory at remote server.
%% Args:     Pid = pid()
%% Returns:  {ok, Dir} | {error, elogin} | {error, econn}
pwd(Pid) ->
  gen_server:call(Pid, pwd, infinity).

%% lpwd(Pid)
%%
%% Purpose:  Get the current working directory at local server.
%% Args:     Pid = pid()
%% Returns:  {ok, Dir} | {error, elogin} 
lpwd(Pid) ->
  gen_server:call(Pid, lpwd, infinity).

%% cd(Pid, Dir)
%%
%% Purpose:  Change current working directory at remote server.
%% Args:     Pid = pid(), Dir = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, econn}
cd(Pid, Dir) ->
  gen_server:call(Pid, {cd, Dir}, infinity).

%% lcd(Pid, Dir)
%%
%% Purpose:  Change current working directory for the local client.
%% Args:     Pid = pid(), Dir = string()
%% Returns:  ok | {error, epath}
lcd(Pid, Dir) ->
  gen_server:call(Pid, {lcd, Dir}, infinity).

%% ls(Pid)
%% ls(Pid, Dir)
%%
%% Purpose:  List the contents of current directory (ls/1) or directory
%%           Dir (ls/2) at remote server.
%% Args:     Pid = pid(), Dir = string()
%% Returns:  {ok, Listing} | {error, epath} | {error, elogin} | {error, econn}
ls(Pid) ->
  ls(Pid, "").
ls(Pid, Dir) ->
  gen_server:call(Pid, {dir, long, Dir}, infinity).

%% nlist(Pid)
%% nlist(Pid, Dir)
%%
%% Purpose:  List the contents of current directory (ls/1) or directory
%%           Dir (ls/2) at remote server. The returned list is a stream
%%           of file names.
%% Args:     Pid = pid(), Dir = string()
%% Returns:  {ok, Listing} | {error, epath} | {error, elogin} | {error, econn}
nlist(Pid) ->
  nlist(Pid, "").
nlist(Pid, Dir) ->
  gen_server:call(Pid, {dir, short, Dir}, infinity).

%% rename(Pid, CurrFile, NewFile)
%%
%% Purpose:  Rename a file at remote server.
%% Args:     Pid = pid(), CurrFile = NewFile = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, econn}
rename(Pid, CurrFile, NewFile) ->
  gen_server:call(Pid, {rename, CurrFile, NewFile}, infinity).

%% delete(Pid, File)
%%
%% Purpose:  Remove file at remote server.
%% Args:     Pid = pid(), File = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, econn}
delete(Pid, File) ->
  gen_server:call(Pid, {delete, File}, infinity).

%% mkdir(Pid, Dir)
%%
%% Purpose:  Make directory at remote server.
%% Args:     Pid = pid(), Dir = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, econn}
mkdir(Pid, Dir) ->
  gen_server:call(Pid, {mkdir, Dir}, infinity).

%% rmdir(Pid, Dir)
%%
%% Purpose:  Remove directory at remote server.
%% Args:     Pid = pid(), Dir = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, econn}
rmdir(Pid, Dir) ->
  gen_server:call(Pid, {rmdir, Dir}, infinity).

%% type(Pid, Type)
%%
%% Purpose:  Set transfer type.
%% Args:     Pid = pid(), Type = ascii | binary
%% Returns:  ok | {error, etype} | {error, elogin} | {error, econn}
type(Pid, Type) ->
  gen_server:call(Pid, {type, Type}, infinity).

%% recv(Pid, RFile [, LFile])
%%
%% Purpose:  Transfer file from remote server.
%% Args:     Pid = pid(), RFile = LFile = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, econn}
recv(Pid, RFile) ->
  recv(Pid, RFile, "").

recv(Pid, RFile, LFile) ->
  gen_server:call(Pid, {recv, RFile, LFile}, infinity).

%% recv_bin(Pid, RFile)
%%
%% Purpose:  Transfer file from remote server into binary.
%% Args:     Pid = pid(), RFile = string()
%% Returns:  {ok, Bin} | {error, epath} | {error, elogin} | {error, econn}
recv_bin(Pid, RFile) ->
  gen_server:call(Pid, {recv_bin, RFile}, infinity).

%% send(Pid, LFile [, RFile])
%%
%% Purpose:  Transfer file to remote server.
%% Args:     Pid = pid(), LFile = RFile = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, econn}
send(Pid, LFile) ->
  send(Pid, LFile, "").

send(Pid, LFile, RFile) ->
  gen_server:call(Pid, {send, LFile, RFile}, infinity).

%% send_bin(Pid, Bin, RFile)
%%
%% Purpose:  Transfer a binary to a remote file.
%% Args:     Pid = pid(), Bin = binary(), RFile = string()
%% Returns:  ok | {error, epath} | {error, elogin} | {error, enotbinary} 
%%           | {error, econn}
send_bin(Pid, Bin, RFile) when binary(Bin) ->
  gen_server:call(Pid, {send_bin, Bin, RFile}, infinity);
send_bin(Pid, Bin, RFile) ->
  {error, enotbinary}.

%% send_chunk_start(Pid, RFile)
%%
%% Purpose:  Start transfer of chunks to remote file.
%% Args:     Pid = pid(), RFile = string().
%% Returns:  ok | {error, elogin} | {error, epath} | {error, econn}
send_chunk_start(Pid, RFile) ->
  gen_server:call(Pid, {send_chunk_start, RFile}, infinity).


%% send_chunk(Pid, Bin)
%%
%% Purpose:  Send chunk to remote file.
%% Args:     Pid = pid(), Bin = binary().
%% Returns:  ok | {error, elogin} | {error, enotbinary} | {error, echunk}
%%           | {error, econn}
send_chunk(Pid, Bin) when binary(Bin) ->
  gen_server:call(Pid, {send_chunk, Bin}, infinity);
send_chunk(Pid, Bin) ->
  {error, enotbinary}.

%% send_chunk_end(Pid)
%%
%% Purpose:  End sending of chunks to remote file.
%% Args:     Pid = pid().
%% Returns:  ok | {error, elogin} | {error, echunk} | {error, econn}
send_chunk_end(Pid) ->
  gen_server:call(Pid, send_chunk_end, infinity).

%% close(Pid)
%%
%% Purpose:  End the ftp session.
%% Args:     Pid = pid()
%% Returns:  ok
close(Pid) ->
  case (catch gen_server:call(Pid, close, 30000)) of
      ok -> 
	  ok;
      {'EXIT',{noproc,_}} ->
	  %% Already gone...
	  ok;
      Res ->
	  Res
  end.

%% formaterror(Tag) 
%%
%% Purpose:  Return diagnostics.
%% Args:     Tag = atom() | {error, atom()}
%% Returns:  string().
formaterror(Tag) ->
  errstr(Tag).

%% help()
%%
%% Purpose:  Print list of valid commands.
%%
%% Undocumented.
%%
help() ->
  io:format("\n  Commands:\n"
	    "  ---------\n"
	    "  cd(Pid, Dir)\n"
	    "  close(Pid)\n"
	    "  delete(Pid, File)\n"
	    "  formaterror(Tag)\n"
	    "  help()\n"
	    "  lcd(Pid, Dir)\n"
	    "  lpwd(Pid)\n"
	    "  ls(Pid [, Dir])\n"
	    "  mkdir(Pid, Dir)\n"
	    "  nlist(Pid [, Dir])\n"
	    "  open(Host [, Flags])\n"
	    "  pwd(Pid)\n"
	    "  recv(Pid, RFile [, LFile])\n"
	    "  recv_bin(Pid, RFile)\n"
	    "  rename(Pid, CurrFile, NewFile)\n"
	    "  rmdir(Pid, Dir)\n"
	    "  send(Pid, LFile [, RFile])\n"
	    "  send_chunk(Pid, Bin)\n"
	    "  send_chunk_start(Pid, RFile)\n"
	    "  send_chunk_end(Pid)\n"
	    "  send_bin(Pid, Bin, RFile)\n"
	    "  type(Pid, Type)\n"
	    "  user(Pid, User, Pass)\n").

%%
%% INIT
%%

-record(state, {csock = undefined, dsock = undefined, flags = undefined, 
		ldir = undefined, type = undefined, chunk = false,
		pending = undefined}).

init([Flags]) ->
  sock_start(),				
  process_flag(priority, low),
  {ok, LDir} = file:get_cwd(),
  {ok, #state{flags = Flags, ldir = LDir}}.

%%
%% HANDLERS
%%

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


-define(STOP_RET(E),{stop, normal, {error, E}, 
		     State#state{csock = undefined}}).


rescode(?POS_PREL,_,_)                   -> pos_prel;
rescode(?POS_COMPL,_,_)                  -> pos_compl;
rescode(?POS_INTERM,_,_)                 -> pos_interm;
rescode(?TRANS_NEG_COMPL,?FILE_SYSTEM,2) -> trans_no_space;
rescode(?TRANS_NEG_COMPL,_,_)            -> trans_neg_compl;
rescode(?PERM_NEG_COMPL,?FILE_SYSTEM,2)  -> perm_no_space;
rescode(?PERM_NEG_COMPL,?FILE_SYSTEM,3)  -> perm_fname_not_allowed;
rescode(?PERM_NEG_COMPL,_,_)             -> perm_neg_compl.

retcode(trans_no_space,_)         -> etnospc;
retcode(perm_no_space,_)          -> epnospc;
retcode(perm_fname_not_allowed,_) -> efnamena;
retcode(_,Otherwise)              -> Otherwise.



handle_call({open,sockets,Host},From,State) ->
  OpenProcess=proc_lib:spawn_link(ftp,sock_connect,[Host,From,self()]),
  case timer:send_after(?OPEN_TIMEOUT,{open_timeout,From,OpenProcess}) of
    {ok,TimeoutRef} ->
      {noreply,State#state{pending={OpenProcess,TimeoutRef}}};
    {error,Reason} ->
      {stop,normal,{error,Reason},State}
  end;
handle_call({open,ip_comm,Host},From,State) ->
  case sock_connect(Host,dummy,dummy) of
    {error, What} ->
      {stop, normal, {error, What}, State};
    CSock ->
      result(CSock, State#state.flags),
      {reply, {ok, self()}, State#state{csock = CSock}}
  end;
handle_call({user, User, Pass}, _From, State) ->
  #state{csock = CSock, flags = Flags} = State,
  case ctrl_cmd(CSock, "USER ~s", [User], Flags) of
    pos_interm ->
      case ctrl_cmd(CSock, "PASS ~s", [Pass], Flags) of
	pos_compl ->
	  set_type(binary, CSock, Flags),
	  {reply, ok, State#state{type = binary}};
	{error,enotconn} ->
	  ?STOP_RET(econn);
	_ ->
	  {reply, {error, euser}, State}
      end;
    pos_compl ->
      set_type(binary, CSock, Flags),
      {reply, ok, State#state{type = binary}};
    {error, enotconn} ->
      ?STOP_RET(econn);
    _ ->
      {reply, {error, euser}, State}
  end;

handle_call(pwd, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  %%
  %% NOTE: The directory string comes over the control connection.
  case sock_write(CSock, mk_cmd("PWD", [])) of
    ok ->
      {_, Line} = result_line(CSock, Flags),
      {_, Cs} = split($", Line),			% XXX Ugly
      {Dir0, _} = split($", Cs),
      Dir = lists:delete($", Dir0),
      {reply, {ok, Dir}, State};
    {error, enotconn} ->
      ?STOP_RET(econn)
  end;

handle_call(lpwd, _From, State) ->
  #state{csock = CSock, flags = Flags, ldir = LDir} = State,
  {reply, {ok, LDir}, State};

handle_call({cd, Dir}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  case ctrl_cmd(CSock, "CWD ~s", [Dir], Flags) of
    pos_compl ->
      {reply, ok, State};
    {error, enotconn} ->
      ?STOP_RET(econn);
    _ ->
      {reply, {error, epath}, State}
  end;

handle_call({lcd, Dir}, _From, State) ->
  #state{csock = CSock, flags = Flags, ldir = LDir0} = State,
  LDir = absname(LDir0, Dir),
  case file:read_file_info(LDir) of
    {ok, _ } ->
      {reply, ok, State#state{ldir = LDir}};
    _  ->
      {reply, {error, epath}, State}
  end;

handle_call({dir, Len, Dir}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags, type = Type} = State,
  set_type(ascii, Type, CSock, Flags),
  LSock = listen_data(CSock, raw, Flags),
  Cmd = case Len of
	  short -> "NLST";
	  long -> "LIST"
	end,
  Result = case Dir of 
	     "" ->
	       ctrl_cmd(CSock, Cmd, "", Flags);
	     _ ->
	       ctrl_cmd(CSock, Cmd ++ " ~s", [Dir], Flags)
	   end,
  case Result of
    pos_prel ->
      DSock = accept_data(LSock),
      DirData = recv_data(DSock),
      Reply0 = case result(CSock, Flags) of
		 pos_compl ->
		   {ok, DirData};
		 _ ->
		   {error, epath}
	       end,
      sock_close(DSock),
      reset_type(ascii, Type, CSock, Flags),
      {reply, Reply0, State};
    {closed, _Why} ->
      ?STOP_RET(econn);
    _ ->
      sock_close(LSock),
      {reply, {error, epath}, State}
  end;


handle_call({rename, CurrFile, NewFile}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  case ctrl_cmd(CSock, "RNFR ~s", [CurrFile], Flags) of
    pos_interm ->
      case ctrl_cmd(CSock, "RNTO ~s", [NewFile], Flags) of
	pos_compl ->
	  {reply, ok, State};
	_ ->
	  {reply, {error, epath}, State}
      end;
    {error, enotconn} ->
      ?STOP_RET(econn);
    _ ->
      {reply, {error, epath}, State}
  end;

handle_call({delete, File}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  case ctrl_cmd(CSock, "DELE ~s", [File], Flags) of
    pos_compl ->
      {reply, ok, State};
    {error, enotconn} ->
      ?STOP_RET(econn);
    _ ->
      {reply, {error, epath}, State}
  end;

handle_call({mkdir, Dir}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  case ctrl_cmd(CSock, "MKD ~s", [Dir], Flags) of
    pos_compl ->
      {reply, ok, State};
    {error, enotconn} ->
      ?STOP_RET(econn);
    _ ->
      {reply, {error, epath}, State}
  end;

handle_call({rmdir, Dir}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  case ctrl_cmd(CSock, "RMD ~s", [Dir], Flags) of
    pos_compl ->
      {reply, ok, State};
    {error, enotconn} ->
      ?STOP_RET(econn);
    _ ->
      {reply, {error, epath}, State}
  end;

handle_call({type, Type}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  case Type of
    ascii ->
      set_type(ascii, CSock, Flags),
      {reply, ok, State#state{type = ascii}};
    binary ->
      set_type(binary, CSock, Flags),
      {reply, ok, State#state{type = binary}};
    _ ->
      {reply, {error, etype}, State}
  end;

handle_call({recv, RFile, LFile}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags, ldir = LDir} = State,
  ALFile = case LFile of
	     "" ->
	       absname(LDir, RFile);
	     _ ->
	       absname(LDir, LFile)
	   end,
  case file_open(ALFile, write) of
    {ok, Fd} ->
      LSock = listen_data(CSock, binary, Flags),
      Ret = case ctrl_cmd(CSock, "RETR ~s", [RFile], Flags) of
	      pos_prel ->
		DSock = accept_data(LSock),
		recv_file(DSock, Fd),
		Reply0 = case result(CSock, Flags) of
			   pos_compl ->
			     ok;
			   _ ->
			     {error, epath}
			 end,
		sock_close(DSock),
		{reply, Reply0, State};
	      {error, enotconn} ->
		?STOP_RET(econn);
	      _ ->
		{reply, {error, epath}, State}
	    end,
      file_close(Fd),
      Ret;
    {error, _What} ->
      {reply, {error, epath}, State}
  end;

handle_call({recv_bin, RFile}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags, ldir = LDir} = State,
  LSock = listen_data(CSock, binary, Flags),
  case ctrl_cmd(CSock, "RETR ~s", [RFile], Flags) of
    pos_prel ->
      DSock = accept_data(LSock),
      Bin = recv_binary(DSock),
      Reply0 = case result(CSock, Flags) of
		 pos_compl ->
		   {ok, Bin};
		 _ ->
		   {error, epath}
	       end,
      sock_close(DSock),
      {reply, Reply0, State};
    {error, enotconn} ->
      ?STOP_RET(econn);
    _ ->
      {reply, {error, epath}, State}
  end;

handle_call({send, LFile, RFile}, _From, State) when State#state.chunk == false ->
    #state{csock = CSock, flags = Flags, ldir = LDir} = State,
    ARFile = case RFile of
	       "" ->
		   LFile;
	       _ ->
		   RFile
	   end,
    ALFile = absname(LDir, LFile),
    case file_open(ALFile, read) of
      {ok, Fd} ->
	  LSock = listen_data(CSock, binary, Flags),
	  case ctrl_cmd(CSock, "STOR ~s", [ARFile], Flags) of
	      pos_prel ->
		  DSock = accept_data(LSock),
		  SFreply = send_file(Fd, DSock),
		  file_close(Fd),
		  sock_close(DSock),
		  case {SFreply,result(CSock, Flags)} of
		      {ok,pos_compl} ->
			  {reply, ok, State};
		      {ok,Other} ->
			  debug(Flags,"     error: unknown reply: ~p~n",
				[Other]),
			  {reply, {error, epath}, State};
		      {{error,Why},Result} ->
			  ?STOP_RET(retcode(Result,econn))
		  end;
	      {error, enotconn} ->
		  ?STOP_RET(econn);
	      Other ->
		  debug(Flags,"     error: ctrl failed: ~p~n",[Other]),
		  {reply, {error, epath}, State}
	  end;
	{error, Reason} ->
	    debug(Flags,"     error: file open: ~p~n",[Reason]),
	    {reply, {error, epath}, State}
    end;

handle_call({send_bin, Bin, RFile}, _From, State) when State#state.chunk == false ->
    #state{csock = CSock, flags = Flags, ldir = LDir} = State,
    LSock = listen_data(CSock, binary, Flags),
    case ctrl_cmd(CSock, "STOR ~s", [RFile], Flags) of
	pos_prel ->
	    DSock  = accept_data(LSock),
	    SReply = sock_write(DSock, Bin),
	    sock_close(DSock),
	    case {SReply,result(CSock,Flags)} of
		{ok,pos_compl} ->
		    {reply, ok, State};
		{ok,trans_no_space} ->
		    ?STOP_RET(etnospc);
		{ok,perm_no_space} ->
		    ?STOP_RET(epnospc);
		{ok,perm_fname_not_allowed} ->
		    ?STOP_RET(efnamena);
		{ok,Other} ->
		    debug(Flags,"     error: unknown reply: ~p~n",[Other]),
		    {reply, {error, epath}, State};
		{{error,Why},Result} ->
		    ?STOP_RET(retcode(Result,econn))
	    %% {{error,_Why},_Result} ->
	    %%    ?STOP_RET(econn)
	    end;

	{error, enotconn} ->
	    ?STOP_RET(econn);

	Other ->
	    debug(Flags,"     error: ctrl failed: ~p~n",[Other]),
	    {reply, {error, epath}, State}
    end;

handle_call({send_chunk_start, RFile}, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags, ldir = LDir} = State,
  LSock = listen_data(CSock, binary, Flags),
  case ctrl_cmd(CSock, "STOR ~s", [RFile], Flags) of
    pos_prel ->
      DSock = accept_data(LSock),
      {reply, ok, State#state{dsock = DSock, chunk = true}};
    {error, enotconn} ->
      ?STOP_RET(econn);
    Otherwise ->
      debug(Flags,"     error: ctrl failed: ~p~n",[Otherwise]),
      {reply, {error, epath}, State}
  end;

handle_call({send_chunk, Bin}, _From, State) when State#state.chunk == true ->
  #state{dsock = DSock, csock = CSock, flags = Flags} = State,
  case DSock of
    undefined ->
      {reply,{error,econn},State};
    _ ->
      case sock_write(DSock, Bin) of
	ok ->
	  {reply, ok, State};
	Other ->
	  debug(Flags,"     error: chunk write error: ~p~n",[Other]),
	  {reply, {error, econn}, State#state{dsock = undefined}}
      end
  end;

handle_call(send_chunk_end, _From, State) when State#state.chunk == true ->
    #state{csock = CSock, dsock = DSock, flags = Flags} = State,
    case DSock of
	undefined ->
	    Result = result(CSock, Flags),
	    case Result of
		pos_compl ->
		    {reply,ok,State#state{dsock = undefined, 
					  chunk = false}};
		trans_no_space ->
		    ?STOP_RET(etnospc);
		perm_no_space ->
		    ?STOP_RET(epnospc);
		perm_fname_not_allowed ->
		    ?STOP_RET(efnamena);
		Result ->
		    debug(Flags,"     error: send chunk end (1): ~p~n",
			  [Result]),
		    {reply,{error,epath},State#state{dsock = undefined, 
						     chunk = false}}
	    end;
	_ ->
	    sock_close(DSock),
	    Result = result(CSock, Flags),
	    case Result of
		pos_compl ->
		    {reply,ok,State#state{dsock = undefined, 
					  chunk = false}};
		trans_no_space ->
		    sock_close(CSock),
		    ?STOP_RET(etnospc);
		perm_no_space ->
		    sock_close(CSock),
		    ?STOP_RET(epnospc);
		perm_fname_not_allowed ->
		    sock_close(CSock),
		    ?STOP_RET(efnamena);
		Result ->
		    debug(Flags,"     error: send chunk end (2): ~p~n",
			  [Result]),
		    {reply,{error,epath},State#state{dsock = undefined, 
						     chunk = false}}
	    end
    end;

handle_call(close, _From, State) when State#state.chunk == false ->
  #state{csock = CSock, flags = Flags} = State,
  ctrl_cmd(CSock, "QUIT", [], Flags),
  sock_close(CSock),
  {stop, normal, ok, State};

handle_call(_, _From, State) when State#state.chunk == true ->
  {reply, {error, echunk}, State}.

handle_cast(Msg, State) ->
  {noreply, State}.

handle_info({Sock, {fromsocket, Bytes}}, State) when Sock == State#state.csock ->
  put(leftovers, Bytes ++ leftovers()),
  {noreply, State};

%% Data connection closed (during chunk sending)
handle_info({Sock, {socket_closed, _Reason}}, State) when Sock == State#state.dsock ->
  {noreply, State#state{dsock = undefined}};
%% Control connection closed.
handle_info({Sock, {socket_closed, _Reason}}, State) when Sock == State#state.csock ->
  debug(State#state.flags,"   sc : ~s~n",[leftovers()]),
  {stop, ftp_server_close, State#state{csock = undefined}};

%% Only used for sockets communication (not ip_comm)
handle_info({open_reply,From,{ok,CSock}},State) ->
  case State#state.pending of
    {OpenProcess,TimeoutRef} ->
      gen_server:reply(From,{ok,self()}),
      timer:cancel(TimeoutRef),
      %%io:format("timer canceled~n"),
      result(CSock,State#state.flags),
      {noreply,State#state{csock=CSock,pending=undefined}};
    undefined ->
      %%io:format("timeout reply already sent~n"),
      {noreply,State}
  end;

handle_info({open_reply,From,Reply},State) ->
  gen_server:reply(From,Reply),
  {noreply,State};

%% Only used for sockets communication (not ip_comm)
handle_info({open_timeout,From,OpenProcess},State) ->
  case State#state.pending of
    {OpenProcess,TimeoutRef} ->
      gen_server:reply(From,{error,ehost}),
      exit(OpenProcess,normal),
      %%io:format("open process killed~n"),
      {stop,normal,State};  
    false ->
      %%io:format("normal reply already sent~n"),
      {noreply,State}
  end;

handle_info(Info, State) ->
  error_logger:info_msg("ftp : ~w : Unexpected message: ~w\n", 
			[self(), Info]),
  {noreply, State}.

terminate(Reason, State) ->
  ok.

%%
%% CONTROL CONNECTION
%%

ctrl_cmd(CSock, Fmt, Args, Flags) ->
  Cmd = mk_cmd(Fmt, Args),
  case sock_write(CSock, Cmd) of
    ok ->
      debug(Flags,"   command: ~s",[Cmd]),
      result(CSock, Flags);
    {error, enotconn} ->
      {error, enotconn};
    Other ->
	  Other
  end.

mk_cmd(Fmt, Args) ->
  [io_lib:format(Fmt, Args)| "\r\n"].		% Deep list ok.

%%
%% TRANSFER TYPE
%%

%%
%% set_type(NewType, CurrType, CSock, Flags)
%% reset_type(NewType, CurrType, CSock, Flags)
%%
set_type(Type, Type, CSock, Flags) ->
  ok;
set_type(NewType, _OldType, CSock, Flags) ->
  set_type(NewType, CSock, Flags).

reset_type(Type, Type, CSock, Flags) ->
  ok;
reset_type(_NewType, OldType, CSock, Flags) ->
  set_type(OldType, CSock, Flags).

set_type(ascii, CSock, Flags) ->
  ctrl_cmd(CSock, "TYPE A", [], Flags);
set_type(binary, CSock, Flags) ->
  ctrl_cmd(CSock, "TYPE I", [], Flags).

%%
%% DATA CONNECTION 
%%

%% Create a listen socket for a data connection and send a PORT command 
%% containing the IP address and port number. Mode is binary or raw.
%%
listen_data(CSock, Mode, Flags) ->
  {IP, _} = sock_name(CSock), % IP address of control conn.
  LSock = sock_listen(Mode, IP),
  Port = sock_listen_port(LSock),
  {A1, A2, A3, A4} = IP,
  {P1, P2} = {Port div 256, Port rem 256},
  ctrl_cmd(CSock, "PORT ~w,~w,~w,~w,~w,~w", [A1, A2, A3, A4, P1, P2], 
	   Flags),
  LSock.

%%
%% Accept the data connection and close the listen socket.
%%
accept_data(LSock) ->
  Sock = sock_accept(LSock),
  sock_close(LSock),
  Sock.

%%
%% DATA COLLECTION (ls, dir)
%%
%% Socket is a byte stream in ASCII mode.
%%

%% Receive data (from data connection).
recv_data(Sock) ->
    recv_data(Sock, [], 0).
recv_data(Sock, Sofar, ?OPER_TIMEOUT) ->
    sock_close(Sock),
    lists:flatten(lists:reverse(Sofar));
recv_data(Sock, Sofar, Retry) ->
    case sock_read(Sock) of
	{ok, Data} ->
	    recv_data(Sock, [Data| Sofar], 0);
	{error, timeout} ->
	    %% Retry..
	    recv_data(Sock, Sofar, Retry+1);
	{closed, _} ->
	    lists:flatten(lists:reverse(Sofar))
    end.

%%
%% BINARY TRANSFER
%%

%% recv_binary(Sock) = {ok, Bin}
%% 
recv_binary(Sock) ->
  recv_binary(Sock, [], 0).

recv_binary(Sock, Bs, ?OPER_TIMEOUT) ->
    sock_close(Sock),
    {closed, timeout};
recv_binary(Sock, Bs, Retry) ->
    case sock_read(Sock) of
	{ok, Bin} ->
	    recv_binary(Sock, [Bs, Bin], 0);
	{error, timeout} ->
	    recv_binary(Sock, Bs, Retry+1);
	{closed, _Why} ->
	    {ok, list_to_binary(Bs)}
  end.

%%
%% FILE TRANSFER
%%

recv_file(Sock, Fd) ->
    recv_file(Sock, Fd, 0).

recv_file(Sock, Fd, ?OPER_TIMEOUT) ->
    sock_close(Sock),
    {closed, timeout};
recv_file(Sock, Fd, Retry) ->
    case sock_read(Sock) of
	{ok, Bin} ->
	    file_write(Fd, Bin),
	    recv_file(Sock, Fd);
	{error, timeout} ->
	    recv_file(Sock, Fd, Retry+1);
	{closed, How} ->
	    {closed, How}
  end.

%%
%% send_file(Fd, Sock) = ok | {error, Why}
%%

send_file(Fd, Sock) ->
  {N, Bin} = file_read(Fd),
  if 
    N > 0 ->
      case sock_write(Sock, Bin) of
	ok ->
	  send_file(Fd, Sock);
	{error, Reason} ->
	  {error, Reason}
      end;
    true ->
      ok
  end.

%%
%% PARSING OF RESULT LINES
%%

%% Excerpt from RFC 959:
%%
%%      "A reply is defined to contain the 3-digit code, followed by Space
%%      <SP>, followed by one line of text (where some maximum line length
%%      has been specified), and terminated by the Telnet end-of-line
%%      code.  There will be cases however, where the text is longer than
%%      a single line.  In these cases the complete text must be bracketed
%%      so the User-process knows when it may stop reading the reply (i.e.
%%      stop processing input on the control connection) and go do other
%%      things.  This requires a special format on the first line to
%%      indicate that more than one line is coming, and another on the
%%      last line to designate it as the last.  At least one of these must
%%      contain the appropriate reply code to indicate the state of the
%%      transaction.  To satisfy all factions, it was decided that both
%%      the first and last line codes should be the same.
%%
%%         Thus the format for multi-line replies is that the first line
%%         will begin with the exact required reply code, followed
%%         immediately by a Hyphen, "-" (also known as Minus), followed by
%%         text.  The last line will begin with the same code, followed
%%         immediately by Space <SP>, optionally some text, and the Telnet
%%         end-of-line code.
%%
%%            For example:
%%                                123-First line
%%                                Second line
%%                                  234 A line beginning with numbers
%%                                123 The last line
%%
%%         The user-process then simply needs to search for the second
%%         occurrence of the same reply code, followed by <SP> (Space), at
%%         the beginning of a line, and ignore all intermediary lines.  If
%%         an intermediary line begins with a 3-digit number, the Server
%%         must pad the front  to avoid confusion.
%%
%%            This scheme allows standard system routines to be used for
%%            reply information (such as for the STAT reply), with
%%            "artificial" first and last lines tacked on.  In rare cases
%%            where these routines are able to generate three digits and a
%%            Space at the beginning of any line, the beginning of each
%%            text line should be offset by some neutral text, like Space.
%%
%%         This scheme assumes that multi-line replies may not be nested."

%% We have to collect the stream of result characters into lines (ending
%% in "\r\n"; we check for "\n"). When a line is assembled, left-over 
%% characters are saved in the process dictionary.
%%

%% result(Sock, Flags) = rescode()
%% 
result(Sock, Flags) ->
  result(Sock, Flags, false).

result_line(Sock, Flags) ->
  result(Sock, Flags, true).

%% result(Sock, Flags, Bool) = rescode() | {rescode(), Lines}
%% Printout if Bool = true.
%%
result(Sock, Flags, RetForm) ->
    case getline(Sock, Flags) of
	Line when length(Line) > 3 ->
	    [D1, D2, D3| Tail] = Line,
	    case Tail of
		[$-| _] ->
		    parse_to_end(Sock, [D1, D2, D3, $ ], Flags); % 3 digits + space
		_ ->
		    ok
	    end,
	    Res1 = D1 - $0,
	    Res2 = D2 - $0,
	    Res3 = D3 - $0,
	    verbose(Flags,"    ~w : ~s", [Res1, Line]),
	    ResCode = rescode(Res1,Res2,Res3),
	    case RetForm of
		true ->
		    {ResCode, Line};
		_ ->
		    ResCode
	    end;
	_ ->
	    case RetForm of
		true ->
		    {rescode(?PERM_NEG_COMPL,-1,-1), []};
		_ ->
		    rescode(?PERM_NEG_COMPL,-1,-1)
	    end
    end.

leftovers() ->
  case get(leftovers) of
    undefined -> [];
    X -> X
  end.

%% getline(Sock, Flags) = Line
%%
getline(Sock, Flags) ->
  getline(Sock, leftovers(), Flags).

getline(Sock, Rest, Flags) ->
  getline1(Sock, split($\n, Rest), Flags, 0).

getline1(Sock, {[], Rest}, Flags, ?OPER_TIMEOUT) ->
    sock_close(Sock),
    put(leftovers, Rest),
    [];
getline1(Sock, {[], Rest}, Flags, Retry) ->
    case sock_read(Sock) of
	{ok, More} ->
	    debug(Flags,"   gl : ~s~n",[More]),
	    %% debug(Flags,"gl:     ~s~n",[More]),
	    getline(Sock, Rest ++ More, Flags);
	{error, timeout} ->
	    %% Retry..
	    getline1(Sock, {[], Rest}, Flags, Retry+1);
	Error ->
	    put(leftovers, Rest),
	    []
    end;
getline1(Sock, {Line, Rest}, Flags, Retry) ->
    put(leftovers, Rest),
    Line.

parse_to_end(Sock, Prefix, Flags) ->
  Line = getline(Sock, Flags),
  case lists:prefix(Prefix, Line) of
    false ->
      parse_to_end(Sock, Prefix, Flags);
    true ->
      ok
  end.


%% Split list after first occurence of S.
%% Returns {Prefix, Suffix} ({[], Cs} if S not found).
split(S, Cs) ->
  split(S, Cs, []).

split(S, [S| Cs], As) ->
  {lists:reverse([S|As]), Cs};
split(S, [C| Cs], As) ->
  split(S, Cs, [C| As]);
split(_, [], As) ->
  {[], lists:reverse(As)}.

%%
%%  FILE INTERFACE
%%
%%  All files are opened raw in binary mode.
%%
-define(BUFSIZE, 4096).

file_open(File, Option) ->
  file:rawopen(File, {binary, Option}).

file_close(Fd) ->
  file:close(Fd).


file_read(Fd) ->				% Compatible with pre R2A.
  case file:read(Fd, ?BUFSIZE) of
    {ok, {N, Bytes}} ->
      {N, Bytes};
    {ok, Bytes} ->
      {size(Bytes), Bytes};
    eof ->
      {0, []}
  end.

file_write(Fd, Bytes) ->
  file:write(Fd, Bytes).

absname(Dir, File) ->				% Args swapped.
  filename:absname(File, Dir).	

%%
%% SOCKET INTERFACE
%%

-define(FTPPORT, 21).

%% sock_start()
%%

%%
%% USE GEN_TCP
%%

sock_start() ->
  inet_db:start().

%%
%% Connect to FTP server at Host (TCP port 21) in raw mode, in order to 
%% establish a control connection.
%%

%% This is so ugly! The socket code has to removed for R4.
sock_connect(Host,dummy,dummy) ->
  case (catch gen_tcp:connect(Host, ?FTPPORT, 
			      [{packet, 0}, {active, false}],?OPEN_TIMEOUT)) of
    {'EXIT', _} ->				% XXX Probably no longer needed.
      {error, ehost};
    {error, _} ->
      {error, ehost};
    {ok, Sock} ->
      Sock
  end.

%% 
%% Create a listen socket (any port) in binary or raw non-packet mode for 
%% data connection.
%%
sock_listen(Mode, IP) ->
  Opts = case Mode of 
	   binary ->
	     [binary, {packet, 0}];
	   raw ->
	     [{packet, 0}]
	 end,
  {ok, Sock} = gen_tcp:listen(0, [{ip, IP}, {active, false} | Opts]),
  Sock.

sock_accept(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  Sock.

sock_close(undefined) ->
  ok;
sock_close(Sock) ->
  gen_tcp:close(Sock).

sock_read(Sock) ->
  case gen_tcp:recv(Sock, 0, ?BYTE_TIMEOUT) of
      {ok, Bytes} ->
	  {ok, Bytes};
      {error, closed} ->
	  {closed, closed};			% Yes
      {error, etimedout} ->
	  {error, timeout};
      Other ->
	  Other
  end.

%%    receive
%%	{tcp, Sock, Bytes} ->
%%	    {ok, Bytes};
%%	{tcp_closed, Sock} ->
%%	    {closed, closed}
%%    end.

sock_write(Sock, Bytes) ->
  gen_tcp:send(Sock, Bytes).

sock_name(Sock) ->
  {ok, {IP, Port}} = inet:sockname(Sock),
  {IP, Port}.

sock_listen_port(LSock) ->
  {ok, Port} = inet:port(LSock),
  Port.


%%
%%  ERROR STRINGS
%%
errstr({error, Reason}) ->
  errstr(Reason);

errstr(echunk) -> "Synchronisation error during chung sending.";
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
errstr(Reason) -> 
  lists:flatten(io_lib:format("Unknown error: ~w", [Reason])).


%% verbose -> ok
%%
%% Prints the string if the Flags list is non-epmty
%%
%% Params: Flags  Flags, possibly including the debug flag
%%         FStr   Format string
%%         Args   Arguments to the format string
%% 
verbose([],FStr,Args) ->
    ok;
verbose(_Flags,FStr,Args) ->
    dprint(FStr,Args).

    
%% debug -> ok
%%
%% Prints the string if the Flags list contains the debug atom.
%%
%% Params: Flags  Flags, possibly including the debug flag
%%         FStr   Format string
%%         Args   Arguments to the format string
%% 
debug([],_FStr,_Args) ->
    ok;
debug(Flags,FStr,Args) ->
    case lists:member(debug,Flags) of
	true ->
	    dprint(FStr,Args);
	_ ->
	    ok
    end.

dprint(FStr,Args) ->
    io:format(FStr,Args).
