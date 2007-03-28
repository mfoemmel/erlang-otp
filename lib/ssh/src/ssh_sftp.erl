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

%%% Description: SFTP protocol front-end

-module(ssh_sftp).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").

-include("ssh.hrl").
-include("ssh_xfer.hrl").

-import(lists, [member/2, map/2, foldl/3, reverse/1, foreach/2]).
%%--------------------------------------------------------------------
%% External exports
%% -export([start/3, start_link/3]).
%% -export([start/2, start_link/2]).
%% -export([start/1, start_link/1]).
-export([connect/1, connect/2, connect/3]).

-export([open_mode/2]).

%% API
-export([open/3, opendir/2, close/2, readdir/2, pread/4, read/3,
	 apread/4, aread/3, pwrite/4, write/3, apwrite/4, awrite/3,
	 position/3, real_path/2, read_file_info/2, get_file_info/2,
	 write_file_info/3, read_link_info/2, read_link/2, make_symlink/3,
	 rename/3, delete/2, make_dir/2, del_dir/2, stop/1, send_window/1,
	 recv_window/1, list_dir/2, read_file/2, write_file/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

%% Other exports
-export([info_to_attr/1, attr_to_info/1]).

-record(state, 
	{
	  xf,
	  rep_buf = <<>>,
	  req_id,
	  req_list = [],  %% {ReqId, Fun}
	  inf   %% list of fileinf
	 }).

-record(fileinf,
	{
	  handle,
	  offset,
	  size,
	  mode
	 }).

-define(FILEOP_TIMEOUT, 60000).

-define(NEXT_REQID(S),
	S#state { req_id = (S#state.req_id + 1) band 16#ffffffff}).

-define(XF(S), S#state.xf).
-define(REQID(S), S#state.req_id).

%%====================================================================
%% External functions
%%====================================================================
open(Pid, File, Mode) ->
    gen_server:call(Pid, {open, false, File, Mode}, ?FILEOP_TIMEOUT).

opendir(Pid, Path) ->
    gen_server:call(Pid, {opendir, false, Path}, ?FILEOP_TIMEOUT).

close(Pid, Handle) ->
    gen_server:call(Pid, {close,false,Handle}, ?FILEOP_TIMEOUT).

readdir(Pid,Handle) ->
    gen_server:call(Pid, {readdir,false,Handle}, ?FILEOP_TIMEOUT).

pread(Pid, Handle, Offset, Len) ->
    gen_server:call(Pid, {pread,false,Handle, Offset, Len}, ?FILEOP_TIMEOUT).

read(Pid, Handle, Len) ->
    gen_server:call(Pid, {read,false,Handle, Len}, ?FILEOP_TIMEOUT).    

apread(Pid, Handle, Offset, Len) ->
    gen_server:call(Pid, {pread,true,Handle, Offset, Len}, ?FILEOP_TIMEOUT).

aread(Pid, Handle, Len) ->
    gen_server:call(Pid, {read,true,Handle, Len}, ?FILEOP_TIMEOUT).    

pwrite(Pid, Handle, Offset, Data) ->
    gen_server:call(Pid, {pwrite,false,Handle,Offset,Data}, ?FILEOP_TIMEOUT).

write(Pid, Handle, Data) ->
    gen_server:call(Pid, {write,false,Handle,Data}, ?FILEOP_TIMEOUT).

apwrite(Pid, Handle, Offset, Data) ->
    gen_server:call(Pid, {pwrite,true,Handle,Offset,Data}, ?FILEOP_TIMEOUT).

awrite(Pid, Handle, Data) ->
    gen_server:call(Pid, {write,true,Handle,Data}, ?FILEOP_TIMEOUT).

position(Pid, Handle, Pos) ->
    gen_server:call(Pid, {position, Handle, Pos}, ?FILEOP_TIMEOUT).

real_path(Pid, Path) ->
    gen_server:call(Pid, {real_path, false, Path}, ?FILEOP_TIMEOUT).

read_file_info(Pid, Name) ->
    gen_server:call(Pid, {read_file_info,false,Name}, ?FILEOP_TIMEOUT).

get_file_info(Pid, Handle) ->
    gen_server:call(Pid, {get_file_info,false,Handle}, ?FILEOP_TIMEOUT).

write_file_info(Pid, Name, Info) ->
    gen_server:call(Pid, {write_file_info,false,Name, Info}, ?FILEOP_TIMEOUT).

read_link_info(Pid, Name) ->
    gen_server:call(Pid, {read_link_info,false,Name}, ?FILEOP_TIMEOUT).

read_link(Pid, LinkName) ->
    case gen_server:call(Pid, {read_link,false,LinkName}, ?FILEOP_TIMEOUT) of
	 {ok, [{Name, _Attrs}]} ->
	    {ok, Name};
	ErrMsg ->
	    ErrMsg
    end.

make_symlink(Pid, Name, Target) ->
    gen_server:call(Pid, {make_symlink,false, Name, Target}, ?FILEOP_TIMEOUT).
 
rename(Pid, FromFile, ToFile) ->
    gen_server:call(Pid, {rename,false,FromFile, ToFile}, ?FILEOP_TIMEOUT).

delete(Pid, Name) ->
    gen_server:call(Pid, {delete,false,Name}, ?FILEOP_TIMEOUT).

make_dir(Pid, Name) ->
    gen_server:call(Pid, {make_dir,false,Name}, ?FILEOP_TIMEOUT).

del_dir(Pid, Name) ->
    gen_server:call(Pid, {del_dir,false,Name}, ?FILEOP_TIMEOUT).


stop(Pid) ->
    gen_server:call(Pid, stop).

send_window(Pid) ->
    gen_server:call(Pid, send_window, ?FILEOP_TIMEOUT).

recv_window(Pid) ->
    gen_server:call(Pid, recv_window, ?FILEOP_TIMEOUT).


list_dir(Pid, Name) ->
    case opendir(Pid, Name) of
	{ok,Handle} ->
	    Res = do_list_dir(Pid, Handle, []),
	    close(Pid, Handle),
	    case Res of
		{ok, List} ->
		    NList = foldl(fun({Nm, _Info},Acc) -> 
					  [Nm|Acc] end, 
				  [], List),
		    {ok,NList};
		Error -> Error
	    end;
	Error ->
	    Error
    end.

do_list_dir(Pid, Handle, Acc) ->
    case readdir(Pid, Handle) of
	{name, Names} ->
	    do_list_dir(Pid, Handle, Acc ++ Names);
	eof ->
	    {ok, Acc};
	Error ->
	    Error
    end.


read_file(Pid, Name) ->
    case open(Pid, Name, [read, binary]) of
	{ok, Handle} ->
	    {ok,{_WindowSz,PacketSz}} = recv_window(Pid),
	    Res = read_file_loop(Pid, Handle, PacketSz, []),
	    close(Pid, Handle),
	    Res;
	Error ->
	    Error
    end.

read_file_loop(Pid, Handle, PacketSz, Acc) ->
    case read(Pid, Handle, PacketSz) of
	{ok, Data}  ->
	    read_file_loop(Pid, Handle, PacketSz, [Data|Acc]);
	eof ->
	    {ok, list_to_binary(reverse(Acc))};
	Error ->
	    Error
    end.

write_file(Pid, Name, List) when list(List) ->
    write_file(Pid, Name, list_to_binary(List));
write_file(Pid, Name, Bin) ->
    case open(Pid, Name, [write, binary]) of
	{ok, Handle} ->
	    {ok,{_Window,Packet}} = send_window(Pid),
	    Res = write_file_loop(Pid, Handle, 0, Bin, size(Bin), Packet),
	    close(Pid, Handle),
	    Res;
	Error ->
	    Error
    end.

write_file_loop(_Pid, _Handle, _Pos, _Bin, 0, _PacketSz) ->
    ok;
write_file_loop(Pid, Handle, Pos, Bin, Remain, PacketSz) ->
    if Remain >= PacketSz ->
	    <<_:Pos/binary, Data:PacketSz/binary, _/binary>> = Bin,
	    case write(Pid, Handle, Data) of
		ok ->
		    write_file_loop(Pid, Handle, 
				    Pos+PacketSz, Bin, Remain-PacketSz,
				    PacketSz);
		Error ->
		    Error
	    end;
       true ->
	    <<_:Pos/binary, Data/binary>> = Bin,
	    write(Pid, Handle, Data)
    end.


%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
%% start_link(CM) when is_pid(CM) ->
%%     gen_server:start_link(?MODULE, [CM], []);
%% start_link(Host) when is_list(Host) ->
%%     gen_server:start_link(?MODULE, [Host, 22, []], []).

%% start_link(Host, Opts) ->
%%     gen_server:start_link(?MODULE, [Host, 22, Opts], []).
    
%% start_link(Host, Port, Opts) ->
%%     gen_server:start_link(?MODULE, [Host, Port, Opts], []).

connect(CM) when is_pid(CM) ->
    gen_server:start(?MODULE, [CM], []);
connect(Host) when is_list(Host) ->
    gen_server:start(?MODULE, [Host, 22, []], []).

connect(Host, Opts) ->
    gen_server:start(?MODULE, [Host, 22, Opts], []).
    
connect(Host, Port, Opts) ->
    gen_server:start(?MODULE, [Host, Port, Opts], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([CM]) ->
    case ssh_xfer:attach(CM, ?FILEOP_TIMEOUT) of
	{ok,Xf,RBuf} ->
	    {ok, #state { req_id = 0, xf = Xf, rep_buf=RBuf,
			  inf = new_inf()}};
	Error ->
	    {stop, Error }
    end;
init([Host,Port,Opts]) ->
    SaveFlag = process_flag(trap_exit, true),
    case ssh_xfer:connect(Host, Port, Opts) of
	{ok, Xf, RBuf} ->
	    process_flag(trap_exit, SaveFlag),
	    {ok, #state { req_id = 0, xf = Xf, rep_buf=RBuf,
			  inf = new_inf()}};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({open,Async,FileName,Mode}, From, State) ->
    XF = State#state.xf,
    {Access,Flags,Attrs} = open_mode(XF#ssh_xfer.vsn, Mode),
    ReqID = State#state.req_id,
    ssh_xfer:open(XF, ReqID, FileName, Access, Flags, Attrs),
    case Async of
	true ->
	    {reply, {async,ReqID},
	     wait_req(ReqID, State,
		      fun({ok,Handle},State1) ->
			      open2(ReqID,FileName,Handle,Mode,Async,
				    From,State1);
			 (Rep,State1) ->
			      async_reply(ReqID, Rep, From, State1)
		      end)};
	false ->
	    {noreply,
	     wait_req(ReqID, State,
		      fun({ok,Handle},State1) ->
			      open2(ReqID,FileName,Handle,Mode,Async,
				    From,State1);
			 (Rep,State1) ->
			      sync_reply(Rep, From, State1)
		      end)}
    end;

handle_call({opendir,Async,Path}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:opendir(?XF(State), ReqID, Path),
    make_reply(ReqID, Async, From, State);

handle_call({readdir,Async,Handle}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:readdir(?XF(State), ReqID, Handle),
    make_reply(ReqID, Async, From, State);

handle_call({close,_Async,Handle}, From, State) ->
    %% wait until all operations on handle are done
    case get_size(Handle, State) of
	undefined ->
	    ReqID = State#state.req_id,
	    ssh_xfer:close(?XF(State), ReqID, Handle),
	    make_reply_post(ReqID, false, From, State,
			    fun(Rep, State1) ->
				    {Rep, erase_handle(Handle, State1)}
			    end);
	_ ->
	    case lseek_position(Handle, cur, State) of
		{ok,_} ->
		    ReqID = State#state.req_id,
		    ssh_xfer:close(?XF(State), ReqID, Handle),
		    make_reply_post(ReqID, false, From, State,
				    fun(Rep, State1) ->
					    {Rep, erase_handle(Handle, State1)}
				    end);
		Error ->
		    {reply, Error, State}
	    end
    end;

handle_call({pread,Async,Handle,At,Length}, From, State) ->
    case lseek_position(Handle, At, State) of
	{ok,Offset} ->
	    ReqID = State#state.req_id,
	    ssh_xfer:read(?XF(State),ReqID,Handle,Offset,Length),
	    %% To get multiple async read to work we must update the offset
	    %% before the operation begins
	    State1 = update_offset(Handle, Offset+Length, State),
	    make_reply_post(ReqID,Async,From,State1,
			    fun({ok,Data}, State2) ->
				    io:format("get_mode ~p\n", [Handle]),
				    case get_mode(Handle, State2) of
					binary -> {{ok,Data}, State2};
					text ->
					    {{ok,binary_to_list(Data)}, State2}
				    end;
			       (Rep, State2) -> 
				    {Rep, State2}
			    end);
	Error ->
	    {reply, Error, State}
    end;

handle_call({read,Async,Handle,Length}, From, State) ->
    case lseek_position(Handle, cur, State) of
	{ok,Offset} ->
	    ReqID = State#state.req_id,
	    ssh_xfer:read(?XF(State),ReqID,Handle,Offset,Length),
	    %% To get multiple async read to work we must update the offset
	    %% before the operation begins
	    State1 = update_offset(Handle, Offset+Length, State),
	    make_reply_post(ReqID,Async,From,State1,
			    fun({ok,Data}, State2) ->
				    case get_mode(Handle, State2) of
					binary -> {{ok,Data}, State2};
					text ->
					    {{ok,binary_to_list(Data)}, State2}
				    end;
			       (Rep, State2) -> {Rep, State2}
			    end);
	Error ->
	    {reply, Error, State}
    end;

handle_call({pwrite,Async,Handle,At,Data0}, From, State) ->
    case lseek_position(Handle, At, State) of
	{ok,Offset} ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    ReqID = State#state.req_id,
	    Size = size(Data),
	    ssh_xfer:write(?XF(State),ReqID,Handle,Offset,Data),
	    State1 = update_size(Handle, Offset+Size, State),
	    make_reply(ReqID, Async, From, State1);
	Error ->
	    {reply, Error, State}
    end;

handle_call({write,Async,Handle,Data0}, From, State) ->
    case lseek_position(Handle, cur, State) of
	{ok,Offset} ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    ReqID = State#state.req_id,
	    Size = size(Data),
	    ssh_xfer:write(?XF(State),ReqID,Handle,Offset,Data),
	    State1 = update_offset(Handle, Offset+Size, State),
	    make_reply(ReqID, Async, From, State1);
	Error ->
	    {reply, Error, State}
    end;

handle_call({position,Handle,At}, _From, State) ->
    %% We could make this auto sync when all request to Handle is done?
    case lseek_position(Handle, At, State) of
	{ok,Offset} ->
	    {reply, {ok, Offset}, update_offset(Handle, Offset, State)};
	Error ->
	    {reply, Error, State}
    end;

handle_call({rename,Async,FromFile,ToFile}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:rename(?XF(State),ReqID,FromFile,ToFile,[overwrite]),
    make_reply(ReqID, Async, From, State);

handle_call({delete,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:remove(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

handle_call({make_dir,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:mkdir(?XF(State), ReqID, Name,
		   #ssh_xfer_attr{ type = directory }),
    make_reply(ReqID, Async, From, State);

handle_call({del_dir,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:rmdir(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

handle_call({real_path,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:realpath(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

handle_call({read_file_info,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:stat(?XF(State), ReqID, Name, all),
    make_reply(ReqID, Async, From, State);

handle_call({get_file_info,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:fstat(?XF(State), ReqID, Name, all),
    make_reply(ReqID, Async, From, State);

handle_call({read_link_info,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:lstat(?XF(State), ReqID, Name, all),
    make_reply(ReqID, Async, From, State);

handle_call({read_link,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:readlink(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

handle_call({make_symlink, Async, Path, TargetPath}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:symlink(?XF(State), ReqID, Path, TargetPath),
    make_reply(ReqID, Async, From, State);

handle_call({write_file_info,Async,Name,Info}, From, State) ->
    ReqID = State#state.req_id,
    A = info_to_attr(Info),
    ssh_xfer:setstat(?XF(State), ReqID, Name, A),
    make_reply(ReqID, Async, From, State);

handle_call(send_window, _From, State) ->
    XF = State#state.xf,
    {reply, ssh_cm:send_window(XF#ssh_xfer.cm, XF#ssh_xfer.channel,
			      ?FILEOP_TIMEOUT), State};

handle_call(recv_window, _From, State) ->
    XF = State#state.xf,
    {reply, ssh_cm:recv_window(XF#ssh_xfer.cm, XF#ssh_xfer.channel,
			       ?FILEOP_TIMEOUT), State};

handle_call(stop, _From, State) ->
    XF = State#state.xf,
    #ssh_xfer{cm = CM, channel = Channel} = XF,
    ssh_cm:close(CM, Channel),
    ssh_cm:stop(CM),
    {stop, normal, ok, State};

handle_call(Call, _From, State) ->    
    {reply, {error, bad_call, Call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({ssh_cm, CM, {data,Channel,Type,Data}}, State) ->
    ssh_cm:adjust_window(CM, Channel, size(Data)),
    if Type == 0 ->
	    Data0 = State#state.rep_buf,
	    State1 = handle_reply(State,CM,Channel,
				  <<Data0/binary,Data/binary>>),
	    {noreply, State1};
       true ->
	    error_logger:format("ssh: STDERR: ~s\n", [binary_to_list(Data)]),
	    {noreply, State}
    end;
handle_info({ssh_cm, CM, {exit_signal,Channel,_SIG,Err,_Lang}},State) ->
    ssh_cm:close(CM, Channel),
    State1 = reply_all(State, CM, Channel, {error, Err}),
    ?dbg(true, "handle_info: exit_signal ~p ~p ~p\n", [_SIG, Err, _Lang]),
    {stop, normal, State1};
handle_info({ssh_cm, CM, {exit_status,Channel,_Status}},State) ->
    ssh_cm:close(CM, Channel),
    State1 = reply_all(State, CM, Channel, eof),
    ?dbg(true, "handle_info: exit_status ~p\n", [_Status]),
    {stop, normal, State1};
handle_info({ssh_cm, CM, {eof, Channel}},State) ->
    State1 = reply_all(State, CM, Channel, eof),
    ?dbg(true, "handle_info: eof \n", []),
    {stop, normal, State1};
handle_info({ssh_cm, CM, {closed, Channel}},State) ->
    State1 = reply_all(State, CM, Channel, {error, closed}),
    ?dbg(true, "handle_info: closed\n", []),
    {stop, normal, State1};
handle_info(_Info, State) ->
    ?dbg(true, "sftp: got info ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
open2(OrigReqID,FileName,Handle,Mode,Async,From,State) ->
    I0 = State#state.inf,
    FileMode = case member(binary, Mode) orelse member(raw, Mode) of
		   true -> binary;
		   false -> text
	       end,
    I1 = add_new_handle(Handle, FileMode, I0),
    State0 = State#state{inf = I1},
    ReqID = State0#state.req_id,
    ssh_xfer:stat(State0#state.xf, ReqID, FileName, [size]),
    case Async of
	true ->
	    wait_req(ReqID, State0,
		     fun({ok,FI},State1) ->
			     Size = FI#file_info.size,
			     State2 = if is_integer(Size) ->
					      put_size(Handle, Size, State1);
					 true ->
					      State1
				      end,
			     async_reply(OrigReqID, {ok,Handle}, From, State2);
			(_, State1) ->
			     async_reply(OrigReqID, {ok,Handle}, From, State1)
		     end);
	false ->
	    wait_req(ReqID, State0,
		     fun({ok,FI},State1) ->
			     Size = FI#file_info.size,
			     State2 = if is_integer(Size) ->
					      put_size(Handle, Size, State1);
					 true ->
					      State1
				      end,
			     sync_reply({ok,Handle}, From, State2);
			(_, State1) ->
			     sync_reply({ok,Handle}, From, State1)
		     end)
    end.


async_reply(ReqID, Reply, _From={To,_}, State) ->
    To ! {async_reply, ReqID, Reply},
    State.


sync_reply(Reply, From, State) ->
    gen:reply(From, Reply),
    State.


reply_all(State, _Cm, _Channel, Reply) ->
    List = State#state.req_list,
    foreach(fun({_ReqID,Fun}) ->
		    catch Fun(Reply,State)
	    end, List),
    State#state { req_list = []}.


make_reply(ReqID, true, From, State) ->
    {reply, {async, ReqID},
     wait_req(ReqID, State,
	      fun(Reply,State1) -> 
		      async_reply(ReqID,Reply,From,State1)
	      end)};
make_reply(ReqID, false, From, State) ->
    {noreply, 
     wait_req(ReqID, State,
	      fun(Reply,State1) -> 
		      sync_reply(Reply, From, State1) 
	      end)}.

make_reply_post(ReqID, true, From, State, PostFun) ->
    {reply, {async, ReqID},
     wait_req(ReqID, State,
	      fun(Reply,State1) ->
		      case catch PostFun(Reply, State1) of
			  {'EXIT',_} ->
			      async_reply(ReqID,Reply, From, State1);
			  {Reply1, State2} ->
			      async_reply(ReqID,Reply1, From, State2)
		      end
	      end)};
make_reply_post(ReqID, false, From, State, PostFun) ->
    {noreply,
     wait_req(ReqID, State,
	      fun(Reply,State1) ->
		      case catch PostFun(Reply, State1) of
			  {'EXIT',_} ->
			      sync_reply(Reply, From, State1);
			  {Reply1, State2} ->
			      sync_reply(Reply1, From, State2)
		      end
	      end)}.


wait_req(ReqID, State, Fun) ->
    List = [{ReqID,Fun} | State#state.req_list],
    ID = (State#state.req_id + 1) band 16#ffffffff,
    State#state { req_list = List, req_id = ID }.

handle_reply(State, Cm, Channel, Data) ->
    case Data of
	<<?UINT32(Len), RData:Len/binary, RBuf/binary>> ->
	    case catch ssh_xfer:xf_reply(?XF(State), RData) of
		{'EXIT', _Reason} ->
		    ?dbg(true, "handle_reply: error ~p\n", [_Reason]),
		    handle_reply(State, Cm, Channel, RBuf);
		XfReply={_, ReqID, _} ->
		    State1 = handle_req_reply(State, ReqID, XfReply),
		    handle_reply(State1, Cm, Channel, RBuf)
	    end;
	RBuf ->
	    State#state { rep_buf = RBuf }
    end.

handle_req_reply(State, ReqID, XfReply) ->
    case lists:keysearch(ReqID, 1, State#state.req_list) of
	false ->
	    error_logger:format("handle_req_reply: req_id=~p not found\n",
				[ReqID]),
	    State;
	{value,{_,Fun}} ->
	    List = lists:keydelete(ReqID, 1, State#state.req_list),
	    State1 = State#state { req_list = List },
	    case catch Fun(xreply(XfReply),State1) of
		{'EXIT', _} ->  State1;
		State2 -> State2
	    end
    end.

xreply({handle,_,H}) -> {ok, H};
xreply({data,_,Data}) -> {ok, Data};
xreply({name,_,Names}) -> {ok, Names};
xreply({attrs, _, A}) -> {ok, attr_to_info(A)};
xreply({extended_reply,_,X}) -> {ok, X};
xreply({status,_,{ok, _Err, _Lang, _Rep}}) -> ok;
xreply({status,_,{eof, _Err, _Lang, _Rep}}) -> eof;
xreply({status,_,{Stat, _Err, _Lang, _Rep}}) -> {error, Stat};
xreply({Code, _, Reply}) -> {Code, Reply}.


%% convert: file_info -> ssh_xfer_attr
info_to_attr(I) when is_record(I, file_info) ->
    #ssh_xfer_attr { permissions = I#file_info.mode,
		     size = I#file_info.size,
		     type = I#file_info.type,
		     owner = I#file_info.uid,
		     group = I#file_info.gid,
		     atime = datetime_to_unix(I#file_info.atime),
		     mtime = datetime_to_unix(I#file_info.mtime),
		     createtime = datetime_to_unix(I#file_info.ctime)}.

%% convert: ssh_xfer_attr -> file_info
attr_to_info(A) when is_record(A, ssh_xfer_attr) ->
    #file_info{
      size   = A#ssh_xfer_attr.size,
      type   = A#ssh_xfer_attr.type,
      access = read_write, %% FIXME: read/write/read_write/none
      atime  = unix_to_datetime(A#ssh_xfer_attr.atime),
      mtime  = unix_to_datetime(A#ssh_xfer_attr.mtime),
      ctime  = unix_to_datetime(A#ssh_xfer_attr.createtime),
      mode   = A#ssh_xfer_attr.permissions,
      links  = 1,
      major_device = 0,
      minor_device = 0,
      inode  = 0,
      uid    = A#ssh_xfer_attr.owner,
      gid    = A#ssh_xfer_attr.group}.

unix_to_datetime(undefined) ->
    undefined;
unix_to_datetime(Sec) ->
    calendar:gregorian_seconds_to_datetime(Sec + 62167219200).

datetime_to_unix(undefined) ->
    undefined;
datetime_to_unix(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.


open_mode(Vsn,Modes) when Vsn >= 5 ->
    open_mode5(Modes);
open_mode(_Vsn, Modes) ->
    open_mode3(Modes).

open_mode5(Modes) ->
    A = #ssh_xfer_attr{type = regular},
    {Fl, Ac} = case {member(write, Modes),
		     member(read, Modes),
		     member(append, Modes)} of
		   {_, _, true} ->
		       {[append_data],
			[read_attributes,
			 append_data, write_attributes]};
		   {true, false, false} ->
		       {[create_truncate],
			[write_data, write_attributes]};
		   {true, true, _} ->
		       {[open_or_create],
			[read_data, read_attributes,
			 write_data, write_attributes]};
		   {false, true, _} ->
		       {[open_existing],
			[read_data, read_attributes]}
	       end,
    {Ac, Fl, A}.

open_mode3(Modes) ->
    A = #ssh_xfer_attr{type = regular},
    Fl = case {member(write, Modes),
	       member(read, Modes),
	       member(append, Modes)} of
	     {_, _, true} ->
		 [append];
	     {true, false, false} ->
		 [write, creat, trunc];
	     {true, true, _} ->
		 [read, write];
	     {false, true, _} ->
		 [read]
	 end,
    {[], Fl, A}.

%% open_mode(3, Mode, Access, Flags, Attrs) ->
%%     open_mode(3,Mode,[read_data,read_attributes|Access], [read|Flags], Attrs);
%% open_mode(5, [read|Mode], Access, Flags, Attrs) ->
%%     Flags1 =
%% 	case member(write, Mode) orelse member(truncate_existing, Flags) of
%% 	    false -> [open_existing | Flags];
%% 	    true  -> Flags
%% 	end,
%%     open_mode(5, Mode, [read_data,read_attributes|Access], Flags1, Attrs);
%% open_mode(3, [write|Mode], Access, Flags, Attrs) ->
%%     open_mode(3, Mode, [write_data,write_attributes|Access], 
%% 	      [write,creat,trunc|Flags], Attrs);
%% open_mode(5, [write|Mode], Access, Flags, Attrs) ->
%%     Flags1 =
%% 	case member(read, Mode) orelse member(existing, Flags) of
%% 	    true -> Flags;
%% 	    false -> [create_truncate|Flags]
%% 	end,
%%     open_mode(5, Mode, [write_data,write_attributes|Access], 
%% 	      Flags1, Attrs);
%% open_mode(3, [append|Mode],Access, Flags, Attrs) ->
%%     open_mode(3, Mode, [write_data,write_attributes|Access], 
%% 	      [write,creat,trunc,append|Flags], Attrs);
%% open_mode(5, [append|Mode],Access, Flags, Attrs) ->
%%     open_mode(5, Mode, [write_data,write_attributes,append_data|Access], 
%% 	      [open_or_create,write_data,write_attributes,append_data|Flags],
%% 	      Attrs);
%% open_mode(Vsn, [raw|Mode],Access, Flags, Attrs) ->
%%     open_mode(Vsn, Mode, Access, Flags, Attrs);
%% open_mode(Vsn, [binary|Mode],Access, Flags, Attrs) ->
%%     open_mode(Vsn, Mode, Access, Flags, Attrs);
%% open_mode(_, [], Access, Flags, Attrs) ->
%%     {Access, Flags, Attrs}.



%% accessors for inf dict
new_inf() -> dict:new().

add_new_handle(Handle, FileMode, Inf) ->
    dict:store(Handle, #fileinf{offset=0, size=0, mode=FileMode}, Inf).

update_size(Handle, NewSize, State) ->
    OldSize = get_size(Handle, State),
    if NewSize > OldSize ->
	    put_size(Handle, NewSize, State);
       true ->
	    State
    end.

%% set_offset(Handle, NewOffset) ->
%%     put({offset,Handle}, NewOffset).

update_offset(Handle, NewOffset, State0) ->
    State1 = put_offset(Handle, NewOffset, State0),
    update_size(Handle, NewOffset, State1).



%% access size and offset for handle
put_size(Handle, Size, State) ->
    Inf0 = State#state.inf,
    case dict:find(Handle, Inf0) of
	{ok, FI} ->
	    State#state{inf=dict:store(Handle, FI#fileinf{size=Size}, Inf0)};
	_ ->
	    State#state{inf=dict:store(Handle, #fileinf{size=Size,offset=0},
				       Inf0)}
    end.

put_offset(Handle, Offset, State) ->
    Inf0 = State#state.inf,
    case dict:find(Handle, Inf0) of
	{ok, FI} ->
	    State#state{inf=dict:store(Handle, FI#fileinf{offset=Offset},
				       Inf0)};
	_ ->
	    State#state{inf=dict:store(Handle, #fileinf{size=Offset,
							offset=Offset}, Inf0)}
    end.

get_size(Handle, State) ->
    case dict:find(Handle, State#state.inf) of
	{ok, FI} ->
	    FI#fileinf.size;
	_ ->
	    undefined
    end.

%% get_offset(Handle, State) ->
%%     {ok, FI} = dict:find(Handle, State#state.inf),
%%     FI#fileinf.offset.

get_mode(Handle, State) ->
    io:format("Mode\n"),
    case dict:find(Handle, State#state.inf) of
	{ok, FI} ->
	    io:format("Mode for ~p - ~p\n", [Handle, FI#fileinf.mode]),
	    FI#fileinf.mode;
	_ ->
	    io:format("undefined mode for ~p\n", [Handle]),
	    undefined
    end.

erase_handle(Handle, State) ->
    FI = dict:erase(Handle, State#state.inf),
    State#state{inf = FI}.

%%
%% Caluclate a integer offset
%%
lseek_position(Handle, Pos, State) ->
    case dict:find(Handle, State#state.inf) of
	{ok, #fileinf{offset=O, size=S}} ->
	    lseek_pos(Pos, O, S);
	_ ->
	    {error, einval}
    end.

lseek_pos(_Pos, undefined, _) ->
    {error, einval};
lseek_pos(Pos, _CurOffset, _CurSize)
  when integer(Pos), 0 =< Pos, Pos < ?SSH_FILEXFER_LARGEFILESIZE ->
    {ok,Pos};
lseek_pos(bof, _CurOffset, _CurSize) ->
    {ok,0};
lseek_pos(cur, CurOffset, _CurSize) ->
    {ok,CurOffset};
lseek_pos(eof, _CurOffset, CurSize) ->
    {ok,CurSize};
lseek_pos({bof, Offset}, _CurOffset, _CurSize)
  when integer(Offset), 0 =< Offset, Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    {ok, Offset};
lseek_pos({cur, Offset}, CurOffset, _CurSize)
  when integer(Offset), -(?SSH_FILEXFER_LARGEFILESIZE) =< Offset, 
       Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    NewOffset = CurOffset + Offset,
    if NewOffset < 0 ->
	    {ok, 0};
       true ->
	    {ok, NewOffset}
    end;
lseek_pos({eof, Offset}, _CurOffset, CurSize) 
  when integer(Offset), -(?SSH_FILEXFER_LARGEFILESIZE) =< Offset, 
       Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    NewOffset = CurSize + Offset,
    if NewOffset < 0 ->
	    {ok, 0};
       true ->
	    {ok, NewOffset}
    end;
lseek_pos(_, _, _) ->
    {error, einval}.

