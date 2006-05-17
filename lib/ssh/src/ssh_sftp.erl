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
	  req_list = []  %% {ReqId, Fun}
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

read_link(Pid, Name) ->
    gen_server:call(Pid, {read_link,false,Name}, ?FILEOP_TIMEOUT).

make_symlink(Pid, Old, New) ->
    gen_server:call(Pid, {make_symlink,false,Old,New}, ?FILEOP_TIMEOUT).    

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
	    {ok, #state { req_id = 0, xf = Xf, rep_buf=RBuf }};
	Error ->
	    {stop, Error }
    end;
init([Host,Port,Opts]) ->
    case ssh_xfer:connect(Host, Port, Opts) of
	{ok, Xf, RBuf} ->
	    {ok, #state { req_id = 0, xf = Xf, rep_buf=RBuf }};
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
handle_call({open,Async,FileName,Mode}, From, St) ->
    XF = St#state.xf,
    {Access,Flags,Attrs} = open_mode(XF#ssh_xfer.vsn, Mode),
    ReqID = St#state.req_id,
    ssh_xfer:open(XF, ReqID, FileName, Access, Flags, Attrs),
    case Async of
	true ->
	    {reply, {async,ReqID},
	     wait_req(ReqID, St,
		      fun({ok,Handle},St1) ->
			      open2(ReqID,FileName,Handle,Mode,Async,From,St1);
			 (Rep,St1) ->
			      async_reply(ReqID, Rep, From, St1)
		      end)};
	false ->
	    {noreply,
	     wait_req(ReqID, St,
		      fun({ok,Handle},St1) ->
			      open2(ReqID,FileName,Handle,Mode,Async,From,St1);
			 (Rep,St1) ->
			      sync_reply(Rep, From, St1)
		      end)}
    end;

handle_call({opendir,Async,Path}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:opendir(?XF(St), ReqID, Path),
    make_reply(ReqID, Async, From, St);

handle_call({readdir,Async,Handle}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:readdir(?XF(St), ReqID, Handle),
    make_reply(ReqID, Async, From, St);

handle_call({close,_Async,Handle}, From, St) ->
    %% wait until all operations on handle are done
    case get({size, Handle}) of
	undefined ->
	    ReqID = St#state.req_id,
	    ssh_xfer:close(?XF(St), ReqID, Handle),
	    make_reply_post(ReqID, false, From, St,
			    fun(Rep) ->
				    erase({offset,Handle}),
				    erase({size,Handle}),
				    erase({mode,Handle}),
				    Rep
			    end);
	_ ->
	    case lseek_position(Handle, cur) of
		{ok,_} ->
		    ReqID = St#state.req_id,
		    ssh_xfer:close(?XF(St), ReqID, Handle),
		    make_reply_post(ReqID, false, From, St,
				    fun(Rep) ->
					    erase({offset,Handle}),
					    erase({size,Handle}),
					    erase({mode,Handle}),
				    Rep
				    end);
		Error ->
		    {reply,Error, St}
	    end
    end;

handle_call({pread,Async,Handle,At,Length}, From, St) ->
    case lseek_position(Handle, At) of
	{ok,Offset} ->
	    ReqID = St#state.req_id,
	    ssh_xfer:read(?XF(St),ReqID,Handle,Offset,Length),
	    %% To get multiple async read to work we must update the offset
	    %% before the operation begins
	    update_offset(Handle, Offset+Length),
	    make_reply_post(ReqID,Async,From,St,
			    fun({ok,Data}) ->
				    case get({mode,Handle}) of
					binary -> {ok,Data};
					text -> {ok,binary_to_list(Data)}
				    end;
			       (Rep) -> 
				    Rep
			    end);
	Error ->
	    {reply, Error, St}
    end;

handle_call({read,Async,Handle,Length}, From, St) ->
    case lseek_position(Handle,cur) of
	{ok,Offset} ->
	    ReqID = St#state.req_id,
	    ssh_xfer:read(?XF(St),ReqID,Handle,Offset,Length),
	    %% To get multiple async read to work we must update the offset
	    %% before the operation begins
	    update_offset(Handle, Offset+Length),
	    make_reply_post(ReqID,Async,From,St,
			    fun({ok,Data}) ->
				    case get({mode,Handle}) of
					binary -> {ok,Data};
					text -> {ok,binary_to_list(Data)}
				    end;
			       (Rep) -> Rep
			    end);
	Error ->
	    {reply, Error, St}
    end;

handle_call({pwrite,Async,Handle,At,Data0}, From, St) ->
    case lseek_position(Handle, At) of
	{ok,Offset} ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    ReqID = St#state.req_id,
	    Size = size(Data),
	    ssh_xfer:write(?XF(St),ReqID,Handle,Offset,Data),
	    update_size(Handle, Offset+Size),
	    make_reply(ReqID, Async, From, St);
	Error ->
	    {reply, Error, St}
    end;

handle_call({write,Async,Handle,Data0}, From, St) ->
    case lseek_position(Handle, cur) of
	{ok,Offset} ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    ReqID = St#state.req_id,
	    Size = size(Data),
	    ssh_xfer:write(?XF(St),ReqID,Handle,Offset,Data),
	    update_offset(Handle, Offset+Size),
	    make_reply(ReqID, Async, From, St);
	Error ->
	    {reply, Error, St}
    end;

handle_call({position,Handle,At}, _From, St) ->
    %% We could make this auto sync when all request to Handle is done?
    case lseek_position(Handle, At) of
	{ok,Offset} ->
	    update_offset(Handle, Offset),
	    {reply, {ok, Offset}, St};
	Error ->
	    {reply, Error, St}
    end;

handle_call({rename,Async,FromFile,ToFile}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:rename(?XF(St),ReqID,FromFile,ToFile,[overwrite]),
    make_reply(ReqID, Async, From, St);

handle_call({delete,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:remove(?XF(St), ReqID, Name),
    make_reply(ReqID, Async, From, St);

handle_call({make_dir,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:mkdir(?XF(St), ReqID, Name,
		   #ssh_xfer_attr{ type = directory }),
    make_reply(ReqID, Async, From, St);

handle_call({del_dir,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:rmdir(?XF(St), ReqID, Name),
    make_reply(ReqID, Async, From, St);

handle_call({real_path,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:realpath(?XF(St), ReqID, Name),
    make_reply(ReqID, Async, From, St);

handle_call({read_file_info,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:stat(?XF(St), ReqID, Name, all),
    make_reply(ReqID, Async, From, St);

handle_call({get_file_info,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:fstat(?XF(St), ReqID, Name, all),
    make_reply(ReqID, Async, From, St);

handle_call({read_link_info,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:lstat(?XF(St), ReqID, Name, all),
    make_reply(ReqID, Async, From, St);

handle_call({read_link,Async,Name}, From, St) ->
    ReqID = St#state.req_id,
    ssh_xfer:readlink(?XF(St), ReqID, Name),
    make_reply(ReqID, Async, From, St);

handle_call({write_file_info,Async,Name,Info}, From, St) ->
    ReqID = St#state.req_id,
    A = info_to_attr(Info),
    ssh_xfer:setstat(?XF(St), ReqID, Name, A),
    make_reply(ReqID, Async, From, St);

handle_call(send_window, _From, St) ->
    XF = St#state.xf,
    {reply, ssh_cm:send_window(XF#ssh_xfer.cm, XF#ssh_xfer.channel,
			      ?FILEOP_TIMEOUT), St};

handle_call(recv_window, _From, St) ->
    XF = St#state.xf,
    {reply, ssh_cm:recv_window(XF#ssh_xfer.cm, XF#ssh_xfer.channel,
			       ?FILEOP_TIMEOUT), St};

handle_call(stop, _From, St) ->
    XF = St#state.xf,
    #ssh_xfer{cm = CM, channel = Channel} = XF,
    ssh_cm:close(CM, Channel),
    ssh_cm:stop(CM),
    {stop, normal, ok, St};

handle_call(Call, _From, St) ->    
    {reply, {error, bad_call, Call}, St}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, St) ->
    {noreply, St}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({ssh_cm, CM, {data,Channel,Type,Data}}, St) ->
    ssh_cm:adjust_window(CM, Channel, size(Data)),
    if Type == 0 ->
	    Data0 = St#state.rep_buf,
	    St1 = handle_reply(St,CM,Channel,<<Data0/binary,Data/binary>>),
	    {noreply, St1};
       true ->
	    error_logger:format("ssh: STDERR: ~s\n", [binary_to_list(Data)]),
	    {noreply, St}
    end;
handle_info({ssh_cm, CM, {exit_signal,Channel,_SIG,Err,_Lang}},St) ->
    ssh_cm:close(CM, Channel),
    St1 = reply_all(St, CM, Channel, {error, Err}),
    ?dbg(true, "handle_info: exit_signal ~p ~p ~p\n", [_SIG, Err, _Lang]),
    {stop, normal, St1};
handle_info({ssh_cm, CM, {exit_status,Channel,_Status}},St) ->
    ssh_cm:close(CM, Channel),
    St1 = reply_all(St, CM, Channel, eof),
    ?dbg(true, "handle_info: exit_status ~p\n", [_Status]),
    {stop, normal, St1};
handle_info({ssh_cm, CM, {eof, Channel}},St) ->
    St1 = reply_all(St, CM, Channel, eof),
    ?dbg(true, "handle_info: eof \n", []),
    {stop, normal, St1};
handle_info({ssh_cm, CM, {closed, Channel}},St) ->
    St1 = reply_all(St, CM, Channel, {error, closed}),
    ?dbg(true, "handle_info: closed\n", []),
    {stop, normal, St1};
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
open2(OrigReqID,FileName,Handle,Mode,Async,From,St) ->
    put({offset,Handle}, 0),
    put({size,Handle}, 0),
    case member(binary, Mode) orelse member(raw, Mode) of
	true -> put({mode,Handle}, binary);
	false -> put({mode,Handle}, text)
    end,
    ReqID = St#state.req_id,
    ssh_xfer:stat(St#state.xf, ReqID, FileName, [size]),
    case Async of
	true ->
	    wait_req(ReqID, St,
		     fun({ok,FI},St1) ->
			     if integer(FI#file_info.size) ->
				     put({size,Handle}, FI#file_info.size);
				true ->
				     ok
			     end,
			     async_reply(OrigReqID, {ok,Handle}, From, St1);
			(_, St1) ->
			     async_reply(OrigReqID, {ok,Handle}, From, St1)
		     end);
	false ->
	    wait_req(ReqID, St,
		     fun({ok,FI},St1) ->
			     if is_integer(FI#file_info.size) ->
				     put({size,Handle}, FI#file_info.size);
				true ->
				     ok
			     end,
			     sync_reply({ok,Handle}, From,St1);
			 (_, St1) ->
			     sync_reply({ok,Handle}, From,St1)
		     end)
    end.



async_reply(ReqID, Reply, _From={To,_}, St) ->
    To ! {async_reply, ReqID, Reply},
    St.


sync_reply(Reply, From, St) ->
    gen:reply(From, Reply),
    St.


reply_all(St, _Cm, _Channel, Reply) ->
    List = St#state.req_list,
    foreach(fun({_ReqID,Fun}) ->
		    catch Fun(Reply,St)
	    end, List),
    St#state { req_list = []}.


make_reply(ReqID, true, From, St) ->
    {reply, {async, ReqID},
     wait_req(ReqID, St,
	      fun(Reply,St1) -> 
		      async_reply(ReqID,Reply,From,St1)
	      end)};
make_reply(ReqID, false, From, St) ->
    {noreply, 
     wait_req(ReqID, St,
	      fun(Reply,St1) -> 
		      sync_reply(Reply, From, St1) 
	      end)}.

make_reply_post(ReqID, true, From, St, PostFun) ->
    {reply, {async, ReqID},
     wait_req(ReqID, St,
	      fun(Reply,St1) ->
		      case catch PostFun(Reply) of
			  {'EXIT',_} ->
			      async_reply(ReqID,Reply, From, St1);
			  Reply1 ->
			      async_reply(ReqID,Reply1, From, St1)
		      end
	      end)};
make_reply_post(ReqID, false, From, St, PostFun) ->
    {noreply,
     wait_req(ReqID, St,
	      fun(Reply,St1) ->
		      case catch PostFun(Reply) of
			  {'EXIT',_} ->
			      sync_reply(Reply, From, St1);
			  Reply1 ->
			      sync_reply(Reply1, From, St1)
		      end
	      end)}.


wait_req(ReqID, St, Fun) ->
    List = [{ReqID,Fun} | St#state.req_list],
    ID = (St#state.req_id + 1) band 16#ffffffff,
    St#state { req_list = List, req_id = ID }.

handle_reply(St, Cm, Channel, Data) ->
    case Data of
	<<?UINT32(Len), RData:Len/binary, RBuf/binary>> ->
	    case catch ssh_xfer:xf_reply(?XF(St), RData) of
		{'EXIT', _Reason} ->
		    ?dbg(true, "handle_reply: error ~p\n", [_Reason]),
		    handle_reply(St, Cm, Channel, RBuf);
		XfReply={_, ReqID, _} ->
		    St1 = handle_req_reply(St, ReqID, XfReply),
		    handle_reply(St1, Cm, Channel, RBuf)
	    end;
	RBuf ->
	    St#state { rep_buf = RBuf }
    end.

handle_req_reply(St, ReqID, XfReply) ->
    case lists:keysearch(ReqID, 1, St#state.req_list) of
	false ->
	    error_logger:format("handle_req_reply: req_id=~p not found\n",
				[ReqID]),
	    St;
	{value,{_,Fun}} ->
	    List = lists:keydelete(ReqID, 1, St#state.req_list),
	    St1 = St#state { req_list = List },
	    case catch Fun(xreply(XfReply),St1) of
		{'EXIT', _} ->  St1;
		St2 -> St2
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


update_size(Handle, NewSize) ->
    OldSize = get({size,Handle}),
    if NewSize > OldSize ->
	    put({size,Handle}, NewSize);
       true ->
	    ok
    end.

%% set_offset(Handle, NewOffset) ->
%%     put({offset,Handle}, NewOffset).

update_offset(Handle, NewOffset) ->
    put({offset,Handle}, NewOffset),
    update_size(Handle, NewOffset).

%%
%% Caluclate a integer offset
%%
lseek_position(Handle, Pos) ->
    lseek_pos(Pos, get({offset,Handle}), get({size,Handle})).

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

