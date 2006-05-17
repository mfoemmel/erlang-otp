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

%%% Description: SFTP server daemon

-module(ssh_sftpd).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").

-include("ssh.hrl").
-include("ssh_xfer.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([listen/1, listen/2, listen/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  xf,   			% [{channel,ssh_xfer states}...]
	  cwd,				% current dir (on first connect)
	  remote_channel,		% remote channel
	  handles			% list of open handles
	  %% handle is either {<int>, directory, {Path, unread|eof}} or
	  %% {<int>, file, {Path, IoDevice}}
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: listen() -> Pid | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
listen(Port) ->
    listen(any, Port, []).
listen(Port, Options) ->
    listen(any, Port, Options).
listen(Addr, Port, Options) ->
    ssh_cm:listen(fun() ->
			  {ok,Pid} = 
			      gen_server:start_link(?MODULE, [Options], []),
			  Pid
		  end, Addr, Port, Options).

%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok
%% Description: Stops the listener
%%--------------------------------------------------------------------
stop(Pid) ->
    ssh_cli:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Options]) ->
    {ok, D} = file:get_cwd(),
    CWD = proplists:get_value(cwd, Options, D),
    State = #state{cwd = CWD, handles = []},
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({ssh_cm, CM, {open, Channel, RemoteChannel, _}}, State) ->
    XF = #ssh_xfer{vsn = 5, ext = [], cm = CM, channel = Channel},
    State1 = State#state{xf = XF, remote_channel = RemoteChannel},
    {noreply, State1};
handle_info({ssh_cm, CM, {data, Channel, Type, Data}}, State) ->
    ssh_cm:adjust_window(CM, Channel, size(Data)),
    State1 = handle_data(Type, Data, State),
    {noreply, State1};
handle_info({ssh_cm, CM, {subsystem, _Channel, WantsReply, "sftp"}}, State) ->
    CM = (State#state.xf)#ssh_xfer.cm, 		% hmmm going through xf...
    case WantsReply of
	true -> CM ! {ssh_cm, self(), {success, State#state.remote_channel}}
    end,
    {noreply, State};
handle_info(_Info, State) ->
    ?dbg(true, "handle_info: Info=~p State=~p\n", [_Info, State]),
    {noreply, State}.

handle_data(0, <<?UINT32(_Len), Op, ?UINT32(ReqId), Data/binary>>, State) ->
    ?dbg(true, "handle_op: Op=~p ReqId=~p Data=~p\n", [Op, ReqId, Data]),
    handle_op(Op, ReqId, Data, State);
handle_data(_, Data, State) ->
    error_logger:format("ssh: STDERR: ~s\n", [binary_to_list(Data)]),
    State.

handle_op(?SSH_FXP_INIT, Version, <<>>, State) ->
    XF = State#state.xf,
    Vsn = lists:min([XF#ssh_xfer.vsn, Version]),
    XF1 = XF#ssh_xfer{vsn = Vsn},
    ssh_xfer:xf_send_reply(XF1, ?SSH_FXP_VERSION, <<?UINT32(Vsn)>>),
    State#state{xf = XF1};
handle_op(?SSH_FXP_REALPATH, ReqId,
	  <<?UINT32(Rlen), RPath:Rlen/binary>>,
	  State) ->
    RelPath = binary_to_list(RPath),
    AbsPath = relate_file_name(RelPath, State),
    ?dbg(true, "handle_op ?SSH_FXP_REALPATH: RelPath=~p AbsPath=~p\n",
	 [RelPath, AbsPath]),
    XF = State#state.xf,
    Attr = #ssh_xfer_attr{type=directory},
    ssh_xfer:xf_send_name(XF, ReqId, AbsPath, Attr),
    State;
handle_op(?SSH_FXP_OPENDIR, ReqId,
	 <<?UINT32(RLen), RPath:RLen/binary>>,
	  State) ->
    RelPath = binary_to_list(RPath),
    AbsPath = relate_file_name(RelPath, State),
    XF = State#state.xf,
    case filelib:is_dir(AbsPath) of
	false ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_NOT_A_DIRECTORY,
				    "Not a directory"),
	    State;
	true ->
	    add_handle(State, XF, ReqId, directory, {RelPath,unread})
    end;
handle_op(?SSH_FXP_READDIR, ReqId,
	  <<?UINT32(HLen), BinHandle:HLen/binary>>,
	  State) ->
    XF = State#state.xf,
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, directory, {_RelPath, eof}} ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_EOF),
	    State;
	{Handle, directory, {RelPath, _}} ->
	    read_dir(State, XF, ReqId, Handle, RelPath);
	_ ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_CLOSE,  ReqId,
	  <<?UINT32(HLen), BinHandle:HLen/binary>>,
	  State) ->
    Handles = State#state.handles, 
    XF = State#state.xf,
    case get_handle(Handles, BinHandle) of
	{Handle, Type, T} ->
	    case Type of
		file ->
		    close_our_file(T);
		_ ->
		    ok
	    end,
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_OK),
	    State#state{handles = lists:keydelete(Handle, 1, Handles)};
	_ ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_LSTAT, ReqId, Data, State) ->
    stat((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State, read_link_info);
handle_op(?SSH_FXP_STAT, ReqId, Data, State) ->
    stat((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State, read_file_info);
handle_op(?SSH_FXP_FSTAT, ReqId, Data, State) ->
    fstat((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State);
handle_op(?SSH_FXP_OPEN, ReqId, Data, State) ->
    open((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State);
handle_op(?SSH_FXP_READ, ReqId, <<?UINT32(HLen), BinHandle:HLen/binary,
				 ?UINT64(Offset), ?UINT32(Len)>>,
	  State) ->
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, file, {_Path, IoDevice}} ->
	    read_file(ReqId, IoDevice, Offset, Len, State);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId, ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_WRITE, ReqId,
	  <<?UINT32(HLen), BinHandle:HLen/binary, ?UINT64(Offset),
	   ?UINT32(Len), Data:Len/binary>>, State) ->
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, file, {_Path, IoDevice}} ->
	    write_file(ReqId, IoDevice, Offset, Data, State);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId, ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_READLINK, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, State) ->
    RelPath = binary_to_list(BPath),
    AbsPath = relate_file_name(RelPath, State),
    case file:read_link(AbsPath) of
	{ok, NewPath} ->
	    ssh_xfer:xf_send_name(State#state.xf, ReqId, NewPath,
				  #ssh_xfer_attr{type=regular});
	{error, Error} ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ssh_xfer:encode_erlang_status(Error))
    end,
    State;
handle_op(?SSH_FXP_SETSTAT, ReqId, <<?UINT32(PLen), BPath:PLen/binary,
				    Attr/binary>>, State) ->
    Path = relate_file_name(BPath, State),
    Status = set_stat(Attr, Path, State),
    send_status(Status, ReqId, State);
handle_op(?SSH_FXP_MKDIR, ReqId, <<?UINT32(PLen), BPath:PLen/binary,
				  Attr/binary>>, State) ->
    Path = relate_file_name(BPath, State),
    case file:make_dir(Path) of
	ok ->
	    set_stat(Attr, Path, State),
	    send_status(ok, ReqId, State);
	{error, Error} ->
	    send_status({error, Error}, ReqId, State)
    end;
handle_op(?SSH_FXP_FSETSTAT, ReqId, <<?UINT32(HLen), BinHandle:HLen/binary, 
				     Attr/binary>>, State) ->
    Handles = State#state.handles,
    case get_handle(Handles, BinHandle) of
	{_Handle, _Type, {Path,_}} ->
	    Status = set_stat(Attr, Path, State),
	    send_status(Status, ReqId, State);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_REMOVE, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, State) ->
    Path = relate_file_name(BPath, State),
    Status = file:delete(Path),
    send_status(Status, ReqId, State);    
handle_op(?SSH_FXP_RMDIR, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, State) ->
    Path = relate_file_name(BPath, State),
    Status = file:del_dir(Path),
    send_status(Status, ReqId, State);
handle_op(?SSH_FXP_RENAME, ReqId,
	  <<?UINT32(PLen), BPath:PLen/binary, ?UINT32(PLen2), BPath2:PLen2/binary,
	   ?UINT32(Flags)>>, State) ->
    Path = relate_file_name(BPath, State),
    Path2 = relate_file_name(BPath2, State),
    case Flags band ?SSH_FXP_RENAME_ATOMIC of
	0 ->
	    case Flags band ?SSH_FXP_RENAME_OVERWRITE of
		0 ->
		    case file:read_link_info(Path2) of
			{ok, _Info} ->
			    ssh_xfer:xf_send_status(State#state.xf, ReqId,
						    ?SSH_FX_FILE_ALREADY_EXISTS),
			    State;
			_ ->
			    rename(Path, Path2, ReqId, State)
		    end;
		_ ->
		    rename(Path, Path2, ReqId, State)
	    end;
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ?SSH_FX_OP_UNSUPPORTED),
	    State
    end;
handle_op(?SSH_FXP_SYMLINK, ReqId,
	  <<?UINT32(PLen), BPath:PLen/binary, ?UINT32(PLen2),
	   BPath2:PLen2/binary>>, State) ->
    Path = relate_file_name(BPath, State),
    Path2 = relate_file_name(BPath2, State),
    Status = file:make_symlink(Path2, Path),
    send_status(Status, ReqId, State).

%% TBD!
 %% -define(SSH_FXP_SETSTAT,	9).
 %% -define(SSH_FXP_MKDIR,	14).
 %% -define(SSH_FXP_FSETSTAT,	10).
 %% -define(SSH_FXP_REMOVE,		13).
 %% -define(SSH_FXP_RMDIR,		15).
 %% -define(SSH_FXP_RENAME,		18).
 %% -define(SSH_FXP_SYMLINK,	20).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

new_handle([], H) ->
    H;
new_handle([{N, _} | Rest], H) when N > H ->
    new_handle(Rest, N+1);
new_handle([_ | Rest], H) ->
    new_handle(Rest, H).

add_handle(State, XF, ReqId, Type, DirFileTuple) ->
    Handles = State#state.handles,
    Handle = new_handle(Handles, 0),
    ssh_xfer:xf_send_handle(XF, ReqId, integer_to_list(Handle)),
    State#state{handles = [{Handle, Type, DirFileTuple} | Handles]}.
    
get_handle(Handles, BinHandle) ->
    case catch list_to_integer(binary_to_list(BinHandle)) of
	I when integer(I) ->
	    case lists:keysearch(I, 1, Handles) of
		{value, T} -> T;
		false -> error
	    end;
	_ ->
	    error
    end.

%%% read_dir/5: read directory, send names, and return new state
read_dir(State, XF, ReqId, Handle, RelPath) ->
    AbsPath = relate_file_name(RelPath, State),
    ?dbg(true, "read_dir: AbsPath=~p\n", [AbsPath]),
    case file:list_dir(AbsPath) of
	{ok, Files} ->
	    NamesAndAttrs = get_attrs(AbsPath, Files),
	    ssh_xfer:xf_send_names(XF, ReqId, NamesAndAttrs),
	    Handles = lists:keyreplace(Handle, 1,
				       State#state.handles,
				       {Handle, directory, {RelPath,eof}}),
	    State#state{handles = Handles};
	{error, Error} ->
	    send_status({error, Error}, ReqId, State)
    end.

%%% get_attrs: get stat of each file and return
get_attrs(RelPath, Files) ->
    lists:map(fun(F) ->
		      Path = filename:absname(F, RelPath),
		      ?dbg(true, "get_attrs fun: F=~p\n", [F]),
		      {ok, Info} = file:read_link_info(Path),
		      Attrs = ssh_sftp:info_to_attr(Info),
		      {F, Attrs}
	      end, Files).

close_our_file(_T) ->
    ok.

%%% stat: do the stat
stat(Vsn, ReqId, Data, State, F) when Vsn =< 3->
    <<?UINT32(BLen), BPath:BLen/binary>> = Data,
    stat(ReqId, binary_to_list(BPath), State, F);
stat(Vsn, ReqId, Data, State, F) when Vsn >= 4->
    <<?UINT32(BLen), BPath:BLen/binary, ?UINT32(_Flags)>> = Data,
    stat(ReqId, binary_to_list(BPath), State, F).

fstat(Vsn, ReqId, Data, State) when Vsn =< 3->
    <<?UINT32(HLen), Handle:HLen/binary>> = Data,
    fstat(ReqId, Handle, State);
fstat(Vsn, ReqId, Data, State) when Vsn >= 4->
    <<?UINT32(HLen), Handle:HLen/binary, ?UINT32(_Flags)>> = Data,
    fstat(ReqId, Handle, State).

fstat(ReqId, BinHandle, State) ->
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, _Type, {Path, _}} ->
	    stat(ReqId, Path, State, read_file_info);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId, ?SSH_FX_INVALID_HANDLE),
	    State
    end.

stat(ReqId, RelPath, State, F) ->
    AbsPath = relate_file_name(RelPath, State),
    XF = State#state.xf,
    ?dbg(false, "stat: AbsPath=~p\n", [AbsPath]),
    case file:F(AbsPath) of
	{ok, FileInfo} ->
	    ssh_xfer:xf_send_attr(XF, ReqId, ssh_sftp:info_to_attr(FileInfo)),
	    State;
	{error, E} ->
	    send_status({error, E}, ReqId, State)
    end,
    State.

decode_4_open_flag(create_new) ->
    [write];
decode_4_open_flag(create_truncate) ->
    [write];
decode_4_open_flag(truncate_existing) ->
    [write];
decode_4_open_flag(open_existing) ->
    [read,write].

decode_4_flags([OpenFlag | Flags]) ->
    decode_4_flags(Flags, decode_4_open_flag(OpenFlag)).

decode_4_flags([], Flags) ->
    Flags;
decode_4_flags([append_data|R], _Flags) ->
    decode_4_flags(R, [append]);
decode_4_flags([append_data_atomic|R], _Flags) ->
    decode_4_flags(R, [append]);
decode_4_flags([_|R], Flags) ->
    decode_4_flags(R, Flags).

open(Vsn, ReqId, Data, State) when Vsn =< 3 ->
    <<?UINT32(BLen), BPath:BLen/binary, ?UINT32(PFlags),
     _Attrs/binary>> = Data,
    Path = binary_to_list(BPath),
%%     ?dbg(true, "open: PFlags=~p\n", [PFlags]),
    Flags = ssh_xfer:decode_open_flags(Vsn, PFlags) -- [creat, excl, trunc],
%%     ?dbg(true, "open: F=~p\n", [F]),
%%     Flags = F -- [trunc],
%%     Flags = case lists:member(trunc, F) of
%% 		true -> F -- [trunc];
%% 		_ -> F
%% 	    end,
    ?dbg(true, "open: Flags=~p\n", [Flags]),
    do_open(ReqId, State, Path, Flags);
open(Vsn, ReqId, Data, State) when Vsn >= 4 ->
    <<?UINT32(BLen), BPath:BLen/binary, ?UINT32(_Access),
     ?UINT32(PFlags), _Attrs/binary>> = Data,
    Path = binary_to_list(BPath),
    Fl = ssh_xfer:decode_open_flags(Vsn, PFlags),
    ?dbg(true, "open: Fl=~p\n", [Fl]),
    Flags = decode_4_flags(Fl),
    ?dbg(true, "open: Flags=~p\n", [Flags]),
    do_open(ReqId, State, Path, Flags).

do_open(ReqId, State, Path, Flags) ->
    XF = State#state.xf,
    F = [raw, binary | Flags],
    case file:open(Path, F) of
	{ok, IoDevice} ->
	    add_handle(State, XF, ReqId, file, {Path,IoDevice});
	{error, Error} ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ssh_xfer:encode_erlang_status(Error)),
	    State

    end.

%%% relate given filenames to our CWD (needed? seems we get abs. path most times)
relate_file_name(F, State) when binary(F) ->
    relate_file_name(binary_to_list(F), State);
relate_file_name(F, #state{cwd = CWD}) ->
    F1 = filename:absname(F, CWD),    
    filename:join(fix_file_name(lists:reverse(filename:split(F1)), [])).

%%% fix file just a little: a/b/.. -> a/b and a/. -> a
fix_file_name(["..", _ | Rest], Acc) ->
    fix_file_name(Rest, Acc);
fix_file_name(["." | Rest], Acc) ->
    fix_file_name(Rest, Acc);
fix_file_name([A | Rest], Acc) ->
    fix_file_name(Rest, [A | Acc]);
fix_file_name([], Acc) ->
    Acc.
    
read_file(ReqId, IoDevice, Offset, Len, State) ->
    case file:position(IoDevice, {bof, Offset}) of
	{ok, _NewPos} ->
	    case file:read(IoDevice, Len) of
		{ok, Data} ->
		    ssh_xfer:xf_send_data(State#state.xf, ReqId, Data),
		    State;
		{error, Error} ->
		    send_status({error, Error}, ReqId, State);
		eof ->
		    send_status(eof, ReqId, State)
	    end;
	{error, Error} ->
		    send_status({error, Error}, ReqId, State)
    end.

write_file(ReqId, IoDevice, Offset, Data, State) ->
    case file:position(IoDevice, {bof, Offset}) of
	{ok, _NewPos} ->
	    Status = file:write(IoDevice, Data),
	    send_status(Status, ReqId, State);
	{error, Error} ->
	    send_status({error, Error}, ReqId, State)
    end.

get_status(ok) ->
    ?SSH_FX_OK;
get_status(eof) ->
    ?SSH_FX_EOF;
get_status({error,Error}) ->
    ssh_xfer:encode_erlang_status(Error).

send_status(Status, ReqId, State) ->
    ssh_xfer:xf_send_status(State#state.xf, ReqId, get_status(Status)),
    State.

%%
set_stat(<<>>, _Path, _State) ->
    ok;
set_stat(Attr, Path, State) ->
    {DecodedAttr, _Rest} = ssh_xfer:decode_ATTR((State#state.xf)#ssh_xfer.vsn, Attr),
    ?dbg(true, "set_stat DecodedAttr=~p\n", [DecodedAttr]),
    Info = ssh_sftp:attr_to_info(DecodedAttr),
    case file:read_link_info(Path) of
	{ok, OldInfo} ->
	    NewInfo = set_file_info(Info, OldInfo),
	    ?dbg(true, "set_stat Path=~p\nInfo=~p\nOldInfo=~p\nNewInfo=~p\n",
		 [Path, Info, OldInfo, NewInfo]),
	    file:write_file_info(Path, NewInfo);
	{error, Error} ->
	    {error, Error}
    end.


set_file_info_sel(undefined, F) ->
    F;
set_file_info_sel(F, _) ->
    F.

set_file_info(#file_info{atime = Dst_atime, mtime = Dst_mtime, ctime = Dst_ctime,
			 mode = Dst_mode, uid = Dst_uid, gid = Dst_gid},
	      #file_info{atime = Src_atime, mtime = Src_mtime, ctime = Src_ctime,
			 mode = Src_mode, uid = Src_uid, gid = Src_gid}) ->    
    #file_info{atime = set_file_info_sel(Dst_atime, Src_atime),
	       mtime = set_file_info_sel(Dst_mtime, Src_mtime),
	       ctime = set_file_info_sel(Dst_ctime, Src_ctime),
	       mode = set_file_info_sel(Dst_mode, Src_mode),
	       uid = set_file_info_sel(Dst_uid, Src_uid),
	       gid = set_file_info_sel(Dst_gid, Src_gid)}.

rename(Path, Path2, ReqId, State) ->
    Status = file:rename(Path, Path2),
    send_status(Status, ReqId, State).
