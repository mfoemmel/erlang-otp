%%<copyright>
%% <year>2004-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
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

%%% Description: SFTP server daemon

-module(ssh_sftpd).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").

-include("ssh.hrl").
-include("ssh_xfer.hrl").

-define(DEFAULT_TIMEOUT, 5000).

%%--------------------------------------------------------------------
%% External exports
-export([subsystem_spec/1,
	 listen/1, listen/2, listen/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  xf,   			% [{channel,ssh_xfer states}...]
	  cwd,				% current dir (on first connect)
	  root,				% root dir
	  remote_channel,		% remote channel
	  pending,                      % binary() 
	  file_handler,			% atom() - callback module 
	  file_state,                   % state for the file callback module
	  handles			% list of open handles
	  %% handle is either {<int>, directory, {Path, unread|eof}} or
	  %% {<int>, file, {Path, IoDevice}}
	 }).

%%====================================================================
%% API
%%====================================================================
subsystem_spec(Options) ->
    Name = make_ref(),
    StartFunc = {gen_server, 
		 start_link, [ssh_sftpd, [Options], []]},
    Restart = transient, 
    Shutdown = 3600,
    Modules = [ssh_sftpd],
    Type = worker,
    {"sftp", {Name, StartFunc, Restart, Shutdown, Type, Modules}}.

%%--------------------------------------------------------------------
%% Function: listen() -> Pid | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
listen(Port) ->
    listen(any, Port, []).
listen(Port, Options) ->
    listen(any, Port, Options).
listen(Addr, Port, Options) ->
    SubSystems = [subsystem_spec(Options)],
    ssh:daemon(Addr, Port, [{subsystems, SubSystems} |Options]).

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
    {FileMod, FS0} = case proplists:get_value(file_handler, Options, 
					      {ssh_sftpd_file,[]}) of
			 {F, S} ->
			     {F, S};
			 F ->
			     {F, []}
		     end,
    
    {{ok, Default}, FS1} = FileMod:get_cwd(FS0),
    CWD = proplists:get_value(cwd, Options, Default),
    Root = proplists:get_value(root, Options, ""),
    State = #state{cwd = CWD, root = Root, handles = [], pending = <<>>,
		   file_handler = FileMod, file_state = FS1},
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

handle_info({ssh_cm, CM, {open, Channel, RemoteChannel, _Type}}, State) ->
    XF = #ssh_xfer{vsn = 5, ext = [], cm = CM, channel = Channel},
    State1 = State#state{xf = XF, remote_channel = RemoteChannel},
    {noreply, State1};
handle_info({ssh_cm, CM, {data, Channel, Type, Data}}, State) ->
    ssh_connection:adjust_window(CM, Channel, size(Data)),
    State1 = handle_data(Type, Data, State),
    {noreply, State1};

handle_info({ssh_cm, CM, {subsystem, _, WantReply, "sftp"}}, 
	    #state{remote_channel = ChannelId} = State) ->
    ssh_connection:reply_request(CM, WantReply, success, ChannelId),
    {noreply, State};

%% The client has terminated the session
%% TODO: why check channel in xf ssh_xfer?
handle_info({ssh_cm, _, {eof, Channel}}, 
	    State = #state{xf = #ssh_xfer{channel = Channel}}) ->
    {stop, normal, State};

handle_info({ssh_cm, _CM, {closed, _Channel}}, State) ->
    %% ignore -- we'll get an {eof, Channel} soon??
    {noreply, State};

handle_info(_Info, State) ->
    ?dbg(true, "handle_info: Info=~p State=~p\n", [_Info, State]),
    {noreply, State}.

handle_data(0, <<?UINT32(Len), Msg:Len/binary, Rest/binary>>, 
	    State = #state{pending = <<>>}) ->
    <<Op, ?UINT32(ReqId), Data/binary>> = Msg,
    NewState = handle_op(Op, ReqId, Data, State),
    case Rest of
	<<>> ->
	    NewState;   
	_ ->
	    handle_data(0, Rest, NewState)
    end;
	     
handle_data(0, Data, State = #state{pending = <<>>}) ->
    State#state{pending = Data};

handle_data(Type, Data, State = #state{pending = Pending}) -> 
     handle_data(Type, <<Pending/binary, Data/binary>>, 
                 State#state{pending = <<>>});
 
handle_data(_, Data, State) ->
    error_logger:format("ssh: STDERR: ~s\n", [binary_to_list(Data)]),
    State.

handle_op(?SSH_FXP_INIT, Version, B, State) when is_binary(B) ->
    XF = State#state.xf,
    Vsn = lists:min([XF#ssh_xfer.vsn, Version]),
    XF1 = XF#ssh_xfer{vsn = Vsn},
    ssh_xfer:xf_send_reply(XF1, ?SSH_FXP_VERSION, <<?UINT32(Vsn)>>),
    State#state{xf = XF1};
handle_op(?SSH_FXP_REALPATH, ReqId,
	  <<?UINT32(Rlen), RPath:Rlen/binary>>,
	  #state{root = Root} = State) ->
    RelPath = binary_to_list(RPath),
    AbsPath = relate_file_name(RelPath, State),
    NewAbsPath = case AbsPath of
		     Root -> 
			 "/";
		     Other ->
			 Other -- Root
		 end,
    ?dbg(true, "handle_op ?SSH_FXP_REALPATH: RelPath=~p AbsPath=~p\n",
	 [RelPath, NewAbsPath]),
    XF = State#state.xf,
    Attr = #ssh_xfer_attr{type=directory},
    ssh_xfer:xf_send_name(XF, ReqId, NewAbsPath, Attr),
    State;
handle_op(?SSH_FXP_OPENDIR, ReqId,
	 <<?UINT32(RLen), RPath:RLen/binary>>,
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    RelPath = binary_to_list(RPath),
    AbsPath = relate_file_name(RelPath, State0),
    
    XF = State0#state.xf,
    {IsDir, FS1} = FileMod:is_dir(AbsPath, FS0),
    State1 = State0#state{file_state = FS1},
    case IsDir of
	false ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_NOT_A_DIRECTORY,
				    "Not a directory"),
	    State1;
	true ->
	    add_handle(State1, XF, ReqId, directory, {RelPath,unread})
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
	  State = #state{handles = Handles, xf = XF,
			 file_handler = FileMod, file_state = FS0}) ->
    case get_handle(Handles, BinHandle) of
	{Handle, Type, T} ->
	    FS1 = case Type of
			     file ->
				 close_our_file(T, FileMod, FS0);
			     _ ->
				 FS0
			 end,
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_OK),
	    State#state{handles = lists:keydelete(Handle, 1, Handles),
			file_state = FS1};
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
	    ssh_xfer:xf_send_status(State#state.xf, ReqId, 
				    ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_WRITE, ReqId,
	  <<?UINT32(HLen), BinHandle:HLen/binary, ?UINT64(Offset),
	   ?UINT32(Len), Data:Len/binary>>, State) ->
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, file, {_Path, IoDevice}} ->
	    write_file(ReqId, IoDevice, Offset, Data, State);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_READLINK, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, 
	  State = #state{file_handler = FileMod, file_state = FS0}) ->
    RelPath = binary_to_list(BPath),
    AbsPath = relate_file_name(RelPath, State),
    {Res, FS1} = FileMod:read_link(AbsPath, FS0),
    case Res of
	{ok, NewPath} ->
	    ssh_xfer:xf_send_name(State#state.xf, ReqId, NewPath,
				  #ssh_xfer_attr{type=regular});
	{error, Error} ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ssh_xfer:encode_erlang_status(Error))
    end,
    State#state{file_state = FS1};
handle_op(?SSH_FXP_SETSTAT, ReqId, <<?UINT32(PLen), BPath:PLen/binary,
				    Attr/binary>>, State0) ->
    Path = relate_file_name(BPath, State0),
    {Status, State1} = set_stat(Attr, Path, State0),
    send_status(Status, ReqId, State1);
handle_op(?SSH_FXP_MKDIR, ReqId, <<?UINT32(PLen), BPath:PLen/binary,
				  Attr/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    {Res, FS1} = FileMod:make_dir(Path, FS0),
    State1 = State0#state{file_state = FS1},
    case Res of
	ok ->
	    {_, State2} = set_stat(Attr, Path, State1),
	    send_status(ok, ReqId, State2);
	{error, Error} ->
	    send_status({error, Error}, ReqId, State1)
    end;
handle_op(?SSH_FXP_FSETSTAT, ReqId, <<?UINT32(HLen), BinHandle:HLen/binary, 
				     Attr/binary>>, 
	  State0 = #state{handles = Handles}) ->

    case get_handle(Handles, BinHandle) of
	{_Handle, _Type, {Path,_}} ->
	    {Status, State1} = set_stat(Attr, Path, State0),
	    send_status(Status, ReqId, State1);
	_ ->
	    ssh_xfer:xf_send_status(State0#state.xf, ReqId,
				    ?SSH_FX_INVALID_HANDLE),
	    State0
    end;
handle_op(?SSH_FXP_REMOVE, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    %%  case FileMod:is_dir(Path) of %% This version 6 we still have ver 5
    %% 	true ->
    %% 	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
    %% 				    ?SSH_FX_FILE_IS_A_DIRECTORY); 
    %% 	false ->
    {Status, FS1} = FileMod:delete(Path, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1);
    %%end;
handle_op(?SSH_FXP_RMDIR, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    {Status, FS1} = FileMod:del_dir(Path, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1);

handle_op(?SSH_FXP_RENAME, ReqId,
	  Bin = <<?UINT32(PLen), _:PLen/binary, ?UINT32(PLen2), 
		 _:PLen2/binary>>, 
	  State = #state{xf = #ssh_xfer{vsn = 3}}) ->
    handle_op(?SSH_FXP_RENAME, ReqId, <<Bin/binary, 0:32>>, State);

handle_op(?SSH_FXP_RENAME, ReqId,
	  <<?UINT32(PLen), BPath:PLen/binary, ?UINT32(PLen2), 
	   BPath2:PLen2/binary, ?UINT32(Flags)>>,
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    Path2 = relate_file_name(BPath2, State0),
    case Flags band ?SSH_FXP_RENAME_ATOMIC of
	0 ->
	    case Flags band ?SSH_FXP_RENAME_OVERWRITE of
		0 ->
		    {Res, FS1} = FileMod:read_link_info(Path2, FS0),
		    State1 = State0#state{file_state = FS1},
		    case Res of
			{ok, _Info} ->
			    ssh_xfer:xf_send_status(
			      State1#state.xf, 
			      ReqId,
			      ?SSH_FX_FILE_ALREADY_EXISTS),
			    State1;
			_ ->
			    rename(Path, Path2, ReqId, State1)
		    end;
		_ ->
		    rename(Path, Path2, ReqId, State0)
	    end;
	_ ->
	    ssh_xfer:xf_send_status(State0#state.xf, ReqId,
				    ?SSH_FX_OP_UNSUPPORTED),
	    State0
    end;
handle_op(?SSH_FXP_SYMLINK, ReqId,
	  <<?UINT32(PLen), BPath:PLen/binary, ?UINT32(PLen2),
	   BPath2:PLen2/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    Path2 = relate_file_name(BPath2, State0),
    {Status, FS1} = FileMod:make_symlink(Path2, Path, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_, #state{handles=Handles, file_handler=FileMod, file_state=FS}) ->
    CloseFun = fun({_, file, {_, Fd}}, FS0) ->
		       {_Res, FS1} = FileMod:close(Fd, FS0),
		       FS1;
		  (_, FS0) ->
		       FS0
	       end,
    lists:foldl(CloseFun, FS, Handles),
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
    case (catch list_to_integer(binary_to_list(BinHandle))) of
	I when integer(I) ->
	    case lists:keysearch(I, 1, Handles) of
		{value, T} -> T;
		false -> error
	    end;
	_ ->
	    error
    end.

%%% read_dir/5: read directory, send names, and return new state
read_dir(State0 = #state{file_handler = FileMod, file_state = FS0},
	 XF, ReqId, Handle, RelPath) ->
    AbsPath = relate_file_name(RelPath, State0),
    ?dbg(true, "read_dir: AbsPath=~p\n", [AbsPath]),
    {Res, FS1} = FileMod:list_dir(AbsPath, FS0),
    case Res of
	{ok, Files} ->
	    {NamesAndAttrs, FS2} = get_attrs(AbsPath, Files, FileMod, FS1),
	    ssh_xfer:xf_send_names(XF, ReqId, NamesAndAttrs),
	    Handles = lists:keyreplace(Handle, 1,
				       State0#state.handles,
				       {Handle, directory, {RelPath,eof}}),
	    State0#state{handles = Handles, file_state = FS2};
	{error, Error} ->
	    State1 = State0#state{file_state = FS1},
	    send_status({error, Error}, ReqId, State1)
    end.

%%% get_attrs: get stat of each file and return
get_attrs(RelPath, Files, FileMod, FS) ->
    get_attrs(RelPath, Files, FileMod, FS, []).

get_attrs(_RelPath, [], _FileMod, FS, Acc) ->
    {lists:reverse(Acc), FS};
get_attrs(RelPath, [F | Rest], FileMod, FS0, Acc) ->
    Path = filename:absname(F, RelPath),
    ?dbg(true, "get_attrs fun: F=~p\n", [F]),
    case FileMod:read_link_info(Path, FS0) of
	{{ok, Info}, FS1} ->
	    Attrs = ssh_sftp:info_to_attr(Info),
	    get_attrs(RelPath, Rest, FileMod, FS1, [{F, Attrs} | Acc]);
	{{error, enoent}, FS1} ->
	    get_attrs(RelPath, Rest, FileMod, FS1, Acc);
	{Error, FS1} ->
	    {Error, FS1}
    end.

close_our_file({_,Fd}, FileMod, FS0) ->
    {_Res, FS1} = FileMod:close(Fd, FS0),
    FS1.

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
	    ssh_xfer:xf_send_status(State#state.xf, ReqId, 
				    ?SSH_FX_INVALID_HANDLE),
	    State
    end.

stat(ReqId, RelPath, State0=#state{file_handler=FileMod, 
				   file_state=FS0}, F) ->
    AbsPath = relate_file_name(RelPath, State0),
    XF = State0#state.xf,
    ?dbg(false, "stat: AbsPath=~p\n", [AbsPath]),
    {Res, FS1} = FileMod:F(AbsPath, FS0),
    State1 = State0#state{file_state = FS1},
    case Res of
	{ok, FileInfo} ->
	    ssh_xfer:xf_send_attr(XF, ReqId, 
				  ssh_sftp:info_to_attr(FileInfo)),
	    State1;
	{error, E} ->
	    send_status({error, E}, ReqId, State1)
    end.

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
    Flags = ssh_xfer:decode_open_flags(Vsn, PFlags) -- [creat, excl, trunc],
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

do_open(ReqId, State0, Path, Flags) ->
    #state{file_handler = FileMod, file_state = FS0, root = Root} = State0,
    XF = State0#state.xf,
    F = [raw, binary | Flags],
    %%   case FileMod:is_dir(Path) of %% This is version 6 we still have 5
    %% 	true ->
    %% 	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
    %% 				    ?SSH_FX_FILE_IS_A_DIRECTORY);
    %% 	false ->
    
    AbsPath = case Root of
		  "" ->
		      Path;
		  _ ->
		      relate_file_name(Path, State0)  
	      end,

    {Res, FS1} = FileMod:open(AbsPath, F, FS0),
    State1 = State0#state{file_state = FS1},
    case Res of
	{ok, IoDevice} ->
	    add_handle(State1, XF, ReqId, file, {Path,IoDevice});
	{error, Error} ->
	    ssh_xfer:xf_send_status(State1#state.xf, ReqId,
				    ssh_xfer:encode_erlang_status(Error)),
	    State1
    end.

relate_file_name(File, State) when binary(File) ->
    relate_file_name(binary_to_list(File), State);
relate_file_name(File, #state{cwd = CWD, root = ""}) ->
    relate(File, CWD);
relate_file_name(File, #state{root = Root}) ->
    case within_root(Root, File) of
	File ->
	    File;
	Root ->
	    NewFile = relate(string:strip(File, left, $/), Root),
	    within_root(Root, NewFile)
    end.

within_root(Root, File) ->
    case lists:prefix(Root, File) of
	true ->
	    File;
	false ->
	    Root
    end.

relate(File0, Path) ->
    File1 = filename:absname(File0, Path),
    Parts = fix_file_name(filename:split(File1), []),
    filename:join(Parts).

%%% fix file just a little: a/b/.. -> a and a/. -> a
fix_file_name([".." | Rest], ["/"] = Acc) ->
    fix_file_name(Rest, Acc);
fix_file_name([".." | Rest], [_Dir | Paths]) ->
    fix_file_name(Rest, Paths);
fix_file_name(["." | Rest], Acc) ->
    fix_file_name(Rest, Acc);
fix_file_name([A | Rest], Acc) ->
    fix_file_name(Rest, [A | Acc]);
fix_file_name([], Acc) ->
    lists:reverse(Acc).
    
read_file(ReqId, IoDevice, Offset, Len,	
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    {Res1, FS1} = FileMod:position(IoDevice, {bof, Offset}, FS0),
    case Res1 of
	{ok, _NewPos} ->
	    {Res2, FS2} = FileMod:read(IoDevice, Len, FS1),
	    State1 = State0#state{file_state = FS2},
	    case Res2 of
		{ok, Data} ->
		    ssh_xfer:xf_send_data(State1#state.xf, ReqId, Data),
		    State1;
		{error, Error} ->
		    send_status({error, Error}, ReqId, State1);
		eof ->
		    send_status(eof, ReqId, State1)
	    end;
	{error, Error} ->
    	    State1 = State0#state{file_state = FS1},
	    send_status({error, Error}, ReqId, State1)
    end.

write_file(ReqId, IoDevice, Offset, Data, 
	   State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    {Res, FS1} = FileMod:position(IoDevice, {bof, Offset}, FS0),
    case Res of
	{ok, _NewPos} ->
	    {Status, FS2} = FileMod:write(IoDevice, Data, FS1),
	    State1 = State0#state{file_state = FS2},
	    send_status(Status, ReqId, State1);
	{error, Error} ->
	    State1 = State0#state{file_state = FS1},
	    send_status({error, Error}, ReqId, State1)
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
set_stat(<<>>, _Path, State) ->
    {ok, State};
set_stat(Attr, Path, 
	 State0 = #state{file_handler=FileMod, file_state=FS0}) ->
    {DecodedAttr, _Rest} = 
	ssh_xfer:decode_ATTR((State0#state.xf)#ssh_xfer.vsn, Attr),
    ?dbg(true, "set_stat DecodedAttr=~p\n", [DecodedAttr]),
    Info = ssh_sftp:attr_to_info(DecodedAttr),
    {Res1, FS1} = FileMod:read_link_info(Path, FS0),
    case Res1 of
	{ok, OldInfo} ->
	    NewInfo = set_file_info(Info, OldInfo),
	    ?dbg(true, "set_stat Path=~p\nInfo=~p\nOldInfo=~p\nNewInfo=~p\n",
		 [Path, Info, OldInfo, NewInfo]),
	    {Res2, FS2} = FileMod:write_file_info(Path, NewInfo, FS1),
	    State1 = State0#state{file_state = FS2},
	    {Res2, State1};
	{error, Error} ->
	    State1 = State0#state{file_state = FS1},
	    {{error, Error}, State1}
    end.


set_file_info_sel(undefined, F) ->
    F;
set_file_info_sel(F, _) ->
    F.

set_file_info(#file_info{atime = Dst_atime, mtime = Dst_mtime, 
			 ctime = Dst_ctime,
			 mode = Dst_mode, uid = Dst_uid, gid = Dst_gid},
	      #file_info{atime = Src_atime, mtime = Src_mtime, 
			 ctime = Src_ctime,
			 mode = Src_mode, uid = Src_uid, gid = Src_gid}) ->  
    #file_info{atime = set_file_info_sel(Dst_atime, Src_atime),
	       mtime = set_file_info_sel(Dst_mtime, Src_mtime),
	       ctime = set_file_info_sel(Dst_ctime, Src_ctime),
	       mode = set_file_info_sel(Dst_mode, Src_mode),
	       uid = set_file_info_sel(Dst_uid, Src_uid),
	       gid = set_file_info_sel(Dst_gid, Src_gid)}.

rename(Path, Path2, ReqId, State0) ->
    #state{file_handler = FileMod, file_state = FS0} = State0,
    {Status, FS1} = FileMod:rename(Path, Path2, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1).
