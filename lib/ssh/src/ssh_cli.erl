%%<copyright>
%% <year>2005-2007</year>
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
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_cli).

-behaviour(gen_server).

-include("ssh.hrl").
-include("ssh_connect.hrl").

%% Internal API
-export([child_spec/1, child_spec/2, child_spec/3, child_spec/4]).

%% backwards compatibility
-export([listen/1, listen/2, listen/3, listen/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% defines
-define(DBG_IO_REQUEST, true).

%% state
-record(state, {
	  cm,
	  channel,
	  pty,
	  group,
	  buf,
	  shell,
	  remote_channel,
	  options,
	  address,
	  port
	 }).

%%====================================================================
%% API
%%====================================================================
child_spec(Shell) ->
    child_spec(Shell, 22).

child_spec(Shell, Port) ->
    child_spec(Shell, Port, []).

child_spec(Shell, Port, Opts) ->
    child_spec(Shell, any, Port, Opts).

child_spec(Shell, Address, Port, Opts) ->
    Name = make_ref(),
    StartFunc = {gen_server, 
		 start_link, [?MODULE, 
			      [Shell, Address, Port, Opts], []]},
    Restart = temporary, 
    Shutdown = 3600,
    Modules = [ssh_cli],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

%%--------------------------------------------------------------------
%% Function: listen(...) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts a listening server
%% Note that the pid returned is NOT the pid of this gen_server;
%% this server is started when an SSH connection is made on the
%% listening port
%%--------------------------------------------------------------------
listen(Shell) ->
    listen(Shell, 22).

listen(Shell, Port) ->
    listen(Shell, Port, []).

listen(Shell, Port, Opts) ->
    listen(Shell, any, Port, Opts).

listen(Shell, HostAddr, Port, Opts) ->
    ssh:daemon(HostAddr, Port, [{shell, Shell} | Opts]).
    

%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok
%% Description: Stops the listener
%%--------------------------------------------------------------------
stop(Pid) ->
    ssh:stop_listener(Pid).

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
init([Shell, Address, Port, Opts]) ->
    {ok, #state{shell = Shell, 
		address = Address,
		port = Port,
		options = Opts}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    Result = ssh_cm:stop(State#state.cm),
    {stop, normal, Result, State};
handle_call(info, _From, State) ->
    {reply, State, State};
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
handle_info({ssh_cm, CM, {open, Channel, RemoteChannel, {session}}}, State) ->
    {noreply,
     State#state{cm = CM, channel = Channel, remote_channel = RemoteChannel}};

handle_info({ssh_cm, CM, {data, _Channel, _Type, Data}}, 
	    #state{remote_channel = ChannelId} = State) ->
    ssh_connection:adjust_window(CM, ChannelId, size(Data)),
    State#state.group ! {self(), {data, binary_to_list(Data)}},
    {noreply, State};

handle_info({ssh_cm, CM, {pty, _Channel, WantReply, Pty}}, 
	    #state{remote_channel = ChannelId} = State0) ->
    ssh_connection:reply_request(CM, WantReply, success, ChannelId),
    State = State0#state{pty = Pty},
    set_echo(State),
    {noreply, State};

handle_info({ssh_cm, _CM,
	     {window_change, _Channel, Width, Height, PixWidth, PixHeight}},
	    State) ->
    #state{buf = Buf, pty = Pty, cm = CM, channel = Channel} = State,
    NewPty = Pty#ssh_pty{width = Width, height = Height,
			 pixel_width = PixWidth,
			 pixel_height = PixHeight},
    {Chars, NewBuf} = io_request({window_change, Pty}, Buf, NewPty),
    write_chars(CM, Channel, Chars),
    {noreply, State#state{pty = NewPty, buf = NewBuf}};
handle_info({Group, Req}, State) when Group==State#state.group ->
    ?dbg(?DBG_IO_REQUEST, "io_request: ~w\n", [Req]),
    #state{buf = Buf, pty = Pty, cm = CM, channel = Channel} = State,
    {Chars, NewBuf} = io_request(Req, Buf, Pty),
    write_chars(CM, Channel, Chars),
    {noreply, State#state{buf = NewBuf}};
handle_info({ssh_cm, CM, {shell, WantReply}}, 
	    #state{remote_channel = ChannelId} = State) ->
    NewState = start_shell(CM, State),
    process_flag(trap_exit, true),
    ssh_connection:reply_request(CM, WantReply, success, ChannelId),
    {noreply, NewState};
handle_info({ssh_cm, CM, {exec, Cmd}}, #state{channel = ChannelId} = State) ->
    Reply = case erl_scan:string(Cmd) of
		{ok, Tokens, _EndList} ->
		    case erl_parse:parse_exprs(Tokens) of
			{ok, Expr_list} ->
			    case (catch erl_eval:exprs(
					  Expr_list,
					  erl_eval:new_bindings())) of
				{value, Value, _NewBindings} ->
				    Value;
				{'EXIT', {E, _}} -> E;
				E -> E
			    end;
			E -> E
		    end;
		E -> E
	    end,
    write_chars(CM, ChannelId, io_lib:format("~p\n", [Reply])),
    ssh_connection:send_eof(CM, ChannelId),
    ssh_connection:close(CM, ChannelId),
    {noreply, State};
handle_info({get_cm, From}, #state{cm=CM} = State) ->
    From ! {From, cm, CM},
    {noreply, State};
handle_info({ssh_cm, _CM, {eof, _Channel}}, State) ->
    {stop, normal, State};
handle_info({ssh_cm, _CM, {closed, _Channel}}, State) ->
    %% ignore -- we'll get an {eof, Channel} soon??
    {noreply, State};
handle_info({ssh_cm, CM, {subsystem, ChannelId, _WantsReply, SsName}} = Msg,
	    #state{address = Address, port = Port, options = Opts,
		   remote_channel = RemoteChannel} = State) ->
    case check_subsystem(SsName, Opts) of
	none ->
	    {noreply, State};
	%% Backwards compatibility
	Module when is_atom(Module) ->
	    {ok, SubSystemD} = 
		gen_server:start_link(Module, [Opts], []),
	    SubSystemD ! 
		{ssh_cm, CM, {open, ChannelId, RemoteChannel, {session}}},
	    SubSystemD ! Msg,
	    ssh_connection_manager:controlling_process(CM, ChannelId, 
						       SubSystemD, self()),
	    {stop, normal, State};
	Fun when is_function(Fun) ->
	    SubSystemD = Fun(),
	    SubSystemD ! 
		{ssh_cm, CM, {open, ChannelId, RemoteChannel, {session}}},
	    SubSystemD ! Msg,
	    ssh_connection_manager:controlling_process(CM, ChannelId, 
						       SubSystemD, self()),
	    {stop, normal, State};
	ChildSpec ->
	    SystemSup = ssh_system_sup:system_supervisor(Address, Port),
	    ChannelSup = ssh_system_sup:channel_supervisor(SystemSup),
	    {ok, SubSystemD} 
		= ssh_channel_sup:start_child(ChannelSup, ChildSpec),
	    SubSystemD ! 
		{ssh_cm, CM, {open, ChannelId, RemoteChannel, {session}}},
	    SubSystemD ! Msg,
	    ssh_connection_manager:controlling_process(CM, ChannelId, 
						       SubSystemD, self()),
	    empty_mailbox_workaround(SubSystemD),
	    {stop, normal, State}
    
    end;
handle_info({'EXIT', Group, normal},
	    #state{cm=CM, channel=Channel, group=Group} = State) ->
    ssh_connection:close(CM, Channel),
    ssh_cm:stop(CM),
    {stop, normal, State};
handle_info(Info, State) ->
    ?dbg(true, "~p:handle_info: BAD info ~p\n(State ~p)\n", 
	 [?MODULE, Info, State]),
    {stop, {bad_info, Info}, State}.

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

%%% io_request, handle io requests from the user process
io_request({window_change, OldTty}, Buf, Tty) ->
    window_change(Tty, OldTty, Buf);
io_request({put_chars, Cs}, Buf, Tty) ->
    put_chars(bin_to_list(Cs), Buf, Tty);
io_request({insert_chars, Cs}, Buf, Tty) ->
    insert_chars(bin_to_list(Cs), Buf, Tty);
io_request({move_rel, N}, Buf, Tty) ->
    move_rel(N, Buf, Tty);
io_request({delete_chars,N}, Buf, Tty) ->
    delete_chars(N, Buf, Tty);
io_request(beep, Buf, _Tty) ->
    {[7], Buf};

%% New in R12
io_request({get_geometry,columns},Buf,Tty) ->
    {ok, Tty#ssh_pty.width, Buf};
io_request({get_geometry,rows},Buf,Tty) ->
    {ok, Tty#ssh_pty.height, Buf};
io_request({requests,Rs}, Buf, Tty) ->
    io_requests(Rs, Buf, Tty, []);
io_request(tty_geometry, Buf, Tty) ->
    io_requests([{move_rel, 0}, {put_chars, [10]}], Buf, Tty, []);
     %{[], Buf};
io_request(_R, Buf, _Tty) ->
    {[], Buf}.

io_requests([R|Rs], Buf, Tty, Acc) ->
    {Chars, NewBuf} = io_request(R, Buf, Tty),
    io_requests(Rs, NewBuf, Tty, [Acc|Chars]);
io_requests([], Buf, _Tty, Acc) ->
    {Acc, Buf}.

%%% return commands for cursor navigation, assume everything is ansi
%%% (vt100), add clauses for other terminal types if needed
ansi_tty(N, L) ->
    ["\e[", integer_to_list(N), L].

get_tty_command(up, N, _TerminalType) ->
    ansi_tty(N, $A);
get_tty_command(down, N, _TerminalType) ->
    ansi_tty(N, $B);
get_tty_command(right, N, _TerminalType) ->
    ansi_tty(N, $C);
get_tty_command(left, N, _TerminalType) ->
    ansi_tty(N, $D).


-define(PAD, 10).
-define(TABWIDTH, 8).

%% convert input characters to buffer and to writeout
%% Note that the buf is reversed but the buftail is not
%% (this is handy; the head is always next to the cursor)
conv_buf([], AccBuf, AccBufTail, AccWrite, Col) ->
    {AccBuf, AccBufTail, lists:reverse(AccWrite), Col};
conv_buf([13, 10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl2(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([13 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [13 | AccWrite], 0);
conv_buf([10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([9 | Rest], AccBuf, AccBufTail, AccWrite, Col) ->
    NSpaces = (Col + (?TABWIDTH - 1)) rem ?TABWIDTH + 1,
    AccB = string:chars(?PAD, NSpaces-1) ++ [9 | AccBuf],
    AccW = string:chars(32, NSpaces) ++ [AccWrite],
    AccBT = nthtail(NSpaces, AccBufTail),
    conv_buf(Rest, AccB, AccBT, AccW, Col + NSpaces);
conv_buf([C | Rest], AccBuf, AccBufTail, AccWrite, Col) ->
    conv_buf(Rest, [C | AccBuf], tl1(AccBufTail), [C | AccWrite], Col + 1).


%%% put characters at current position (possibly overwriting
%%% characters after current position in buffer)
put_chars(Chars, {Buf, BufTail, Col}, _Tty) ->
    {NewBuf, NewBufTail, WriteBuf, NewCol} =
	conv_buf(Chars, Buf, BufTail, [], Col),
    {WriteBuf, {NewBuf, NewBufTail, NewCol}}.

%%% insert character at current position
insert_chars([], {Buf, BufTail, Col}, _Tty) ->
    {[], {Buf, BufTail, Col}};
insert_chars(Chars, {Buf, BufTail, Col}, Tty) ->
    {NewBuf, _NewBufTail, WriteBuf, NewCol} =
	conv_buf(Chars, Buf, [], [], Col),
    M = move_cursor(NewCol + length(BufTail), NewCol, Tty),
    {[WriteBuf, BufTail | M], {NewBuf, BufTail, NewCol}}.

%%% delete characters at current position, (backwards if negative argument)
delete_chars(0, {Buf, BufTail, Col}, _Tty) ->
    {[], {Buf, BufTail, Col}};
delete_chars(N, {Buf, BufTail, Col}, Tty) when N > 0 ->
    NewBufTail = nthtail(N, BufTail),
    M = move_cursor(Col + length(NewBufTail) + N, Col, Tty),
    {[NewBufTail, lists:duplicate(N, $ ) | M],
     {Buf, NewBufTail, Col}};
delete_chars(N, {Buf, BufTail, Col}, Tty) -> % N < 0
    NewBuf = nthtail(-N, Buf),
    NewCol = Col + N,
    M1 = move_cursor(Col, NewCol, Tty),
    M2 = move_cursor(NewCol + length(BufTail) - N, NewCol, Tty),
    {[M1, BufTail, lists:duplicate(-N, $ ) | M2],
     {NewBuf, BufTail, NewCol}}.

%%% Window change, redraw the current line (and clear out after it
%%% if current window is wider than previous)
window_change(Tty, OldTty, Buf)
  when OldTty#ssh_pty.width == Tty#ssh_pty.width ->
    {[], Buf};
window_change(Tty, OldTty, {Buf, BufTail, Col}) ->
    M1 = move_cursor(Col, 0, OldTty),
    N = max(Tty#ssh_pty.width - OldTty#ssh_pty.width, 0) * 2,
    S = lists:reverse(Buf, [BufTail | lists:duplicate(N, $ )]),
    M2 = move_cursor(length(Buf) + length(BufTail) + N, Col, Tty),
    {[M1, S | M2], {Buf, BufTail, Col}}.
    
%% move around in buffer, respecting pad characters
step_over(0, Buf, [?PAD | BufTail], Col) ->
    {[?PAD | Buf], BufTail, Col+1};
step_over(0, Buf, BufTail, Col) ->
    {Buf, BufTail, Col};
step_over(N, [C | Buf], BufTail, Col) when N < 0 ->
    N1 = ifelse(C == ?PAD, N, N+1),
    step_over(N1, Buf, [C | BufTail], Col-1);
step_over(N, Buf, [C | BufTail], Col) when N > 0 ->
    N1 = ifelse(C == ?PAD, N, N-1),
    step_over(N1, [C | Buf], BufTail, Col+1).

%%% an empty line buffer
empty_buf() -> {[], [], 0}.

%%% col and row from position with given width
col(N, W) -> N rem W.
row(N, W) -> N div W.

%%% move relative N characters
move_rel(N, {Buf, BufTail, Col}, Tty) ->
    {NewBuf, NewBufTail, NewCol} = step_over(N, Buf, BufTail, Col),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewBuf, NewBufTail, NewCol}}.

%%% give move command for tty
move_cursor(A, A, _Tty) ->
    [];
move_cursor(From, To, #ssh_pty{width=Width, term=Type}) ->
    Tcol = case col(To, Width) - col(From, Width) of
	       0 -> "";
	       I when I < 0 -> get_tty_command(left, -I, Type);
	       I -> get_tty_command(right, I, Type)
	end,
    Trow = case row(To, Width) - row(From, Width) of
	       0 -> "";
	       J when J < 0 -> get_tty_command(up, -J, Type);
	       J -> get_tty_command(down, J, Type)
	   end,
    [Tcol | Trow].

%%% write out characters
%%% make sure that there is data to send
%%% before calling ssh_connection:send
write_chars(_, _, []) ->
    ok;
write_chars(_, _, [[]]) ->
    ok;
write_chars(CM, Channel, Chars) ->
    Type = 0,
    ssh_connection:send(CM, Channel, Type, Chars).
%%% tail, works with empty lists
tl1([_|A]) -> A;
tl1(_) -> [].

%%% second tail
tl2([_,_|A]) -> A;
tl2(_) -> [].

%%% nthtail as in lists, but no badarg if n > the length of list
nthtail(0, A) -> A;
nthtail(N, [_ | A]) when N > 0 -> nthtail(N-1, A);
nthtail(_, _) -> [].

%%% utils
max(A, B) when A > B -> A;
max(_A, B) -> B.

ifelse(Cond, A, B) ->
    case Cond of
	true -> A;
	_ -> B
    end.	    

bin_to_list(B) when binary(B) ->
    binary_to_list(B);
bin_to_list(L) when list(L) ->
    lists:flatten([bin_to_list(A) || A <- L]);
bin_to_list(I) when integer(I) ->
    I.

start_shell(CM, State) ->
    Shell = State#state.shell,
    ?dbg(true, "start_shell: self()=~p CM=~p Shell=~p\n",
	 [self(), CM, Shell]),
    ShellFun = case is_function(Shell) of
		   true ->
		       case erlang:fun_info(Shell, arity) of
			   {arity, 1} ->
			       {ok, User} = ssh_userreg:lookup_user(CM),
			       fun() -> Shell(User) end;
			   {arity, 2} ->
			       {ok, User} = ssh_userreg:lookup_user(CM),
			       {ok, PeerAddr} = ssh_cm:get_peer_addr(CM),
			       fun() -> Shell(User, PeerAddr) end;
			   _ ->
			       Shell
		       end;
		   _ ->
		       Shell
	       end,
    Echo = get_echo(State#state.pty),
    Group = group:start(self(), ShellFun, [{echo, Echo}]),
    State#state{group = Group, buf = empty_buf()}.

check_subsystem(SsName, Options) ->
    Spec = ssh_sftpd:subsystem_spec(Options),
    DefaultSubSys = [Spec],
    Subsystems = proplists:get_value(subsystems, 
				     Options, DefaultSubSys),
    proplists:get_value(SsName, Subsystems, none).


% Pty can be undefined if the client never sets any pty options before
% starting the shell.
get_echo(undefined) ->
    true;
get_echo(#ssh_pty{modes = Modes}) ->
    case proplists:get_value(echo, Modes, 1) of 
	1 ->
	    true;
	0 ->
	    false
    end.

% Group is undefined if the pty options are sent between open and
% shell messages.
set_echo(#state{group = undefined}) ->
    ok;
set_echo(#state{group = Group, pty = Pty}) ->
    Echo = get_echo(Pty),
    Group ! {self(), echo, Echo}.

empty_mailbox_workaround(Pid) ->
    receive 
	{ssh_cm, _, _} = Msg ->
 	    Pid ! Msg,
 	    empty_mailbox_workaround(Pid)
    after 0 ->
 	    ok
    end.

	    
