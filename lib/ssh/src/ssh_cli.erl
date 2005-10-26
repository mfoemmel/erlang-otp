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
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_cli).

-behaviour(gen_server).

-include("ssh.hrl").
-include("ssh_connect.hrl").

%% API
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
	  expand_fun
	 }).

%%====================================================================
%% API
%%====================================================================
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

listen(Shell, Addr, Port, Opts) ->
    ssh_cm:listen(
      fun() ->
	      {ok, Pid} =
		  gen_server:start_link(?MODULE, [Shell, Opts], []),
	      Pid
      end, Addr, Port, Opts).

%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok
%% Description: Stops the listener
%%--------------------------------------------------------------------
stop(Pid) ->
    ssh_cm:stop_listener(Pid).

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
init([Shell, Opts]) ->
    {ok, #state{shell = Shell,
		expand_fun = proplists:get_value(expand_fun, Opts)}}.

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
handle_info({ssh_cm, CM, {open, Channel, {session}}}, State) ->
    ?dbg(true, "session open: self()=~p CM=~p Channel=~p\n",
	 [self(), CM, Channel]),
    GroupOpts = case State#state.expand_fun of
		    false -> [];
		    Fun -> [{expand_fun, Fun}]
		end,
    Group = group:start(self(), State#state.shell, GroupOpts),
    process_flag(trap_exit, true),
    {noreply,
     State#state{cm = CM, channel = Channel,
		 buf = empty_buf(), group = Group}};
handle_info({ssh_cm, _CM, {data, _Channel, _Type, Data}}, State) ->
    State#state.group ! {self(), {data, binary_to_list(Data)}},
    {noreply, State};
handle_info({ssh_cm, _CM, {pty, _Channel, _WantReply, Pty}}, State) ->
    {noreply, State#state{pty = Pty}};
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
handle_info({ssh_cm, _CM, {shell}}, State) ->
    {noreply, State};
handle_info({ssh_cm, _CM, {exec, Cmd}}, State) ->
    State#state.group ! {self(), {data, Cmd ++ "\n"}},
    {noreply, State};
handle_info({ssh_cm, _CM, {eof, _Channel}}, State) ->
    {stop, normal, State};
handle_info({'EXIT', Group, normal}, State) when Group==State#state.group ->
    {stop, normal, State};
handle_info(_Info, State) ->
    ?dbg(true, "~p:handle_info: BAD info ~p\n(State ~p)\n", [?MODULE, _Info, State]),
    {noreply, State}.

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
    window_change(Buf, OldTty, Tty);
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
io_request({requests,Rs}, Buf, Tty) ->
    io_requests(Rs, Buf, Tty, []);
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
%% characters below 32 (except for cr and lf) are
%% padded (with 10s), so the buf will bytewise be as wide as 
%% the printed result
conv_buf([], AccBuf, AccBufTail, AccWrite, Col) ->
    {AccBuf, AccBufTail, lists:reverse(AccWrite), Col};
conv_buf([13, 10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl2(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([13 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([9 | Rest], AccBuf, AccBufTail, AccWrite, Col) ->
    NSpaces = (Col + (?TABWIDTH - 1)) rem ?TABWIDTH + 1,
    AccB = string:chars(?PAD, NSpaces-1) ++ [9 | AccBuf],
    AccW = string:chars(32, NSpaces) ++ [AccWrite],
    AccBT = nthtail(NSpaces, AccBufTail),
    conv_buf(Rest, AccB, AccBT, AccW, Col + NSpaces);
conv_buf([C | Rest], AccBuf, AccBufTail, AccWrite, Col) when C < 32 ->
    AccB = [10, 10, 10, C | AccBuf],
    AccW = [oct_dig(C, 2), oct_dig(C, 1), oct_dig(C, 0), "\\" | AccWrite],
    conv_buf(Rest, AccB, tl4(AccBufTail), AccW, Col + 4);
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
window_change({Buf, BufTail, Col}, OldTty, Tty)
  when OldTty#ssh_pty.width == Tty#ssh_pty.width ->
    {Buf, BufTail, Col};
window_change({Buf, BufTail, Col}, OldTty, Tty) ->
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
write_chars(CM, Channel, Chars) ->
    Type = 0,
    CM ! {ssh_cm, self(), {data, Channel, Type, Chars}}.

%%% tail, works with empty lists
tl1([_|A]) -> A;
tl1(_) -> [].

%%% second tail
tl2([_,_|A]) -> A;
tl2(_) -> [].

%%% fourth tail
tl4([_,_,_,_|A]) -> A;
tl4(_) -> [].

%%% nthtail as in lists, but no badarg if n > the length of list
nthtail(0, A) -> A;
nthtail(N, [_ | A]) when N > 0 -> nthtail(N-1, A);
nthtail(_, _) -> [].

%%% the octal digit of a number (0 is least significant)
oct_dig(N, D) ->
    ((N bsr (D*3)) band 7) + $0.

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

