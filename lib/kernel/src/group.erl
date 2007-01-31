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
-module(group).

%% A group leader process for user io.

-export([start/2, start/3, server/3]).
-export([interfaces/1]).

-import(io_lib, [deep_char_list/1]).

start(Drv, Shell) ->
    start(Drv, Shell, []).

start(Drv, Shell, Options) ->
    spawn_link(group, server, [Drv, Shell, Options]).

server(Drv, Shell, Options) ->
    process_flag(trap_exit, true),
    edlin:init(),
    put(line_buffer, proplists:get_value(line_buffer, Options, [])),
    put(read_mode, list),
    put(user_drv, Drv),
    put(expand_fun,
	proplists:get_value(expand_fun, Options,
			    fun(B) -> edlin_expand:expand(B) end)),
    start_shell(Shell),
    server_loop(Drv, get(shell), []).

%% Return the pid of user_drv and the shell process.
%% Note: We can't ask the group process for this info since it
%% may be busy waiting for data from the driver.
interfaces(Group) ->
    case process_info(Group, dictionary) of
	{dictionary,Dict} ->
	    get_pids(Dict, [], false);
	_ ->
	    []
    end.

get_pids([Drv = {user_drv,_} | Rest], Found, _) ->
    get_pids(Rest, [Drv | Found], true);
get_pids([Sh = {shell,_} | Rest], Found, Active) ->
    get_pids(Rest, [Sh | Found], Active);
get_pids([_ | Rest], Found, Active) ->
    get_pids(Rest, Found, Active);
get_pids([], Found, true) ->
    Found;
get_pids([], _Found, false) ->
    [].

%% start_shell(Shell)
%%  Spawn a shell with its group_leader from the beginning set to ourselves.
%%  If Shell a pid the set its group_leader.

start_shell({Mod,Func,Args}) ->
    start_shell1(Mod, Func, Args);
start_shell({Node,Mod,Func,Args}) ->
    start_shell1(net, call, [Node,Mod,Func,Args]);
start_shell(Shell) when atom(Shell) ->
    start_shell1(Shell, start, []);
start_shell(Shell) when is_function(Shell) ->
    start_shell1(Shell);
start_shell(Shell) when pid(Shell) ->
    group_leader(self(), Shell),		% we are the shells group leader
    link(Shell),				% we're linked to it.
    put(shell, Shell);
start_shell(_Shell) ->
    ok.

start_shell1(M, F, Args) ->
    G = group_leader(),
    group_leader(self(), self()),
    case catch apply(M, F, Args) of
	Shell when pid(Shell) ->
	    group_leader(G, self()),
	    link(Shell),			% we're linked to it.
	    put(shell, Shell);
	Error ->				% start failure
	    exit(Error)				% let the group process crash
    end.

start_shell1(Fun) ->
    G = group_leader(),
    group_leader(self(), self()),
    case catch Fun() of
	Shell when pid(Shell) ->
	    group_leader(G, self()),
	    link(Shell),			% we're linked to it.
	    put(shell, Shell);
	Error ->				% start failure
	    exit(Error)				% let the group process crash
    end.

server_loop(Drv, Shell, Buf0) ->
    receive
	{io_request,From,ReplyAs,Req} when pid(From) ->
	    Buf = io_request(Req, From, ReplyAs, Drv, Buf0),
	    server_loop(Drv, Shell, Buf);
	{driver_id,ReplyTo} ->
	    ReplyTo ! {self(),driver_id,Drv},
	    server_loop(Drv, Shell, Buf0);
	{'EXIT',Drv,interrupt} ->
	    %% Send interrupt to the shell.
	    exit_shell(interrupt),
	    server_loop(Drv, Shell, Buf0);
	{'EXIT',Drv,R} ->
	    exit(R);
	{'EXIT',Shell,R} ->
	    exit(R);
	_ ->
	    server_loop(Drv, Shell, Buf0)
    end.

exit_shell(Reason) ->
    case get(shell) of
	undefined -> true;
	Pid -> exit(Pid, Reason)
    end.

io_request(Req, From, ReplyAs, Drv, Buf0) ->
    case io_request(Req, Drv, Buf0) of
	{ok,Reply,Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
	{error,Reply,Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
	{exit,R} ->
	    %% 'kill' instead of R, since the shell is not always in
	    %% a state where it is ready to handle a termination
	    %% message.
	    exit_shell(kill),
	    exit(R)
    end.

io_request({put_chars,Binary}, Drv, Buf) when binary(Binary) -> % New in R9C
    send_drv(Drv, {put_chars,Binary}),
    {ok,ok,Buf};
io_request({put_chars,Chars}, Drv, Buf) ->
    case catch list_to_binary(Chars) of
	Binary when binary(Binary) ->
	    send_drv(Drv, {put_chars,Binary}),
	    {ok,ok,Buf};
	_ ->
	    {error,{error,{put_chars,Chars}},Buf}
    end;
io_request({put_chars,M,F,As}, Drv, Buf) ->
    case catch apply(M, F, As) of
	Binary when binary(Binary) ->
	    send_drv(Drv, {put_chars,Binary}),
	    {ok,ok,Buf};
	Chars ->
	    case catch list_to_binary(Chars) of
		B when binary(B) ->
		    send_drv(Drv, {put_chars,B}),
		    {ok,ok,Buf};
		_ ->
		    {error,{error,F},Buf}
	    end
    end;
%% These are new in R9C
io_request({get_chars,Prompt,N}, Drv, Buf) ->
    get_chars(Prompt, io_lib, collect_chars, N, Drv, Buf);
io_request({get_chars,Prompt,Mod,Func,XtraArg}, Drv, Buf) ->
    get_chars(Prompt, Mod, Func, XtraArg, Drv, Buf);
io_request({get_line,Prompt}, Drv, Buf) ->
    get_chars(Prompt, io_lib, collect_line, [], Drv, Buf);
io_request({setopts,Opts}, Drv, Buf) when list(Opts) ->
    setopts(Opts, Drv, Buf);
%% End of new in R9C
io_request({get_until,Prompt,M,F,As}, Drv, Buf) ->
    get_chars(Prompt, io_lib, get_until, {M,F,As}, Drv, Buf);
io_request({requests,Reqs}, Drv, Buf) ->
    io_requests(Reqs, {ok,ok,Buf}, Drv);
io_request(get_password,Drv,Buf) ->
    get_password_chars(Drv, Buf);
io_request(_, _Drv, Buf) ->
    {error,{error,request},Buf}.

%% Status = io_requests(RequestList, PrevStat, Drv)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,ok,Buf}, Drv) ->
    io_requests(Rs, io_request(R, Drv, Buf), Drv);
io_requests([_|_], Error, _Drv) ->
    Error;
io_requests([], Stat, _) ->
    Stat.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.

%% send_drv(Drv, Message)
%% send_drv_reqs(Drv, Requests)

send_drv(Drv, Msg) ->
    Drv ! {self(),Msg}.

send_drv_reqs(_Drv, []) -> [];
send_drv_reqs(Drv, Rs) ->
    send_drv(Drv, {requests,Rs}).

%% setopts
setopts(Opts0,_Drv, Buf) ->
    Opts = proplists:substitute_negations([{list,binary}], Opts0),
    put(expand_fun, proplists:get_value(expand_fun, Opts, get(expand_fun))),
    case proplists:get_value(binary, Opts) of
	true ->
	    put(read_mode,binary),
	    {ok,ok,Buf};
	false ->
	    put(read_mode,list),
	    {ok,ok,Buf};
	_ ->
	    {error,{error,badarg},Buf}
    end.

%% get_chars(Prompt, Module, Function, XtraArgument, Drv, Buffer)
%%  Gets characters from the input Drv until as the applied function
%%  returns {stop,Result,Rest}. Does not block output until input has been
%%  received.
%%  Returns:
%%	{Result,NewSaveBuffer}
%%	{error,What,NewSaveBuffer}

get_password_chars(Drv,Buf) ->
    case get_password_line(Buf, Drv) of
	{done, Line, Buf1} ->
	    {ok, Line, Buf1};
	interrupted ->
	    {error, {error, interrupted}, []};
	terminated ->
	    {exit, terminated}
    end.

get_chars(Prompt, M, F, Xa, Drv, Buf) ->
    Pbs = prompt_bytes(Prompt),
    get_chars_loop(Pbs, M, F, Xa, Drv, Buf, start).

get_chars_loop(Pbs, M, F, Xa, Drv, Buf0, State) ->
    case get_line(Buf0, Pbs, Drv) of
	{done,Line,Buf1} ->
	    get_chars_apply(Pbs, M, F, Xa, Drv, Buf1, State, Line);
	interrupted ->
	    {error,{error,interrupted},[]};
	terminated ->
	    {exit,terminated}
    end.

get_chars_apply(Pbs, M, F, Xa, Drv, Buf, State0, Line) ->
    case catch M:F(State0, cast(Line,get(read_mode)), Xa) of
	{stop,Result,Rest} ->
	    {ok,Result,append(Rest, Buf)};
	{'EXIT',_} ->
	    {error,{error,err_func(M, F, Xa)},[]};
	State1 ->
	    edlin:start(Pbs),
	    get_chars_loop(Pbs, M, F, Xa, Drv, Buf, State1)
    end.

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F;
err_func(_, F, _) ->
    F.

%% get_line(Chars, PromptBytes, Drv)
%%  Get a line with eventual line editing. Handle other io requests
%%  while getting line.
%%  Returns:
%%	{done,LineChars,RestChars}
%%	interrupted

get_line(Chars, Pbs, Drv) ->
    {more_chars,Cont,Rs} = edlin:start(Pbs),
    send_drv_reqs(Drv, Rs),
    get_line1(edlin:edit_line(Chars, Cont), Drv, new_stack(get(line_buffer))).

get_line1({done,Line,Rest,Rs}, Drv, _Ls) ->
    send_drv_reqs(Drv, Rs),
    put(line_buffer, [Line|lists:delete(Line, get(line_buffer))]),
    {done,Line,Rest};
get_line1({undefined,{_A,Mode,Char},Cs,Cont,Rs}, Drv, Ls0) 
  when ((Mode == none) and (Char == $\^P))
       or ((Mode == meta_left_sq_bracket) and (Char == $A)) ->
    send_drv_reqs(Drv, Rs),
    case up_stack(Ls0) of
	{none,Ls} ->
	    send_drv(Drv, beep),
	    get_line1(edlin:edit_line(Cs, Cont), Drv, Ls);
	{Lcs,Ls} ->
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    {more_chars,Ncont,Nrs} = edlin:start(edlin:prompt(Cont)),
	    send_drv_reqs(Drv, Nrs),
	    get_line1(edlin:edit_line1(lists:sublist(Lcs, 1, length(Lcs)-1),
				      Ncont),
		      Drv,
		      Ls)
    end;
get_line1({undefined,{_A,Mode,Char},_Cs,Cont,Rs}, Drv, Ls0) 
  when ((Mode == none) and (Char == $\^N))
       or ((Mode == meta_left_sq_bracket) and (Char == $B)) ->
    send_drv_reqs(Drv, Rs),
    case down_stack(Ls0) of
	{none,_Ls} ->
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    get_line1(edlin:start(edlin:prompt(Cont)), Drv, Ls0);
	{Lcs,Ls} ->
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    {more_chars,Ncont,Nrs} = edlin:start(edlin:prompt(Cont)),
	    send_drv_reqs(Drv, Nrs),
	    get_line1(edlin:edit_line1(lists:sublist(Lcs, 1, length(Lcs)-1),
				      Ncont),
		      Drv,
		      Ls)
    end;
get_line1({expand, Before, Cs0, Cont,Rs}, Drv, Ls0) ->
    send_drv_reqs(Drv, Rs),
    ExpandFun = get(expand_fun),
    {Found, Add, Matches} = ExpandFun(Before),
    case Found of
	no -> send_drv(Drv, beep);
	yes -> ok
    end,
    Cs1 = append(Add, Cs0),
    Cs = case Matches of
	     [] -> Cs1;
	     _ -> MatchStr = edlin_expand:format_matches(Matches),
		  send_drv(Drv, {put_chars, list_to_binary(MatchStr)}),
		  [$\^L | Cs1]
	 end,
    get_line1(edlin:edit_line(Cs, Cont), Drv, Ls0);
get_line1({undefined,_Char,Cs,Cont,Rs}, Drv, Ls) ->
    send_drv_reqs(Drv, Rs),
    send_drv(Drv, beep),
    get_line1(edlin:edit_line(Cs, Cont), Drv, Ls);
get_line1({What,Cont0,Rs}, Drv, Ls) ->
    send_drv_reqs(Drv, Rs),
    receive
	{Drv,{data,Cs}} ->
	    get_line1(edlin:edit_line(Cs, Cont0), Drv, Ls);
	{Drv,eof} ->
	    get_line1(edlin:edit_line(eof, Cont0), Drv, Ls);
	{io_request,From,ReplyAs,Req} when pid(From) ->
	    {more_chars,Cont,_More} = edlin:edit_line([], Cont0),
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    io_request(Req, From, ReplyAs, Drv, []), %WRONG!!!
	    send_drv_reqs(Drv, edlin:redraw_line(Cont)),
	    get_line1({more_chars,Cont,[]}, Drv, Ls);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,_} ->
	    terminated
    after
	get_line_timeout(What)->
	    get_line1(edlin:edit_line([], Cont0), Drv, Ls)
    end.

get_line_timeout(blink) -> 1000;
get_line_timeout(more_chars) -> infinity.

new_stack(Ls) -> {stack,Ls,{},[]}.

up_stack({stack,[L|U],{},D}) ->
    {L,{stack,U,L,D}};
up_stack({stack,[L|U],C,D}) ->
    {L,{stack,U,L,[C|D]}};
up_stack({stack,[],{},D}) ->
    {none,{stack,[],{},D}};
up_stack({stack,[],C,D}) ->
    {none,{stack,[C],{},D}}.

down_stack({stack,U,{},[L|D]}) ->
    {L,{stack,U,L,D}};
down_stack({stack,U,C,[L|D]}) ->
    {L,{stack,[C|U],L,D}};
down_stack({stack,U,{},[]}) ->
    {none,{stack,U,{},[]}};
down_stack({stack,U,C,[]}) ->
    {none,{stack,U,{},[C]}}.

%% This is get_line without line editing (except for backspace) and
%% without echo.
get_password_line(Chars, Drv) ->
    get_password1(edit_password(Chars,[]),Drv).

get_password1({Chars,[]}, Drv) ->
    receive
	{Drv,{data,Cs}} ->
	    get_password1(edit_password(Cs,Chars),Drv);
	{io_request,From,ReplyAs,Req} when pid(From) ->
	    %send_drv_reqs(Drv, [{delete_chars, -length(Pbs)}]),
	    io_request(Req, From, ReplyAs, Drv, []), %WRONG!!!
	    %% I guess the reason the above line is wrong is that Buf is
	    %% set to []. But do we expect anything but plain output?

	    send_drv_reqs(Drv, [{put_chars, Chars}]),
	    get_password1({Chars, []}, Drv);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,_} ->
	    terminated
    end;
get_password1({Chars,Rest},Drv) ->
    send_drv_reqs(Drv,[{put_chars, "\n"}]),
    {done,lists:reverse(Chars),case Rest of done -> []; _ -> Rest end}.

edit_password([],Chars) ->
    {Chars,[]};
edit_password([$\r],Chars) ->
    {Chars,done};
edit_password([$\r|Cs],Chars) ->
    {Chars,Cs};
edit_password([$\177|Cs],[]) ->       %% Being able to erase characters is
    edit_password(Cs,[]);             %% the least we should offer, but
edit_password([$\177|Cs],[_|Chars]) ->%% is backspace enough?
   edit_password(Cs,Chars);
edit_password([Char|Cs],Chars) ->
    edit_password(Cs,[Char|Chars]).

%% prompt_bytes(Prompt)
%%  Return a list of bytes for the Prompt.

prompt_bytes(Prompt) when atom(Prompt) ->
    atom_to_list(Prompt);
prompt_bytes({format,Format,Args}) ->
    case catch io_lib:format(Format,Args) of
	{'EXIT',_} ->
	    "???";
	List ->
	    lists:flatten(List)
    end;
prompt_bytes(Prompt) ->
    lists:flatten(io_lib:format("~p", [Prompt])).

cast(L, binary) when is_list(L) ->
    list_to_binary(L);
cast(Other, _) ->
    Other.

append(B, L) when is_binary(B) ->
    binary_to_list(B)++L;
append(L1, L2) when is_list(L1) ->
    L1++L2;
append(_Eof, L) ->
    L.
