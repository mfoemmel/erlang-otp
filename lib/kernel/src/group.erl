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

-export([start/2,server/2]).

-import(io_lib, [deep_char_list/1]).

start(Drv, Shell) ->
    spawn_link(group, server, [Drv, Shell]).

server(Drv, Shell) ->
    process_flag(trap_exit, true),
    edlin:init(),
    put(line_buffer, []),
    start_shell(Shell),
    server_loop(Drv, get(shell), []).

%% start_shell(Shell)
%%  Spawn a shell with its group_leader from the beginning set to ourselves.
%%  If Shell a pid the set its group_leader.

start_shell({Mod,Func,Args}) ->
    start_shell1(Mod, Func, Args);
start_shell({Node,Mod,Func,Args}) ->
    start_shell1(net, call, [Node,Mod,Func,Args]);
start_shell(Shell) when atom(Shell) ->
    start_shell1(Shell, start, []);
start_shell(Shell) when pid(Shell) ->
    group_leader(self(), Shell),		%We are the shells group leader
    link(Shell),				%We're linked to it.
    put(shell, Shell);
start_shell(Shell) ->
    ok.

start_shell1(M, F, Args) ->
    G = group_leader(),
    group_leader(self(), self()),
    Shell = apply(M, F, Args),
    group_leader(G, self()),
    link(Shell),				%We're linked to it.
    put(shell, Shell).

server_loop(Drv, Shell, Buf0) ->
    receive
	{io_request,From,ReplyAs,Req} when pid(From) ->
	    Buf = io_request(Req, From, ReplyAs, Drv, Buf0),
	    server_loop(Drv, Shell, Buf);
	{'EXIT',Drv,interrupt} ->
	    %% Send interrupt to the shell.
	    exit_shell(interrupt),
	    server_loop(Drv, Shell, Buf0);
	{'EXIT',Drv,R} ->
	    exit(R);
	{'EXIT',Shell,R} ->
	    exit(R);
	Other ->
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

io_request({put_chars,Chars}, Drv, Buf) ->
    case deep_char_list(Chars) of
	true ->
	    send_drv(Drv, {put_chars,Chars}),
	    {ok,ok,Buf};
	false ->
	    {error,{error,put_chars},Buf}
    end;
io_request({put_chars,M,F,As}, Drv, Buf) ->
    case deep_char_list(Chars = (catch apply(M, F, As))) of
	true ->
	    send_drv(Drv, {put_chars,Chars}),
	    {ok,ok,Buf};
	false ->
	    {error,{error,F},Buf}
    end;
io_request({get_until,Prompt,M,F,As}, Drv, Buf0) ->
    get_until(Prompt, M, F, As, Drv, Buf0);
io_request({requests,Reqs}, Drv, Buf) ->
    io_requests(Reqs, {ok,ok,Buf}, Drv);
io_request(Other, Drv, Buf) ->
    {error,{error,request},Buf}.

%% Status = io_requests(RequestList, PrevStat, Drv)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,ok,Buf}, Drv) ->
    io_requests(Rs, io_request(R, Drv, Buf), Drv);
io_requests([R|Rs], Error, Drv) ->
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

send_drv_reqs(Drv, []) -> [];
send_drv_reqs(Drv, Rs) ->
    send_drv(Drv, {requests,Rs}).

%% get_until(Prompt, Module, Function, Arguments, Drv, Buffer)
%%  Gets characters from the input Drv as long as the applied function
%%  returns {more,Continuation}. Does not block output until input has been
%%  received.
%%  Returns:
%%	{Result,NewSaveBuffer}
%%	{error,What,NewSaveBuffer}

get_until(Prompt, M, F, As, Drv, Buf) ->
    Pbs = prompt_bytes(Prompt),
    get_until1(get_line(Buf, Pbs, Drv), Pbs, [], M, F, As, Drv).

get_until1({done,Line,RestChars}, Pbs, Cont0, M, F, As, Drv) ->
    case catch apply(M, F, [Cont0,Line|As]) of
	{done,Result,Rest} ->
	    {ok,Result, Rest++RestChars};
	{more,Cont} ->
	    LineCont = edlin:start(Pbs),
	    get_until1(get_line(RestChars, Pbs, Drv), Pbs, Cont, M, F, As, Drv);
	Other ->
	    {error,{error,F},[]}
    end;
get_until1(interrupted, Pbs, Cont, M, F, As, Drv) ->
    {error,{error,interrupted},[]};
get_until1(terminated, Pbs, Cont, M, F, As, Drv) ->
    {exit,terminated}.

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

get_line1({done,Line,Rest,Rs}, Drv, Ls) ->
    send_drv_reqs(Drv, Rs),
    put(line_buffer, [Line|lists:delete(Line, get(line_buffer))]),
    {done,Line,Rest};
get_line1({undefined,{A,none,$\^P},Cs,Cont,Rs}, Drv, Ls0) ->
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
get_line1({undefined,{A,none,$\^N},Cs,Cont,Rs}, Drv, Ls0) ->
    send_drv_reqs(Drv, Rs),
    case down_stack(Ls0) of
	{none,Ls} ->
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
get_line1({undefined,Char,Cs,Cont,Rs}, Drv, Ls) ->
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
	    {more_chars,Cont,More} = edlin:edit_line([], Cont0),
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    io_request(Req, From, ReplyAs, Drv, []), %WRONG!!!
	    send_drv_reqs(Drv, edlin:redraw_line(Cont)),
	    get_line1({more_chars,Cont,[]}, Drv, Ls);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,R} ->
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
