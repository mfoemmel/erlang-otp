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
-module(user).

%% Basic standard i/o server for user interface port.

-export([start/0, start/1, start_out/0, server/1, server/2]).
	 

%%
%% The basic server and start-up.
%%

start() ->
    start_port([eof]).

start([Mod,Fun|Args]) ->
    %% Mod,Fun,Args should return a pid. That process is supposed to act
    %% as the io port.
    Pid = apply(Mod, Fun, Args),  % This better work!
    Id = spawn(user, server, [Pid]),
    register(user, Id),
    Id.

start_out() ->
    %% Output-only version of start/0
    start_port([out]).

start_port(PortSettings) ->
    Id = spawn(user,server,[{fd,0,1},PortSettings]),
    register(user,Id),
    Id.

server(Pid) when pid(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    run(Pid).

server(PortName,PortSettings) ->
    process_flag(trap_exit, true),
    Port = open_port(PortName,PortSettings),
    run(Port).

run(P) ->
    case init:get_argument(noshell) of
	%% non-empty list -> noshell
	{ok, [H|T]} -> 
	    put(noshell, true),
	    server_loop(P, []);
	_ ->
	    group_leader(self(), self()),
	    catch_loop(P, shell:start())
    end.

catch_loop(Port, Shell) ->
    case catch server_loop(Port, []) of
	new_shell ->
	    exit(Shell, kill),
	    catch_loop(Port, shell:start());
	{'EXIT',R} ->
	    exit(R)
    end.

server_loop(Port, Buf0) ->
    receive
	{Port,{data,Bytes}} ->
	    Nosh = get(noshell),
	    case lists:member(7,Bytes) of
		true when  Nosh == undefined ->
		    throw(new_shell);
		_ ->
		    server_loop(Port,lists:append(Buf0,Bytes))
	    end;
	{io_request,From,ReplyAs,Request} when pid(From) ->
	    Buf = io_request(Request, From, ReplyAs, Port, Buf0),
	    server_loop(Port, Buf);
	{Port, eof} ->
	    put(eof, true),
	    server_loop(Port, Buf0);
	%% Ignore messages from port here.
	{'EXIT',Port,badsig} ->			%Ignore badsig errors
	    server_loop(Port, Buf0);
	{'EXIT',Port,What} ->			%Port has exited
	    exit(What);
	Other ->				%Ignore other messages
	    server_loop(Port, Buf0)
    end.

%% NewSaveBuffer = io_request(Request, FromPid, ReplyAs, Port, SaveBuffer)

io_request(Req, From, ReplyAs, Port, Buf0) ->
    case io_request(Req, Port, Buf0) of
	{Status,Reply,Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
	{exit,What} ->
	    send_port(Port, close),
	    exit(What)
    end.

io_request({put_chars,Chars}, Port, Buf) ->
    case io_lib:deep_char_list(Chars) of	%Check deep list
	true ->
	    put_port(Chars, Port),
	    {ok,ok,Buf};
	false ->
	    {error,{error,put_chars},Buf}
    end;
io_request({put_chars,Mod,Func,Args}, Port, Buf) ->
    io_request({put_chars,catch apply(Mod,Func,Args)}, Port, Buf);
io_request({get_until,Prompt,M,F,As}, Port, Buf) ->
    case get(eof) of
	undefined ->
	    get_until(Prompt, M, F, As, Port, Buf);
	true when Buf == [] ->
	    {ok, eof, Buf};
	_ ->
	    get_until(Prompt, M, F, As, Port, Buf)
    end;
io_request({requests,Reqs}, Port, Buf) ->
    io_requests(Reqs, {ok,ok,Buf}, Port);
io_request(R, Port, Buf) ->			%Unknown request
    {ok,{error,{request,R}},Buf}.		%Ignore but give error (?)

%% Status = io_requests(RequestList, PrevStat, Port)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,Res,Buf}, Port) ->
    io_requests(Rs, io_request(R, Port, Buf), Port);
io_requests([_|_], Error, _) ->
    Error;
io_requests([], Stat, _) ->
    Stat.

%% put_port(DeepList, Port)
%%  Take a deep list of characters, flatten and output them to the
%%  port.

put_port(List, Port) ->
    send_port(Port, {command, List}).

%% send_port(Port, Command)

send_port(Port, Command) ->
    Port ! {self(),Command}.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.

%% get_until(Prompt, Module, Function, Arguments, Port, Buffer)
%%  Gets characters from the input port as long as the applied function
%%  returns {more,Continuation}. Does not block output until input has been
%%  received.
%%  Returns:
%%	{Status,Result,NewSaveBuffer}
%%	{exit,Reason}

get_until(Prompt, M, F, As, Port, Buf) ->
    prompt(Port, Prompt),
    get_until1(Prompt, M, F, As, Port, Buf).

get_until1(Prompt, M, F, As, Port, []) ->
    receive
	{Port,{data,Bytes}} ->
	    Nosh = get(noshell),
	    case lists:member(7,Bytes) of
		true when Nosh == undefined ->
		    throw(new_shell);
		_ ->
		    get_until2(catch apply(M, F, [[],Bytes|As]),
			       M, F, As, Port)
	    end;
	{Port, eof} ->
	    put(eof, true),
	    {ok, eof, []};
%%	{io_request,From,ReplyAs,Request} when pid(From) ->
%%	    get_until1_out(Request, From, ReplyAs, Prompt, M, F, As, Port);
	{io_request,From,ReplyAs,{put_chars,Chars}} when pid(From) ->
	    get_until1_out({put_chars,Chars}, From, ReplyAs,
			   Prompt, M, F, As, Port);
	{io_request,From,ReplyAs,{put_chars,M1,F1,A1}} when pid(From) ->
	    get_until1_out({put_chars,M1,F1,A1}, From, ReplyAs,
			   Prompt, M, F, As, Port);
	{'EXIT',From,What} when node(From) == node() ->
	    {exit,What}
    end;

get_until1(Prompt, M, F, As, Port, Buf) ->
    get_until2(catch apply(M, F, [[],Buf|As]), M, F, As, Port).

get_until1_out(Req, From, ReplyAs, Prompt, M, F, As, Port) ->
    io_request(Req, From, ReplyAs, Port, []),	%No new buf here!
    prompt(Port, Prompt),
    get_until1(Prompt, M, F, As, Port, []).

get_until2({more,Cont}, M, F, As, Port) ->
    case get(eof) of
	undefined ->
	    receive
		{Port,{data,Bytes}} ->
		    Nosh = get(noshell),
		    case lists:member(7,Bytes) of
			true when Nosh == undefined ->
			    throw(new_shell);
			_ ->
			    get_until2(catch apply(M, F, [Cont,Bytes|As]),
				       M, F, As, Port)
		    end;
		{Port, eof} ->
		    put(eof, true),
		    get_until2(catch apply(M, F, [Cont,eof|As]),M, F,As, Port);
		{'EXIT',From,What} when node(From) == node() ->
		    {exit,What}
	    end;
	_ ->
	    get_until2(catch apply(M, F, [Cont,eof|As]),M, F,As, Port)
    end;
get_until2({done,Result,Buf}, M, F, As, Port) ->
    {ok,Result,Buf};
get_until2(Other, M, F, As, Port) ->
    {error,{error,get_until},[]}.

%% prompt(Port, Prompt)
%%  Print Prompt onto port Port, special case just atoms and print unquoted.

prompt(Port, Prompt) when atom(Prompt) ->
    List = io_lib:format('~s', [Prompt]),
    put_port(List, Port);
prompt(Port, {format,Format,Args}) ->
    case catch io_lib:format(Format,Args) of
	{'EXIT',_} ->
	    put_port("???", Port);
	List ->
	    put_port(List, Port)
    end;
prompt(Port, Prompt) ->
    List = io_lib:write(Prompt),
    put_port(List, Port).
