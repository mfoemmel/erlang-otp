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
%% Portions created by Ericsson are Copyright 2002, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $
%%

-module(bin_io).

-export([open_read/1,open_create/0,open_append/1,close/1]).

-record(state, {mode,bin,buf,read_mode}).

-define(READ_SIZE, 256).			%Bytes per chunk read

%% The main interface.

open_read(Bin) when binary(Bin) ->
    {ok,spawn(fun () ->
		      server_loop(#state{mode=read,bin=Bin,buf=Bin,
					 read_mode=list})
	      end)}.

open_create() ->
    {ok,spawn(fun () ->
		      server_loop(#state{mode=write,bin= <<>>,buf=[],
					 read_mode=list})
	      end)}.

open_append(Bin) when binary(Bin) ->
    {ok,spawn(fun () ->
		      server_loop(#state{mode=write,bin=Bin,buf=[],
					 read_mode=list})
	      end)}.

close(Io) ->
    Io ! {bin_request,self(),Io,close},
    receive
	{bin_reply,Io,Rep} -> Rep
    end.

%% server_loop(State) -> void().
%%  Main server loop.  We never return.

server_loop(St0) ->
    receive
	{bin_request,From,ReplyAs,Req} when pid(From) ->
	    %% Handle bin_io specific requests.
	    case bin_request(Req, St0) of
		{ok,Rep,St1} ->
		    bin_reply(From, ReplyAs, Rep),
		    server_loop(St1);
		{error,Rep,St1} ->
		    bin_reply(From, ReplyAs, Rep),
		    server_loop(St1);
		{stop,Reason,Rep,St1} ->
		    bin_reply(From, ReplyAs, Rep),
		    exit(Reason)
	    end;
	{io_request,From,ReplyAs,Req} when pid(From) ->
	    %% Handle general io requests.
	    case io_request(Req, St0) of
		{ok,Rep,St1} ->
		    io_reply(From, ReplyAs, Rep),
		    server_loop(St1);
		{error,Rep,St1} ->
		    io_reply(From, ReplyAs, Rep),
		    server_loop(St1);
		{stop,Reason,Rep,St1} ->
		    io_reply(From, ReplyAs, Rep),
		    exit(Reason)
	    end;
	{file_request, From, ReplyAs, close} ->
	    %% io:format("closing bin~n"),
	    From ! {file_reply, ReplyAs, ok},
	    exit(normal);
	Unknown ->
	    io:format("bin_io oops~p~n",[Unknown]),
	    %% Ignore other unknown messages.
	    server_loop(St0)
    end.

bin_reply(From, ReplyAs, Reply) ->
    From ! {bin_reply, ReplyAs, Reply}.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

%% bin_request(Request, State) ->
%%      {ok,Reply,State} | {error,Reply,State} | {stop,Reason,Reply,State}.
%%
%%  Handle bin_io specific requests.

bin_request(close, #state{mode=Mode,bin=Bin}=State) ->
    Rep = case Mode of
	      read -> ok;
	      write -> {ok,list_to_binary(Bin)}
	  end,
    {stop,normal,Rep,State#state{buf=[]}};
bin_request(Unknown, State) ->
    Reason = {request,Unknown},
    {error,{error,Reason},State}.

%% io_request(Request, State) ->
%%      {ok,Reply,State} | {error,Reply,State} | {stop,Reason,Reply,State}.
%%
%%  Handle general io requests.


io_request({put_chars,Chars}, #state{mode=write,bin=Bin}=St) 
  when binary(Chars) ->						% New in R9C
    {ok,ok,St#state{bin=[Bin|Chars]}};
io_request({put_chars,Chars}, #state{mode=write,bin=Bin}=St) ->
    case catch list_to_binary(Chars) of
	{'EXIT',Reason} ->
	    {stop,Reason,{error,Reason},St};
	MoreBin -> {ok,ok,St#state{bin=[Bin|MoreBin]}}
    end;
io_request({put_chars,Mod,Func,Args}, #state{mode=write}=St) ->
    case catch apply(Mod, Func, Args) of
	Chars when list(Chars) -> io_request({put_chars,Chars}, St);
	Bin when binary(Bin) -> io_request({put_chars,[Bin]}, St);
	Other -> {error,{error,Func},St}
    end;
%% These are new in R9C
io_request({get_chars,_Prompt,N}, #state{mode=read}=St) ->
    get_chars(io_lib, collect_chars, N, St);
io_request({get_chars,_Prompt,Mod,Func,XtraArg}, #state{mode=read}=St) ->
    get_chars(Mod, Func, XtraArg, St);
io_request({get_line,_Prompt}, #state{mode=read}=St) ->
    get_chars(io_lib, collect_line, [], St);
io_request({setopts,Opts}, St) when list(Opts) ->
    setopts(Opts, St);
%% End of new in R9C
io_request({get_until,_Prompt,Mod,Func,ExtraArgs}, #state{mode=read}=St) ->
    get_until(Mod, Func, ExtraArgs, St);
io_request({requests,Reqs}, St) when list(Reqs) ->
    io_request_loop(Reqs, {ok,ok,St});
io_request(Unknown, State) ->
    Reason = {error,Unknown},
    {error,{error,Reason},State}.

%% io_request_loop([Request], Result) -> Result.
%%  Process list of requests as long as results are ok.

io_request_loop([], Res) -> Res;
io_request_loop([Req|Reqs], {ok,Rep,St}) ->
    io_request_loop(Reqs, io_request(Req, St));
io_request_loop([Req|Reqs], Res) -> Res.

%% setopts
setopts(Opts0, St) ->
    Opts = proplists:substitute_negations([{list,binary}], Opts0),
    case proplists:get_value(binary, Opts) of
	true ->
	    {ok,ok,St#state{read_mode=binary}};
	false ->
	    {ok,ok,St#state{read_mode=list}};
	_ ->
	    {error,{error,badarg},St}
    end.

%% get_chars(Module, Func, ExtraArg, State) ->
%%      {ok,Reply,State} | {error,Reply,State}.
%%  Apply the get_chars loop scanning the binary until the scan
%%  function has enough.  Buffer any remaining bytes until the next
%%  call.
get_chars(M, F, Xa, St) ->
    get_chars_loop(M, F, Xa, St, start).

get_chars_loop(M, F, Xa, #state{buf=[Buf|Bin],read_mode=ReadMode}=St, State) ->
    get_chars_apply(M, F, Xa, St#state{buf=Bin}, State, Buf);
get_chars_loop(M, F, Xa, #state{buf=Bin}=St, State) ->
    case size(Bin) of
	0 ->
	    get_chars_apply(M, F, Xa, St, State, eof);
	N when N < ?READ_SIZE ->
	    get_chars_apply(M, F, Xa, St#state{buf= <<>>}, State, Bin);
	N ->
	    {B1,B2} = split_binary(Bin, ?READ_SIZE),
	    get_chars_apply(M, F, Xa, St#state{buf=B2}, State, B1)
    end.

get_chars_apply(M, F, Xa, #state{buf=Bin,read_mode=ReadMode}=St, State, Buf) ->
    case catch M:F(State, cast(Buf, ReadMode), Xa) of
	{stop,Res,[]} ->
	    {ok,Res,St};
	{stop,Res,<<>>} ->
	    {ok,Res,St};
	{stop,Res,NewBuf} ->
	    {ok,Res,St#state{buf=[NewBuf|Bin]}};
	{'EXIT',_} ->
	    {error,{error,F},St};
	NewState ->
	    get_chars_loop(M, F, Xa, St, NewState)
    end.

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F;
err_func(_, F, _) ->
    F.

cast(L, binary) when list(L) ->
    list_to_binary(L);
cast(B, list) when binary(B) ->
    binary_to_list(B);
cast(Eof, _undefined) ->
    Eof.

%% get_until(Module, Func, [ExtraArg], State) ->
%%      {ok,Reply,State} | {error,Reply,State}.
%%  Apply the get_until loop scanning the binary until the scan
%%  function has enough.  Buffer any remaining bytes until the next
%%  call.

get_until(Mod, Func, ExtraArgs, #state{buf=[Buf|Bin]}=St) ->
    get_until_loop(Mod, Func, ExtraArgs, St#state{buf=Bin},
		   catch apply(Mod, Func, [[],Buf|ExtraArgs]));
get_until(Mod, Func, ExtraArgs, #state{buf=Bin}=St) ->
    get_until_loop(Mod, Func, ExtraArgs, St#state{buf=Bin}, {more,[]}).

get_until_loop(M, F, As, #state{buf=Bin}=St, {more,Cont}) ->
    case size(Bin) of
	0 -> get_until_loop(M, F, As, St, catch apply(M, F, [Cont,eof|As]));
	S when S < ?READ_SIZE ->
	    get_until_loop(M, F, As, St#state{buf= <<>>},
			   catch apply(M, F, [Cont,binary_to_list(Bin)|As]));
	S ->
	    {B1,B2} = split_binary(Bin, ?READ_SIZE),
	    get_until_loop(M, F, As, St#state{buf=B2},
			   catch apply(M, F, [Cont,binary_to_list(B1)|As]))
    end;
get_until_loop(M, F, As, St0, {done,Res,Buf}) ->
    St1 = if Buf == [] -> St0;
	     true -> St0#state{buf=[Buf|St0#state.buf]}
	  end,
    {ok,Res,St1};
get_until_loop(M, F, As, St, Other) ->
    {error,{error,F},St}.

