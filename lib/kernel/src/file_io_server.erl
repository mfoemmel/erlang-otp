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
-module(file_io_server).

%% A simple file server for io to one file instance per server instance.

-export([format_error/1]).
-export([start/3, start_link/3]).

-record(state, {handle, owner, mref, buf}).

-define(PRIM_FILE, prim_file).
-define(READ_SIZE, 128).

-define(eat_message(M, T), receive M -> M after T -> timeout end).

%%%-----------------------------------------------------------------
%%% Exported functions

format_error({Line, ?MODULE, Reason}) ->
    io_lib:format("~w", [Reason]);
format_error({Line, Mod, Reason}) ->
    Mod:format_error(Reason);
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

start(Owner, FileName, ModeList) 
  when pid(Owner), list(FileName), list(ModeList) ->
    do_start(spawn, Owner, FileName, ModeList).

start_link(Owner, FileName, ModeList) 
  when pid(Owner), list(FileName), list(ModeList) ->
    do_start(spawn_link, Owner, FileName, ModeList).

%%%-----------------------------------------------------------------
%%% Server starter, dispatcher and helpers

do_start(Spawn, Owner, FileName, ModeList) ->
    Self = self(),
    Ref = make_ref(),
    Pid = 
	erlang:Spawn(
	  fun() ->
		  %% process_flag(trap_exit, true),
		  case ?PRIM_FILE:open(FileName, ModeList) of
		      {error, Reason} = E ->
			  exit(Reason);
		      {ok, Handle} ->
			  %% XXX must I handle R6 nodes here?
			  M = erlang:monitor(process, Owner),
			  Self ! {Ref, ok},
			  server_loop(
			    #state{handle = Handle,
				   owner  = Owner, 
				   mref   = M, 
				   buf    = []})
		  end
	  end),
    Mref = erlang:monitor(process, Pid),
    receive
	{Ref, ok} ->
	    erlang:demonitor(Mref),
	    receive
		{'DOWN', Mref, _, _, Reason} ->
		    {error, Reason}
	    after 0 ->
		    {ok, Pid}
	    end;
	{'DOWN', Mref, _, _, Reason} ->
	    {error, Reason}
    end.

server_loop(#state{mref = Mref} = State) ->
    receive
	{file_request, From, ReplyAs, Request} when pid(From) ->
	    case file_request(Request, State) of
		{reply, Reply, NewState} ->
		    file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    file_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	{io_request, From, ReplyAs, Request} when pid(From) ->
	    case io_request(Request, State) of
		{reply, Reply, NewState} ->
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    io_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	{'DOWN', Mref, _, _, Reason} ->
	    exit(Reason);
	_ ->
	    server_loop(State)
    end.

file_reply(From, ReplyAs, Reply) ->
    From ! {file_reply, ReplyAs, Reply}.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

%%%-----------------------------------------------------------------
%%% file requests

file_request({pread, At, Sz}, 
	     #state{handle = Handle, buf = Buf} = State) ->
    case position(Handle, At, Buf) of
	{ok, _Offs} ->
	    case ?PRIM_FILE:read(Handle, Sz) of
		{error, _Reason} = Reply ->
		    {error, Reply, State#state{buf = []}};
		Reply ->
		    {reply, Reply, State#state{buf = []}}
	    end;
	Reply ->
	    {error, Reply, State#state{buf = []}}
    end;
file_request({pwrite, At, Bytes}, 
	     #state{handle = Handle, buf = Buf} = State) ->
    case position(Handle, At, Buf) of
	{ok, _Offs} ->
	    case ?PRIM_FILE:write(Handle, Bytes) of
		{error, _Reason} = Reply ->
		    {error, Reply, State#state{buf = []}};
		Reply ->
		    {reply, Reply, State#state{buf = []}}
	    end;
	Reply ->
	    {error, Reply, State#state{buf = []}}
    end;
file_request(sync, 
	     #state{handle = Handle} = State) ->
    case ?PRIM_FILE:sync(Handle) of
	{error, Reason} = Error ->
	    {stop, normal, Error, State};
	Reply ->
	    {reply, Reply, State}
    end;
file_request(close, 
	     #state{handle = Handle} = State) ->
    Reply = ?PRIM_FILE:close(Handle),
    {stop, normal, Reply, State#state{buf = []}};
file_request({position, At}, 
	     #state{handle = Handle, buf = Buf} = State) ->
    case position(Handle, At, Buf) of
	{error, _Reason} = Reply ->
	    {error, Reply, State#state{buf = []}};
	Reply ->
	    {reply, Reply, State#state{buf = []}}
    end;
file_request(truncate, 
	     #state{handle = Handle} = State) ->
    case ?PRIM_FILE:truncate(Handle) of
	{error, Reason} = Reply ->
	    {stop, normal, Reply, State#state{buf = []}};
	Reply ->
	    {reply, Reply, State}
    end;
file_request(Unknown, 
	     #state{} = State) ->
    Reason = {request, Unknown},
    {error, {error, Reason}, State}.

%%%-----------------------------------------------------------------
%%% io request 

io_request({put_chars, Chars}, #state{handle = Handle} = State) ->
    case ?PRIM_FILE:write(Handle, Chars) of
	{error, Reason} = Reply ->
	    {stop, normal, Reply, State};
	Reply ->
	    {reply, Reply, State}
    end;
io_request({put_chars, Mod, Func, Args}, #state{} = State) ->
    case catch apply(Mod, Func, Args) of
	Chars when list(Chars); binary(Chars) ->
	    io_request({put_chars, Chars}, State);
	_ ->
	    {error, {error, Func}, State}
    end;
io_request({get_until, _Prompt, Mod, Func, XtraArgs}, #state{} = State) ->
    get_until(Mod, Func, XtraArgs, State);
io_request({requests, Requests}, #state{} = State) when list(Requests) ->
    io_request_loop(Requests, {reply, ok, State});
io_request(Unknown, #state{} = State) ->
    Reason = {request, Unknown},
    {error, {error, Reason}, State}.



%% Process a list of requests as long as the results are ok.

io_request_loop([], Result) ->
    Result;
io_request_loop([_Request | _Tail], 
		{stop, _Reason, _Reply, _State} = Result) ->
    Result;
io_request_loop([_Request | _Tail],
		{error, _Reply, _State} = Result) ->
    Result;
io_request_loop([Request | Tail], 
		{reply, _Reply, State}) ->
    io_request_loop(Tail, io_request(Request, State)).


%% Process the io request get_until

get_until(Mod, Func, XtraArgs, #state{buf = []} = State) ->
    get_until_loop(Mod, Func, XtraArgs, State,
		   {more, []});
get_until(Mod, Func, XtraArgs, #state{buf = <<>>} = State) ->
    get_until_loop(Mod, Func, XtraArgs, State,
		   {more, []});
get_until(Mod, Func, XtraArgs, #state{buf = Buf} = State) ->
    get_until_loop(Mod, Func, XtraArgs, State,
		   catch apply(Mod, Func, [[], Buf | XtraArgs])).

get_until_loop(Mod, Func, XtraArgs, #state{handle = Handle} = State, 
	       {more, Cont}) ->
    case ?PRIM_FILE:read(Handle, ?READ_SIZE) of 
	eof ->
	    get_until_loop(
	      Mod, Func, XtraArgs, State#state{buf = []},
	      catch apply(Mod, Func, [Cont, eof | XtraArgs]));
	{ok, Buf} when binary(Buf) ->
	    get_until_loop(
	      Mod, Func, XtraArgs, State#state{buf = Buf},
	      catch apply(Mod, Func, 
			  [Cont, binary_to_list(Buf) | XtraArgs]));
	{ok, Buf} ->
	    get_until_loop(
	      Mod, Func, XtraArgs, State#state{buf = Buf},
	      catch apply(Mod, Func, [Cont, Buf | XtraArgs]));
	{error, Reason} = Error ->
	    {stop, Reason, Error, State#state{buf = []}}
    end;
get_until_loop(_Mod, _Func, _XtraArgs, #state{} = State,
	      {done, Result, Buf}) ->
    {reply, Result, State#state{buf = Buf}};
get_until_loop(_Mod, Func, _XtraArgs, #state{} = State,
	      _Other) ->
    {error, {error, Func}, State}.

%%%-----------------------------------------------------------------
%%% ?PRIM_FILE helpers

%% Compensates ?PRIM_FILE:position/2 for the number of bytes 
%% we have buffered

position(Handle, cur, Buf) ->
    position(Handle, {cur, 0}, Buf);
position(Handle, {cur, Offs}, Buf) when list(Buf) ->
    ?PRIM_FILE:position(Handle, {cur, Offs-length(Buf)});
position(Handle, {cur, Offs}, Buf) when binary(Buf) ->
    ?PRIM_FILE:position(Handle, {cur, Offs-size(Buf)});
position(Handle, At, _Buf) ->
    ?PRIM_FILE:position(Handle, At).

