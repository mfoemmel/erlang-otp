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
-module(log_mf_h).

-behaviour(gen_event).

-export([init/3, init/4]).

-export([init/1, handle_event/2, handle_info/2, terminate/2]).
-export([handle_call/2, code_change/3]). 

-record(state, {dir, maxB, maxF, curB, curF, cur_fd, index = [], pred}).

%%%-----------------------------------------------------------------
%%% This module implements an event handler that writes events
%%% to multiple files (configureable).
%%%-----------------------------------------------------------------
%% Func: init/3, init/4
%% Args: EventMgr = pid() | atom()
%%       Dir  = string()
%%       MaxB = integer() 
%%       MaxF = byte()
%%       Pred = fun(Event) -> boolean()
%% Purpose: An event handler.  Writes binary events
%%          to files in the directory Dir.  Each file is called
%%          1, 2, 3, ..., MaxF.  Writes MaxB bytes on each
%%          file.  Creates a file called 'index' in the Dir.  This
%%          file contains the last written FileName.
%%          On startup, this file is read, and the next available
%%          filename is used as first logfile.
%%          Each event is filtered with the predicate function Pred.
%%          Reports can be browsed with Report Browser Tool (rb).
%% Returns: Args = term()
%%          The Args term should be used in a call to
%%          gen_event:add_handler(EventMgr, log_mf_h, Args).
%%-----------------------------------------------------------------
init(Dir, MaxB, MaxF) -> init(Dir, MaxB, MaxF, fun(_) -> true end).
init(Dir, MaxB, MaxF, Pred) -> {Dir, MaxB, MaxF, Pred}.

%%-----------------------------------------------------------------
%% Call-back functions from gen_event
%%-----------------------------------------------------------------
init({Dir, MaxB, MaxF, Pred}) when is_integer(MaxF), MaxF > 0, MaxF < 256 -> 
    First = 
	case read_index_file(Dir) of
	    {ok, LastWritten} -> inc(LastWritten, MaxF);
	    _ -> 1
	end,
    case catch file_open(Dir, First) of
	{ok, Fd} -> {ok, #state{dir = Dir, maxB = MaxB, maxF = MaxF,
				pred = Pred,
				curF = First, cur_fd = Fd, curB = 0}};
	Error -> Error
    end.

%%-----------------------------------------------------------------
%% This function may crash!  In this case, this
%% handler is removed by gen_event from the event handlers.
%% Fails: 'file_open' if file:open failed for a log file.
%%        'write_index_file' if file:write_file failed for the
%%            index file.
%%        {file_exit, Reason} if the current Fd crashes. 
%%-----------------------------------------------------------------
handle_event(Event, State) ->
    #state{curB = CurB, maxB = MaxB, curF = CurF, maxF = MaxF,
	   dir = Dir, cur_fd = CurFd, pred = Pred} = State,
    case catch Pred(Event) of
	true -> 
	    Bin = term_to_binary(tag_event(Event)),
	    Size = byte_size(Bin),
	    NewState =
		if
		    CurB + Size < MaxB -> State;
		    true ->
			ok = file:close(CurFd),
			NewF = inc(CurF, MaxF),
			{ok, NewFd} = file_open(Dir, NewF),
			State#state{cur_fd = NewFd, curF = NewF, curB = 0}
		end,
	    [Hi,Lo] = put_int16(Size),
	    file:write(NewState#state.cur_fd, [Hi, Lo, Bin]),
	    {ok, NewState#state{curB = NewState#state.curB + Size + 2}};
	_ ->
	    {ok, State}
    end.

handle_info({emulator, GL, Chars}, State) ->
    handle_event({emulator, GL, Chars}, State);
handle_info(_, State) ->
    {ok, State}.

terminate(_, State) ->
    ok = file:close(State#state.cur_fd),
    State.

handle_call(null, State) ->
    {ok, null, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Misc functions
%%-----------------------------------------------------------------
file_open(Dir, FileNo) ->
    case file:open(Dir ++ [$/ | integer_to_list(FileNo)], [raw, write]) of
	{ok, Fd} ->
	    write_index_file(Dir, FileNo),
	    {ok, Fd};
	_ -> 
	    exit({file, open})
    end.

put_int16(I) ->
    [((I band 16#ff00) bsr 8),I band 16#ff].

tag_event(Event) ->    
    {erlang:localtime(), Event}.

read_index_file(Dir) ->
    case file:open(Dir ++ "/index", [raw, read]) of
	{ok, Fd} ->
	    Res = case catch file:read(Fd, 1) of
		      {ok, [Index]} -> {ok, Index};
		      _ -> error
		  end,
	    ok = file:close(Fd),
	    Res;
	_ -> error
    end.

%%-----------------------------------------------------------------
%% Write the index file.  This file contains one binary with
%% the last used filename (an integer).
%%-----------------------------------------------------------------
write_index_file(Dir, Index) ->
    case file:open(Dir ++ "/index", [raw, write]) of
	{ok, Fd} ->
	    file:write(Fd, [Index]),
	    ok = file:close(Fd);
	_ -> exit(open_index_file)
    end.

inc(N, Max) ->
    if
	N < Max -> N + 1;
	true -> 1
    end.
