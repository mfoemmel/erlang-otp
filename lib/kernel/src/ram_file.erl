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
-module(ram_file).

%% Binary RAM file interface

-export([open/2, close/1]).
-export([write/2, read/2, pread/3, pwrite/3, position/2, truncate/1, sync/1]).

-export([get_size/1, get_file/1, set_file/2, get_file_close/1]).
-export([compress/1, uncompress/1, uuencode/1, uudecode/1]).

-export([open_mode/1]).  %% used by ftp-file

-define(RAM_FILE_DRV, "ram_file_drv").

%% XXX FOLLOWING DEFINITIONS MUST BE IN SYNC WITH file.erl (move to file.hrl)
%% --------------------------------------------------------------------------
-define(FILE_OPEN,             1).

-define(FILE_PREAD,           17).
-define(FILE_PWRITE,          18).

-define(FILE_RESP_OK,          0).
-define(FILE_RESP_ERROR,       1).
-define(FILE_RESP_DATA,        2).
-define(FILE_RESP_NUMBER,      3).
-define(FILE_RESP_INFO,        4).

-define(EFILE_MODE_READ,       1).
-define(EFILE_MODE_WRITE,      2).
-define(EFILE_MODE_READ_WRITE, 3).

%% Use this mask to get just the mode bits to be passed to the driver.
-define(EFILE_MODE_MASK, 3).

%% These are used internally in this module, and never passed to the driver.
-define(BINARY_FILE, 2#10000000000).
-define(RAW_PORT,    2#01000000000).


%% other operations
-define(RAM_FILE_GET,           30).
-define(RAM_FILE_SET,           31).
-define(RAM_FILE_GET_CLOSE,     32).
-define(RAM_FILE_COMPRESS,      33).
-define(RAM_FILE_UNCOMPRESS,    34).
-define(RAM_FILE_UUENCODE,      35).
-define(RAM_FILE_UUDECODE,      36).
-define(RAM_FILE_SIZE,          37).

-record(fd, {port, number}).  %% XXX Move to file.hrl

%% END OF SYNC
%% --------------------------------------------------------------------------

open(Data, Mode0) ->
    case load_driver() of
	{ok, Drv} ->
	    case open_mode(Mode0) of
		{ok,Mode} ->
		    ll_open(Data, Mode band (bnot ?RAW_PORT));
		Error -> Error
	    end;
	Error -> Error
    end.

close(Fd) -> file:close(Fd).

read(Fd, Sz) -> file:read(Fd, Sz).

write(Fd, Bytes) -> file:write(Fd, Bytes).

sync(Fd) -> file:sync(Fd).

truncate(Fd) -> file:truncate(Fd).

position(Fd, Pos) -> file:position(Fd, Pos).

pread(Fd, At, Sz) when record(Fd, fd), integer(At), integer(Sz) -> 
    case call_port(Fd#fd.port, [?FILE_PREAD,i32(At),i32(Sz)]) of
	{ok, {0,_Data}} -> eof;
	{ok, {_Size,Data}} -> {ok, Data};
	Other -> Other
    end.

pwrite(Fd, At, Bytes) when record(Fd, fd), integer(At) -> 
    call_port(Fd#fd.port, [?FILE_PWRITE, i32(At), Bytes]).

get_file(Fd) when record(Fd, fd) ->
    case call_port(Fd#fd.port, [?RAM_FILE_GET]) of
	{ok, {_Sz,Data}} -> {ok, Data};
	Error -> Error
    end.

set_file(Fd,Data) when record(Fd, fd) ->
    call_port(Fd#fd.port, [?RAM_FILE_SET, Data]).

get_file_close(Fd) when record(Fd, fd) ->
    case call_port(Fd#fd.port, [?RAM_FILE_GET_CLOSE]) of
	{ok, {_Sz, Data}} -> {ok, Data};
	Error -> Error
    end.

get_size(Fd) when record(Fd, fd) ->
    call_port(Fd#fd.port, [?RAM_FILE_SIZE]).

compress(Fd) when record(Fd, fd) ->
    call_port(Fd#fd.port, [?RAM_FILE_COMPRESS]).

uncompress(Fd) when record(Fd, fd) ->
    call_port(Fd#fd.port, [?RAM_FILE_UNCOMPRESS]).

uuencode(Fd) when record(Fd, fd) ->
    call_port(Fd#fd.port, [?RAM_FILE_UUENCODE]).

uudecode(Fd) when record(Fd, fd) ->
    call_port(Fd#fd.port, [?RAM_FILE_UUDECODE]).

%% UTILS 

-ifdef(LOADABLE).
driver_dir() ->
    code:lib_dir(?MODULE).

load_driver() ->
    erl_ddll:start(),
    {ok,DrvList} = erl_ddll:loaded_drivers(),
    case lists:member(?RAM_FILE_DRV, DrvList) of
	true -> {ok, ?RAM_FILE_DRV};
	false ->
	    case erl_ddll:load_driver(driver_dir(), ?RAM_FILE_DRV) of
		ok -> {ok, ?RAM_FILE_DRV};
		Error -> Error
	    end
    end.
-else.
load_driver() ->
    {ok, ?RAM_FILE_DRV}.

-endif.

ll_open(Data, Mode) ->
    Cmd = [?FILE_OPEN, i32(Mode band ?EFILE_MODE_MASK), Data],
    case catch mkport(Cmd, Mode band ?BINARY_FILE) of
	{'EXIT', _} ->
	    {error, emfile};
	P ->
	    case get_response(P) of
		{ok, Number}  -> 
		    {ok, #fd{port = P, number = Number}};
		Error ->
		    unlink(P),
		    exit(P, die),
		    Error
	    end
    end.
    
mkport(Cmd, BinaryFile) ->
    Opts = if
	BinaryFile == 0 -> [];
	true -> [binary]  %% flag is set
    end,
    P = open_port({spawn, ?RAM_FILE_DRV}, Opts),	% can fail
    P ! {self(), {command, Cmd}},
    P.

open_mode(Mode) when list(Mode) ->
    new_open_mode(Mode);
open_mode(Mode) ->
    new_open_mode(translate_old_mode(Mode)).

new_open_mode(List) ->
    case new_open_mode(List, 0) of
	{ok,Mode} when Mode band ?EFILE_MODE_READ_WRITE == 0 ->
	    {ok, Mode bor ?EFILE_MODE_READ};
	Other ->
	    Other
    end.

new_open_mode([read|Rest], Result) ->
    new_open_mode(Rest, Result bor ?EFILE_MODE_READ);
new_open_mode([write|Rest], Result) ->
    new_open_mode(Rest, Result bor ?EFILE_MODE_WRITE);
new_open_mode([binary|Rest], Result) ->
    new_open_mode(Rest, Result bor ?BINARY_FILE);
new_open_mode([raw|Rest], Result) ->
    new_open_mode(Rest, Result bor ?RAW_PORT);
new_open_mode([], Result) ->
    {ok, Result};
new_open_mode(_, _) ->
    {error, einval}.

translate_old_mode(read) -> [read];
translate_old_mode(write) -> [write];
translate_old_mode(read_write) -> [read, write];
translate_old_mode({binary, Mode}) -> [binary|translate_old_mode(Mode)];
translate_old_mode({character, Mode}) -> translate_old_mode(Mode);
translate_old_mode(_) -> [this_is_an_error].


call_port(Port, Command) ->
    Port ! {self(), {command, Command}},
    get_response(Port).

get_response(Port) ->
    receive
	{Port, {data, [Response|Rest]}} ->
	    get_response(Response, Rest);
	{'EXIT', Port, Reason} ->
	    {error, port_died}
    end.

get_response(?FILE_RESP_OK, []) ->
    ok;
get_response(?FILE_RESP_OK, Data) ->
    {ok, Data};
get_response(?FILE_RESP_ERROR, List) when list(List) ->
    {error, list_to_atom(List)};
get_response(?FILE_RESP_NUMBER, [X1, X2, X3, X4]) ->
    {ok, i32(X1, X2, X3, X4)};
get_response(?FILE_RESP_DATA, [X1, X2, X3, X4|Data]) ->
    {ok, {i32(X1, X2, X3, X4), Data}};
get_response(X, Data) ->
    {error, {bad_response_from_port, X, Data}}.

i32(Int) when binary(Int) ->
    i32(binary_to_list(Int));

i32(Int)  when integer(Int) -> [(Int bsr 24) band 255,
				(Int bsr 16) band 255,
				(Int bsr  8) band 255,
				Int band 255];
i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

i32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.
