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

%% Generic file contents operations
-export([open/2, close/1]).
-export([write/2, read/2, copy/3,
	 pread/2, pread/3, pwrite/2, pwrite/3, 
	 position/2, truncate/1, sync/1]).

%% Specialized file operations
-export([get_size/1, get_file/1, set_file/2, get_file_close/1]).
-export([compress/1, uncompress/1, uuencode/1, uudecode/1]).

-export([open_mode/1]).  %% used by ftp-file

-export([ipread_s32bu_p32bu/3]).



%% Includes and defines

-define(RAM_FILE_DRV, "ram_file_drv").

-include("file.hrl").



%% --------------------------------------------------------------------------
%% These operation codes were once identical between efile_drv.c
%% and ram_file_drv.c, but now these drivers are not depeding on each other.
%% So, the codes could be changed to more logical values now, but why indeed?

%% Defined "file" functions 
-define(RAM_FILE_OPEN,            1).
-define(RAM_FILE_READ,            2).
-define(RAM_FILE_LSEEK,           3).
-define(RAM_FILE_WRITE,           4).
-define(RAM_FILE_FSYNC,           9).
-define(RAM_FILE_TRUNCATE,       14).
-define(RAM_FILE_PREAD,          17).
-define(RAM_FILE_PWRITE,         18).

%% Other operations
-define(RAM_FILE_GET,            30).
-define(RAM_FILE_SET,            31).
-define(RAM_FILE_GET_CLOSE,      32).
-define(RAM_FILE_COMPRESS,       33).
-define(RAM_FILE_UNCOMPRESS,     34).
-define(RAM_FILE_UUENCODE,       35).
-define(RAM_FILE_UUDECODE,       36).
-define(RAM_FILE_SIZE,           37).

%% Open modes for RAM_FILE_OPEN
-define(RAM_FILE_MODE_READ,       1).
-define(RAM_FILE_MODE_WRITE,      2).
-define(RAM_FILE_MODE_READ_WRITE, 3).
%% Use this mask to get just the mode bits to be passed to the driver.
-define(RAM_FILE_MODE_MASK, 3).

%% Seek modes for RAM_FILE_LSEEK
-define(RAM_FILE_SEEK_SET,        0).
-define(RAM_FILE_SEEK_CUR,        1).
-define(RAM_FILE_SEEK_END,        2).

%% Return codes
-define(RAM_FILE_RESP_OK,         0).
-define(RAM_FILE_RESP_ERROR,      1).
-define(RAM_FILE_RESP_DATA,       2).
-define(RAM_FILE_RESP_NUMBER,     3).
-define(RAM_FILE_RESP_INFO,       4).

%% --------------------------------------------------------------------------
%% Generic file contents operations.
%%
%% Supposed to be called by applications through module file.

open(Data, ModeList) when list(ModeList) ->
    case load_driver() of
	{ok, _Drv} ->
	    case open_mode(ModeList) of
		{error, _} = Error ->
		    Error;
		{Mode, Opts} ->
		    case ll_open(Data, Mode, Opts) of
			{ok, Port} ->
			    {ok, 
			     #file_descriptor{module = ?MODULE, data = Port}};
			Error ->
			    Error
		    end
	    end;
	Error -> Error
    end;
%% Old obsolete mode specification
open(Data, Mode) ->
    case mode_list(Mode) of
	ModeList when list(ModeList) ->
	    open(Data, ModeList);
	Error ->
	    Error
    end;
open(_, _) ->
    {error, einval}.

close(#file_descriptor{module = ?MODULE, data = Port}) -> 
    ll_close(Port).

read(#file_descriptor{module = ?MODULE, data = Port}, Size) 
  when integer(Size), Size > 0 -> 
    Cmd = [?RAM_FILE_READ | i32(Size)],
    case call_port(Port, Cmd) of
	{ok, {0, _Data}} ->
	    eof;
	{ok, {_Size, Data}} ->
	    {ok, Data};
	{error, enomem} ->
	    %% Garbage collecting here might help if the current processes
	    %% has some old binaries left.
	    erlang:garbage_collect(),
	    case call_port(Port, Cmd) of
		{ok, {0, _Data}} ->
		    eof;
		{ok, {_Size, Data}} ->
		    {ok, Data};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end;
read(#file_descriptor{module = ?MODULE}, _) ->
    {error, einval}.

write(#file_descriptor{module = ?MODULE, data = Port}, Bytes) -> 
    case call_port(Port, [?RAM_FILE_WRITE | Bytes]) of
	{ok, _Size} ->
	    ok;
	Error ->
	    Error
    end.




copy(#file_descriptor{module = ?MODULE} = Source,
     #file_descriptor{module = ?MODULE} = Dest,
     Length)
  when integer(Length), Length >= 0;
       atom(Length) ->
    %% XXX Should be moved down to the driver for optimization.
    file:copy_opened(Source, Dest, Length);
copy(#file_descriptor{module = ?MODULE}, 
     #file_descriptor{module = ?MODULE}, 
     _) ->
    {error, einval}.


sync(#file_descriptor{module = ?MODULE, data = Port}) -> 
    call_port(Port, [?RAM_FILE_FSYNC]).

truncate(#file_descriptor{module = ?MODULE, data = Port}) -> 
    call_port(Port, [?RAM_FILE_TRUNCATE]).

position(#file_descriptor{module = ?MODULE, data = Port}, Pos) -> 
    case lseek_position(Pos) of
	{ok, Offs, Whence} ->
	    call_port(Port, [?RAM_FILE_LSEEK, i32(Offs), i32(Whence)]);
	Error ->
	    Error
    end.



pread(#file_descriptor{module = ?MODULE, data = Port}, L) when list(L) ->
    pread_int(Port, L, []);
pread(#file_descriptor{module = ?MODULE}, _) ->
    {error, einval}.

pread_int(_Port, [], R) ->
    {ok, lists:reverse(R)};
pread_int(Port, [{At, Sz} | T], R)
  when integer(At), At >= 0, integer(Sz), Sz >= 0 -> 
    case call_port(Port, [?RAM_FILE_PREAD, i32(At), i32(Sz)]) of
	{ok, {0,_Data}} -> 
	    pread_int(Port, T, [eof | R]);
	{ok, {_Size,Data}} -> 
	    pread_int(Port, T, [Data | R]);
	Error -> 
	    Error
    end;
pread_int(_, _, _) ->
    {error, einval}.

pread(#file_descriptor{module = ?MODULE, data = Port}, At, Sz) 
  when integer(At), At >= 0, integer(Sz), Sz >= 0 -> 
    case call_port(Port, [?RAM_FILE_PREAD, i32(At), i32(Sz)]) of
	{ok, {0,_Data}} -> 
	    eof;
	{ok, {_Size,Data}} -> 
	    {ok, Data};
	Error -> 
	    Error
    end;
pread(#file_descriptor{module = ?MODULE}, _, _) ->
    {error, einval}.



pwrite(#file_descriptor{module = ?MODULE, data = Port}, L) when list(L) ->
    pwrite_int(Port, L, 0);
pwrite(#file_descriptor{module = ?MODULE}, _) ->
    {error, einval}.

pwrite_int(_Port, [], _R) ->
    ok;
pwrite_int(Port, [{At, Bytes} | T], R)
  when integer(At), At >= 0 -> 
    case call_port(Port, [?RAM_FILE_PWRITE, i32(At) | Bytes]) of
	{ok, _Size} -> 
	    pwrite_int(Port, T, R+1);
	{error, Reason} -> 
	    {error, {R, Reason}}
    end;
pwrite_int(_, _, _) ->
    {error, einval}.

pwrite(#file_descriptor{module = ?MODULE, data = Port}, At, Bytes) 
  when integer(At), At >= 0 -> 
    case call_port(Port, [?RAM_FILE_PWRITE, i32(At), Bytes]) of
	{ok, _Size} ->
	    ok;
	Error ->
	    Error
    end;
pwrite(#file_descriptor{module = ?MODULE}, _, _) ->
    {error, einval}.



ipread_s32bu_p32bu(#file_descriptor{module = ?MODULE} = Handle, Pos, MaxSize) ->
    file:ipread_s32bu_p32bu_int(Handle, Pos, MaxSize).



%% --------------------------------------------------------------------------


get_file(#file_descriptor{module = ?MODULE, data = Port}) ->
    case call_port(Port, [?RAM_FILE_GET]) of
	{ok, {_Sz, Data}} -> 
	    {ok, Data};
	Error -> 
	    Error
    end;
get_file(#file_descriptor{}) ->
    {error, enotsup}.

set_file(#file_descriptor{module = ?MODULE, data = Port}, Data) ->
    call_port(Port, [?RAM_FILE_SET | Data]);
set_file(#file_descriptor{}, _) ->
    {error, enotsup}.

get_file_close(#file_descriptor{module = ?MODULE, data = Port}) ->
    case call_port(Port, [?RAM_FILE_GET_CLOSE]) of
	{ok, {_Sz, Data}} -> 
	    {ok, Data};
	Error -> 
	    Error
    end;
get_file_close(#file_descriptor{}) ->
    {error, enotsup}.

get_size(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_SIZE]);
get_size(#file_descriptor{}) ->
    {error, enotsup}.

compress(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_COMPRESS]);
compress(#file_descriptor{}) ->
    {error, enotsup}.

uncompress(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_UNCOMPRESS]);
uncompress(#file_descriptor{}) ->
    {error, enotsup}.


uuencode(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_UUENCODE]);
uuencode(#file_descriptor{}) ->
    {error, enotsup}.

uudecode(#file_descriptor{module = ?MODULE, data = Port}) ->
    call_port(Port, [?RAM_FILE_UUDECODE]);
uudecode(#file_descriptor{}) ->
    {error, enotsup}.



%%%-----------------------------------------------------------------
%%% Functions to communicate with the driver



-ifdef(LOADABLE).
driver_dir() ->
    code:lib_dir(?MODULE).

load_driver() ->
    erl_ddll:start(),
    {ok,DrvList} = erl_ddll:loaded_drivers(),
    case lists:member(?RAM_FILE_DRV, DrvList) of
	true -> 
	    {ok, ?RAM_FILE_DRV};
	false ->
	    case erl_ddll:load_driver(driver_dir(), ?RAM_FILE_DRV) of
		ok -> 
		    {ok, ?RAM_FILE_DRV};
		{error, _} = Error -> 
		    Error
	    end
    end.
-else.
load_driver() ->
    {ok, ?RAM_FILE_DRV}.

-endif.

ll_open(Data, Mode, Opts) ->
    case (catch erlang:open_port({spawn, ?RAM_FILE_DRV}, Opts)) of
	{'EXIT', _Reason} ->
	    {error, emfile};
	Port ->
	    case call_port(Port, [?RAM_FILE_OPEN, i32(Mode) | Data]) of
		{error, _} = Error ->
		    ll_close(Port),
		    Error;
		{ok, _} ->
		    {ok, Port}
	    end
    end.
    
call_port(Port, Command) ->
    case (catch list_to_binary(Command)) of
	Cmd when binary(Cmd) ->
	    case (catch erlang:port_command(Port, Cmd)) of
		{'EXIT', _} ->
		    {error, einval};
		_ ->
		    get_response(Port)
	    end;
	_ ->
	    {error, einval}
    end.

get_response(Port) ->
    receive
	{Port, {data, [Response|Rest]}} ->
	    translate_response(Response, Rest);
	{'EXIT', Port, _Reason} ->
	    {error, port_died}
    end.

ll_close(Port) ->
    catch erlang:port_close(Port),
    receive %% In case the caller is the owner and traps exits
	{'EXIT', Port, _} ->
	    ok
    after 0 ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% Utility functions.

mode_list(read) ->
    [read];
mode_list(write) ->
    [write];
mode_list(read_write) ->
    [read, write];
mode_list({binary, Mode}) when atom(Mode) ->
    [binary | mode_list(Mode)];
mode_list({character, Mode}) when atom(Mode) ->
    mode_list(Mode);
mode_list(_) ->
    [{error, einval}].



%% Converts a list of mode atoms into an mode word for the driver.
%% Returns {Mode, Opts} wher Opts is a list of options for 
%% erlang:open_port/2, or {error, einval} upon failure.

open_mode(List) when list(List) ->
    case open_mode(List, {0, []}) of
	{Mode, Opts} when Mode band 
			  (?RAM_FILE_MODE_READ bor ?RAM_FILE_MODE_WRITE) 
			  == 0 ->
	    {Mode bor ?RAM_FILE_MODE_READ, Opts};
	Other ->
	    Other
    end.

open_mode([ram|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode, Opts});
open_mode([read|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode bor ?RAM_FILE_MODE_READ, Opts});
open_mode([write|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode bor ?RAM_FILE_MODE_WRITE, Opts});
open_mode([binary|Rest], {Mode, Opts}) ->
    open_mode(Rest, {Mode, [binary | Opts]});
open_mode([], {Mode, Opts}) ->
    {Mode, Opts};
open_mode(_, _) ->
    {error, einval}.



%% Converts a position tuple {bof, X} | {cur, X} | {eof, X} into
%% {ok, Offset, OriginCode} for the driver.
%% Returns {error, einval} upon failure.

lseek_position(Pos) when integer(Pos) ->
    lseek_position({bof, Pos});
lseek_position(bof) ->
    lseek_position({bof, 0});
lseek_position(cur) ->
    lseek_position({cur, 0});
lseek_position(eof) ->
    lseek_position({eof, 0});
lseek_position({bof, Offset}) when integer(Offset) ->
    {ok, Offset, ?RAM_FILE_SEEK_SET};
lseek_position({cur, Offset}) when integer(Offset) ->
    {ok, Offset, ?RAM_FILE_SEEK_CUR};
lseek_position({eof, Offset}) when integer(Offset) ->
    {ok, Offset, ?RAM_FILE_SEEK_END};
lseek_position(_) ->
    {error, einval}.



translate_response(?RAM_FILE_RESP_OK, []) ->
    ok;
translate_response(?RAM_FILE_RESP_OK, Data) ->
    {ok, Data};
translate_response(?RAM_FILE_RESP_ERROR, List) when list(List) ->
    {error, list_to_atom(List)};
translate_response(?RAM_FILE_RESP_NUMBER, [X1, X2, X3, X4]) ->
    {ok, i32(X1, X2, X3, X4)};
translate_response(?RAM_FILE_RESP_DATA, [X1, X2, X3, X4|Data]) ->
    {ok, {i32(X1, X2, X3, X4), Data}};
translate_response(X, Data) ->
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
