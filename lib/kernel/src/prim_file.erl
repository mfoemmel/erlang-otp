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
-module(prim_file).

%% File driver interface


-export([open/2, close/1, sync/1, position/2, truncate/1,
	 write/2, pwrite/3, read/2, pread/3]).

-export([get_cwd/0, get_cwd/1, set_cwd/1,
	 delete/1, rename/2, make_dir/1, del_dir/1,
	 read_file_info/1, write_file_info/2,
	 make_link/2, make_symlink/2,
	 read_link/1, read_link_info/1,
	 list_dir/1]).

%% interface for module file (with port argument)
-export([p_get_cwd/1, p_get_cwd/2, p_set_cwd/2,
	 p_delete/2, p_rename/3, p_make_dir/2, p_del_dir/2,
	 p_read_file_info/2, p_write_file_info/3,
	 p_make_link/3, p_make_symlink/3,
	 p_read_link/2, p_read_link_info/2,
	 p_list_dir/2]).

-export([read_file/1, write_file/2]).


-include_lib("kernel/include/file.hrl").

-define(FILE_OPEN,             1).
-define(FILE_READ,             2).
-define(FILE_LSEEK,            3).
-define(FILE_WRITE,            4).
-define(FILE_FSTAT,            5).
-define(FILE_PWD,              6).
-define(FILE_READDIR,          7).
-define(FILE_CHDIR,            8).
-define(FILE_FSYNC,            9).
-define(FILE_MKDIR,            10).
-define(FILE_DELETE,           11).
-define(FILE_RENAME,           12).
-define(FILE_RMDIR,            13).
-define(FILE_TRUNCATE,         14).
-define(FILE_READ_FILE,        15).
-define(FILE_WRITE_INFO,       16).
-define(FILE_PREAD,            17).
-define(FILE_PWRITE,           18).
-define(FILE_LSTAT,            19).
-define(FILE_READLINK,         20).
-define(FILE_LINK,             21).
-define(FILE_SYMLINK,          22).


-define(FILE_RESP_OK,          0).
-define(FILE_RESP_ERROR,       1).
-define(FILE_RESP_DATA,        2).
-define(FILE_RESP_NUMBER,      3).
-define(FILE_RESP_INFO,        4).

%%
%% Int to bytes
%%
-define(int8(X), [(X) band 16#ff]).

-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int24(X), [((X) bsr 16) band 16#ff,
		   ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int32(X), 
	[((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff]).

%% Bytes to unsigned
-define(u32(X3,X2,X1,X0), 
	(((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u24(X2,X1,X0),
	(((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u16(X1,X0),
	(((X1) bsl 8) bor (X0))).
 
-define(u8(X0), (X0)).

%% Bytes to signed
-define(i32(X3,X2,X1,X0),
        (?u32(X3,X2,X1,X0) - 
         (if (X3) > 127 -> 16#100000000; true -> 0 end))).

-define(i24(X2,X1,X0),
        (?u24(X2,X1,X0) - 
         (if (X2) > 127 -> 16#1000000; true -> 0 end))).
	
-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(i8(X0),
	(?u8(X0) -
	 (if (X0) > 127 -> 16#100; true -> 0 end))).

%% Open modes for the efile driver's open function.

-define(EFILE_MODE_READ, 1).
-define(EFILE_MODE_WRITE, 2).	%% Implies truncating file when used alone.
-define(EFILE_MODE_READ_WRITE, 	3).
-define(EFILE_MODE_APPEND,	4).
-define(EFILE_COMPRESSED, 	8).
-define(EFILE_NO_TRUNCATE,      16). %% Special for reopening on VxWorks

%% Use this mask to get just the mode bits to be passed to the driver.
-define(EFILE_MODE_MASK, 15).

%% These are used internally in this module, and never passed to the driver.
-define(BINARY_FILE, 2#10000000000).
-define(RAW_PORT,    2#01000000000).

%% Seek modes for the efile driver's seek function.

-define(EFILE_SEEK_SET, 0).
-define(EFILE_SEEK_CUR, 1).
-define(EFILE_SEEK_END, 2).

-define(FT_DEVICE, 	1).
-define(FT_DIRECTORY, 	2).
-define(FT_REGULAR,	3).
-define(FT_SYMLINK,	4).
-define(FT_OTHER,	5).

-define(FA_NONE,    	0).
-define(FA_WRITE, 	1).
-define(FA_READ,	2).
-define(FA_READ_WRITE,	3).


-record(fd, {port, number}).

%%
%% OPEN(File, Mode)
%%
open(File, ModeList) ->
    case open_mode(ModeList) of
	{error, Reason} -> {error, Reason};
	Mode ->
	    MM = Mode band ?EFILE_MODE_MASK,
	    Opts = if Mode band ?BINARY_FILE == 0 -> []; true -> [binary] end,
	    case mkport(Opts) of
		{ok, Port} ->
		    Fd = #fd{port = Port},
		    case sync_cmd(Fd, [?FILE_OPEN, ?int32(MM), File, 0]) of
			{ok, N} -> {ok, Fd#fd { number = N}};
			Error -> 
			    unlink(Port),
			    port_close(Port),
			    Error
		    end;
		Error -> Error
	    end
    end.

%%
%% CLOSE(Fd)
%%
close(Fd) ->
    Port = Fd#fd.port,
    unlink(Port),
    port_close(Port),
    ok.

%%
%% WRITE(Fd, Bytes) -> ok | {error,Reason}
%%
write(Fd, Bytes) ->
    case sync_cmd(Fd, [?FILE_WRITE,Bytes]) of
	{ok, _Size} -> ok;
	Other -> Other
    end.

%%
%% PWRITE(Fd, Offs, Bytes) -> ok | {error,Reason}
%%
pwrite(Fd, Offs, Bytes) ->
    case sync_cmd(Fd, [?FILE_PWRITE,?int32(Offs), Bytes]) of
	{ok, _Size} -> ok;
	Other -> Other
    end.

%%
%% SYNC(Fd) -> ok | {error,Reason}
%%
sync(Fd) ->
    sync_cmd(Fd, [?FILE_FSYNC]).

%%
%% READ(Fd, Size)
%%
read(Fd, Size) ->
    case sync_cmd(Fd, [?FILE_READ|?int32(Size)]) of
	{ok, {0,_Data}}     -> eof; %% bug if Size=0
	{ok, {_Size, Data}} -> {ok, Data};
	Other -> Other
    end.


%%
%% PREAD(Fd, Offs, Size)
%% bug! if reading 0 bytes!!!
%%
pread(Fd, Offs, Size) ->
    case sync_cmd(Fd, [?FILE_PREAD, ?int32(Offs), ?int32(Size)]) of
	{ok, {0, _Data}}    -> eof;
	{ok, {_Size, Data}} -> {ok, Data};
	Other -> Other
    end.

%%
%% POSITION(Fd, At)
%%
position(Fd, {bof,Offs}) -> 
    sync_cmd(Fd, [?FILE_LSEEK, ?int32(Offs), ?int32(?EFILE_SEEK_SET)]);
position(Fd, {cur,Offs}) ->
    sync_cmd(Fd, [?FILE_LSEEK, ?int32(Offs), ?int32(?EFILE_SEEK_CUR)]);
position(Fd, {eof,Offs}) ->
    sync_cmd(Fd, [?FILE_LSEEK, ?int32(Offs), ?int32(?EFILE_SEEK_END)]);

position(Fd, bof) -> position(Fd, {bof,0});
position(Fd, eof) -> position(Fd, {eof,0});
position(Fd, cur) -> position(Fd, {cur,0});
position(Fd, Pos) when integer(Pos) -> position(Fd, {bof,Pos});
position(_, _) -> {error, einval}.

%%
%% TRUNCATE(Fd)
%%
truncate(Fd) ->
    sync_cmd(Fd, [?FILE_TRUNCATE]).

%%
get_cwd() -> 
    port_cmd(?FILE_PWD, [0]).

get_cwd([Letter, $:]) when $A =< Letter, Letter =< $Z ->
    port_cmd(?FILE_PWD, [(Letter-$A)+1]);
get_cwd([Letter, $:]) when $a =< Letter, Letter =< $z ->
    port_cmd(?FILE_PWD, [(Letter-$a)+1]);
get_cwd(_) ->
    {error, einval}.

set_cwd(Dirname)  -> port_cmd(?FILE_CHDIR, Dirname).
delete(Name)      -> port_cmd(?FILE_DELETE, Name).
rename(From, To)  -> port_cmd(?FILE_RENAME, [From, 0, To]).
make_dir(Name)    -> port_cmd(?FILE_MKDIR, Name).
del_dir(Name)     -> port_cmd(?FILE_RMDIR, Name).
read_file_info(Name) -> port_cmd(?FILE_FSTAT, Name).
write_file_info(Name, Info) when record(Info, file_info) ->
    port_cmd(?FILE_WRITE_INFO, [info_to_list(Info), Name]).
read_link_info(Name) -> port_cmd(?FILE_LSTAT, Name).
read_link(Name) -> port_cmd(?FILE_READLINK, Name).
make_link(Old, New) -> port_cmd(?FILE_LINK, [Old, 0, New]).
make_symlink(Old, New) -> port_cmd(?FILE_SYMLINK, [Old, 0, New]).


list_dir(Name) ->
    case catch erlang:open_port_prim({spawn, efile}, []) of
	{'EXIT',_} -> {error, emfile};
	Port ->
	    Result = p_list_dir(Port, Name),
	    unlink(Port),
	    port_close(Port),
	    Result
    end.

%% "Open port" version of the above commands file commands
p_get_cwd(Port) -> 
    port_cmd(Port, ?FILE_PWD, [0]).

p_get_cwd(Port, [Letter, $:]) when $A =< Letter, Letter =< $Z ->
    port_cmd(Port, ?FILE_PWD, [(Letter-$A)+1]);
p_get_cwd(Port, [Letter, $:]) when $a =< Letter, Letter =< $z ->
    port_cmd(Port, ?FILE_PWD, [(Letter-$a)+1]);
p_get_cwd(_, _) ->
    {error, einval}.

p_set_cwd(Port, Dirname) -> port_cmd(Port, ?FILE_CHDIR, Dirname).
p_delete(Port, Name)     -> port_cmd(Port, ?FILE_DELETE, Name).
p_rename(Port, From, To) -> port_cmd(Port, ?FILE_RENAME, [From, 0, To]).
p_make_dir(Port, Name)   -> port_cmd(Port, ?FILE_MKDIR, Name).
p_del_dir(Port, Name)    -> port_cmd(Port, ?FILE_RMDIR, Name).
p_read_file_info(Port, Name) -> port_cmd(Port, ?FILE_FSTAT, Name).
p_write_file_info(Port, Name, Info) when record(Info, file_info) ->
    port_cmd(Port, ?FILE_WRITE_INFO, [info_to_list(Info), Name]).
p_read_link_info(Port,Name) -> port_cmd(Port,?FILE_LSTAT, Name).
p_read_link(Port,Name) -> port_cmd(Port,?FILE_READLINK, Name).
p_make_link(Port,Old, New) -> port_cmd(Port,?FILE_LINK, [Old, 0, New]).
p_make_symlink(Port,Old, New) -> port_cmd(Port,?FILE_SYMLINK, [Old, 0, New]).


p_list_dir(Port, Name) ->
    case catch port_command(Port, [?FILE_READDIR, Name, 0]) of
	{'EXIT',_} -> {error, einval};
	true -> collect_files(Port, [])
    end.

read_file(Name) -> 
    case mkport([binary]) of
	{ok,P} ->
	    Command = [?FILE_READ_FILE, Name, 0],
	    port_command(P, Command),
	    Result =
		case get_response(P) of
		    {error, enomem} ->
			%% It could possibly help to do a gargabe collection
			%% here, if the file server has some references
			%% to binaries read earlier.
			erlang:garbage_collect(),
			port_command(P, Command),
			get_response(P);
		    Other ->
			Other
		end,
	    unlink(P),
	    port_close(P),
	    Result;
	Error -> Error
    end.


write_file(Name, Bin) ->
    case open(Name, [binary, write, raw]) of
	{ok, Fd} ->
	    Result = 
		case write(Fd, Bin) of
		    {ok, _} -> ok;
		    Error -> Error
		end,
	    close(Fd),
	    Result;
	Error ->
	    Error
    end.


info_to_list(#file_info {mode=Mode, uid=Uid, gid=Gid,
			 atime=Atime0, mtime=Mtime0, ctime=Ctime}) ->
    {Atime, Mtime} =
	case {Atime0, Mtime0} of
	    {undefined, Mtime0} -> {erlang:localtime(), Mtime0};
	    {Atime0, undefined} -> {Atime0, Atime0};
	    Complete -> Complete
	end,
    [int_to_bytes(Mode), int_to_bytes(Uid), int_to_bytes(Gid),
     date_to_bytes(Atime), date_to_bytes(Mtime), date_to_bytes(Ctime)].


%%
%% Do single file command
%%
port_cmd(Port, Op, Chars) ->
    sync_cmd(#fd{port = Port}, [Op, Chars, 0]).

port_cmd(Op, Chars) ->
    case mkport([]) of
	{ok, Port} ->
	    Result = sync_cmd(#fd{port = Port}, [Op, Chars, 0]),
	    unlink(Port),
	    port_close(Port),
	    Result;
	Error -> Error
    end.

sync_cmd(Fd, Command) ->
    Port = Fd#fd.port,
    port_command(Port, Command),
    get_response(Port).


mkport(Opts) ->
    case catch erlang:open_port_prim({spawn,efile}, Opts) of
	{'EXIT', _} -> {error, emfile};
	Port -> {ok, Port}
    end.

%% Receives the response from an efile port.
%% Returns: {ok, ListOrBinary}|{error, Reason}

get_response(Port) ->
    erlang:bump_reductions(100),
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
    {ok, ?u32(X1,X2,X3,X4)};
get_response(?FILE_RESP_DATA, [X1,X2,X3,X4|Data]) ->
    {ok, {?u32(X1, X2, X3, X4), Data}};
get_response(?FILE_RESP_INFO, List) when list(List) ->
    {ok, transform_ints(getints(List))};
get_response(X, Data) ->
    {error, {bad_response_from_port, X, Data}}.



%%
%% Utils
%%

int_to_bytes(Int) when integer(Int) -> ?int32(Int);
int_to_bytes(undefined) -> [255,255,255,255].

date_to_bytes(undefined) ->
    MinusOne = [255,255,255,255],
    [MinusOne, MinusOne, MinusOne, MinusOne, MinusOne, MinusOne];
date_to_bytes({{Y, Mon, D}, {H, Min, S}}) ->
    [?int32(Y), ?int32(Mon), ?int32(D), ?int32(H), ?int32(Min), ?int32(S)].

collect_files(Port, Result) ->
    case get_response(Port) of
	ok ->
	    {ok, Result};
	{ok, Name} ->
	    collect_files(Port, [Name|Result]);
	Error ->
	    Error
    end.

transform_ints(Ints) ->
    [HighSize, LowSize, Type|Tail0] = Ints,
    Size = HighSize * 16#100000000 + LowSize,
    [Ay, Am, Ad, Ah, Ami, As|Tail1]  = Tail0,
    [My, Mm, Md, Mh, Mmi, Ms|Tail2] = Tail1,
    [Cy, Cm, Cd, Ch, Cmi, Cs|Tail3] = Tail2,
    [Mode, Links, Major, Minor, Inode, Uid, Gid, Access] = Tail3,
    #file_info {
		size = Size,
		type = file_type(Type),
		access = file_access(Access),
		atime = {{Ay, Am, Ad}, {Ah, Ami, As}},
		mtime = {{My, Mm, Md}, {Mh, Mmi, Ms}},
		ctime = {{Cy, Cm, Cd}, {Ch, Cmi, Cs}},
		mode = Mode,
		links = Links,
		major_device = Major,
		minor_device = Minor,
		inode = Inode,
		uid = Uid,
		gid = Gid}.

file_type(?FT_DEVICE) -> device;
file_type(?FT_DIRECTORY) -> directory;
file_type(?FT_REGULAR) -> regular;
file_type(?FT_SYMLINK) -> symlink;
file_type(?FT_OTHER) -> other.

file_access(?FA_NONE) -> none;   
file_access(?FA_WRITE) -> write;
file_access(?FA_READ) -> read;
file_access(?FA_READ_WRITE) -> read_write.

getints([X1,X2,X3,X4|Tail]) ->
    [?u32(X1,X2,X3,X4) | getints(Tail)];
getints([]) -> [].

open_mode(List) ->
    case open_mode(List, 0) of
	Mode when Mode band (?EFILE_MODE_READ bor ?EFILE_MODE_WRITE) == 0 ->
	    Mode bor ?EFILE_MODE_READ;
	Other ->
	    Other
    end.

open_mode([read|T],  M)      -> open_mode(T, M bor ?EFILE_MODE_READ);
open_mode([write|T], M)      -> open_mode(T, M bor ?EFILE_MODE_WRITE);
open_mode([binary|T], M)     -> open_mode(T, M bor ?BINARY_FILE);
open_mode([raw|T], M)        -> open_mode(T, M bor ?RAW_PORT);
open_mode([compressed|T], M) -> open_mode(T, M bor ?EFILE_COMPRESSED);
open_mode([append|T], M)     -> open_mode(T, M bor ?EFILE_MODE_APPEND);
open_mode([], M) -> M;
open_mode(_, _) -> {error, einval}.

