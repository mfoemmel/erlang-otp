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
%%     $Id$
%%
%% Purpose : A simple file server.
%%           This bit just does primitive *atomic* operations
%%           on files. For comments and revision history see
%%           file.erl

-module(boot_fprim).

%% Notes these routines EXIT if any of the arguments are incorrect
%% FileNames must be flat strings, etc.

%% -compile(export_all).

-export([change_group/2,
	 change_mode/2, 
	 change_owner/2, 
	 change_owner/3, 
	 change_time/2,
	 change_time/3,
	 delete/1, 
	 del_dir/1, 
	 list_dir/1,
	 make_dir/1, 
	 read_file/1, 
	 read_file_info/1,	
	 rename/2, 
	 write_file/2,
	 write_file_info/2
	]).

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

-define(FILE_RESP_OK,          0).
-define(FILE_RESP_ERROR,       1).
-define(FILE_RESP_DATA,        2).
-define(FILE_RESP_NUMBER,      3).
-define(FILE_RESP_INFO,        4).

-record(fd, {port, number}).

%% Open modes for the efile driver's open function.
-define(EFILE_MODE_READ, 1).
-define(EFILE_MODE_WRITE, 2).
-define(EFILE_MODE_READ_WRITE, 3).  
-define(EFILE_COMPRESS, 4).

%% Use this mask to get just the mode bits to be passed to the driver.
-define(EFILE_MODE_MASK, 7).


%% These are used internally in this module, and never passed to the driver.
-define(BINARY_FILE, 2#10000000000).
-define(RAW_PORT,    2#01000000000).

%% Seek modes for the efile driver's seek function.

-define(EFILE_SEEK_SET, 0).
-define(EFILE_SEEK_CUR, 1).
-define(EFILE_SEEK_END, 2).

delete(File) -> 
    valid_filename(File), fm_op(?FILE_DELETE, File).

rename(From, To) -> 
    valid_filename(From), valid_filename(To),
    fm_op(?FILE_RENAME, [From, 0, To]).

make_dir(Dir) -> 
     valid_filename(Dir), fm_op(?FILE_MKDIR, Dir).

del_dir(Dir) ->
    valid_filename(Dir), fm_op(?FILE_RMDIR, Dir).

read_file_info(File) -> 
    valid_filename(File), fm_op(?FILE_FSTAT, File).

write_file_info(Name, Info) when record(Info, file_info) ->
    valid_filename(Name),
    write_file_info_op(?FILE_WRITE_INFO, Name, Info).

list_dir(Dir) -> 
    valid_filename(Dir), list_dir_op(Dir).

read_file(File) ->
    valid_filename(File), do_read_file(File).

write_file(File, IOL) ->
    valid_filename(File),
    valid_io_list(IOL),
    Bin = coerse_to_binary(IOL),
    do_write_file(File, Bin).

change_mode(Name, Mode) when integer(Mode) ->
    write_file_info(Name, #file_info{mode=Mode}).

change_owner(Name, OwnerId) when integer(OwnerId) ->
    write_file_info(Name, #file_info{uid=OwnerId}).

change_owner(Name, OwnerId, GroupId) when integer(OwnerId), integer(GroupId) ->
    write_file_info(Name, #file_info{uid=OwnerId, gid=GroupId}).

change_group(Name, GroupId) when integer(GroupId) ->
    write_file_info(Name, #file_info{gid=GroupId}).

change_time(Name, Time) when tuple(Time) ->
    write_file_info(Name, #file_info{mtime=Time}).

change_time(Name, Atime, Mtime) when tuple(Atime), tuple(Mtime) ->
    write_file_info(Name, #file_info{atime=Atime, mtime=Mtime}).

%%______________________________________________________________________
%% Now the routines that actually do the work


%% this is useful

with_port(Fun) ->
    case catch erlang:open_port_prim({spawn, efile}, [binary]) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Port ->
	    Result = case (catch Fun(Port)) of
			 {'EXIT', Reason2} ->
			     {error, file_prim1};
			 Ret ->
			     Ret
		     end,
	    close_port(Port),
	    Result
    end.
    
do_read_file(Name) -> 
    with_port(fun(P) ->
		     Command = [?FILE_READ_FILE, Name, 0],
		     port_command(P, Command),
		     case get_response(P) of
			 {error, enomem} ->
			     erlang:garbage_collect(),
			     port_command(P, Command),
			     get_response(P);
			 Other ->
			     Other
		     end
	     end).

do_write_file(File, Bin) ->
    %% here the arguments are checked
    %% File is a flat string and Bin is a binary
    case open(File, [binary, write, raw]) of
	{ok, Port} ->
	    Result = case write(Port, Bin) of
			 {ok, _} -> ok;
			 Error   -> Error
		     end,
	    close_port(Port),
	    Result;
	Error ->
	    Error
    end.

open(File, Mode0) ->
    Mode = open_mode(Mode0),
    Cmd = [?FILE_OPEN, i32(Mode band ?EFILE_MODE_MASK), File, 0],
    case mkport(Cmd, Mode band ?BINARY_FILE) of
	{'EXIT', _} ->
	    {error, emfile};
	P ->
	    case get_response(P) of
		{ok, Number}  ->  
		    {ok, P};
		Error ->
		    close_port(P),
		    Error
	    end
    end.


write(Port, Bytes) ->
    port_command(Port, [?FILE_WRITE, Bytes]),
    case get_response(Port) of
	{ok, _Size} ->
	    ok;
	Other ->
	    Other
    end.

close_port(Port) ->
    unlink(Port),
    exit(Port, die),
    ok.

fm_op(Op, Chars) ->
    with_port(fun(Port) ->
		     port_command(Port, [Op, Chars, 0]),
		     get_response(Port)
	     end).


list_dir_op(Chars) ->
    with_port(fun(Port) ->
		     port_command(Port, [?FILE_READDIR|[Chars, 0]]),
		     collect_files(Port, [])
	     end).

collect_files(Port, Result) ->
    case get_response(Port) of
	ok ->
	    {ok, Result};
	{ok, Name} ->
	    collect_files(Port, [Name|Result]);
	Error ->
	    Error
    end.

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
get_response(?FILE_RESP_INFO, List) when list(List) ->
    {ok, transform_ints(getints(List))};
get_response(X, Data) ->
    {error, {bad_response_from_port, X, Data}}.

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
    
file_type(1) -> device;
file_type(2) -> directory;
file_type(3) -> regular;
file_type(4) -> symlink;
file_type(_) -> other.

file_access(0) -> none;   
file_access(1) -> write;
file_access(2) -> read;
file_access(3) -> read_write.

write_file_info_op(Op, Name, Info) ->
    with_port(fun(Port) ->
		     port_command(Port, [Op, info_to_list(Name, Info)]),
		     get_response(Port)
	     end).

info_to_list(Name, #file_info {mode=Mode, uid=Uid, gid=Gid,
			       atime=Atime0, mtime=Mtime0, ctime=Ctime}) ->
    {Atime, Mtime} =
	case {Atime0, Mtime0} of
	    {undefined, Mtime0} -> {erlang:localtime(), Mtime0};
	    {Atime0, undefined} -> {Atime0, Atime0};
	    Complete -> Complete
	end,
    [int_to_bytes(Mode), int_to_bytes(Uid), int_to_bytes(Gid),
     date_to_bytes(Atime), date_to_bytes(Mtime), date_to_bytes(Ctime),
     Name, 0].

open_mode(Flags) when list(Flags) ->
    case open_mode(Flags, 0) of
	Mode when Mode band (?EFILE_MODE_READ bor ?EFILE_MODE_WRITE) == 0 ->
	    Mode bor ?EFILE_MODE_READ;
	Other ->
	    Other
    end.

open_mode([read|Rest], Result) ->
    open_mode(Rest, Result bor ?EFILE_MODE_READ);
open_mode([write|Rest], Result) ->
    open_mode(Rest, Result bor ?EFILE_MODE_WRITE);
open_mode([binary|Rest], Result) ->
    open_mode(Rest, Result bor ?BINARY_FILE);
open_mode([raw|Rest], Result) ->
    open_mode(Rest, Result bor ?RAW_PORT);
open_mode([compressed|Rest], Result) ->
    open_mode(Rest, Result bor ?EFILE_COMPRESS);
open_mode([], Result) ->
    Result;
open_mode([H|T], _) ->
    exit(badMode).

mkport(Cmd, BinaryFile) ->
    Opts = if
	BinaryFile == 0 -> [];
	true -> [binary]  %% flag is set
    end,
    case catch erlang:open_port_prim({spawn, efile}, Opts) of
	P ->
	    port_command(P, Cmd),
	    P;
	{'EXIT', Reason} ->
	    {error, {'EXIT', Reason}}
    end.

%%----------------------------------------------------------------------
%% valid_filename -> ok | exit(..) if the argument is
%% invalid

valid_filename([H|T]) when integer(H), H > 0, H =< 255 ->
    valid_filename(T);
valid_filename([H|_]) ->
    exit(badFileName);
valid_filename([]) ->
    ok.

valid_io_list(B) when binary(B)        -> ok;
valid_io_list([H|T])                   -> valid_io_list(H),valid_io_list(T);
valid_io_list(I) when I >= 0, I =< 255 -> ok;
valid_io_list([])                      -> ok;
valid_io_list(_)                       -> exit(badIOList).

coerse_to_binary(B) when binary(B) -> B;
coerse_to_binary(L) when list(L)   -> list_to_binary(L).

int_to_bytes(Int) when integer(Int) ->
    i32(Int);
int_to_bytes(undefined) ->
    [255, 255, 255, 255].

date_to_bytes(undefined) ->
    MinusOne = [255, 255, 255, 255],
    [MinusOne, MinusOne, MinusOne, MinusOne, MinusOne, MinusOne];
date_to_bytes({{Y, Mon, D}, {H, Min, S}}) ->
    [i32(Y), i32(Mon), i32(D), i32(H), i32(Min), i32(S)].

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
    
getints([X1,X2,X3,X4|Tail]) ->
    [i32(X1,X2,X3,X4) | getints(Tail)];
getints([]) -> [].

