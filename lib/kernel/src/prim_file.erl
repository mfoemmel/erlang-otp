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

%% Interface module to the file driver.



%%% Interface towards a single file's contents. Uses ?FD_DRV.

%% Generic file contents operations
-export([open/2, close/1, sync/1, position/2, truncate/1,
	 write/2, pwrite/2, pwrite/3, read/2, pread/2, pread/3, copy/3]).

%% Specialized file operations
-export([open/1, open/3]).
-export([read_file/1, read_file/2, write_file/2]).
-export([ipread_s32bu_p32bu/3]).



%%% Interface towards file system and metadata. Uses ?DRV.

%% Takes an optional port (opens a ?DRV port per default) as first argument.
-export([get_cwd/0, get_cwd/1, get_cwd/2, 
	 set_cwd/1, set_cwd/2,
	 delete/1, delete/2, 
	 rename/2, rename/3, 
	 make_dir/1, make_dir/2,
	 del_dir/1, del_dir/2,
	 read_file_info/1, read_file_info/2,
	 write_file_info/2, write_file_info/3,
	 make_link/2, make_link/3,
	 make_symlink/2, make_symlink/3,
	 read_link/1, read_link/2,
	 read_link_info/1, read_link_info/2,
	 list_dir/1, list_dir/2]).
%% How to start and stop the ?DRV port.
-export([start/0, stop/1]).

%% Debug exports
-export([open_int/4, open_mode/1, open_mode/4]).

%%%-----------------------------------------------------------------
%%% Includes and defines

-include("file.hrl").

-define(DRV,    efile).
-define(FD_DRV, efile).

-define(LARGEFILESIZE, (1 bsl 31)).

%% Driver commands
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
-define(FILE_CLOSE,            23).
-define(FILE_PWRITEV,          24).
-define(FILE_PREADV,           25).
-define(FILE_SETOPT,           26).
-define(FILE_IPREAD,           27).

%% Driver responses
-define(FILE_RESP_OK,          0).
-define(FILE_RESP_ERROR,       1).
-define(FILE_RESP_DATA,        2).
-define(FILE_RESP_NUMBER,      3).
-define(FILE_RESP_INFO,        4).
-define(FILE_RESP_NUMERR,      5).
-define(FILE_RESP_LDATA,       6).
-define(FILE_RESP_N2DATA,      7).
-define(FILE_RESP_EOF,         8).

%% Open modes for the driver's open function.
-define(EFILE_MODE_READ,       1).
-define(EFILE_MODE_WRITE,      2).
-define(EFILE_MODE_READ_WRITE, 3).  
-define(EFILE_MODE_APPEND,     4).
-define(EFILE_COMPRESSED,      8).

%% Use this mask to get just the mode bits to be passed to the driver.
-define(EFILE_MODE_MASK, 15).

%% Seek modes for the driver's seek function.
-define(EFILE_SEEK_SET, 0).
-define(EFILE_SEEK_CUR, 1).
-define(EFILE_SEEK_END, 2).

%% Options
-define(FILE_OPT_DELAYED_WRITE, 0).
-define(FILE_OPT_READ_AHEAD,    1).

%% IPREAD variants
-define(IPREAD_S32BU_P32BU, 0).



%%%-----------------------------------------------------------------
%%% Functions operating on a file through a handle. ?FD_DRV.
%%%
%%% Generic file contents operations.
%%%
%%% Supposed to be called by applications through module file.


%% Opens a file using the driver port Port. Returns {error, Reason}
%% | {ok, FileDescriptor}
open(Port, File, ModeList) when port(Port), list(File), list(ModeList) ->
    case open_mode(ModeList) of
	{error, _} = Error ->
	    Error;
	{ok, Mode, _Portopts, _Setopts} ->
	    open_int(Port, File, Mode, [])
    end;
open(_,_,_) ->
    {error, einval}.

%% Opens a file. Returns {error, Reason} | {ok, FileDescriptor}.
open(File, ModeList) when list(File), list(ModeList) ->
    case open_mode(ModeList) of
	{error, _} = Error ->
	    Error;
	{ok, Mode, Portopts, Setopts} ->
	    open_int({?FD_DRV, Portopts}, File, Mode, Setopts)
    end;
open(_, _) ->
    {error, einval}.

%% Opens a port that can be used for open/3 or read_file/2.
%% Returns {ok, Port} | {error, Reason}.
open(Portopts) when list(Portopts) ->
%     drv_open(?FD_DRV, Portopts).
    case drv_open(?FD_DRV, Portopts) of
	{error, _} = Error ->
	    Error;
	Other ->
	    Other
    end;
open(_) ->
    {error, einval}.

open_int({Driver, Portopts}, File, Mode, Setopts) ->
    case drv_open(Driver, Portopts) of
	{ok, Port} ->
	    open_int(Port, File, Mode, Setopts);
	{error, _} = Error ->
	    Error
    end;
open_int(Port, File, Mode, Setopts) ->
    case (catch list_to_binary([?FILE_OPEN, 
				i32(Mode band ?EFILE_MODE_MASK), File, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    case drv_command(Port, Cmd) of
		{ok, Number} ->
		    open_int_setopts(Port, Number, Setopts);
		{error, _} = Error ->
		    drv_close(Port),
		    Error
	    end
    end.

open_int_setopts(Port, Number, []) ->
    {ok, #file_descriptor{module = ?MODULE, data = {Port, Number}}};    
open_int_setopts(Port, Number, [Cmd | Tail]) ->
    case drv_command(Port, Cmd) of
	ok ->
	    open_int_setopts(Port, Number, Tail);
	{error, _} = Error ->
	    drv_close(Port),
	    Error
    end.



%% Returns ok.

close(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    case drv_command(Port, <<?FILE_CLOSE>>) of
	ok ->
	    drv_close(Port);
	{error, _} = Error ->
	    Error
    end;
%% Closes a port opened with open/1.
close(Port) when port(Port) ->
    drv_close(Port).



%% Returns {error, Reason} | ok.
write(#file_descriptor{module = ?MODULE, data = {Port, _}}, Bytes) ->
    case drv_command(Port, [?FILE_WRITE,Bytes]) of
	{ok, _Size} ->
	    ok;
	{error, _} = Error ->
	    Error
    end.

%% Returns ok | {error, {WrittenCount, Reason}}
pwrite(#file_descriptor{module = ?MODULE, data = {Port, _}}, L) when list(L) ->
    case (catch pwrite_int(Port, L, 0, [], [])) of
	{'EXIT', _} ->
	    {error, einval};
	Result ->
	    Result
    end;
pwrite(#file_descriptor{module = ?MODULE}, _) ->
    {error, einval}.

pwrite_int(Port, [], 0, [], []) ->
    ok;
pwrite_int(Port, [], N, Spec, Data) ->
    Header = list_to_binary([<<?FILE_PWRITEV, N:32>> | lists:reverse(Spec)]),
    case drv_command(Port, [Header | lists:reverse(Data)]) of
	{ok, _Size} ->
	    ok;
	{error, {_R, _Reason}} = Error ->
	    Error
    end;
pwrite_int(Port, [{Offs, Bin} | T], N, Spec, Data)
  when integer(Offs), 0 =< Offs, Offs < ?LARGEFILESIZE,
       binary(Bin) ->
    Size = size(Bin),
    pwrite_int(Port, T, N+1, [<<Offs:32, Size:32>> | Spec], [Bin | Data]);
pwrite_int(Port, [{Offs, Bytes} | T], N, Spec, Data)
  when integer(Offs), 0 =< Offs, Offs < ?LARGEFILESIZE,
       list(Bytes) ->
    Bin = list_to_binary(Bytes), % Might throw badarg
    Size = size(Bin),
    pwrite_int(Port, T, N+1, [<<Offs:32, Size:32>> | Spec], [Bin | Data]);
pwrite_int(Port, [_|_], _N, _Spec, _Data) ->
    {error, einval}.



%% Returns {error, Reason} | ok.
pwrite(#file_descriptor{module = ?MODULE, data = {Port, _}}, Offs, Bytes) 
       when integer(Offs), 0 =< Offs, Offs < ?LARGEFILESIZE ->
    case drv_command(Port, [?FILE_PWRITE, i32(Offs) | Bytes]) of
	{ok, _Size} ->
	    ok;
	{error, _} = Error ->
	    Error
    end;
pwrite(#file_descriptor{module = ?MODULE}, _, _) ->
    {error, einval}.



%% Returns {error, Reason} | ok.
sync(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    drv_command(Port, [?FILE_FSYNC]).

%% Returns {ok, Data} | eof | {error, Reason}.
read(#file_descriptor{module = ?MODULE, data = {Port, _}}, Size)
  when integer(Size), 0 =< Size, Size < ?LARGEFILESIZE ->
    case drv_command(Port, [?FILE_READ|i32(Size)]) of
	{ok, {0, _Data}} ->
	    eof;
	{ok, 0} ->
	    eof;
	{ok, {_Size, Data}} ->
	    {ok, Data};
	{error, enomem} ->
	    %% Garbage collecting here might help if the current processes
	    %% has some old binaries left.
	    erlang:garbage_collect(),
	    case drv_command(Port, [?FILE_READ|i32(Size)]) of
		{ok, {0, _Data}} ->
		    eof;
		{ok, {_Size, Data}} ->
		    {ok, Data};
		Other ->
		    Other
	    end;
	{error, _} = Error ->
	    Error
    end;
read(#file_descriptor{module = ?MODULE}, _) ->
    {error, einval}.

%% Returns {ok, [Data|eof, ...]} | {error, Reason}
pread(#file_descriptor{module = ?MODULE, data = {Port, _}}, L) when list(L) ->
    case (catch pread_int(Port, L, 0, [])) of
	{'EXIT', _} ->
	    {error, einval};
	Result ->
	    Result
    end;
pread(#file_descriptor{module = ?MODULE}, _) ->
    {error, einval}.

pread_int(Port, [], 0, []) ->
    {ok, []};
pread_int(Port, [], N, Spec) ->
    Command = list_to_binary([<<?FILE_PREADV, N:32>> | lists:reverse(Spec)]),
    case drv_command(Port, Command) of
	{ok, _} = Result ->
	    Result;
	{error, {_R, _Reason}} = Error ->
	    Error
    end;
pread_int(Port, [{Offs, Size} | T], N, Spec)
  when integer(Offs), 0 =< Offs, Offs < ?LARGEFILESIZE,
       integer(Size), 0 =< Size, Size < ?LARGEFILESIZE ->
    pread_int(Port, T, N+1, [<<Offs:32, Size:32>> | Spec]);
pread_int(Port, [_|_], _N, _Spec) ->
    {error, einval}.



%% Returns {ok, Data} | eof | {error, Reason}.
pread(#file_descriptor{module = ?MODULE, data = {Port, _}}, Offs, Size) 
  when integer(Offs), 0 =< Offs, Offs < ?LARGEFILESIZE,
       integer(Size), 0 =< Size, Size < ?LARGEFILESIZE ->
    case drv_command(Port, <<?FILE_PREADV, 1:32, Offs:32, Size:32>>) of
	{ok, [eof]} ->
	    eof;
	{ok, [Data]} ->
	    {ok, Data};
	{error, _} = Error ->
	    Error
    end;
%     case drv_command(Port, [?FILE_PREAD, i32(Offs), i32(Size)]) of
% 	{ok, {0, _Data}} ->
% 	    eof;
% 	{ok, {_Size, Data}} ->
% 	    {ok, Data};
% 	{error, _} = Error ->
% 	    Error
%     end;
pread(#file_descriptor{module = ?MODULE}, _, _) ->
    {error, einval}.



%% Returns {ok, Position} | {error, Reason}.
position(#file_descriptor{module = ?MODULE, data = {Port, _}}, At) ->
    case lseek_position(At) of
	{error, _} = Error ->
	    Error;
	{ok, Offs, Whence} ->
	    drv_command(Port, <<?FILE_LSEEK, Offs:32/signed, Whence:32>>)
    end.

%% Returns {error, Reaseon} | ok.
truncate(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    drv_command(Port, [?FILE_TRUNCATE]).



%% Returns {error, Reason} | {ok, BytesCopied}
copy(#file_descriptor{module = ?MODULE} = Source,
     #file_descriptor{module = ?MODULE} = Dest,
     Length)
  when integer(Length), Length >= 0;
       atom(Length) ->
    %% XXX Should be moved down to the driver for optimization.
    file:copy_opened(Source, Dest, Length);
copy(#file_descriptor{module = ?MODULE} = Source,
     #file_descriptor{module = ?MODULE} = Dest,
     _) ->
    {error, einval}.



ipread_s32bu_p32bu(#file_descriptor{module = ?MODULE, data = {Port, _}},
		   Offs,
		   MaxSize)
  when integer(Offs), 0 =< Offs, Offs < ?LARGEFILESIZE,
       integer(MaxSize), 0 =< MaxSize, MaxSize < ?LARGEFILESIZE ->
    drv_command(Port, <<?FILE_IPREAD, ?IPREAD_S32BU_P32BU,
		       Offs:32/big-unsigned, 
		       MaxSize:32/big-unsigned>>);
ipread_s32bu_p32bu(#file_descriptor{module = ?MODULE} = Handle,
		   Offs,
		   MaxSize)
  when integer(Offs), 0 =< Offs, Offs < ?LARGEFILESIZE ->
    ipread_s32bu_p32bu(Handle, Offs, ?LARGEFILESIZE-1).



%% Returns {ok, Contents} | {error, Reason}
read_file(File) ->
    case drv_open(?FD_DRV, [binary]) of
	{ok, Port} ->
	    Result = read_file(Port, File),
	    close(Port),
	    Result;
	{error, _} = Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% Functions operating on a file through a handle. ?FD_DRV.
%%%
%%% Specialized file contents operations.

%% Takes a Port opened with open/1.
read_file(Port, File) when port(Port) ->
    case (catch list_to_binary([?FILE_READ_FILE | File])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    case drv_command(Port, Cmd) of
		{ok, _} = Result ->
		    Result;
		{error, enomem} ->
		    %% It could possibly help to do a 
		    %% gargabe collection here, 
		    %% if the file server has some references
		    %% to binaries read earlier.
		    erlang:garbage_collect(),
		    drv_command(Port, Cmd);
		{error, _} = Error ->
		    Error
	    end
    end.

    

%% Returns {error, Reason} | ok.
write_file(File, Bin) ->
    case open(File, [binary, write]) of
	{ok, Handle} ->
	    Result = 
		case write(Handle, Bin) of
		    {ok, _} -> ok;
		    Error -> Error
		end,
	    close(Handle),
	    Result;
	{error, _} = Error ->
	    Error
    end.



%%%-----------------------------------------------------------------
%%% Functions operating on files without handle to the file. ?DRV.
%%%
%%% Supposed to be called by applications through module file.



%% Returns {ok, Port}, the Port should be used as first argument in all
%% the following functions. Returns {error, Reason} upon failure.
start() ->
    case catch erlang:open_port_prim({spawn, ?DRV}, []) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Port ->
	    {ok, Port}
    end.

stop(Port) when port(Port) ->
    catch erlang:port_close(Port),
    ok.



%%% The following functions take an optional Port as first argument.
%%% If the port is not supplied, a temporary one is opened and then
%%% closed after the request has been performed.



%% get_cwd/{0,1,2}

get_cwd() ->
    get_cwd_int(0).

get_cwd(Port) when port(Port) ->
    get_cwd_int(Port, 0);
get_cwd([]) ->
    get_cwd_int(0);
get_cwd([Letter, $: | _]) when $a =< Letter, Letter =< $z ->
    get_cwd_int(Letter - $a + 1);
get_cwd([Letter, $: | _]) when $A =< Letter, Letter =< $Z ->
    get_cwd_int(Letter - $A + 1);
get_cwd(_) ->
    {error, einval}.

get_cwd(Port, []) ->
    get_cwd_int(Port, 0);
get_cwd(Port, [Letter, $: | _]) when $a =< Letter, Letter =< $z ->
    get_cwd_int(Port, Letter - $a + 1);
get_cwd(Port, [Letter, $: | _]) when $A =< Letter, Letter =< $Z ->
    get_cwd_int(Port, Letter - $A + 1);
get_cwd(_, _) ->
    {error, einval}.

get_cwd_int(Drive) ->
    get_cwd_int({?DRV, []}, Drive).

get_cwd_int(Port, Drive) ->
    drv_command(Port, [?FILE_PWD, Drive]).



%% set_cwd/{1,2}

set_cwd(Dir) ->
    set_cwd_int({?DRV, []}, Dir).

set_cwd(Port, Dir) when port(Port) ->
    set_cwd_int(Port, Dir).

set_cwd_int(Port, Dir0) ->
    Dir = 
	(catch
	 case os:type() of
	     vxworks -> 
		 %% chdir on vxworks doesn't support
		 %% relative paths
		 %% must call get_cwd from here and use
		 %% absname/2, since
		 %% absname/1 uses file:get_cwd ...
		 case get_cwd_int(Port, 0) of
		     {ok, AbsPath} ->
			 filename:absname(Dir0, AbsPath);
		     _Badcwd ->
			 Dir0
		 end;
	     _Else ->
		 Dir0
	 end),
    %% Dir is now either a string or an EXIT tuple.
    %% An EXIT tuple will fail in the following catch.
    case (catch list_to_binary([?FILE_CHDIR, Dir, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% delete/{1,2}

delete(File) ->
    delete_int({?DRV, []}, File).

delete(Port, File) when port(Port) ->
    delete_int(Port, File).

delete_int(Port, File) ->
    case (catch list_to_binary([?FILE_DELETE, File, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% rename/{2,3}

rename(From, To) ->
    rename_int({?DRV, []}, From, To).

rename(Port, From, To) when port(Port) ->
    rename_int(Port, From, To).

rename_int(Port, From, To) ->
    case (catch list_to_binary([?FILE_RENAME, From, 0, To, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% make_dir/{1,2}

make_dir(Dir) ->
    make_dir_int({?DRV, []}, Dir).

make_dir(Port, Dir) when port(Port) ->
    make_dir_int(Port, Dir).

make_dir_int(Port, Dir) ->
    case (catch list_to_binary([?FILE_MKDIR, Dir, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% del_dir/{1,2}

del_dir(Dir) ->
    del_dir_int({?DRV, []}, Dir).

del_dir(Port, Dir) when port(Port) ->
    del_dir_int(Port, Dir).

del_dir_int(Port, Dir) ->
    case (catch list_to_binary([?FILE_RMDIR, Dir, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% read_file_info/{1,2}

read_file_info(File) ->
    read_file_info_int({?DRV, []}, File).

read_file_info(Port, File) when port(Port) ->
    read_file_info_int(Port, File).

read_file_info_int(Port, File) ->
    case (catch list_to_binary([?FILE_FSTAT, File, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% write_file_info/{2,3}

write_file_info(File, Info) ->
    write_file_info_int({?DRV, []}, File, Info).

write_file_info(Port, File, Info) when port(Port) ->
    write_file_info_int(Port, File, Info).

write_file_info_int(Port, 
		    File, 
		    #file_info{mode=Mode, 
			       uid=Uid, 
			       gid=Gid,
			       atime=Atime0, 
			       mtime=Mtime0, 
			       ctime=Ctime}) ->
    {Atime, Mtime} =
	case {Atime0, Mtime0} of
	    {undefined, Mtime0} -> {erlang:localtime(), Mtime0};
	    {Atime0, undefined} -> {Atime0, Atime0};
	    Complete -> Complete
	end,
    case (catch list_to_binary([?FILE_WRITE_INFO, 
				int_to_bytes(Mode), 
				int_to_bytes(Uid), 
				int_to_bytes(Gid),
				date_to_bytes(Atime), 
				date_to_bytes(Mtime), 
				date_to_bytes(Ctime),
				File, 0])) 
	of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% make_link/{2,3}

make_link(Old, New) ->
    make_link_int({?DRV, []}, Old, New).

make_link(Port, Old, New) when port(Port) ->
    make_link_int(Port, Old, New).

make_link_int(Port, Old, New) ->
    case (catch list_to_binary([?FILE_LINK, Old, 0, New, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% make_symlink/{2,3}

make_symlink(Old, New) ->
    make_symlink_int({?DRV, []}, Old, New).

make_symlink(Port, Old, New) when port(Port) ->
    make_symlink_int(Port, Old, New).

make_symlink_int(Port, Old, New) ->
    case (catch list_to_binary([?FILE_SYMLINK, Old, 0, New, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% read_link/{2,3}

read_link(Link) ->
    read_link_int({?DRV, []}, Link).

read_link(Port, Link) when port(Port) ->
    read_link_int(Port, Link).

read_link_int(Port, Link) ->
    case (catch list_to_binary([?FILE_READLINK, Link, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% read_link_info/{2,3}

read_link_info(Link) ->
    read_link_info_int({?DRV, []}, Link).

read_link_info(Port, Link) when port(Port) ->
    read_link_info_int(Port, Link).

read_link_info_int(Port, Link) ->
    case (catch list_to_binary([?FILE_LSTAT, Link, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd)
    end.



%% list_dir/{1,2}

list_dir(Dir) ->
    list_dir_int({?DRV, []}, Dir).

list_dir(Port, Dir) when port(Port) ->
    list_dir_int(Port, Dir).

list_dir_int(Port, Dir) ->
    case (catch list_to_binary([?FILE_READDIR, Dir, 0])) of
	{'EXIT', _} ->
	    {error, einval};
	Cmd ->
	    drv_command(Port, Cmd, fun drv_get_responses/1, [[]])
    end.



%%%-----------------------------------------------------------------
%%% Functions to communicate with the driver



%% Opens a driver port and converts any problems into {error, emfile}.
%% Returns {ok, Port} when succesful.

drv_open(Driver, Portopts) ->
    case catch erlang:open_port_prim({spawn, Driver}, Portopts) of
	{'EXIT', Reason} ->
	    {error, emfile};
	Port ->
	    {ok, Port}
    end.



%% Closes a port in a safe way. Returns ok.

drv_close(Port) ->
    catch erlang:port_close(Port),
    receive %% Ugly workaround in case the caller==owner traps exits
	{'EXIT', Port, _Reason} -> 
	    ok
    after 0 -> 
	    ok
    end.



%% Issues a command to a port and gets the response.
%% If Port is {Driver, Portopts} a port is first opened and 
%% then closed after the result has been received.
%% Returns {ok, Result} or {error, Reason}.

drv_command(Port, Command) ->
    drv_command(Port, Command, fun drv_get_response/1, []).

drv_command(Port, Command, Fun, ExtraArgs) when port(Port) ->
    case catch erlang:port_command(Port, Command) of
	{'EXIT', _} ->
	    {error, einval};
	_ ->
	    Fun([Port | ExtraArgs])
    end;
drv_command({Driver, Portopts}, Command, Fun, ExtraArgs) ->
    case drv_open(Driver, Portopts) of
	{ok, Port} ->
	    Result = drv_command(Port, Command, Fun, ExtraArgs),
	    drv_close(Port),
	    Result;
	{error, _} = Error ->
	    Error
    end.


    
%% Receives the response from a driver port.
%% Returns: {ok, ListOrBinary}|{error, Reason}

drv_get_response([Port]) ->
    erlang:bump_reductions(100),
    receive
	{Port, {data, [Response|Rest]}} ->
	    translate_response(Response, Rest);
	{'EXIT', Port, Reason} ->
	    {error, port_died}
    end.

drv_get_responses([Port, Result]) ->
    case drv_get_response([Port]) of
	ok ->
	    {ok, Result};
	{ok, Name} ->
	    drv_get_responses([Port, [Name|Result]]);
	{error, _} = Error ->
	    Error
    end.



%%%-----------------------------------------------------------------
%%% Utility functions.



%% Converts a list of mode atoms into an mode word for the driver.
%% Returns {ok, Mode, Portopts, Setopts} where Portopts is a list of 
%% options for erlang:open_port_prim/2 and Setopts is a list of 
%% setopt commands to send to the port, or {error, einval} upon failure.

open_mode(List) when list(List) ->
    case open_mode(List, 0, [], []) of
	{ok, Mode, Portopts, Setopts} when Mode band 
			  (?EFILE_MODE_READ bor ?EFILE_MODE_WRITE) 
			  == 0 ->
	    {ok, Mode bor ?EFILE_MODE_READ, Portopts, Setopts};
	Other ->
	    Other
    end.

open_mode([raw|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode, Portopts, Setopts);
open_mode([read|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_READ, Portopts, Setopts);
open_mode([write|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_WRITE, Portopts, Setopts);
open_mode([binary|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode, [binary | Portopts], Setopts);
open_mode([compressed|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_COMPRESSED, Portopts, Setopts);
open_mode([append|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_APPEND bor ?EFILE_MODE_WRITE, 
	      Portopts, Setopts);
open_mode([delayed_write|Rest], Mode, Portopts, Setopts) ->
    open_mode([{delayed_write, 64*1024, 2000}|Rest], Mode, Portopts, Setopts);
open_mode([{delayed_write, Size, Delay}|Rest], Mode, Portopts, Setopts) 
  when integer(Size), 0 =< Size, Size < ?LARGEFILESIZE,
       integer(Delay), 0 =< Delay, Delay < 1 bsl 32 ->
    open_mode(Rest, Mode, Portopts, 
	      [<<?FILE_SETOPT, ?FILE_OPT_DELAYED_WRITE, Size:32, Delay:32>> 
	       | Setopts]);
open_mode([{read_ahead, Size}|Rest], Mode, Portopts, Setopts)
  when integer(Size), 0 =< size, Size < ?LARGEFILESIZE ->
    open_mode(Rest, Mode, Portopts,
	      [<<?FILE_SETOPT, ?FILE_OPT_READ_AHEAD, Size:32>> | Setopts]);
open_mode([read_ahead|Rest], Mode, Portopts, Setopts) ->
    open_mode([{read_ahead, 64*1024}|Rest], Mode, Portopts, Setopts);
open_mode([], Mode, Portopts, Setopts) ->
    {ok, Mode, lists:reverse(Portopts), lists:reverse(Setopts)};
open_mode(_, _Mode, _Portopts, _Setopts) ->
    {error, einval}.



%% Converts a position tuple {bof, X} | {cur, X} | {eof, X} into
%% {ok, Offset, OriginCode} for the driver.
%% Returns {error, einval} upon failure.

lseek_position(Pos)
  when integer(Pos), 0 =< Pos, Pos < ?LARGEFILESIZE ->
    lseek_position({bof, Pos});
lseek_position(bof) ->
    lseek_position({bof, 0});
lseek_position(cur) ->
    lseek_position({cur, 0});
lseek_position(eof) ->
    lseek_position({eof, 0});
lseek_position({bof, Offset})
  when integer(Offset), 0 =< Offset, Offset < ?LARGEFILESIZE ->
    {ok, Offset, ?EFILE_SEEK_SET};
lseek_position({cur, Offset})
  when integer(Offset), -(?LARGEFILESIZE) =< Offset, Offset < ?LARGEFILESIZE ->
    {ok, Offset, ?EFILE_SEEK_CUR};
lseek_position({eof, Offset})
  when integer(Offset), -(?LARGEFILESIZE) =< Offset, Offset < ?LARGEFILESIZE ->
    {ok, Offset, ?EFILE_SEEK_END};
lseek_position(_) ->
    {error, einval}.



%% Translates the response from the driver into 
%% {ok, Result} or {error, Reason}.

translate_response(?FILE_RESP_OK, []) ->
    ok;
translate_response(?FILE_RESP_OK, Data) ->
    {ok, Data};
translate_response(?FILE_RESP_ERROR, List) when list(List) ->
    {error, list_to_atom(List)};
translate_response(?FILE_RESP_NUMBER, [X1, X2, X3, X4]) ->
    {ok, i32(X1, X2, X3, X4)};
translate_response(?FILE_RESP_DATA, [X1, X2, X3, X4|Data]) ->
    {ok, {i32(X1, X2, X3, X4), Data}};
translate_response(?FILE_RESP_INFO, List) when list(List) ->
    {ok, transform_ints(getints(List))};
translate_response(?FILE_RESP_NUMERR, [X1, X2, X3, X4 | List]) ->
    {error, {i32(X1, X2, X3, X4), list_to_atom(List)}};
translate_response(?FILE_RESP_LDATA, List) ->
    {ok, transform_ldata(List)};
translate_response(?FILE_RESP_N2DATA = X, 
		   [X1,X2,X3,X4, Y1,Y2,Y3,Y4, Z1,Z2,Z3,Z4 | D] = Data) ->
    Offset = i32(X1,X2,X3,X4),
    ReadSize = i32(Y1,Y2,Y3,Y4),
    Size = i32(Z1,Z2,Z3,Z4), 
    case {ReadSize, D} of
	{0, []} ->
	    {ok, {Size, Offset, eof}};
	{0, _} ->
	    {error, {bad_response_from_port, X, Data}};
	{_, []} ->
	    {error, {bad_response_from_port, X, Data}};
	_ ->
	    {ok, {Size, Offset, D}}
    end;
translate_response(?FILE_RESP_N2DATA, 
		   <<Offset:32, 0:32, Size:32>>) ->
    {ok, {Size, Offset, eof}};
translate_response(?FILE_RESP_N2DATA, 
		   [<<Offset:32, 0:32, Size:32>> | <<>>]) ->
    {ok, {Size, Offset, eof}};
translate_response(?FILE_RESP_N2DATA = X, 
		   [<<_:32, 0:32, _:32>> | _] = Data) ->
    {error, {bad_response_from_port, X, Data}};
translate_response(?FILE_RESP_N2DATA = X, 
		   [<<_:32, _:32, _:32>> | <<>>] = Data) ->
    {error, {bad_response_from_port, X, Data}};
translate_response(?FILE_RESP_N2DATA, 
		   [<<Offset:32, _ReadSize:32, Size:32>> | D]) ->
    {ok, {Size, Offset, D}};
translate_response(?FILE_RESP_EOF, []) ->
    eof;
translate_response(X, Data) ->
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



transform_ldata(<<0:32>>) ->
    [];
transform_ldata([<<N:32, Sizes/binary>> | Datas]) ->
    transform_ldata(N, Sizes, Datas, []);
%% Binary mode above, list mode below
transform_ldata([X1,X2,X3,X4 | List]) ->
    transform_ldata(i32(X1,X2,X3,X4), List).

%% List mode
transform_ldata(0, []) ->
    [];
transform_ldata(N, List) ->
    transform_ldata(N, List, []).

%% List mode
transform_ldata(0, List, Sizes) ->
    transform_ldata(0, List, lists:reverse(Sizes), []);
transform_ldata(N, [X1,X2,X3,X4 | List], Sizes) ->
    transform_ldata(N-1, List, [i32(X1,X2,X3,X4) | Sizes]).

transform_ldata(1, <<0:32>>, <<>>, R) ->
    lists:reverse(R, [eof]);
transform_ldata(1, <<Size:32>>, Data, R) 
  when binary(Data), size(Data) == Size ->
    lists:reverse(R, [Data]);
transform_ldata(N, <<0:32, Sizes/binary>>, [<<>> | Datas], R) ->
    transform_ldata(N-1, Sizes, Datas, [eof | R]);
transform_ldata(N, <<Size:32, Sizes/binary>>, [Data | Datas], R) 
  when binary(Data), size(Data) == Size ->
    transform_ldata(N-1, Sizes, Datas, [Data | R]);
%% Binary mode above, list mode below
transform_ldata(0, [], [], R) ->
    lists:reverse(R);
transform_ldata(0, List, [0 | Sizes], R) ->
    transform_ldata(0, List, Sizes, [eof | R]);
transform_ldata(0, List, [Size | Sizes], R) ->
    {Front, Rear} = lists_split(List, Size),
    transform_ldata(0, Rear, Sizes, [Front | R]).



lists_split(List, 0) when list(List) ->
    {[], List};
lists_split(List, N) when list(List), integer(N), N < 0 ->
    erlang:fault(badarg, [List, N]);
lists_split(List, N) when list(List), integer(N) ->
    case lists_split(List, N, []) of
	premature_end_of_list ->
	    erlang:fault(badarg, [List, N]);
	Result ->
	    Result
    end.

lists_split(List, 0, Rev) ->
    {lists:reverse(Rev), List};
lists_split([], N, Rev) ->
    premature_end_of_list;
lists_split([Hd | Tl], N, Rev) ->
    lists_split(Tl, N-1, [Hd | Rev]).
