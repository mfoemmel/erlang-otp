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
-module(file).

%% A simple file server.

-behaviour(gen_server).

-export([get_cwd/0, get_cwd/1, set_cwd/1, delete/1, rename/2,
	 make_dir/1, del_dir/1, list_dir/1,
	 read_file_info/1, write_file_info/2,
	 read_link_info/1, read_link/1,
	 make_link/2, make_symlink/2,
	 read_file/1, write_file/2,
	 change_owner/2, change_owner/3, change_group/2,
	 change_mode/2, change_time/2, change_time/3]).
-export([consult/1,path_consult/2,eval/1,path_eval/2]).
-export([path_open/3,open/2,close/1,position/2,truncate/1, sync/1]).
-export([format_error/1]).

-export([pread/3, pwrite/3]).

%% Obsolete.

-export([file_info/1]).

%% Private exports.
-export([start/0,start_link/0,stop/0, file/3, init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2]).
-export([rawopen/2, raw_read_file_info/1, raw_write_file_info/2,
	 read/2, write/2]).
-export([relay/1]).

-include("file.hrl").

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
-define(FILE_READLINK,        20).
-define(FILE_LINK,             21).
-define(FILE_SYMLINK,          22).


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
-define(EFILE_MODE_APPEND, 4).
-define(EFILE_COMPRESSED, 8).

%% Use this mask to get just the mode bits to be passed to the driver.
-define(EFILE_MODE_MASK, 15).

%% These are used internally in this module, and never passed to the driver.
-define(BINARY_FILE, 2#10000000000).
-define(RAW_PORT,    2#01000000000).

%% Seek modes for the efile driver's seek function.

-define(EFILE_SEEK_SET, 0).
-define(EFILE_SEEK_CUR, 1).
-define(EFILE_SEEK_END, 2).


format_error({Line, file, Reason}) ->
    io_lib:format("~w", [Reason]);
format_error({Line, Mod, Reason}) ->
    Mod:format_error(Reason);
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

%%% The following interface functions take one or more filename arguments.

get_cwd() ->        call({get_cwd, 0}).

get_cwd([Letter, $:]) when $A =< Letter, Letter =< $Z ->
    call({get_cwd, Letter-$A+1});
get_cwd([Letter, $:]) when $a =< Letter, Letter =< $z ->
    call({get_cwd, Letter-$a+1});
get_cwd(_) ->
    {error, einval}.

set_cwd(Dirname) -> check_and_call(set_cwd, [file_name2(Dirname)]).
delete(Name) ->     check_and_call(delete, [file_name2(Name)]).
rename(From, To) -> check_and_call(rename, [file_name2(From), file_name2(To)]).
make_dir(Name) ->   check_and_call(make_dir, [file_name2(Name)]).
del_dir(Name) ->    check_and_call(del_dir, [file_name2(Name)]).
read_file_info(Name) -> check_and_call(read_file_info, [file_name2(Name)]).
read_link_info(Name) -> check_and_call(read_link_info, [file_name2(Name)]).
read_link(Name) -> check_and_call(read_link, [file_name2(Name)]).
write_file_info(Name, Info) when record(Info, file_info) ->
    check_and_call(write_file_info, [file_name2(Name), Info]).
list_dir(Name) ->   check_and_call(list_dir, [file_name2(Name)]).
read_file(Name) ->  check_and_call(read_file, [file_name2(Name)]).

make_link(Old, New) ->
    check_and_call(make_link, [file_name2(Old), file_name2(New)]).

make_symlink(Old, New) ->
    check_and_call(make_symlink, [file_name2(Old), file_name2(New)]).

write_file(Name, Bin) ->
    check_and_call(write_file, [file_name2(Name), check_binary(Bin)]).

open(Name, Mode0) ->
    case open_mode(Mode0) of
	Mode when Mode band ?RAW_PORT /= 0 ->
	    ll_raw_open(Name, Mode bxor ?RAW_PORT);
	Other ->
	    check_and_call(open, [file_name2(Name), Other])
    end.

%% Obsolete function.

file_info(Name) ->
    case read_file_info(Name) of
	{ok, #file_info {size=Size, type=Type, access=Access,
			 atime=Atime0, mtime=Mtime0}} ->
	    {{A1, A2, A3}, {A4, A5, A6}} = Atime0,
	    Atime = {A1, A2, A3, A4, A5, A6},
	    {{M1, M2, M3}, {M4, M5, M6}} = Mtime0,
	    Mtime = {M1, M2, M3, M4, M5, M6},
	    {ok, {Size, Type, Access, Atime, Mtime, unused, unused}};
	Other ->
	    Other
    end.

%% Raw functions, operating on the local node only.
%% This function is obsolete and undocumented!  Don't use!

rawopen(Name, Mode) ->
    ll_raw_open(Name, open_mode(Mode)).

raw_read_file_info(Name) ->
    Args = [file_name2(Name)],
    case check_args(Args) of
	ok ->
	    [N] = Args,
	    Port = mkport([?FILE_FSTAT, file_name2(N), 0], 0),
	    Result = get_response(Port),
	    unlink(Port),
	    exit(Port, die),
	    Result;
	Error ->
	    Error
    end.

raw_write_file_info(Name, Info) ->
    Args = [file_name2(Name)],
    case check_args(Args) of
	ok ->
	    [N] = Args,
	    Command = [?FILE_WRITE_INFO, info_to_list(Name, Info)],
	    Port = mkport(Command, 0),
	    Result = get_response(Port),
	    unlink(Port),
	    exit(Port, die),
	    Result;
	Error ->
	    Error
    end.


%%% The following interface functions operate on open files.
%%% The File argument must be either a Pid or a fd record.

read(File, Sz) when pid(File) ->
    case io:get_chars(File, '', Sz) of
	List when list(List) ->
	    {ok, List};
	Other ->
	    Other
    end;
read(File, Sz) when record(File, fd) ->
    case ll_read(File, Sz) of
	{error, enomem} ->
	    %% Garbage collecting here might help if the current processes
	    %% has some old binaries left.

	    erlang:garbage_collect(),
	    ll_read(File, Sz);
	Other ->
	    Other
    end.

%% No Whence supported, only absolute positions
pread(File, At, Sz) when record(File, fd) , integer(At), At >= 0->
    ll_pread(File, At, Sz);
pread(File, At, Sz) when pid(File) ->
    request(File, {pread, At, Sz}),
    wait_file_reply(File).

write(File, Bytes) when pid(File) ->
    io:put_chars(File, Bytes);
write(File, Bytes) when record(File, fd) ->
    ll_write(File, Bytes).


pwrite(File, At, Bytes) when record(File, fd), integer(At), At >= 0 ->
    ll_pwrite(File, At, Bytes);
pwrite(File, At, Bytes) when pid(File) ->
    request(File, {pwrite, At, Bytes}),
    wait_file_reply(File).

sync(File) when pid(File) ->
    request(File, sync),
    wait_file_reply(File);
sync(File) when record(File, fd) ->
    ll_sync(File).

close(File) when pid(File) ->
    unlink(File),
    exit(File, die),
    ok;
close(File) when record(File, fd) ->
    Port = File#fd.port,
    unlink(Port),
    exit(Port, die),
    ok.

position(File, At) when pid(File) ->
    request(File, {position,At}),
    wait_file_reply(File);
position(File, At) when record(File, fd) ->
    case position_file(File, At, []) of
	{ok, Res, _} -> Res;
	{error, E, _} -> E
    end.

truncate(File) when pid(File) ->
    request(File, truncate),
    wait_file_reply(File);
truncate(File) when record(File, fd) ->
    ll_truncate(File).


%%% The following functions, built upon the other interface functions,
%%% provide a higher-lever interface to files.

consult(File) ->
    case open(File, [read]) of
	{ok, Fd} ->
	    R = consult_stream(Fd),
	    close(Fd),
	    R;
	Error ->
	    Error
    end.
    
path_consult(Path, File) ->
    case path_open(Path, File, [read]) of
	{ok,Fd,Full} ->
	    case consult_stream(Fd) of
		{ok, List} ->
		    close(Fd),
		    {ok, List, Full};
		{error, E} ->
		    close(Fd),
		    {error, E}
	    end;
	Error ->
	    Error
    end.

eval(File) ->
    case open(File, [read]) of
	{ok,Fd} ->
	    R = eval_stream(Fd, erl_eval:new_bindings()),
	    close(Fd),
	    R;
	Error ->
	    Error
    end.

path_eval(Path, File) ->
    case path_open(Path, File, [read]) of
	{ok,Fd,Full} ->
	    case eval_stream(Fd, erl_eval:new_bindings()) of
		ok ->
		    close(Fd),
		    {ok,Full};
		{error,E} ->
		    close(Fd),
		    {error,E}
	    end;
	Error ->
	    Error
    end.

consult_stream(Fd) ->
    case catch consult_stream(io:read(Fd, ''), Fd) of
	{'EXIT', Reason} ->
	    {error, einval};
	{error, Reason} ->
	    {error, Reason};
	List ->
	    {ok, List}
    end.

consult_stream({ok,Term}, Fd) ->
    [Term|consult_stream(io:read(Fd, ''), Fd)];
consult_stream({error,What}, Fd) ->
    throw({error, What});
consult_stream(eof, Fd) ->
    [].

eval_stream(Fd, Bs) ->
    case eval_stream(Fd, [], Bs) of
	[] -> ok;
	[Error1 | _] -> {error,Error1}
    end.

eval_stream(Fd, E, Bs) ->
    eval_stream(io:parse_erl_exprs(Fd, ''), Fd, E, Bs).

eval_stream({ok,Form,EndLine}, Fd, E, Bs0) ->
    case catch erl_eval:exprs(Form, Bs0) of
	{value,V,Bs} ->
	    eval_stream(Fd, E, Bs);
	{'EXIT',Reason} ->
	    eval_stream(Fd, [{EndLine,file,Reason}|E], Bs0)
    end;
eval_stream({error,What,EndLine}, Fd, E, Bs) ->
    eval_stream(Fd, [What | E], Bs);
eval_stream({eof,EndLine}, Fd, E, Bs) ->
    lists:reverse(E).

%% path_open(Paths, Filename, Mode) ->
%%	{ok,FileDescriptor,FullName}
%%	{error,Reason}
%%
%% Searches the Paths for file Filename which can be opened with Mode.
%% The path list is ignored if Filename contains an absolute path.

path_open(Ps, Name0, Mode) ->
    case file_name2(Name0) of
	{error, Error} ->
	    {error, Error};
	Name ->
	    case filename:pathtype(Name) of
		relative ->
		    path_open1(Ps, Name, Mode, enoent);
		_ ->
		    path_open1(Name, Mode)
	    end
    end.

path_open1(Name, Mode) ->
    case open(Name, Mode) of
	{ok, Fd} -> {ok, Fd, Name};
	Error -> Error
    end.

path_open1([Path0|Rest], Name0, Mode, LastError) ->
    case file_name2(Path0) of
	{error, Error} ->
	    {error, Error};
	Path ->
	    Name = filename:join(Path, Name0),
	    case open(Name, Mode) of
		{ok, Fd} ->
		    {ok, Fd, Name};
		{error, enoent} ->
		    path_open1(Rest, Name0, Mode, LastError);
		Error ->
		    Error
	    end
    end;
path_open1([], Name, Mode, LastError) ->
    {error, LastError}.

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
    

%% The basic file server and start-up.
%%
%% The file server just handles the open command/message and acts as a
%% router for messages between the port and the file processes. If a
%% file process terminates we close the associated file.

start() -> do_start(start).
start_link() -> do_start(start_link).

do_start(F) ->
    case init:get_argument(master) of
	error ->
	    gen_server:F({local,file_server}, file, [], []);
	{ok, [[Node]]} ->
	    start_slave([list_to_atom(Node)],F)
    end.

start_slave([Node],F) when atom(Node) ->
    %%    If we are not alive here things will get out of hand
    Filer = rpc:call(Node,erlang,whereis,[file_server]),
    case Filer of
	XX when pid(XX) ->
	    Id = start_relay(Filer, F),
	    register(file_server,Id),
	    {ok,Id};
	_ ->
	    error_logger:error_msg('can not get remote filer ',[]),
	    %% Time to die
	    halt()
    end.

%% We have the relay process file internal.
%% We do not need to load slave as an mandatory module
%% during system startup.

start_relay(Filer, start_link) ->
    spawn_link(file,relay,[Filer]);
start_relay(Filer, _) ->
    spawn(file,relay,[Filer]).

relay(Pid) ->
    receive
        X ->
            Pid ! X
    end,
    relay(Pid).
    
stop() -> call(stop).

init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, efile}, []),
    {ok,Port}.


%% This function handles all functions that operate on or more
%% filenames.  It uses a single open port.

handle_call({open, Name, Mode}, From, Port) ->
    spawn(file, file, [From, Name, Mode]),
    {noreply, Port};

handle_call({read_file, Name}, _From, Port) ->
    {reply, do_read_file(Name), Port};

handle_call({write_file, F, Bin}, _From, Port) ->
    {reply, do_write_file(F, Bin), Port};

handle_call({set_cwd, Name}, _From, Port) ->
    Mod_name = case os:type() of
		   vxworks -> 
		       %%% chdir on vxworks doesn't support
		       %%% relative paths
		       %%% must call get_cwd from here and use
		       %%% absname/2, since
		       %%% absbase/1 uses file:get_cwd ...
		       case handle_call({get_cwd, 0}, _From, Port) of
			   {reply, {ok, AbsPath}, _Port} ->
			       filename:absname(Name, AbsPath);
			   _Badcwd ->
			       Name
		       end;
		   _Else ->
		       Name
	       end,
    {reply, fm_op(?FILE_CHDIR, Mod_name, Port), Port};

handle_call({delete, Name}, _From, Port) ->
    {reply, fm_op(?FILE_DELETE, Name, Port), Port};

handle_call({rename, Fr, To}, _From, Port) ->
    {reply, fm_op(?FILE_RENAME, [Fr, 0, To], Port), Port};

handle_call({make_dir, Name}, _From, Port) ->
    {reply, fm_op(?FILE_MKDIR, Name, Port), Port};

handle_call({del_dir, Name}, _From, Port) ->
    {reply, fm_op(?FILE_RMDIR, Name, Port), Port};

handle_call({list_dir, Name}, _From, Port) ->
    {reply, list_dir_op(Name, Port), Port};

handle_call({get_cwd, Drive}, _From, Port) ->
    port_command(Port, [?FILE_PWD, Drive]),
    {reply, get_response(Port), Port};

handle_call({read_file_info, Name}, _From, Port) ->
    {reply, fm_op(?FILE_FSTAT, Name, Port), Port};

handle_call({write_file_info, Name, Info}, _From, Port) ->
    {reply, write_file_info_op(?FILE_WRITE_INFO, Name, Info, Port), Port};

handle_call({read_link_info, Name}, _From, Port) ->
    {reply, fm_op(?FILE_LSTAT, Name, Port), Port};

handle_call({read_link, Name}, _From, Port) ->
    {reply, fm_op(?FILE_READLINK, Name, Port), Port};

handle_call({make_link, Old, New}, _From, Port) ->
    {reply, fm_op(?FILE_LINK, [Old, 0, New], Port), Port};

handle_call({make_symlink, Old, New}, _From, Port) ->
    {reply, fm_op(?FILE_SYMLINK, [Old, 0, New], Port), Port};

handle_call(stop, _From, Port) ->
    {stop, normal, stopped, Port};

handle_call(_, _, Port) ->
    {noreply, Port}.

handle_cast(_, Port) ->
    {noreply, Port}.

handle_info({'EXIT', Port, Reason}, Port) ->
    error_logger:error_msg('Port controlling ~w terminated in file_server',
			   [file_server]),
    {stop, normal, Port};
handle_info(_, Port) ->
    {noreply, Port}.

terminate(_Reason, Port) ->
    port_close(Port).


%% Reads a complete file in one operation.
%%
%% Note: This function (as well as do_write_file/2) should be called
%% from within the file server to allow remote filers.  It must not
%% called from the client.

do_read_file(Name) -> 
    case catch open_port({spawn, efile}, [binary]) of
	{'EXIT', Reason} ->
	    {error, Reason};
	P ->
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
	    exit(P, die),
	    Result
    end.

do_write_file(File, Bin) ->
    case open(File, [binary, write, raw]) of
	{ok, Fdesc} ->
	    Result = 
		case write(Fdesc, Bin) of
		    {ok, _} -> ok;
		    Error -> Error
		end,
	    close(Fdesc),
	    Result;
	Error ->
	    Error
    end.

fm_op(Op, Chars, Port) ->
    port_command(Port, [Op, Chars, 0]),
    get_response(Port).

list_dir_op(Chars, Port) ->
    port_command(Port, [?FILE_READDIR|[Chars, 0]]),
    collect_files(Port, []).

collect_files(Port, Result) ->
    case get_response(Port) of
	ok ->
	    {ok, Result};
	{ok, Name} ->
	    collect_files(Port, [Name|Result]);
	Error ->
	    Error
    end.

write_file_info_op(Op, Name, Info, Port) ->
    port_command(Port, [Op, info_to_list(Name, Info)]),
    get_response(Port).

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

int_to_bytes(Int) when integer(Int) ->
    i32(Int);
int_to_bytes(undefined) ->
    [255, 255, 255, 255].

date_to_bytes(undefined) ->
    MinusOne = [255, 255, 255, 255],
    [MinusOne, MinusOne, MinusOne, MinusOne, MinusOne, MinusOne];
date_to_bytes({{Y, Mon, D}, {H, Min, S}}) ->
    [i32(Y), i32(Mon), i32(D), i32(H), i32(Min), i32(S)].
    

%% This is the process which is spawned for each file which is opened.

file({ReplyTo, ReplyAs}, Name, Mode) ->
    process_flag(trap_exit, true),
    link(ReplyTo),
    put(owner, ReplyTo),
    case ll_open(Name, Mode) of
	{error, Reason} ->
	    unlink(ReplyTo),
	    gen_server:reply({ReplyTo, ReplyAs}, {error, Reason}),
	    exit(normal);
	{ok, Fd} ->
	    put(fd, Fd),
	    gen_server:reply({ReplyTo, ReplyAs}, {ok, self()}),
	    file_loop([])
    end.

file_loop(Buf0) ->
    receive
	{file_request,From,ReplyAs,Request} when pid(From) ->
	    Buf = file_request(Request, From, ReplyAs, Buf0),
	    file_loop(Buf);
	{io_request,From,ReplyAs,Request} when pid(From) ->
	    Buf = io_request(Request, From, ReplyAs, Buf0),
	    file_loop(Buf);
	%% An exit signal from anyone causes us to exit.
	{'EXIT',_,Reason}  ->
	    unlink(get(owner)),
	    exit(normal);               
	Other ->				%Ignore other messages
	    file_loop(Buf0)
    end.

%% file_request(Request, Sender, ReplyAs, Descriptor, Server, Buffer)
%%  Returns the new buffer.

file_request(Req, From, ReplyAs, Buf0) ->
    case file_request(Req, Buf0) of
	{Status,Reply,Buf} ->
	    file_reply(From, ReplyAs, Reply),
	    Buf;
	{exit,normal} ->
	    file_reply(From, ReplyAs, ok),
	    exit(normal);
	{exit,R} ->
	    file_reply(From, ReplyAs, {error,R}),
	    exit(normal)
    end.

file_request({position,At}, Buf) ->
    position_file(get(fd), At, Buf);
file_request(truncate, Buf) ->
    case ll_truncate(get(fd)) of
	{error, Reason} -> {exit, Reason};
	ok -> {ok, ok, []}
    end;
file_request(sync, Buf) ->
    case ll_sync(get(fd)) of
	{error, Reason} -> {exit, Reason};
	ok -> {ok, ok, Buf}
    end;

file_request({pread, At, Sz}, Buf) -> 
    case position_file(get(fd), At, Buf) of
	{error, Err, Buf2} -> 
	    {error, Err, Buf2};
	{ok, Res, Buf2} -> 
	    case ll_read(get(fd), Sz) of
		eof -> {ok, eof, []};
		{ok, Data} -> {ok, {ok, Data}, []};
		{error, Reason} -> {error, {error, Reason}, []}
	    end
    end;

file_request({pwrite, At, Data}, Buf) ->
    case position_file(get(fd), At, Buf) of
	{error, Err, Buf2} -> 
	    {error, Err, Buf2};
	{ok, Res, Buf2} -> 
	    io_request({put_chars, Data}, Buf2)
    end;

file_request(close, Buf) ->
    {exit,normal};
file_request(R, Buf) ->				% Unknown request
    {ok,{error,{request,R}},Buf}.		% Ignore but give error (?)

%% io_request(Request, Sender, ReplyAs, Buffer)
%%  Returns the new buffer.

io_request(Req, From, ReplyAs, Buf0) ->
    case io_request(Req, Buf0) of
	{Status, Reply, Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
	{exit, normal} ->
	    io_reply(From, ReplyAs, ok),
	    exit(normal);
	{exit, R} ->
	    io_reply(From, ReplyAs, {error,R}),
	    exit(normal)
    end.

io_request({put_chars, Chars}, Buf) ->
    case ll_write(get(fd), Chars) of
	{error, Reason} ->
	    {exit, Reason};
	ok ->
	    {ok, ok, Buf}
    end;

io_request({put_chars,Mod,Func,Args}, Buf) ->
    case catch apply(Mod,Func,Args) of
	Chars when list(Chars) ->
	    io_request({put_chars,Chars}, Buf);
	Other ->
	    {error,{error,Func}, Buf}
    end;
io_request({get_until,Prompt,M,F,As}, Buf) ->
    get_until(M, F, As, Buf);
io_request({requests,Reqs},  Buf) ->
    io_requests(Reqs, {ok,ok,Buf});
io_request(R, Buf) ->	                	%Unknown request
    {ok,{error,{request,R}},Buf}.		%Ignore but give error (?)

%% Status = io_requests(RequestList, PrevStat, Descriptor, Server)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,Res,Buf}) ->
    io_requests(Rs, io_request(R, Buf));
io_requests([_|_], Error) ->
    Error;
io_requests([], Stat) ->
    Stat.

%% position_file(Descriptor, At, ReadBuffer)
%%  Position the file at At. Returns:
%%
%%	{ok,NewPosition,NewReadBuffer}
%%	{error,{error,position},NewReadBuffer}
%%
%%  NewReadBuffer is always [] here. This is less efficient but makes
%%  life easier.

position_file(Fd, At, Buf) ->
    case file_position(At, 0, length(Buf)) of
	{error, Reason} ->
	    {error, {error, Reason}, []};
	{ok, Offs, Whence} ->
	    case ll_lseek(Fd, Offs, Whence) of
		{error, Reason} ->
		    {error,{error,Reason},[]};
		{ok, Res} ->
		    {ok, {ok, Res}, []}
	    end
    end.

%% get_until(Module, Function, Arguments, Descriptor, Server, Buffer)
%%  Gets characters from the input file as long as the applied function
%%  returns {more,Continuation}. Returns:
%%
%%	{Status,Result,NewSaveBuffer}
%%	{exit,Reason}

get_until(M, F, As, Buf) ->
    get_until1(catch apply(M, F, [[],Buf|As]), M, F, As).

get_until1({more,Cont}, M, F, As) ->
    case ll_read(get(fd), 128) of
	eof ->
	    get_until1(catch apply(M, F, [Cont,eof|As]), M, F, As);
	{ok, Bs} when list(Bs) ->
	    get_until1(catch apply(M, F, [Cont,Bs|As]), M, F, As);
	{ok, Bin} when binary(Bin) ->
	    get_until1(catch apply(M, F, [Cont,binary_to_list(Bin)|As]),
		       M, F, As);
	{error,Reason} ->
	    {exit,Reason}
    end;

get_until1({done,Result,Buf}, M, F, As) ->
    {ok,Result,Buf};
get_until1(Other, M, F, As) ->
    {error,{error,F},[]}.
  


%%% Internal functions operating on a port.
%%% (The 'll' in all these calls stands for 'low level'.)

ll_raw_open(Name, Mode) ->
    Args = [file_name2(Name), Mode],
    case check_args(Args) of
	ok ->
	    [N, M] = Args,
	    ll_open(N, M);
	Error ->
	    Error
    end.

%% ll_open(Name, Mode) -> {ok, #fd}|{error, Reason}

ll_open(File, Mode) ->
    Cmd = [?FILE_OPEN, i32(Mode band ?EFILE_MODE_MASK), File, 0],
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

ll_write(Fdesc, Bytes) ->
    case ll_command(Fdesc, [?FILE_WRITE,Bytes]) of
	{ok, _Size} ->
	    ok;
	Other ->
	    Other
    end.

ll_pwrite(Fdesc, Offs, Bytes) ->
    case ll_command(Fdesc, [?FILE_PWRITE,i32(Offs), Bytes]) of
	{ok, _Size} ->
	    ok;
	Other ->
	    Other
    end.

ll_sync(Fdesc) ->
    ll_command(Fdesc, [?FILE_FSYNC]).

ll_read(Fdesc, Size) ->
    case ll_command(Fdesc, [?FILE_READ|i32(Size)]) of
	{ok, {0, _Data}} ->
	    eof;
	{ok, {_Size, Data}} ->
	    {ok, Data};
	Other ->
	    Other
    end.


ll_pread(Fdesc, Offs, Size) ->
    case ll_command(Fdesc, [?FILE_PREAD, i32(Offs), i32(Size)]) of
	{ok, {0, _Data}} ->
	    eof;
	{ok, {_Size, Data}} ->
	    {ok, Data};
	Other ->
	    Other
    end.

ll_lseek(Fdesc, Offset, Whence) ->
    ll_command(Fdesc, [?FILE_LSEEK, i32(Offset), i32(Whence)]).

ll_truncate(Fdesc) ->
    ll_command(Fdesc, [?FILE_TRUNCATE]).

ll_command(Fdesc, Command) ->
    Port = Fdesc#fd.port,
    port_command(Port, Command),
    get_response(Port).

mkport(Cmd, BinaryFile) ->
    Opts = if
	BinaryFile == 0 -> [];
	true -> [binary]  %% flag is set
    end,
    P = open_port({spawn, efile}, Opts),	% can fail
    port_command(P, Cmd),
    P.


%%% Utility functions.

%% file_name2(FileName)
%% 	Generates a flat file name from a deep list of atoms and 
%% 	characters (integers).

file_name2([C|T]) when integer(C), C > 0, C =< 255 ->
    case file_name2(T) of
	{error, Error} ->
	    {error, Error};
	Name ->
	    [C|Name]
    end;
file_name2([H|T]) ->
    case {file_name2(H), file_name2(T)} of
	{{error, Error}, _} ->
	    {error, Error};
	{_, {error, Error}} ->
	    {error, Error};
	{N1, N2} ->
	    N1++N2
    end;
file_name2([]) ->
    [];
file_name2(N) when atom(N) ->
    atom_to_list(N);
file_name2(_) ->
    {error, einval}.

check_binary(Bin) when binary(Bin) ->
    Bin;
check_binary(List) when list(List) ->
    %% Convert the list to a binary, to avoid copying a list to the file server.
    list_to_binary(List);
check_binary(_) ->
    {error, einval}.

open_mode(Mode) when list(Mode) ->
    new_open_mode(Mode);
open_mode(Mode) ->
    new_open_mode(translate_old_mode(Mode)).

new_open_mode(List) ->
    case new_open_mode(List, 0) of
	Mode when Mode band (?EFILE_MODE_READ bor ?EFILE_MODE_WRITE) == 0 ->
	    Mode bor ?EFILE_MODE_READ;
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
new_open_mode([compressed|Rest], Result) ->
    new_open_mode(Rest, Result bor ?EFILE_COMPRESSED);
new_open_mode([append|Rest], Result) ->
    new_open_mode(Rest, Result bor ?EFILE_MODE_APPEND bor ?EFILE_MODE_WRITE);
new_open_mode([], Result) ->
    Result;
new_open_mode(_, _) ->
    {error, einval}.

translate_old_mode(read) -> [read];
translate_old_mode(write) -> [write];
translate_old_mode(read_write) -> [read, write];
translate_old_mode({binary, Mode}) -> [binary|translate_old_mode(Mode)];
translate_old_mode({character, Mode}) -> translate_old_mode(Mode);
translate_old_mode(_) -> [this_is_an_error].

%% file_position(At, Offset, VirtualOffset)

file_position(Pos, _, Voff) when integer(Pos) ->
    file_position(bof, Pos, Voff);
file_position(bof, Offset, Voff) when integer(Offset) ->
    {ok, Offset, ?EFILE_SEEK_SET};
file_position({From,Offset}, _, Voff) ->
    file_position(From, Offset, Voff);
file_position(cur, Offset, Voff) when integer(Offset) ->
    {ok, Offset-Voff, ?EFILE_SEEK_CUR};
file_position(eof, Offset, Voff) when integer(Offset) ->
    {ok, Offset, ?EFILE_SEEK_END};
file_position(_, _, _) ->
    {error, einval}.

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

call(Req) -> gen_server:call(file_server, Req, infinity).

check_and_call(Command, Args) ->
    case check_args(Args) of
	ok ->
	    gen_server:call(file_server, list_to_tuple([Command|Args]),
			    infinity);
	Error ->
	    Error
    end.

check_args([{error, Error}|_Rest]) ->
    {error, Error};
check_args([_Name|Rest]) ->
    check_args(Rest);
check_args([]) ->
    ok.

%% io_reply(From, ReplyAs, Reply)
%% request(Io, Request)
%% file_reply(From, ReplyAs, Reply)
%% wait_file_reply(From)
%%  The functions for sending and waiting for i/o command acknowledgement.
%%  The messages sent have the following formats:
%%
%%	{file_request,From,ReplyAs,Request}
%%	{file_reply,ReplyAs,Reply}
%%	{io_reply,ReplyAs,Reply}

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.

request(Io, Request) ->
    Io ! {file_request,self(),Io,Request}.

file_reply(From, ReplyAs, Reply) ->
    From ! {file_reply,ReplyAs,Reply}.

wait_file_reply(From) ->
    receive
	{file_reply,From,Reply} ->
	    Reply;
	{'EXIT',From,Reason} ->			%In case we are trapping exits
	    {error, terminated}
    end.
