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

%% Interface module for the file server and the file io servers.



%%% External exports

-export([format_error/1]).
%% File system and metadata.
-export([get_cwd/0, get_cwd/1, set_cwd/1, delete/1, rename/2,
	 make_dir/1, del_dir/1, list_dir/1,
	 read_file_info/1, write_file_info/2,
	 read_link_info/1, read_link/1,
	 make_link/2, make_symlink/2,
	 read_file/1, write_file/2, write_file/3]).
%% Specialized
-export([ipread_s32bu_p32bu/3]).
%% Generic file contents.
-export([open/2, close/1, 
	 read/2, write/2, 
	 pread/2, pread/3, pwrite/2, pwrite/3,
	 position/2, truncate/1, sync/1,
	 copy/2, copy/3]).
%% High level operations
-export([consult/1, path_consult/2]).
-export([eval/1, eval/2, path_eval/2, path_eval/3, path_open/3]).
-export([script/1, script/2, path_script/2, path_script/3]).
-export([change_owner/2, change_owner/3, change_group/2,
	 change_mode/2, change_time/2, change_time/3]).

-export([pid2name/1]).

%%% Obsolete exported functions

-export([file_info/1, raw_read_file_info/1, raw_write_file_info/2]).
-export([rawopen/2]).

-deprecated([{file_info,1}]).

%% Internal export to prim_file and ram_file until they implement
%% an efficient copy themselves.
-export([copy_opened/3]).

-export([ipread_s32bu_p32bu_int/3]).


%%% Includes and defines
-include("file.hrl").

-define(FILE_IO_SERVER_TABLE, file_io_servers).

-define(OLD_FILE_SERVER, file_server). % Registered name
-define(FILE_SERVER, file_server_2).   % Registered name
-define(PRIM_FILE, prim_file).         % Module
-define(RAM_FILE, ram_file).           % Module


%%%-----------------------------------------------------------------
%%% === Concerning backwards compatibility between R8 and older nodes
%%%
%%% The R7 file server protocol and the R8 file server protocol 
%%% differs in three different details:
%%% * The mode specification for an 'open' request is an integer
%%%   (flag bits) in R7, and a list of atoms in R8. This was changed
%%%   because the flag bits were considered too inflexible.
%%% * The 'get_cwd' request useful for Windows systems in R7 had an 
%%%   integer drive number as argument, but in R8 it is supposed to 
%%%   be a string starting with "D:". This is because the drive
%%%   number was regarded to low-level.
%%% * A 'copy' request has been added in R8.
%%%
%%% The backwards compatibility problem is solved as follows:
%%%
%%% An R7 node registers the file server under the name 'file_server',
%%% the code for the server is find in module 'file'.
%%%
%%% An R8 node registers the file server under the name 'file_server_2',
%%% executed by module 'file_server'. It also registers a relay
%%% process named 'file_server' executed by module 'old_file_server'
%%% that relays all messages to the name 'file_server_2'.
%%%
%%% If the R8 node is a slave node, module 'file_server' will try to 
%%% locate the name 'file_server_2' on the master node. If it exists
%%% a relay process will register the name 'file_server_2' to relay
%%% all messages to the master 'file_server_2'. If it does not exist
%%% a stupid nameless process is created just to keep
%%% the kernel supervisor happy.
%%%
%%% Module 'old_file_server' will do roughly the same with the name
%%% 'file_server' towards the master node, with the exception that
%%% it will first look for the name 'file_server_2' on the master
%%% node, and then for 'file_server', and if neither name does exist, 
%%% the kernel will crash during init.
%%%
%%% When the R8 node is not a slave node, the module 'file_server' 
%%% is itself backwards compatible in that respect that it 
%%% understands the R7 requests and replies correctly.
%%%
%%% Calls to module 'file' when an R8 node is a slave to an R7 master
%%% will be translated when necessary by doing a local name lookup
%%% with erlang:whereis/1 for the name 'file_server_2'. If that name
%%% does not exist, the request will be in R7 format and sent to
%%% the name 'file_server' (locally, as usual), otherwise an R8 
%%% request is sent to 'file_server_2'.
%%%
%%% In the pictures below, nota that it is not common in practice
%%% for a slave to have slaves. It is just to show that it is
%%% theoretically possible.
%%%
%%%-----------------------------------------------------------------
%%% R8 Master with R8 and R7 slaves:
%%%                                                                 
%%% +============================================+                  
%%% | R7 Slave                                   |                  
%%% |                                            |                  
%%% | +-------+                  +-----------+   |                  
%%% | |<X.X.X>|----------------->|file_server|   |                  
%%% | +-------+                  +-----------+   |                  
%%% |                                  |         |                  
%%% +==================================|=========+                  
%%%                                    |                            
%%%                                    |                            
%%% +==================================|=========+                  
%%% | R8 Slave                         |         |                  
%%% |                                  |         |                  
%%% | +-------+                        |         |                  
%%% | |<X.X.X>|                        |         |                  
%%% | +-------+                        |         |                  
%%% |        |                         |         |                  
%%% |        V                         V         |                  
%%% |       +-------------+      +-----------+   |                  
%%% |       |file_server_2|      |file_server|   |                  
%%% |       +-------------+      +-----------+   |                  
%%% |              |                   |         |                  
%%% |              |    +--------------+         |                  
%%% |              |    |                        |                  
%%% +==============|====|========================+                  
%%%                |    |                                           
%%%                |    |                                           
%%% +==============|====|========================+                  
%%% | R8 Master    |    |                        |                  
%%% |              V    V                        |                  
%%% |       +-------------+      +-----------+   |                  
%%% |       |file_server_2|<-----|file_server|   |                  
%%% |       +-------------+      +-----------+   |                  
%%% |        A                         A         |                  
%%% |        |                         |         |                  
%%% | +-------+                        |         |                  
%%% | |<X.X.X>|                        |         |                  
%%% | +-------+                        |         |                  
%%% |                                  |         |                  
%%% +==================================|=========+                  
%%%                                    |                            
%%%                                    |                            
%%% +==================================|=========+                  
%%% | R7 Slave                         |         |                  
%%% |                                  |         |                  
%%% | +-------+                  +-----------+   |                  
%%% | |<X.X.X>|----------------->|file_server|   |                  
%%% | +-------+                  +-----------+   |                  
%%% |                                            |                  
%%% +============================================+                  
%%%                                                                 
%%%-----------------------------------------------------------------
%%% R7 Master with R7 and R8 slaves:
%%%                                                                 
%%% +============================================+                  
%%% | R7 Slave                                   |                  
%%% |                                            |                  
%%% | +-------+                  +-----------+   |                  
%%% | |<X.X.X>|----------------->|file_server|   |                  
%%% | +-------+                  +-----------+   |                  
%%% |                                  |         |                  
%%% +==================================|=========+                  
%%%                                    |                            
%%%                                    |                            
%%% +==================================|=========+                  
%%% | R8 Slave                         |         |                  
%%% |                                  |         |                  
%%% | +-------+                        |         |                  
%%% | |<X.X.X>|                        |         |                  
%%% | +-------+                        |         |                  
%%% |        |                         |         |                  
%%% |        |                         V         |                  
%%% |        |                   +-----------+   |                  
%%% |        +------------------>|file_server|   |                  
%%% |                            +-----------+   |                  
%%% |                                  |         |                  
%%% +==================================|=========+                  
%%%                                    |                            
%%%                                    |                            
%%% +==================================|=========+                  
%%% | R7 Master                        |         |                  
%%% |                                  V         |                  
%%% | +-------+                  +-----------+   |                  
%%% | |<X.X.X>|----------------->|file_server|   |                  
%%% | +-------+                  +-----------+   |                  
%%% |                                  A         |                  
%%% +==================================|=========+                  
%%%                                    |                            
%%%                                    |                            
%%% +==================================|=========+                  
%%% | R7 Slave                         |         |                  
%%% |                                  |         |                  
%%% | +-------+                  +-----------+   |                  
%%% | |<X.X.X>|----------------->|file_server|   |                  
%%% | +-------+                  +-----------+   |                  
%%% |                                            |                  
%%% +============================================+                  
%%%                                                                 
%%%-----------------------------------------------------------------


%% Mode bit mask values for R7 file server call {open, Name, Mode}
-define(OLD_MODE_READ,       (1 bsl 0)).
-define(OLD_MODE_WRITE,      (1 bsl 1)).
-define(OLD_MODE_APPEND,     (1 bsl 2)).
-define(OLD_MODE_COMPRESSED, (1 bsl 3)).
-define(OLD_MODE_BINARY,     (1 bsl 10)).

%%%-----------------------------------------------------------------
%%% General functions

format_error({_Line, ?MODULE, Reason}) ->
    io_lib:format("~w", [Reason]);
format_error({_Line, Mod, Reason}) ->
    Mod:format_error(Reason);
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

pid2name(Pid) when pid(Pid) ->
    case whereis(file_server) of
	undefined ->
	    undefined;
	_ ->
	    case ets:lookup(?FILE_IO_SERVER_TABLE, Pid) of
		[{_, Name} | _] ->
		    {ok, Name};
		_ ->
		    undefined
	    end
    end.

%%%-----------------------------------------------------------------
%%% File server functions.
%%% Functions that do not operate on a single open file.
%%% Stateless.

get_cwd() -> 
    case file_server() of
	?FILE_SERVER ->
	    call(?FILE_SERVER, get_cwd, []);
	?OLD_FILE_SERVER ->
	    call(?OLD_FILE_SERVER, get_cwd, [0])
    end.
get_cwd(Dirname) ->
    case file_server() of
	?FILE_SERVER ->
	    check_and_call(?FILE_SERVER, get_cwd, [file_name(Dirname)]);
	?OLD_FILE_SERVER ->
	    case Dirname of
		[D, $: | _] when 
		      integer(D), $A =< D, D =< $Z ->
		    call(?OLD_FILE_SERVER, get_cwd, [1+D-$A]);
		[D, $: | _] when 
		      integer(D), $a =< D, D =< $z ->
		    call(?OLD_FILE_SERVER, get_cwd, [1+D-$a]);
		_ ->
		    {error, einval}
	    end
    end.
set_cwd(Dirname) -> 
    check_and_call(set_cwd, [file_name(Dirname)]).
delete(Name) ->
    check_and_call(delete, [file_name(Name)]).
rename(From, To) ->
    check_and_call(rename, [file_name(From), file_name(To)]).
make_dir(Name) ->
    check_and_call(make_dir, [file_name(Name)]).
del_dir(Name) ->
    check_and_call(del_dir, [file_name(Name)]).
read_file_info(Name) ->
    check_and_call(read_file_info, [file_name(Name)]).
read_link_info(Name) ->
    check_and_call(read_link_info, [file_name(Name)]).
read_link(Name) ->
    check_and_call(read_link, [file_name(Name)]).
write_file_info(Name, Info) when record(Info, file_info) ->
    check_and_call(write_file_info, [file_name(Name), Info]).
list_dir(Name) ->
    check_and_call(list_dir, [file_name(Name)]).
read_file(Name) ->
    check_and_call(read_file, [file_name(Name)]).
make_link(Old, New) ->
    check_and_call(make_link, [file_name(Old), file_name(New)]).
make_symlink(Old, New) ->
    check_and_call(make_symlink, [file_name(Old), file_name(New)]).
write_file(Name, Bin) ->
    check_and_call(write_file, [file_name(Name), check_binary(Bin)]).

%% This whole operation should be moved to the file_server and prim_file
%% when it is time to change file server protocol again.
%% Meanwhile, it is implemented here, slihtly less efficient.
%%
write_file(Name, Bin, ModeList) when list(ModeList) ->
    case check_binary(Bin) of
	B when binary(B) ->
	    case open(Name, [binary, write | 
			     lists:delete(binary, 
					  lists:delete(write, ModeList))]) of
		{ok, Handle} ->
		    case write(Handle, B) of
			ok ->
			    close(Handle);
			E1 ->
			    close(Handle),
			    E1
		    end;
		E2 ->
		    E2
	    end;
	E3 ->
	    E3
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

%% Obsolete, undocumented, local node only, don't use!.
%% XXX to be removed.
raw_read_file_info(Name) ->
    Args = [file_name(Name)],
    case check_args(Args) of
	ok ->
	    [FileName] = Args,
	    ?PRIM_FILE:read_file_info(FileName);
	Error ->
	    Error
    end.

%% Obsolete, undocumented, local node only, don't use!.
%% XXX to be removed.
raw_write_file_info(Name, #file_info{} = Info) ->
    Args = [file_name(Name)],
    case check_args(Args) of
	ok ->
	    [FileName] = Args,
	    ?PRIM_FILE:write_file_info(FileName, Info);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% File io server functions.
%%% They operate on a single open file.
%%% Stateful.

%% Contemporary mode specification - list of options
open(Item, ModeList) when list(ModeList) ->
    case lists:member(raw, ModeList) of
	%% Raw file, use ?PRIM_FILE to handle this file
	true ->
	    %% check if raw file mode is disabled
	    case catch application:get_env(kernel, raw_files) of
		{ok,false} ->
		    open(Item, lists:delete(raw, ModeList));
		_ ->				% undefined | {ok,true}
		    Args = [file_name(Item) | ModeList],
		    case check_args(Args) of
			ok ->
			    [FileName | _] = Args,
			    %% We rely on the returned Handle (in {ok, Handle})
			    %% being a pid() or a #file_descriptor{}
			    ?PRIM_FILE:open(FileName, ModeList);
			Error ->
			    Error
		    end
	    end;
	false ->
	    case lists:member(ram, ModeList) of
		%% RAM file, use ?RAM_FILE to handle this file
		true ->
		    case check_args(ModeList) of
			ok ->
			    ?RAM_FILE:open(Item, ModeList);
			Error ->
			    Error
		    end;
		%% File server file
		false ->
		    case file_server() of
			?OLD_FILE_SERVER ->
			    Mode = old_mode(ModeList),
			    Args = [file_name(Item), Mode],
			    case check_args(Args) of
				ok ->
				    call(?OLD_FILE_SERVER, open, Args);
				Error ->
				    Error
			    end;
			?FILE_SERVER ->
			    Args = [file_name(Item) | ModeList],
			    case check_args(Args) of 
				ok ->
				    [FileName | _] = Args,
				    call(?FILE_SERVER, 
					 open, [FileName, ModeList]);
				Error ->
				    Error
			    end;
			Error ->
			    Error
		    end
	    end
    end;
%% Old obsolete mode specification in atom or 2-tuple format
open(Item, Mode) ->
    open(Item, mode_list(Mode)).

%% Obsolete, undocumented, local node only, don't use!.
%% XXX to be removed.
rawopen(Name, Mode) ->
    Args = [file_name(Name) | mode_list(Mode)],
    case check_args(Args) of
	ok ->
	    [FileName | ModeList] = Args,
	    %% We rely on the returned Handle (in {ok, Handle})
	    %% being a pid() or a #file_descriptor{}
	    ?PRIM_FILE:open(FileName, ModeList);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% The following interface functions operate on open files.
%%% The File argument must be either a Pid or a handle 
%%% returned from ?PRIM_FILE:open.

close(File) when pid(File) ->
    R = file_request(File, close),
    case wait_file_reply(File, R) of
	{error, terminated} ->
	    ok;
	Other ->
	    Other
    end;
%%    unlink(File),
%%    exit(File, close),
%%    ok;
close(#file_descriptor{module = Module} = Handle) ->
    Module:close(Handle);
close(_) ->
    {error, einval}.

read(File, Sz) when pid(File), integer(Sz), Sz >= 0 ->
    case io:get_chars(File, '', Sz) of
	Data when list(Data); binary(Data) ->
	    {ok, Data};
	Other ->
	    Other
    end;
read(#file_descriptor{module = Module} = Handle, Sz) 
  when integer(Sz), Sz >= 0 ->
    Module:read(Handle, Sz);
read(_, _) ->
    {error, einval}.



pread(File, L) when pid(File), list(L) ->
    pread_int(File, L, []);
pread(#file_descriptor{module = Module} = Handle, L) when list(L) ->
    Module:pread(Handle, L);
pread(_, _) ->
    {error, einval}.

pread_int(_File, [], R) ->
    {ok, lists:reverse(R)};
pread_int(File, [{At, Sz} | T], R) when integer(Sz), Sz >= 0 ->
    case pread(File, At, Sz) of
	{ok, Data} ->
	    pread_int(File, T, [Data | R]);
	eof ->
	    pread_int(File, T, [eof | R]);
	{error, _} = Error ->
	    Error
    end;
pread_int(_, _, _) ->
    {error, einval}.

pread(File, At, Sz) when pid(File), integer(Sz), Sz >= 0 ->
    R = file_request(File, {pread, At, Sz}),
    wait_file_reply(File, R);
%% No Whence supported, only absolute positions
pread(#file_descriptor{module = Module} = Handle, Offs, Sz) 
  when integer(Offs), Offs >= 0, integer(Sz), Sz >= 0 ->
    Module:pread(Handle, Offs, Sz);
pread(_, _, _) ->
    {error, einval}.



write(File, Bytes) when pid(File) ->
    io:put_chars(File, Bytes);
write(#file_descriptor{module = Module} = Handle, Bytes) ->
    Module:write(Handle, Bytes);
write(_, _) ->
    {error, einval}.



pwrite(File, L) when pid(File), list(L) ->
    pwrite_int(File, L, 0);
pwrite(#file_descriptor{module = Module} = Handle, L) when list(L) ->
    Module:pwrite(Handle, L);
pwrite(_, _) ->
    {error, einval}.

pwrite_int(_File, [], _R) ->
    ok;
pwrite_int(File, [{At, Bytes} | T], R) ->
    case pwrite(File, At, Bytes) of
	ok ->
	    pwrite_int(File, T, R+1);
	{error, Reason} ->
	    {error, {R, Reason}}
    end;
pwrite_int(_, _, _) ->
    {error, einval}.

pwrite(File, At, Bytes) when pid(File) ->
    R = file_request(File, {pwrite, At, Bytes}),
    wait_file_reply(File, R);
pwrite(#file_descriptor{module = Module} = Handle, Offs, Bytes) 
  when integer(Offs), Offs >= 0 ->
    Module:pwrite(Handle, Offs, Bytes);
pwrite(_, _, _) ->
    {error, einval}.



sync(File) when pid(File) ->
    R = file_request(File, sync),
    wait_file_reply(File, R);
sync(#file_descriptor{module = Module} = Handle) ->
    Module:sync(Handle);
sync(_) ->
    {error, einval}.

position(File, At) when pid(File) ->
    R = file_request(File, {position,At}),
    wait_file_reply(File, R);
position(#file_descriptor{module = Module} = Handle, At) ->
    Module:position(Handle, At);
position(_, _) ->
    {error, einval}.

truncate(File) when pid(File) ->
    R = file_request(File, truncate),
    wait_file_reply(File, R);
truncate(#file_descriptor{module = Module} = Handle) ->
    Module:truncate(Handle);
truncate(_) ->
    {error, einval}.



copy(Source, Dest) ->
    copy_int(file_server(), Source, Dest, infinity).

copy(Source, Dest, Length) 
  when integer(Length), Length >= 0;
       atom(Length) ->
    copy_int(file_server(), Source, Dest, Length);
copy(_, _, _) ->
    {error, einval}.

%% Here we know that Length is either an atom or an integer >= 0
%% (by the way, atoms > integers)
%%
%% Copy between open files. 
copy_int(_FileServer, Source, Dest, Length) 
  when pid(Source), pid(Dest);
       pid(Source), record(Dest, file_descriptor);
       record(Source, file_descriptor), pid(Dest) ->
    copy_opened_int(Source, Dest, Length, 0);
%% Copy between open raw files, both handled by the same module
copy_int(_FileServer, #file_descriptor{module = Module} = Source,
	 #file_descriptor{module = Module} = Dest,
     Length) ->
    Module:copy(Source, Dest, Length);
%% Copy between open raw files
copy_int(_FileServer, Source, Dest, Length) 
  when record(Source, file_descriptor), record(Dest, file_descriptor) ->
    copy_opened_int(Source, Dest, Length, 0);
%%
%% At least one of the files needs to be opened
%%
%% Copy between filenames, let the server do the copy
copy_int(?FILE_SERVER, {SourceName, SourceOpts}, {DestName, DestOpts}, Length) 
  when list(SourceOpts), list(DestOpts) ->
   check_and_call(?FILE_SERVER, copy, 
		   [file_name(SourceName), SourceOpts,
		    file_name(DestName), DestOpts,
		    Length]);
%% Must open Source
copy_int(FileServer, {SourceName, SourceOpts}, Dest, Length) 
  when list(SourceOpts) ->
    case file_name(SourceName) of
	{error, _} = Error ->
	    Error;
	Source ->
	    case open(Source, [read | SourceOpts]) of
		{ok, Handle} ->
		    Result = copy_int(FileServer, Handle, Dest, Length),
		    close(Handle),
		    Result;
		{error, _} = Error ->
		    Error
	    end
    end;
%% Must open Dest
copy_int(FileServer, Source, {DestName, DestOpts}, Length)
  when list(DestOpts) ->
    case file_name(DestName) of
	{error, _} = Error ->
	    Error;
	Dest ->
	    case open(Dest, [write | DestOpts]) of
		{ok, Handle} ->
		    Result = copy_int(FileServer, Source, Handle, Length),
		    close(Handle),
		    Result;
		{error, _} = Error ->
		    Error
	    end
    end;
%%
%% Neither of Source and Dest is a {Name, Opts} tuple, so they must
%% be open file handles or filenames
%%
%% Copy from open file to filename
copy_int(FileServer, Source, Dest, Length) 
  when pid(Source);
       record(Source, file_descriptor) ->
    copy_int(FileServer, Source, {Dest, []}, Length);
%% Copy from filename to open file
copy_int(FileServer, Source, Dest, Length) 
  when pid(Dest);
       record(Dest, file_descriptor) ->
    copy_int(FileServer, {Source, []}, Dest, Length);
%% Copy between filenames
copy_int(FileServer, Source, Dest, Length) ->
    copy_int(FileServer, {Source, []}, {Dest, []}, Length).



copy_opened(Source, Dest, Length)
  when integer(Length), Length >= 0;
       atom(Length) ->
    copy_opened_int(Source, Dest, Length);
copy_opened(_, _, _) ->
    {error, einval}.

%% Here we know that Length is either an atom or an integer >= 0
%% (by the way, atoms > integers)

copy_opened_int(Source, Dest, Length)
  when pid(Source), pid(Dest) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(Source, Dest, Length)
  when pid(Source), record(Dest, file_descriptor) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(Source, Dest, Length)
  when record(Source, file_descriptor), pid(Dest) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(Source, Dest, Length)
  when record(Source, file_descriptor), record(Dest, file_descriptor) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(_, _, _) ->
    {error, einval}.

%% Here we know that Source and Dest are handles to open files, Length is
%% as above, and Copied is an integer >= 0

%% Copy loop in client process
copy_opened_int(_, _, Length, Copied) when Length =< 0 -> % atom() > integer()
    {ok, Copied};
copy_opened_int(Source, Dest, Length, Copied) ->
    N = if Length > 65536 -> 65536; true -> Length end, % atom() > integer() !
    case read(Source, N) of
	{ok, Data} ->
	    M = if binary(Data) -> size(Data);
		   list(Data)   -> length(Data)
		end,
	    case write(Dest, Data) of
		ok ->
		    if M < N ->
			    %% Got less than asked for - must be end of file
			    {ok, Copied+M};
		       true ->
			    %% Decrement Length (might be an atom (infinity))
			    NewLength = if atom(Length) -> Length;
					   true         -> Length-M
					end,
			    copy_opened_int(Source, Dest, NewLength, Copied+M)
		    end;
		{error, _} = Error ->
		    Error
	    end;
	eof ->
	    {ok, Copied};
	{error, _} = Error ->
	    Error
    end.


%% Special indirect pread function. Introduced for Dets.
%% Reads a header from pos 'Pos', the header is first a size encoded as
%% 32 bit big endian unsigned and then a position also encoded as
%% 32 bit big endian. Finally it preads the data from that pos and size 
%% in the file.

ipread_s32bu_p32bu(File, Pos, MaxSize) when pid(File) ->
    ipread_s32bu_p32bu_int(File, Pos, MaxSize);
ipread_s32bu_p32bu(#file_descriptor{module = Module} = Handle, Pos, MaxSize) ->
    Module:ipread_s32bu_p32bu(Handle, Pos, MaxSize);
ipread_s32bu_p32bu(_, _, _) ->
    {error, einval}.

ipread_s32bu_p32bu_int(File, Pos, MaxSize) 
  when integer(MaxSize), MaxSize >= 0, MaxSize < (1 bsl 31) ->
    case pread(File, Pos, 8) of
	{ok, Header} ->
	    ipread_s32bu_p32bu_2(File, Header, MaxSize);
	Error ->
	    Error
    end;
ipread_s32bu_p32bu_int(File, Pos, infinity) ->
    ipread_s32bu_p32bu_int(File, Pos, (1 bsl 31)-1);
ipread_s32bu_p32bu_int(_File, _Pos, _MaxSize) ->
    {error, einval}.

ipread_s32bu_p32bu_2(_File, 
		     <<0:32/big-unsigned, Pos:32/big-unsigned>>,
		     _MaxSize) ->
    {ok, {0, Pos, eof}};
ipread_s32bu_p32bu_2(File, 
		     <<Size:32/big-unsigned, Pos:32/big-unsigned>>,
		     MaxSize) 
  when Size =< MaxSize ->
    case pread(File, Pos, Size) of
	{ok, Data} ->
	    {ok, {Size, Pos, Data}};
	eof ->
	    {ok, {Size, Pos, eof}};
	Error ->
	    Error
    end;
ipread_s32bu_p32bu_2(_File, 
		     <<_:8/binary>>,
		     _MaxSize) ->
    eof;
ipread_s32bu_p32bu_2(_File,
		     <<_/binary>>,
		     _MaxSize) ->
    eof;
ipread_s32bu_p32bu_2(File,
		    Header,
		    MaxSize) when list(Header) ->
    ipread_s32bu_p32bu_2(File, list_to_binary(Header), MaxSize).



%%%-----------------------------------------------------------------
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
	{ok, Fd, Full} ->
	    case consult_stream(Fd) of
		{ok, List} ->
		    close(Fd),
		    {ok, List, Full};
		E1 ->
		    close(Fd),
		    E1
	    end;
	E2 ->
	    E2
    end.

eval(File) ->
    eval(File, erl_eval:new_bindings()).

eval(File, Bs) ->
    case open(File, [read]) of
	{ok, Fd} ->
	    R = eval_stream(Fd, ignore, Bs),
	    close(Fd),
	    R;
	Error ->
	    Error
    end.

path_eval(Path, File) ->
    path_eval(Path, File, erl_eval:new_bindings()).

path_eval(Path, File, Bs) ->
    case path_open(Path, File, [read]) of
	{ok, Fd, Full} ->
	    case eval_stream(Fd, ignore, Bs) of
		ok ->
		    close(Fd),
		    {ok, Full};
		E1 ->
		    close(Fd),
		    E1
	    end;
	E2 ->
	    E2
    end.

script(File) ->
    script(File, erl_eval:new_bindings()).

script(File, Bs) ->
    case open(File, [read]) of
	{ok, Fd} ->
	    R = eval_stream(Fd, return, Bs),
	    close(Fd),
	    R;
	Error ->
	    Error
    end.

path_script(Path, File) ->
    path_script(Path, File, erl_eval:new_bindings()).

path_script(Path, File, Bs) ->
    case path_open(Path, File, [read]) of
	{ok,Fd,Full} ->
	    case eval_stream(Fd, return, Bs) of
		{ok,R} ->
		    close(Fd),
		    {ok, R, Full};
		E1 ->
		    close(Fd),
		    E1
	    end;
	E2 ->
	    E2
    end.
    

%% path_open(Paths, Filename, Mode) ->
%%	{ok,FileDescriptor,FullName}
%%	{error,Reason}
%%
%% Searches the Paths for file Filename which can be opened with Mode.
%% The path list is ignored if Filename contains an absolute path.

path_open(PathList, Name, Mode) ->
    case file_name(Name) of
	{error, Error} ->
	    {error, Error};
	FileName ->
	    case filename:pathtype(FileName) of
		relative ->
		    path_open_first(PathList, FileName, Mode, enoent);
		_ ->
		    case open(Name, Mode) of
			{ok, Fd} -> 
			    {ok, Fd, Name};
			Error -> 
			    Error
		    end
	    end
    end.

change_mode(Name, Mode) 
  when integer(Mode) ->
    write_file_info(Name, #file_info{mode=Mode}).

change_owner(Name, OwnerId) 
  when integer(OwnerId) ->
    write_file_info(Name, #file_info{uid=OwnerId}).

change_owner(Name, OwnerId, GroupId) 
  when integer(OwnerId), integer(GroupId) ->
    write_file_info(Name, #file_info{uid=OwnerId, gid=GroupId}).

change_group(Name, GroupId) 
  when integer(GroupId) ->
    write_file_info(Name, #file_info{gid=GroupId}).

change_time(Name, Time) 
  when tuple(Time) ->
    write_file_info(Name, #file_info{mtime=Time}).

change_time(Name, Atime, Mtime) 
  when tuple(Atime), tuple(Mtime) ->
    write_file_info(Name, #file_info{atime=Atime, mtime=Mtime}).

%%%-----------------------------------------------------------------
%%% Helpers

consult_stream(Fd) ->
    case catch consult_stream(io:read(Fd, ''), Fd) of
	{'EXIT', _Reason} ->
	    {error, einval};
	{error, Reason} ->
	    {error, Reason};
	List ->
	    {ok, List}
    end.

consult_stream({ok,Term}, Fd) ->
    [Term|consult_stream(io:read(Fd, ''), Fd)];
consult_stream({error,What}, _Fd) ->
    throw({error, What});
consult_stream(eof, _Fd) ->
    [].

eval_stream(Fd, Handling, Bs) ->
    eval_stream(Fd, Handling, undefined, [], Bs).

eval_stream(Fd, Handling, Last, E, Bs) ->
    eval_stream(io:parse_erl_exprs(Fd, ''), Fd, Handling, Last, E, Bs).


eval_stream({ok,Form,EndLine}, Fd, Handling, Last, E, Bs0) ->
    case catch erl_eval:exprs(Form, Bs0) of
	{value,V,Bs} ->
	    eval_stream(Fd, Handling, {V}, E, Bs);
	{'EXIT',Reason} ->
	    eval_stream(Fd, Handling, Last, [{EndLine,?MODULE,Reason}|E], Bs0)
    end;
eval_stream({error,What,_EndLine}, Fd, H, L, E, Bs) ->
    eval_stream(Fd, H, L, [What | E], Bs);
eval_stream({eof,_EndLine}, _Fd, H, Last, E, _Bs) ->
    case {H, Last, E} of
	{return, {Val}, []} ->
	    {ok, Val};
	{return, undefined, E} ->
	    {error, hd(lists:reverse(E, [{error, undefined_script}]))};
	{ignore, _, []} ->
	    ok;
	{_, _, [_|_] = E} ->
	    {error, hd(lists:reverse(E))}
    end.

path_open_first([Path|Rest], Name, Mode, LastError) ->
    case file_name(Path) of
	{error, Error} ->
	    {error, Error};
	FilePath ->
	    FileName = filename:join(FilePath, Name),
	    case open(FileName, Mode) of
		{ok, Fd} ->
		    {ok, Fd, FileName};
		{error, enoent} ->
		    path_open_first(Rest, Name, Mode, LastError);
		Error ->
		    Error
	    end
    end;
path_open_first([], _Name, _Mode, LastError) ->
    {error, LastError}.

%%%-----------------------------------------------------------------
%%% Utility functions.

%% file_name(FileName)
%% 	Generates a flat file name from a deep list of atoms and 
%% 	characters (integers).

file_name(N) ->
    catch file_name_1(N).

file_name_1([C|T]) when integer(C), C > 0, C =< 255 ->
    [C|file_name_1(T)];
file_name_1([H|T]) ->
    file_name_1(H) ++ file_name_1(T);
file_name_1([]) ->
    [];
file_name_1(N) when atom(N) ->
    atom_to_list(N);
file_name_1(_) ->
    throw({error, einval}).

check_binary(Bin) when binary(Bin) ->
    Bin;
check_binary(List) when list(List) ->
    %% Convert the list to a binary, 
    %% to avoid copying a list to the file server.
    case catch list_to_binary(List) of
	{'EXIT', _} ->
	    {error, einval};
	Bin ->
	    Bin
    end;
check_binary(_) ->
    {error, einval}.

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



%% Convert mode list to bit mask for old R7 file server

old_mode(List) when list(List) ->
    case old_mode(List, 0) of
	Mode when Mode band (?OLD_MODE_READ bor ?OLD_MODE_WRITE) == 0 ->
	    Mode bor ?OLD_MODE_READ;
	Other ->
	    Other
    end.

old_mode([read|Rest], Mode) ->
    old_mode(Rest, Mode bor ?OLD_MODE_READ);
old_mode([write|Rest], Mode) ->
    old_mode(Rest, Mode bor ?OLD_MODE_WRITE);
old_mode([binary|Rest], Mode) ->
    old_mode(Rest, Mode bor ?OLD_MODE_BINARY);
old_mode([compressed|Rest], Mode) ->
    old_mode(Rest, Mode bor ?OLD_MODE_COMPRESSED);
old_mode([append|Rest], Mode) ->
    old_mode(Rest, Mode bor ?OLD_MODE_APPEND bor ?OLD_MODE_WRITE);
old_mode([{delayed_write, _, _}|Rest], Mode) ->
    old_mode(Rest, Mode);
old_mode([delayed_write|Rest], Mode) ->
    old_mode(Rest, Mode);
old_mode([{read_ahead, _}|Rest], Mode) ->
    old_mode(Rest, Mode);
old_mode([read_ahead|Rest], Mode) ->
    old_mode(Rest, Mode);
old_mode([], Mode) ->
    Mode;
old_mode(_, _) ->
    {error, einval}.


%%-----------------------------------------------------------------
%% Functions for communicating with the file server

%% Selects between an old and a new file server
file_server() ->
    case whereis(?FILE_SERVER) of
	Pid when pid(Pid) ->
	    ?FILE_SERVER;
	undefined ->
	    ?OLD_FILE_SERVER
    end.

%% Currently not used
%% 
%% call(Command, Args) ->
%%     call(file_server(), Command, Args).

call(FileServer, Command, Args) when list(Args) ->
    gen_server:call(FileServer, list_to_tuple([Command | Args]), infinity).

check_and_call(Command, Args) ->
    check_and_call(file_server(), Command, Args).

check_and_call(FileServer, Command, Args) when list(Args) ->
    case check_args(Args) of
	ok ->
	    call(FileServer, Command, Args);
	Error ->
	    Error
    end.

check_args([{error, Error}|_Rest]) ->
    {error, Error};
check_args([_Name|Rest]) ->
    check_args(Rest);
check_args([]) ->
    ok.

%%-----------------------------------------------------------------
%% Functions for communicating with a file io server.
%% The messages sent have the following formats:
%%
%%	{file_request,From,ReplyAs,Request}
%%	{file_reply,ReplyAs,Reply}

file_request(Io, Request) ->
    R = erlang:monitor(process, Io),
    Io ! {file_request,self(),Io,Request},
    R.

wait_file_reply(From, Ref) ->
    receive
	{file_reply,From,Reply} ->
	    erlang:demonitor(Ref),
	    receive {'DOWN', Ref, _, _, _} -> ok after 0 -> ok end,
	    %% receive {'EXIT', From, _} -> ok after 0 -> ok end,
	    Reply;
	{'DOWN', Ref, _, _, _} ->
	    %% receive {'EXIT', From, _} -> ok after 0 -> ok end,
	    {error, terminated}
    end.
