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

-module(ear).

-compile(export_all).

-export([start/1]).
-import(lists, [foldl/3, foreach/2, map/2, member/2]).

%% ear -a Archive Mod Mod Dir
%% ear -l Archive
%% ear -d Archive Mod Mod
%% ear -e Archive 

%% Note ear *cannot* be used to build the origonal
%%   erlang.ear (since erlang.ear) is used for the code base for
%%   ear :-)
%%   Erlang.ear is build inside boot_tools.erl

%% examples
%%   ear -a foo.ear /ldisk/otp_src_P9_2002-05-26/lib/kernel/ebin/*.beam
%%   ear -l 

%% we want all these commands to work like a transaction
%% their every works or nothing.
%% This means we do all checking *before* manipulating the
%% archive

start([_|X]) ->
    X0 = map(fun(I) -> binary_to_list(I) end, X),
    %% io:format("EAR:~p~n",[X0]),
    [_|Args] = X0,
    ear(Args),
    erlang:halt().

ear(["-l", Archive]) ->
    is_archive(Archive),
    Pid = boot_pds:open(Archive, read),
    Keys = lists:sort(boot_pds:keys(Pid)),
    foreach(fun(I) -> io:format("~s~n", [print_name(I)]) end, Keys),
    boot_pds:close(Pid);
ear(["-a", Archive|Objs]) ->
    is_archive(Archive),
    Updates = foldl(fun add_object/2, [], Objs),
    Pid = boot_pds:open(Archive, read_write),
    map(fun({add,Key,Val}) ->
		boot_pds:store(Pid, Key, Val)
	end, Updates),
    boot_pds:close(Pid);
ear(["-r", Archive|Objs]) ->
    is_archive(Archive),
    Delete = map(fun get_obj_name/1, Objs),
    %% io:format("delete=~p~n",[Delete]),
    Pid = boot_pds:open(Archive, read_write),
    Keys = boot_pds:keys(Pid),
    foreach(fun(I) ->
		    case member(I, Keys) of
			true ->
			    true;
			false ->
			    boot_pds:close(Pid),
			    io:format("*** archive does not have element:~s~n",
				      [print_name(I)]),
			    erlang:halt()
		    end
	    end, Delete),
    foreach(fun(I) -> boot_pds:delete(Pid, I) end, Delete),
    boot_pds:close(Pid);
ear(["-e", Archive|Objs]) ->
    is_archive(Archive),
    Extract = map(fun get_obj_name/1, Objs),
    %% io:format("extract=~p~n",[Extract]),
    Pid = boot_pds:open(Archive, read_write),
    Keys = boot_pds:keys(Pid),
    %% check the archive has the required element
    foreach(fun(I) ->
		    case member(I, Keys) of
			true ->
			    true;
			false ->
			    boot_pds:close(Pid),
			    io:format("*** archive does not have element:~s~n",
				      [print_name(I)]),
			    erlang:halt()
		    end
	    end, Extract),
    %% check we won't clobber the file
    foreach(fun(I) ->
		    F = print_name(I),
		    case exists(F) of
			true ->
			    boot_pds:close(Pid),
			    io:format("*** ~s exists and will not be "
				      " overwritten~n",
				      [F]),
			    erlang:halt();
			false ->
			    true
		    end
	    end, Extract),
    %% Finally extract the files
    foreach(fun(I) ->
		    F = print_name(I),
		    {ok, Bin} = boot_pds:fetch(Pid, I),
		    file:write_file(F, Bin)
	    end, Extract),
    boot_pds:close(Pid);
ear(["-h"]) ->
    usage();
ear(_) ->
    usage().
    
exists(File) ->
     case boot_fprim:read_file_info(File) of
	 {ok, _} -> true;
	 _       -> false
     end.

is_archive(F) ->
     case filename:extension(F) of
	".ear" ->
	    true;
	 _ ->
	     io:format("*** ~s is not an archive (.ear file)~n", [F]),
	     erlang:halt()
     end.

print_name({mod, I})    -> atom_to_list(I) ++ ".beam";
print_name({include,F}) -> F ++ ".hrl".

get_obj_name(Obj) ->
    case filename:extension(Obj) of
	".beam" ->
	    F1 = filename:rootname(filename:basename(Obj)),
	    A = list_to_atom(F1),
	    {mod,A};
	".hrl" ->
	    F1 = filename:rootname(filename:basename(Obj)),
	    {include, F1};
	_ ->
	    io:format("*** ~s not .beam or .hrl~n", [Obj]),
	    erlang:halt()
    end.

add_object(Obj, L) ->
    case filename:extension(Obj) of
	".beam" ->
	    case get_module_name(Obj) of
		{ok, Mod} ->
		    {ok, Bin} = file:read_file(Obj),
		    [{add, {mod,Mod}, Bin}|L];
		error -> 
		    io:format("** bad module :~s~n", [Obj]),
		    erlang:halt()
	    end;
	".hrl" ->
	    F1 = filename:rootname(filename:basename(Obj)),
	    case file:read_file(Obj) of
		{ok, Bin} ->
		    [{add, {include, F1}, Bin}|L];
		_ ->
		    io:format("** bad include file :~s~n", [Obj]),
		    erlang:halt()
	    end;
	_ ->
	    io:format("** bad extension :~s~n", [Obj]),
	    erlang:halt()
    end.

get_module_name(F) ->
    case beam_lib:info(F) of
	L when list(L) ->
	    case [M || {module, M} <- L] of
		[Mod] ->
		    {ok, Mod};
		_  ->
		    error
	    end;
	_ ->
	    error
    end.

usage() ->
    io:format("Usage: ear [-l |-a |-r |-e] archive files\n"
	      "       ear -h\n"
	      "commands:\n"
	      "  h    - help\n"
	      "  l    - list archive\n"
	      "  a    - add file(s) to archive\n"
	      "  r    - remove file(s) from archive\n"
	      "  e    - extract file(s) from archive\n"
	      "notes:\n"
	      "  archive  files must have the extension .ear\n"
	      "  archived files may have extensions .beam or .hrl\n"
	      "examples:\n"
	      "  > ear -a myLib.ear *.beam my_include.hrl\n"
	      "  > ear -a myLib.ear PathToLib/ebin/*.beam\n"
	      "  > ear -a myLib.ear PathToLib/include/*.hrl\n").
