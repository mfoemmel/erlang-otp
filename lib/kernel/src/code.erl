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
-module(code).

%% This is the interface module to the code server. It also contains
%% some implementation details.  See also related modules: code_*.erl
%% in this directory.

-export([objfile_extension/0, 
	 set_path/1, 
	 get_path/0, 
	 load_file/1,
	 ensure_loaded/1,
	 load_abs/1,
	 load_abs/2,
	 load_binary/3,
	 delete/1,
	 purge/1,
	 soft_purge/1,
	 is_loaded/1,
	 all_loaded/0,
	 stop/0,
	 root_dir/0,
	 lib_dir/0,
	 lib_dir/1,
	 compiler_dir/0,
	 priv_dir/1,
	 stick_dir/1,
	 unstick_dir/1,
	 stick_mod/1,
	 unstick_mod/1,
	 is_sticky/1,
	 get_object_code/1,
	 add_path/1,
	 add_pathsz/1,
	 add_paths/1,
	 add_pathsa/1,
	 add_patha/1,
	 add_pathz/1,
	 del_path/1,
	 replace_path/2,
	 rehash/0,
	 start/0, start/1,
	 start_link/0, start_link/1,
	 which/1,
	 clash/0]).

-include_lib("kernel/include/file.hrl").

%% User interface.
%%
%% objfile_extension()		-> ".jam" | ".vee" | ...
%% set_path(Dir*)		-> true
%% get_path()			-> Dir*
%% add_path(Dir)		-> true | {error, What}
%% add_patha(Dir)		-> true | {error, What}
%% add_pathz(Dir)		-> true | {error, What}
%% add_paths(DirList)           -> true | {error, What}
%% add_pathsa(DirList)          -> true | {error, What}
%% add_pathsz(DirList)          -> true | {error, What}
%% del_path(Dir)		-> true | {error, What}
%% replace_path(Name,Dir)       -> true | {error, What}
%% load_file(File)		-> {error,What} | {module, Mod}
%% load_abs(File)		-> {error,What} | {module, Mod}
%% load_abs(File,Mod)		-> {error,What} | {module, Mod}
%% load_binary(Mod,File,Bin)    -> {error,What} | {module,Mod}
%% ensure_loaded(Module)	-> {error,What} | {module, Mod}
%% delete(Module)
%% purge(Module)  kills all procs running old code
%% soft_purge(Module)           -> true | false
%% is_loaded(Module)		-> {file, File} | false
%% all_loaded()			-> {Module, File}*
%% get_object_code(Mod)         -> error | {Mod, Bin, Filename}
%% stop()			-> true
%% root_dir()                   
%% compiler_dir()
%% lib_dir()
%% priv_dir(Name)
%% stick_dir(Dir)               -> ok | {error ,Reason}
%% is_sticky(Module)           -> true | false
%% which(Module)                -> Filename
%% clash() ->                   -> print out


%% User interface

objfile_extension() ->
    code_aux:objfile_extension().

load_file(Mod)     ->  call({load_file,Mod}).
ensure_loaded(Mod) ->  call({ensure_loaded,Mod}).
load_abs(File)     ->  call({load_abs,File,[]}).
load_abs(File,M)   ->  call({load_abs,File,M}).
load_binary(Mod,File,Bin) -> call({load_binary,Mod,File,Bin}).
delete(Mod)        ->  call({delete,Mod}).
purge(Mod)         ->  call({purge,Mod}).
soft_purge(Mod)    ->  call({soft_purge,Mod}).
is_loaded(Mod)     ->  call({is_loaded,Mod}).
get_object_code(M) ->  call( {get_object_code, M}).
all_loaded()       ->  call(all_loaded).
stop()             ->  call(stop).
root_dir()         ->  call({dir,root_dir}).
lib_dir()          ->  call({dir,lib_dir}).
lib_dir(Name)      ->  call({dir,{lib_dir,Name}}).
compiler_dir()     ->  call({dir,compiler_dir}).
priv_dir(Name)     ->  call({dir,{priv_dir,Name}}).
stick_dir(Dir)     ->  call({stick_dir,Dir}).
unstick_dir(Dir)   ->  call({unstick_dir,Dir}).
stick_mod(Dir)     ->  call({stick_mod,Dir}).
unstick_mod(Dir)   ->  call({unstick_mod,Dir}).
is_sticky(Mod)     ->  call({is_sticky,Mod}).
set_path(PathList) ->  call({set_path,PathList}).
get_path()         ->  call(get_path).
add_path(Dir)      ->  call({add_path,last,Dir}).
add_pathz(Dir)     ->  call({add_path,last,Dir}).
add_patha(Dir)     ->  call({add_path,first,Dir}).
add_paths(Dirs)    ->  call({add_paths,last,Dirs}).
add_pathsz(Dirs)   ->  call({add_paths,last,Dirs}).
add_pathsa(Dirs)   ->  call({add_paths,first,Dirs}).
del_path(Name)     ->  call({del_path,Name}).
replace_path(Name,Dir)   ->  call({replace_path,Name,Dir}).
rehash()          ->   call(rehash).
     

call(Req) ->
    gen_server:call(code_server,Req,infinity).

start() ->
    start([stick]).

start(Flags) ->
    do_start(start,Flags).

start_link() ->
    start_link([stick]).

start_link(Flags) ->
    do_start(start_link,Flags).
    
%%-----------------------------------------------------------------
%% In the init phase, code must not use any modules not yet loaded,
%% either pre_loaded (e.g. init) or first in the script (e.g.
%% erlang).  Therefore, keep the modules used in init phase to a
%% minimum, and make sure they are loaded before init is called.
%% Try to call these modules from do_start instead.
%% file is used in init - this is ok; file has been started before
%% us, so the module is loaded.
%%-----------------------------------------------------------------
do_start(F,Flags) ->    

    %% The following module_info/1 calls are here to ensure
    %% that the modules are loaded prior to their use elsewhere in 
    %% the code_server.
    %% Otherwise a dead-lock may occur when the code_server is starting.

    ets:module_info(module),
    code_server:module_info(module),
    code_aux:module_info(module),
    packages:module_info(module),
    string:module_info(module),
    file:module_info(module),
    lists_sort:module_info(module),
    catch load_hipe_modules(),

    Mode = get_mode(Flags),
    case init:get_argument(root) of 
	{ok,[[Root0]]} ->
	    Root = filename:join([Root0]), % Normalize.  Use filename
	    case gen_server:F({local,code_server},code_server,[Root,Mode],[]) of
		{ok,Pid} ->
		    case Mode of
			interactive ->
			    case lists:member(stick,Flags) of
				true -> do_stick_dirs();
				_    -> ok
			    end;
			_ ->
			    ok
		    end,
		    {ok,Pid};
		Other ->
		    Other
	    end;
	Other ->
	    error_logger:error_msg("Can not start code server ~w ~n",[Other]),
	    {error, crash}
    end.

load_hipe_modules() ->
    hipe_unified_loader:module_info(module),
    case erlang:system_info(hipe_architecture) of
	ultrasparc -> hipe_sparc_loader:module_info(module);
	x86 -> hipe_x86_loader:module_info(module);
	undefined -> ok
    end.

do_stick_dirs() ->
    do_s("compiler"),
    do_s("stdlib"),
    do_s("kernel").

do_s(Lib) ->
    case lib_dir(Lib) of
	{error, _} ->
	    false;
	Dir ->
	    stick_dir(filename:append(Dir, "ebin"))
    end.

get_mode(Flags) ->
    case lists:member(embedded, Flags) of
	true -> embedded;
	_Otherwise -> 
	    case init:get_argument(mode) of
		{ok,[["embedded"]]} ->
		    embedded;
		_Else ->
		    interactive
	    end
    end.

%% Find out which version of a particular module we would
%% load if we tried to load it, unless it's already loaded.
%% In that case return the name of the file which contains
%% the loaded object code

which(Module) when atom(Module) ->
    case is_loaded(Module) of
	false ->
	    which2(Module);
	{file, File} ->
	    File
    end.

which2(Module) ->
    Base = code_aux:to_path(Module),
    File = filename:basename(Base) ++ objfile_extension(),
    Path = get_path(),
    which(File, filename:dirname(Base), Path).

which(_,_,[]) ->
    non_existing;
which(File,Base,[Directory|Tail]) ->
    Path = if Base == "." -> Directory;
	      true -> filename:join(Directory, Base)
	   end,
    case file:list_dir(Path) of
	{ok,Files} ->
	    case lists:member(File,Files) of
		true ->
		    filename:append(Path, File);
		false ->
		    which(File,Base,Tail)
	    end;
	_ ->
	    which(File,Base,Tail)
    end.


%% Search the entire path system looking for name clashes

clash() ->
    Path = get_path(),
    Struct = lists:flatten(build(Path)),
    Len = length(search(Struct)),
    io:format("** Found ~w name clashes in code paths ~n",[Len]).

search([]) -> [];
search([{Dir,File} | Tail]) ->
    case lists:keysearch(File,2,Tail) of
	false -> 
	    search(Tail);
	{value,{Dir2,File}} ->
	    io:format("** ~s hides ~s~n",
		      [filename:join(Dir,File),
		       filename:join(Dir2,File)]),
	    [clash | search(Tail)]
    end.

build([]) -> [];
build([Dir|Tail]) ->
    Files = filter(objfile_extension(), Dir, file:list_dir(Dir)),
    [decorate(Files, Dir) | build(Tail)].

decorate([], _) -> [];
decorate([File|Tail], Dir) ->
    [{Dir, File} | decorate(Tail, Dir)].

filter(_Ext, Dir, {error,_}) ->     
    io:format("** Bad path can't read ~s~n", [Dir]), [];
filter(Ext, _, {ok,Files}) -> 
    filter2(Ext, length(Ext), Files).

filter2(_Ext, _Extlen, []) -> [];
filter2(Ext, Extlen,[File|Tail]) ->
    case has_ext(Ext,Extlen, File) of 
	true -> [File | filter2(Ext, Extlen, Tail)];
	false -> filter2(Ext, Extlen, Tail)
    end.

has_ext(Ext, Extlen,File) ->
    L = length(File),
    case catch lists:nthtail(L - Extlen, File) of
	Ext -> true;
	_ -> false
    end.
