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
	 uc_dir/0,
	 priv_dir/1,
	 stick_dir/1,
	 unstick_dir/1,
	 get_object_code/1,
	 add_path/1,
	 add_pathsz/1,
	 add_paths/1,
	 add_pathsa/1,
	 add_patha/1,
	 add_pathz/1,
	 del_path/1,
	 replace_path/2,
	 add_uc/1,
	 start/0, start/1,
	 start_link/0, start_link/1,
	 which/1,
	 clash/0,
	 rel_loaded_p/1]).

%% Following functions are exported for the interpreter
-export([interpret/1,
	 interpret_binary/3,
	 delete_interpret/1,
	 interpreted/0,
	 interpreted/1]).


-include_lib("kernel/include/file.hrl").

%% moddb in state contains:
%%     {Module,FileName}      - Loaded module from FileName
%%     {Module,FileName,true} - Loaded module from FileName, relative current
%%                              directory at loading time.
%%     {{sticky,Module},true} - A sticky module.


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
%% load_binary(Mod,File,Bin)    -> {error,What} | {module,Mod}
%% ensure_loaded(Module)	-> {error,What} | {module, Mod} |
%%                                 {interpret, Mod}
%% delete(Module)
%% purge(Module)  kills all procs running old code
%% soft_purge(Module)           -> true | false
%% is_loaded(Module)		-> {file, File} | false
%% all_loaded()			-> {Module, File}*
%% add_uc(Name)			-> true
%% get_object_code(Mod)         -> error | {Mod, Bin, Filename}
%% stop()			-> true
%% root_dir()                   
%% compiler_dir()
%% uc_dir()
%% lib_dir()
%% priv_dir(Name)
%% stick_dir(Dir)               -> ok | {error ,Reason}
%% which(Module)                -> Filename
%% clash() ->                   -> print out


%% User interface

objfile_extension() ->
    code_aux:objfile_extension().



load_file(Mod)     ->  call({load_file,Mod}).
ensure_loaded(Mod) ->  call({ensure_loaded,Mod}).
load_abs(File)     ->  call({load_abs,File}).
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
uc_dir()           ->  call({dir,uc_dir}).
priv_dir(Name)     ->  call({dir,{priv_dir,Name}}).
stick_dir(Dir)     ->  call({stick_dir,Dir}).
unstick_dir(Dir)   ->  call({unstick_dir,Dir}).
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
rel_loaded_p(Mod)  -> call({rel_loaded_p,Mod}).
interpreted()      ->  call({interpreted}).

%% interpret/1 - 
%% 

interpret(Module)  ->  call({interpret,Module}).

interpreted(Module)->  call({interpreted,Module}).
delete_interpret(Module) ->  call({delete_int,Module}).

%% interpret_binary/3 - Adds a "compiled" binary to the interpreter database
%%
%% Returns:
%%   ok
%%   {{module, Module}, List}
%%   {error, sticky_directory} 
%%   {error, no_interpreter}
%% 

interpret_binary(Mod,File,Bin) ->
    call({interpret_binary,Mod,File,Bin}).

call(Req) ->
    gen_server:call(code_server,Req,infinity).

add_uc(Name) ->
    add_pathz(filename:join(uc_dir(),code_aux:to_list(Name))).

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
    fixtable_server:module_info(module),
    code_server:module_info(module),
    code_server_int:module_info(module),
    code_aux:module_info(module),
    string:module_info(module),

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
    case lists:member(embedded,Flags) of
	true ->
	    embedded;
	_Otherwise -> 
	    case init:get_argument(mode) of
		{ok,[["embedded"]]} ->
		    embedded;
		_Else ->
		    interactive
	    end
    end.

%% Find out which version of a particular module we would
%% load if we tried to load it, unless it's allready loaded.    
%% In that case return the name of the file which contains
%% the loaded object code

which(Module) when atom (Module) ->
    case is_loaded(Module) of
        false ->
            which2(Module);
        {file, File} ->
            File
    end.

which2(Module) ->
    Ext = objfile_extension(),
    File = lists:concat([Module, Ext]),
    Path = get_path(),
    which(File,Path).

which(_,[]) ->
    non_existing;

which(Module,[Directory|Tail]) ->
    {ok,Files} = file:list_dir(Directory),
    case lists:member(Module,Files) of
	true ->
	    filename:append(Directory, Module);
	false ->
	    which(Module,Tail)
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

filter(Ext, Dir, {error,_}) ->     
    io:format("** Bad path can't read ~s~n", [Dir]), [];
filter(Ext, _, {ok,Files}) -> 
    filter2(Ext, length(Ext), Files).

filter2(Ext,Extlen, []) -> [];
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



