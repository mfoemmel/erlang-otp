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
%%     $Id $
%%

-module(boot_code_loader).

%% -compile(export_all).

-export([startMeUp/0, startMeUp/1, libDir/0, prim_loaded/1,
	 include_file/2, ensure_loaded/1, go/1]).

-import(lists, [member/2]).

%% The EARS directory is found as follows
%% 1) The current directory
%% 2) The environment variable $ERLANG_EARS
%% 3) The fixed directory /usr/local/lib/erlsae/lib

%% The *first* directory which contains the file
%% erlang.ear will be assumed to be the lib
%% directory


default_dir() ->
    "/usr/local/lib/erlsae/lib".
    
startMeUp() ->
    startMeUp(libDir()).

libDir() ->
    %% erlang:display({checking,"."}),
    case boot_fprim:read_file_info("erlang.ear") of
	{ok, _} ->  ".";
	_       ->  libDir1()
    end.

libDir1() ->
    case os:getenv("ERLANG_EARS") of
	false -> libDir2();
	Dir0  ->
	    Dir = expand_env(Dir0),
	    %% erlang:display({checking, Dir}),
	    case boot_fprim:read_file_info(Dir ++ "/erlang.ear") of
		{ok, _} -> Dir;
		_       -> libDir2()
	    end
    end.

libDir2() ->
    D1 = default_dir(),
    %% erlang:display({checking, D1}),
    case boot_fprim:read_file_info(D1 ++ "/erlang.ear") of
	{ok, _} -> D1;
	_       -> erlang:display(fatal_error_cannot_locate_code_dir),
		   halt(1)
    end.

startMeUp(Dir) ->
    register(code_server, spawn(?MODULE, go, [Dir])).

ensure_loaded(Mod) ->
    code_server ! {ensure_loaded, self(), Mod},
    receive
	{code_server, Result} ->
	    Result
    end.

prim_loaded(Mods) ->
    code_server ! {prim_loaded, self(), Mods},
    receive
	{code_server, Result} ->
	    Result
    end.

include_file(Lib, Name) ->
    code_server ! {include_file, self(), Lib, Name},
    receive
	{code_server, Result} ->
	    Result
    end.
		     
go(Dir) ->
    case boot_fprim:list_dir(Dir) of
	{ok, Files} ->
	    Desc = open_libs(Files, Dir, []),
	    %% erlang:display({desc,Desc}),
	    loop(Desc, []);
	_ ->
	    exit(library)
    end.

loop(Desc, Loaded) ->
    receive
	{ensure_loaded, From, Mod} ->
	    %% erlang:display({boot_code_loader,ensure_loaded,Mod}),
	    case member(Mod, Loaded) of
		true ->
		    %Err = {module, Mod},
		    %erlang:display(Err),
		    From ! {code_server, {module, Mod}},
		    loop(Desc, Loaded);
		false ->
		    Result = load_mod(Mod, Desc),
		    %% erlang:display({boot_code_loader_loop_result,Result}),
		    From ! {code_server, Result},
		    loop(Desc, [Mod|Loaded])
	    end;
	{include_file, From, Lib, Name} ->
	    %% erlang:display({boot_code_loader, include_file,Lib,Name}),
	    From ! {code_server, inc_file(Lib, Name, Desc)},
	    loop(Desc, Loaded);
	{prim_loaded, From, Mods} ->
	    %% tell the server these were prim loaded
	    From ! {code_server, ack},
	    loop(Desc, Mods ++ Loaded);
	{'$gen_call', {From, Tag}, get_path} ->
	    {ok, Cwd} = file:get_cwd(),
	    From ! {Tag, Cwd},
	    loop(Desc, Loaded);
	Other ->
	    erlang:display({boot_code_loader,opps1, Other})
    end.

inc_file(_, Name, [{P,Lib,Keys}|_]) ->
    %% erlang:display({boot_code_loader,checking,Lib,Name,Keys}),
    case member({include,Name}, Keys) of
	true ->
	    boot_pds:fetch(P, {include,Name});
	false ->
	    error
    end;
inc_file(Lib, Name, [_|T]) ->
    inc_file(Lib, Name, T);
inc_file(_, _, []) ->
    error.
	    
load_mod(Mod, [{P,_,Keys}|T]) ->
    case member({mod,Mod}, Keys) of
	true ->
	    {ok, Bin} = boot_pds:fetch(P, {mod,Mod}),
	    case erlang:load_module(Mod, Bin) of
		M = {module,Mod} ->
		    M;
		Other ->
		    {error, {cannotLoadModule, Mod, Other}}
	    end;
	false ->
	    load_mod(Mod, T)
    end;            
load_mod(Mod, []) ->
    {error, {cannotFindmodule,Mod}}.

open_libs([H|T], Dir, L) ->
    case reverse(H) of
	"rae." ++ Rest ->
	    Lib = Dir ++ "/" ++ H,
	    %% erlang:display({reading, Lib}),
	    P = boot_pds:open(Lib, read),
	    open_libs(T, Dir, [{P,reverse(Rest),boot_pds:keys(P)}|L]);
	_ ->
	    open_libs(T, Dir, L)
    end;
open_libs([], _, L) ->
    L.

expand_env(Str) ->
    Cset = seq($a, $z, seq($A, $Z, seq($0, $9, "_"))),
    expand_env(Str, Cset).

expand_env([$$|Cs], Cset) ->
    subst_var(Cs, Cset, []);
expand_env([C|Cs], Cset) ->
    [C|expand_env(Cs, Cset)];
expand_env([], _) -> [].

subst_var([C|Cs0]=Cs, Cset, Acc) ->
    case member(C, Cset) of
	true ->
	    subst_var(Cs0, Cset, [C|Acc]);
	false ->
	    Name = reverse(Acc),
	    case os:getenv(Name) of
		false ->
		    [$$|Name ++ expand_env(Cs, Cset)];
		Val ->
		    Val ++ expand_env(Cs, Cset)
	    end
    end;
subst_var([], _, Acc) ->
    Name = reverse(Acc),
    case os:getenv(Name) of
	false -> [$$|Name];
	Val -> Val
    end.

seq(Min, Min, L) -> [Min|L];
seq(Min, Max, L) -> seq(Min, Max-1, [Max|L]).

reverse(X) -> lists:reverse(X, []).
