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

-module(boot_tools).

-export([make_tools/0,
	 start/1,
	 print_args/1,
	 pack_beams/1,
	 make_stub/0]).

-import(lists, [append/1, filter/2, foldl/3, 
		foreach/2, map/2, member/2, reverse/1]).

%% running make_sae()
%%   Creates the basic system
%%   ecc elink and erlang.ear
%%   The system should be runnable after
%%   mk_sys() has been run.

make_tools() ->
    mk_progs(),
    mk_lib().

make_stub() ->
    {ok, S} = file:open("stub.hrl", write),
    case file:read_file("erl_main_sae.exe") of
	{ok, B} ->
	    io:format(S, "stub() -> ~p.~n~n", [B]);
	{error, _} ->
	    io:format(S, "stub() -> ~p.~n~n", [none])
    end,
    file:close(S).

mk_lib() ->
    pack_libs("erlang", [stdlib, kernel, compiler]).

pack_libs(File, Libs) ->
    P = boot_pds:open(File ++ ".ear", read_write),
    foreach(fun(I) -> add_library(I, P) end, Libs),
    boot_pds:close(P),
    ok.
		    
add_library(Lib, P) ->
    LibS = atom_to_list(Lib),
    L = filename:join([os:getenv("ERL_TOP"),"lib",LibS]),
    io:format("Packing: ~s\n", [L]),
    pack_library(P, L ++ "/ebin", L ++ "/include").

pack_library(P, Beam, Include) ->
    Files = filelib:wildcard(Beam ++ "/*.beam"),
    foreach(fun(I) ->
		    Name = filename2name(I),
		    {ok, B} = file:read_file(I),
		    {Mod, B1} = beam_lib_strip(Name, B),
		    boot_pds:store(P, {mod,Mod}, B1)
	    end, Files),
    Files1 = filelib:wildcard(Include ++ "/*.hrl"),
    foreach(fun(I) ->
		    Name = filename2name(I),
		    {ok, B} = file:read_file(I),
		    boot_pds:store(P, {include,Name}, B)
	    end, Files1),
    ok.

filename2name(F) -> filename:rootname(filename:basename(F)).

%%----------------------------------------------------------------------

mk_progs() ->
    Erlang = filename:join([os:getenv("ERL_TOP"),"lib","kernel",
			    "ebin","erlang.beam"]),
    Bin1 = pack_beams(["fake_demand_epp.beam",
		       "fake_error_handler.beam",
		       "fake_demand_init.beam",
		       "bin_io.beam",
		       "boot_code_loader.beam", 
		       "boot_linker.beam",
		       Erlang,
		       "boot_fprim.beam", 
		       "boot_pds.beam",
		       "boot_tools.beam"]),
    mk_app("ecc", ecc, ["ecc.beam"], Bin1),
    mk_app("elink", elink, ["elink.beam"], Bin1).

mk_app(Out, StartMod, Mods, Bin1) ->
    %% io:format("App=~p Start=~p Mods=~p~n",[Out,StartMod, Mods]),
    Bin2 = pack_beams(Mods),
    {ok, Bin3} = file:read_file("boot_tools.beam"),
    {_, Bin3s} = beam_lib_strip(boot_tools, Bin3),
    {ok,Dir} = file:get_cwd(),
    Bin = term_to_binary({StartMod, Dir, Bin1, Bin2, Bin3s}),
    %% io:format("calling boot_linker:~n"),
    boot_linker:link(unix, Out, ear_dir(Dir), Bin3s, Bin),
    io:format("done~n").

ear_dir(Dir0) ->
    Dir = case os:getenv("ERLANG_EARS") of
	      false -> Dir0;
	      D -> D
	  end,
    [{"ERLANG_EARS", Dir}].

%% pack_beams(["Dir/F.beam", ...])
%%   Pack mods will find the module name from the beam code
%%   and strip the code

pack_beams(Mods) ->
    map(fun(F) ->
		case  file:read_file(F) of
		    {ok, Bin} ->
			{Mod, Bin1} = beam_lib_strip(F, Bin),
			%% io:format("boot_tools:get_mod:~p~n",[Mod]),
			{Mod, Bin1};
		    _ ->
			erlang:display({no_such_file, F}),
			erlang:halt()
		end
	end, Mods).

%% This is the first *ever* routine to get called
start(Args=[Bin,_|T]) ->
    {StartMod, _Dir, Mods1, Mods2, _} = binary_to_term(Bin),
    Loaded = load_mods(Mods1) ++ load_mods(Mods2),
    %%erlang:display({code,handler,starting}),
    boot_code_loader:startMeUp(),
    boot_code_loader:prim_loaded(Loaded),
    boot_code_loader:ensure_loaded(erl_open_port),
    boot_code_loader:ensure_loaded(user),
    %% tricky bit, this sets up a file system and IO user server
    %% up and running
    user:start(),
    G = whereis(user), 
    group_leader(G, self()),
    %% io:format("IO is running ...\n"),
    file_server:start(),
    %% io:format("File server running ...\n"),
    %% io:format("launching:~p~n",[StartMod]),
    case (catch StartMod:start(Args)) of
	{'EXIT', _} ->
	    erlang:halt();
	R ->
	    R
    end.		

load_mods([{Mod,Code}|T]) ->
    %% erlang:display({loading,Mod}),
    case erlang:load_module(Mod, Code) of
	{module,Mod} ->
	    [Mod|load_mods(T)];
	Other ->
	    erlang:display({bad_module,Mod}),
	    erlang:halt(-1)
    end;
load_mods([]) -> [].

print_args([Bin,ProgName|X]) -> 
    %% erlang:display({binary_size, size(Bin)}),
    erlang:display({progname, binary_to_list(ProgName)}),
    print_args(1, X).

print_args(N, [H|T]) ->
    erlang:display({arg,N,binary_to_list(H)}),
    print_args(N+1, T);
print_args(N, []) ->
    true.

beam_lib_strip(File, B) ->
    case beam_lib:strip(B) of
	{ok, {Mod,B1}} ->
	    {Mod, B1};
	_ ->
	    %% This case clause is for testing only
	    %% it is here to program out eay around an earlier error
	    %% in beam_lib:strip (handling compressed files)
	    %% which is not corrected
	    %% 
	    io:format("Beam lib strip errror:~p~n", [File]),
	    ModStr = filename:rootname(filename:basename(File)),
	    Mod = list_to_atom(ModStr),
	    {Mod, B}
    end.
