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
-module(elink).

%% Purpose : Make an binary load module from a number of object 
%%           code modules and a start function
%% Usage   : elink -b BLM -o Out -m M1 M1 M2 -s M F A

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     IMPORTANT NOTE                     %%
%%       This module can ONLY CALL prim_file.erl          %%
%%                    AND NOTHING ELSE                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("kernel/include/file.hrl").
%% -compile(export_all).
-export([main/1, make/5, make_elink/0, patch/3,
	 make_program/3, unpack_program/1]).

%% This can be run in two modes
%%   1) From the development system make_stand_alone/0
%%   2) In the target system where it is launched with main/1

%% Note we don't need to load error_handler (cos nothing is undefined!)

make_elink() ->
    Mods = pack([prim_file, elink]),
    make("elink", Mods, elink, main, []).

main(Args) ->
    case (catch main1(Args)) of
	{'EXIT', Why} ->
	    erlang:display(Why);
	_ ->
	    true
    end,
    halt().

main1(Args) ->
    {OutObj, Mods, StartMod, StartFun} = parse_args(Args),
    Mods1 = pack(Mods),
    make(OutObj, Mods1, StartMod, StartFun, []).

%% Ms = [{modName:atom(), code:bin()}].

make(OutObj, Ms, StartMod, StartFun, Env) ->
    check_start_function(StartMod, StartFun, Ms),
    make_program(OutObj, {Ms, {StartMod, StartFun}}, Env).

%% packs Ms = [{Mod,Bin}], StartMod, StartFun, StartArgs

make_program(OutObj, BLM, Env) ->
    Payload = term_to_binary(BLM),
    Len = size(Payload),
    Header = ["#!/usr/bin/env beam_evm\n",
	      Env,
	      ":", integer_to_list(Len),"\n"],
    Everything = [Header,Payload,"--end--\n"],
    prim_file:write_file(OutObj, Everything),
    prim_file:write_file_info(OutObj, #file_info{mode=8#755}),
    true.

patch(File, Key, Val) ->
    {Strs, Stuff} = unpack_program(File),
    Strs1 = patch1(Key, Strs, Val, []),
    Env = map(fun({I,J}) -> I ++ "=" ++ J ++ "\n" end, Strs1),
    make_program(File, Stuff, Env).

patch1(Key, [{Key,_}|T], New, L) -> 
    reverse(L) ++  [{Key,New}|T];
patch1(Key, [H|T], New, L) ->
    patch1(Key, T, New, [H|L]);
patch1(Key, [], New, L) ->
    [{Key,New}|reverse(L)].

unpack_program(File) ->
    {ok, Bin} = file:read_file(File),
    Size = min(10000, size(Bin)- 1),
    {B1, _} = split_binary(Bin, Size),
    Str = binary_to_list(B1),
    {Strs, L1} = get_headers(Str, 0, []),
    {_, B2} = split_binary(Bin, L1),
    {Bin1, _} = split_binary(B2, size(B2) - 8),
    Strs1 = first(tl(Strs)),
    Strs2 = map(fun(I) -> split(I) end, Strs1),
    {Strs2, binary_to_term(Bin1)}.

split(Str) -> split(Str, []).

split([$=|T], L) -> {reverse(L), first(T)};
split([H|T], L) -> split(T, [H|L]);
split([], L) -> {reverse(L), ""}.
    

min(X, Y) when X < Y -> X;
min(X, Y)            -> Y.

first([_])   ->  [];
first([H|T]) -> [H|first(T)].

get_headers([$:|T], N, L) ->
    {Str1, _} = get_line(T),
    {reverse([[$:|Str1]|L]), N + length(Str1) +1};
get_headers(Str, N, L) ->
    {Str1, Rest} = get_line(Str),
    get_headers(Rest, N + length(Str1), [Str1|L]).

get_line(Str) -> get_line(Str, []).

get_line([$\n|T], L) -> {reverse([$\n|L]), T};
get_line([H|T], L)   -> get_line(T, [H|L]);
get_line([], _)      -> exit(get_line).

check_start_function(Mod, Func, [{Mod,Bin}|_]) ->
    %% load the module and test it
    case erlang:load_module(Mod, Bin) of
	{module, Mod} ->
	   case erlang:function_exported(Mod,Func,1) of
	       false ->
		   erlang:display({fatal,error,module,Mod,
				   'does not export',Func,'/1'}),
		   exit(oops);
	       _ ->
		   true
	   end;
	Other ->
	    erlang:display({errorloading,Mod,Other,size(Bin)}),
	    exit(oops)
    end;
check_start_function(Mod, Func, [_|T]) ->
    check_start_function(Mod, Func, T);
check_start_function(Mod, Func, []) ->
    erlang:display({fatal,error,module,Mod,'not found'}),
    exit(usage).

pack([Mod|T]) ->
    S = atom_to_list(Mod),
    SMod = real_name(S),
    Path = "../ebin/" ++ S ++ ".beam",
    case prim_file:read_file(Path) of
	{ok, Bin} ->
	    [{list_to_atom(SMod), Bin}|pack(T)];
	_ ->
	    exit({'elink:',no,such,file,list_to_atom(Path)})
    end;
pack([]) -> [].

real_name([$f,$a,$k,$e,$_|T]) -> skip_under(T);
real_name(M)                  -> M.

skip_under([$_|T]) -> T;
skip_under([_|T])  -> skip_under(T);
skip_under([])     -> exit(bad_fake_name).

%% parse_args(Env) -> {OutFileName, Mods, StartMod, StartFun, StartArgs}

parse_args(Env) ->
    OutObj = case collect("-o", Env) of
		 [X] -> X;
		 _ -> usage()
	     end,
    Mods = collect("-m", Env),
    case Mods of
	[] -> usage();
	_ -> true
    end,
    ModsA = map(fun(I) -> list_to_atom(I) end, Mods),
    case collect("-s", Env) of
	[M,F] ->
	    {OutObj, ModsA, list_to_atom(M), list_to_atom(F)};
	_ ->
	    usage()
    end.

collect(Start, [Start|T]) -> collect1(T);
collect(Start, [_|T])     -> collect(Start, T);
collect(_, [])            -> [].

collect1([[$-|_]|T]) -> [];
collect1([H|T])      -> [H|collect1(T)];
collect1([])         -> [].

usage() -> 
    erlang:display('usage elink -o ExeFile -m Mod1 Mod2 .. -s Mod Func'),
    exit({error, arguments}).

map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].

reverse(X) -> reverse(X, []).

reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L)   -> L.
     


