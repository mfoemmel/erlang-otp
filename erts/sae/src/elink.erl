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
%%       This module can ONLY CALL file_prim.erl          %%
%%                    AND NOTHING ELSE                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("kernel/include/file.hrl").

-export([main/1, test/0, make/5, make_stand_alone/0,
	 make_bundle/1,	 make_program/2, unpack_program/1]).

%% This can be run in two modes
%%   1) From the development system make_stand_alone/0
%%   2) In the target system where it is launched with main/1

%% Note we don't need to load error_handler (cos nothing is undefined!)

test() ->
    make("hello", pack([otp_pre_init,init,erl_prim_loader]), otp_pre_init, main, []).

make_bundle(Files) ->
    erlang:display(Files),
    halt().

make_stand_alone() ->
    Mods = pack([file_prim, elink]),
    make("elink", Mods, elink, main, []).

main(Env) ->
    {OutObj, Mods, StartMod, StartFun, StartArgs} = parse_args(Env),
    Mods1 = pack(Mods),
    make(OutObj, Mods1, StartMod, StartFun, StartArgs).

%% Ms = [{modName:atom(), code:bin()}].

make(OutObj, Ms, StartMod, StartFun, StartArgs) ->
    check_start_function(StartMod, StartFun, Ms),
    make_program(OutObj, {Ms, {StartMod, StartFun, StartArgs}}).

%% packs Ms = [{Mod,Bin}], StartMod, StartFun, StartArgs

make_program(OutObj, BLM) ->
    Payload = term_to_binary(BLM),
    Len = size(Payload),
    Header = ["#!/clearcase/otp/erts/bin/sparc-sun-solaris2.5.1/beam_evm.debug\n",
	      "ROOTDIR=/clearcase/otp/erts\n",
	      ":", integer_to_list(Len),"\n"],
    Everything = [Header,Payload,"--end--\n"],
    file_prim:write_file(OutObj, Everything),
    file_prim:change_mode(OutObj, 8#755),
    true.

unpack_program(File) ->
    {ok, Bin} = file:read_file(File),
    case size(Bin) of
	Size when Size > 8 ->
	    {B1, B2} = split_binary(Bin, Size - 8),
	    case binary_to_list(B2) of
		[N1,N2,N3,N4,22,33,44,55] ->
		    Length = (N1 bsl 24) + (N2 bsl 16) + (N3 bsl 8) + N4,
		    Npad = npad(Length),
		    {_, B4} = split_binary(B1, size(B1)-Length-Npad),
		    B7 = case Npad of
			     0 -> B4;
			     _ ->
				 {B5, B6} = split_binary(B4, Length),
				  B5
			 end,
		    binary_to_term(B7);
		_ -> 
		    exit(not_blm)
	    end;
	_ ->
	    exit(not_blm)
    end.

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
    %% erlang:display({pack, Mod}),
    case file_prim:read_file(atom_to_list(Mod) ++ ".beam") of
	{ok, Bin} ->
	    [{Mod, Bin}|pack(T)];
	_ ->
	    exit({no,such,file,Mod,'jam'})
    end;
pack([]) -> [].

npad(L) ->
    case 4 - (L rem 4) of
	4 -> 0;
	N -> N
    end.

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
	[M,F|A] ->
	    {OutObj, ModsA, list_to_atom(M), list_to_atom(F), A};
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
    erlang:display('usage elink -o ExeFile -m Mod1 Mod2 .. -s M F Arg1 Arg2 ..'),
    exit({error, arguments}).

map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].





