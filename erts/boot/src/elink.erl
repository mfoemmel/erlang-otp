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
%% Purpose: stand-alone Erlang linker
%%
%%   elink -t [windows  |unix] [-d|-s] -o Exe -s M -m M1.beam M2.beam 
%%      Make a demand linked executable
%%      Exec will load the code in M1 M2 M3
%%      then run M:start ...
%%
%%   elink -o Exe -s M - F1 F2 F2
%%      Ditto except there is *no* demand linked code
%%
%%      When this is started dynmic code loading has been started so
%%      we can call what we like

-module(elink).

-export([start/1]).
-import(lists, [map/2]).

start(Args=[Bin,_|T]) ->
    Args1 = map(fun(I) -> binary_to_list(I) end, T),
    run(Args1, Bin),
    erlang:halt().

run(["-t", OSstr, DemandStr, "-o", Out, "-S", Mod, "-m" | Files], Bin) ->
    Os = check_type(OSstr),
    Demand = check_demand(DemandStr),
    check_out(Os, Out),
    Beams = beams(Files),
    mk_exec(Demand, Os, Bin, Out, list_to_atom(Mod), Beams);
run(["-r", BeamEvm|Files], _) ->
    rebase(Files, BeamEvm);
run(Args, _) ->
    error("Cannot parse arguments:~p~n", [Args]),
    usage().

check_type("unix") -> unix;
check_type("windows") -> windows;
check_type(Str) ->
    error("*** invalid -t:~s should be windows or unix~n",[Str]),
    usage().
    
check_demand("-s") ->
    error("*** -s not yet implemented~n",[]),
    usage();
check_demand("-d") ->
    dynamic;
check_demand(Str) ->
    error("*** invalid ~s should be -s or -d~n",[Str]),
    usage().

check_out(unix, _) -> true;
check_out(windows, F) ->
    case filename:extension(F) of
	".exe" -> true;
	_ ->
	    error("*** invalid filename:~s should have extension .exe~n",
		  [F]),
	    usage()
    end.

%% Just check that all the filenames have the right extension

beams(F) ->
    map(fun(I) ->
		case filename:extension(I) of
		    ".beam" ->
			I;
		    _ ->
			error("*** invalid file:~s "
			      "must have extension .beam~n",
			      [I]),
			usage()
		end
	end, F).
			
usage() ->
    error("Usage:\n"
	  "  elink -t [windows | unix] [-d | -s] -o out[.exe]\n"
	  "        -S mod -m m1.beam m2.beam m2.beam ...\n\n", []),
    halt(1).

error(F, A) ->
    io:format(F, A).

mk_exec(dynamic, Os, Bin, Out, Start, Beams) ->
    {_StartMod, Dir, Bin1, _, BinSae} = binary_to_term(Bin), 
    Extra = boot_tools:pack_beams(Beams),
    B = term_to_binary({Start, Dir, Bin1, Extra, BinSae}, [compressed]),
    EarDir = ear_dir(Dir),
    boot_linker:link(Os, Out, [{"ERLANG_EARS", EarDir}], BinSae, B),
    true.

ear_dir(Dir) ->
    case os:getenv("ERLANG_EARS") of
	false -> Dir;
	EarDir -> EarDir
    end.

rebase([Name|T], BeamEvm) ->
    case is_script(Name) of
	false -> ok;
	true -> rebase_1(Name, BeamEvm)
    end,
    rebase(T, BeamEvm);
rebase([], _) -> ok.

rebase_1(Name, BeamEvm) ->
    {ok,Bin0} = file:read_file(Name),
    EarDir = case os:getenv("ERLANG_EARS") of
		 false -> "";
		 EarDir0 -> ["ERLANG_EARS=",EarDir0]
	     end,
    Head = ["#!/bin/sh\n",
	    "exec " ++ BeamEvm ++ " $0 ${1+\"$@\"}\n",EarDir,"\n"],
    Bin = [Head|rebase_2(Bin0)],
    ok = file:write_file(Name, Bin).

rebase_2(<<$#,T/binary>>) ->
    rebase_2(skip_to_eol(T));
rebase_2(<<"exec",T/binary>>) ->
    rebase_2(skip_to_eol(T));
rebase_2(<<"ERLANG_EARS=",T/binary>>) ->
    rebase_2(skip_to_eol(T));
rebase_2(T) -> T.

skip_to_eol(<<$\n,T/binary>>) -> T;
skip_to_eol(<<_,T/binary>>) -> skip_to_eol(T).
    
is_script(Name) ->
    case file:open(Name, [read,raw,binary]) of
	{ok,F} ->
	    Res = case file:read(F, 2) of
		      {ok,<<"#!">>} -> true;
		      {ok,_} -> false
		  end,
	    file:close(F),
	    Res;
	{ok,eisdir} -> false
    end.
