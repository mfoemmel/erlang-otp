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
-module(ebundle).

%% Purpose : Utility for bundling Erlang modules.

-include_lib("kernel/include/file.hrl").

-export([unix/1,win32/1]).

%%% This module is meant for bootstrapping Standalone Erlang.
%%% It can be run in a R5 system, but not standalone, because
%%% it uses the file module, not the file_prim module.

unix(Args) ->
    make_bundle(Args, fun make_script/2).

make_script(File, Payload) ->
    Script = ["#!/clearcase/otp/erts/bin/sparc-sun-solaris2.7/beam_evm\n",
	      ":",integer_to_list(size(Payload)),"\n",Payload,"--end--\n"],
    ok = file:write_file(File, Script),
    file:write_file_info(File, #file_info{mode=8#770}).
    
win32(Args) ->
    make_bundle(Args, fun(File, Payload) -> file:write_file(File, Payload) end).

make_bundle(Args, Fun) ->
    case catch make_bundle1(Args, Fun) of
	{'EXIT',Reason} ->
	    io:format("Error: ~P\n", [Reason,10]),
	    halt(1);
	ok ->
	    halt(0);
	Other ->
	    io:format("Bad return value: ~P\n", [Other,10]),
	    halt(1)
    end.

make_bundle1([Output|Files], Fun) ->
    Modules = read_modules(Files),
    MF = start_function(Modules),
    Payload = term_to_binary({Modules,MF}),
    Fun(Output, Payload).

read_modules([File|Fs]) ->
    Mod = list_to_atom(filename:rootname(filename:basename(File))),
    {ok,Bin} = file:read_file(File),
    [{Mod,Bin}|read_modules(Fs)];
read_modules([]) -> [].

start_function([{Mod,Bin}|_]) ->
    {Mod,main}.
