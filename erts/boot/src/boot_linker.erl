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

-module(boot_linker).

-export([link/5, stub/0]).

-import(lists, [map/2, member/2]).

-use([lists]).

-include("stub.hrl").

-include_lib("kernel/include/file.hrl").

link(Type, OutFile, EnvDict, BeamCode, Bin) ->
    EnvBin = make_env_string(Type, EnvDict),
    case Type of
	windows ->
	    %% io:format("calling coff pack: Stub:~p out=~p",
	    %% [size(stub()), OutFile]),
	    OutB = coff:pack(stub(), EnvBin, BeamCode, Bin),
	    file:write_file(OutFile, OutB);
	unix ->
	    make_program(OutFile, EnvBin, BeamCode, Bin)
    end.

make_env_string(Type, Dict) ->
    Str = map(fun({Key, Val}) ->
		      case (member($=, Key) or member($:, Key))of
			  true ->
			      exit({bad_key, Key});
			  false ->
			      case (member($\r, Val) or member($\n, Val)) of
				  true ->
				      exit({bad_value, Val});
				  false ->
				      [Key,$=,Val,separator(Type)]
			      end
		      end
	      end, Dict),
    list_to_binary(Str).

separator(windows) -> "\r\n";
separator(unix)    -> "\n".
    
make_program(OutFile, Env, BeamCode, Code) ->
    %% erlang:display({boot_linker,make,OutFile}),
    Head = case os:getenv("ERL_BEAM_EVM") of
	       false ->
		   "#!/usr/bin/env beam_evm\n";
	       BeamEvm ->
		   ["#!/bin/sh\n",
		    "exec " ++ BeamEvm ++ " $0 ${1+\"$@\"}\n"]
	   end,
    Data = [Head,
	    Env,
	    ":", integer_to_list(size(BeamCode)), "\n", BeamCode,
	    ":", integer_to_list(size(Code)),  "\n", Code,
	    "--end--\n"],
    prim_file:write_file(OutFile, Data),
    prim_file:write_file_info(OutFile, #file_info{mode=8#755}),
    true.
