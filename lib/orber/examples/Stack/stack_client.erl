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
-module('stack_client').
-include_lib("orber/include/corba.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/COSS/CosNaming/lname.hrl").
 
-export([run/0, run/1]).


run() ->
    NS = corba:resolve_initial_references("NameService"),
    run_1(NS).

run(HostRef) ->
    NS = corba:resolve_initial_references_remote("NameService", HostRef),
    run_1(NS).


run_1(NS) ->
    NC = lname_component:set_id(lname_component:create(), "StackFactory"),
    N = lname:insert_component(lname:create(), 1, NC),
    case catch 'CosNaming_NamingContext':resolve(NS, N) of
	{'EXCEPTION', E} ->
	    io:format("The stack factory server is not registered~n",[]);
	SF ->
	    %% Create the stack
	    SS = 'StackModule_StackFactory':create_stack(SF),

	    %% io:format("SS pid ~w~n",[iop_ior:get_key(SS)]),
	    'StackModule_Stack':push(SS, 4),
	    'StackModule_Stack':push(SS, 7),
	    'StackModule_Stack':push(SS, 1),
	    'StackModule_Stack':push(SS, 1),
	    Res = 'StackModule_Stack':pop(SS),
	    io:format("~w~n", [Res]),
	    Res1 = 'StackModule_Stack':pop(SS),
	    io:format("~w~n", [Res1]),
	    Res2 = 'StackModule_Stack':pop(SS),
	    io:format("~w~n", [Res2]),
	    Res3 = 'StackModule_Stack':pop(SS),
	    io:format("~w~n", [Res3]),

	    %% Remove the stack
	    'StackModule_StackFactory':destroy_stack(SF, SS)
	    
    end.


