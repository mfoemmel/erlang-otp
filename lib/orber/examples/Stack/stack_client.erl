%% stack_client example file.

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


