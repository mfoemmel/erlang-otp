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

-module(asn1rt_driver_handler).

-include("asn1_records.hrl").

-export([init/1,load_driver/0,unload_driver/0]).

-ifdef(asn1_r7b_enable_driver).

load_driver() ->
    spawn(asn1rt_driver_handler, init, [self()]).

init(From) ->
    case whereis(asn1_port_owner) of
	undefined -> %driver not loaded
	    case catch register(asn1_port_owner,self()) of
		{'EXIT',Reason} ->
		    asn1_port_owner ! {request_port,From};
		_ ->
		    ets:new(asn1_driver_table,[named_table]),
		    Dir = filename:join([code:priv_dir(asn1),"lib"]),
		    erl_ddll:load_driver(Dir,"asn1_erl_drv"),
		    Port = open_port({spawn,"asn1_erl_drv"},[]),
		    ets:insert(asn1_driver_table,{asn1_port,Port}),
		    From ! {reply,Port},
		    loop(Port)
	    end;
	_ -> %driver loaded
	    asn1_port_owner ! {request_port,From}
    end.

loop(Port) ->
    receive
	{request_port,Sender} ->
	    Sender ! {reply,Port},
	    loop(Port);
	unload ->
	    port_close(Port),
	    erl_ddll:unload_driver("asn1_erl_drv")
    end.


unload_driver() ->
    case whereis(asn1_port_owner) of
	Pid when pid(Pid) ->
	    asn1_port_owner ! unload,
	    ok;
	_ ->
	    ok
    end.

-else.

load_driver() ->
    case is_driver_owner_registered() of %to prevent unecessary spawn
	false ->
	    spawn(asn1rt_driver_handler, init, [self()]);
	_ ->
	    asn1_driver_owner ! {are_you_ready,self()},
	    ok
    end.

init(From) ->
    case is_driver_owner_registered() of
	false ->
	    case catch register(asn1_driver_owner,self()) of
		true -> 
		    Dir = filename:join([code:priv_dir(asn1),"lib"]),
		    case catch erl_ddll:load_driver(Dir,"asn1_erl_drv") of
			ok ->
			    open_named_port(From);
			Error -> % if erl_ddll:load_driver fails
			    asn1_driver_owner ! unload,
			    From ! Error
		    end,
		    loop();
		{'EXIT',{badarg,_}} ->
		    asn1_driver_owner ! {are_you_ready,From},
		    ok
	    end;
	_ ->
	    asn1_driver_owner ! {are_you_ready,From},
	    ok
    end.


open_named_port(From) ->
    case is_port_open(asn1_driver_port) of
	false ->
	    case catch open_port({spawn,"asn1_erl_drv"},[]) of
		{'EXIT',Reason} ->
		    From ! {port_error,Reason};
		Port ->
		    register(asn1_driver_port,Port),
		    From ! driver_ready
	    end;
	_ ->
	    From ! driver_ready,
	    ok
    end.

is_driver_owner_registered() ->
    case whereis(asn1_driver_owner) of
	Pid when pid(Pid) ->
	    true;
	_ ->
	    false
    end.

is_port_open(Name) ->
    case whereis(Name) of
	Port when port(Port) ->
	    true;
	_ -> false
    end.

loop() ->
    receive
	unload ->
	    case whereis(asn1_driver_port) of
		Port when port(Port) ->
		    port_close(Port);
		_ -> ok
	    end,
	    erl_ddll:unload_driver("asn1_erl_drv"),
	    ok;
	{are_you_ready,From} ->
	    From ! driver_ready,
	    loop();
	_ ->
	    loop()
    end.

unload_driver() ->
    case whereis(asn1_driver_owner) of
	Pid when pid(Pid) ->
	    Pid ! unload,
	    ok;
	_ -> 
	    ok
    end.

-endif.
