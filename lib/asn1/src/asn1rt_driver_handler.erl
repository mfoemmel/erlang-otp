%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%

-module(asn1rt_driver_handler).

-include("asn1_records.hrl").

-export([load_driver/0,unload_driver/0,client_port/0]).

%% Internal exports
-export([init/1]).

%% Macros
-define(port_names,
	{ asn1_drv01, asn1_drv02, asn1_drv03, asn1_drv04,
	  asn1_drv05, asn1_drv06, asn1_drv07, asn1_drv08,
	  asn1_drv09, asn1_drv10, asn1_drv11, asn1_drv12,
	  asn1_drv13, asn1_drv14, asn1_drv15, asn1_drv16 }).

%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

load_driver() ->
    case is_driver_owner_registered() of %to prevent unecessary spawn
	false ->
	    Self = self(),
	    spawn(asn1rt_driver_handler, init, [Self]);
	_ ->
	    asn1_driver_owner ! {are_you_ready,self()},
	    ok
    end,
    receive
	driver_ready ->
	    ok;
	Error ->
	    Error
    after 10000 -> %% 10 seconds
	    {error,{timeout,waiting_for_drivers}}
    end.

init(From) ->
    case is_driver_owner_registered() of
	false ->
	    case catch register(asn1_driver_owner,self()) of
		true -> 
		    Dir = filename:join([code:priv_dir(asn1),"lib"]),
		    case catch erl_ddll:load_driver(Dir,asn1_erl_drv) of
			ok ->
			    open_named_ports(From),
			    asn1_driver_owner ! {are_you_ready,From};
			{error,_} -> % if erl_ddll:load_driver fails
			    OSDir = filename:join(Dir,erlang:system_info(system_architecture)),
			    case catch erl_ddll:load_driver(OSDir,asn1_erl_drv) of
				ok ->
				    open_named_ports(From),
				    asn1_driver_owner ! {are_you_ready,From};
				Error2 ->
				    asn1_driver_owner ! unload,
				    From ! Error2
			    end
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

    
open_named_ports(From) ->
    open_named_ports(From,size(?port_names)).

open_named_ports(From,0) ->
    From ! driver_ready;
open_named_ports(From,N) ->
    case catch open_port({spawn,"asn1_erl_drv"},[]) of
	{'EXIT',Reason} ->
	    From ! {port_error,Reason};
	Port ->
	    register(element(N,?port_names),Port),
	    open_named_ports(From,N-1)
    end.

is_driver_owner_registered() ->
    case whereis(asn1_driver_owner) of
	Pid when pid(Pid) ->
	    true;
	_ ->
	    false
    end.

%% is_port_open(Name) ->
%%     case whereis(Name) of
%% 	Port when port(Port) ->
%% 	    true;
%% 	_ -> false
%%     end.

loop() ->
    receive
	unload ->
	    close_ports(size(?port_names)),
	    erl_ddll:unload_driver(asn1_erl_drv),
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
close_ports(0) ->
    ok;
close_ports(N) ->   
    element(N,?port_names) ! {self(), close}, %% almost same as port_close(Name)
    close_ports(N-1).

client_port() ->
    element(erlang:system_info(scheduler_id) rem size(?port_names) + 1,
	    ?port_names).
