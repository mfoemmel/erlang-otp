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
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for an instance of the http server. (You may
%%          have several instances running on the same machine.) Hangs under
%%          httpd_sup.
%%----------------------------------------------------------------------

-module(httpd_instance_sup).

-behaviour(supervisor).

-include("httpd_verbosity.hrl").

-export([init/1]).

%% Internal API
-export([start/2, start_link/2, start2/2, start_link2/2, stop/1, stop/2, stop2/1]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([ConfigFile, ConfigList, Verbosity, Addr, Port]) -> 
    init(ConfigFile, ConfigList, Verbosity, Addr, Port);
init(BadArg) ->
    {error, {badarg, BadArg}}.

init(ConfigFile, ConfigList, Verbosity, Addr, Port) ->
    Flags = {one_for_one, 0, 1},
    AccSupVerbosity  = get_acc_sup_verbosity(Verbosity),
    MiscSupVerbosity = get_misc_sup_verbosity(Verbosity),
    Children  = [sup_spec(httpd_acceptor_sup, Addr, Port, AccSupVerbosity), 
		 sup_spec(httpd_misc_sup, Addr, Port, MiscSupVerbosity), 
		 worker_spec(httpd_manager, Addr, Port, ConfigFile, ConfigList, 
			     Verbosity)],
    {ok, {Flags, Children}}.


%%%=========================================================================
%%%  ??? functions
%%%=========================================================================

start(ConfigFile, Verbosity) ->
    case start_link(ConfigFile, Verbosity) of
	{ok, Pid} ->
	    unlink(Pid),
	    {ok, Pid};

	Else ->
	    Else
    end.

    
start_link(ConfigFile, Verbosity) ->
    case get_addr_and_port(ConfigFile) of
	{ok, ConfigList, Addr, Port} ->
	    Name    = make_name(Addr, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE, 
				  [ConfigFile, ConfigList, 
				   Verbosity, Addr, Port]);

	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason};

	Else ->
	    error_logger:error_report(Else),
	    {stop, Else}
    end.

    
start2(ConfigList, Verbosity) ->
    case start_link2(ConfigList, Verbosity) of
	{ok, Pid} ->
	    unlink(Pid),
	    {ok, Pid};

	Else ->
	    Else
    end.

    
start_link2(ConfigList, Verbosity) ->
    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
    Name    = make_name(Addr, Port),
    SupName = {local, Name},
    supervisor:start_link(SupName, ?MODULE, 
			  [undefined, ConfigList, Verbosity, Addr, Port]).
    

stop(Pid) when pid(Pid) ->
    do_stop(Pid);
stop(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok, _, Addr, Port} ->
	    stop(Addr, Port);
	    
	Error ->
	    Error
    end;
stop(_StartArgs) ->
    ok.


stop(Addr, Port) when integer(Port) ->
    Name = make_name(Addr, Port), 
    case whereis(Name) of
	Pid when pid(Pid) ->
	    do_stop(Pid),
	    ok;
	_ ->
	    not_started
    end.
    

stop2(ConfigList) when list(ConfigList) ->
    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
    stop(Addr, Port).

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
do_stop(Pid) ->
    exit(Pid, shutdown).

sup_spec(SupModule, Addr, Port, Verbosity) ->
    Name = {SupModule, Addr, Port},
    StartFunc = {SupModule, start_link, [Addr, Port, Verbosity]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [SupModule],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
    
worker_spec(WorkerModule, Addr, Port, ConfigFile, ConfigList, Verbosity) ->
    Name = {WorkerModule, Addr, Port},
    StartFunc = {WorkerModule, start_link, [ConfigFile, ConfigList, Verbosity]}, 
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [WorkerModule],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

make_name(Addr,Port) ->
    httpd_util:make_name("httpd_instance_sup",Addr,Port).

get_addr_and_port(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
	    {ok, ConfigList, Addr, Port};
	Error ->
	    Error
    end.

get_addr_and_port2(ConfigList) ->
    Port = httpd_util:key1search(ConfigList, port, 80),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    {ok, Addr, Port}.

get_acc_sup_verbosity(V) ->
    case httpd_util:key1search(V, all) of
	undefined ->
	    httpd_util:key1search(V, acceptor_sup_verbosity, ?default_verbosity);
	Verbosity ->
	    Verbosity
    end.

get_misc_sup_verbosity(V) ->
    case httpd_util:key1search(V, all) of
	undefined ->
	    httpd_util:key1search(V, misc_sup_verbosity, ?default_verbosity);
	Verbosity ->
	    Verbosity
    end.
