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
%% Purpose: The supervisor for acceptor processes in the http server, 
%%          hangs under the httpd_instance_sup_<Addr>_<Port> supervisor.
%%----------------------------------------------------------------------

-module(httpd_acceptor_sup).

-behaviour(supervisor).

-include("httpd_verbosity.hrl").

%% API
-export([start_link/3, start_acceptor/4, stop_acceptor/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Addr, Port, AccSupVerbosity) ->
    SupName = make_name(Addr, Port),
    supervisor:start_link({local, SupName}, ?MODULE, [AccSupVerbosity]).

%%----------------------------------------------------------------------
%% Function: [start|stop]_acceptor/5
%% Description: Starts/stops an [auth | security] worker (child) process
%%----------------------------------------------------------------------
start_acceptor(SocketType, Addr, Port, ConfigDb) ->
    Verbosity = get_acc_verbosity(),
    start_worker(httpd_acceptor, SocketType, Addr, Port, 
		 ConfigDb, Verbosity, self(), []).

stop_acceptor(Addr, Port) ->
    stop_worker(httpd_acceptor, Addr, Port).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Verbosity]) -> 
    do_init(Verbosity);

init(BadArg) ->
    {error, {badarg, BadArg}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================      
do_init(Verbosity) ->
    put(verbosity,?vvalidate(Verbosity)),
    put(sname,acc_sup),
    ?vlog("starting", []),
    Flags     = {one_for_one, 500, 100},
    Workers   = [],
    {ok, {Flags, Workers}}.

make_name(Addr,Port) ->
    httpd_util:make_name("httpd_acc_sup",Addr,Port).

get_acc_verbosity() ->
    get_verbosity(get(acceptor_verbosity)).

get_verbosity(undefined) ->
    ?default_verbosity;
get_verbosity(V) ->
    ?vvalidate(V).

start_worker(M, SocketType, Addr, Port, ConfigDB, Verbosity, Manager, 
	     Modules) ->
    SupName = make_name(Addr, Port),
    Args    = [Manager, SocketType, Addr, Port, ConfigDB, Verbosity],
    Spec    = {{M, Addr, Port},
	       {M, start_link, Args}, 
	       permanent, timer:seconds(1), worker, [M] ++ Modules},
    supervisor:start_child(SupName, Spec).

stop_worker(M, Addr, Port) ->
    SupName = make_name(Addr, Port),
    Name    = {M, Addr, Port},
    case supervisor:terminate_child(SupName, Name) of
	ok ->
	    supervisor:delete_child(SupName, Name);
	Error ->
	    Error
    end.
