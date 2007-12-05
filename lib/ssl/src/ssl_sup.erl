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

-module(ssl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
%% init([]) -> {ok,  {SupFlags,  [ChildSpec]}}
%%
init([]) ->
    
    %% OLD ssl - moved start to ssl.erl only if old
    %% ssl is acctualy run!
    %%Child1 = {ssl_server, {ssl_server, start_link, []},
    %%	       permanent, 2000, worker, [ssl_server]},

    %% Does not start any port programs so it does matter
    %% so much if it is not used!
    Child2 = {ssl_broker_sup, {ssl_broker_sup, start_link, []},
	      permanent, 2000, supervisor, [ssl_broker_sup]},


    %% New ssl
    SessionCertManager = session_and_cert_manager_child_spec(),
    ConnetionManager = connection_manager_child_spec(),

    {ok, {{one_for_all, 10, 3600}, [Child2, SessionCertManager,
				    ConnetionManager]}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

session_and_cert_manager_child_spec() ->
    Name = ssl_manager,  
    StartFunc = {ssl_manager, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

connection_manager_child_spec() ->
    Name = ssl_connection,  
    StartFunc = {ssl_connection_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_connection],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.


