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
-module(eva_sup).

-behaviour(supervisor).

-define(SupFlags, {one_for_one, 4, 3600}).

-define(EvaServer, {eva_server,
		    {eva_server, start_link, []},
		    permanent, 2000, worker, [eva_server]}).

-define(EvaSnmp, {eva_snmp_adaptation,
		  {eva_snmp_adaptation, start_link, []},
		  permanent, 2000, worker, [eva_snmp_adaptation]}).

%% External exports
-export([create_tables/1, create_tables_log/1]).
-export([start_link/0, start_link_log/1]).

-export([create_tables_snmp/1, create_tables_log_snmp/1]).
-export([start_link_snmp/0, start_link_log_snmp/3]).

%% Internal exports
-export([init/1]).

create_tables(Nodes) ->
    eva_server:create_tables(Nodes).

create_tables_log(Nodes) ->
    eva_server:create_tables(Nodes).

create_tables_snmp(Nodes) ->
    eva_server:create_tables(Nodes),
    eva_snmp_adaptation:create_tables(Nodes).

create_tables_log_snmp(Nodes) ->
    eva_server:create_tables(Nodes),
    eva_snmp_adaptation:create_tables(Nodes),
    eva_log_snmp:create_tables(Nodes),
    log_snmp:create_tables(Nodes).

start_link() ->
    supervisor:start_link({local, eva_sup}, ?MODULE, plain).

start_link_log(DefaultLog) ->
    supervisor:start_link({local, eva_sup}, ?MODULE, {log, DefaultLog}).

start_link_snmp() ->
    supervisor:start_link({local, eva_sup}, ?MODULE, snmp).

start_link_log_snmp(DefaultLog, LogDir, MaxDirSize) ->
    supervisor:start_link({local, eva_sup}, ?MODULE,
			  {log_snmp, DefaultLog, LogDir, MaxDirSize}).


init(plain) ->
    {ok, {?SupFlags, [?EvaServer]}};

init(snmp) ->
    {ok, {?SupFlags, [?EvaServer, ?EvaSnmp]}};

init({log, DefaultLog}) ->
    EvaLog = {eva_log_sup,
	      {eva_log_sup, start_link, [DefaultLog]},
	      permanent, infinity, supervisor, [eva_log_sup]},      
    {ok, {?SupFlags, [?EvaServer, EvaLog]}};

init({log_snmp, DefaultLog, LogDir, MaxDirSize}) ->
    EvaLog = {eva_log_sup,
	      {eva_log_sup, start_link, [DefaultLog, LogDir, MaxDirSize]},
	      permanent, infinity, supervisor, [eva_log_sup]},      
    {ok, {?SupFlags, [?EvaServer, ?EvaSnmp, EvaLog]}}.
