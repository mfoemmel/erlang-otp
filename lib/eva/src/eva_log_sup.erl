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
-module(eva_log_sup).

-behaviour(supervisor).

-define(LogServer, {log_server,
		    {log_server, start_link, []},
		    permanent, 2000, worker, [log_server]}).

-define(LogSnmp, {log_snmp,
		  {log_snmp, start_link, []},
		  permanent, 2000, worker, [log_snmp]}).

%% External exports
-export([start_link/1, start_link/3]).

%% Internal exports
-export([init/1]).

%%-----------------------------------------------------------------
%% Convinent supervisor to use for LOG, with or without snmp
%% implementation.
%%-----------------------------------------------------------------
start_link(DefaultLog) ->
    supervisor:start_link({local, eva_log_sup}, ?MODULE, DefaultLog).
start_link(DefaultLog, LogDir, MaxDirSize) ->
    supervisor:start_link({local, eva_log_sup}, ?MODULE,
			  {DefaultLog, LogDir, MaxDirSize}).

init({DefaultLog, LogDir, MaxDirSize}) ->
    SupFlags = {rest_for_one, 4, 3600},
    EvaLogH = {eva_log_h_sup,
	       {eva_log, start_link, [DefaultLog]},
	       permanent, 2000, worker, [eva_log]},
    EvaLogSnmp = {eva_log_snmp,
		  {eva_log_snmp, start_link, [LogDir, MaxDirSize]},
		  permanent, 2000, worker, [eva_log_snmp]},
    {ok, {SupFlags, [?LogServer, EvaLogH, ?LogSnmp, EvaLogSnmp]}};

init(DefaultLog) ->
    SupFlags = {rest_for_one, 4, 3600},
    EvaLogH = {eva_log_h_sup,
	       {eva_log, start_link, [DefaultLog]},
	       permanent, 2000, worker, [eva_log]},
    {ok, {SupFlags, [?LogServer, EvaLogH]}}.
