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
-module(oms).

%% External exports
-export([start/2, stop/1, create/0]).

%%%-----------------------------------------------------------------
%%% This module implements the application OMS.
%%%-----------------------------------------------------------------
-behaviour(application).

start(_, []) ->
    EvaLogDir = "ex_log/",
    {ok, "default"} =
	disk_log:open([{name, "default"}, {file, EvaLogDir ++ "default.LOG"},
		       {type, wrap}, {size, {10000, 4}}]),
    case application:get_env(oms, use_snmp) of
	{ok, false} ->
	    eva_sup:start_link_log({"default", 24*3600});
	_ ->
	    %% When we use snmp, we'll have to specify where the SNMP mgr
	    %% can create eva logs (EvaLogDir), and how large the logs in
	    %% that dir can be (120K).
	    eva_sup:start_link_log_snmp({"default", 24*3600}, EvaLogDir, 120000)
    end.

stop(State) ->
    ok.

create() ->
    application:load(oms),
    file:make_dir("ex_log"),
    case application:get_env(oms, use_snmp) of
	{ok, false} ->
	    eva_sup:create_tables_log([node()]);
	_ ->
	    eva_sup:create_tables_log_snmp([node()])
    end.

