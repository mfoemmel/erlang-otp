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
%% Purpose: The top supervisor for the http server (httpd) hangs under 
%%          inets_sup.
%%----------------------------------------------------------------------

-module(httpd_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_child/2, stop_child/2]).

%% Supervisor callback
-export([init/1]).


%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(HttpdServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HttpdServices]).

start_child(ConfigFile, Verbosity) ->
    {ok, Spec} = httpd_child_spec(ConfigFile, Verbosity),
    supervisor:start_child(?MODULE, Spec).
    
stop_child(Addr, Port) ->
    Name = {httpd_instance_sup, Addr, Port},
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.
    
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([HttpdServices]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = child_spec(HttpdServices, []),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
child_spec([], Acc) ->
    Acc;
child_spec([{httpd, ConfigFile, Verbosity} | Rest], Acc) ->
    case httpd_child_spec(ConfigFile, Verbosity) of
	{ok, Spec} ->
	    child_spec(Rest, [Spec | Acc]);
	{error, Reason} ->
	    error_msg("Failed creating child spec "
		      "using ~p for reason: ~p", [ConfigFile, Reason]),
	    child_spec(Rest, Acc)
    end;
child_spec([{httpd, ConfigFile} | Rest], Acc) ->
    case httpd_child_spec(ConfigFile, []) of
	{ok, Spec} ->
	    child_spec(Rest, [Spec | Acc]);
	{error, Reason} ->
	    error_msg("Failed creating child spec "
		      "using ~p for reason: ~p", [ConfigFile, Reason]),
	    child_spec(Rest, Acc)
    end.

httpd_child_spec(ConfigFile, Verbosity) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    Port = httpd_util:key1search(ConfigList, port, 80),
	    Addr = httpd_util:key1search(ConfigList, bind_address),
	    {ok, httpd_child_spec(ConfigFile, Addr, Port, Verbosity)};
	Error ->
	    Error
    end.

httpd_child_spec(ConfigFile, Addr, Port, Verbosity) ->
    Name = {httpd_instance_sup, Addr, Port},
    StartFunc = {httpd_instance_sup, start_link, [ConfigFile, Verbosity]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpd_instance_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

error_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).
