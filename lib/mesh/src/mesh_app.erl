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

-module(mesh_app).

-behaviour(application).


-export([start/2,
	 stop/1,
	 config_change/3
	]).


-include("mesh_app.hrl").



%%======================================================================
%% Function:      start
%%
%% Return Value:  {ok,Pid}        | 
%%                ignore          | 
%%                {error,Reason}
%%
%% Description:   Starts a new instance of the supervisor behaviour,
%%                i.e., starts the MESH application.
%%
%% Parameters:    
%%======================================================================

start(Type, StartArgs) ->
    case supervisor:start_link({local,mesh_sup}, mesh_sup, mesh_sup_init) of
	{ok, SupPid} ->
	    case application:get_env(mesh, snmp_adapted) of
		{ok,true} ->
		    case start_snmp_adaption() of
			{ok, Child} ->
			    {ok, SupPid};
			Other ->
			    Other
		    end;
		Other ->
		    {ok, SupPid}
	    end;
	Other ->
	    Other
    end.
	
	



%%======================================================================
%% Function:      stop
%%
%% Return Value:  ok
%%
%% Description:   Stops the MESH application globally.
%%
%% Parameters:    None.
%%======================================================================

stop(State) ->
    ok.




%%======================================================================
%% Function:      config_change
%%
%% Return Value:  ok
%%
%% Description:   Used to change the configuration. This function is called
%%                with all environment variables found in the .app-file.
%%
%% Parameters:    None.
%%======================================================================

config_change(Changed, New, Removed) ->
    handle_changed(Changed),
    handle_new(New),
    handle_removed(Removed),
    ok.
    



%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************


handle_changed([{snmp_adapted,true} | T]) ->
    start_snmp_adaption(),
    handle_changed(T);
handle_changed([{snmp_adapted,false} | T]) ->
    kill_snmp_adaption(),
    handle_changed(T);
handle_changed([H | T]) ->
    handle_changed(T);
handle_changed([]) ->
    done.



handle_new([{snmp_adapted,true} | T]) ->
       %% Only if set to true do we need to do anything,
       %% otherwise the snmp adaption isn't started!
    start_snmp_adaption(),
    handle_new(T);
handle_new([H | T]) ->
    handle_new(T);
handle_new([]) ->
    done.



handle_removed([snmp_adapted | T]) ->
       %% Just try to kill the snmp adaption, if not 
       %% present only an error tuple will be returned.
    kill_snmp_adaption(),
    handle_removed(T);
handle_removed([H | T]) ->
    handle_removed(T);
handle_removed([]) ->
    done.



start_snmp_adaption() ->
    supervisor:start_child(mesh_sup, ?SnmpAdaptionSpec).



kill_snmp_adaption() ->
    supervisor:terminate_child(mesh_sup, mesh_snmp),
    supervisor:delete_child(mesh_sup, mesh_snmp).
