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
%%-----------------------------------------------------------------
%% Purpose: Supervisor
%%-----------------------------------------------------------------
-module(megaco_tcp_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/0, 
	 start_accept_child/2
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1, 
	 terminate/2
	]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Start an megaco net element supervisor
%%-----------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).
%%-----------------------------------------------------------------
%% Func: start_accept_child/1
%% Description: Starts the process that keeps track of the TCP 
%%              accept processes
%%-----------------------------------------------------------------
start_accept_child(SupPid, Data) ->
    case supervisor:start_child(SupPid, 
				{megaco_tcp_accept, 
				 {megaco_tcp_accept, start_link, [Data]},  
				 temporary, 10000, worker, 
				 [megaco_tcp_accept]}) of
	{ok, ChildPid} ->
	    ChildPid;
	{error, Reason} ->
	    {error, Reason}
    end.
%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the supervisor
%%-----------------------------------------------------------------
init([]) ->
    SupFlags = {one_for_one, 5, 1000},	% Max 5 restarts in 1 second
    ChildSpec = [sup_spec(megaco_tcp_accept_sup, []),
		 sup_spec(megaco_tcp_connection_sup,[]),
		 worker_spec(megaco_tcp, [{self(), []}], [gen_server])],
    {ok, {SupFlags, ChildSpec}}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%% Description: Termination function for the supervisor
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%-----------------------------------------------------------------
%% Local functions
%%-----------------------------------------------------------------

sup_spec(Name, Args) ->
    {Name, 
     {Name, start_link, Args}, 
     permanent, 10000, supervisor, [Name, supervisor]}.

worker_spec(Name, Args, Mods) ->
    {Name, {Name, start_link, Args}, 
     permanent, 10000, worker, [Name] ++ Mods}.

