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
-module(mnesia_session_sup).

-behaviour(supervisor).

-export([start/1, init/1]).

-export([start_link_session/1]).

-define(VERBOSE(F, A),
	mnesia_session_lib:verbose(F, A, mnesia_session_lib:get_env(debug))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% top supervisor callback functions

start(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, [Name]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sub supervisor callback functions

init([Name]) ->
    Flags = {simple_one_for_one, 4, 3600},
    MFA = {?MODULE, start_link_session, [Name]},
    Modules = [?MODULE, supervisor],
    KillAfter = mnesia_session_top_sup:supervisor_timeout(timer:seconds(3)),
    Workers = [{Name, MFA, temporary, KillAfter, worker, Modules}],
    {ok, {Flags, Workers}}.

start_link_session(mnesia_session_sup) ->
    case mnesia_session:oe_create_link(session) of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    Error = {session_start_error, Other},
	    ?VERBOSE("~p~n", [Error]),
	    Other
    end;
	
start_link_session(mnesia_corba_session_sup) ->
    case catch mnesia_corba_session:oe_create_link(corba_session) of
	{'EXCEPTION', Reason} ->
	    Error = {corba_session_start_exception, Reason},
	    ?VERBOSE("~p~n", [Error]),
	    {error, Error};
	{'EXIT', Reason} ->
	    Error = {corba_session_start_exit, Reason},
	    ?VERBOSE("~p~n", [Error]),
	    {error, Error};
	SessionObjectKey ->
	    Pid = corba:get_pid(SessionObjectKey),
	    Msg = {mnesia_corba_session_key, Pid, SessionObjectKey},
	    mnesia_corba_connector ! Msg,
	    {ok, Pid}
    end.
