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
-module(mnesia_connector_impl).

%%%----------------------------------------------------------------------
%%% Purpose : Callback module which handles a connector
%%%----------------------------------------------------------------------

%%-compile(export_all).
-export([init/1, code_change/3, terminate/2]).
-export([connect/1, disconnect/2]).

-record(state, {debug}).
-define(SUPERVISOR, mnesia_session_sup).

-define(DBG(F, A, D), mnesia_session_lib:debug(F, A, D)).

init([]) -> 
    {ok, #state{debug = mnesia_session_lib:get_env(debug)}}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    ok.

connect(State) ->
    Res = supervisor:start_child(?SUPERVISOR, []),
    ?DBG("connect() -> ~p~n", [Res], State#state.debug),
    case Res of
	{ok, SessionPid} ->
	    {reply, SessionPid, State};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end.

disconnect(State, SessionPid) ->
    ?DBG("disconnect(~p)~n", [SessionPid], State#state.debug),
    exit(SessionPid, shutdown),
    {reply, ok, State}.


