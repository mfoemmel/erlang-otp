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
-module(mnesia_corba_connector_impl).

%%%----------------------------------------------------------------------
%%% Purpose : Callback module which handles a corba connector
%%%----------------------------------------------------------------------

%%-compile(export_all).
-export([init/1, terminate/2, 
	 connect/1, disconnect/2]).

-record(state, {debug}).
-define(SUPERVISOR, mnesia_corba_session_sup).

-define(DBG(F, A, D), mnesia_session_lib:debug(F, A, D)).
-define(VERBOSE(F, A, D), mnesia_session_lib:verbose(F, A, D)).

init([]) ->
    State = #state{debug = mnesia_session_lib:get_env(debug)},
    {ok,State}.

terminate(Reason, State) -> 
    ok.

connect(State) ->
    Res = (catch mnesia_corba_session:oe_create_link(corba_session, [{pseudo,true}])),
    ?DBG("connect() -> ~p~n", [Res], State#state.debug),
    case Res of
	{'EXCEPTION', Reason} ->
	    Error = {corba_session_start_exception, Reason},
	    ?VERBOSE("~p~n", [Error], State#state.debug),
	    {reply, {error, Error}, State};
	{'EXIT', Reason} ->
	    Error = {corba_session_start_exit, Reason},
	    ?VERBOSE("~p~n", [Error], State#state.debug),
	    {reply, {error, Error}, State};
	SessionObjectKey ->
	    {reply, SessionObjectKey, State}
    end.

disconnect(State, SessionObjectkey) ->
    ?DBG("disconnect(~p)~n", [SessionObjectkey], State#state.debug),
    Res = corba:dispose(SessionObjectkey),
    {reply, Res, State}.


