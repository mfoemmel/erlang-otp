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
%%%----------------------------------------------------------------------
%%% Purpose : Implements the administrative server
%%%----------------------------------------------------------------------

-module(jive_server).

-behaviour(gen_server).

-export([start_server/0]).

%% Behaviour interface
-export([handle_call/3,
	 handle_info/2,  
	 init/1,
	 code_change/3,
	 handle_cast/2,
	 terminate/2]).

%%---------------------------------------------------------------------------
%% This module implements the administrative server for Jive. It holds
%% the mapping between Erlang process id's and integers used in Java
%% to identify the Erlang process. It also holds access rights, i.e.
%% what Erlang functions are allowed to be executed from Java.
%%---------------------------------------------------------------------------

%%
%% The 'State' variable is bound to a tuple with three elements
%%
%%     State      = {AllowedDB,JpidDB,NextJpid}
%%     AllowedDB  = all | [AllowedMFA]
%%     AllowedMFA = {Module, Function, Arity}
%%     Module     = atom()
%%     Function   = atom()
%%     Arity      = integer()
%%     JpidDB     = [] | [{Jpid,pid()}]
%%     Jpid       = integer()
%%     NextJpid   = integer()
%%
%% If you change this internal structure you have to define a way
%% to translate the old format to the new one.


%%    
%% Spawn the server. This is the call made by the supervisor to start
%% this gen_server
%%
start_server() ->
    gen_server:start_link({local,jive_server}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit,true),
    Allowed = get_default_allowed(),
    State = {Allowed, [], 0},
    {ok, State}.


%%
%% Register a PID. Don't return an error if it is already registered.
%%
handle_call({set,Pid}, From, State) ->		% Know it is a PID
    %% Note that whe don't check if the Pid is a running process
    {Allowed, Pids, NextJpid} = State,
    case lists:keysearch(Pid, 2, Pids) of 
	{value, {Jpid, Pid}} ->
	    {reply, {pidid, Jpid}, State};
	false ->
	    NewState = {Allowed, [{NextJpid,Pid} | Pids], NextJpid+1},
	    {reply, {pidid, NextJpid}, NewState}
    end;

%%
%% Check if a PID is registered
%%
handle_call({get,Jpid}, From, State) ->
    {Allowed, Pids, NextJpid} = State,
    case lists:keysearch(Jpid, 1, Pids) of 
	{value, {Jpid, Pid}} ->
	    {reply, Pid, State};
	false ->
	    {reply, error, State}
    end;

%%
%% Check if a function as allowed
%%
handle_call({check_allowed,M,F,A}, From, State) ->
    MFA = {M, F, A},
    case MFA of
	{M,F,A} when atom(M), atom(F), integer(A) ->
	    case State of	
		{all, Pids, NextJpid} ->
		    {reply, ok, State};
		{Allowed, Pids, NextJpid} ->
		    case lists:member(MFA, Allowed) of 
			true ->
			    {reply, ok, State};
			false ->
			    {reply, error, State}
		    end
	    end;
	_ ->
	    {reply, error, State}
    end.

%%
%% Unregister process
%%
handle_cast({remove,Pid}, State) ->		% Know it is a PID
    unlink(Pid),
    {noreply, state_unregister(Pid, State)};

%%
%% Allow all to connect. This makes it inpossible to register or
%% unregister functions later on.
%%
handle_cast({allow,all}, State) ->
    {Allowed, Pids, NextJpid} = State,
    {noreply, {all, Pids, NextJpid}};

%%
%% Allow function for execution
%%
handle_cast({allow, MFA}, State) ->		% Whe know it has right type
    case State of
	{all, Pids, NextJpid} ->		% No change, we accept all
	    {noreply, State};
	{Allowed, Pids, NextJpid} ->
	    case lists:member(MFA, Allowed) of 
		true ->			% Already in db
		    {noreply,State};
		false ->		% Add new entry
		    NewState = {[MFA | Allowed], Pids, NextJpid},
		    {noreply,NewState}
	    end
    end;

%%
%% Terminate server
%%
handle_cast(shutdown, State) ->
    {stop, normal, State};

%%
%% For debugging
%%
handle_cast(dump_state, State) ->
    do_dump_state(State),
    {noreply, State}.


handle_info({'EXIT',From,Reason}, State) ->
    % A process we seem to be linked to has terminated,
    % check if it is in our list of registered Jpid's
    % and remove if it is there.
    {noreply, state_unregister(From, State)};

handle_info(What, State) ->
    {noreply, State}.

terminate(Reason, State) ->			% normal, shurdown, Any
    % XXX: Cleanup here?
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%
%% Unregister a PID, don't complain if it was not in DB
%%
state_unregister(Pid, State) ->
    {Allowed, Pids, NextJpid} = State,
    NewPids = lists:keydelete(Pid,2,Pids),
    {Allowed, NewPids, NextJpid}.

%%
%% Support old way of getting allowed functions from 'args'
%%
get_default_allowed() -> 
    case application:get_env(jive, allowed) of
	{ok, List} -> List;
	_ ->
	    case application:get_env(jive, args) of
		{ok, [Port, Allowed]} -> Allowed;
		_ -> []
	    end
    end.

%%---------------------------------------------------------------------------
%%
%% For debugging
%%
do_dump_state(State) ->
    {Allowed, Pids, NextJpid} = State,
    io:format("Allowed  : ~n~p~n~n",[Allowed]),
    io:format("Jpids    : ~n~p~n~n",[Pids]),
    io:format("NextJpid : ~n~p~n~n",[NextJpid]).

%%--------------------------------EOF----------------------------------------
