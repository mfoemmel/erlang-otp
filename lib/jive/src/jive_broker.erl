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
%%% Purpose : Wait for connections and start workers
%%%----------------------------------------------------------------------

-module(jive_broker).

-export([start/0,
	 start/1,
	 init/2,
	 system_continue/3,
	 system_terminate/4,
         system_code_change/4,
	 write_debug/3]).

-include("jive.hrl").

%%---------------------------------------------------------------------------
%%
%% NOTE: If you change the format of the State variable you have to change
%% the function system_code_change/4 to update the format.
%%
%% This module never get any messages except system messages. It is
%% waiting in gen_tcp:accept() most of the time for new connections
%% and when it get a connection it starts a worker under the
%% 'jive_worker_sup' supervisor. We use the timeout option to
%% gen_tcp:accept() to get back to the loop from time to time to
%% be able to get system messages.
%%
%%
%% ALTERNATIVE SOLUTION NOT USED:
%% In this process we use an unusual way of handling code change, we simply
%% ignore all system messages. This makes the code change handling give up
%% and kill this process. This is fine because this process don't keep any
%% information it can't get while restarted and it doesn't keep any
%% information about the connections set up. The only risk this approach take
%% is that someone else steal the port we listened to while we are gone.
%% 
%%---------------------------------------------------------------------------

start() ->
    start([]).

start(Options) ->				% XXX: When any Options?
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Options]),
    {ok, Pid}.

init(Parent, Options) ->
    Deb = sys:debug_options(?DBG_OPTS ++ Options),
    Port = get_port(Options),
    AcceptTimeout = 
	case application:get_env(jive, accept_timeout) of
	    {ok, AcceptT} ->
		AcceptT;
	    undefined ->
		1000				% XXX: Create a macro!!!
	end,

    % We crash if we can't open the socket to listen
    {ok, ListenSocket} = gen_tcp:listen(Port, [{packet,4},{reuseaddr,true}]),

    loop(Parent, Deb, ListenSocket, AcceptTimeout).

loop(Parent, Deb, ListenSocket, AcceptTimeout) ->
    receive
	{system, From, Request} ->
	    State = {ListenSocket, AcceptTimeout},
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	What ->
	    NewDeb = sys:handle_debug(Deb, {?MODULE, write_debug}, jive_broker,
				      {in, What}),
	    loop(Parent, NewDeb, ListenSocket, AcceptTimeout)
    after 0 ->
	    case gen_tcp:accept(ListenSocket, AcceptTimeout) of
		{ok, Sock} ->
		    case supervisor:start_child(jive_worker_sup, [Sock]) of
			{ok,Pid} when pid(Pid) ->
			    gen_tcp:controlling_process(Sock, Pid),
			    loop(Parent, Deb, ListenSocket, AcceptTimeout);
			Other ->
			    NewDeb = sys:handle_debug(Deb,
						      {?MODULE, write_debug},
						      jive_broker,
						      {start_child_result,
						       Other}),
			    gen_tcp:close(Sock),
			    loop(Parent, NewDeb, ListenSocket, AcceptTimeout)
		    end;
		{error, timeout} ->
		    %% This is what we want to enable system events
		    loop(Parent, Deb, ListenSocket, AcceptTimeout);
		Other ->
		    NewDeb = sys:handle_debug(Deb,
					      {?MODULE, write_debug},
					      jive_broker,
					      {accept_result,Other}),
		    loop(Parent, NewDeb, ListenSocket, AcceptTimeout)
	    end
    end.


%%
%% In all cases we close the listen socket
%%
system_terminate(Reason, Parent, Deb, {ListenSocket, AcceptTimeout}) ->
    gen_tcp:close(ListenSocket),		% XXX: Needed?
    exit(Reason).

%%
%% XXX: What to do for real? Error logger?
%%
write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

%%---------------------------------------------------------------------------

%%
%% system_continue/3 has to be changed if the State variable change format.
%%
system_continue(Parent, Deb, State) ->
    {ListenSocket, AcceptTimeout} = State,
    loop(Parent, Deb, ListenSocket, AcceptTimeout).

%%
%% This is where we update the state/loop variable if needed. Right now
%% we are the first version that support code update so we pass
%% the data on unmodified.
%%
system_code_change(Misc, Module, OldVsn, Extra) ->
    {ok, Misc}.

%%
%% Read application option 'args'
%%
get_port([{port,Port} | Options]) ->
    Port;
get_port([_ | Options]) ->
    get_port(Options);
get_port([]) ->
    case application:get_env(jive, args) of
	{ok, [Port|_]} -> Port;
	undefined ->
	    case application:get_env(jive, listen_port) of
		{ok, Port} -> 
		    Port;
		undefined ->
		    ?DEFAULT_PORT
	    end
    end.

