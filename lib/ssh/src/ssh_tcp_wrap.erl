%%<copyright>
%% <year>2005-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

%%% Description: tcp listen and accept-loop

-module(ssh_tcp_wrap).

-export([spawn_server/3, server/4]).
-export([server_init/4, server_loop/3, accept_loop/4]). %% helper

-define(ACCEPT_TIMEOUT, 10000).

%% Spawn the server loop
spawn_server(Port, Opts, Fun) ->
    Pid = spawn(?MODULE, server_init, [self(),Port,Opts,Fun]),
    Ref = erlang:monitor(process, Pid),
    receive
	{'DOWN', Ref, _, _, Reason} ->
	    {error, Reason};
	{Pid, Reply} ->
	    erlang:demonitor(Ref),
	    Reply
    end.

server_init(Starter, Port, Opts, Fun) ->
    case gen_tcp:listen(Port, Opts) of
	{ok, Listen} ->
	    if Port == 0 ->
		    case inet:sockname(Listen) of
			{ok, {_,Port1}} -> 
			    Starter ! {self(), {ok,self(),Port1}},
			    server_loop(self(), Listen, Fun);
			Error ->
			    gen_tcp:close(Listen),
			    Starter ! {self(), Error}
		    end;
	       true ->
		    Starter ! {self(), {ok,self(),Port}},
		    server_loop(self(), Listen, Fun)
	    end;
	Error ->
	    error_logger:format(
	      "Failed to listen on port: ~p opts:~p: rsn:~p~n",
	      [Port, Opts,Error]),
	    Starter ! {self(), Error}
    end.


%% Run the server loop
server(Port, Opts, Fun, From) ->
    case gen_tcp:listen(Port, Opts) of
	{ok, Listen} ->
	    From ! ok,
	    server_loop(self(), Listen, Fun);
	Error ->
	    error_logger:format(
	      "Failed to listen on port: ~p opts:~p: rsn:~p~n",
	      [Port, Opts,Error]),
	    From ! Error
    end.

server_loop(User, Listen, Fun) ->
    Pid = spawn(fun() -> 
			?MODULE:accept_loop(User, erlang:monitor(process,User),
					    Listen, Fun)
		end),
    Ref = erlang:monitor(process, Pid),
    receive
	{'DOWN', Ref, _, _, _Reason} ->
	    ?MODULE:server_loop(User, Listen, Fun);
	{Pid, stop} ->
	    stopped;
	{User, stop} ->
	    stopped;
	{Pid, _Result} ->
	    erlang:demonitor(Ref),
	    ?MODULE:server_loop(User, Listen, Fun)
    end.

accept_loop(User, Ref, Listen, Fun) ->
    %% Timeout makes it possible to replace this module
    %% once every ?ACCEPT_TIMEOUT milliseconds
    case gen_tcp:accept(Listen, ?ACCEPT_TIMEOUT) of
	{ok, S} ->
	    %% poll if 'User' is still alive
	    receive
		{'DOWN', Ref, _, _, Reason} ->
		    gen_tcp:close(S),
		    exit(Reason)
	    after 0 ->
		    User ! {self(), ok},
		    Fun(S)
	    end;
	{error, timeout} ->
	    %% poll if 'User' is still alive
	    receive
		{'DOWN', Ref, _, _, Reason} ->
		    exit(Reason)
	    after 0 ->
		    ?MODULE:accept_loop(User, Ref, Listen, Fun)
	    end;
	{error, closed} ->
	    Listen ! {self(), stop};
	Error ->
	    Listen ! {self(), Error}
    end.
