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


-module(tv_table_owner).



-export([create/5
	]).


-export([internal_create/3,
	 start/0,
	 init/0
	]).



-define(REGISTERED_NAME, tv_table_owner).



create(mnesia, _Node, _LocalNode, _TableName, _Options) ->
    error;
create(ets, _Node, true, TabName, Options) ->
    case catch internal_create(ets, TabName, Options) of
	{TabName, Pid} when pid(Pid) ->
	    {ok, {TabName,Pid}};
	{TabNo, Pid} when pid(Pid) ->
	    {ok, {TabNo,Pid}};
	_OtherResult ->
	    error
    end;
create(ets, Node, false, TabName, Options) ->
    case catch rpc:block_call(Node, ?MODULE, internal_create, [ets, TabName, Options]) of
	{TabName, Pid} when pid(Pid) ->
	    {ok, {TabName,Pid}};
	{TabNo, Pid} when pid(Pid) ->
	    {ok, {TabNo,Pid}};
	_OtherResult ->
	    error
    end.
	




internal_create(ets, TabName, Options) ->
    ?MODULE:start(),
    ?REGISTERED_NAME ! {create, self(), ets, TabName, Options},
    receive
	{?REGISTERED_NAME, Result} ->
	    Result
    after 
	5000 ->
	    error
    end.






start() ->
    case whereis(?REGISTERED_NAME) of
	undefined ->
	    ServerPid = spawn(?MODULE, init, []),
	    case catch register(?REGISTERED_NAME, ServerPid) of
		true ->
		    ok;
		{'EXIT', _Reason} ->
		    exit(ServerPid, kill),
		    timer:sleep(500),
		    start()
	    end;
	Pid when pid(Pid) ->
	    ok
    end.








init() ->
       %% Currently no initialisations!
    loop().






loop() ->
    receive
	
	{create, Sender, ets, TabName, Options} ->
	    Sender ! {?REGISTERED_NAME, (catch ets:new(TabName, Options))},
	    loop();


	_Other ->
	    loop()

    end.
	    
