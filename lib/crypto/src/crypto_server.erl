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

%% Purpose: Provide cryptographic algorithms.

-module(crypto_server).

-behaviour(gen_server).

-export([start_link/0]).

%% Internal exports, call-back functions.
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,
	 terminate/2]).

%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

start_link() ->
    gen_server:start_link({local, crypto_server}, crypto_server, [], []).

init([]) ->
    process_flag(trap_exit, true),
    erl_ddll:start(),
    PrivDir = code:priv_dir(crypto),
    LibDir = filename:join([PrivDir, "lib"]),
    erl_ddll:load_driver(LibDir, crypto_drv),
    Cmd = "crypto_drv elibcrypto " ++ filename:join([LibDir, "elibcrypto"]),
    Port = open_port({spawn, Cmd}, []),
    T = ets:new(crypto_server_table, [set, protected, named_table]),
    ets:insert(T, {port, Port}),
    {ok, {Port, []}}.

%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, {Port, Libraries}) when is_pid(Pid) ->
    {noreply, {Port, Libraries}};

handle_info({'EXIT', Port, Reason}, {Port, Libraries}) when is_port(Port) ->
    {stop, {port_died, Reason}, {Port, Libraries}};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, {Port, _Libraries}) ->
    Port ! {self, close},
    ok.




    
