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

-module(ssh_crypto_server).

-behaviour(gen_server).

-export([
	 start_link/0,
	 port_control/2
	]).

-export([
         code_change/3,
	 handle_call/3,
         handle_cast/2,
         handle_info/2,  
         init/1,
         terminate/2
	]).

-define(SSH_CRYPTO_DRV, "ssh_crypto_drv").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

port_control(Cmd, Data) ->
    gen_server:call(?MODULE, {port_control, Cmd, Data}).

%%% gen_server callbacks

code_change(OldVsn, State, Extra) ->
    {ok, State}.

handle_call({port_control, Cmd, Data}, From, Port) ->
    {reply, erlang:port_control(Port, Cmd, Data), Port};
handle_call(Call, From, Port) ->
    {reply, {error, unknown}, Port}.

handle_cast(Cast, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_exited, Reason}, Port};
handle_info(Info, State) ->
    {noreply, State}.

init(Params) ->
    process_flag(trap_exit, true),
    erl_ddll:start(),
    PrivDir = code:priv_dir(ssh),
    LibDir = filename:join([PrivDir, "lib"]),
    erl_ddll:load_driver(LibDir, ?SSH_CRYPTO_DRV),
    Port = open_port({spawn, ?SSH_CRYPTO_DRV}, []), 
    T = ets:new(ssh_crypto_server_table, [set, protected, named_table]),
    ets:insert(T, {port, Port}),
    {ok, Port}.

terminate({port_exited,_},_) ->
    ok;
terminate(Reason, Port) ->
    erlang:port_close(Port),
    ok.
