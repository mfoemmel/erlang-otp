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
%%% Purpose : Initilize the mnesia__corba_session application in erlang
%%%----------------------------------------------------------------------

%% Start the server with:
%%  erl -sname server -orber \"domain server\"
%%  and run setup:server()

%% Start the client with:
%%  erl -sname client -orber iiop_port 5001 -orber bootstrap_port 5002 -orber domain \"client\"
%%  and run setup:client()

-module(setup).

-export([orber_install/1, start_server_apps/1, start_client_apps/1, 
	 server/0, client/0]).

server() ->
    Ns = [node()],
    orber_install(Ns),
    start_server_apps(Ns),
    
    %% Both server and client needs to register the types
    oe_person:oe_register().

client() ->
    Ns = [node()],
    orber_install(Ns),
    start_client_apps(Ns),

    %% Both server and client needs to register the types
    oe_person:oe_register().

%% Orber needs to installed
orber_install(Ns) ->
    ok = mnesia:delete_schema(Ns),
    ok = mnesia:create_schema(Ns),
    [rpc:call(Node, mnesia, start, []) || Node <- Ns],
    orber:install(Ns).

%% Start the needed applications mnesia, orber and mnesia_corba_session.
start_server_apps(Ns) ->    
    [rpc:call(Node, mnesia, start, []) || Node <- Ns],
    orber:start(),
    %% Use the application parameters, in a real appl.
    mnesia_session_lib:start([{enable_corba, true}]).

%% Start the needed applications mnesia, orber and mnesia_corba_session.
start_client_apps(Ns) ->
    [rpc:call(Node, mnesia, start, []) || Node <- Ns],
    orber:start().


