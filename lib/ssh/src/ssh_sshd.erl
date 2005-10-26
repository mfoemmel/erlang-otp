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
%% Description: This module uses the erlang shell and
%% ssh_cli to make an erlang sshd

-module(ssh_sshd).

%% API
-export([listen/0, listen/1, listen/2, listen/3, stop/1]).

listen() ->
    listen(22).

listen(Port) ->
    listen(Port, []).

listen(Port, Opts) ->
    listen(any, Port, Opts).

listen(Addr, Port, Opts) ->
    ssh_cli:listen({shell,start,[]}, Addr, Port, Opts).

stop(Pid) ->
    ssh_cli:stop(Pid).
