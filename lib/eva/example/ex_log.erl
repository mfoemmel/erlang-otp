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
-module(ex_log).
-compile(export_all).

init() ->
    file:make_dir("ex_log").

%S1
start() ->
    disk_log:open([{name, "ex_log"},
		   {file, "ex_log/ex_log.LOG"},
		   {type, wrap},
		   {size, {10000, 4}}]),
    log:open("ex_log", ex_log_type, 3600).

test() ->
    %% Log an item
    disk_log:log("ex_log", {1, "log this"}),

    %% Set the administrative status of the log to 'down'
    log:set_admin_status("ex_log", down),
    
    %% Try  to log - this one won't be logged
    disk_log:log("ex_log", {2, "won't be logged"}),
    
    Logs1 = log:get_logs(),

    %% Set the administrative status of the log to 'up'
    log:set_admin_status("ex_log", up),
    
    %% Log an item
    disk_log:log("ex_log", {3, "log this"}),

    Logged = disk_log:chunk("ex_log", start),
    {Logs1, Logged}.
%S1
