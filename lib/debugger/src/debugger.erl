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
%%% Purpose : Exported API to the DEBUGGER GUI.

-module(debugger).

-export([
	 start/0,
	 start_timeout/1,
	 q/3,
	 quick/3,
	 a_start/3,
	 a_start/4,
	 attach_menus/1
	]).



%% start/0 - starts the DEBUGGER by opening the monitor window.
%% This function is intended to be called by the end user from the Erlang 
%% shell. It will take the time it takes to start the DEBUGGER.

start() ->
    dbg_ui_mon:start_timeout(infinity).


%% start_timeont/1 - starts the DEBUGGER by opening the monitor window.
%% This function is intended as the API for programs that wants to start
%% the DEBUGGER. The Timeout parameter specifies how many milliseconds to 
%% wait before Exiting with a {startup_timeout, ?MODULE} EXIT reason.

start_timeout(Timeout) ->
    dbg_ui_mon:start_timeout(Timeout).



attach_menus(A) ->
    dbg_ui_mon:attach_menus(A).




a_start(A,B,C) ->
    dbg_ui_trace:a_start(A,B,C).
    
a_start(A,B,C,D) ->
    dbg_ui_trace:a_start(A,B,C,D).




%%
%% quick start a module
%%

quick (Module, Fun, Args) ->
    dbg_ui_mon:quick (Module, Fun, Args).


q (Module, Fun, Args) ->
    dbg_ui_mon:quick (Module, Fun, Args).

