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
-module(os_mon).

-behaviour(supervisor).

%% External exports
-export([start/2, stop/1]).

%% Internal exports
-export([init/1]).

%%%-----------------------------------------------------------------
%%% This module implements the application OS Monitor,
%%% and a supervisor for the application.
%%%-----------------------------------------------------------------
start(_, _) ->
    supervisor:start_link({local, os_mon_sup}, os_mon, []).

stop(_) -> ok.

init([]) ->
    {SupFlags,
     SysInfo} = case os:type() of
		  {win32,_} ->
		      {{one_for_one,5,3600},
		       start_sysinfo()};
		  _ ->
		      {{one_for_one,4,3600},
		       []}
		end,		      
    {DiskSup, 
     MemSup, 
     CpuSup, 
     OsSup} = case os:type() of
		  vxworks -> {[], 
			      start_memsup(), 
			      [], 
			      []};
		  {win32,_} ->
	               {start_disksup(),
			start_memsup(),
			[],
			[]};
		  _ -> {start_disksup(),
			start_memsup(),
			start_cpu_sup(),
			start_os_sup()}
	      end,
    {ok, {SupFlags, SysInfo ++ DiskSup ++ MemSup ++ CpuSup ++ OsSup}}.

start_sysinfo() ->
	    [{os_mon_sysinfo,
	       {os_mon_sysinfo, start_link, []},
	       permanent, 2000, worker, [os_mon_sysinfo]}].


start_disksup() ->
    case application:get_env(os_mon, start_disksup) of
	{ok, true} ->
	    [{disksup,
	       {disksup, start_link, []},
	       permanent, 2000, worker, [disksup]}];
	_ ->
	    []
    end.

start_memsup() ->
    case application:get_env(os_mon, start_memsup) of
	{ok, true} ->
	    [{memsup,
	      {memsup, start_link, []},
	      permanent, 2000, worker, [memsup]}];
	_ ->
	    []
    end.

start_cpu_sup() ->
    case application:get_env(os_mon, start_cpu_sup) of
	{ok, true} ->
	    [{cpu_sup,
	      {cpu_sup, start_link, []},
	      permanent, 2000, worker, [cpu_sup]}];
	_ ->
	    []
    end.

start_os_sup() ->
    case application:get_env(os_mon, start_os_sup) of
	{ok, true} ->
	    [{os_sup,
	       {os_sup, start_link, []},
	       permanent, 10000, worker, [os_sup]}];
	_ ->
	    []
    end.

