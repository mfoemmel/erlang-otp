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
-module(httpd).
-export([start/0,start/1,start_link/1,start_child/0,start_child/1,stop/0,
	 stop/1,stop_child/0,stop_child/1,restart/0,restart/1,parse_query/1]).

%% start

start() ->
  start("/var/tmp/server_root/conf/8888.conf").

start(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok,ConfigList} ->
	    Port = httpd_util:key1search(ConfigList,port,80),
	    Name = list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
	    gen_server:start({local,Name},
			     httpd_listener,
			     [ConfigFile, ConfigList, Port],[]);
	{error,Reason} ->
	    error_logger:error_report(Reason),
	    {stop,Reason}
    end.

%% start_link

start_link(ConfigFile) ->
  case httpd_conf:load(ConfigFile) of
    {ok,ConfigList} ->
      Port=httpd_util:key1search(ConfigList,port,80),
      Name=list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
      gen_server:start_link({local,Name},httpd_listener,[ConfigFile,ConfigList,
							 Port],[]);
    {error,Reason} ->
      {stop,Reason}
  end.

%% start_child

start_child() ->
  start_child("/var/tmp/server_root/conf/8888.conf").

start_child(ConfigFile) ->
  case httpd_conf:load(ConfigFile) of
    {ok,ConfigList} ->
      Port=httpd_util:key1search(ConfigList,port,80),
      Name=list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
      supervisor:start_child(inets,{Name,{httpd,start,
					  [ConfigFile,ConfigList,Port]},
				    transient,brutal_kill,worker,
				    [httpd_listener]});
    {error,Reason} ->
      {stop,Reason}
  end.

%% stop

stop() ->
  stop(8888).

stop(Port) when integer(Port) ->
  Name=list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
  case whereis(Name) of
    Pid when pid(Pid) ->
      httpd_listener:stop(Pid);
    _ ->
      not_started
  end;
stop(Pid) when pid(Pid) ->
    httpd_listener:stop(Pid).

%% stop_child

stop_child() ->
  stop_child(8888).

stop_child(Port) when integer(Port) ->
  Name=list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
  supervisor:terminate_child(inets,Name),
  supervisor:delete_child(inets,Name).

%% restart

restart() ->
  restart(8888).

restart(Port) when integer(Port) ->
  Name=list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
  case whereis(Name) of
    Pid when pid(Pid) ->
      httpd_listener:restart(Pid);
    _ ->
      not_started
  end.

%% parse_query

parse_query(String) ->
  {ok,SplitString}=regexp:split(String,"[&;]"),
  foreach(SplitString).

foreach([]) ->
  [];
foreach([KeyValue|Rest]) ->
  {ok,Plus2Space,_}=regexp:gsub(KeyValue,"[\+]"," "),
  case regexp:split(Plus2Space,"=") of
    {ok,[Key|Value]} ->
      [{httpd_util:decode_hex(Key),
	httpd_util:decode_hex(lists:flatten(Value))}|foreach(Rest)];
    {ok,_} ->
      foreach(Rest)
  end.


