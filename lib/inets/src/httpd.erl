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
-export([multi_start/1, multi_start_link/1,
	 start/0,start/1,start_link/0,start_link/1,
	 start_child/0,start_child/1,
	 multi_stop/1,
	 stop/0,stop/1,stop/2,
	 stop_child/0,stop_child/1,stop_child/2,
	 multi_restart/1,
	 restart/0,restart/1,restart/2,
	 parse_query/1]).

-export([get_status/1,get_status/2,get_status/3,verbosity/3,verbosity/4]).

-include("httpd.hrl").

%% multi_start

multi_start(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mstart(ConfigFiles);
	Error ->
	    Error
    end.

mstart(ConfigFiles) ->
    mstart(ConfigFiles,[]).
mstart([],Results) ->
    {ok,lists:reverse(Results)};
mstart([H|T],Results) ->
    Res = start(H),
    mstart(T,[Res|Results]).


%% start

start() ->
    start("/var/tmp/server_root/conf/8888.conf").

start(ConfigFile) ->
    ?LOG("start -> ConfigFile = ~s",[ConfigFile]),
    case httpd_conf:load(ConfigFile) of
	{ok,ConfigList} ->
	    httpd_manager:start(ConfigFile,ConfigList);
	{error,Reason} ->
	    error_logger:error_report(Reason),
	    {stop,Reason}
    end.


%% multi_start_link

multi_start_link(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mstart_link(ConfigFiles);
	Error ->
	    Error
    end.

mstart_link(ConfigFiles) ->
    mstart_link(ConfigFiles,[]).
mstart_link([],Results) ->
    {ok,lists:reverse(Results)};
mstart_link([H|T],Results) ->
    Res = start_link(H),
    mstart_link(T,[Res|Results]).

%% start_link

start_link() ->
    start("/var/tmp/server_root/conf/8888.conf").

start_link(ConfigFile) ->
    ?LOG("start_link -> ConfigFile = ~s",[ConfigFile]),
    case httpd_conf:load(ConfigFile) of
	{ok,ConfigList} ->
	    httpd_manager:start_link(ConfigFile,ConfigList);
	{error,Reason} ->
	    {stop,Reason}
    end.

%% start_child

start_child() ->
  start_child("/var/tmp/server_root/conf/8888.conf").

start_child(ConfigFile) ->
  case httpd_conf:load(ConfigFile) of
    {ok,ConfigList} ->
      Port = httpd_util:key1search(ConfigList,port,80),
      Addr = httpd_util:key1search(ConfigList,bind_address),
      Name = make_name(Addr,Port),
      ?LOG("start_child -> Name = ~p",[Name]),
      supervisor:start_child(inets,{Name,{httpd,start,[ConfigFile]},
				    transient,brutal_kill,worker,
				    [httpd_manager]});
    {error,Reason} ->
      {stop,Reason}
  end.


%% multi_stop

multi_stop(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mstop(ConfigFiles);
	Error ->
	    Error
    end.

mstop(ConfigFiles) ->
    mstop(ConfigFiles,[]).
mstop([],Results) ->
    {ok,lists:reverse(Results)};
mstop([H|T],Results) ->
    Res = stop(H),
    mstop(T,[Res|Results]).


%% stop

stop() ->
  stop(8888).

stop(Pid) when pid(Pid) ->
    httpd_manager:stop(Pid);
stop(Port) when integer(Port) ->
    stop(undefined,Port);
stop(ConfigFile) when list(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok,ConfigList} ->
	    Port = httpd_util:key1search(ConfigList,port,80),
	    Addr = httpd_util:key1search(ConfigList,bind_address),
	    stop(Addr,Port);
	Error ->
	    Error
    end.

stop(Addr,Port) when integer(Port) ->
  Name = make_name(Addr,Port),
  ?LOG("stop -> Name = ~p",[Name]),
  case whereis(Name) of
    Pid when pid(Pid) ->
      httpd_manager:stop(Pid);
    _ ->
      not_started
  end.

%% stop_child

stop_child() ->
  stop_child(8888).

stop_child(Port) ->
    stop_child(undefined,Port).

stop_child(Addr,Port) when integer(Port) ->
  Name = make_name(Addr,Port),
  ?LOG("stop_child -> Name = ~p",[Name]),
  supervisor:terminate_child(inets,Name),
  supervisor:delete_child(inets,Name).


%% multi_restart

multi_restart(MultiConfigFile) ->
    case read_multi_file(MultiConfigFile) of
	{ok,ConfigFiles} ->
	    mrestart(ConfigFiles);
	Error ->
	    Error
    end.

mrestart(ConfigFiles) ->
    mrestart(ConfigFiles,[]).
mrestart([],Results) ->
    {ok,lists:reverse(Results)};
mrestart([H|T],Results) ->
    Res = restart(H),
    mrestart(T,[Res|Results]).


%% restart

restart() ->
  restart(8888).

restart(Port) when integer(Port) ->
    restart(undefined,Port);
restart(ConfigFile) when list(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok,ConfigList} ->
	    Port = httpd_util:key1search(ConfigList,port,80),
	    Addr = httpd_util:key1search(ConfigList,bind_address),
	    restart(Addr,Port);
	Error ->
	    Error
    end.
    

restart(Addr,Port) when integer(Port) ->
  Name = make_name(Addr,Port),
  ?LOG("restart -> Name = ~p",[Name]),
  case whereis(Name) of
    Pid when pid(Pid) ->
      httpd_manager:restart(Pid);
    _ ->
      not_started
  end.


verbosity(Port,Who,Verbosity) ->
    verbosity(undefined,Port,Who,Verbosity).

verbosity(Addr,Port,Who,Verbosity) ->
    Name = make_name(Addr,Port),
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:verbosity(Pid,Who,Verbosity);
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


%% Function:    get_status(ConfigFile)        -> Status
%%              get_status(Port)              -> Status
%%              get_status(Addr,Port)         -> Status
%%              get_status(Port,Timeout)      -> Status
%%              get_status(Addr,Port,Timeout) -> Status
%%
%% Arguments:   ConfigFile -> string()  
%%                            Configuration file from which Port and 
%%                            BindAddress will be extracted.
%%              Addr       -> {A,B,C,D} | string()
%%                            Bind Address of the http server
%%              Port       -> integer()
%%                            Port number of the http server
%%              Timeout    -> integer()
%%                            Timeout time for the call
%%
%% Returns:     Status -> list()
%%
%% Description: This function is used when the caller runs in the 
%%              same node as the http server or if calling with a 
%%              program such as erl_call (see erl_interface).
%% 
get_status(ConfigFile) when list(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok,ConfigList} ->
	    Port = httpd_util:key1search(ConfigList,port,80),
	    Addr = httpd_util:key1search(ConfigList,bind_address),
	    get_status(Addr,Port);
	Error ->
	    Error
    end;

get_status(Port) when integer(Port) ->
    get_status(undefined,Port,5000).

get_status(Port,Timeout) when integer(Port), integer(Timeout) ->
    get_status(undefined,Port,Timeout);

get_status(Addr,Port) when list(Addr), integer(Port) ->
    get_status(Addr,Port,5000).

get_status(Addr,Port,Timeout) when integer(Port) ->
    Name = make_name(Addr,Port), 
    case whereis(Name) of
	Pid when pid(Pid) ->
	    httpd_manager:get_status(Pid,Timeout);
	_ ->
	    not_started
    end.


%% make_name

make_name(Addr,Port) ->
    httpd_util:make_name("httpd",Addr,Port).


%% Multi stuff
%%

read_multi_file(File) ->
    read_mfile(file:open(File,read)).

read_mfile({ok,Fd}) ->
    read_mfile(read_line(Fd),Fd,[]);
read_mfile(Error) ->
    Error.

read_mfile(eof,_Fd,SoFar) ->
    {ok,lists:reverse(SoFar)};
read_mfile({error,Reason},_Fd,SoFar) ->
    {error,Reason};
read_mfile([$#|Comment],Fd,SoFar) ->
    read_mfile(read_line(Fd),Fd,SoFar);
read_mfile([],Fd,SoFar) ->
    read_mfile(read_line(Fd),Fd,SoFar);
read_mfile(Line,Fd,SoFar) ->
    read_mfile(read_line(Fd),Fd,[Line|SoFar]).

read_line(Fd)      -> read_line1(io:get_line(Fd,[])).
read_line1(eof)    -> eof;
read_line1(String) -> httpd_conf:clean(String).


