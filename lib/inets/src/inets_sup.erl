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
-module(inets_sup).
-export([crock/0,start/2,stop/1,init/1]).

%% crock (Used for debugging!)

crock() ->
  application:start(sasl),
  application:start(inets).

%% start

start(Type,State) ->
  supervisor:start_link({local,inets_sup},inets_sup,[]).

%% stop

stop(State) ->
  ok.

%% init

init([]) ->
  case get_services() of
    {error,Reason} ->
      {error,Reason};
    Services ->
      SupFlags={one_for_one,10,3600},
      {ok,{SupFlags,child_spec(Services,0)}}
  end.

get_services() ->
  case catch application:get_env(inets,services) of
    {ok,Services} ->
      Services;
    _ ->
      []
  end.

child_spec([],_) ->
    [];
child_spec([{httpd,ConfigFile}|Rest],N) ->
    [{{httpd,N},{httpd,start_link,[ConfigFile]},transient,brutal_kill,worker,
      [ftp,
       httpd,
       httpd_conf,
       httpd_example,
       httpd_manager,
       httpd_listener,
       httpd_parse,
       httpd_request,
       httpd_response,
       httpd_socket,
       httpd_util,
       httpd_verbosity,
       inets_sup,
       mod_actions,
       mod_alias,
       mod_auth,
       mod_cgi,
       mod_dir,
       mod_disk_log,
       mod_esi,
       mod_get,
       mod_head,
       mod_include,
       mod_log,
       mod_auth_mnesia,
       mod_auth_plain,
       mod_auth_dets,
       mod_security]}|child_spec(Rest,N+1)].

