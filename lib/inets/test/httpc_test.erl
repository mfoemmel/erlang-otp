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
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the inets application
%%----------------------------------------------------------------------
-module(httpc_test).

-compile(export_all).

-include("inets_test_lib.hrl").


%% These are JUST temporary
-ifndef(NOPROXY_HOST).
-define(NOPROXY_HOST, "otp.ericsson.se").
-define(NOPROXY_PORT, "8000").
-endif.

-ifndef(PROXY_HOST).
-define(PROXY_HOST, "www.erlang.org").
-define(PROXY_PORT, undefined).
-endif.


% t()     -> megaco_test_lib:t(?MODULE).
% t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    Timeout = inets_test_lib:get_config(tc_timeout, Config, ?SECS(10)),
    Pid = ?WD_START(Timeout),
    [{watchdog, Pid}|Config].

fin_per_testcase(Case, Config) ->
    case ?CONFIG(watchdog, Config) of
	Pid when pid(Pid) ->
	    ?WD_STOP(Pid);
	_ ->
	    ok
    end,
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    [
     ip_comm, ssl
    ].


ip_comm(suite) ->
    Cases = 
	[
	 ip_noproxy,
	 ip_proxy
	].

ip_noproxy(suite) ->
    Cases = 
	[
	 ip_noproxy_simple_op,
	 ip_noproxy_get,
	 ip_noproxy_head,
	 ip_noproxy_post
	],
    ?EXPANDABLE(init_noproxy_ip, Cases, stop_noproxy_ip).

ip_proxy(suite) ->
    Cases = 
	[
	 ip_proxy_simple_op,
	 ip_proxy_get,
	 ip_proxy_head,
	 ip_proxy_post
	],
    ?EXPANDABLE(init_proxy_ip, Cases, stop_proxy_ip).


init_noproxy_ip(suite) -> [];
init_noproxy_ip(doc) -> [];
init_noproxy_ip(Config) when list(Config) ->
    %% Here we should propably start the server...
    [{type, ip_comm}, {host, ?NOPROXY_HOST}, {port, ?NOPROXY_PORT}|Config].

stop_noproxy_ip(suite) -> [];
stop_noproxy_ip(doc) -> [];
stop_noproxy_ip(Config) when list(Config) ->
    Config.

ip_noproxy_simple_op(suite) -> [];
ip_noproxy_simple_op(doc) ->
    ["Perform the operations GET, POST, ... "
     "with only the needed options (simple)"];
ip_noproxy_simple_op(Config) -> 
    simple_op(Config).

ip_noproxy_get(suite) -> [];
ip_noproxy_get(doc) ->
    ["Perform the GET operation, excercising all the options"];
ip_noproxy_get(Config) -> 
    ?SKIP(not_yet_implemented).

ip_noproxy_head(suite) -> [];
ip_noproxy_head(doc) ->
    ["Perform the HEAD operation, excercising all the options"];
ip_noproxy_head(Config) -> 
    ?SKIP(not_yet_implemented).

ip_noproxy_post(suite) -> [];
ip_noproxy_post(doc) ->
    ["Perform the POST operation, excercising all the options"];
ip_noproxy_post(Config) -> 
    ?SKIP(not_yet_implemented).


init_proxy_ip(suite) -> [];
init_proxy_ip(doc) -> [];
init_proxy_ip(Config) when list(Config) ->
    %% Here we should propably start the server...
    [{type, ip_comm}, {host, ?PROXY_HOST}, {port, ?PROXY_PORT},
     {settings, [{http_useproxy,true},{http_proxy,{"proxy",82}}]}|Config].

stop_proxy_ip(suite) -> [];
stop_proxy_ip(doc) -> [];
stop_proxy_ip(Config) when list(Config) ->
    Config.

ip_proxy_simple_op(suite) -> [];
ip_proxy_simple_op(doc) ->
    ["Perform the operations GET, POST, ... "
     "with only the needed options (simple)"];
ip_proxy_simple_op(Config) -> 
    simple_op(Config).

ip_proxy_get(suite) -> [];
ip_proxy_get(doc) ->
    ["Perform the GET operation, excercising all the options"];
ip_proxy_get(Config) -> 
    ?SKIP(not_yet_implemented).

ip_proxy_head(suite) -> [];
ip_proxy_head(doc) ->
    ["Perform the HEAD operation, excercising all the options"];
ip_proxy_head(Config) -> 
    ?SKIP(not_yet_implemented).

ip_proxy_post(suite) -> [];
ip_proxy_post(doc) ->
    ["Perform the POST operation, excercising all the options"];
ip_proxy_post(Config) -> 
    ?SKIP(not_yet_implemented).



ssl(suite) ->
    Cases = 
	[
	 ssl_noproxy,
	 ssl_proxy
	].

ssl_noproxy(suite) ->
    Cases = 
	[
	 ssl_noproxy_simple_op,
	 ssl_noproxy_get,
	 ssl_noproxy_head,
	 ssl_noproxy_post
	],
    ?EXPANDABLE(init_noproxy_ssl, Cases, stop_noproxy_ssl).

ssl_proxy(suite) ->
    Cases = 
	[
	 ssl_proxy_simple_op,
	 ssl_proxy_get,
	 ssl_proxy_head,
	 ssl_proxy_post
	],
    ?EXPANDABLE(init_proxy_ssl, Cases, stop_proxy_ssl).


init_noproxy_ssl(suite) -> [];
init_noproxy_ssl(doc) -> [];
init_noproxy_ssl(Config) when list(Config) ->
    %% Here we should propably start the server...
    [{type, ssl}, {host, ?NOPROXY_HOST}, {port, ?NOPROXY_PORT}|Config].

stop_noproxy_ssl(suite) -> [];
stop_noproxy_ssl(doc) -> [];
stop_noproxy_ssl(Config) when list(Config) ->
    Config.

ssl_noproxy_simple_op(suite) -> [];
ssl_noproxy_simple_op(doc) ->
    ["Perform the operations GET, POST, ... "
     "with only the needed options (simple)"];
ssl_noproxy_simple_op(Config) -> 
    simple_op(Config).

ssl_noproxy_get(suite) -> [];
ssl_noproxy_get(doc) ->
    ["Perform the GET operation, excercising all the options"];
ssl_noproxy_get(Config) -> 
    ?SKIP(not_yet_implemented).

ssl_noproxy_head(suite) -> [];
ssl_noproxy_head(doc) ->
    ["Perform the HEAD operation, excercising all the options"];
ssl_noproxy_head(Config) -> 
    ?SKIP(not_yet_implemented).

ssl_noproxy_post(suite) -> [];
ssl_noproxy_post(doc) ->
    ["Perform the POST operation, excercising all the options"];
ssl_noproxy_post(Config) -> 
    ?SKIP(not_yet_implemented).


init_proxy_ssl(suite) -> [];
init_proxy_ssl(doc) -> [];
init_proxy_ssl(Config) when list(Config) ->
    %% Here we should propably start the server...
    [{type, ssl}, {host, ?PROXY_HOST}, {port, ?PROXY_PORT},
     {settings, [{http_useproxy,true},{http_proxy,{"proxy",82}}]}|Config].

stop_proxy_ssl(suite) -> [];
stop_proxy_ssl(doc) -> [];
stop_proxy_ssl(Config) when list(Config) ->
    Config.

ssl_proxy_simple_op(suite) -> [];
ssl_proxy_simple_op(doc) ->
    ["Perform the operations GET, POST, ... "
     "with only the needed options (simple)"];
ssl_proxy_simple_op(Config) -> 
    simple_op(Config).

ssl_proxy_get(suite) -> [];
ssl_proxy_get(doc) ->
    ["Perform the GET operation, excercising all the options"];
ssl_proxy_get(Config) -> 
    ?SKIP(not_yet_implemented).

ssl_proxy_head(suite) -> [];
ssl_proxy_head(doc) ->
    ["Perform the HEAD operation, excercising all the options"];
ssl_proxy_head(Config) -> 
    ?SKIP(not_yet_implemented).

ssl_proxy_post(suite) -> [];
ssl_proxy_post(doc) ->
    ["Perform the POST operation, excercising all the options"];
ssl_proxy_post(Config) -> 
    ?SKIP(not_yet_implemented).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_op(suite) -> [];
simple_op(doc) -> [];
simple_op(Config) -> 
    ?SKIP(not_yet_implemented),
    URL = url("/", Config),
    Settings = inets_test_lib:get_config(settings, Config, []),
    io:format("simple_op -> "
	      "~nURL:      ~p"
	      "~nSettings: ~p~n", [URL, Settings]),
    http:request_sync(get, {URL, []}, Settings).


url(Uri, Config) ->
    Host = ?CONFIG(host, Config),
    case inets_test_lib:get_config(type, Config, ip_comm) of
	ip_comm ->
	    url1("http://", Host, Uri, Config);
	ssl ->
	    url1("https://", Host, Uri, Config)
    end.
    
url1(Prot, Host, Uri, Config) ->
    case ?CONFIG(port, Config) of
	undefined ->
	    Prot ++ Host ++ Uri;
	Port when list(Port) ->
	    Prot ++ Host ++ ":" ++ Port ++ Uri
    end.
    


