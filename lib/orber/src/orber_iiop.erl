%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: orber_iiop.erl
%% Description:
%%    This file contains the interface to the iiop operations
%%
%% Creation date: 970115
%%
%%-----------------------------------------------------------------
-module(orber_iiop).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/orber_debug.hrl").

-behaviour(supervisor).
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start_sup/1, request/5, request/6, locate/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, handle_call/3]).

%%-----------------------------------------------------------------
%% Server state record
%%-----------------------------------------------------------------
-record(state, {db=[]}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_sup/1
%%-----------------------------------------------------------------
start_sup(Opts) ->
    supervisor:start_link({local, orber_iiop_sup}, ?MODULE,
			  {orber_iiop_sup, Opts}).

%%%-----------------------------------------------------------------
%%% Func: connect/1
%%%-----------------------------------------------------------------
%connect(OrbName) ->
%    orber_iiop_net:connect(OrbName).

%%%-----------------------------------------------------------------
%%% Func: request/5
%%%-----------------------------------------------------------------
request(ObjData, Op, Parameters, TypeCodes, ResponseExpected) ->
    request(ObjData, Op, Parameters, TypeCodes, ResponseExpected, infinity).

request({SocketType, Host, IIOP_port, ObjKey}, Op, Parameters, TypeCodes, 
	ResponseExpected, Timeout) ->
    request({SocketType, Host, IIOP_port, ObjKey, orber:giop_version()}, Op, 
	    Parameters, TypeCodes, ResponseExpected, Timeout);

request({SocketType, Host, IIOP_port, ObjKey, Version}, Op, Parameters, TypeCodes, 
	ResponseExpected, Timeout) ->
    SocketOptions = case SocketType of
			normal ->
			    [];
			ssl ->
			    [{certfile, orber:ssl_client_certfile()},
			     {verify, orber:ssl_client_verify()},
			     {depth, orber:ssl_client_depth()}] ++ ssl_client_cacertfile_option()
		    end,
    Proxy = case orber_iiop_pm:connect(Host, IIOP_port, SocketType, SocketOptions) of
		{'EXCEPTION', E} ->
		    corba:raise(E);
		X ->
		    X
	    end,
    orber_iiop_outproxy:request(Proxy, ObjKey, Op, Parameters, TypeCodes, 
				ResponseExpected, Timeout, Version).

%%-----------------------------------------------------------------
%% Func: locate/1
%%-----------------------------------------------------------------
locate({SocketType, Host, IIOP_port, ObjKey}) ->
    locate({SocketType, Host, IIOP_port, ObjKey, orber:giop_version()});
locate({SocketType, Host, IIOP_port, ObjKey, Version}) ->
    SocketOptions = case SocketType of
			normal ->
			    [];
			ssl ->
			    [{certfile, orber:ssl_client_certfile()},
			     {verify, orber:ssl_client_verify()},
			     {depth, orber:ssl_client_depth()}] ++ ssl_client_cacertfile_option()
		    end,
    Proxy = case orber_iiop_pm:connect(Host, IIOP_port, SocketType, SocketOptions) of
		{'EXCEPTION', E} ->
		    corba:raise(E);
		X ->
		    X
	    end,
    orber_iiop_outproxy:locate(Proxy, ObjKey, Version).


ssl_client_cacertfile_option() ->
    case orber:ssl_client_cacertfile() of
	[] ->
	    [];
	X when list(X) ->
	    {cacertfile, X};
	_ ->
	    []
    end.

%%%-----------------------------------------------------------------
%%% Func: cancel/1
%%%-----------------------------------------------------------------
%cancel(X) ->
%	ok.

%%%-----------------------------------------------------------------
%%% Func: message_error/1
%%%-----------------------------------------------------------------
%message_error(X) ->
%	ok.

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({orber_iiop_sup, Opts}) ->
    ?PRINTDEBUG("init iiop supervisor"),
    IIOP_port      =  orber:iiop_port(),
    Bootstrap_port =  orber:bootstrap_port(),
    SSL_port       =  orber:iiop_ssl_port(),
    SupFlags       = {one_for_one, 5, 1000},	%Max 5 restarts in 1 second
    PortList = if
		   SSL_port > 0 ->
		       [{port, ssl, SSL_port}];
		   true ->
		       []
		 end,
    ChildSpec = 
	case orber:is_lightweight() of
	    true ->
		[
		 {orber_iiop_outsup, {orber_iiop_outsup, start,
				      [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_outsup]},
		 {orber_iiop_pm, {orber_iiop_pm, start,
				  [Opts]},
		  permanent, 10000, worker, [orber_iiop_pm]}
		];
	    false ->
		ChildSpec1 = 
		    if
			Bootstrap_port == IIOP_port ->
			    [{orber_iiop_net, {orber_iiop_net, start,
					       [[{port, normal, IIOP_port} | PortList]]},
			      permanent, 10000, worker, [orber_iiop_net]}];
			Bootstrap_port < 1024 ->
%%%		     Bootstrap_port < 2024 -> % Used for testing without being root
			    [{orber_iiop_net, {orber_iiop_net, start,
					       [[{port, IIOP_port}| PortList]]},
			      permanent, 10000, worker, [orber_iiop_net]},
			     {orber_bootstrap, {orber_bootstrap,
						start, [{port, normal, Bootstrap_port}]}, permanent, 
			      10000, worker, [orber_bootstrap]}];
			true ->
			    [{orber_iiop_net, {orber_iiop_net, start,
					       [[{port, normal, IIOP_port},
						 {port, normal, Bootstrap_port}| PortList]]},
			      permanent, 10000, worker, [orber_iiop_net]}]
		    end,
		[
		 {orber_iiop_outsup, {orber_iiop_outsup, start,
				      [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_outsup]},
		 {orber_iiop_pm, {orber_iiop_pm, start,
				  [Opts]},
		  permanent, 10000, worker, [orber_iiop_pm]},
		 {orber_iiop_insup, {orber_iiop_insup, start,
				     [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_insup]},
		 {orber_iiop_socketsup, {orber_iiop_socketsup, start,
					 [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_socketsup]} | 
		 ChildSpec1
		]
	end,
    {ok, {SupFlags, ChildSpec}}.





%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(Reason, State) ->
    ?PRINTDEBUG2("iiop supervisor terminated with reason: ~p", [Reason]),
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call(Req, From, State) ->
    {reply, ok, State}.


