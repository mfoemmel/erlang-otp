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
%% File: orber_iiop_spm.erl
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains the mapping of addresses on the format {Host, Port} 
%%    to a proxy pid.
%%
%% Creation date: 990615
%%
%%-----------------------------------------------------------------
-module(orber_iiop_pm).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([connect/4, disconnect/2, 
	 init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/0]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    ignore.
start(Opts) ->
    gen_server:start_link({local, 'orber_iiop_pm'}, ?MODULE, Opts, []).


connect(Host, Data, SocketType, SocketOptions) ->
    gen_server:call(orber_iiop_pm, {connect, Host, Data, SocketType, SocketOptions}).

disconnect(Host, Port) ->
    gen_server:call(orber_iiop_pm, {disconnect, Host, Port}).


%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/0 (Only used for test purpose !!!!!!)
%%-----------------------------------------------------------------
stop() ->
    gen_server:call(orber_iiop_pm, stop).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init(Opts) ->
    ?PRINTDEBUG2("orber_iiop_pm init: ~p ", [self()]),
    process_flag(trap_exit, true),
    {ok, ets:new(orber_iiop_pm, [set])}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(Reason, PM) ->
    %% Kill all proxies and close table before terminating
    stop_all_proxies(PM, ets:first(PM)),
    ets:delete(PM),
    ok.

stop_all_proxies(_, '$end_of_table') ->
    ok;
stop_all_proxies(PM, Key) ->
    case ets:lookup(PM, Key) of
	[] ->
	    ok;
	[P] ->
	    orber_iiop_outproxy:stop(P, infinity)
    end,
    stop_all_proxies(PM, ets:next(PM, Key)).

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({connect, Host, Data, SocketType, SocketOptions}, From, PM) ->
    Port = case SocketType of
	       normal -> Data;
	       ssl -> Data#'SSLIOP_SSL'.port
	   end,
    Proxy = case ets:lookup(PM, {Host, Port}) of
		[] ->
		    case catch orber_iiop_outsup:connect(Host, Port, SocketType, SocketOptions) of
			{'error', {'EXCEPTION', E}} ->
			    {'EXCEPTION', E};
			{'error', Reason} ->
			    {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}};
			{ok, undefined} ->
			    {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}};
			{ok, Child} ->
			    link(Child),
			    ets:insert(PM, {{Host, Port}, Child}),
			    Child
		    end;
		[{_, P}] ->
		    P
	    end,
    {reply, Proxy, PM};
handle_call({disconnect, Host, Port}, From, PM) ->
    case ets:lookup(PM, {Host, Port}) of
	[] ->
	    ok;
	[P] ->
	    unlink(P),
	    orber_iiop_outproxy:stop(P, infinity),
	    ets:delete({Host, Port})
    end,
    {reply, ok, PM};
handle_call(stop, From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% Trapping exits 
handle_info({'EXIT', Pid, normal}, PM) ->
    {K, _} = ets:match(PM, {'$1', Pid}),
    ets:delete(PM, K),
    {noreply, PM};
handle_info({'EXIT', Pid, Reason}, PM) ->
    ?PRINTDEBUG2("proxy ~p finished with reason ~p", [Pid, Reason]),
    {K, _} = ets:match(PM, {'$1', Pid}),
    ets:delete(PM, K),
    {noreply, PM};
handle_info(X, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.



