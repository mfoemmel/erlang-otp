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
-module(inet_gethost_native).

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% api exports to be used only from inet_* modules
-export([gethostbyname/1, gethostbyname/2, gethostbyaddr/1]).

-export([test/0]).
%% internal export for server loop

%% These constants are int inet_int.hrl (remove if this file is in kernel)
-include("inet_int.hrl").
-include_lib("inet.hrl").


-define(GETHOSTBYNAME,  1).
-define(GETHOSTBYADDR,  2).

-define(REPLY_OK,    0).
-define(REPLY_ERROR, 1).

-record(state, {port}).

%%
%% Gethostbyname
%% returns {ok, HostEnt} 
%% or
%%     {error, notfound}   when not found
%%     {error, formerr}    badly formated argument
%%     {error, einval}     invalid argument or
%%                         includes internal formating error or
%%                         reply code overflow
%%
%%     {error, Reason}     Port failure etc (abnormal)
%%
%%
gethostbyname(Name) ->
    gethostbyname(Name, inet).

gethostbyname(Name, inet) when list(Name) ->
    getit(?GETHOSTBYNAME, [ ?INET_AF_INET, Name]);
gethostbyname(Name, inet6) when list(Name) ->
    getit(?GETHOSTBYNAME, [?INET_AF_INET6,Name]);
gethostbyname(Name, Type) when atom(Name) ->
    gethostbyname(atom_to_list(Name), Type);
gethostbyname(_, _)  ->
    {error, formerr}.

gethostbyaddr({A,B,C,D}) when integer(A+B+C+D) ->
    getit(?GETHOSTBYADDR, [?INET_AF_INET, [A,B,C,D]]);
gethostbyaddr({0,0,0,0,0,16#ffff,G,H}) when integer(G+H) ->
    gethostbyaddr({G div 256, G rem 256, H div 256, H rem 256});
gethostbyaddr({A,B,C,D,E,F,G,H}) when integer(A+B+C+D+E+F+G+H) ->
    getit(?GETHOSTBYADDR, [?INET_AF_INET6,
			   inet:ip_to_bytes({A,B,C,D,E,F,G,H})]);
gethostbyaddr(Addr) when list(Addr) ->
    case inet_parse:address(Addr) of
        {ok, IP} -> gethostbyaddr(IP);
        Error -> {error, formerr}
    end;
gethostbyaddr(Addr) when atom(Addr) ->
    gethostbyaddr(atom_to_list(Addr));
gethostbyaddr(_) -> {error, formerr}.



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, inet_gethost_native}, inet_gethost_native, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    Port = open_port({spawn, "inet_gethost"}, [{packet,2},eof]),
    {ok, #state{port=Port}, infinity}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({Cmd,Data}, From, State) ->
    Timeout = inet_db:res_option(timeout)*4,
    Port = State#state.port,
    case catch erlang:port_command(Port, [Cmd,Data]) of
	{'EXIT', {badarg,_}} ->
	    {stop, einval,{error,einval}, State};
	{'EXIT', Reason} -> 
	    {stop, Reason, {error, Reason}, State};
	true ->
	    receive
		{Port, {data, Reply}} ->
		    {reply, {ok, Reply}, State};
		{'EXIT', Port, Reason} ->
		    Port1 = maybe_restart_port(Port,State),
		    {reply, {error, Reason}, State#state{port = Port1}}
	    after Timeout ->
		    Port1 = maybe_restart_port(Port,State),
		    {reply, {error, timeout}, State#state{port = Port1}}
	    end
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', Port, Reason},State) -> 
    NewPort=maybe_restart_port(Port,State),
    {noreply,State#state{port=NewPort}};
handle_info({Port,eof}, State) ->
    NewPort=maybe_restart_port(Port,State),
    {noreply,State#state{port=NewPort}}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.


maybe_restart_port(ClosedPort,#state{port=ClosedPort})  ->
    catch erlang:port_close(ClosedPort),
    NewPort = open_port({spawn, "inet_gethost"}, 
			[{packet,2},eof]),
    NewPort;
maybe_restart_port(ClosedPort,#state{port=LivingPort}) ->
    catch erlang:port_close(ClosedPort),
    LivingPort.
%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Test cases
%%
test() ->
    {ok, Host} = inet:gethostname(),
    %% test current host
    {ok, Hent} = gethostbyname(Host),
    %% test unknown host
    {error, notfound} = gethostbyname(kalle_anka_blurf),
    %% test bad arguments
    {error, formerr} = gethostbyname(12),
    %% test bigger name than buffer side on port
    {error, einval} = gethostbyname(lists:duplicate(1024, $x)),
    %% test bad list arguemtns
    {error, einval} = gethostbyname([$f,$o,$o,kalle,$b]),
    
    %% test current host
    {ok, _} = gethostbyaddr(hd(Hent#hostent.h_addr_list)),
    {error, notfound} = gethostbyaddr({0,0,0,0}),

    {error, einval} = gethostbyaddr({0,0,0,500}),

    {ok, _} = gethostbyaddr("127.0.0.1"),     %% this may not be true!!!
    {error, formerr} = gethostbyaddr({1,[]}),
    {error, formerr} = gethostbyaddr("abcd"),
    ok.

getit(Cmd, Data) ->
    ensure_started(),
    Timeout = inet_db:res_option(timeout)*5,
    case gen_server:call(inet_gethost_native, {Cmd, Data}, Timeout) of
	{ok, Reply} ->
	     reply(Reply);
	Error -> Error
    end.
    

do_start(Sup,C) ->
    {Child,_,_,_,_,_} = C,
    case supervisor:start_child(Sup,C) of
	{ok,_} ->
	    ok;
	{error, {already_started, _}} ->
	    ok;
	{error, already_present} ->
	    supervisor:terminate_child(Sup, Child),
	    supervisor:delete_child(Sup, Child),
	    supervisor:start_child(Sup,C),
	    ok
    end.

ensure_started() ->
    case whereis(inet_gethost_native) of
	undefined -> 
	    C = {inet_gethost_native, {?MODULE, start_link, []}, temporary, 
		 1000, worker, [?MODULE]},
	    case whereis(kernel_safe_sup) of
		undefined ->
		    do_start(net_sup,C);
		_ ->
		    do_start(kernel_safe_sup,C)
	    end;
	_ -> 
	    ok
    end.

reply([?REPLY_ERROR | Reason]) -> {error, list_to_atom(Reason)};
reply([?REPLY_OK, 4, ?INET_AF_INET, NAddr, NAlias | T]) ->
    {AddrList, T1} = getaddr(T, 4, NAddr, []),
    {Aliases, Name}  = getaliases(T1, NAlias, []),
    {ok, #hostent { h_name = Name,
		    h_aliases = Aliases,
		    h_addrtype = inet,
		    h_length = 4,
		    h_addr_list = AddrList
		   } };
reply([?REPLY_OK, 16, ?INET_AF_INET6, NAddr, NAlias | T]) ->
    {AddrList, T1} = getaddr(T, 16, NAddr, []),
    {Aliases, Name}  = getaliases(T1, NAlias, []),
    {ok, #hostent { h_name = Name,
		    h_aliases = Aliases,
		    h_addrtype = inet6,
		    h_length = 16,
		    h_addr_list = AddrList
		   } };
reply(_) ->
    {error, einval}.

getaddr(T, _, 0, Acc) ->
    {lists:reverse(Acc), T};
getaddr([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16|T],16,N,Acc) ->
    getaddr(T, 16, N-1, [inet:bytes_to_ip6(X1,X2,X3,X4,X5,X6,X7,X8,
					   X9,X10,X11,X12,X13,X14,X15,X16)]);
getaddr([A,B,C,D | T], 4, N, Acc) ->
    getaddr(T, 4, N-1, [{A,B,C,D} | Acc]).

getaliases(T, 0, Acc) ->
    {lists:reverse(Acc), T};
getaliases(T, N, Acc) ->
    {Alias, T1} = getstr(T, []),
    getaliases(T1, N-1, [Alias | Acc]).
    
getstr([0 | T], Acc) ->
    {lists:reverse(Acc), T};
getstr([C|T], Acc) ->
    getstr(T, [C|Acc]).





















