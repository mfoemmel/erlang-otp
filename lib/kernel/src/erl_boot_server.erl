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
%% A simple boot_server at a CP.
%%
%% This server should know about which slaves (DP's or whatever) to boot.
%% File's (with absolute path name) will be fetched.
%%

-module(erl_boot_server).

-include("inet_boot.hrl").
-behaviour(gen_server).

%% API functions.
-export([start/1, start_link/1, add_slave/1, delete_slave/1,
	 add_subnet/2, delete_subnet/2,
	 which_slaves/0]).

%% Exports for testing (dont't remove; tests suites depend on them).
-export([would_be_booted/1]).

%% Internal exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).
-export([boot_init/1, boot_accept/3]).


-record(state, 
	{
	 priority = 0,  %% priority of this server
	 version = "",  %% Version handled i.e "4.5.3" etc
	 udp_sock,      %% listen port for broadcase requests
	 udp_port,      %% port number must be ?EBOOT_PORT!
	 listen_sock,   %% listen sock for incoming file requests
	 listen_port,   %% listen port number
	 slaves,        %% list of accepted ip addresses
	 bootp          %% boot process 
	}).

-define(single_addr_mask, {255, 255, 255, 255}).

start(Slaves) ->
    case check_arg(Slaves) of
	{ok,AL} ->
	    gen_server:start({local,boot_server}, erl_boot_server, AL, []);
	_ ->
	    {error, {badarg, Slaves}}
    end.

start_link(Slaves) ->
    case check_arg(Slaves) of
	{ok,AL} ->
	    gen_server:start_link({local,boot_server},
				  erl_boot_server, AL, []);
	_ ->
	    {error, {badarg, Slaves}}
    end.

check_arg(Slaves) ->
    check_arg(Slaves, []).

check_arg([Slave|Rest], Result) ->
    case inet:getaddr(Slave, inet) of
	{ok, IP} ->
	    check_arg(Rest, [{?single_addr_mask, IP}|Result]);
	_ ->
	    error
    end;
check_arg([], Result) ->
    {ok, Result};
check_arg(_, _Result) ->
    error.

add_slave(Slave) ->
    case inet:getaddr(Slave, inet) of
	{ok,IP} ->
	    gen_server:call(boot_server, {add, {?single_addr_mask, IP}});
	_ ->
	    {error, {badarg, Slave}}
    end.

delete_slave(Slave) ->
    case inet:getaddr(Slave, inet) of
	{ok,IP} ->
	    gen_server:call(boot_server, {delete, {?single_addr_mask, IP}});
	_ ->
	    {error, {badarg, Slave}}
    end.

add_subnet(Mask, Addr) when tuple(Mask), tuple(Addr) ->
    case member_address(Addr, [{Mask, Addr}]) of
	true ->
	    gen_server:call(boot_server, {add, {Mask, Addr}});
	false ->
	    {error, empty_subnet}
    end.

delete_subnet(Mask, Addr) when tuple(Mask), tuple(Addr) ->
    gen_server:call(boot_server, {delete, {Mask, Addr}}).

which_slaves() ->
    gen_server:call(boot_server, which).

%% Given a host name or IP address, returns true if a host
%% having that IP address would be accepted for booting, and
%% false otherwise.  (Convenient for testing.)

would_be_booted(Addr) ->
    {ok, IP} = inet:getaddr(Addr, inet),
    member_address(IP, which_slaves()).

int16(X) when integer(X) ->
    [(X bsr 8) band 16#ff, (X) band 16#ff].

%% Check if an address is a member

member_address(IP, [{{MA, MB, MC, MD}, {EA, EB, EC, ED}}|Rest]) ->
    {A, B, C, D} = IP,
    if A band MA == EA,
       B band MB == EB,
       C band MC == EC,
       D band MD == ED ->
	    true;
       true ->
	    member_address(IP, Rest)
    end;
member_address(_, []) ->
    false.

%% ------------------------------------------------------------
%% call-back functions.
%% ------------------------------------------------------------

init(Slaves) ->
    {ok, U} = gen_udp:open(?EBOOT_PORT, []),
    {ok, L} = gen_tcp:listen(0, [{packet,4}]),
    {ok,Port} = inet:port(L),
    {ok,UPort} = inet:port(U),
    Ref = make_ref(),
    Pid = proc_lib:spawn_link(?MODULE, boot_init, [Ref]),
    gen_tcp:controlling_process(L, Pid),
    Pid ! {Ref, L},
    %% We trap exit inorder to restart boot_init and udp_port 
    process_flag(trap_exit, true),
    {ok, #state {
		 priority = 0,
		 version = erlang:system_info(version),
		 udp_sock = U, 
		 udp_port = UPort,
		 listen_sock = L, 
		 listen_port = Port,
		 slaves = ordsets:from_list(Slaves),
		 bootp = Pid
		}}.


handle_call({add,Address}, _, S0) ->
    Slaves = ordsets:add_element(Address, S0#state.slaves),
    S0#state.bootp ! {slaves, Slaves},
    {reply, ok, S0#state { slaves = Slaves}};
handle_call({delete,Address}, _, S0) ->
    Slaves = ordsets:del_element(Address, S0#state.slaves),
    S0#state.bootp ! {slaves, Slaves},
    {reply, ok, S0#state { slaves = Slaves }};
handle_call(which, _, S0) ->
    {reply, ordsets:to_list(S0#state.slaves), S0}.

handle_cast(_, Slaves) ->
    {noreply, Slaves}.

handle_info({udp, U, IP, Port, Data}, S0) ->
    Token = ?EBOOT_REQUEST ++ S0#state.version,
    Valid = member_address(IP, ordsets:to_list(S0#state.slaves)),
    if Valid == true, Data == Token ->
	    gen_udp:send(U,IP,Port,[?EBOOT_REPLY,S0#state.priority,
				    int16(S0#state.listen_port),
				    S0#state.version]),
	    {noreply, S0};
       true ->
	    {noreply, S0}
    end;
handle_info(_Info, S0) ->
    {noreply, S0}.

terminate(_Reason, _S0) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Boot server 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boot_init(Tag) ->
    receive
	{Tag, Listen} ->
	    process_flag(trap_exit, true),
	    boot_main(Listen)
    end.

boot_main(Listen) ->
    Tag = make_ref(),
    Pid = proc_lib:spawn_link(?MODULE,boot_accept,[self(),Listen,Tag]),
    boot_main(Listen, Tag, Pid).

boot_main(Listen, Tag, Pid) ->
    receive
	{Tag, _} ->
	    boot_main(Listen);
	{'EXIT', Pid, _} -> 
	    boot_main(Listen);
	{'EXIT', _, Reason} ->
	    exit(Pid, kill),
	    exit(Reason);
	{tcp_closed, Listen} ->
	    exit(closed)
    end.

boot_accept(Server, Listen, Tag) ->
    Reply = gen_tcp:accept(Listen),
    unlink(Server),
    Server ! {Tag, continue},
    case Reply of
	{ok, Socket} ->
	    {ok,{IP,_Port}} = inet:peername(Socket),
	    true = member_address(IP, which_slaves()),
	    boot_loop(Socket)
    end.


boot_loop(Socket) ->
    receive
	{tcp, Socket, Data} ->
	    handle_command(Socket, Data),
	    boot_loop(Socket);
	{tcp_closed, Socket} ->
	    true
    end.

handle_command(S, [$F | File]) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    gen_tcp:send(S, [$f | Bin]);
	{error,Reason} ->
	    Msg = erl_posix_msg:message(Reason),
	    gen_tcp:send(S, [$e | Msg])
    end;
handle_command(S, _Other) ->
    gen_tcp:send(S, [$e | "unknown command"]).


