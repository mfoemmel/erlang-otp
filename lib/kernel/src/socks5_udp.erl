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
-module(socks5_udp).

%% Socks5 wrapper for Udp

-export([open/1, open/2, close/1]).
-export([send/4, recv/2]).
-export([controlling_process/2]).

-export([getserv/1, getaddr/1]).

-include("inet_int.hrl").
-include("socks5.hrl").

-define(REASSEMBLY_TIME, 3000).
-define(IPV4_HEADER, 10).
-define(IPV6_HEADER, 22).

-record(state,
	{
	 frag_cnt = 0,
	 frag_queue = [],
	 frag_timer = undefined,
	 bound_ip,
	 bound_port,
	 socks_header = 0,   %% size of udp socks header
	 user_header = 0,    %% current user header
	 server              %% socks server
	}).


%% inet_tcp port lookup
getserv(Port) when integer(Port) -> {ok, Port};
getserv(Name) when atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_tcp address lookup
getaddr(Address) ->
    case inet:getaddr(Address, inet) of
	{ok, IP} -> {ok, IP};
	{error, nxdomain} -> {ok, Address};
	Error -> Error
    end.

controlling_process(Socket, Owner) -> 
    gen_udp:controlling_process(Socket, Owner).

recv(Socket, Len) ->
    gen_udp:recv(Socket, Len).

close(Socket) ->
    gen_udp:close(Socket).


send(Socket, {A,B,C,D}, Port, Packet) when ?ip(A,B,C,D), integer(Port) ->
    inet_udp:send(Socket, {A,B,C,D}, Port, Packet);
send(Socket, Address, Port, Packet) when integer(Port) ->
    inet_udp:send(Socket, Address, Port, Packet);
send(_, _, _, _) ->
    exit(badarg).

open(Port) -> 
    open(Port, []).


open(Port, Opts) when integer(Port) ->
    case inet:udp_options([{port,Port} | Opts], inet) of
	{ok, R} ->
	    Options = [{ifaddr,R#udp_opts.ifaddr},{header,?IPV4_HEADER}],
	    case inet_udp:open(R#udp_opts.port, Options) of
		{ok, S} ->
		    {ok, {SrcIP, SrcPort}} = inet:sockname(S),
		    case socks5:open([]) of
			{ok, Socks} ->
			    case socks5:associate(Socks, SrcIP, SrcPort) of
				{ok, {BndIP, BndPort}} ->
				    SocksServer = inet_db:socks_option(server),
				    inet:pushf(S, fun handle_socks5/3, 
					       #state { bound_ip = BndIP,
							bound_port = BndPort,
							server = SocksServer,
							socks_header = ?IPV4_HEADER }),
				    inet:setopts(S, R#udp_opts.opts),
				    inet_db:register_socket(S, ?MODULE),
				    {ok, S};
				Error -> 
				    inet_udp:close(S),
				    socks5:close(Socks),
				    Error
			    end;
			Error ->
			    inet_udp:close(S),
			    Error
		    end;
		Error -> 
		    Error
	    end;
	Error -> Error
    end.


%%
%% Handle socks5 udp
%%
handle_socks5(input, {IP,_Port,[_,_,Frag,AType | Data]}, Info) 
when IP == Info#state.server ->
    if
	Frag == 0 -> %% stand alone
	    input_data(AType, Data, reset_queue(Info));
	true ->
	    Frag1 = Frag band 16#7f,
	    Last = if (Frag band 16#80) == 16#80 -> true;
		      true -> false
		   end,
	    if
		Frag1 == Info#state.frag_cnt + 1 ->
		    enqueue_data(Last, AType, Data, Info);
		true ->
		    {false, reset_queue(Info)}
	    end
    end;
handle_socks5(input, Data, Info) ->
    {input, Data, Info};
handle_socks5(output, {IP,Port,Data}, Info) ->
    if
	tuple(IP), size(IP) == 4 ->
	    case socks5:is_direct(IP) of
		true ->
		    {output, {IP,Port,Data}, Info};
		false ->
		    {output, {Info#state.bound_ip, Info#state.bound_port,
			      [0,0,0,?SOCKS5_ATYP_V4,tuple_to_list(IP),
			       ?int16(Port), Data]}, Info}
	    end;
	tuple(IP), size(IP) == 16 ->
	    {output, {Info#state.bound_ip, Info#state.bound_port,
		      [0,0,0,?SOCKS5_ATYP_V6,tuple_to_list(IP),
		       ?int16(Port), Data]}, Info};
	list(IP) ->
	    {output, {Info#state.bound_ip, Info#state.bound_port,
		      [0,0,0,?SOCKS5_ATYP_DOM,length(IP), IP,
		       ?int16(Port), Data]}, Info};
	true ->
	    {false, Info}
    end;
handle_socks5(event, {timeout,Ref,socks5_timeout}, Info) ->
    if Ref == Info#state.frag_timer ->
	    {false, Info#state { frag_cnt = 0, frag_queue = [],
				 frag_timer = undefined }};
       true ->
	    {false, Info}
    end;
%% preserve user header
handle_socks5(option, {header,N}, Info) -> 
    Info1 = Info#state { user_header = N },
    if N < 0 -> 
	    {option, {header,N}, Info1};
       true ->
	    {option, {header, N+Info#state.socks_header}, Info1}
    end;
handle_socks5(Tag, Data, Info) ->
    {Tag, Data, Info}.


input_data(?SOCKS5_ATYP_V4, [A,B,C,D,P1,P0 | Data], Info) ->
    {input, {{A,B,C,D}, ?u16(P1, P0), Data}, Info};
input_data(_, _, Info) ->
    {true, Info}.

enqueue_data(true, ?SOCKS5_ATYP_V4,[A,B,C,D,P1,P0 | Data], Info) ->
    Queue = Info#state.frag_queue,
    %% XXX Potential bug here - Data1 is not used.
    Data1 = merge_data([Data,Queue], Info#state.user_header),
    {input, {{A,B,C,D}, ?u16(P1,P0), Data}, reset_queue(Info)};
enqueue_data(false, ?SOCKS5_ATYP_V4,[_A,_B,_C,_D,_P1,_P0 | Data], Info) ->
    Cnt = Info#state.frag_cnt,
    if
	Cnt == 0 ->
	    TRef = set_timer(?REASSEMBLY_TIME, socks5_timeout),
	    {true, Info#state { 
			       frag_cnt = 1,
			       frag_queue = [Data],
			       frag_timer = TRef }};
	true ->
	    Q = Info#state.frag_queue,
	    {true, Info#state {
			       frag_cnt = Cnt + 1,
			       frag_queue = [Data, Q] }}
    end;
enqueue_data(_, _, _, Info) ->
    {true, reset_queue(Info)}.

%% Merge data to user !!!
merge_data(Queue, Header) ->
    Data = list_to_binary(Queue),
    if 
	Header < 0 ->
	    binary_to_list(Data);
	Header >= size(Data) ->
	    binary_to_list(Data);
	Header == 0 ->
	    Data;
	true ->
	    {B1, B2} = split_binary(Header, Data),
	    binary_to_list(B1) ++  B2
    end.


reset_queue(Info) ->
    case Info#state.frag_timer of
	undefined -> Info;
	TRef -> cancel_timer(TRef),
		Info#state { frag_cnt = 0, frag_queue = [], 
			     frag_timer = undefined }
    end.
    
set_timer(Time, Message) ->
    Ref = erlang:start_timer(Time, self(), Message),
    Ref.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).



