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
%% Purpose: Ack sender process
%%----------------------------------------------------------------------

-module(megaco_ack_sender).

-export([start_link/3, 
	 stop/1,
	 upgrade/2,
	 send_ack/2,
	 timeout/2,
	 maxcount/2]).
-export([system_continue/3, system_terminate/4, system_code_change/4]).
-export([init/4]).


-include_lib("megaco/include/megaco.hrl").
-include("megaco_message_internal.hrl").
-include("megaco_internal.hrl").


-record(state, 
	{
	  parent,
	  conn_handle,
	  timeout,
	  maxcount,
	  acks = []
	 }).

%% -define(d(F,A), io:format("~p~p:" ++ F ++ "~n", [self(),?MODULE|A])).
-define(d(F,A), ok).

%%%-----------------------------------------------------------------
%%% Public API
%%%-----------------------------------------------------------------
start_link(CH, To, Max) ->
    ?d("start_link -> entry with"
	"~n   CH:  ~p"
	"~n   To:  ~p"
	"~n   Max: ~p", [CH, To, Max]),
    Args = [self(), CH, To, Max],
    proc_lib:start_link(?MODULE, init, Args).

stop(Pid) ->
    Pid ! stop.

upgrade(Pid, CH) ->
    Pid ! {upgrade, CH}.

send_ack(Pid, Serial) ->
    Pid ! {send_ack, Serial}.

maxcount(Pid, Max) ->
    Pid ! {maxcount, Max}.

timeout(Pid, Timeout) ->
    Pid ! {timeout, Timeout}.



%%%-----------------------------------------------------------------
%%% Internal exports
%%%-----------------------------------------------------------------

init(Parent, CH, To, Max) ->
    ?d("init -> entry with"
	"~n   Parent: ~p"
	"~n   CH:     ~p"
	"~n   To:     ~p"
	"~n   Max:    ~p", [Parent, CH, To, Max]),
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    S = #state{parent      = Parent, 
	       conn_handle = CH, 
	       timeout     = To, 
	       maxcount    = Max},
    loop(S, To).


%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------

%% idle (= empty)
loop(#state{acks = [], timeout = Timeout} = S, _) ->
    receive 
	{send_ack, Serial} ->
	    loop(S#state{acks = [Serial]}, Timeout);

	{upgrade, CH} ->
	    loop(S#state{conn_handle = CH}, Timeout);

	{maxcount, NewMax} ->
	    loop(S#state{maxcount = NewMax}, Timeout);

	{timeout, NewTimeout} ->
	    loop(S#state{timeout = NewTimeout}, NewTimeout);

	stop ->
	    exit(normal);

	{system, From, Msg} ->
	    ?d("loop(0) -> received system message:"
		"~n   From: ~p"
		"~n   Msg:  ~p", [From, Msg]),
	    Parent = S#state.parent, 
	    sys:handle_system_msg(Msg, From, Parent, 
				  ?MODULE, [], {S, Timeout});
	
	{'EXIT', Parent, Reason} when S#state.parent == Parent ->
	    exit(Reason);

	M ->
	    error_msg("received unexpected message: ignoring: ~n~p", [M]),
	    loop(S, Timeout)

    end;

%% active (= some acks waiting to to be sent)
loop(#state{maxcount = Max, acks = Acks, timeout = Timeout} = S, To) 
  when To >= 0 ->
    Start = t(),
    receive
	{send_ack, Serial} when length(Acks) + 1 >= Max ->
	    send_acks(S#state.conn_handle, [Serial|Acks]),
	    loop(S#state{acks = []}, Timeout);

	{send_ack, Serial} ->
	    loop(S#state{acks = [Serial|Acks]}, to(To, Start));

	{upgrade, CH} ->
	    loop(S#state{conn_handle = CH}, to(To, Start));

	{maxcount, NewMax} ->
	    loop(S#state{maxcount = NewMax}, to(To, Start));

	{timeout, NewTimeout} ->
	    %% We need to recalculate To
	    D = NewTimeout - Timeout,
	    loop(S#state{timeout = NewTimeout}, to(To-D, Start));

	stop ->
	    send_acks(S#state.conn_handle, Acks),
	    exit(normal);

	{system, From, Msg} ->
	    ?d("loop(~p) -> received system message:"
		"~n   From: ~p"
		"~n   Msg:  ~p", [length(Acks), From, Msg]),
	    Parent = S#state.parent, 
	    sys:handle_system_msg(Msg, From, Parent, 
				  ?MODULE, [], {S, to(To, Start)});

	{'EXIT', Parent, Reason} when S#state.parent == Parent ->
	    exit(Reason);

	M ->
	    error_msg("received unexpected message: ignoring: ~n~p", [M]),
	    loop(S, to(To, Start))

    after To ->
	    send_acks(S#state.conn_handle, Acks),
	    loop(S#state{acks = []}, Timeout)
    end;

loop(#state{acks = Acks, timeout = Timeout} = S, _To) ->
    send_acks(S#state.conn_handle, Acks),
    loop(S#state{acks = []}, Timeout).


%%%-----------------------------------------------------------------

send_acks(_, []) ->
    ok;
send_acks(CH, Serials) ->
    case megaco_config:lookup_local_conn(CH) of
	[ConnData] ->
	    do_send_acks(ConnData, Serials);
	[] ->
	    ok
    end.


do_send_acks(ConnData, SerialRanges) ->
    ?d("send_ack -> entry with"
 	"~n   SerialRanges: ~p", [SerialRanges]),    
    %% Encapsule the transaction result into a reply message
    TRAs = make_acks(ranges(SerialRanges), []),
    Body = {transactions, [{transactionResponseAck, TRAs}]},
    megaco_messenger_misc:send_body(ConnData, "send trans ack(s)", Body).


ranges(L) ->
    lists:reverse(ranges(L, [], [])).

ranges([], Range, Ranges) ->
    ranges2(Range, Ranges);
ranges([S1|Sn], [S2|_] = Range, Ranges) when S1 == (S2+1) ->
    ranges(Sn, [S1|Range], Ranges);
ranges([S|Sn], Range, Ranges) ->
    ranges(Sn, [S], ranges2(Range, Ranges)).

ranges2([], Ranges) -> 
    Ranges;
ranges2([S], Ranges) ->
    [{S,S}|Ranges];
ranges2(Range0, Ranges) ->
    Range = lists:reverse(Range0),
    [{hd(Range),lists:last(Range)}|Ranges].


make_acks([], Acks) ->
    lists:reverse(Acks);
make_acks([{S,S}|SerialRanges], Acks) ->
    TRA = #'TransactionAck'{firstAck = S},
    make_acks(SerialRanges, [TRA|Acks]);
make_acks([{F,L}|SerialRanges], Acks) ->
    TRA = #'TransactionAck'{firstAck = F, lastAck = L},
    make_acks(SerialRanges, [TRA|Acks]).



%%%-----------------------------------------------------------------

to(To, Start) ->
    To - (t() - Start).

%% Time in milli seconds
t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).

error_msg(F, A) ->
    error_logger:error_msg("~p:" ++ F ++ "~n", [?MODULE|A]).
 

%%%-----------------------------------------------------------------
%%% System messages handled here
%%%-----------------------------------------------------------------

system_continue(_Parent, _Dbg, {S,To}) ->
    loop(S, To).

system_terminate(Reason, _Parent, _Dbg, {S, _}) ->
    #state{conn_handle = CH, acks = Acks} = S,
    send_acks(CH, Acks),
    exit(Reason).

system_code_change(S, _Module, _OLdVsn, _Extra) ->
    ?d("system_code_change -> entry", []),
    {ok, S}.
