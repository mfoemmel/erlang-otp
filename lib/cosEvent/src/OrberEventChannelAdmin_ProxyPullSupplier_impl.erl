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
%%------------------------------------------------------------------------
%% Module:	OrberEventChannelAdmin_ProxyPullSupplier_impl
%% Description: Implementation of the ProxyPullSupplier interface that defines
%%		the second step for connecting pull consumers to the event channel	
%% Created:	971001	
%% Modified:	
%%------------------------------------------------------------------------

%%-----------------------------------------------------------
-module('OrberEventChannelAdmin_ProxyPullSupplier_impl').

-include("event_service.hrl").
-include_lib("orber/include/corba.hrl").

-export([init/1, terminate/2, connect_pull_consumer/3, pull/1, try_pull/1, disconnect_pull_supplier/2, push_to_queue/2, handle_info/2]).

%%------------------------------------------------------------------------
%% Standard methods for the interface
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
init({EventChannel, EventChannelPid}) ->
    process_flag(trap_exit, true),
    link(EventChannelPid),

    {ok, #proxy_pull_supplier{ 
       event_channel_pid = EventChannelPid,
       event_channel = EventChannel}}.

%%------------------------------------------------------------------------
%% Func: 
%%------------------------------------------------------------------------
terminate(From, Reason) ->
    ok.

%%------------------------------------------------------------------------
%% Func: 	connect_pull_consumer/3
%% Args:	PullConsumer could be NIL object reference
%% Return: 	if the ProxyPullSupplier is laready connected to a 
%%	PullConsumer, then the 'AlreadyConnected' exception is 
%%	raised
%%------------------------------------------------------------------------
connect_pull_consumer(OE_THIS, OE_State, PullConsumer) ->

    case corba_object:is_nil(OE_State#proxy_pull_supplier.consumer) of
	true ->
	    case OE_State#proxy_pull_supplier.consumer_pid of
		secret ->
		    %% if Proxy has been previously connected to a PullConsumer 
		    %% with NIL object Reference then the Pid of that process is
		    %% unknown
		    corba:raise(#'AlreadyConnected'{});
		_ ->
		    case corba_object:is_nil(PullConsumer) of
			true ->
			    %% we can't get it's Pid
			    {void, OE_State#proxy_pull_supplier {consumer = PullConsumer, consumer_pid = secret}};
			_ ->
			    %% link Proxy process to PullConsumer process
			    {_, Key} = iop_ior:get_key(PullConsumer),
			    Pid = orber_objectkeys:get_pid(Key),
			    link(Pid),

			    {void, OE_State#proxy_pull_supplier {consumer = PullConsumer, consumer_pid = Pid}}
		    end
	    end;
	_ ->
	    corba:raise(#'AlreadyConnected'{})
    end.

%%------------------------------------------------------------------------
%% Func: pull/1
%% Args:
%% Return:
%% Comments:	it will not return until some message arrives
%%------------------------------------------------------------------------
pull(OE_State) ->
    %% Data should be of type ANY = #any{typecode, value}, where typecode for
    %% sequence = {tk_sequence, ElemTypeCode, MaxLength}, where MaxLength = 0
    %% for unbounded sequences

    %% check that the proxy has a consumer attached to it
    case corba_object:is_nil(OE_State#proxy_pull_supplier.consumer) of
	true ->
	    case OE_State#proxy_pull_supplier.consumer_pid of
		secret ->
		    Do_pull = true;
		_ ->
		    Do_pull = false
	    end;
	_ ->
	    Do_pull = true
    end,

    %% pull data if Porxy has a consumer attached to it
    case Do_pull of
	true ->
	     #any{value=Data} = 'OrberEventChannelAdmin_EventChannel':pull(OE_State#proxy_pull_supplier.event_channel),

	    %% pop last message from the list
	    {QueueData, NewQueue} = pop_from_queue(OE_State#proxy_pull_supplier.queue),

	    AllData = [QueueData|Data],

	    case AllData of
		[empty|[]] ->
		    sleep(20),
		    pull(OE_State);
		[empty|T] ->	
		    {#any{typecode={tk_sequence, tk_any, 0}, value=T},
		     OE_State#proxy_pull_supplier {queue = NewQueue}};
		_ ->
		    {#any{typecode={tk_sequence, tk_any, 0}, value=AllData},
		     OE_State#proxy_pull_supplier {queue = NewQueue}}
	    end;
	_ ->
	    corba:raise(#'Disconnected'{})
    end.


%%------------------------------------------------------------------------
%% Func:	try_pull/1
%% Args:
%% Return:
%%------------------------------------------------------------------------
try_pull(OE_State) ->
    %% Data should be of type ANY = #any{typecode, value}, where typecode for
    %% sequence = {tk_sequence, ElemTypeCode, MaxLength}, where MaxLength = 0
    %% for unbounded sequences

    %% check that the proxy has a consumer attached to it
    case corba_object:is_nil(OE_State#proxy_pull_supplier.consumer) of
	true ->
	    case OE_State#proxy_pull_supplier.consumer_pid of
		secret ->
		    Do_pull = true;
		_ ->
		    Do_pull = false
	    end;
	_ ->
	    Do_pull = true
    end,

    %% pull data if Porxy has a consumer attached to it
    case Do_pull of
	true ->
	    #any{value=Data} = 'OrberEventChannelAdmin_EventChannel':pull(OE_State#proxy_pull_supplier.event_channel),

	    %% pop last message from the list
	    {QueueData, NewQueue} = pop_from_queue(OE_State#proxy_pull_supplier.queue),

	    AllData = [QueueData|Data],

	    case AllData of
		[empty|[]] ->
		    {{#any{typecode=tk_long, value=0}, false},
		     OE_State#proxy_pull_supplier{queue = NewQueue}};
		[empty|T]  ->
		    {{#any{typecode={tk_sequence, tk_any, 0}, value=T}, true},
		     OE_State#proxy_pull_supplier {queue = NewQueue}};
		_ ->
		    {{#any{typecode={tk_sequence, tk_any, 0}, value=AllData}, true},
		     OE_State#proxy_pull_supplier {queue = NewQueue}}
	    end;
	_ ->
	    corba:raise(#'Disconnected'{})
    end.


%%------------------------------------------------------------------------
%% Func: disconnect_pull_supplier/2
%% Args:
%% Return:
%% Comments:	Proxy will be removed from EventChannel's list when it
%% receives EXIT signal
%%------------------------------------------------------------------------
disconnect_pull_supplier(OE_THIS, OE_State) ->
    case corba_object:is_nil(OE_State#proxy_pull_supplier.consumer) of
	true ->
	    %% it is possibe that there is a consumer attached to this proxy
	    %% but it was attached as a NIL obj ref
	    false;	% nothing to do
	_ ->
	    'CosEventComm_PullConsumer':disconnect_pull_consumer(OE_State#proxy_pull_supplier.consumer)
    end,

    'OrberEventChannelAdmin_EventChannel':rm_proxy_pull_supplier(OE_State#proxy_pull_supplier.event_channel, {OE_THIS, self()}),

						% kill the process
    {stop, normal, void, OE_State}.

%%------------------------------------------------------------------------
%% Func:	handle_info/2	
%% Args:		
%% Return:	
%% Comments: exits if EXIT signal received from Event Channel or Consumer.  
%% if EXIT from Event channel then consumer will be notified about the lose
%% of the connection
%%------------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->

    case State#proxy_pull_supplier.consumer_pid of 
	Pid ->
	    {stop, normal, []};
	_ ->
	    case State#proxy_pull_supplier.event_channel_pid of 
		Pid ->
		    case corba_object:is_nil(State#proxy_pull_supplier.consumer) of
			true ->
			    false;	% nothing to do
			_ ->
			    'CosEventComm_PullConsumer':disconnect_pull_consumer(State#proxy_pull_supplier.consumer)
		    end,

		    {stop, normal, []};
		_ ->
		    {noreply, State}
	    end
    end.


%%------------------------------------------------------------------------
%% Non-standard methods 
%%------------------------------------------------------------------------


%%------------------------------------------------------------------------
%% Func:	push_to_queue/2
%% Args:
%% Return: adds data to its queue
%% Comments: only push Data to the queue if the proxy has a consumer 
%% attached to it
%%------------------------------------------------------------------------
push_to_queue(OE_State, Data) ->
    case corba_object:is_nil(OE_State#proxy_pull_supplier.consumer) of
	true ->
						% check that it is not a secret Consumer
	    case OE_State#proxy_pull_supplier.consumer_pid of
		secret ->
		    NewQueue = lists:append(OE_State#proxy_pull_supplier.queue, [Data]),
		    {void, OE_State#proxy_pull_supplier {queue = NewQueue}};
		_ ->
		    {void, OE_State}
	    end;
	_ ->
	    NewQueue = lists:append(OE_State#proxy_pull_supplier.queue, [Data]),
	    {void, OE_State#proxy_pull_supplier {queue = NewQueue}}
    end.


%%------------------------------------------------------------------------
%% Func:	pop_from_queue/1
%% Args:
%% Return:
%%------------------------------------------------------------------------
pop_from_queue([]) ->
    {empty, []};
pop_from_queue([H|T]) ->
    {H, T}.



%%------------------------------------------------------------------------
%% Func:	sleep/1	
%% Purpose:	
%% Args:		
%% Return:	
%%------------------------------------------------------------------------
sleep(Time) ->
    receive
    after Time ->
	    true
    end.



