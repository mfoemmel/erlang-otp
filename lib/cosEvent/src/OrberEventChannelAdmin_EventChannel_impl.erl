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
%% Module:	OrberEventChannelAdmin_EventChannel_impl
%% Description:	Implementation of EventChannel interface which defines 3
%%		administrative operations: adding consumers, adding suppliers, and 
%%		destroying the channel	
%% Author:	Helen Airiyan	
%% Created:	971001	
%% Modified:	
%%------------------------------------------------------------------------

-module('OrberEventChannelAdmin_EventChannel_impl').

-include("event_service.hrl").
-include_lib("orber/include/corba.hrl").

-export([init/1, terminate/2, for_consumers/2, for_suppliers/2, destroy/1, push/2, pull/1, add_proxy_push_consumer/2, add_proxy_push_supplier/2, add_proxy_pull_consumer/2, add_proxy_pull_supplier/2, rm_proxy_push_consumer/2, rm_proxy_push_supplier/2, rm_proxy_pull_consumer/2, rm_proxy_pull_supplier/2, handle_info/2]).


%%------------------------------------------------------------------------
%%	Standard methods of the interface
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% Func:
%% Args:
%% Return:
%%------------------------------------------------------------------------
init(Env) ->
    process_flag(trap_exit, true),
    {ok, #event_channel {}}.

%%------------------------------------------------------------------------
%% Func:
%% Args:
%% Return:
%%------------------------------------------------------------------------
terminate(From, Reason) ->
    ok.


%%------------------------------------------------------------------------
%% Func:	for_suppliers/2
%% Args:
%% Return:	SupplierAdmin object
%%------------------------------------------------------------------------
for_suppliers(OE_THIS, OE_State) ->
    case corba_object:is_nil(OE_State#event_channel.supplier_admin) of
	true ->
	    SupplierAdmin =
		'CosEventChannelAdmin_SupplierAdmin':oe_create_link(OE_THIS),
	    {SupplierAdmin, OE_State#event_channel{ supplier_admin = SupplierAdmin}};
	_ ->
	    {OE_State#event_channel.supplier_admin, OE_State}
    end.


%%------------------------------------------------------------------------
%% Func:	for_consumers/2
%% Args:
%% Return:	ConsumerAdmin object
%%------------------------------------------------------------------------
for_consumers(OE_THIS, OE_State) ->
    case corba_object:is_nil(OE_State#event_channel.consumer_admin) of 
	true ->
	    ConsumerAdmin =
		'CosEventChannelAdmin_ConsumerAdmin':oe_create_link(OE_THIS),

	    {ConsumerAdmin, OE_State#event_channel{ consumer_admin = ConsumerAdmin}};
	_ ->
	    {OE_State#event_channel.consumer_admin, OE_State}
    end.


%%------------------------------------------------------------------------
%% Func:	destroy/1
%% Args:
%% Return:
%% Comments:	destoy results in EXIT messages to be sent to all existing
%% proxies, SupplierAdmin and ConsumerAdmin in order for them to exit
%% as well
%%------------------------------------------------------------------------
destroy(OE_State) ->
    {stop, normal, void, OE_State}.


%%------------------------------------------------------------------------
%% Func:		
%% Args:		
%% Return:	
%% Comments: need to catch exit from proxies only
%%------------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->

    case delete(Pid, 2, State#event_channel.ppush_consumers) of
	{true, NewPPushCons} ->
	    {noreply, State#event_channel {ppush_consumers = NewPPushCons}};
	_ ->
	    case delete(Pid, 2, State#event_channel.ppull_consumers) of
		{true, NewPPullCons} ->
		    {noreply, State#event_channel {ppull_consumers = NewPPullCons}};
		_->
		    case delete(Pid, 2, State#event_channel.ppush_suppliers) of
			{true, NewPPushSups} ->
			    {noreply, State#event_channel {ppush_suppliers = NewPPushSups}};
			_ ->
			    case delete(Pid, 2, State#event_channel.ppull_suppliers) of
				{true, NewPPullSups} ->
				    {noreply, State#event_channel {ppull_suppliers = NewPPullSups}};
				_ ->
				    {noreply, State}
			    end
		    end
	    end
    end.

%%------------------------------------------------------------------------
%% Non-standard methods
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% Func:	push/2
%% Args:	Data to be send to all consumers. Data is of type ANY
%% Return:
%% Comments:	send push to all registered proxy push suppliers
%% send push to all registered proxy pull suppliers (for queues)
%%------------------------------------------------------------------------
push(OE_State, Data) ->
    push_proxies(OE_State#event_channel.ppush_suppliers, Data), 
    notify_ppull_suppliers(OE_State#event_channel.ppull_suppliers, Data),
    {void, OE_State}.

%%------------------------------------------------------------------------
%% Func:	push_proxies/2
%% Args:	Data to be pushed to consumers
%% Return:
%% Comments:	send data to all Proxy Push Consumers
%%------------------------------------------------------------------------
push_proxies([], _) ->
    ok;
push_proxies([{Obj, Pid}|T], Data) ->
    'OrberEventChannelAdmin_ProxyPushSupplier':send_data(Obj, Data), 
    push_proxies(T, Data).

%%------------------------------------------------------------------------
%% Func:	notify_ppull_suppliers/2
%% Args:	Data to be pushed to consumers
%% Return:
%% Comments:	send a notification message to all registered Proxy Pull 
%% Suppliers in order for them to store the Data
%%------------------------------------------------------------------------
notify_ppull_suppliers([], Data) ->
    ok;
notify_ppull_suppliers([{Obj, Pid}|T], Data) ->
    'OrberEventChannelAdmin_ProxyPullSupplier':push_to_queue(Obj, Data),
    notify_ppull_suppliers(T, Data).

%%------------------------------------------------------------------------
%% Func:	pull/1
%% Args:
%% Return:	sequence of all data recieved: {{tk_sequence, tk_any, 0}, Value}
%%------------------------------------------------------------------------
pull(OE_State) ->
    Data = pull_proxies(OE_State#event_channel.ppull_consumers, []),
    {#any{typecode={tk_sequence, tk_any, 0}, value=Data}, OE_State}.


%%------------------------------------------------------------------------
%% Func:	pull_proxies/2
%% Args:
%% Return:	list of all data received
%% Comments:	created a list of all data received through Proxy Pull 
%% Consumers
%%------------------------------------------------------------------------
pull_proxies([], DataList) ->
    DataList;
pull_proxies([{Obj, Pid}|T], DataList) ->
    case 'OrberEventChannelAdmin_ProxyPullConsumer':get_data(Obj) of
	{Data, true} ->
	    pull_proxies(T, [Data|DataList]);
	{Data, _} ->
	    pull_proxies(T, DataList)
    end.


%%------------------------------------------------------------------------
%% Func:	add_proxy_push_consumer/2
%% Args:
%% Return: new Proxy and its PID are added to the list of Proxy Push Consumers
%%------------------------------------------------------------------------
add_proxy_push_consumer(OE_State, ProxyPushConsumer) ->
    PpushConsumers = OE_State#event_channel.ppush_consumers,

    {_, Key} = iop_ior:get_key(ProxyPushConsumer),
    Pid = orber_objectkeys:get_pid(Key),

    {void, OE_State#event_channel {ppush_consumers = [{ProxyPushConsumer, Pid}|PpushConsumers]}}.

%%------------------------------------------------------------------------
%% Func:	rm_proxy_push_consumer/2
%% Args:
%% Return:	proxy removed from the list of proxy push consumers	
%% Comments:	this function is not necessary as we can rely on EXIT signals
%% to be caught
%%------------------------------------------------------------------------
rm_proxy_push_consumer(OE_State, {ProxyPushConsumer, Pid}) ->
    NewList = lists:keydelete(Pid, 2, OE_State#event_channel.ppush_consumers),
    {void, OE_State#event_channel {ppush_consumers = NewList}}.

%%------------------------------------------------------------------------
%% Func:	add_proxy_pull_supplier/2
%% Args:
%% Return: new Proxy and its PID are added to the list of Proxy Push Suppliers
%%------------------------------------------------------------------------
add_proxy_push_supplier(OE_State, ProxyPushSupplier) ->
    PpushSuppliers = OE_State#event_channel.ppush_suppliers,

    {_, Key} = iop_ior:get_key(ProxyPushSupplier),
    Pid = orber_objectkeys:get_pid(Key),

    {void, OE_State#event_channel {ppush_suppliers = [{ProxyPushSupplier, Pid}|PpushSuppliers]}}.

%%------------------------------------------------------------------------
%% Func:	rm_proxy_push_supplier/2
%% Args:
%% Return:	proxy removed from the list of proxy push suppliers	
%% Comments:	this function is not necessary as we can rely on EXIT signals
%% to be caught
%%------------------------------------------------------------------------
rm_proxy_push_supplier(OE_State, {ProxyPushSupplier, Pid}) ->
    NewList = lists:keydelete(Pid, 2, OE_State#event_channel.ppush_suppliers),
    {void, OE_State#event_channel {ppush_suppliers = NewList}}.

%%------------------------------------------------------------------------
%% Func:	add_proxy_pull_consumer/2
%% Args:
%% Return: new Proxy and its PID are added to the list of Proxy Pull Consumers
%%------------------------------------------------------------------------
add_proxy_pull_consumer(OE_State, ProxyPullConsumer) ->

    PpullConsumers = OE_State#event_channel.ppull_consumers,

    {_, Key} = iop_ior:get_key(ProxyPullConsumer),
    Pid = orber_objectkeys:get_pid(Key),

    {void, OE_State#event_channel {ppull_consumers = [{ProxyPullConsumer, Pid}|PpullConsumers]}}.

%%------------------------------------------------------------------------
%% Func:	rm_proxy_pull_consumer/2
%% Args:
%% Return:	proxy removed from the list of proxy pull consumers 
%% Comments:	this function is not necessary as we can rely on EXIT signals
%% to be caught
%%------------------------------------------------------------------------
rm_proxy_pull_consumer(OE_State, {ProxyPullConsumer, Pid}) ->
    NewList = lists:keydelete(Pid, 2, OE_State#event_channel.ppull_consumers),
    {void, OE_State#event_channel {ppull_consumers = NewList}}.

%%------------------------------------------------------------------------
%% Func:	add_proxy_pull_supplier/2
%% Args:
%% Return: new Proxy and its PID are added to the list of Proxy Pull Suppliers
%%------------------------------------------------------------------------
add_proxy_pull_supplier(OE_State, ProxyPullSupplier) ->

    PpullSuppliers = OE_State#event_channel.ppull_suppliers,

    {_, Key} = iop_ior:get_key(ProxyPullSupplier),
    Pid = orber_objectkeys:get_pid(Key),

    {void, OE_State#event_channel {ppull_suppliers = [{ProxyPullSupplier, Pid} |PpullSuppliers]}}.

%%------------------------------------------------------------------------
%% Func:	rm_proxy_pull_supplier/2
%% Args:
%% Return:	proxy removed from the list of proxy pull suppliers 
%% Comments:	this function is not necessary as we can rely on EXIT signals
%% to be caught
%%------------------------------------------------------------------------
rm_proxy_pull_supplier(OE_State, {ProxyPullSupplier, Pid} ) ->
    NewList = lists:keydelete(Pid, 2, OE_State#event_channel.ppull_suppliers),
    {void, OE_State#event_channel {ppull_suppliers = NewList}}.



%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
delete(Pid, N, List) ->
    case lists:keydelete(Pid, N, List) of
	List ->
	    {false, List};
	NewList ->
	    {true, NewList}
    end.

