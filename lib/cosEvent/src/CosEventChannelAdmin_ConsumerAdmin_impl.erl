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
%%--------------------------------------------------------------------------
%% File:	CosEventChannelAdmin_ConsumerAdmin_impl
%% Description: Implementation of ConsumerAdmin interface that defines the 
%% 			first step for connecting consumers to the event channel
%% Created:	971001
%% Modified: 
%%--------------------------------------------------------------------------
-module('CosEventChannelAdmin_ConsumerAdmin_impl').

-include("event_service.hrl").

-export([init/1, terminate/2, obtain_push_supplier/1, obtain_pull_supplier/1, handle_info/2]).

init(EventChannel) ->
    process_flag(trap_exit, true),
    %% link Proxy process Event Channel process
    {_, Key} = iop_ior:get_key(EventChannel),
    Pid = orber_objectkeys:get_pid(Key),
    link(Pid),

    {ok, #consumer_admin{
       event_channel_pid = Pid,
       event_channel = EventChannel}}.

terminate(From, Reason) ->
    ok.

%%--------------------------------------------------------------------------
%% Func:	obtain_push_supplier/1
%% Args:		
%% Return:	Proxy Push Supplier object	
%% Comments: when the Proxy object created it;s process is automatically linked	
%% to the creator, i.e Admin.  We don't want that here.
%%--------------------------------------------------------------------------
obtain_push_supplier(OE_State) ->
    ProxyPushSupplier =
	'OrberEventChannelAdmin_ProxyPushSupplier':
	oe_create_link({OE_State#consumer_admin.event_channel,
			OE_State#consumer_admin.event_channel_pid}),

    {_, Key} = iop_ior:get_key(ProxyPushSupplier),
    Pid = orber_objectkeys:get_pid(Key),
    unlink(Pid),

    'OrberEventChannelAdmin_EventChannel':add_proxy_push_supplier(OE_State#consumer_admin.event_channel, ProxyPushSupplier),
    {ProxyPushSupplier, OE_State}.

%%--------------------------------------------------------------------------
%% Func:	obtain_pull_supplier /1
%% Args:		
%% Return:	Proxy Pull Supplier object	
%% Comments: when the Proxy object created it;s process is automatically linked	
%% to the creator, i.e Admin.  We don't want that here.
%%--------------------------------------------------------------------------
obtain_pull_supplier(OE_State) ->
    ProxyPullSupplier =
	'OrberEventChannelAdmin_ProxyPullSupplier':
	oe_create_link({OE_State#consumer_admin.event_channel,
			OE_State#consumer_admin.event_channel_pid}),

    {_, Key} = iop_ior:get_key(ProxyPullSupplier),
    Pid = orber_objectkeys:get_pid(Key),
    unlink(Pid),

    'OrberEventChannelAdmin_EventChannel':add_proxy_pull_supplier(OE_State#consumer_admin.event_channel, ProxyPullSupplier),
    {ProxyPullSupplier, OE_State}.


%%--------------------------------------------------------------------------
%% Func:		
%% Args:		
%% Return:	
%% Comments:	traps EXIT messages.  If Event channel dies it will die also
%%--------------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->

    case State#consumer_admin.event_channel_pid of 
	Pid -> 
	    {stop, normal, []};
	_ ->
	    {noreply, State}
    end.

