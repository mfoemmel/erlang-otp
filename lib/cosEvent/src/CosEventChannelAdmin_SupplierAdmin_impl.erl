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
%% Module:	CosEventChannelAdin_SupplierAdmin_impl
%% Description: Implementation of the SupplierAdmin interface that defines
%%			the first step for connecting suppliers to the event channel.
%%			This interface used by clients to obtain proxy consumers	
%% Created: 971001	
%% Modified:	
%%------------------------------------------------------------------------

-module('CosEventChannelAdmin_SupplierAdmin_impl').

-include("event_service.hrl").

-export([init/1, terminate/2, obtain_push_consumer/1, obtain_pull_consumer/1, handle_info/2]).

init(EventChannel) ->
    process_flag(trap_exit, true),
    %% link Supplier Admin process to Event Channel process
    {_, Key} = iop_ior:get_key(EventChannel),
    Pid = orber_objectkeys:get_pid(Key),
    link(Pid),

    {ok, #supplier_admin{
       event_channel_pid = Pid,
       event_channel = EventChannel}}.

terminate(From, Reason) ->
    ok.

%%------------------------------------------------------------------------
%% Func:	obtain_push_consumer	/1
%% Args:		
%% Return:	Proxy Push Consumer object	
%% Comments:	when an object created it is automatically linked to its
%% 		creator.  Here we don't want Proxy to be connected to the Admin
%%------------------------------------------------------------------------
obtain_push_consumer(OE_State) ->
    ProxyPushConsumer =
	'OrberEventChannelAdmin_ProxyPushConsumer':
	oe_create_link({OE_State#supplier_admin.event_channel,
		OE_State#supplier_admin.event_channel_pid}),

    {_, Key} = iop_ior:get_key(ProxyPushConsumer),
    Pid = orber_objectkeys:get_pid(Key),
    unlink(Pid),

    'OrberEventChannelAdmin_EventChannel':add_proxy_push_consumer(OE_State#supplier_admin.event_channel, ProxyPushConsumer),
    {ProxyPushConsumer, OE_State}.

%%------------------------------------------------------------------------
%% Func:	obtain_pull_consumer/1	
%% Args:		
%% Return:	
%% Comments:	when an object created it is automatically linked to its
%% 		creator.  Here we don't want Proxy to be connected to the Admin
%%------------------------------------------------------------------------
obtain_pull_consumer(OE_State) ->
    ProxyPullConsumer =
	'OrberEventChannelAdmin_ProxyPullConsumer':
	oe_create_link({OE_State#supplier_admin.event_channel,
		OE_State#supplier_admin.event_channel_pid}),

    {_, Key} = iop_ior:get_key(ProxyPullConsumer),
    Pid = orber_objectkeys:get_pid(Key),
    unlink(Pid),

    'OrberEventChannelAdmin_EventChannel':add_proxy_pull_consumer(OE_State#supplier_admin.event_channel, ProxyPullConsumer),
    {ProxyPullConsumer, OE_State}.

%%------------------------------------------------------------------------
%% Func:		
%% Args:		
%% Return:	
%% Comments:	Trap exit signals.  If Event Channel exits, so does the 
%%		SupplierAdmin
%%------------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->

    case State#supplier_admin.event_channel_pid of 
	Pid -> 
	    {stop, normal, []};
	_ ->
	    {noreply, State}
    end.

