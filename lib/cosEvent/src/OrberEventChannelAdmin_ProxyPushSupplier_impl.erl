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
%% Module:	OrberEventChannelAdmin_ProxyPushSupplier_impl
%% Description:	Implementation of the ProxyPushSupplier interface that defines
%%		the second step for connecting push consumers to the event channel	
%% Created:	971001	
%% Modified:	
%%------------------------------------------------------------------------

-module('OrberEventChannelAdmin_ProxyPushSupplier_impl').

-include("event_service.hrl").
-include_lib("orber/include/corba.hrl").

-export([init/1, terminate/2, connect_push_consumer/3, disconnect_push_supplier/2, send_data/2, handle_info/2]).

%%------------------------------------------------------------------------
%% Standard methods for the interface
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
init({EventChannel, EventChannelPid}) ->
    process_flag(trap_exit, true),
    link(EventChannelPid),

    {ok, #proxy_supplier{ 
       event_channel_pid = EventChannelPid,
       event_channel = EventChannel}}.

%%------------------------------------------------------------------------
%% Func:
%% Args:
%% Return:
%%------------------------------------------------------------------------
terminate(From, Reason) ->
    ok.

%%------------------------------------------------------------------------
%% Func:	connect_push_consumer/3
%% Args:
%% Return: 'BAD_PARAM' exception if nil obeject reference is passed
%%		for PullSupplier
%%		'AlreadyConnected' exception if the ProxyPullConsumer already
%%		connected to a PullSupplier
%%------------------------------------------------------------------------
connect_push_consumer(OE_THIS, OE_State, PushConsumer) ->
    case corba_object:is_nil(PushConsumer) of
	true ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
	_ ->
	    case corba_object:is_nil(OE_State#proxy_supplier.consumer) of
		true ->
		    %% link Proxy process to PushConsumer process
		    {_, Key} = iop_ior:get_key(PushConsumer),
		    Pid = orber_objectkeys:get_pid(Key),
		    link(Pid),

		    {void, OE_State#proxy_supplier {consumer = PushConsumer,
						    consumer_pid = Pid}};
		_ ->
		    corba:raise(#'AlreadyConnected'{})
	    end
    end.

%%------------------------------------------------------------------------
%% Func:	disconnect_push_supplier/2
%% Args:
%% Return:
%% Comments:	Proxy will be removed from the EventChannel's list when
%% EC receives an EXIT signal from the proxy
%%------------------------------------------------------------------------
disconnect_push_supplier(OE_THIS, OE_State) ->
    case corba_object:is_nil(OE_State#proxy_supplier.consumer) of
	true ->
	    false;	% nothing to do
	_ ->
	    'CosEventComm_PushConsumer':disconnect_push_consumer(OE_State#proxy_supplier.consumer)
    end,

    %% should we rely on EXIT signal being caught instead ?
    'OrberEventChannelAdmin_EventChannel':rm_proxy_push_supplier(OE_State#proxy_supplier.event_channel, {OE_THIS, self()}),

						% kill the process
    {stop, normal, void, OE_State}.


%%------------------------------------------------------------------------
%% Func:	handle_info/2	
%% Args:		
%% Return:	
%% Comments: exits if receives an EXIT signal from event channel or from the
%% Consumer.  In case if EXIT received from event channel notify consumer
%% about the lose of connection
%%------------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->

    case State#proxy_supplier.consumer_pid of 
	Pid ->
	    {stop, normal, []};
	_ ->
	    case State#proxy_supplier.event_channel_pid of 
		Pid ->
		    case corba_object:is_nil(State#proxy_supplier.consumer) of
			true ->
			    false;
			_ ->
						% notify consumer about lose of connection
			    'CosEventComm_PushConsumer':disconnect_push_consumer(State#proxy_supplier.consumer)
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
%% Func:	send_data/2
%% Args:	Data of type ANY == #any{typecode, value}
%% Return:
%%------------------------------------------------------------------------
send_data(OE_State, Data) ->
    case corba_object:is_nil(OE_State#proxy_supplier.consumer) of
	false ->
	    case catch 'CosEventComm_PushConsumer':push(OE_State#proxy_supplier.consumer, Data) of
		{'EXCEPTION', #'Disconnected'{}} ->
		    {void, OE_State#proxy_supplier {consumer = ?NIL_OBJ_REF,
						    consumer_pid = {}}};
		_ ->
		    {void, OE_State}
	    end;
	_ ->
	    {void, OE_State}
    end.


