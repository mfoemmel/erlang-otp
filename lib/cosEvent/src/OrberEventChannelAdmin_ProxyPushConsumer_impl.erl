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
%% Module:	OrberEventChannelAdmin_ProxyPushConsumer
%% Description: Implementation of the ProxyPush Consumer interface that defines
%% 		the second step for connecting push suppliers to the event channel	
%% Author:	Helen Airiyan
%% Created:	971001	
%% Modified:	
%%------------------------------------------------------------------------

-module('OrberEventChannelAdmin_ProxyPushConsumer_impl').
-include("event_service.hrl").

-export([init/1, terminate/2, connect_push_supplier/3, push/2, disconnect_push_consumer/2, handle_info/2]).

%%------------------------------------------------------------------------
%% Standard methods of the interface
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
init({EventChannel, EventChannelPid}) ->
    process_flag(trap_exit, true),
    link(EventChannelPid),

    {ok, #proxy_consumer{ 
       event_channel_pid = EventChannelPid,
       event_channel = EventChannel}}.


%%------------------------------------------------------------------------
%%------------------------------------------------------------------------
terminate(From, Reason) ->
    ok.

%%------------------------------------------------------------------------
%% Func: 	connect_push_supplier/3
%% Args:	PushSupplier can be a NIL object reference
%% Returns: if the ProxyPushConsumer already connected to a PushSupplier
%%	then the 'AlreadyConnected' exception is raised
%%------------------------------------------------------------------------
connect_push_supplier(OE_THIS, OE_State, PushSupplier) ->

    case corba_object:is_nil(OE_State#proxy_consumer.supplier) of
	true ->
	    case OE_State#proxy_consumer.supplier_pid of
		secret ->
		    %% proxy has been connected to a NIL PushSupplier previously
		    corba:raise(#'AlreadyConnected'{});
		X ->
		    case corba_object:is_nil(PushSupplier) of
			true ->

			    %% can't link
			    {void, OE_State#proxy_consumer {supplier = PushSupplier,
							    supplier_pid = secret}};
			_ ->
			    %% link Proxy process with PushSupplier process
			    {_, Key} = iop_ior:get_key(PushSupplier),
			    Pid = orber_objectkeys:get_pid(Key),
			    link(Pid),

			    {void, OE_State#proxy_consumer {supplier = PushSupplier,
							    supplier_pid = Pid}}
		    end
	    end;
	_ ->
	    corba:raise(#'AlreadyConnected'{})
    end.


%%------------------------------------------------------------------------
%% Func:	push/2
%% Args:	Data of type ANY == {TypeCode, Value}
%% Return:	Throw a Disconnected exception if no PushSupplier attached to 
%%			the proxy
%%------------------------------------------------------------------------
push(OE_State, Data) ->
    case corba_object:is_nil(OE_State#proxy_consumer.supplier) of
	true ->
	    case OE_State#proxy_consumer.supplier_pid of
		secret ->
		    %% proxy has been connected to a NIL PushSupplier previously
		    Do_push = true;
		_ ->
		    Do_push = false
	    end;
	_ ->
	    Do_push = true
    end,

    case Do_push of
	true ->
	    'OrberEventChannelAdmin_EventChannel':push(OE_State#proxy_consumer.event_channel, Data),
	    {void, OE_State};
	_ ->
	    corba:raise(#'Disconnected'{})
    end.


%%------------------------------------------------------------------------
%% Func:	disconnect_push_consumer/2
%% Args:
%% Return:
%% Comments:	proxy will be removed from the EventChannel's list when 
%% it receives an EXIT signal
%%------------------------------------------------------------------------
disconnect_push_consumer(OE_THIS, OE_State) ->
    case corba_object:is_nil(OE_State#proxy_consumer.supplier) of
	true ->
	    %% possibly proxy is connected to a supplier that was passed as a
	    %% NIL object reference
	    false;	% nothing to do
	_ ->
	    'CosEventComm_PushSupplier':disconnect_push_supplier(OE_State#proxy_consumer.supplier)
    end,	

    'OrberEventChannelAdmin_EventChannel':rm_proxy_push_consumer(OE_State#proxy_consumer.event_channel, {OE_THIS, self()}), 

    {stop, normal, void, OE_State}.
						%{void, OE_State}.


%%------------------------------------------------------------------------
%% Func:		
%% Args:		
%% Return:	
%% Comments:	exits if receives EXIT signal from Event Channel or the 
%% supplier.  Resets the
%%------------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->

    case State#proxy_consumer.supplier_pid of 
	Pid -> 
	    {stop, normal, []};
	_ ->
	    case State#proxy_consumer.event_channel_pid of 
		Pid -> 
		    case corba_object:is_nil(State#proxy_consumer.supplier) of
			true ->
			    %% possibly proxy is connected to a supplier that was passed as a
			    %% NIL object reference
			    false;	% nothing to do
			_ ->
			    'CosEventComm_PushSupplier':disconnect_push_supplier(State#proxy_consumer.supplier)
		    end,	

		    {stop, normal, []};
		_ ->
		    {noreply, State}
	    end
    end.

