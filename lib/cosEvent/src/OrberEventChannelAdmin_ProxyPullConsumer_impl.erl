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
%% Module:	OrberEventChannelAdmin_ProxyPullConsumer_impl
%% Description:	Implementation of ProxyPullConsumer interface that defines
%%		the second step for connecting pull suppliers to the event channel
%% Created:	971001	
%% Modified:	
%%------------------------------------------------------------------------

-module('OrberEventChannelAdmin_ProxyPullConsumer_impl').

-include_lib("orber/include/corba.hrl").

-include("event_service.hrl").

-export([init/1, terminate/2, connect_pull_supplier/3, disconnect_pull_consumer/2, get_data/1, handle_info/2]).

%%------------------------------------------------------------------------
%% Standard methods of the interface
%%------------------------------------------------------------------------

%%------------------------------------------------------------------------
%% Func:
%% Args:
%% Return:
%%------------------------------------------------------------------------
init({EventChannel, EventChannelPid}) ->
	process_flag(trap_exit, true),

	link(EventChannelPid),

	{ok, #proxy_consumer{ 
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
%% Func: connect_pull_supplier/3
%% Args:
%% Return: 'BAD_PARAM' exception if nil object reference is passed
%%		for PullSupplier 
%%		'AlreadyConnected' exception is returned if the Proxy Pull Consumer
%% 		already connected to a PullSupplier
%%------------------------------------------------------------------------
connect_pull_supplier(OE_THIS, OE_State, PullSupplier) ->
	case corba_object:is_nil(PullSupplier) of
		true ->
			corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
		_ ->
			case corba_object:is_nil(OE_State#proxy_consumer.supplier) of
				true ->
					% link Proxy process to PullSupplier process
					{_, Key} = iop_ior:get_key(PullSupplier),
					Pid = orber_objectkeys:get_pid(Key),
					link(Pid),

					{void, OE_State#proxy_consumer {supplier = PullSupplier, supplier_pid = Pid}};
				_ ->
					corba:raise(#'AlreadyConnected'{})
			end
	end.
	

%%------------------------------------------------------------------------
%% Func:	disconnect_pull_consumer/2
%% Args:
%% Return:
%% Comments: proxy will be removed from the EventChannels' list when it 
%% receives an EXIT signal
%%------------------------------------------------------------------------
disconnect_pull_consumer(OE_THIS, OE_State) ->
	% check that the supplier has not been disconnected
	case corba_object:is_nil(OE_State#proxy_consumer.supplier) of
		true ->
			false; % do nothing => what should it be ???
		_ ->

			% notify Supplier about the disconnection
			'CosEventComm_PullSupplier':disconnect_pull_supplier(OE_State#proxy_consumer.supplier)
	end,

	%% should we rely on EXIT msg being caught ?
	'OrberEventChannelAdmin_EventChannel':rm_proxy_pull_consumer(OE_State#proxy_consumer.event_channel, {OE_THIS, self()}),

	{stop, normal, void, OE_State}.


%%------------------------------------------------------------------------
%% Func:		
%% Args:		
%% Return:	
%% Comments: if receive EXIT signal from Event Channel exit and notify 
%% supplier.  If receives EXIT signal from the supplier attached to it, 
%% proxy will exit also 
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
							false; % do nothing => what should it be ???
						_ ->
							% notify Supplier about the disconnection
							'CosEventComm_PullSupplier':disconnect_pull_supplier(State#proxy_consumer.supplier)
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
%% Func:	get_data/1
%% Args:
%% Return:	Data + event flag = {{TypeCode, Value}, Has_Event}
%%------------------------------------------------------------------------
get_data(OE_State) ->
 
    case corba_object:is_nil(OE_State#proxy_consumer.supplier) of
	false ->
	    case catch 'CosEventComm_PullSupplier':try_pull(OE_State#proxy_consumer.supplier) of
		{'EXCEPTION', #'Disconnected'{}} ->
		    {{#any{typecode=tk_long, value=0}, false},
		     OE_State#proxy_consumer {supplier = ?NIL_OBJ_REF,
					      supplier_pid = {}}};
		Data ->
		    {Data, OE_State}
	    end;
	_ ->
	    {{#any{typecode=tk_long, value=0}, false}, OE_State}
    end.

