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
%%----------------------------------------------------------------------
%% File        : oe_CosEventComm_PusherS_impl.erl
%% Created     : 20 Mar 2001
%% Description : 
%%
%%----------------------------------------------------------------------
-module(oe_CosEventComm_PusherS_impl).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include("CosEventChannelAdmin.hrl").
-include("CosEventComm.hrl").
-include("cosEventApp.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_info/2]).

%% Exports from "CosEventChannelAdmin::ProxyPushSupplier"
-export([connect_push_consumer/4]).
 
%% Exports from "CosEventComm::PushSupplier"
-export([disconnect_push_supplier/3]).
 

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
%% Exports from "oe_CosEventComm::Event"
-export([send/3, send_sync/4]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {admin_pid, client, typecheck}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init([AdminPid, TypeCheck]) ->
    process_flag(trap_exit, true),
    {ok, #state{admin_pid = AdminPid, typecheck = TypeCheck}}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(Reason, #state{client = undefined}) ->
    ?DBG("Terminating ~p; no client connected.~n", [Reason]),
    ok;
terminate(Reason, #state{client = Client} = State) ->
    ?DBG("Terminating ~p~n", [Reason]),
    cosEventApp:disconnect('CosEventComm_PushConsumer', 
			   disconnect_push_consumer, Client),
    ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------%
%% function : handle_info
%% Arguments: 
%% Returns  : {noreply, State} | 
%%            {stop, Reason, State}
%% Effect   : Functions demanded by the gen_server module. 
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{admin_pid = Pid} = State) ->
    ?DBG("Parent Admin terminated ~p~n", [Reason]),
    orber:debug_level_print("[~p] oe_CosEventComm_PusherS_impl:handle_info(~p); 
My Admin terminated and so will I.", [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info(Info, State) ->
    ?DBG("Unknown Info ~p~n", [Info]),
    {noreply, State}.

%%---------------------------------------------------------------------%
%% Function   : connect_push_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
connect_push_consumer(OE_This, _, #state{client = undefined, 
				      typecheck = TypeCheck} = State, NewClient) ->
    case corba_object:is_nil(NewClient) of
	true ->
	    ?DBG("A NIL client supplied.~n", []),
	    orber:debug_level_print("[~p] oe_CosEventComm_PusherS_impl:connect_push_consumer(..); 
Supplied a NIL reference which is not allowed.", [?LINE], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
	false ->
	    cosEventApp:type_check(NewClient, 'CosEventComm_PushConsumer', TypeCheck),
	    ?DBG("Connected to client.~n", []),
	    {reply, ok, State#state{client = NewClient}}
    end;
connect_push_consumer(_, _, _, _) ->
    corba:raise(#'CosEventChannelAdmin_AlreadyConnected'{}).


%%---------------------------------------------------------------------%
%% Function   : disconnect_push_supplier
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
disconnect_push_supplier(OE_This, _, State) ->
    ?DBG("Disconnect invoked ~p ~n", [State]),
    {stop, normal, ok, State#state{client = undefined}}.

%%======================================================================
%% Internal functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : send
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send(OE_This, #state{client = undefined} = State, Any) ->
    %% No consumer connected.
    ?DBG("Received event ~p but have no client.~n", [Any]),
    {noreply, State};
send(OE_This, #state{client = Client} = State, Any) ->
    %% Push Data
    case catch 'CosEventComm_PushConsumer':push(Client, Any) of
	ok ->
	    ?DBG("Received event ~p and delivered it client.~n", [Any]),
	    {noreply, State};
	{'EXCEPTION', #'CosEventComm_Disconnected'{}} ->
	    ?DBG("Received event ~p but failed to deliver it since the client claims we are disconnected.~n", [Any]),
	    {stop, normal, State#state{client = undefined}};
	Other ->
	    ?DBG("Received event ~p but failed to deliver it to client.~n", [Any]),
	    orber:debug_level_print("[~p] oe_CosEventComm_PusherS_impl:send(~p); 
My Client behaves badly, returned ~p, so I will terminate.", 
				    [?LINE, Any, Other], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end.


%%----------------------------------------------------------------------
%% Function   : send_sync
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send_sync(OE_This, OE_From, #state{client = undefined} = State, Any) ->
    %% No consumer connected.
    ?DBG("Received event ~p but have no client.~n", [Any]),
    {reply, ok, State};
send_sync(OE_This, OE_From, #state{client = Client} = State, Any) ->
    corba:reply(OE_From, ok),
    %% Push Data
    case catch 'CosEventComm_PushConsumer':push(Client, Any) of
	ok ->
	    ?DBG("Received event ~p and delivered (sync) it client.~n", [Any]),
	    {noreply, State};
	{'EXCEPTION', #'CosEventComm_Disconnected'{}} ->
	    ?DBG("Received event ~p but failed to deliver (sync) it since the client claims we are disconnected.~n", [Any]),
	    {stop, normal, State#state{client = undefined}};
	Other ->
	    ?DBG("Received event ~p but failed to deliver (sync) it to client.~n", [Any]),
	    orber:debug_level_print("[~p] oe_CosEventComm_PusherS_impl:send_sync(~p); 
My Client behaves badly, returned ~p, so I will terminate.", 
				    [?LINE, Any, Other], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end.


%%======================================================================
%% END OF MODULE
%%======================================================================

