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
%% File        : CosEventChannelAdmin_SupplierAdmin_impl.erl
%% Created     : 21 Mar 2001
%% Description : 
%%
%%----------------------------------------------------------------------
-module('CosEventChannelAdmin_SupplierAdmin_impl').
 
 
%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("cosEventApp.hrl").

 
%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% Mandatory
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).
 
%% Exports from "CosEventChannelAdmin::SupplierAdmin"
-export([obtain_push_consumer/2, 
         obtain_pull_consumer/2]).
 
%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
 
%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {channel, channel_pid, typecheck, pull_interval}).
 
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
init([Channel, ChannelPid, TypeCheck, PullInterval]) ->
    process_flag(trap_exit, true),
    {ok, #state{channel = Channel, channel_pid = ChannelPid, typecheck = TypeCheck,
		pull_interval = PullInterval}}.
 
%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ?DBG("Terminating ~p~n", [Reason]),
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
handle_info({'EXIT', Pid, Reason}, #state{channel_pid = Pid} = State) ->
    ?DBG("Parent Channel terminated ~p~n", [Reason]),
    orber:debug_level_print("[~p] CosEventChannelAdmin_SupplierAdmin:handle_info(~p); 
My Channel terminated and so will I.", [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info(Info, State) ->
    ?DBG("Unknown Info ~p~n", [Info]),
    {noreply, State}.
 
 
%%----------------------------------------------------------------------
%% Function   : obtain_push_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
obtain_push_consumer(OE_This, #state{channel = Channel, 
				     channel_pid = ChannelPid,
				     typecheck = TypeCheck} = State) ->
    ?DBG("Starting a new CosEventChannelAdmin_ProxyPushConsumer.~n", []),
    {reply, 
     'CosEventChannelAdmin_ProxyPushConsumer':oe_create_link([OE_This, 
                                                              self(),
                                                              Channel,
							      TypeCheck]), 
     State}.
 
%%----------------------------------------------------------------------
%% Function   : obtain_pull_consumer
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
obtain_pull_consumer(OE_This, #state{channel = Channel, 
				     channel_pid = ChannelPid,
				     typecheck = TypeCheck,
				     pull_interval= PullInterval} = State) ->
    ?DBG("Starting a new CosEventChannelAdmin_ProxyPullConsumer.~n", []),
    {reply, 
     'CosEventChannelAdmin_ProxyPullConsumer':oe_create_link([OE_This, 
                                                              self(),
                                                              Channel,
							      TypeCheck,
							      PullInterval]), 
     State}.
 
%%======================================================================
%% Internal functions
%%======================================================================
 
%%======================================================================
%% END OF MODULE
%%======================================================================
