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
%% File        : oe_CosEventComm_CAdmin_impl.erl
%% Created     : 20 Mar 2001
%% Description : 
%%
%%----------------------------------------------------------------------
-module(oe_CosEventComm_CAdmin_impl).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include("cosEventApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
         handle_info/2]).

%% Exports from "CosEventChannelAdmin::ConsumerAdmin"
-export([obtain_push_supplier/3, 
	 obtain_pull_supplier/3]).
 

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
%% Exports from "oe_CosEventComm::Event"
-export([send/3, send_sync/4]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {channel_pid, typecheck, maxevents, proxies = []}).

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
init([ChannelPid, TypeCheck, MaxEvents]) ->
    process_flag(trap_exit, true),
    {ok, #state{channel_pid = ChannelPid, typecheck = TypeCheck, 
		maxevents = MaxEvents}}.

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
    orber:debug_level_print("[~p] oe_CosEventComm_PullerS_impl:handle_info(~p); 
My Channel terminated and so will I which will cause my children to do the same thing.", 
			    [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, Reason, State};
handle_info({'EXIT', Pid, Reason}, #state{proxies = Proxies} = State) ->
    %% A child terminated which is normal. Hence, no logging.
    ?DBG("Probably a child terminated ~p~n", [Reason]),
    {noreply, State#state{proxies = lists:keydelete(Pid, 2, Proxies)}};
handle_info(Info, State) ->
    ?DBG("Unknown Info ~p~n", [Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Function   : obtain_push_supplier
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
obtain_push_supplier(OE_This, _, State) ->
    case catch 'oe_CosEventComm_PusherS':oe_create_link([self(), 
							 State#state.typecheck],
							[{sup_child, true}]) of
	{ok, Pid, Proxy} ->
	    ?DBG("Started a new oe_CosEventComm_PusherS.~n", []),
	    {reply, Proxy, State#state{proxies = [{Proxy, Pid}|State#state.proxies]}};
	Other ->
	    orber:debug_level_print("[~p] oe_CosEventComm_CAdmin:obtain_push_supplier(); Error: ~p", 
				    [?LINE, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : obtain_pull_supplier
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
obtain_pull_supplier(OE_This, _, State) ->
    case catch 'oe_CosEventComm_PullerS':oe_create_link([self(), 
							 State#state.typecheck, 
							 State#state.maxevents],
							[{sup_child, true}]) of
	{ok, Pid, Proxy} ->
	    ?DBG("Started a new oe_CosEventComm_PullerS.~n", []),
	    {reply, Proxy, State#state{proxies = [{Proxy, Pid}|State#state.proxies]}};
	Other ->
	    orber:debug_level_print("[~p] oe_CosEventComm_CAdmin:obtain_pull_supplier(); Error: ~p", 
				    [?LINE, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.


%%----------------------------------------------------------------------
%% Function   : send
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send(OE_This, #state{proxies = Proxies} = State, Any) ->
    ?DBG("Received Event ~p~n", [Any]),
    case send_helper(Proxies, Any, [], false) of
	ok ->
	    ?DBG("Received Event and forwarded it successfully.~n", []),
	    {noreply, State};
	{error, Dropped} ->
	    ?DBG("Received Event but forward failed to: ~p~n", [Dropped]),
	    RemainingProxies = delete_proxies(Dropped, Proxies),
	    {noreply, State#state{proxies = RemainingProxies}}
    end.

%%----------------------------------------------------------------------
%% Function   : send_sync
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send_sync(OE_This, OE_From, #state{proxies = Proxies} = State, Any) ->
    ?DBG("Received Event ~p~n", [Any]),
    corba:reply(OE_From, ok),
    case send_helper(Proxies, Any, [], true) of
	ok ->
	    ?DBG("Received Event and forwarded (sync) it successfully.~n", []),
	    {noreply, State};
	{error, Dropped} ->
	    ?DBG("Received Event but forward (sync) failed to: ~p~n", [Dropped]),
	    RemainingProxies = delete_proxies(Dropped, Proxies),
	    {noreply, State#state{proxies = RemainingProxies}}
    end.


%%======================================================================
%% Internal functions
%%======================================================================
send_helper([], _, [], _) ->
    ok;
send_helper([], _, Dropped, _) ->
    {error, Dropped};
send_helper([{ObjRef, Pid}|T], Event, Dropped, false) ->
    case catch 'oe_CosEventComm_Event':send(ObjRef, Event) of
	ok ->
	    send_helper(T, Event, Dropped, false);
	What ->
	    orber:debug_level_print("[~p] oe_CosEventComm_CAdmin:send_helper(~p, ~p); 
Bad return value ~p. Closing connection.", 
				    [?LINE, ObjRef, Event, What], ?DEBUG_LEVEL),
	    send_helper(T, Event, [{ObjRef, Pid}|Dropped], false)
    end;
send_helper([{ObjRef, Pid}|T], Event, Dropped, Sync) ->
    case catch 'oe_CosEventComm_Event':send_sync(ObjRef, Event) of
	ok ->
	    send_helper(T, Event, Dropped, Sync);
	What ->
	    orber:debug_level_print("[~p] oe_CosEventComm_CAdmin:send_helper(~p, ~p); 
Bad return value ~p. Closing connection.", 
				    [?LINE, ObjRef, Event, What], ?DEBUG_LEVEL),
	    send_helper(T, Event, [{ObjRef, Pid}|Dropped], Sync)
    end.

delete_proxies([], RemainingProxies) ->
    RemainingProxies;
delete_proxies([{_,Pid}|T], Proxies) ->
    Rest = lists:keydelete(Pid, 2, Proxies),
    delete_proxies(T, Rest).

%%======================================================================
%% END OF MODULE
%%======================================================================
