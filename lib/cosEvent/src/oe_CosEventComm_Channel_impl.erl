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
%% File        : oe_CosEventComm_Channel_impl.erl
%% Created     : 20 Mar 2001
%% Description : 
%%
%%----------------------------------------------------------------------
-module(oe_CosEventComm_Channel_impl).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include("cosEventApp.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% Mandatory
-export([init/1,
	 terminate/2,
	 code_change/3,
         handle_info/2]).

%% Exports from "CosEventChannelAdmin::EventChannel"
-export([for_consumers/3, 
	 for_suppliers/3, 
	 destroy/3]).
 

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
%% Exports from "oe_CosEventComm::Event"
-export([send/3, send_sync/4]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {typecheck, pull_interval, maxevents, blocking, cadmins = [],
		server_options}).

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
init([Options, ServerOpts]) ->
    process_flag(trap_exit, true),
    PullI = cosEventApp:get_option(?PULL_INTERVAL, Options),
    TC = cosEventApp:get_option(?TYPECHECK, Options),
    Max = cosEventApp:get_option(?MAXEVENTS, Options),
    Blocking = cosEventApp:get_option(?BLOCKING, Options),
    {ok, #state{typecheck = TC, pull_interval = PullI, maxevents = Max,
		blocking = Blocking, server_options = ServerOpts}}.

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
handle_info({'EXIT', Pid, Reason}, #state{cadmins = CAdmins} = State) ->
    ?DBG("Probably a child terminated with Reason: ~p~n", [Reason]),
    {noreply, State#state{cadmins = lists:keydelete(Pid, 2, CAdmins)}};
handle_info(Info, State) ->
    ?DBG("Unknown Info ~p~n", [Info]),
    {noreply, State}.


%%----------------------------------------------------------------------
%% Function   : for_consumers
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
for_consumers(_, _, #state{server_options = ServerOpts} = State) ->
    case catch 'oe_CosEventComm_CAdmin':oe_create_link([self(),
							State#state.typecheck,
							State#state.maxevents,
							ServerOpts],
						       [{sup_child, true}|ServerOpts]) of
	{ok, Pid, AdminCo} ->
	    ?DBG("Created a new oe_CosEventComm_CAdmin.~n", []),
	    {reply, AdminCo,
	     State#state{cadmins = [{AdminCo, Pid}|State#state.cadmins]}};
	Other ->
	    orber:dbg("[~p] oe_CosEventComm_Channel:for_consumers(); Error: ~p", 
		      [?LINE, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : for_suppliers
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
for_suppliers(OE_This, _, #state{server_options = ServerOpts} = State) ->
    case catch 'CosEventChannelAdmin_SupplierAdmin':oe_create_link([OE_This, self(),
								    State#state.typecheck, 
								    State#state.pull_interval,
								    ServerOpts],
								   [{sup_child, true}|ServerOpts]) of
	{ok, Pid, AdminSu} ->
	    ?DBG("Created a new CosEventChannelAdmin_SupplierAdmin.~n", []),
	    {reply, AdminSu, State};
	Other ->
	    orber:debug_level_print("[~p] oe_CosEventComm_Channel:for_suppliers(); Error: ~p", 
				    [?LINE, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : destroy
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
destroy(_, _, State) ->
    ?DBG("Destroy invoked.", []),
    {stop, normal, ok, State}.

%%----------------------------------------------------------------------
%% Function   : send
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send(OE_This, #state{cadmins = CAdmins} = State, Any) ->
    ?DBG("Received Event ~p~n", [Any]),
    case send_helper(CAdmins, Any, [], false) of
	ok ->
	    ?DBG("Received Event and forwarded it successfully.~n", []),
	    {noreply, State};
	{error, Dropped} ->
	    ?DBG("Received Event but forward failed for: ~p~n", [Dropped]),
	    RemainingAdmins = delete_cadmin(Dropped, CAdmins),
	    {noreply, State#state{cadmins = RemainingAdmins}}
    end.

%%----------------------------------------------------------------------
%% Function   : send_sync
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send_sync(OE_This, OE_From, #state{cadmins = CAdmins, blocking = BL} = State, Any) ->
    ?DBG("Received Event ~p~n", [Any]),
    corba:reply(OE_From, ok),
    case send_helper(CAdmins, Any, [], BL) of
	ok ->
	    ?DBG("Received Event and forwarded (sync) it successfully.~n", []),
	    {reply, ok, State};
	{error, Dropped} ->
	    ?DBG("Received Event but forward (sync) failed for: ~p~n", [Dropped]),
	    RemainingAdmins = delete_cadmin(Dropped, CAdmins),
	    {reply, ok, State#state{cadmins = RemainingAdmins}}
    end.


%%======================================================================
%% Internal functions
%%======================================================================
send_helper([], _, [], _) ->
    ok;
send_helper([], _, Dropped, _) ->
    {error, Dropped};
send_helper([{ObjRef, Pid}|T], Event, Dropped, false) ->
    case catch 'oe_CosEventComm_CAdmin':send(ObjRef, Event) of
	ok ->
	    send_helper(T, Event, Dropped, false);
	What ->
	    orber:debug_level_print("[~p] oe_CosEventComm_Channel:send_helper(~p, ~p); Bad return value ~p. Closing connection.", 
				    [?LINE, ObjRef, Event, What], ?DEBUG_LEVEL),
	    send_helper(T, Event, [{ObjRef, Pid}|Dropped], false)
    end;
send_helper([{ObjRef, Pid}|T], Event, Dropped, Sync) ->
    case catch 'oe_CosEventComm_CAdmin':send_sync(ObjRef, Event) of
	ok ->
	    send_helper(T, Event, Dropped, Sync);
	What ->
	    orber:debug_level_print("[~p] oe_CosEventComm_Channel:send_helper(~p, ~p); Bad return value ~p. Closing connection.", 
				    [?LINE, ObjRef, Event, What], ?DEBUG_LEVEL),
	    send_helper(T, Event, [{ObjRef, Pid}|Dropped], Sync)
    end.


delete_cadmin([], RemainingAdmins) ->
    RemainingAdmins;
delete_cadmin([{_,Pid}|T], CAdmins) ->
    Rest = lists:keydelete(Pid, 2, CAdmins),
    delete_cadmin(T, Rest).

%%======================================================================
%% END OF MODULE
%%======================================================================
