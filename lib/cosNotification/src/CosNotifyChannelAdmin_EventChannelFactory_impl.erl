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
%%-------------------------------------------------------------------
%% File    : CosNotifyChannelAdmin_EventChannelFactory_impl.erl
%% Purpose : 
%% Created : 28 Sep 1999
%%-------------------------------------------------------------------

-module('CosNotifyChannelAdmin_EventChannelFactory_impl').

%%--------------- INCLUDES -----------------------------------
%% Application files
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").
-include("CosNotification_Definitions.hrl").

%%--------------- IMPORTS ------------------------------------

%%--------------- EXPORTS ------------------------------------
%% External
-export([create_channel/5,
	 get_all_channels/3,
	 get_event_channel/4]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- LOCAL DEFINITIONS --------------------------
%% Data structures
-record(state, {adminProp,
		idCounter = 0,
		options,
		etsR}).

%% Data structures constructors
-define(get_InitState(O), 
	#state{options = O,
	       etsR    = ets:new(oe_ets, [set, protected])}).

%% Data structures selectors
-define(get_channels(S),      lists:flatten(ets:match(S#state.etsR, {'_','$1','_'}))).
-define(get_channelIDs(S),    lists:flatten(ets:match(S#state.etsR, {'$1','_','_'}))).
-define(get_channel(S, I),    find_obj(ets:lookup(S#state.etsR, I))).
-define(get_options(S),       S#state.options).
-define(get_IdCounter(S),     S#state.idCounter).

%% Data structures modifiers
-define(add_channel(S,I,R,P), ets:insert(S#state.etsR, {I,R,P})).
-define(del_channel(S,I),     ets:delete(S#state.etsR, I)).
-define(del_channelPid(S,P),  ets:match_delete(S#state.etsR, {'_','_',P})).
-define(set_IdCounter(S,V),   S#state{idCounter=V}).
-define(new_Id(S),            'CosNotification_Common':create_id(S#state.idCounter)).

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: See gen_server documentation.
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(OldVsn, State, Extra) ->
    {ok, State}.

handle_info(Info, State) ->
    ?debug_print("INFO: ~p~n", [Info]),
    case Info of
        {'EXIT', Pid, normal} ->
	    ?del_channelPid(State,Pid),
            {noreply, State};
        Other ->
            ?debug_print("TERMINATED: ~p~n",[Other]),
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init(Env) ->
    process_flag(trap_exit, true),
    {ok, ?get_InitState(Env)}.

terminate(Reason, State) ->
    ok.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : create_channel
%% Arguments: InitQoS
%%            InitAdmin
%% Returns  : Ch - Channel obj ref
%%            Id - Channel Id (out-type)
%%-----------------------------------------------------------
create_channel(OE_THIS, OE_FROM, State, InitQoS, InitAdmin) ->
    {QoS, LQoS} = 'CosNotification_Common':init_qos(InitQoS),
    {IAdm, LAdm} = 'CosNotification_Common':init_adm(InitAdmin),
    Id = ?new_Id(State),
    case 'CosNotifyChannelAdmin_EventChannel':oe_create_link([OE_THIS, QoS, IAdm, 
							      LQoS, LAdm, 
							      ?get_options(State)],
							     [{sup_child, true}]) of
	{ok, Pid, Ch} ->
	    ?add_channel(State, Id, Ch, Pid),
	    {reply, {Ch, Id}, ?set_IdCounter(State, Id)};
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : get_all_channels
%% Arguments: -
%% Returns  : ChannelIDSeq - List of alive channels created 
%%            by this factory.
%%-----------------------------------------------------------
get_all_channels(OE_THIS, OE_FROM, State) ->
    {reply, ?get_channelIDs(State), State}.

%%----------------------------------------------------------%
%% function : get_event_channel
%% Arguments: ChannelId
%% Returns  : ChannelRef | 'CosNotifyChannelAdmin_ChannelNotFound'
%%-----------------------------------------------------------
get_event_channel(OE_THIS, OE_FROM, State, Id) ->
    {reply, ?get_channel(State, Id), State}.

%%--------------- LOCAL FUNCTIONS ----------------------------
find_obj([]) -> {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}};
find_obj([{_, Obj,_}]) -> Obj;
find_obj(_) -> {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}}.

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
