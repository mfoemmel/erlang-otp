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

-module(mesh_snmp).



-behaviour(gen_server).



-export([start_link/0,
	 register_alarms/1,
	 register_events/1
	]).


   %% Trap callbacks
-export([meshTooManyTypes/1,
	 meshTooManyMeasurements/1,
	 meshTypeCapacityExceeded/1,
	 meshThresholdTriggered/1,
	 meshTypeFailure/1,
	 meshMeasurementConnected/1,
	 meshMeasurementTerminated/1,
	 meshMeasurementUnconnected/1,
  	 meshNodeDown/1,
  	 meshNodeUp/1,
	 meshTypeConnected/1,
	 meshTypeUnconnected/1
	]).


   %% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2
	]).


   %% Test functions.
-export([state/0]).


   %% Instrumentation functions.
-export([type_table/1,
	 type_table/3,
	 type_info_table/1,
	 type_info_table/3,
	 meas_table/1,
	 meas_table/3,
	 meas_info_table/1,
	 meas_info_table/3,
	 thresh_table/1,
	 thresh_table/3,
	 reset_meas/1,
	 reset_meas/2,
	 watchdog_types/1,
	 watchdog_types/2,
	 watchdog_meas/1,
	 watchdog_meas/2,
	 current_types/1,
	 current_meas/1
	]).
	 


-include("mesh.hrl").
-include("mesh_snmp.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").
-include_lib("eva/include/eva.hrl").



%% Timeout for calls to the globally registered mesh_snmp server.
-define(TIME, 10000).

-define(SUBSCRIBED_EVENTS, [{table,mesh_type}, {table,mesh_meas}]).

-record(state, {reset_meas,
		wd_nof_types,
		wd_nof_meas,
	       	type_idx,
		meas_idx,
		thresh_idx}).



%%%*********************************************************************
%%%  EXPORTED FUNCTIONS
%%%*********************************************************************


register_alarms(Community) ->
    Alarms = [{meshTooManyTypes, meshTooManyTypes, snmpTrap, Community, 
	       {?MODULE,meshTooManyTypes}},
	      {meshTooManyMeasurements, meshTooManyMeasurements, snmpTrap, Community, 
	       {?MODULE,meshTooManyMeasurements}},
	      {meshTypeCapacityExceeded, meshTypeCapacityExceeded, snmpTrap, Community,
	       {?MODULE,meshTypeCapacityExceeded}},
	      {meshThresholdTriggered, meshThresholdTriggered, snmpTrap, Community,
	       {?MODULE,meshThresholdTriggered}}
	     ],
    eva_snmp_adaptation:register_alarms(Alarms).



register_events(Community) ->
    Events = [{meshTypeFailure, meshTypeFailure, snmpTrap, Community, 
	       {?MODULE,meshTypeFailure}},
	      {meshMeasurementTerminated, meshMeasurementTerminated, snmpTrap, Community, 
	       {?MODULE,meshMeasurementTerminated}},
  	      {meshNodeUp, meshNodeUp, snmpTrap, Community,
  	       {?MODULE,meshNodeUp}}, 
  	      {meshNodeDown, meshNodeDown, snmpTrap, Community,
  	       {?MODULE,meshNodeDown}},
	      {meshTypeUnconnected, meshTypeUnconnected, snmpTrap, Community, 
	       {?MODULE,meshTypeUnconnected}},
	      {meshMeasurementUnconnected, meshMeasurementUnconnected, snmpTrap, Community, 
	       {?MODULE,meshMeasurementUnconnected}},
	      {meshTypeConnected, meshTypeConnected, snmpTrap, Community, 
	       {?MODULE,meshTypeConnected}},
	      {meshMeasurementConnected, meshMeasurementConnected, snmpTrap, Community, 
	       {?MODULE,meshMeasurementConnected}}
	     ],
    eva_snmp_adaptation:register_events(Events).




meshTooManyTypes(#alarm{}) ->
    {value, Oid}  = snmp:name_to_oid(meshWatchDog),
    {ok, {Oid, [0,0], []}}.


meshTooManyMeasurements(#alarm{}) ->
    {value, Oid} = snmp:name_to_oid(meshWatchDog),
    {ok, {Oid, [0,0], []}}.


meshTypeCapacityExceeded(#alarm{cause={{type,Type},{allowed,Max},{currently,Curr}}}) ->
    {value, Oid} = snmp:name_to_oid(typeName),
    TypeStr = term2dispstr(Type),
    {ok, {Oid ++ TypeStr, [0,0], [{typeName, TypeStr, TypeStr},
				  {typeCurrentNofMeas, [0], Curr},
				  {typeAllowedNofMeas, [0], Max}]}}.


meshThresholdTriggered(#alarm{extra={{meas,MeasId},{id,ThreshId}}}) ->
    {value, Oid} = snmp:name_to_oid(threshEntry),
    MeasStr = term2dispstr(MeasId),
    ThreshStr = term2dispstr(ThreshId),
    {ok, {Oid ++ MeasStr ++ [ThreshId], [0,0], [{measId, MeasStr, MeasStr},
						{threshId, [ThreshId], ThreshId}]}}.



meshTypeFailure(#event{extra = {Name,Reason,Time}}) ->
    TypeStr = term2dispstr(Name),
    {ok, [{typeName, TypeStr, TypeStr}]}.



meshMeasurementTerminated(#event{extra = {MeasId,TypeName,Reason,Time}}) ->
    MeasStr = term2dispstr(MeasId),
    TypeStr = term2dispstr(TypeName),
    {ok, [{measId, MeasStr, MeasStr},
	  {typeName, TypeStr, TypeStr}]}.
    


meshNodeUp(#event{extra = {NodeName,Time}}) ->
    NodeNameStr = term2dispstr(NodeName),
    {ok, [{nodeName, [0], NodeNameStr}]}.



meshNodeDown(#event{extra = {NodeName,Time}}) ->
    NodeNameStr = term2dispstr(NodeName),
    {ok, [{nodeName, [0], NodeNameStr}]}.



meshTypeUnconnected(#event{extra = {Type,Reason,Time}}) ->
    TypeStr = term2dispstr(Type),
    {ok, [{typeName, TypeStr, TypeStr}]}.
    


meshMeasurementUnconnected(#event{extra = {MeasId,TypeName,Reason,Time}}) ->
    MeasStr = term2dispstr(MeasId),
    TypeStr = term2dispstr(TypeName),
    {ok, [{measId, MeasStr, MeasStr},
	  {typeName, TypeStr, TypeStr}]}.



meshTypeConnected(#event{extra = {Type,Reason,Time}}) ->
    TypeStr = term2dispstr(Type),
    {ok, [{typeName, TypeStr, TypeStr}]}.



meshMeasurementConnected(#event{extra = {MeasId,TypeName,Reason,Time}}) ->
    MeasStr = term2dispstr(MeasId),
    TypeStr = term2dispstr(TypeName),
    {ok, [{measId, MeasStr, MeasStr},
	  {typeName, TypeStr, TypeStr}]}.










type_table(new) ->
    done.

type_table(get, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_table,get,RowIndex,Cols}, ?TIME);
type_table(get_next, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_table,get_next,RowIndex,Cols}, ?TIME);
type_table(is_set_ok, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_table,is_set_ok,RowIndex,Cols}, ?TIME);
type_table(set, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_table,set,RowIndex,Cols}, ?TIME).




type_info_table(new) ->
    done.

type_info_table(get, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_info_table,get,RowIndex,Cols}, ?TIME);
type_info_table(get_next, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_info_table,get_next,RowIndex,Cols}, ?TIME);
type_info_table(is_set_ok, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_info_table,is_set_ok,RowIndex,Cols}, ?TIME);
type_info_table(set, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {type_info_table,set,RowIndex,Cols}, ?TIME).




meas_table(new) ->
    done.

meas_table(get, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {meas_table,get,RowIndex,Cols}, ?TIME);
meas_table(get_next, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {meas_table,get_next,RowIndex,Cols}, ?TIME);
meas_table(is_set_ok, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {meas_table,is_set_ok,RowIndex,Cols}, ?TIME);
meas_table(set, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {meas_table,set,RowIndex,Cols}, ?TIME).




meas_info_table(new) ->
    done.

meas_info_table(get, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {meas_info_table,get,RowIndex,Cols}, ?TIME);
meas_info_table(get_next, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {meas_info_table,get_next,RowIndex,Cols}, ?TIME).
%% We dont need any set-clauses for this table!




thresh_table(new) ->
    done.

thresh_table(get, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {thresh_table,get,RowIndex,Cols}, ?TIME);
thresh_table(get_next, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {thresh_table,get_next,RowIndex,Cols}, ?TIME);
thresh_table(is_set_ok, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {thresh_table,is_set_ok,RowIndex,Cols}, ?TIME);
thresh_table(set, RowIndex, Cols) ->
    gen_server:call({global,mesh_snmp}, {thresh_table,set,RowIndex,Cols}, ?TIME).





reset_meas(new) ->
    done;
reset_meas(get) ->
    gen_server:call({global,mesh_snmp}, {reset_meas,get}, ?TIME).


reset_meas(is_set_ok, Value) ->
    gen_server:call({global,mesh_snmp}, {reset_meas,is_set_ok,Value}, ?TIME);
reset_meas(set, Value) ->
    gen_server:call({global,mesh_snmp}, {reset_meas,set,Value}, ?TIME).




watchdog_types(new) ->
    done;
watchdog_types(get) ->
    gen_server:call({global,mesh_snmp}, {wd_nof_types,get}, ?TIME).

watchdog_types(is_set_ok, Value) when Value >= 0 ->
    {noError,0};
watchdog_types(is_set_ok, _Value) ->
    inconsistentValue;
watchdog_types(set, Value) ->
    gen_server:call({global,mesh_snmp}, {wd_nof_types,set,Value}, ?TIME).




watchdog_meas(new) ->
    done;
watchdog_meas(get) ->
    gen_server:call({global,mesh_snmp}, {wd_nof_meas,get}, ?TIME).

watchdog_meas(is_set_ok, Value) when Value >= 0 ->
    {noError,0};
watchdog_meas(is_set_ok, _Value) ->
    inconsistentValue;
watchdog_meas(set, Value) ->
    gen_server:call({global,mesh_snmp}, {wd_nof_meas,set,Value}, ?TIME).



current_types(new) ->
    done;
current_types(get) ->
    {value, mnesia:table_info(mesh_type,size) - length(?OWN_MEAS_TYPES)}.



current_meas(new) ->
    done;
current_meas(get) ->
    {value, mnesia:table_info(mesh_meas,size)}.





state() ->
    gen_server:call({global,mesh_snmp}, get_state, 1000).




%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------


start_link() ->
    gen_server:start_link(mesh_snmp, [], []).



%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%======================================================================
%% Function:      init
%%
%% Return Value:  {ok, State}          | 
%%                {ok, State, TimeOut} | 
%%                ignore               | 
%%                {stop, Reason}
%%
%% Description:   Initialises the mesh_server.
%%
%% Parameters:    None used.
%%======================================================================


init(_Any) ->
    process_flag(trap_exit, true),
    global:re_register_name(mesh_snmp, self()),
    lists:foreach(fun(Event) ->
			  mnesia:subscribe(Event)
		  end,
		  ?SUBSCRIBED_EVENTS),
    PrivDir = code:priv_dir(mesh),
    Mibs = [PrivDir ++ "mibs/OTP-MESH-MIB"],
    snmp:load_mibs(snmp_master_agent, Mibs),
    register_alarms("standard trap"),
    register_events("standard trap"),
    {WdTypes, WdMeas} = init_wd(),
    TypeIdx   = init_idx(get_types(), snmp_index:new(string)),
    MeasIdx   = init_idx(get_measurements(), snmp_index:new(string)),
    ThreshIdx = init_thresh_idx(get_thresholds(), snmp_index:new({string,integer})),
    {ok, #state{reset_meas   = ?UNDEFINED,
		wd_nof_types = WdTypes,
		wd_nof_meas  = WdMeas,
		type_idx     = TypeIdx, 
		meas_idx     = MeasIdx,
		thresh_idx   = ThreshIdx}}.



%%======================================================================
%% Function:      handle_call
%%
%% Return Value:  {reply, Reply, State}          |
%%                {reply, Reply, State, Timeout} |
%%                {noreply, State}               | 
%%                {noreply, State, Timeout}      |
%%                {stop, Reason, Reply, State}   |   (terminate/2 is called) 
%%                {stop, Reason, State}              (terminate/2 is called)
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================

handle_call(Msg, From, State) ->
    case Msg of
	
	   %% type_table requests

	{type_table, get, RowIndex, Cols} ->
	    Reply = table_func(get,RowIndex,Cols,type_table,State#state.type_idx,""),
	    {reply, Reply, State};

	{type_table, get_next, RowIndex, Cols} ->
	    Reply = table_func(get_next,RowIndex,Cols,type_table,State#state.type_idx,RowIndex),
	    {reply, Reply, State};

	{type_table, is_set_ok, RowIndex, Cols} ->
	    Reply = table_func(is_set_ok,RowIndex,Cols,type_table,State#state.type_idx,""),
	    {reply, Reply, State};

	{type_table, set, RowIndex, Cols} ->
	    {Reply,NewIdx} = table_func(set,RowIndex,Cols,type_table,State#state.type_idx,""),
	    {reply, Reply, State#state{type_idx=NewIdx}};


	   %% type_info_table requests

	{type_info_table, get, RowIndex, Cols} ->
	    Reply = table_func(get,RowIndex,Cols,type_info_table,State#state.type_idx,""),
	    {reply, Reply, State};

	{type_info_table, get_next, RowIndex, Cols} ->
	    Reply = table_func(get_next,RowIndex,Cols,type_info_table,State#state.type_idx,
			       RowIndex),
	    {reply, Reply, State};


	   %% meas_table requests

	{meas_table, get, RowIndex, Cols} ->
	    Reply = table_func(get,RowIndex,Cols,meas_table,State#state.meas_idx,""),
	    {reply, Reply, State};

	{meas_table, get_next, RowIndex, Cols} ->
	    Reply = table_func(get_next,RowIndex,Cols,meas_table,State#state.meas_idx,RowIndex),
	    {reply, Reply, State};

	{meas_table, is_set_ok, RowIndex, Cols} ->
	    Reply = table_func(is_set_ok,RowIndex,Cols,meas_table,State#state.meas_idx,""),
	    {reply, Reply, State};

	{meas_table, set, RowIndex, Cols} ->
	    {Reply,NewIdx} = table_func(set,RowIndex,Cols,meas_table,State#state.meas_idx,""),
	    {reply, Reply, State#state{meas_idx=NewIdx}};


	   %% meas_info_table requests

	{meas_info_table, get, RowIndex, Cols} ->
	    Reply = table_func(get,RowIndex,Cols,meas_info_table,State#state.meas_idx,""),
	    {reply, Reply, State};

	{meas_info_table, get_next, RowIndex, Cols} ->
	    Reply = table_func(get_next,RowIndex,Cols,meas_info_table,State#state.meas_idx,
			       RowIndex),
	    {reply, Reply, State};


	
	{thresh_table, get, RowIndex, Cols} ->
	    #state{meas_idx   = MeasIdx,
		   thresh_idx = ThreshIdx} = State,
	    Reply = thresh_table_func(get, RowIndex, Cols, MeasIdx, ThreshIdx),
	    {reply, Reply, State};

	{thresh_table, get_next, RowIndex, Cols} ->
	    #state{meas_idx   = MeasIdx,
		   thresh_idx = ThreshIdx} = State,
	    Reply = thresh_table_func(get_next, RowIndex, Cols, MeasIdx, ThreshIdx),
	    {reply, Reply, State};

	{thresh_table, is_set_ok, RowIndex, Cols} ->
	    #state{meas_idx   = MeasIdx,
		   thresh_idx = ThreshIdx} = State,
	    Reply = thresh_table_func(is_set_ok, RowIndex, Cols, MeasIdx, ThreshIdx),
	    {reply, Reply, State};

	{thresh_table, set, RowIndex, Cols} ->
	    #state{meas_idx   = MeasIdx,
		   thresh_idx = ThreshIdx} = State,
	    {Reply,NewThreshIdx} = thresh_table_func(set, RowIndex, Cols, MeasIdx, ThreshIdx),
	    {reply, Reply, State#state{thresh_idx=NewThreshIdx}};




	{reset_meas, get} ->
	    Reply = case State#state.reset_meas of
			?UNDEFINED -> {noValue, unSpecified};
			Value ->      {value, Value}
		    end,
	    {reply, Reply, State};


	{reset_meas, is_set_ok, RowIndex} ->
	    Reply = reset_meas(is_set_ok, RowIndex, State),
	    {reply, Reply, State};

	
	{reset_meas, set, RowIndex} ->
	    {Reply, NewState} = reset_meas(set, RowIndex, State),
	    {reply, Reply, NewState};


	{wd_nof_types, get} ->
	    Reply = case State#state.wd_nof_types of
			infinity -> {value, 1000000000};
			Value ->    {value, Value}
		    end,
	    {reply, Reply, State};


	{wd_nof_types, set, NofTypes} ->
	    NofMeas = State#state.wd_nof_meas,
	    mesh:watchdog_setup(NofTypes, NofMeas),
	    {reply, noError, State#state{wd_nof_types = NofTypes}};


	{wd_nof_meas, get} ->
	    Reply = case State#state.wd_nof_meas of
			infinity -> {value, 1000000000};
			Value ->    {value, Value}
		    end,
	    {reply, Reply, State};


	{wd_nof_meas, set, NofMeas} ->
	    NofTypes = State#state.wd_nof_types,
	    mesh:watchdog_setup(NofTypes, NofMeas),
	    {reply, noError, State#state{wd_nof_meas = NofMeas}};



	   %% Debug information requests

	get_state ->
	    {reply, State, State};
	
	
	   %% Other requests

	Other ->
	    {reply, {genError,0}, State}

    end.
    

%%======================================================================
%% Function:      handle_cast
%%
%% Return Value:  {noreply, State}          |
%%                {noreply, State, Timeout} |
%%                {stop, Reason, State}            (terminate/2 is called)
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================

handle_cast(Msg, State) ->
    case Msg of

	_Other ->
	    {noreply, State}

    end.


%%======================================================================
%% Function:      handle_info
%%
%% Return Value:  {noreply, State}          |
%%                {noreply, State, Timeout} |
%%                {stop, Reason, State}            (terminate/2 is called)
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================

handle_info(Info, State) ->
    case Info of
	{mnesia_table_event, {Action,Rec,_TransId}} ->
	    NewState = update_idx(Action, Rec, State),
	    {noreply, NewState};

	{'EXIT', Sup, Reason} when pid(Sup) ->
	    {stop, Reason, State};

	_Other ->
	    io:format("Received: ~p~n", [Info]),
	    {noreply, State}
    end.


%%======================================================================
%% Function:      terminate
%%
%% Return Value:  Any (ignored by gen_server).
%%
%% Description:   Shuts down the server.
%%
%% Parameters:    
%%======================================================================

terminate(Reason, State) ->
    lists:foreach(fun(Event) ->
			  mnesia:unsubscribe(Event)
		  end,
		  ?SUBSCRIBED_EVENTS),
    ok.



%%%*********************************************************************
%%%  INTERNAL FUNCTIONS
%%%*********************************************************************



init_idx([{RowStatus,NameOrId} | T], Idx) ->
    NewIdx = snmp_index:insert(Idx, lists:flatten(io_lib:write(NameOrId)), {RowStatus,NameOrId}),
    init_idx(T, NewIdx);
init_idx([], Idx) ->
    Idx.
    


init_thresh_idx([{ThreshType, RowStatus, {MeasId,ThreshId}, ThreshRec} | T],Idx) when integer(ThreshId) ->
    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
    NewIdx = snmp_index:insert(Idx, 
			       {MeasIdStr,ThreshId}, 
			       {ThreshType,RowStatus,MeasId,ThreshId,
				ThreshRec#threshold{thresh_type=ThreshType}}),
    init_thresh_idx(T, NewIdx);
init_thresh_idx([{ThreshType, RowStatus, {MeasId,ThreshId}, ThreshRec} | T],Idx) ->
    init_thresh_idx(T, Idx);    
init_thresh_idx([], Idx) ->
    Idx.
    


init_wd() ->
    {ok, WdRec} = get_row(type_table, mesh_watchdog),
    WdRec#mesh_type.max_nof_inst.




update_idx(write, Rec, State) when record(Rec, mesh_type) ->
    TypeName = Rec#mesh_type.name,
    case lists:member(TypeName, ?OWN_MEAS_TYPES) of
	true ->  %% Shall not be visible!
	    case TypeName of
		mesh_watchdog ->
		    {NofTypes, NofMeas} = Rec#mesh_type.max_nof_inst,
		    State#state{wd_nof_types = NofTypes,
				wd_nof_meas  = NofMeas};
		_Other ->
		    State
	    end;
	false ->
	    {RowStatus,NameOrRec} = 
		case Rec#mesh_type.type_failure of
		    false ->
			{?active,TypeName};
		    true ->
			{?notInService,Rec}
		end,
	    State#state{type_idx=snmp_index:insert(State#state.type_idx, 
						   lists:flatten(io_lib:write(TypeName)), 
						   {RowStatus, NameOrRec})}
    end;
update_idx(write, Rec, State) when record(Rec, mesh_meas) ->
    #mesh_meas{id               = MeasId,
	       oper_state       = OperState,
	       upper_thresholds = UpperThresh,
	       lower_thresholds = LowerThresh}  = Rec,
    
    {RowStatus,NameOrRec} = 
	case OperState of
	    disabled ->
		{?notInService,MeasId};
	    enabled ->
		{?active,Rec}
	end,

    RowIndex     = lists:flatten(io_lib:write(MeasId)),
    MeasIdx      = State#state.meas_idx,
    ThreshIdx    = State#state.thresh_idx,
    NewMeasIdx   = snmp_index:insert(MeasIdx,RowIndex,{RowStatus,MeasId}),
    NewThreshIdx = update_thresh_idx(RowStatus,MeasId,UpperThresh,LowerThresh,ThreshIdx),
    State#state{meas_idx   = NewMeasIdx,
		thresh_idx = NewThreshIdx
	       };
update_idx(delete_object, Rec, State) when record(Rec, mesh_type) ->
    TypeName = Rec#mesh_type.name,
    case lists:member(TypeName, ?OWN_MEAS_TYPES) of
  	true ->  %% Shall not be visible!
  	    State;
  	false ->
	       %% Maybe it already has been deleted by us? Only if deleted by another Erlang
	       %% application do we want to store data about it!
	    RowIndex     = lists:flatten(io_lib:write(TypeName)),
	    SnmpRowIndex = [length(RowIndex) | RowIndex],
	    Idx          = State#state.type_idx,
	    case snmp_index:get(Idx, SnmpRowIndex) of
		undefined ->
		    State;
		_Other ->
		    State#state{type_idx=snmp_index:insert(Idx, RowIndex, {?notInService,Rec})}
	    end
    end;
update_idx(delete_object, Rec, State) when record(Rec, mesh_meas) ->
    #mesh_meas{id               = MeasId,
	       upper_thresholds = UpperThresh,
	       lower_thresholds = LowerThresh}  = Rec,
    
       %% Maybe it already has been deleted by us? Only if deleted by another Erlang
       %% application do we want to store data about it!
    RowIndex     = lists:flatten(io_lib:write(MeasId)),
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    MeasIdx      = State#state.meas_idx,
    ThreshIdx    = State#state.thresh_idx,
    case snmp_index:get(MeasIdx, SnmpRowIndex) of
	undefined -> 
	       %% Object deleted previously by us! Now delete
	       %% corresponding thresholds!  :-)  :-)  :-)
	       %% Doesn't matter here what row status we set!!!
	    NewThreshIdx = update_thresh_idx(deleted,MeasId,[],[],ThreshIdx),
	    State#state{thresh_idx = NewThreshIdx};
	_Other ->
	    NewMeasIdx   = snmp_index:insert(MeasIdx,RowIndex,{?notInService,Rec}),
	    NewThreshIdx = update_thresh_idx(?notInService,MeasId,UpperThresh,LowerThresh,
					     ThreshIdx),
	    State#state{meas_idx   = NewMeasIdx,
			thresh_idx = NewThreshIdx}
    end.
    




update_thresh_idx(MeasRowStatus, MeasId, UpperThresh, LowerThresh, Idx) ->
       %% Easiest way to do this (not the most efficient, but the easiest,
       %% doesn't have much time to implement this! :-(  ) is to first
       %% mark all active thresholds (associated with the measurement object in question),
       %% as notInService, then update the index structure with the current active thresholds!

    UpdatedIdx = 
	case MeasRowStatus of
	    deleted ->
		remove_thresholds(MeasId, Idx);
	    _OtherStatus ->
		mark_active_thresholds(MeasId, Idx)
	end,

    NewStatus = 
	case MeasRowStatus of
	    ?active ->
		?active;
	    _Other ->
		?notInService
	end,

    NewUpperThresh = 
	[{upper, NewStatus, {MeasId,X#threshold.id}, X} || X <- UpperThresh],
    NewLowerThresh = 
	[{lower, NewStatus, {MeasId,X#threshold.id}, X} || X <- LowerThresh],
    NewThresh = NewUpperThresh ++ NewLowerThresh,
    init_thresh_idx(NewThresh, UpdatedIdx).




remove_thresholds(MeasId, Idx) ->
    remove_thresholds(Idx, snmp_index:get_next(Idx,""), MeasId).


remove_thresholds(Idx, undefined, _MeasId) ->
    Idx;
remove_thresholds(Idx, {ok,{SnmpRowIndex,{TT,TS,MeasId,TId,TRec}}}, MeasId) ->
    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
    NewIdx = snmp_index:delete(Idx, {MeasIdStr,TId}),
    remove_thresholds(NewIdx, snmp_index:get_next(NewIdx,SnmpRowIndex), MeasId);
remove_thresholds(Idx, {ok,{SnmpRowIndex,_Any}}, MeasId) ->
    remove_thresholds(Idx, snmp_index:get_next(Idx,SnmpRowIndex), MeasId).




mark_active_thresholds(MeasId, Idx) ->
    mark_active_thresholds(Idx, snmp_index:get_next(Idx,""), MeasId).


mark_active_thresholds(Idx, undefined, _MeasId) ->
    Idx;
mark_active_thresholds(Idx, {ok,{SnmpRowIndex,{TT,?active,MeasId,TId,TRec}}}, MeasId) ->
    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
    NewIdx = snmp_index:insert(Idx, {MeasIdStr,TId}, {TT,?notInService,MeasId,TId,TRec}),
    mark_active_thresholds(NewIdx, snmp_index:get_next(NewIdx,SnmpRowIndex), MeasId);
mark_active_thresholds(Idx, {ok,{SnmpRowIndex,_Any}}, MeasId) ->
    mark_active_thresholds(Idx, snmp_index:get_next(Idx,SnmpRowIndex), MeasId).
    





get_types() ->
    Q = query
	    [{X#mesh_type.type_failure, X#mesh_type.name} || X <- table(mesh_type)]
	end,
    F = fun() ->
		mnemosyne:eval(Q)
	end,
    {atomic, RawContent} = 
	mnesia:transaction(F),
    Content = lists:map(fun({false,TName}) ->
				 {?active, TName};
			    ({true,TName}) ->
				 {?notInService, TName}
			 end,
			RawContent),
    lists:foldl(fun(OwnMeasType, Acc) ->
			lists:keydelete(OwnMeasType, 2, Acc)
		end, Content, ?OWN_MEAS_TYPES).




get_measurements() ->
    Q = query
	    [{X#mesh_meas.oper_state, X#mesh_meas.id} || X <- table(mesh_meas)]
	end,
    F = fun() ->
		mnemosyne:eval(Q)
	end,
    {atomic, RawContent} = 
	mnesia:transaction(F),
    lists:map(fun({disabled,MId}) ->
		      {?notInService, MId};
		 ({enabled,MId}) ->
		      {?active, MId}
	      end,
	      RawContent).





get_thresholds() ->
       %% If thresholds are present in the active measurement objects, they are
       %% of course active too!
    Q = query
	    [{X#mesh_meas.id, 
	      X#mesh_meas.upper_thresholds, 
	      X#mesh_meas.lower_thresholds} || X <- table(mesh_meas)]
	end,
    F = fun() ->
		mnemosyne:eval(Q)
	end,
    {atomic, RawContent} = 
	mnesia:transaction(F),
    UpperThresh = 
	lists:foldl(fun({MId, TU, _TL}, Acc) ->
			    Acc ++ [{upper, ?active, {MId,X#threshold.id}, X} || X <- TU]
		    end, 
		    [], 
		    RawContent),
    
    LowerThresh = 
	lists:foldl(fun({MId, _TU, TL}, Acc) ->
			    Acc ++ [{lower, ?active, {MId,X#threshold.id}, X} || X <- TL]
		    end, 
		    [], 
		    RawContent),
    UpperThresh ++ LowerThresh.


    



reset_meas(is_set_ok, RowIndex, State) ->
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    MeasIdx = State#state.meas_idx,
    case snmp_index:get(MeasIdx, SnmpRowIndex) of
	undefined ->
	    {noValue, noSuchObject};
	{ok, {SnmpRowIndex, {_RowStatus,_KeyOrRec}}} ->
	    noError
    end;
reset_meas(set, RowIndex, State) ->
       %% Has to check again that object still is there...  :-/
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    MeasIdx = State#state.meas_idx,
    case snmp_index:get(MeasIdx, SnmpRowIndex) of
	undefined ->
	    {commitFailed, State};
	{ok, {SnmpRowIndex, {RowStatus,_Rec}}} when RowStatus /= ?active ->
	    {noError, State#state{reset_meas = RowIndex}};
	{ok, {SnmpRowIndex, {?active,MeasId}}} ->
	    case get_row(meas_table, MeasId) of
		not_found ->
		    {commitFailed, State};
		{ok, Rec} ->
		    ResetArgs = Rec#mesh_meas.init_args,
		    mesh:reset_measurement(MeasId, ResetArgs),
		    {noError, State#state{reset_meas = RowIndex}}
	    end
    end.





table_func(get, RowIndex, Cols, Table, Idx, _StopRowIndex) ->
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	undefined ->
	    {noValue, noSuchObject};
	{ok, {SnmpRowIndex, {RowStatus,KeyOrRec}}} ->
	    get_cols(Table, RowIndex, {RowStatus,KeyOrRec}, Cols)
    end;
table_func(get_next, RowIndex, Cols, Table, Idx, StopRowIndex) ->
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    case snmp_index:get_next(Idx, SnmpRowIndex) of
	undefined ->
	    case snmp_index:get_next(Idx, "") of
		undefined ->
		    end_of_table(Cols);
		{ok, {NextSnmpRowIndex, {NextRowStatus,NextRowKeyOrRec}}} ->
		    NewCols = add_one_to_cols(Cols),
		    get_next_cols(Table, NextSnmpRowIndex, {NextRowStatus,NextRowKeyOrRec},
				  NewCols, Idx, StopRowIndex)
	    end;
	{ok, {NextSnmpRowIndex, {NextRowStatus,NextRowKeyOrRec}}} ->
	    get_next_cols(Table, NextSnmpRowIndex, {NextRowStatus,NextRowKeyOrRec}, Cols, 
			  Idx, StopRowIndex)
    end;
table_func(is_set_ok, RowIndex, Cols, Table, Idx, _StopRowIndex) ->
    check_if_set_ok(RowIndex, Cols, Table, Idx);
table_func(set, RowIndex, Cols, Table,  Idx, _StopRowIndex) ->
    case is_row_status_col_changed(Table, Cols) of
	{true, ?active} ->
	    set_row_active(Table, Idx, RowIndex, Cols);
	{true, ?createAndGo} ->
	    set_row_active(Table, Idx, RowIndex, Cols);
	{true, ?notInService} ->
	    set_row_notInService(Table, Idx, RowIndex, Cols);
	{true, ?createAndWait} ->
	    set_row_notReady(Table, Idx, RowIndex, Cols);
	{true, ?destroy} ->
	    delete_row(Table, Idx, RowIndex, Cols);
	false ->
	    update_row(Table, Idx, RowIndex, Cols)
    end.
    




thresh_table_func(get, RowIndex, Cols, _MeasIdx, ThreshIdx) ->
       %% Last element in RowIndex is the threshold identifier, i.e., an integer!
    SnmpRowIndex = [(length(RowIndex) - 1) | RowIndex],
    case snmp_index:get(ThreshIdx, SnmpRowIndex) of
	undefined ->
	    {noValue, noSuchObject};
	{ok, {SnmpRowIndex, {ThreshType,RowStatus,MeasId,ThreshId,ThreshRec}}} ->
	    get_thresh_cols(ThreshType, Cols, SnmpRowIndex, ThreshRec, RowStatus)
    end;
thresh_table_func(get_next, RowIndex, Cols, _MeasIdx, ThreshIdx) ->
    SnmpRowIndex = [(length(RowIndex) - 1) | RowIndex],
    case snmp_index:get_next(ThreshIdx, SnmpRowIndex) of
	undefined ->
	    case snmp_index:get_next(ThreshIdx, "") of
		undefined ->
		    end_of_table(Cols);
		{ok,{NextSnmpRowIndex,
		     {ThreshType,NextRowStatus,MeasId,ThreshId,NextThreshRec}}} ->
		    NewCols = add_one_to_cols(Cols),
		    get_next_thresh_cols(ThreshType, NewCols, NextSnmpRowIndex,
					 NextThreshRec, NextRowStatus)
	    end;
	{ok, {NextSnmpRowIndex,{ThreshType,NextRowStatus,MeasId,ThreshId,NextThreshRec}}} -> 
	    get_next_thresh_cols(ThreshType, Cols, NextSnmpRowIndex, 
				 NextThreshRec, NextRowStatus)
    end;
thresh_table_func(is_set_ok, RowIndex, Cols, MeasIdx, ThreshIdx) ->
    ThreshId = lists:last(RowIndex),
    if 
	integer(ThreshId) ->
	    check_if_thresh_set_ok(RowIndex, Cols, MeasIdx, ThreshIdx);
	true ->
	    {inconsistentName, ?ThreshId}
    end;
thresh_table_func(set, RowIndex, Cols, MeasIdx, ThreshIdx) ->
    {MeasId, ThreshId} = rowindex2measthreshid(RowIndex),
    case is_row_status_col_changed(thresh_table, Cols) of
  	{true, ?active} ->
  	    set_thresh_row_active(MeasId, ThreshId, ThreshIdx, RowIndex, Cols);
  	{true, ?createAndGo} ->
  	    set_thresh_row_active(MeasId, ThreshId, ThreshIdx, RowIndex, Cols);
  	{true, ?notInService} ->
  	    set_thresh_row_notInService(MeasId, ThreshId, ThreshIdx, RowIndex, Cols);
  	{true, ?createAndWait} ->
  	    set_thresh_row_notReady(MeasId, ThreshId, ThreshIdx, RowIndex, Cols);
  	{true, ?destroy} ->
  	    delete_thresh_row(MeasId, ThreshId, ThreshIdx, RowIndex, Cols);
  	false ->
	    update_thresh_row(MeasId, ThreshId, ThreshIdx, RowIndex, Cols)
    end.






update_row(Table, Idx, RowIndex, Cols) ->
       %% If this function is ever reached, we can safely assume that the
       %% row already exists. Otherwise the other case-clauses in the set-part
       %% of the table_func function should have catched it, or the is_set_ok
       %% part wouldn't allow this function to be reached, alternatively.
       %% So: the possible previous states are notReady, notInService or active.
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	{ok, {SnmpRowIndex, {?notInService, KeyOrRec}}} ->
	    set_row_notInService(Table, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {?notReady, KeyOrRec}}} ->
	    set_row_notReady(Table, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {?active, KeyOrRec}}} ->
	    update_active_row(Table, KeyOrRec, Idx, RowIndex, Cols)
    end.




update_active_row(Table, RowKey, Idx, RowIndex, Cols) ->
       %% May still be a race condition here, i.e., object removed from Mnesia...
    case get_row(Table, RowKey) of
	not_found ->
	    [{Col,Val} | ColT] = Cols,
	    {{commitFailed,Col}, Idx};
	{ok, RowRec} ->
	       %% Instead of fuzzing with ordering of columns/updates,
	       %% get all values available from the beginning!
	    UpdatedRowRec = insert_col_values(Table, Cols, RowRec),
	    update_active_row2(Table, RowKey, UpdatedRowRec, Cols),
	    {{noError,0}, Idx}
    end.




update_active_row2(type_table, TypeName, Rec, [{Col,Val} | T]) ->
    update_active_row3(type_table, TypeName, Rec, Col),
    update_active_row2(type_table, TypeName, Rec, T);
update_active_row2(meas_table, MeasId, Rec, [{Col,Val} | T]) ->
    update_active_row3(meas_table, MeasId, Rec, Col),
    update_active_row2(meas_table, MeasId, Rec, T);
update_active_row2(_Table, _NameOrId, _Rec, []) ->
    done.




update_active_row3(type_table, TypeName, Rec, ?TypeInfo) ->
    #mesh_type{info         = Info,
	       callback_mod = CbMod,
	       max_nof_inst = MaxInst,
	       admin_state  = AdmState,
	       meas_args    = LockArgs} = Rec,
    mesh:register_type(TypeName, Info, CbMod, MaxInst, AdmState, LockArgs);
update_active_row3(type_table, TypeName, Rec, ?TypeAdminState) ->
    #mesh_type{admin_state = AdmState,
	       meas_args   = LockArgs} = Rec,
    case AdmState of
	locked ->
	    mesh:lock_type(TypeName, LockArgs);
	shutting_down ->
	    mesh:shut_down_type(TypeName);
	unlocked ->
	    mesh:unlock_type(TypeName)
    end;
update_active_row3(type_table, TypeName, Rec, ?TypeMeasArgs) ->
       %% Only need to update the lock args, the rest is
       %% handled when type is explicitly locked.
    LockArgs = Rec#mesh_type.meas_args,
    mesh:set_lock_args(TypeName, LockArgs);
update_active_row3(type_table, TypeName, Rec, ?TypeMaxInst) ->
    #mesh_type{info         = Info,
	       callback_mod = CbMod,
	       max_nof_inst = MaxInst,
	       admin_state  = AdmState,
	       meas_args    = LockArgs} = Rec,
    mesh:register_type(TypeName, Info, CbMod, MaxInst, AdmState, LockArgs);
update_active_row3(meas_table, MeasId, Rec, ?MeasAdminState) ->
    #mesh_meas{admin_state = AdmState,
	       init_args   = Args}  = Rec,
    case AdmState of
	started ->
	    mesh:start_measurement(MeasId, Args);
	stopped ->
	    mesh:stop_measurement(MeasId)
    end;
update_active_row3(_Table, _Key, _Rec, _Col) ->
    done.
    
    



set_row_notReady(Table, Idx, RowIndex, Cols) ->
       %% This function is *only* called if previous row status wasn't notReady!
       %% Only possible previous state is: not created. We must check for the automatic 
       %% transition from notReady to notInService here!
       %% We have already checked that the transition to notReady (or notInService) is allowed!
       %% NOTE: as long as we are using default values when creating the row, there will always 
       %% be a transition to the notInService state.
    NewRec = insert_col_values(Table, Cols, new_rec(Table, RowIndex)),
    set_row_notReady2(Table, Idx, RowIndex, NewRec).




set_row_notReady2(Table, Idx, RowIndex, Rec) ->
    NewRowStatus = 
	case all_cols_set(Table, [], Rec) of
	    false ->
		?notReady;
	    true ->
		?notInService
	end,
    NewIdx = snmp_index:insert(Idx, RowIndex, {NewRowStatus,Rec}),
    {{noError,0}, NewIdx}.





set_row_notInService(Table, Idx, RowIndex, Cols) ->
       %% Possible previous states are: not created or notReady, notInService or active. 
       %% We have already checked that the transition to notInService is allowed!
       %% The automatic transition from notReady to notInService is handled 
       %% in other functions!
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	undefined ->
	       %% A little trick is used here: we call the function 
	       %% set_row_notReady, 'cause it will handle the transition to
	       %% notInService automatically!
	    set_row_notReady(Table, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {RowStatus,KeyOrRec}}} ->
	    case RowStatus of
		?active ->
		    case get_row(Table, KeyOrRec) of
			{ok, TmpRec} ->
			    NewRec = insert_col_values(Table, Cols, TmpRec),
			    set_row_notInService2(Table, Idx, RowIndex, NewRec);
			_Other ->
			    RowStatusCol = case Table of
					       type_table ->
						   ?TypeStatusCol;
					       meas_table ->
						   ?MeasStatusCol
					   end,
			    {{commitFailed,RowStatusCol}, Idx}
		    end;
		_Other ->
		    NewRec = insert_col_values(Table, Cols, KeyOrRec),
		    set_row_notInService2(Table, Idx, RowIndex, NewRec)
	    end
    end.






set_row_notInService2(type_table, Idx, RowIndex, TypeRec) ->
    #mesh_type{name         = Name,
	       info         = Info,
	       callback_mod = CbMod,
	       admin_state  = AdmState,
	       max_nof_inst = MaxNofInst,
	       meas_args    = LockArgs}  = TypeRec,
    mesh:unregister_type(Name, LockArgs),
    NewIdx = snmp_index:insert(Idx, RowIndex, {?notInService,TypeRec}),
    {{noError,0}, NewIdx};
set_row_notInService2(meas_table, Idx, RowIndex, MeasRec) ->
    #mesh_meas{id          = Id,
	       type        = Type,
	       info        = Info,
	       admin_state = AdmState,
	       res_id      = ResId,
	       init_args   = StopArgs} = MeasRec,
    mesh:delete_measurement(Id, StopArgs),
    NewIdx = snmp_index:insert(Idx, RowIndex, {?notInService,MeasRec}),
    {{noError,0}, NewIdx}.
    



set_row_active(Table, Idx, RowIndex, Cols) ->
       %% Possible previous states are: notInService, notReady, not created or active.
       %% We have already checked that it is OK to set the new state!
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	{ok, {SnmpRowIndex, {?active,KeyOrRec}}} ->
	    update_row(Table, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {RowStatus,KeyOrRec}}} ->
	    NewRec = insert_col_values(Table, Cols, KeyOrRec),
	    set_row_active2(Table, Idx, RowIndex, NewRec);
	undefined ->
	    NewRec = insert_col_values(Table, Cols, new_rec(Table, RowIndex)),
	    set_row_active2(Table, Idx, RowIndex, NewRec)
    end.





set_row_active2(type_table, Idx, RowIndex, TypeRec) ->
    Name = list_to_atom(RowIndex),
    #mesh_type{info         = Info,
	       callback_mod = CbMod,
	       admin_state  = AdmState,
	       max_nof_inst = MaxNofInst,
	       meas_args    = LockArgs}  = TypeRec,
    mesh:unregister_type(Name, LockArgs),
    case mesh:register_type(Name, Info, CbMod, MaxNofInst, AdmState, LockArgs) of
	{error, _Reason} ->
	    {{commitFailed,?TypeStatusCol}, Idx};
	{registered, Name} ->
	    NewIdx = snmp_index:insert(Idx, RowIndex, {?active,Name}),
	    {{noError,0}, NewIdx}
    end;
set_row_active2(meas_table, Idx, RowIndex, MeasRec) ->
    Id = list_to_atom(RowIndex),
    #mesh_meas{type        = Type,
	       info        = Info,
	       admin_state = AdmState,
	       res_id      = ResId,
	       init_args   = Args} = MeasRec,
    mesh:delete_measurement(Id, Args),
    case mesh:create_measurement(Id, Type, Info, ResId, AdmState, Args) of
	{error, {no_such_type, Type}} ->
	    {{commitFailed,?MeasType}, Idx};
	{error, _Reason} ->
	    {{commitFailed,?MeasStatusCol}, Idx};
	{created, Id} ->
	    NewIdx = snmp_index:insert(Idx, RowIndex, {?active,Id}),
	    {{noError,0}, NewIdx}
    end.





delete_row(Table, Idx, RowIndex, Cols) ->
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	undefined ->
	    {{noError, 0}, Idx};
	{ok, {SnmpRowIndex,{?active,RowKey}}} ->
	       %% May still be a race condition here, i.e., object removed from Mnesia...
	    case get_row(Table, RowKey) of
		not_found ->
		    {{noError,0}, Idx};
		{ok, RowRec} ->
		       %% Instead of fuzzing with ordering of columns/updates,
		       %% get all values available from the beginning!
		    UpdatedRowRec = insert_col_values(Table, Cols, RowRec),
		    delete_row2(Table, UpdatedRowRec, Idx)
	    end;
	{ok, {SnmpRowIndex,{RowStatus,RowRec}}} ->
	    UpdatedRowRec = insert_col_values(Table, Cols, RowRec),
	    delete_row2(Table, UpdatedRowRec, Idx)
    end.





delete_row2(type_table, Rec, Idx) ->
    #mesh_type{name      = TypeName,
	       meas_args = StopArgs}  = Rec,
    case mesh:unregister_type(TypeName, StopArgs) of
	{unregistered, TypeName} ->
	    {{noError,0}, snmp_index:delete(Idx, lists:flatten(io_lib:write(TypeName)))};
	{error, {no_such_type,TypeName}} ->
	    {{noError,0}, snmp_index:delete(Idx, lists:flatten(io_lib:write(TypeName)))};
	{error, Reason} ->
	    {{commitFailed,?TypeStatusCol}, Idx}
    end;
delete_row2(meas_table, Rec, Idx) ->
    #mesh_meas{id        = MeasId,
	       init_args = StopArgs}  = Rec,
    case mesh:delete_measurement(MeasId, StopArgs) of
	{deleted, MeasId} ->
	       %% Don't bother about deleting corresponding thresholds here!
	       %% That is taken care of when the mnesia event message is received! 
	       %% Banzai!!!  :-0  :-)
	    {{noError,0}, snmp_index:delete(Idx, lists:flatten(io_lib:write(MeasId)))};
	{error, {no_such_measurement,MeasId}} ->
	    {{noError,0}, snmp_index:delete(Idx, lists:flatten(io_lib:write(MeasId)))};	    
	{error, Reason} ->
	    {{commitFailed,?MeasStatusCol}, Idx}
    end.





is_row_status_col_changed(Table, Cols) ->
    StatusCol = case Table of
		    type_table ->
			?TypeStatusCol;
		    meas_table ->
			?MeasStatusCol;
		    thresh_table ->
			?ThreshStatusCol
		end,
    case lists:keysearch(StatusCol, 1, Cols) of
	{value, {StatusCol,Status}} ->
	    {true, Status};
	_Other ->
	    false
    end.




is_callback_col_changed(Cols) ->   %% Only valid for type_table!
    case lists:keysearch(?TypeCallbackMod, 1, Cols) of
	{value, {StatusCol,Mod}} ->
	    {true, Mod};
	_Other ->
	    false
    end.





check_if_set_ok(RowIndex, Cols, Table, Idx) ->
    SnmpRowIndex = [length(RowIndex) | RowIndex],
    {CurrRowStatus, CurrRec} = 
	case snmp_index:get(Idx, SnmpRowIndex) of
	    undefined ->
		{undefined, undefined};
	    {ok, {SnmpRowIndex, {RowStatus,KeyOrRec}}} ->
		{RowStatus, KeyOrRec}
	end,
    NewRowStatus = 
	case is_row_status_col_changed(Table, Cols) of
	    {true, ?active} ->
		?active;
	    {true, ?notInService} ->
		?notInService;
	    {true, ?createAndGo} ->
		?createAndGo;
	    {true, ?createAndWait} ->
		?createAndWait;
	    {true, ?destroy} ->
		?destroy;
	    false ->
		CurrRowStatus
	end,
    check_if_set_ok2(CurrRowStatus, NewRowStatus, Table, Cols, CurrRec).
    



check_if_thresh_set_ok(RowIndex, Cols, MeasIdx, ThreshIdx) ->
    {MeasId,ThreshId} = rowindex2measthreshid(RowIndex),
       %% Last element in RowIndex is the threshold identifier, i.e., an integer!
    SnmpRowIndex = [(length(RowIndex) - 1) | RowIndex],
    {CurrRowStatus, CurrRec} = 
	case snmp_index:get(ThreshIdx, SnmpRowIndex) of
	    undefined ->
		{undefined, undefined};
	    {ok, {SnmpRowIndex, {ThreshType,RowStatus,MeasId,ThreshId,ThreshRec}}} ->
		{RowStatus, ThreshRec}
	end,
    NewRowStatus = 
	case is_row_status_col_changed(thresh_table, Cols) of
	    {true, ?active} ->
		?active;
	    {true, ?notInService} ->
		?notInService;
	    {true, ?createAndGo} ->
		?createAndGo;
	    {true, ?createAndWait} ->
		?createAndWait;
	    {true, ?destroy} ->
		?destroy;
	    false ->
		CurrRowStatus
	end,

    MeasIndex = lists:sublist(RowIndex, length(RowIndex) - 1),
    MeasSnmpIndex = [length(MeasIndex) | MeasIndex],
    MeasInfo =   % MeasInfo = {MeasExists,MeasStatus}
	case snmp_index:get(MeasIdx, MeasSnmpIndex) of
	    undefined ->
		{false, undefined};
	    {ok, {MeasSnmpIndex, {CurrMeasStatus, KeyOrRec}}} ->
		{true, CurrMeasStatus}
	end,
    check_if_thresh_set_ok2(CurrRowStatus, NewRowStatus, Cols, CurrRec, MeasInfo).





check_if_set_ok2(undefined, ?createAndGo, Table, Cols, _CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, new_rec(Table, "undefined")) of
		false when Table == type_table ->
		    {inconsistentValue, ?TypeStatusCol};
		false when Table == meas_table ->
		    {inconsistentValue, ?MeasStatusCol};
		true ->
		    {noError,0}
	    end
    end;
check_if_set_ok2(undefined, ?createAndWait, Table, Cols, _CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_set_ok2(undefined, ?notInService, Table, Cols, _CurrRec) ->
       %% Appears that this operation shall not be allowed, well, well...
       %% If someone changes his/her mind, the code is below, in the commented section.  :-)
    case Table of
	type_table ->
	    {inconsistentValue, ?TypeStatusCol};
	meas_table ->
	    {inconsistentValue, ?MeasStatusCol}
    end;
%      case col_values_ok(Table, Cols) of
%  	{false, Col} ->
%  	    {inconsistentValue, Col};
%  	true ->
%  	    case all_cols_set(Table, Cols, new_rec(Table, "undefined")) of
%  		false when Table == type_table ->
%  		    {inconsistentValue, ?TypeStatusCol};
%  		false when Table == meas_table ->
%  		    {inconsistentValue, ?MeasStatusCol};
%  		true ->
%  		    {noError, 0}
%  	    end
%      end;
check_if_set_ok2(undefined, ?destroy, Table, Cols, _CurrRec) ->
    {noError, 0};
check_if_set_ok2(?notReady, ?active, Table, Cols, CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, CurrRec) of
		false when Table == type_table ->
		    {inconsistentValue, ?TypeStatusCol};
		false when Table == meas_table ->
		    {inconsistentValue, ?MeasStatusCol};
		true ->
		    {noError, 0}
	    end
    end;
check_if_set_ok2(?notReady, ?notInService, Table, Cols, CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, CurrRec) of
		false when Table == type_table ->
		    {inconsistentValue, ?TypeStatusCol};
		false when Table == meas_table ->
		    {inconsistentValue, ?MeasStatusCol};
		true ->
		    {noError, 0}
	    end
    end;
check_if_set_ok2(?notReady, ?notReady, Table, Cols, _CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_set_ok2(?notReady, ?destroy, _Table, _Cols, _CurrRec) ->
    {noError, 0};
check_if_set_ok2(?notInService, ?active, Table, Cols, CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, CurrRec) of
		false when Table == type_table ->
		    {inconsistentValue, ?TypeStatusCol};
		false when Table == meas_table ->
		    {inconsistentValue, ?MeasStatusCol};
		true ->
		    {noError, 0}
	    end
    end;
check_if_set_ok2(?notInService, ?notInService, Table, Cols, _CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_set_ok2(?notInService, ?destroy, _Table, _Cols, _CurrRec) ->
    {noError, 0};
check_if_set_ok2(?active, ?active, type_table, Cols, NameOrId) ->
    case col_values_ok(type_table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case is_callback_col_changed(Cols) of
		{true, Mod} ->
		    case get_row(type_table, NameOrId) of
			{ok, Rec} ->
			    if 
				Rec#mesh_type.callback_mod == Mod ->
				    {noError, 0};
				true ->
				    {inconsistentValue, ?TypeCallbackMod}
			    end;
			_Other ->
			    {noError, 0}
		    end;
		false ->
		    {noError, 0}
	    end
    end;
check_if_set_ok2(?active, ?active, meas_table, Cols, NameOrId) ->
    case col_values_ok(meas_table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	       %% Only allowed to modify administrative state and arguments in an active 
	       %% (i.e., already created) measurement!!!
	    AllowedList = [?MeasAdminState, ?MeasArgs, ?MeasStatusCol],
	    case [Col || {Col,Val} <- Cols, not(lists:member(Col, AllowedList))] of
		[] ->
		    {noError, 0};
		[ForbiddenCol | T] ->
		    {inconsistentValue, ForbiddenCol}
	    end
    end;
check_if_set_ok2(?active, ?notInService, Table, Cols, _CurrRec) ->
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_set_ok2(?active, ?destroy, _Table, _Cols, _CurrRec) ->
    {noError, 0};
check_if_set_ok2(CurrRowStatus, NewRowStatus, Table, _Cols, _CurrRec) ->
    case Table of
	type_table ->
	    {inconsistentValue, ?TypeStatusCol};
	meas_table ->
	    {inconsistentValue, ?MeasStatusCol}
    end.
    




check_if_thresh_set_ok2(undefined, ?createAndGo, Cols, _CurrRec, MeasInfo) ->
    Table = thresh_table,
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, new_rec(Table, "undefined")) of
		false ->
		    {inconsistentValue, ?ThreshStatusCol};
		true ->
		       %% Has to check that measurement object exists and is active!  :-)
		    case MeasInfo of
			{true, ?active} ->
			    {noError,0};
			_Other ->
			    {inconsistentName, ?ThreshMeasId}
		    end
	    end
    end;
check_if_thresh_set_ok2(undefined, ?createAndWait, Cols, _CurrRec, _MeasInfo) ->
    case col_values_ok(thresh_table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_thresh_set_ok2(undefined, ?notInService, Cols, _CurrRec, _MeasInfo) ->
       %% Appears that this operation shall not be allowed, well, well...
       %% If someone changes his/her mind, the code is similar to the 
       %% commented parts in the check_if_set_ok2 function.  :-)
    {inconsistentValue, ?ThreshStatusCol};
check_if_thresh_set_ok2(undefined, ?destroy, _Cols, _CurrRec, _MeasInfo) ->
    {noError, 0};
check_if_thresh_set_ok2(?notReady, ?active, Cols, CurrRec, MeasInfo) ->
    Table = thresh_table,
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, CurrRec) of
		false ->
		    {inconsistentValue, ?ThreshStatusCol};
		true ->
		       %% Has to check that measurement object exists and is active!  :-)
		    case MeasInfo of
			{true, ?active} ->
			    {noError,0};
			_Other ->
			    {inconsistentValue, ?ThreshStatusCol}
		    end
	    end
    end;
check_if_thresh_set_ok2(?notReady, ?notInService, Cols, CurrRec, _MeasInfo) ->
    Table = thresh_table,
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, CurrRec) of
		false ->
		    {inconsistentValue, ?ThreshStatusCol};
		true ->
		    {noError, 0}
	    end
    end;
check_if_thresh_set_ok2(?notReady, ?notReady, Cols, _CurrRec, _MeasInfo) ->
    case col_values_ok(thresh_table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_thresh_set_ok2(?notReady, ?destroy, _Cols, _CurrRec, _MeasInfo) ->
    {noError, 0};
check_if_thresh_set_ok2(?notInService, ?active, Cols, CurrRec, MeasInfo) ->
    Table = thresh_table,
    case col_values_ok(Table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    case all_cols_set(Table, Cols, CurrRec) of
		false ->
		    {inconsistentValue, ?ThreshStatusCol};
		true ->
		       %% Has to check that measurement object exists and is active!  :-)
		    case MeasInfo of
			{true, ?active} ->
			    {noError,0};
			_Other ->
			    {inconsistentValue, ?ThreshStatusCol}
		    end
	    end
    end;
check_if_thresh_set_ok2(?notInService, ?notInService, Cols, _CurrRec, _MeasInfo) ->
    case col_values_ok(thresh_table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_thresh_set_ok2(?notInService, ?destroy, _Cols, _CurrRec, _MeasInfo) ->
    {noError, 0};
check_if_thresh_set_ok2(?active, ?active, Cols, CurrRec, MeasInfo) ->
    case col_values_ok(thresh_table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	       %% Only allowed to modify administrative state in an active 
	       %% (i.e., already created) threshold!!!
	    AllowedList = [?ThreshAdminState, ?ThreshStatusCol],
	    case [Col || {Col,Val} <- Cols, not(lists:member(Col, AllowedList))] of
		[] ->
		    {noError, 0};
		[ForbiddenCol | T] ->
		    {inconsistentValue, ForbiddenCol}
	    end
    end;
check_if_thresh_set_ok2(?active, ?notInService, Cols, _CurrRec, _MeasInfo) ->
    case col_values_ok(thresh_table, Cols) of
	{false, Col} ->
	    {inconsistentValue, Col};
	true ->
	    {noError, 0}
    end;
check_if_thresh_set_ok2(?active, ?destroy, _Cols, _CurrRec, _MeasInfo) ->
    {noError, 0};
check_if_thresh_set_ok2(CurrRowStatus, NewRowStatus, _Cols, _CurrRec, _MeasInfo) ->
    {inconsistentValue, ?ThreshStatusCol}.





all_cols_set(type_table, Cols, TypeRec) ->
    NewTypeRec = insert_col_values(type_table, Cols, TypeRec),
    #mesh_type{info         = Info,
	       callback_mod = CbMod,
	       admin_state  = AdmState,
	       max_nof_inst = MaxNofInst}  = NewTypeRec,
    not(lists:member(?UNDEFINED, [Info,CbMod,AdmState,MaxNofInst]));
all_cols_set(meas_table, Cols, MeasRec) ->
    NewMeasRec = insert_col_values(meas_table, Cols, MeasRec),
    #mesh_meas{type        = Type,
	       info        = Info,
	       res_id      = ResId,
	       admin_state = AdmState,
	       init_args   = InitArgs} = NewMeasRec,
    not(lists:member(?UNDEFINED, [Type,Info,ResId,AdmState,InitArgs]));
all_cols_set(thresh_table, Cols, ThreshRec) ->
       %% Have to be sure ?ThreshVal1 comes before ?ThreshVal2 in the Cols list!!!
    NewCols = lists:keysort(1, Cols),
    NewThreshRec = insert_col_values(thresh_table, NewCols, ThreshRec),
    #threshold{thresh_type = Type,
	       thresh1     = Val1,
	       thresh2     = Val2,
	       status      = AdmState} = NewThreshRec,
    not(lists:member(?UNDEFINED, [Type,Val1,Val2,AdmState])).
    




insert_col_values(type_table, [{Col,Val} | T], TypeRec) ->
    NewTypeRec =
	case Col of
	    ?TypeInfo ->
		TypeRec#mesh_type{info = convert_type_col_val_to_term(Col, Val)};
	    ?TypeCallbackMod ->
		TypeRec#mesh_type{callback_mod = convert_type_col_val_to_term(Col, Val)};
	    ?TypeAdminState ->
		TypeRec#mesh_type{admin_state = convert_type_col_val_to_term(Col, Val)};
	    ?TypeMeasArgs ->
		TypeRec#mesh_type{meas_args = element(2, convert_type_col_val_to_term(Col,Val))};
	    ?TypeMaxInst ->
		TypeRec#mesh_type{max_nof_inst = convert_type_col_val_to_term(Col, Val)};
	    _Other ->
		TypeRec
	end,
    insert_col_values(type_table, T, NewTypeRec);
insert_col_values(meas_table, [{Col,Val} | T], MeasRec) ->
    NewMeasRec =
	case Col of
	    ?MeasType ->
		MeasRec#mesh_meas{type = convert_meas_col_val_to_term(Col, Val)};
	    ?MeasInfo ->
		MeasRec#mesh_meas{info = convert_meas_col_val_to_term(Col, Val)};
	    ?MeasResId ->
		MeasRec#mesh_meas{res_id = element(2,convert_meas_col_val_to_term(Col, Val))};
	    ?MeasAdminState ->
		MeasRec#mesh_meas{admin_state = convert_meas_col_val_to_term(Col, Val)};
	    ?MeasArgs ->
		MeasRec#mesh_meas{init_args = element(2, convert_meas_col_val_to_term(Col, Val))};
	    _Other ->
		MeasRec
	end,
    insert_col_values(meas_table, T, NewMeasRec);
insert_col_values(thresh_table, [{Col,Val} | T], ThreshRec) ->
    NewThreshRec =
	case Col of
	    ?ThreshType ->
		ThreshRec#threshold{thresh_type = convert_thresh_col_val_to_term(Col, Val)};
	    ?ThreshAdminState ->
		ThreshRec#threshold{status = convert_thresh_col_val_to_term(Col, Val)};
	    ?ThreshVal1 ->
		   %% Default action: thresh2 gets the same value as thresh1.
		   %% Since the Cols list is sorted, the thresh2 value will
		   %% be overwritten, should there be a ?ThreshVal2 value specified!
		Thresh1 = element(2, convert_thresh_col_val_to_term(Col, Val)),
		ThreshRec#threshold{thresh1 = Thresh1,
				    thresh2 = Thresh1};
	    ?ThreshVal2 ->
		Thresh2 = element(2, convert_thresh_col_val_to_term(Col, Val)),
		ThreshRec#threshold{thresh2 = Thresh2};
	    _Other ->
		ThreshRec
	end,
    insert_col_values(thresh_table, T, NewThreshRec);
insert_col_values(_Table, [], Rec) ->
    Rec.


    


new_rec(type_table, RowIndex) ->
    TypeName = list_to_atom(RowIndex),
    #mesh_type{name         = TypeName,
	       info         = "",
	       callback_mod = undefined_module,
	       admin_state  = unlocked,
	       max_nof_inst = 50};
new_rec(meas_table, RowIndex) ->
    MeasId = list_to_atom(RowIndex),
    #mesh_meas{id          = MeasId,
	       type        = undefined_type,
	       info        = "",
	       res_id      = undefined_resource,
	       admin_state = started,
	       init_args   = []};
new_rec(thresh_table, RowIndex) ->
    ThreshId = lists:last(RowIndex),
    #threshold{id          = ThreshId,
	       thresh_type = upper,
	       thresh1     = 0,
	       thresh2     = 0,
	       status      = enabled}.




col_values_ok(Table, [{Col,Value} | T]) ->
    case col_value_ok(Table, Col, Value) of
	true ->
	    col_values_ok(Table, T);
	false ->
	    {false, Col}
    end;
col_values_ok(_Table, []) ->
    true.
		
    
	    

col_value_ok(type_table, Col, Val) ->	
    Res = (catch convert_type_col_val_to_term(Col, Val)),
    case Col of 
	?TypeInfo when list(Res) ->
	    true;
	?TypeCallbackMod when atom(Res) ->
	    case mesh:check_callback_module(Res) of
		ok ->
		    true;
		_Other ->
		    false
	    end;
	?TypeAdminState when atom(Res) ->
	    case Res of
		locked ->
		    true;
		unlocked ->
		    true;
		shutting_down ->
		    true;
		_Other ->
		    false
	    end;
	?TypeMeasArgs ->
	    case Res of 
		{ok, _Term} ->
		    true;
		{error, _Reason} ->
		    false
	    end;
	?TypeMaxInst when integer(Res), Res >= 0 ->
	    true;
	?TypeStatusCol ->
	    true;  %% Wouldn't be here else.
	_Other ->
	    false
    end;
col_value_ok(meas_table, Col, Val) ->
    Res = (catch convert_meas_col_val_to_term(Col, Val)),
    case Col of
	?MeasType when atom(Res) ->
	    true;
	?MeasInfo when list(Res) ->
	    true;
	?MeasAdminState when atom(Res) ->
	    case Res of
		started ->
		    true;
		stopped ->
		    true;
		_Other ->
		    false
	    end;
	?MeasResId ->
	    case Res of
		{ok, _Term} ->
		    true;
		{error, _Reason} ->
		    false
	    end;
	?MeasStatusCol ->
	    true;   %% Wouldn't be here else.
	?MeasArgs ->
	    case Res of
		{ok, _Term} ->
		    true;
		{error, _Reason} ->
		    false
	    end;
	_Other ->
	    false
    end;
col_value_ok(thresh_table, Col, Val) ->
    Res = (catch convert_thresh_col_val_to_term(Col, Val)),
    case Col of
	?ThreshType when atom(Res) ->
	    case Res of
		upper ->
		    true;
		lower ->
		    true;
		_Other ->
		    false
	    end;
	?ThreshVal1 ->
	    true;        %% Exit signal else.
	?ThreshVal2 ->
	    true;        %% Exit signal else.
	?ThreshAdminState when atom(Res) ->
	    case Res of
		enabled ->
		    true;
		disabled ->
		    true;
		_Other ->
		    false
	    end;
	?ThreshStatusCol ->
	    true;   %% Wouldn't be here else.
	_Other ->
	    false
    end.




convert_type_col_val_to_term(?TypeInfo, Val) ->
    Val;
convert_type_col_val_to_term(?TypeCallbackMod, Val) ->
    catch list_to_atom(Val);
convert_type_col_val_to_term(?TypeAdminState, Val) ->
    case Val of
	?unlocked ->
	    unlocked;
	?shutting_down ->
	    shutting_down;
	?locked ->
	    locked
    end;
convert_type_col_val_to_term(?TypeMeasArgs, Val) ->
    case catch erl_scan:string(Val ++ ". ") of
	{ok, ScannedStr, _No} ->
	    case erl_parse:parse_term(ScannedStr) of
		{ok, Term} ->
		    {ok, Term};
		_Other ->
		    {error, not_a_term}
	    end;
	_Other ->
	    {error, not_a_term}
    end;
convert_type_col_val_to_term(?TypeMaxInst, Val) ->
    Val;
convert_type_col_val_to_term(?TypeStatusCol, Val) ->
    Val.
    


convert_meas_col_val_to_term(?MeasType, Val) ->
    catch list_to_atom(Val);
convert_meas_col_val_to_term(?MeasInfo, Val) ->
    Val;
convert_meas_col_val_to_term(?MeasResId, Val) ->
    case catch erl_scan:string(Val ++ ". ") of
	{ok, ScannedStr, _No} ->
	    case erl_parse:parse_term(ScannedStr) of
		{ok, Term} ->
		    {ok, Term};
		_Other ->
		    {error, not_a_term}
	    end;
	_Other ->
	    {error, not_a_term}
    end;
convert_meas_col_val_to_term(?MeasAdminState, Val) ->
    case Val of
	?started ->
	    started;
	?stopped ->
	    stopped
    end;
convert_meas_col_val_to_term(?MeasArgs, Val) ->
    case catch erl_scan:string(Val ++ ". ") of
	{ok, ScannedStr, _No} ->
	    case erl_parse:parse_term(ScannedStr) of
		{ok, Term} ->
		    {ok, Term};
		_Other ->
		    {error, not_a_term}
	    end;
	_Other ->
	    {error, not_a_term}
    end;
convert_meas_col_val_to_term(?MeasStatusCol, Val) ->
    Val.




convert_thresh_col_val_to_term(?ThreshType, Val) ->
    case Val of
	?upper ->
	    upper;
	?lower ->
	    lower
    end;
convert_thresh_col_val_to_term(?ThreshVal1, Val) ->
    case catch erl_scan:string(Val ++ ". ") of
	{ok, ScannedStr, _No} ->
	    case erl_parse:parse_term(ScannedStr) of
		{ok, Term} ->
		    {ok, Term};
		_Other ->
		    {error, not_a_term}
	    end;
	_Other ->
	    {error, not_a_term}
    end;
convert_thresh_col_val_to_term(?ThreshVal2, Val) ->
    case catch erl_scan:string(Val ++ ". ") of
	{ok, ScannedStr, _No} ->
	    case erl_parse:parse_term(ScannedStr) of
		{ok, Term} ->
		    {ok, Term};
		_Other ->
		    {error, not_a_term}
	    end;
	_Other ->
	    {error, not_a_term}
    end;
convert_thresh_col_val_to_term(?ThreshAdminState, Val) ->
    case Val of
	?enabled ->
	    enabled;
	?disabled ->
	    disabled
    end;
convert_thresh_col_val_to_term(?ThreshStatusCol, Val) ->
    Val.





get_row(type_table, Key) ->
    mnesia_read_row({mesh_type, Key});
get_row(type_info_table, Key) ->
    mnesia_read_row({mesh_type, Key});
get_row(meas_table, Key) ->
    mnesia_read_row({mesh_meas,Key});
get_row(meas_info_table, Key) ->
    mnesia_read_row({mesh_meas,Key}).
    




mnesia_read_row(Oid) ->
    case mnesia:transaction(fun() -> mnesia:read(Oid) end) of
	{atomic, [Rec]} ->  % Table type is 'set', not 'bag'!!!
	    {ok, Rec};
	_Other ->
	    not_found
    end.




get_cols(Table, RowIndex, {?active,RowKey}, Cols) ->
    case get_row(Table, RowKey) of
	not_found ->
	    {noValue, noSuchObject};
	{ok, RowRec} ->
	    get_cols2(Table, Cols, RowIndex, RowRec, ?active)
    end;
get_cols(Table, RowIndex, {RowStatus,RowRec}, Cols) ->
    get_cols2(Table, Cols, RowIndex, RowRec, RowStatus).
    



get_thresh_cols(ThreshType, [Col | T], SnmpRowIndex, Rec, RowStatus) when Col < ?FirstThreshCol ->
    [{noValue, noSuchInstance} | get_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_thresh_cols(ThreshType, [Col | T], SnmpRowIndex, Rec, RowStatus) when Col > ?LastThreshCol ->
    [{noValue, noSuchInstance} | get_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_thresh_cols(ThreshType, [?ThreshType | T], SnmpRowIndex, Rec, RowStatus) ->
    [{value, ThreshType} | get_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_thresh_cols(ThreshType, [?ThreshStatusCol | T], SnmpRowIndex, Rec, RowStatus) ->
    [{value, RowStatus} | get_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_thresh_cols(ThreshType, [Col | T], SnmpRowIndex, Rec, RowStatus) ->
    Value = get_thresh_rec_value(Col, Rec),
    [{value, Value} | get_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_thresh_cols(ThreshType, [], _SnmpRowIndex, _Rec, _RowStatus) ->
    [].




get_next_cols(Table, NextSnmpRowIndex, {?active,NextRowKey}, Cols, Idx, StopRowIndex) ->
    case get_row(Table, NextRowKey) of
	   %% There is a pathological case here:
	   %% the object we're looking for may have been removed from 
	   %% the Mnesia table, but not yet from the index structure. 
	   %% In this case we have to continue the search for the next 
	   %% row. But what if all objects in the table have been removed
	   %% (or replaced with new ones)? In this case we will never stop 
	   %% the search!  :-(
	   %% The solution: we compare the next row oid with the one we started
	   %% with. If they are equal, we have traversed the whole structure, and
	   %% may report that the table currently is empty (i.e., as far as we right
	   %% now knows!).
	not_found when tl(NextSnmpRowIndex) == StopRowIndex ->
	    end_of_table(Cols);
	not_found ->
	    table_func(get_next, tl(NextSnmpRowIndex), Cols, Table, Idx, StopRowIndex);
	{ok, RowRec} ->
	    get_next_cols2(Table, Cols, NextSnmpRowIndex, RowRec, ?active)
    end;
get_next_cols(Table, NextSnmpRowIndex, {NextRowStatus,NextRowRec}, Cols, Idx, StopRowIndex) ->
    get_next_cols2(Table, Cols, NextSnmpRowIndex, NextRowRec, NextRowStatus).
    

    

get_next_thresh_cols(ThreshType,[Col|T],SnmpRowIndex,Rec,RowStatus) when Col < ?FirstThreshCol ->
    get_next_thresh_cols(ThreshType, [?FirstThreshCol|T], SnmpRowIndex, Rec, RowStatus);
get_next_thresh_cols(ThreshType,[Col|T],SnmpRowIndex,Rec,RowStatus) when Col > ?LastThreshCol ->
    [endOfTable | get_next_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_next_thresh_cols(ThreshType, [?ThreshType | T], SnmpRowIndex, Rec, RowStatus) ->
    [{[?ThreshType | tl(SnmpRowIndex)], ThreshType} |
     get_next_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_next_thresh_cols(ThreshType, [?ThreshStatusCol | T], SnmpRowIndex, Rec, RowStatus) ->
    [{[?ThreshStatusCol | tl(SnmpRowIndex)], RowStatus} |
     get_next_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_next_thresh_cols(ThreshType, [Col | T], SnmpRowIndex, Rec, RowStatus) ->
    Value = get_thresh_rec_value(Col, Rec),
    [{[Col | tl(SnmpRowIndex)], Value} | 
     get_next_thresh_cols(ThreshType, T, SnmpRowIndex, Rec, RowStatus)];
get_next_thresh_cols(ThreshType, [], _SnmpRowIndex, _Rec, _RowStatus) ->
    [].

    


get_cols2(type_table, [Col | T], RowIndex, Rec, RowStatus) when Col < ?FirstTypeCol ->
    [{noValue, noSuchInstance} | get_cols2(type_table, T, RowIndex, Rec, RowStatus)];
get_cols2(type_table, [Col | T], RowIndex, Rec, RowStatus) when Col > ?LastTypeCol ->
    [{noValue, noSuchInstance} | get_cols2(type_table, T, RowIndex, Rec, RowStatus)];
get_cols2(type_table, [?TypeStatusCol | T], RowIndex, Rec, RowStatus) ->
    [{value, RowStatus} | get_cols2(type_table, T, RowIndex, Rec, RowStatus)];
get_cols2(type_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_type_rec_value(Col, Rec),
    [{value, Value} | get_cols2(type_table, T, RowIndex, Rec, RowStatus)];
get_cols2(type_table, [], _RowIndex, _Rec, _RowStatus) ->
    [];

get_cols2(type_info_table, [Col | T], RowIndex, Rec, RowStatus) when Col < ?FirstTypeCol ->
    [{noValue, noSuchInstance} | get_cols2(type_info_table, T, RowIndex, Rec, RowStatus)];
get_cols2(type_info_table, [Col | T], RowIndex, Rec, RowStatus) when Col > ?LastTypeCol ->
    [{noValue, noSuchInstance} | get_cols2(type_info_table, T, RowIndex, Rec, RowStatus)];
get_cols2(type_info_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_type_info_rec_value(Col, Rec),
    [{value, Value} | get_cols2(type_info_table, T, RowIndex, Rec, RowStatus)];
get_cols2(type_info_table, [], _RowIndex, _Rec, _RowStatus) ->
    [];

get_cols2(meas_table, [Col | T], RowIndex, Rec, RowStatus) when Col < ?FirstMeasCol ->
    [{noValue, noSuchInstance} | get_cols2(meas_table, T, RowIndex, Rec, RowStatus)];
get_cols2(meas_table, [Col | T], RowIndex, Rec, RowStatus) when Col > ?LastMeasCol ->
    [{noValue, noSuchInstance} | get_cols2(meas_table, T, RowIndex, Rec, RowStatus)];
get_cols2(meas_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_meas_info_rec_value(Col, Rec),
    [{value, Value} | get_cols2(meas_table, T, RowIndex, Rec, RowStatus)];    
get_cols2(meas_table, [], _RowIndex, _Rec, _RowStatus) ->
    [];

get_cols2(meas_info_table, [Col | T], RowIndex, Rec, RowStatus) when Col < ?FirstMeasCol ->
    [{noValue, noSuchInstance} | get_cols2(meas_info_table, T, RowIndex, Rec, RowStatus)];
get_cols2(meas_info_table, [Col | T], RowIndex, Rec, RowStatus) when Col > ?LastMeasCol ->
    [{noValue, noSuchInstance} | get_cols2(meas_info_table, T, RowIndex, Rec, RowStatus)];
get_cols2(meas_info_table, [?MeasStatusCol | T], RowIndex, Rec, RowStatus) ->
    [{value, RowStatus} | get_cols2(meas_info_table, T, RowIndex, Rec, RowStatus)];
get_cols2(meas_info_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_meas_rec_value(Col, Rec),
    [{value, Value} | get_cols2(meas_info_table, T, RowIndex, Rec, RowStatus)];    
get_cols2(meas_info_table, [], _RowIndex, _Rec, _RowStatus) ->
    [].





get_next_cols2(type_table, [Col | T], RowIndex, Rec, RowStatus) when Col < ?FirstTypeCol ->
    get_next_cols2(type_table, [?FirstTypeCol | T], RowIndex, Rec, RowStatus);
get_next_cols2(type_table, [Col | T], RowIndex, Rec, RowStatus) when Col > ?LastTypeCol ->
    [endOfTable | get_next_cols2(type_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(type_table, [?TypeStatusCol | T], RowIndex, Rec, RowStatus) ->
    [{[?TypeStatusCol | tl(RowIndex)], RowStatus} |
     get_next_cols2(type_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(type_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_type_rec_value(Col, Rec),
    [{[Col | tl(RowIndex)], Value} | 
     get_next_cols2(type_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(type_table, [], _RowIndex, _Rec, _RowStatus) ->
    [];

get_next_cols2(type_info_table, [Col|T], RowIndex, Rec, RowStatus) when Col < ?FirstTypeInfoCol ->
    get_next_cols2(type_info_table, [?FirstTypeCol | T], RowIndex, Rec, RowStatus);
get_next_cols2(type_info_table, [Col|T], RowIndex, Rec, RowStatus) when Col > ?LastTypeInfoCol ->
    [endOfTable | get_next_cols2(type_info_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(type_info_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_type_info_rec_value(Col, Rec),
    [{[Col | tl(RowIndex)], Value} | 
     get_next_cols2(type_info_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(type_info_table, [], _RowIndex, _Rec, _RowStatus) ->
    [];

get_next_cols2(meas_table, [Col | T], RowIndex, Rec, RowStatus) when Col < ?FirstMeasCol ->
    get_next_cols2(meas_table, [?FirstTypeCol | T], RowIndex, Rec, RowStatus);
get_next_cols2(meas_table, [Col | T], RowIndex, Rec, RowStatus) when Col > ?LastMeasCol ->
    [endOfTable | get_next_cols2(meas_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(meas_table, [?MeasStatusCol | T], RowIndex, Rec, RowStatus) ->
    [{[?MeasStatusCol | tl(RowIndex)], RowStatus} | 
     get_next_cols2(meas_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(meas_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_meas_rec_value(Col, Rec),
    [{[Col | tl(RowIndex)], Value} | 
     get_next_cols2(meas_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(meas_table, [], _RowIndex, _Rec, _RowStatus) ->
    [];

get_next_cols2(meas_info_table, [Col|T], RowIndex, Rec, RowStatus) when Col < ?FirstMeasInfoCol ->
    get_next_cols2(meas_info_table, [?FirstTypeCol | T], RowIndex, Rec, RowStatus);
get_next_cols2(meas_info_table, [Col|T], RowIndex, Rec, RowStatus) when Col > ?LastMeasInfoCol ->
    [endOfTable | get_next_cols2(meas_info_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(meas_info_table, [Col | T], RowIndex, Rec, RowStatus) ->
    Value = get_meas_info_rec_value(Col, Rec),
    [{[Col | tl(RowIndex)], Value} | 
     get_next_cols2(meas_info_table, T, RowIndex, Rec, RowStatus)];
get_next_cols2(meas_info_table, [], _RowIndex, _Rec, _RowStatus) ->
    [].




get_type_rec_value(?TypeInfo, Rec) ->
    term2dispstr(Rec#mesh_type.info);
get_type_rec_value(?TypeCallbackMod, Rec) ->
    term2dispstr(Rec#mesh_type.callback_mod);
get_type_rec_value(?TypeAdminState, Rec) ->
    Rec#mesh_type.admin_state;
get_type_rec_value(?TypeMeasArgs, Rec) ->
    term2dispstr(Rec#mesh_type.meas_args);
get_type_rec_value(?TypeMaxInst, Rec) ->
    Rec#mesh_type.max_nof_inst.



get_meas_rec_value(?MeasType, Rec) ->
    term2dispstr(Rec#mesh_meas.type);
get_meas_rec_value(?MeasInfo, Rec) ->
    term2dispstr(Rec#mesh_meas.info);
get_meas_rec_value(?MeasResId, Rec) ->
    term2dispstr(Rec#mesh_meas.res_id);
get_meas_rec_value(?MeasAdminState, Rec) ->
    Rec#mesh_meas.admin_state;
get_meas_rec_value(?MeasArgs, Rec) ->
    term2dispstr(Rec#mesh_meas.init_args).



get_type_info_rec_value(?TypeInfoCurrInst, Rec) ->
    Rec#mesh_type.curr_nof_inst.



%  get_meas_info_rec_value(?MeasInfoOperState, Rec) ->
%      term2dispstr(Rec#mesh_meas.oper_state);
get_meas_info_rec_value(?MeasInfoLastVal, Rec) ->
    term2dispstr(Rec#mesh_meas.last_meas_value);
get_meas_info_rec_value(?MeasInfoLastValTime, Rec) ->
    term2dispstr(Rec#mesh_meas.time_stamp);
get_meas_info_rec_value(?MeasInfoLastValInfo, Rec) ->
    term2dispstr(Rec#mesh_meas.meas_info);
get_meas_info_rec_value(?MeasInfoMaxTideCurr, Rec) ->
    TideM = Rec#mesh_meas.max_tidemark,
    term2dispstr(TideM#tidemark.current);
get_meas_info_rec_value(?MeasInfoMaxTidePrev, Rec) ->
    TideM = Rec#mesh_meas.max_tidemark,
    term2dispstr(TideM#tidemark.previous);
get_meas_info_rec_value(?MeasInfoMinTideCurr, Rec) ->
    TideM = Rec#mesh_meas.min_tidemark,
    term2dispstr(TideM#tidemark.current);    
get_meas_info_rec_value(?MeasInfoMinTidePrev, Rec) ->
    TideM = Rec#mesh_meas.min_tidemark,
    term2dispstr(TideM#tidemark.previous);
get_meas_info_rec_value(?MeasInfoLastReset, Rec) ->
    TideM = Rec#mesh_meas.min_tidemark,
    univ_time2date_and_time(TideM#tidemark.reset_time).




get_thresh_rec_value(?ThreshVal1, Rec) ->
    term2dispstr(Rec#threshold.thresh1);
get_thresh_rec_value(?ThreshVal2, Rec) ->
    term2dispstr(Rec#threshold.thresh2);
get_thresh_rec_value(?ThreshAdminState, Rec) ->
    Rec#threshold.status.




term2dispstr(Term) when list(Term) ->
    Term;
term2dispstr(Term) ->
    lists:flatten(io_lib:write(Term)).



univ_time2date_and_time(undefined) ->
       %% {noValue,unSpecified};
       %% [99, 99, 9, 9, 9, 9, 9, 9];
    [0, 0, 0, 0, 0, 0, 0, 0];    
univ_time2date_and_time({{Year,Month,Day}, {Hour,Minute,Second}}) ->
       %% The reset time is already in universal time!
    lists:sublist(snmp:local_time_to_date_and_time({{Year,Month,Day}, {Hour,Minute,Second}}), 
		  8);
univ_time2date_and_time(Other) ->
    [0, 0, 0, 0, 0, 0, 0, 0].


add_one_to_cols([Col | T]) ->
    [Col + 1 | add_one_to_cols(T)];
add_one_to_cols([]) ->
    [].




end_of_table([Col | T]) ->
    [endOfTable | end_of_table(T)];
end_of_table([]) ->
    [].





update_thresh_row(MeasId, ThreshId, Idx, RowIndex, Cols) ->
       %% If this function is ever reached, we can safely assume that the
       %% row already exists. Otherwise the other case-clauses in the set-part
       %% of the table_func function should have catched it, or the is_set_ok
       %% part wouldn't allow this function to be reached, alternatively.
       %% So: the possible previous states are notReady, notInService or active.
    SnmpRowIndex = [(length(RowIndex)-1) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	{ok, {SnmpRowIndex, {ThreshType,?notInService,MeasId,ThreshId,ThreshRec}}} ->
	    set_thresh_row_notInService(MeasId, ThreshId, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {ThreshType,?notReady,MeasId,ThreshId,ThreshRec}}} ->
	    set_thresh_row_notReady(MeasId, ThreshId, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {ThreshType,?active,MeasId,ThreshId,ThreshRec}}} ->
	    update_active_thresh_row(MeasId, ThreshId, ThreshRec, Idx, Cols)
    end.



update_active_thresh_row(MeasId, ThreshId, Rec, Idx, Cols) ->
       %% Instead of fuzzing with ordering of columns/updates,
       %% get all values available from the beginning!
    UpdatedRec = insert_col_values(thresh_table, Cols, Rec),
    update_active_thresh_row2(MeasId, ThreshId, UpdatedRec, Cols),
    ThreshType = UpdatedRec#threshold.thresh_type,
    MeasIdStr  = lists:flatten(io_lib:write(MeasId)),
    NewIdx     = snmp_index:insert(Idx, {MeasIdStr,ThreshId}, 
				   {ThreshType,?active,MeasId,ThreshId,UpdatedRec}),
    {{noError,0}, NewIdx}.



update_active_thresh_row2(MeasId, ThreshId, Rec, [{?ThreshAdminState,_Val} | T]) ->
    case Rec#threshold.status of
	enabled ->
	    mesh:enable_threshold(MeasId, ThreshId);
	disabled ->
	    mesh:disable_threshold(MeasId, ThreshId)
    end,
    update_active_thresh_row2(MeasId, ThreshId, Rec, T);
update_active_thresh_row2(MeasId, ThreshId, Rec, [{_Col,_Val} | T]) ->
    update_active_thresh_row2(MeasId, ThreshId, Rec, T);
update_active_thresh_row2(_MeasId, _ThreshId, _Rec, []) ->
    done.


    


set_thresh_row_notReady(MeasId, ThreshId, Idx, RowIndex, Cols) ->
    NewRec = insert_col_values(thresh_table, Cols, new_rec(thresh_table, RowIndex)),
    ThreshType = NewRec#threshold.thresh_type,
    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
    NewRowStatus = 
	case all_cols_set(thresh_table, [], NewRec) of
	    false ->
		?notReady;
	    true ->
		?notInService
	end,
    NewIdx = snmp_index:insert(Idx, {MeasIdStr,ThreshId}, 
			       {ThreshType,NewRowStatus,MeasId,ThreshId,NewRec}),
    {{noError,0}, NewIdx}.





set_thresh_row_notInService(MeasId, ThreshId, Idx, RowIndex, Cols) ->
       %% Possible previous states are: not created or notReady, notInService or active. 
       %% We have already checked that the transition to notInService is allowed!
       %% The automatic transition from notReady to notInService is handled 
       %% in other functions!
    SnmpRowIndex = [(length(RowIndex)-1) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	undefined ->
	       %% A little trick is used here: we call the function 
	       %% set_row_notReady, 'cause it will handle the transition to
	       %% notInService automatically!
	    set_thresh_row_notReady(MeasId, ThreshId, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {ThreshType,RowStatus,MeasId,ThreshId,ThreshRec}}} ->
	    NewRec = insert_col_values(thresh_table, Cols, ThreshRec),
	    set_thresh_row_notInService2(MeasId, ThreshId, Idx, ThreshRec)
    end.






set_thresh_row_notInService2(MeasId, ThreshId, Idx, Rec) ->
    ThreshType = Rec#threshold.thresh_type,
    mesh:remove_threshold(MeasId, ThreshId),
    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
    NewIdx = snmp_index:insert(Idx, {MeasIdStr,ThreshId}, 
			       {ThreshType,?notInService,MeasId,ThreshId,Rec}),
    {{noError,0}, NewIdx}.
    



set_thresh_row_active(MeasId, ThreshId, Idx, RowIndex, Cols) ->
       %% Possible previous states are: notInService, notReady, not created or active.
       %% We have already checked that it is OK to set the new state!
    SnmpRowIndex = [(length(RowIndex)-1) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	{ok, {SnmpRowIndex, {ThreshType,?active,MeasId,ThreshId,ThreshRec}}} ->
	    update_thresh_row(MeasId, ThreshId, Idx, RowIndex, Cols);
	{ok, {SnmpRowIndex, {ThreshType,RowStatus,MeasId,ThreshId,ThreshRec}}} ->
	    NewRec = insert_col_values(thresh_table, Cols, ThreshRec),
	    set_thresh_row_active2(MeasId, ThreshId, Idx, RowIndex, NewRec);
	undefined ->
	    NewRec = insert_col_values(thresh_table, Cols, new_rec(thresh_table, RowIndex)),
	    set_thresh_row_active2(MeasId, ThreshId, Idx, RowIndex, NewRec)
    end.





set_thresh_row_active2(MeasId, ThreshId, Idx, RowIndex, Rec) ->
    #threshold{thresh_type = ThreshType,
	       thresh1     = Val1,
	       thresh2     = Val2,
	       status      = AdmState} = Rec,
    mesh:remove_threshold(MeasId, ThreshId),
    Result =
	case ThreshType of
	    upper ->
		mesh:set_upper_threshold(MeasId, ThreshId, {Val1,Val2}, AdmState);
	    lower ->
		mesh:set_lower_threshold(MeasId, ThreshId, {Val1,Val2}, AdmState)
	end,
    case Result of
	{error, _Reason} ->
	    {{commitFailed,?ThreshStatusCol}, Idx};
	{threshold_set, _Id} ->
	    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
	    NewIdx = snmp_index:insert(Idx, {MeasIdStr,ThreshId}, 
				       {ThreshType,?active,MeasId,ThreshId,Rec}),
	    {{noError,0}, NewIdx}
    end.





delete_thresh_row(MeasId, ThreshId, Idx, RowIndex, Cols) ->
    SnmpRowIndex = [(length(RowIndex)-1) | RowIndex],
    case snmp_index:get(Idx, SnmpRowIndex) of
	undefined ->
	    {{noError, 0}, Idx};
	{ok, {SnmpRowIndex, {ThreshType,RowStatus,MeasId,ThreshId,ThreshRec}}} ->
	    case mesh:remove_threshold(MeasId, ThreshId) of
		{threshold_removed, _Id} ->
		    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
		    {{noError,0}, snmp_index:delete(Idx, {MeasIdStr,ThreshId})};
		{error, {no_such_threshold, {MeasId,ThreshId}}} ->
		    MeasIdStr = lists:flatten(io_lib:write(MeasId)),
		    {{noError,0}, snmp_index:delete(Idx, {MeasIdStr,ThreshId})};
		{error, Reason} ->
		    {{commitFailed,?ThreshStatusCol}, Idx}
	    end
    end.





rowindex2measthreshid(RowIndex) ->
    {list_to_atom(lists:sublist(RowIndex, length(RowIndex)-1)),
     lists:last(RowIndex)}.


