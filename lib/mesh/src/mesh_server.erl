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

-module(mesh_server).


-behaviour(gen_server).

-include("mesh.hrl").


-export([start_link/0,
	 create_tables/1
	]).


-export([executer/4
	]).


   %% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2
	]).





-record(state, {max_nof_types     = infinity,
		max_nof_meas      = infinity,
		type_alarm_active = false,
		meas_alarm_active = false
	       }).




%%% Todo: need to improve mnesia accesses; right now pretty many of them
%%% are awfully inefficient.


%%%*********************************************************************
%%%  EXPORTED FUNCTIONS
%%%*********************************************************************


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------


start_link() ->
    gen_server:start_link(mesh_server, [], []).




create_tables(Nodes) ->
    case create_type_table(Nodes) of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    create_meas_table(Nodes)
    end.




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
       %% Two assumptions: 
       %%    1. a node going down will be a rare occurrence.
       %%    2. the type and meas mnesia tables will be relatively small.
       %% Together this means that we may afford to scan the tables for
       %% types and measurements residing on a specific node when it has
       %% gone down, instead of keeping a (hard to maintain) list of used 
       %% nodes.
    net_kernel:monitor_nodes(true),
    case mnesia:force_load_table(mesh_type) of
	{error, Reason} ->
	    io:format("Mnesia tables not available!~n"),
	    {stop, {mnesia_tables_not_available, Reason}};
	yes ->
	    case mnesia:force_load_table(mesh_meas) of
		{error, Reason} ->
		    io:format("Mnesia tables not available!~n"),
		    {stop, {mnesia_tables_not_available, Reason}};
		yes ->
		       %% We ought to check for takeover here; we can't just call
		       %% re_register, because we might have crashed and restarted 
		       %% during takeover to other node.
		    global:re_register_name(mesh_server, self()),
		    NewState  = init_watchdog(),
		    %% io:format("MESH server started, pid ~p!~n", [self()]),
		    register_alarms(),
		    register_events(),
		    mesh_log:open_log(alarm),
		    mesh_log:open_log(event),
		    mesh_log:open_log(measurement),
		    {ok, NewState}
	    end
    end.






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
	
	{register_type,TypeId,Extra,InterfaceMod,NofInst,AdminState} ->
	    Rec = #mesh_type{name         = TypeId, 
			     info         = Extra, 
			     callback_mod = InterfaceMod, 
			     max_nof_inst = NofInst,
			     admin_state  = AdminState},
	    {Reply, NewState} = register_type(Rec, undefined, State),
	    {reply, Reply, NewState};

	   %% If SNMP! If and *only* if!!!
	{register_type,TypeId,Extra,InterfaceMod,NofInst,AdminState,LockArgs} ->
	    Rec = #mesh_type{name         = TypeId, 
			     info         = Extra, 
			     callback_mod = InterfaceMod, 
			     max_nof_inst = NofInst,
			     admin_state  = AdminState},
	    {Reply, NewState} = register_type(Rec, {ok,LockArgs}, State),
	    {reply, Reply, NewState};

	{unregister_type,TypeName,Args} ->
	    {Reply, NewState} = unregister_type(TypeName, Args, State),
	    {reply, Reply, NewState};

	list_types ->
	    Reply = list_types(),
	    {reply, Reply, State};

	{lock_type,TypeName,Args} ->
	    {Reply, NewState} = lock_type(TypeName, Args, State),
	    {reply, Reply, NewState};

	{unlock_type,TypeName} ->
	    Reply = unlock_type(TypeName),
	    {reply, Reply, State};

	{shut_down_type,TypeName} ->
	    Reply = shut_down_type(TypeName),
	    {reply, Reply, State};

	{create_measurement,MeasId,TypeId,Extra,ResId,AdmState,InitArgs} ->
	    Rec = #mesh_meas{id          = MeasId,
			     type        = TypeId,
			     info        = Extra,
			     res_id      = ResId,
			     admin_state = AdmState,
			     init_args   = InitArgs},
	    {Reply, NewState} = create_measurement(Rec, State),
	    {reply, Reply, NewState};
	
	{delete_measurement,MeasName,Args} ->
	    {Reply, NewState} = delete_measurement(MeasName, Args, State),
	    {reply, Reply, NewState};

	{list_measurements,TypeName} ->
	    Reply = list_measurements(TypeName),
	    {reply, Reply, State};

	{revive_measurement,MeasName} ->
	    Reply = revive_measurement(MeasName),
	    {reply, Reply, State};

	{start_measurement,MeasName,Args} ->
	    Reply = start_measurement(MeasName, Args),
	    {reply, Reply, State};

	{stop_measurement,MeasName} ->
	    Reply = stop_measurement(MeasName),
	    {reply, Reply, State};

	{reset_measurement,MeasName,Args} ->
	    Reply = reset_measurement(MeasName, Args),
	    {reply, Reply, State};

	{get_measurement_report,MeasName} ->
	    Reply = get_measurement_report(MeasName),
	    {reply, Reply, State};

	{set_threshold,ThreshType,MeasId,ThreshId,Value,Status} ->
	    Reply = set_threshold(ThreshType,MeasId,ThreshId,Value,Status),
	    {reply, Reply, State};

	{remove_threshold,MeasName,ThreshName} ->
	    Reply = remove_threshold(MeasName, ThreshName),
	    {reply, Reply, State};

	{remove_thresholds,MeasName} ->
	    Reply = remove_thresholds(MeasName),
	    {reply, Reply, State};

	{list_thresholds,MeasName} ->
	    Reply = list_thresholds(MeasName),
	    {reply, Reply, State};

	{threshold_status,Meas,Thresh,Status} ->
	    Reply = set_threshold_status(Meas, Thresh, Status),
	    {reply, Reply, State};

	{report_tidemarks,MeasName} ->
	    Reply = report_tidemarks(MeasName),
	    {reply, Reply, State};

	{reset_tidemarks,MeasName} ->
	    Reply = reset_tidemarks(MeasName),
	    {reply, Reply, State};

	{watchdog_setup,NofTypes,NofMeas} ->
	    {Reply, NewState} = watchdog_setup(NofTypes, NofMeas, State),
	    {reply, Reply, NewState};

	{check_callback_module, Mod} ->
	    Reply = check_module(Mod),
	    {reply, Reply, State};
	
	{set_lock_args, Type, Args} ->
	    Reply = set_lock_args(Type, Args),
	    {reply, Reply, State};

	get_state ->
	    {reply, State, State};

	alive ->
	    {reply, true, State};

	Other ->
	    {reply, {error, {bad_format,Other}, State}}

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

	{measurement_report,MeasName,Value,Time,Info} ->
	    measurement_report(MeasName, Value, Time, Info),
	    {noreply, State};

	{measurement_terminated,MeasName,Reason} ->
	    measurement_terminated(MeasName, Reason),
	    {noreply, State};

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
	{'EXIT', MRP, Reason} when pid(MRP) ->
	    mrp_terminated(MRP, Reason),
	    {noreply, State};
	{nodedown, Node} ->
	    node_down(Node),
	    {noreply, State};
	{nodeup, Node} ->
	    node_up(Node),
	    {noreply, State};
	_Other ->
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
    mesh_log:close_log(alarm),
    mesh_log:close_log(event),
    mesh_log:close_log(measurement),
    net_kernel:monitor_nodes(false),
    ok.





%%%*********************************************************************
%%%  INTERNAL FUNCTIONS
%%%*********************************************************************



init_watchdog() ->
    {atomic,Wd} =  
	mnesia:transaction(
	  fun() ->
		  case mnesia:read({mesh_type, mesh_watchdog}) of
		      [] ->
			  NewWd = ?NEW_WATCHDOG_REC,
			  mnesia:write(NewWd),
			  NewWd;
		      [OldWd] ->
			  OldWd
		  end
	  end),
    {MaxNofTypes, MaxNofMeas} = Wd#mesh_type.max_nof_inst,
    {TypeAlarm, MeasAlarm}    = Wd#mesh_type.curr_nof_inst,
    TypeRecList = lists:keysort(#mesh_type.mrp_init, 
				get_all_types()),
    MeasRecList = lists:keysort(#mesh_meas.mrp, 
				get_all_meas()),
    check_type_mrps(TypeRecList),
    check_meas_mrps(MeasRecList),
    NofOwnTypes = length(?OWN_MEAS_TYPES),
    State = 
	#state{max_nof_types     = MaxNofTypes,
	       max_nof_meas      = MaxNofMeas,
	       type_alarm_active = TypeAlarm,
	       meas_alarm_active = MeasAlarm},
    watchdog_check(meas, watchdog_check(type, State)).



curr_nof_types() ->
    mnesia:table_info(mesh_type,size) - length(?OWN_MEAS_TYPES).


curr_nof_meas() ->
    mnesia:table_info(mesh_meas,size).



check_type_mrps([]) ->
    done;
check_type_mrps(L) ->
    check_type_mrps(L, undefined, false).




check_type_mrps([H | T], PrevMrp, PrevAlive) ->
    #mesh_type{name          = Type,
	       info          = Info,
	       mrp_init      = MRP,
	       mrp_init_node = Node} = H,
    MrpAlive = case MRP of
		   PrevMrp ->	
		       PrevAlive;
		   _Other ->
		       mrp_alive(MRP, Node)
	       end,
    case MrpAlive of
	false when MRP /= undefined ->
	    handle_terminated_type(H, {mesh_restarted,mrp_terminated});
	false ->
	    done;
	true ->
	    start_sup_mrp(MRP)
    end,
    check_type_mrps(T, MRP, MrpAlive);
check_type_mrps([], _PrevMrp, _PrevAlive) ->
    done.


    

check_meas_mrps([]) ->
    done;
check_meas_mrps(L) ->
    check_meas_mrps(L, undefined, false).



check_meas_mrps([H | T], PrevMrp, PrevAlive) ->
    #mesh_meas{id         = Meas,
	       type       = Type,
	       info       = Info,
	       mrp        = MRP,
	       node       = Node,
	       oper_state = OperState}  = H,
    case OperState of
	disabled ->
	       %% Already noted as terminated!
	    check_meas_mrps(T, MRP, false);
	enabled ->
	    MrpAlive = 
		case MRP of
		    PrevMrp ->
			PrevAlive;
		    _Other ->
			mrp_alive(MRP, Node)
		end,
	    case MrpAlive of
		false when MRP /= undefined ->
		    measurement_terminated(H#mesh_meas.id, {mesh_restarted,mrp_lost});
		   %% Ought to be impossible to have an MRP other than a pid here...
		   %% If error occurs we may have to alter our opinion, though.  :-)
		true ->
		    start_sup_mrp(MRP),
		    mnesia_write(H#mesh_meas{oper_state = enabled})
	    end,
	    check_meas_mrps(T, MRP, MrpAlive)
    end;
check_meas_mrps([], _PrevMrp, _PrevAlive) ->
    done.




node_up(Node) ->
    case get_meas_on_node(Node) of
	[] ->
	    done;
	MeasList ->
	    eva:asend_event(?NODEUP_EVENT, 
			    mesh_server,
			    {Node, calendar:universal_time()}),
	    connected_measurements(undefined, 
				   false,
				   lists:keysort(#mesh_meas.mrp, MeasList), 
				   Node)
    end,
    case get_types_on_node(Node) of
	[] ->
	    done;
	TypeList ->
	    connected_types(TypeList, Node)
    end.





connected_measurements(PrevMrp, Alive, [H|T], Node) ->
    #mesh_meas{id   = Meas,
	       type = Type,
	       info = Info,
	       mrp  = MRP}  = H,
    MrpAlive = case MRP of 
		   PrevMrp ->
		       Alive;
		   _Other ->
		       mrp_alive(MRP, Node)
	       end,
    case MrpAlive of
	false ->
	    measurement_terminated(H#mesh_meas.id, {mesh_restarted,mrp_lost});
	true ->
	    eva:asend_event(?MEAS_CONNECTED_EVENT, 
			    mesh_server,
			    {Meas, Type, nodeup, calendar:universal_time()}),
	    mnesia_write(H#mesh_meas{oper_state = enabled})
    end,
    connected_measurements(MRP, MrpAlive, T, Node);
connected_measurements(_PrevMrp, _Alive, [], _Node) ->
    done.
	    
	    
	    
	    
	    
connected_types([H | T], Node) ->
    #mesh_type{name     = Type,
	       info     = Info,
	       mrp_init = MRP} = H,
    case mrp_alive(MRP, Node) of
	false ->
	    handle_terminated_type(H, {mesh_restarted, mrp_lost}),
	    connected_types(T, Node);
	true ->
	    eva:asend_event(?TYPE_CONNECTED_EVENT, 
			    mesh_server,
			    {Type, nodeup, calendar:universal_time()}),
	    connected_types(T, Node)
    end;
connected_types([], _Node) ->
    done.




mrp_alive(Pid, Node) ->
    case catch rpc:block_call(Node, erlang, process_info, [Pid, status]) of
	{status,Status} ->
	    true;
	_Other ->
	    false
    end.





unconnected_types([H | T]) ->
    #mesh_type{name = Type,
	       info = Info}  = H,
    eva:asend_event(?TYPE_UNCONNECTED_EVENT, 
		    mesh_server,
		    {Type, nodedown, calendar:universal_time()}),
    unconnected_measurements(T);
unconnected_types([]) ->
    done.





unconnected_measurements([H | T]) ->
    #mesh_meas{id   = Meas,
	       type = Type,
	       info = Info} = H,
    eva:asend_event(?MEAS_UNCONNECTED_EVENT, 
		    mesh_server,
		    {Meas, Type, nodedown, calendar:universal_time()}),
    mnesia_write(H#mesh_meas{oper_state = disabled}),
       %% Measurements may be of many differerent types!
       %% Have to get type each time! (Well, not really, but easiest way...  :-} )
    unconnected_measurements(T);
unconnected_measurements([]) ->
    done.

	    




mnesia_write(Rec) ->
    {atomic,Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:write(Rec)
	  end).




register_type(Rec, MaybeLockArgs, State) ->
    #mesh_type{name          = TypeName,
	       callback_mod  = CallbackMod,
	       curr_nof_inst = CurrInst,
	       admin_state   = AdmState}  = Rec,

    NewAdmState = 
	case AdmState of
	    shutting_down when CurrInst == 0 ->
		locked;
	    _Other ->
		AdmState
	end,
    LockArgs = 
	case MaybeLockArgs of
	    undefined ->
		[];
	    {ok, Term} ->
		Term
	end,
    NewRec = Rec#mesh_type{admin_state = NewAdmState,
			   meas_args   = LockArgs},
    
    case type_registered(TypeName, CallbackMod) of
	{already_registered, OldType} ->
	    {{error,{already_registered,TypeName}}, State};
	{reregister, OldType} ->
	    {Result,LockArgsToUse} = reregister_type(NewRec, OldType, MaybeLockArgs),
	    NewState = 
		case NewAdmState of
		    locked ->
			{_ResultLock, TmpState} = lock_type(TypeName, LockArgsToUse, State),
			TmpState;
		    _OtherAdmState ->
			State
		end,
	    {Result, NewState};
	new_type ->
	    register_new_type(TypeName, CallbackMod, NewRec, State)
    end.




unregister_type(TypeName, Args, State) ->
    case lists:member(TypeName, ?OWN_MEAS_TYPES) of
	true ->
	    {{error, {reserved_type, TypeName}}, State};
	false ->
	    case type_registered(TypeName, {undefined}) of
		new_type ->
		    {{error,{no_such_type,TypeName}}, State};
		{already_registered, TypeRec} ->
		    #mesh_type{callback_mod = CallbackMod,
			       mrp_init     = MRP,
			       fault_id     = FaultId}  = TypeRec,
		    MeasList = get_measurements(TypeName),
		    NewState = delete_measurements(MeasList, Args, State),
		    case call_user(CallbackMod, terminate, [MRP]) of
			ok ->
			    mnesia_delete(mesh_type, TypeName),
			    case FaultId of
				undefined ->
				    done;
				_Other ->
				    eva:aclear_alarm(FaultId)
			    end,
			    NewState2 = watchdog_check(type, NewState),
			    {{unregistered,TypeName}, NewState2};
			{'EXIT', Reason} ->
			    {{error,{type_still_registered, Reason}}, NewState};
			{error, Reason} ->
			    {{error,{type_still_registered, Reason}}, NewState}
		    end
	    end
    end.
		    




list_types() ->
    TypeRecList = get_all_types(),
    Result = 
	lists:map(
	  fun(H) ->
		  #mesh_type{name          = Name, 
			     info          = Extra,
			     callback_mod  = CallbackMod,
			     max_nof_inst  = MaxInst,
			     curr_nof_inst = CurrInst,
			     admin_state   = AdminState} = H,
		  {Name, [{extra, Extra},
			  {interface_mod, CallbackMod},
			  {instances, CurrInst},
			  {max_instances, MaxInst},
			  {administrative_state, AdminState}
			 ]}
	  end, TypeRecList),
    Result.




lock_type(TypeName, Args, State) ->
    case type_registered(TypeName, {undefined}) of
	new_type ->
	    {{error,{no_such_type,TypeName}}, State};
	{already_registered, TypeRec} ->
	    MeasList = get_measurements(TypeName),
	    NewState = delete_measurements(MeasList, Args, State),
	    mnesia_write(TypeRec#mesh_type{admin_state   = locked,
					   meas_args     = Args,
					   curr_nof_inst = 0}),
	       %% The number of types isn't changed!
	    {{locked,TypeName}, NewState}
    end.
	    




unlock_type(TypeName) ->
    case type_registered(TypeName, {undefined}) of
	new_type ->
	    {error, {no_such_type,TypeName}};
	{already_registered, TypeRec} ->
	       %% Unlocking doesn't change the number of measurements!
	    mnesia_write(TypeRec#mesh_type{admin_state = unlocked}),
	    {unlocked,TypeName}
    end.




shut_down_type(TypeName) ->
    case type_registered(TypeName, {undefined}) of
	new_type ->
	    {error,{no_such_type,TypeName}};
	{already_registered, TypeRec} ->
	       %% Shutting down doesn't change the number of measurements!
	    case TypeRec#mesh_type.curr_nof_inst of
		N when N /= 0 ->
		    mnesia_write(TypeRec#mesh_type{admin_state = shutting_down}),
		    {shutting_down,TypeName};
		0 ->
		    mnesia_write(TypeRec#mesh_type{admin_state = locked}),
		    {locked,TypeName}
	    end
    end.


    



create_measurement(MeasRec, State) ->
    MeasName = MeasRec#mesh_meas.id,
       %% Check MeasName unused!
    case measurement_created(MeasName) of
	{already_created, _Rec} ->
	    {{error,{already_created,MeasName}}, State};
	new_meas ->
	    create_new_measurement(MeasName, MeasRec, State, false)
    end.





delete_measurement(MeasName, Args, State) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {{error,{no_such_measurement,MeasName}}, State};
	{already_created, MeasRec} ->
	    #mesh_meas{type         = TypeName,
		       info         = Info,
		       callback_mod = CallbackMod,
		       mrp          = MRP,
		       oper_state   = OperState}  = MeasRec,
	    case call_user(CallbackMod, delete_measurement, [MRP,MeasName,Args]) of
		{'EXIT', Reason} ->
		    {{error,{not_deleted,Reason}}, State};
		{error, Reason} ->
		    {{error,{not_deleted,Reason}}, State};
		ok ->
		    eva:asend_event(?MEAS_TERMINATED_EVENT,
				    mesh_server,
				    {MeasName, TypeName, measurement_deleted, 
				     calendar:universal_time()}),
		    mnesia_delete(mesh_meas, MeasName),
		    remove_meas_from_type_rec(TypeName),
		    NewState = watchdog_check(meas, State),
		    {{deleted,MeasName},NewState}
	    end
    end.






measurement_terminated(Meas, Reason) ->
    case measurement_created(Meas) of
	new_meas ->
	    done;
	{already_created, MeasRec} ->
	    #mesh_meas{type = Type,
		       info = Info} = MeasRec,
	    eva:asend_event(?MEAS_TERMINATED_EVENT,
			    mesh_server,
			    {Meas, Type, Reason, calendar:universal_time()}),
	       %% Contrary to popular belief the measurement still exists,
	       %% right now it is only disabled.  :-)
	       %% This means that "too many measurement" alarms shall NOT be 
	       %% cleared here! Only if the measurement is explicitly deleted
	       %% shall we investigate if any alarm shall be cleared!
	    mnesia_write(MeasRec#mesh_meas{oper_state = disabled})
    end.

	    




mrp_terminated(MRP, Reason) ->
    {atomic,MeasList} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:index_read(mesh_meas,
				    MRP,
				    #mesh_meas.mrp)
	  end),
    handle_terminated_measurements(MeasList, Reason),
    {atomic,TypeList} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:index_read(mesh_type,
				    MRP,
				    #mesh_type.mrp_init)
	  end),
    case TypeList of
	[] ->
	    done;
	[TypeRec] ->
	    handle_terminated_type(TypeRec, {mrp_terminated,Reason})
    end.




node_down(Node) ->
    case get_meas_on_node(Node) of
	[] ->
	    done;
	MeasList ->
	    eva:asend_event(?NODEDOWN_EVENT, 
			    mesh_server,
			    {Node, calendar:universal_time()}),
	    unconnected_measurements(MeasList)
    end,
    case get_types_on_node(Node) of
	[] ->
	    done;
	TypeList ->
	    unconnected_types(TypeList)
    end.
    
	       


add_meas_to_type_rec(Type) ->
    {already_registered,TypeRec} = type_registered(Type, {undefined}),
    #mesh_type{curr_nof_inst = NofInst} = TypeRec,
    NewNofInst = NofInst+1,
    mnesia_write(TypeRec#mesh_type{curr_nof_inst=NewNofInst}).



remove_meas_from_type_rec(Type) ->
    {already_registered,TypeRec} = type_registered(Type, {undefined}),
    #mesh_type{curr_nof_inst = NofInst,
	       admin_state   = AdmState} = TypeRec,
    NewNofInst = max(0, NofInst-1),
       %% Maybe we here ought to reset previous notification
       %% about too many instances (the capacity decrease case).
    NewAdmState = 
	case AdmState of
	    shutting_down when NewNofInst == 0 ->
		locked;
	    _Other ->
		AdmState
	end,
    mnesia_write(TypeRec#mesh_type{curr_nof_inst=NewNofInst,
				   admin_state=NewAdmState}).




handle_terminated_measurements([], _Reason) ->
    done;
handle_terminated_measurements([H | T], Reason) ->
    measurement_terminated(H#mesh_meas.id, {mrp_terminated,Reason}),
    handle_terminated_measurements(T, Reason).



handle_terminated_type(TypeRec, Reason) ->
    #mesh_type{name     = Name, 
	       info     = Info}  = TypeRec,
    eva:asend_event(?TYPE_FAILURE_EVENT,
		    mesh_server,
		    {Name, Reason, calendar:universal_time()}),
    mnesia_write(TypeRec#mesh_type{mrp_init      = undefined,
				   mrp_init_node = undefined,
				   curr_nof_inst = 0,
				   type_failure  = true}).

    




list_measurements(TypeName) ->
    case type_registered(TypeName, {undefined}) of
	new_type ->
	    {error,{no_such_type,TypeName}};
	{already_registered, TypeRec} ->
	    #mesh_type{callback_mod = CallbackMod,
		       mrp_init     = MRP}  = TypeRec,
	    MeasRecList = get_measurements(TypeName),
	    Result = 
		lists:keysort(1,
			      lists:map(
				fun(H) ->
					#mesh_meas{id            = MeasName, 
						   info          = Extra,
						   res_id        = ResId,
						   init_args     = InitArgs,
						   oper_state    = OperState,
						   admin_state   = AdmState} = H,
					{MeasName, [{extra, Extra},
						    {resources, ResId},
						    {initial_arguments, InitArgs},
						    {operability_state, OperState},
						    {administrative_state, AdmState}
						   ]}
				end, MeasRecList)
			     ),
	    Result
    end.
	    
	    



revive_measurement(Meas) ->
    case measurement_created(Meas) of
	new_meas ->
	    {error,{no_such_measurement,Meas}};
	{already_created, MeasRec} ->
	    case MeasRec#mesh_meas.oper_state of
		enabled ->
		    {error,{already_running,Meas}};
		disabled ->
		    Node = MeasRec#mesh_meas.node,
		    case lists:member(Node, [node() | nodes()]) of
			false ->   %% Must be a node here, Node cannot be 'undefined'!!!
			    {error,{nodedown,Node}};
			true ->
			    {Result, State} = 
				create_new_measurement(Meas, MeasRec, #state{}, true),
			    case Result of
				{created,Meas} ->
				       %% Have to set thresholds in measurement object again!
				    #mesh_meas{callback_mod     = CallbackMod,
					       mrp              = MRP,
					       upper_thresholds = Upper,
					       lower_thresholds = Lower}  = MeasRec,
				    revive_thresholds(upper, Upper, CallbackMod, MRP, Meas),
				    revive_thresholds(lower, Lower, CallbackMod, MRP, Meas),
				    case reset_measurement(Meas, MeasRec#mesh_meas.init_args) of
					{reset,Meas} ->
					    {revived,Meas};
					{error, Reason} ->
					    {error, {reset_failed,Reason}}
				    end;
				_Other ->
				    Result
			    end
		    end
	    end
    end.
    



revive_thresholds(ThreshType, [H | T], CallbackMod, MRP, Meas) ->
    #threshold{id      = Thresh,
	       status  = Status,
	       thresh1 = Value1,
	       thresh2 = Value2}  = H,
    Func = 
	case ThreshType of 
	    upper ->
		set_upper_threshold;
	    lower ->
		set_lower_threshold
	end,
    call_user(CallbackMod, Func, [MRP, Meas, Thresh, {Value1,Value2}, Status]),
    revive_thresholds(ThreshType, T, CallbackMod, MRP, Meas);
revive_thresholds(_ThreshType, [], _CallbackMod, _MRP, _Meas) ->
    done.




start_measurement(MeasName, Args) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    #mesh_meas{callback_mod     = CallbackMod,
		       mrp              = MRP,
		       oper_state       = OperState,
		       upper_thresholds = UpThresh,
		       lower_thresholds = LoThresh}  = MeasRec,
	    case OperState of
		disabled ->
		    {error, {measurement_disabled,MeasName}};
		enabled ->
		    case call_user(CallbackMod, start_measurement, [MRP,MeasName,Args]) of
			{'EXIT', Reason} ->
			    {error,{not_started,Reason}};
			{error, Reason} ->
			    {error,{not_started,Reason}};
			ok ->
			    NewUpThresh = reset_thresholds(UpThresh),
			    NewLoThresh = reset_thresholds(LoThresh),
			    mnesia_write(
			      MeasRec#mesh_meas{admin_state      = started,
						init_args        = Args,
						upper_thresholds = NewUpThresh,
						lower_thresholds = NewLoThresh}
			     ),
			    {started,MeasName}
		    end
	    end
    end.




stop_measurement(MeasName) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    #mesh_meas{callback_mod = CallbackMod,
		       oper_state   = OperState,
		       mrp          = MRP}  = MeasRec,
	    case OperState of
		disabled ->
		    {error, {measurement_disabled,MeasName}};
		enabled ->
		    case call_user(CallbackMod, stop_measurement, [MRP,MeasName]) of
			{'EXIT', Reason} ->
			    {error,{not_stopped,Reason}};
			{error, Reason} ->
			    {error,{not_stopped,Reason}};
			ok ->
			    mnesia_write(MeasRec#mesh_meas{admin_state = stopped}),
			    {stopped,MeasName}
		    end
	    end
    end.





reset_measurement(MeasName, Args) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    #mesh_meas{callback_mod     = CallbackMod,
		       mrp              = MRP,
		       upper_thresholds = UpThresh,
		       lower_thresholds = LoThresh,
		       max_tidemark     = MaxTideM,
		       min_tidemark     = MinTideM,
		       oper_state       = OperState}  = MeasRec,
	    case OperState of
		disabled ->
		    {error, {measurement_disabled,MeasName}};
		enabled ->
		    ResetTime = calendar:universal_time(),
		    case call_user(CallbackMod, reset_measurement, [MRP,MeasName,Args]) of
			{'EXIT', Reason} ->
			    {error,{not_reset,Reason}};
			{error, Reason} ->
			    {error,{not_reset,Reason}};
			ok ->
			    NewUpThresh = reset_thresholds(UpThresh),
			    NewLoThresh = reset_thresholds(LoThresh),
			    NewMaxTideM = reset_one_tidemark(MaxTideM, ResetTime),
			    NewMinTideM = reset_one_tidemark(MinTideM, ResetTime),
			    mnesia_write(
			      MeasRec#mesh_meas{init_args        = Args,
						upper_thresholds = NewUpThresh,
						lower_thresholds = NewLoThresh,
						max_tidemark     = NewMaxTideM,
						min_tidemark     = NewMinTideM,
						last_meas_value  = undefined,
						time_stamp       = undefined,
						meas_info        = []}
			     ),
			    {reset,MeasName}
		    end
	    end
    end.





measurement_report(MeasName, Value, Time, MeasInfo) ->
    case measurement_created(MeasName) of
	new_meas ->
	    done;
	{already_created, MeasRec} ->
	    #mesh_meas{type             = TypeName,
		       upper_thresholds = Upper,
		       lower_thresholds = Lower,
		       max_tidemark     = MaxTideM,
		       min_tidemark     = MinTideM,
		       oper_state       = OperState} = MeasRec,
	    case OperState of
		disabled ->
		    done;
		enabled ->
		    NewUpper = check_thresholds(upper, Value, Upper, 
						{MeasName,TypeName,Value,Time,MeasInfo}),
		    NewLower = check_thresholds(lower, Value, Lower, 
						{MeasName,TypeName,Value,Time,MeasInfo}),
		    NewMaxTideM = update_tidemark(max, Value, MaxTideM),
		    NewMinTideM = update_tidemark(min, Value, MinTideM),
		    eva:asend_event(?MEASUREMENT_REPORT, 
				    mesh_server,
				    {{name,MeasName}, 
				     {value,Value}, 
				     {time,Time}, 
				     {info, MeasInfo}}),
		    mnesia_write(
		      MeasRec#mesh_meas{last_meas_value  = Value,
					time_stamp       = Time,
					meas_info        = MeasInfo,
					upper_thresholds = NewUpper,
					lower_thresholds = NewLower,
					max_tidemark     = NewMaxTideM,
					min_tidemark     = NewMinTideM}
		     )
	    end
    end.








update_tidemark(_Type, Val, TideM) when TideM#tidemark.current == undefined ->
    TideM#tidemark{current = Val};
update_tidemark(max, Val, TideM) when Val > TideM#tidemark.current ->
    TideM#tidemark{current = Val};
update_tidemark(min, Val, TideM) when Val < TideM#tidemark.current ->
    TideM#tidemark{current = Val};
update_tidemark(_Type, _Val, TideM) ->
    TideM.
    
	    
	    
	    

check_thresholds(Type, Val, [], NoteInfo) ->
    [];
check_thresholds(Type, Val, L, NoteInfo) ->
    check_thresholds(Type, Val, L, NoteInfo, []).





check_thresholds(Type, Val, 
		 [H | T], NoteInfo, Acc) when H#threshold.status == enabled,
					      H#threshold.switch == on ->
    #threshold{thresh1 = Thresh1,
	       thresh2 = Thresh2}  = H,
    %% Compare Val to Thresh1!
    if 
	Val >= Thresh1, Type == upper ->
	    FaultId = eva:get_fault_id(),
	    {MeasName,TypeName,Value,Time,MeasInfo} = NoteInfo,
	    eva:asend_alarm(?THRESH_ALARM,                          %% Name
			    FaultId,                                %% FaultId
			    mesh_server,                            %% Sender
			    {upper_threshold_triggered, 
			     {value,Val}},                          %% Cause
			    {{meas,MeasName}, 
			     {id,H#threshold.id}}),                 %% Extra
	    check_thresholds(Type, Val, T, NoteInfo, 
			     [H#threshold{switch=off,fault_id=FaultId} | Acc]);
	Val =< Thresh1, Type == lower ->
	    FaultId = eva:get_fault_id(),
	    {MeasName,TypeName,Value,Time,MeasInfo} = NoteInfo,
	    eva:asend_alarm(?THRESH_ALARM,                          %% Name
			    FaultId,                                %% FaultId
			    mesh_server,                            %% Sender
			    {lower_threshold_triggered, 
			     {value,Val}},                          %% Cause
			    {{meas,MeasName}, 
			     {id,H#threshold.id}}),                 %% Extra
	    check_thresholds(Type, Val, T, NoteInfo, 
			     [H#threshold{switch=off,fault_id=FaultId} | Acc]);
	true ->
	    check_thresholds(Type, Val, T, NoteInfo, [H | Acc])
    end;
check_thresholds(Type, Val, 
		 [H | T], NoteInfo, Acc) when H#threshold.status == enabled ->
    #threshold{thresh1 = Thresh1,
	       thresh2 = Thresh2} = H,
    %% Compare Val to Thresh2!
    if
	Val =< Thresh2, Type == upper ->
	    eva:aclear_alarm(H#threshold.fault_id),
	    check_thresholds(Type, Val, T, NoteInfo, 
			     [H#threshold{switch=on,fault_id=undefined} | Acc]);
	Val >= Thresh2, Type == lower ->
	    eva:aclear_alarm(H#threshold.fault_id),
	    check_thresholds(Type, Val, T, NoteInfo, 
			     [H#threshold{switch=on,fault_id=undefined} | Acc]);
	true ->
	    check_thresholds(Type, Val, T, NoteInfo, [H | Acc])
    end;
check_thresholds(Type, Val, [H | T], NoteInfo, Acc) ->   %% Disabled thresholds
    check_thresholds(Type, Val, T, NoteInfo, [H | Acc]);    
check_thresholds(Type, Val, [], NoteInfo, Acc) ->
    lists:reverse(Acc).
	    
    
	       
    


get_measurement_report(Meas) ->
    case measurement_created(Meas) of
	new_meas ->
	    {error,{no_such_measurement,Meas}};
	{already_created, MeasRec} ->
	    #mesh_meas{last_meas_value = Value,
		       time_stamp      = Time,
		       meas_info       = Info}  = MeasRec,
	    {Meas, Value, Time, Info}
    end.





set_threshold(ThreshType, Meas, Thresh, {Value1,Value2}, Status) ->
    case measurement_created(Meas) of
	new_meas ->
	    {error,{no_such_measurement,Meas}};
	{already_created, MeasRec} ->
	    #mesh_meas{upper_thresholds = Upper,
		       lower_thresholds = Lower,
		       oper_state       = OperState}  = MeasRec,
	    case OperState of
		disabled ->
		    {error, {measurement_disabled,Meas}};
		enabled ->
		    %% Check threshold identifier unused!
		    case {lists:keymember(Thresh, #threshold.id, Upper),
			  lists:keymember(Thresh, #threshold.id, Lower)} of
			
			{false,false} ->
			    NewThreshRec = #threshold{id      = Thresh,
						      status  = Status,
						      thresh1 = Value1,
						      thresh2 = Value2,
						      switch  = on},
			    case ThreshType of
				upper ->
				       %% Tell the user application about the threshold.
				       %% Very optional for the user to use this, but
				       %% the opportunity shall be there, in case he wants
				       %% a "silent" measurement, just checking for a threshold,
				       %% and first then reporting.
				       %% Since the use of this is optional, we don't check
				       %% the return value.
				    CallbackMod = MeasRec#mesh_meas.callback_mod,
				    MRP         = MeasRec#mesh_meas.mrp,
				    call_user(CallbackMod, set_upper_threshold, [MRP, 
										 Meas, 
										 Thresh,
										 {Value1,Value2}, 
										 Status]),
				    mnesia_write(
				      MeasRec#mesh_meas{upper_thresholds = [NewThreshRec | Upper]}
				     );
				lower ->
				    CallbackMod = MeasRec#mesh_meas.callback_mod,
				    MRP         = MeasRec#mesh_meas.mrp,
				    call_user(CallbackMod, set_lower_threshold, [MRP, 
										 Meas, 
										 Thresh,
										 {Value1,Value2}, 
										 Status]),
				    mnesia_write(
				      MeasRec#mesh_meas{lower_thresholds = [NewThreshRec | Lower]}
				     )
			    end,
			    {threshold_set, {Meas,Thresh}};
			_Other ->
			    {error, {already_set, {Meas,Thresh}}}
		    end
	    end
    end.

		    




remove_threshold(MeasName, ThreshName) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    case MeasRec#mesh_meas.oper_state of
		disabled ->
		    {error, {measurement_disabled,MeasName}};
		enabled ->
		    case delete_one_threshold(ThreshName, 
					      MeasRec#mesh_meas.upper_thresholds) of
			{ok, List} ->
			    CallbackMod = MeasRec#mesh_meas.callback_mod,
			    MRP         = MeasRec#mesh_meas.mrp,
			    call_user(CallbackMod, remove_threshold, [MRP, MeasName,ThreshName]),
			    mnesia_write(MeasRec#mesh_meas{upper_thresholds = List}),
			    {threshold_removed, {MeasName,ThreshName}};
			false ->
			    case delete_one_threshold(
				   ThreshName, 
				   MeasRec#mesh_meas.lower_thresholds) of
				{ok, List} ->
				    CallbackMod = MeasRec#mesh_meas.callback_mod,
				    MRP         = MeasRec#mesh_meas.mrp,
				    call_user(CallbackMod, remove_threshold, [MRP, MeasName, 
									      ThreshName]),
				    mnesia_write(
				      MeasRec#mesh_meas{lower_thresholds = List}),
				    {threshold_removed, {MeasName,ThreshName}};
				false ->
				    {error, {no_such_threshold,{MeasName,ThreshName}}}
			    end
		    end
	    end
    end.
		    




remove_thresholds(MeasName) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    mnesia_write(MeasRec#mesh_meas{upper_thresholds = [],
					   lower_thresholds = []}),
	    {thresholds_removed,MeasName}
    end.





list_thresholds(MeasName) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    #mesh_meas{upper_thresholds = Upper,
		       lower_thresholds = Lower} = MeasRec,
	    F = fun(H) ->
			#threshold{id      = Name,
				   status  = Status,
				   thresh1 = Val1,
				   thresh2 = Val2} = H,
			{Name, Status, {Val1,Val2}}
		end,
	    UpRes  = lists:keysort(1, lists:map(F, Upper)),
	    LowRes = lists:keysort(1, lists:map(F, Lower)),
	    {MeasName, {upper_thresholds,UpRes}, {lower_thresholds,LowRes}}
    end.





set_threshold_status(Meas, Thresh, Status) ->
    case measurement_created(Meas) of
	new_meas ->
	    {error,{no_such_measurement,Meas}};
	{already_created, MeasRec} ->
	    CallbackMod = MeasRec#mesh_meas.callback_mod,
	    MRP         = MeasRec#mesh_meas.mrp,
	    {CallbackFun, RetStatus} = 
		case Status of
		    disabled ->
			F = fun(M,T) -> 
				    call_user(CallbackMod, disable_threshold, [MRP,M,T])
			    end,
			{F, threshold_disabled};
		    enabled ->
			F = fun(M,T) ->
				    call_user(CallbackMod, enable_threshold, [MRP,M,T])
			    end,
			{F, threshold_enabled}
		end,
	    Upper = MeasRec#mesh_meas.upper_thresholds,
	    case set_threshold_status2(Thresh, Status, Upper, []) of
		{ok, NewUpper} ->
		    CallbackFun(Meas,Thresh),
		    mnesia_write(MeasRec#mesh_meas{upper_thresholds = NewUpper}),
		    {RetStatus,{Meas,Thresh}};
		not_found ->
		    Lower = MeasRec#mesh_meas.lower_thresholds,
		    case set_threshold_status2(Thresh, Status, Lower, []) of
			{ok, NewLower} ->
			    CallbackFun(Meas,Thresh),
			    mnesia_write(
			      MeasRec#mesh_meas{lower_thresholds = NewLower}),
			    {RetStatus,{Meas,Thresh}};
			not_found ->
			    {error, {no_such_threshold,{Meas,Thresh}}}
		    end
	    end
    end.
		    
    




report_tidemarks(MeasName) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    #mesh_meas{type            = MeasType,
		       max_tidemark    = MaxTideM,
		       min_tidemark    = MinTideM}  = MeasRec,
	    #tidemark{current    = MaxCurr,
		      previous   = MaxPrev,
		      reset_time = MaxReset} = MaxTideM,
	    #tidemark{current    = MinCurr,
		      previous   = MinPrev,
		      reset_time = MinReset} = MinTideM,
	    {MeasName, MeasType, 
	     {max_tidemark, [{current,MaxCurr},
			     {previous,MaxPrev},
			     {reset,MaxReset}
			    ]},
	     {min_tidemark, [{current,MinCurr},
			     {previous,MinPrev},
			     {reset,MinReset}
			    ]}
	    }
    end.
		  
	    






reset_tidemarks(MeasName) ->
    case measurement_created(MeasName) of
	new_meas ->
	    {error,{no_such_measurement,MeasName}};
	{already_created, MeasRec} ->
	    #mesh_meas{max_tidemark    = MaxTideM,
		       min_tidemark    = MinTideM}  = MeasRec,
	    ResetTime = calendar:universal_time(),
	    NewMaxTideM = reset_one_tidemark(MaxTideM, ResetTime),
	    NewMinTideM = reset_one_tidemark(MinTideM, ResetTime),
	    mnesia_write(MeasRec#mesh_meas{max_tidemark = NewMaxTideM,
					   min_tidemark = NewMinTideM}
			),
	    {tidemarks_reset, MeasName}
    end.
		      

	    
	    


watchdog_setup(NofTypes, NofMeas, State) ->
    {atomic,Result} =  
	mnesia:transaction(
	  fun() ->
		  mnesia:read({mesh_type, mesh_watchdog})
	  end),
    WdRec = 
	case Result of
	    [] ->
		#state{type_alarm_active = TypeAlarm,
		       meas_alarm_active = MeasAlarm}  = State,
		NewWd = ?NEW_WATCHDOG_REC,
		NewWd#mesh_type{curr_nof_inst = {TypeAlarm,MeasAlarm}};
	    [OldWd] ->
		OldWd
	end,
    mnesia_write(WdRec#mesh_type{max_nof_inst = {NofTypes,NofMeas}}),
    NewState = watchdog_check(type, State#state{max_nof_types = NofTypes,
						max_nof_meas  = NofMeas}),
    NewState2 = watchdog_check(meas, NewState),
    {ok, NewState2}.
    

    


	    
	    
	    
	    

reset_one_tidemark(Max, ResetTime) ->
    Curr = Max#tidemark.current,
    Max#tidemark{current    = undefined,  %% This shall be the current value 
		                          %% of the associated measurement. 
		                          %% Here we choose to wait for the next
		                          %% value instead of using the previous
		                          %% one.
		 previous   = Curr,
		 reset_time = ResetTime}.
		  
		  

set_threshold_status2(Thresh, Status, [], Acc) ->
    not_found;
set_threshold_status2(Thresh, Status, [H | T], Acc) when H#threshold.id /= Thresh ->
    set_threshold_status2(Thresh, Status, T, [H | Acc]);
set_threshold_status2(Thresh, Status, [H | T], Acc) ->
       %% We don't know the history here! The user may try to enable an
       %% already enabled threshold, or he may try to disable an already
       %% disabled threshold, and so on. Have to check the switch field 
       %% each time, even if this means some unnecessary operations!
    case H#threshold.switch of
	on ->
	    done;
	off ->    %% Threshold alarm active here!
	    eva:aclear_alarm(H#threshold.fault_id)
    end,
    NewH = H#threshold{status   = Status, 
		       switch   = on,       
		       fault_id = undefined},
    {ok, lists:reverse(Acc) ++ [NewH | T]}.






reset_thresholds(ThreshList) ->
       %% Sets the on/off swith to on for all thresh1 values,
       %% and to off for all thresh2 values.
    lists:map(fun(H) ->
		      case H#threshold.switch of
			  on ->
			      done;
			  off ->
			         %% Switch can only be 'off' if threshold enabled!
			      eva:aclear_alarm(H#threshold.fault_id)
		      end,
		      H#threshold{switch=on, fault_id=undefined}
	      end, ThreshList).





delete_one_threshold(Thresh, List) ->
    case lists:keysearch(Thresh, #threshold.id, List) of
	{value, ThreshRec} ->
	    NewList = lists:keydelete(Thresh, #threshold.id, List),
	    case ThreshRec#threshold.switch of
		on ->
		    done;
		off ->
		    eva:aclear_alarm(ThreshRec#threshold.fault_id)
	    end,
	    {ok, NewList};
	false ->
	    false  %% Thresh not in list!
    end.




create_new_measurement(MeasName, MeasRec, State, Revival) ->
       %% Check type is registered!
    TypeName = MeasRec#mesh_meas.type,
    case type_registered(TypeName, {undefined}) of
	new_type ->
	    {{error,{no_such_type,TypeName}}, State};
	{already_registered, TypeRec} ->
	       %% Check node running!
	    Node = TypeRec#mesh_type.mrp_init_node,
	    case lists:member(Node, [node() | nodes()]) of
		false when Node /= undefined ->
		    {{error,{nodedown,Node}}, State};
		_Any ->
		    %% Check type OK!
		    case TypeRec#mesh_type.type_failure of
			true ->
			    {{error,{type_failure,no_mrp}}, State};
			false ->
			    %% Check type not busy!
			    UsageState = 
				case Revival of
				    false ->
					type_usage_state(TypeRec);
				    true ->
					case type_usage_state(TypeRec) of
					    busy ->
						MeasList   = get_measurements(TypeName),
						NofEnabled = length(
							       [E || 
								   E <- MeasList, 
								   E#mesh_meas.oper_state == enabled]),
						type_usage_state(
						  TypeRec#mesh_type{curr_nof_inst=NofEnabled});
					    OtherState ->
						OtherState
					end
				end,
			    case UsageState of
				busy ->
				    {{error,{type_busy,TypeName}}, State};
				_Other ->
				    %% Check type admin state!
				    case TypeRec#mesh_type.admin_state of
					locked ->
					    {{error,{type_locked,TypeName}}, State};
					shutting_down ->
					    {{error,{type_shutting_down,TypeName}}, State};
					unlocked ->
					    init_measurement(MeasName, TypeName, 
							     MeasRec, TypeRec, State)
				    end
			    end
		    end
	    end
    end.






init_measurement(MeasName, TypeName, MeasRec, TypeRec, State) ->
    #mesh_meas{res_id     = ResId,
	       init_args  = InitArgs,
	       oper_state = OldOperState} = MeasRec,
    
    #mesh_type{callback_mod = CallbackMod,
	       mrp_init     = MrpInit}  = TypeRec,
    
    case call_user(CallbackMod, create_measurement, [MrpInit, 
						     TypeName, 
						     MeasName, 
						     ResId, 
						     InitArgs]) of
	MRP when pid(MRP) ->
	    start_sup_mrp(MRP),
	    Node = get_mrp_node(MRP),
	       %% When tidemarks created they may be regarded as being reset!
	       %% Maybe we ought to save the creation time for the measurement?
	       %% On the other hand, the user may use the 'Extra' field for
	       %% this purpose.  :-/
	    mnesia_write(MeasRec#mesh_meas{callback_mod = CallbackMod,
					   mrp          = MRP,
					   node         = Node,
					   oper_state   = enabled}),
	    NewState = 
		case OldOperState of
		    disabled ->
			   %% Here we are reviving an old measurement,
			   %% which already has been counted!!!
			State;
		    _Other ->
			add_meas_to_type_rec(TypeName),
			watchdog_check(meas, State)
		end,
	    {{created,MeasName}, NewState};
	{'EXIT', Reason} ->
	    {{error,{create_failure,Reason}}, State};
	{error, Reason} ->
	    {{error,{create_failure,Reason}}, State};
	Other ->
	    {{error,{create_failure,no_pid_returned}}, State}
    end.

    



measurement_created(MeasName) ->
    {atomic,Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:read({mesh_meas,MeasName})
	  end),
    case Result of
	[OldMeasRec] ->
	    {already_created, OldMeasRec};
	[] ->
	    new_meas
    end.





mnesia_delete(Tab, Key) ->
    {atomic,Result} =
	mnesia:transaction(
	  fun() ->
		     %% We ought to use mnesia:delete here, but due to requirements
		     %% of the SNMP adaptation it is better to use the mnesia:delete_object
		     %% function, even if slightly slower.  :-/

		  case mnesia:read({Tab,Key}) of
		      [Rec] ->
			  mnesia:delete_object(Rec);
		      [] ->  
			     %% We allow someone else to mess with the table, as 
			     %% long as	the object *is* removed.
			  ok
		  end
	  end).
    






get_all_types() ->
    {atomic, Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:match_object(
		    mnesia:table_info(mesh_type, 
				      wild_pattern)
		   )
	  end),
    lists:keysort(#mesh_type.name, 
		  remove_own_meas_types(Result)
		 ).



get_all_meas() ->
    {atomic,Result} =
	mnesia:transaction(
	  fun() ->
		  mnesia:match_object(
		    mnesia:table_info(mesh_meas, 
				      wild_pattern)
		   )
	  end),
    lists:keysort(#mesh_meas.id, 
		  Result
		 ).
		  





remove_own_meas_types([]) ->
    [];
remove_own_meas_types(L) ->
    remove_own_meas_types(L, ?OWN_MEAS_TYPES).



remove_own_meas_types(L, []) ->
    L;
remove_own_meas_types(L, [H | T]) ->
    NewL = lists:keydelete(H,
			   #mesh_type.name, 
			   L),
    remove_own_meas_types(NewL, T).

    



get_types_on_node(Node) ->
    {atomic,Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:index_read(mesh_type,
				    Node, 
				    #mesh_type.mrp_init_node)
	  end),
    Result.





get_meas_on_node(Node) ->
    {atomic,Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:index_read(mesh_meas,
				    Node,
				    #mesh_meas.node)
	  end),
    Result.






get_measurements(TypeName) ->
    {atomic,Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:index_read(mesh_meas,
				    TypeName,
				    #mesh_meas.type)
	  end),
    Result.




delete_measurements([], _Args, State) ->
    State;
delete_measurements([H | T], Args, State) ->
       %% This function includes removal of stored measurement data.
    {Result,NewState} = delete_measurement(H#mesh_meas.id, Args, State),
    delete_measurements(T, Args, NewState).
    



register_new_type(TypeName, CallbackMod, Rec, State) ->
       %% Don't have to check usage state since new type!
    case init_type(CallbackMod, TypeName) of
	{error, Reason} ->
	    {{error,Reason}, State};
	{ok, MRP} ->
	    start_sup_mrp(MRP),
	    Node = get_mrp_node(MRP),
	    mnesia_write(Rec#mesh_type{mrp_init = MRP,
				       mrp_init_node = Node,
				       type_failure  = false}),
	    NewState = watchdog_check(type, State),
	    {{registered,TypeName}, NewState}
    end.



get_mrp_node(undefined) ->
    undefined;
get_mrp_node(Pid) ->
    node(Pid).




type_registered(TypeName, CallbackMod) ->
    {atomic,Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:read({mesh_type,TypeName})
	  end),
    case Result of
	[OldType] when OldType#mesh_type.callback_mod == CallbackMod ->
	    {reregister, OldType};
	[OldType] ->
	    {already_registered, OldType};
	[] ->
	    new_type
    end.




init_type(CallbackMod, TypeName) ->
    case call_user(CallbackMod, init, [TypeName]) of
	{'EXIT', Reason} ->
	    {error, Reason};
	MRP when pid(MRP) ->
	    {ok, MRP};
	undefined ->
	    {ok, undefined};
	Other ->
	    {error, {no_pid_returned,Other}}
    end.





reregister_type(NewRec, OldRec, MaybeLockArgs) ->
    #mesh_type{info          = NewInfo,
	       name          = TypeName,
	       callback_mod  = CallbackMod,
	       max_nof_inst  = NewMaxInst,
	       admin_state   = NewAdminState} = NewRec,

    #mesh_type{mrp_init      = OldMrp,
	       curr_nof_inst = OldCurrInst,
	       mrp_init_node = OldNode,
	       fault_id      = OldFaultId,
	       meas_args     = OldLockArgs} = OldRec,

    NewLockArgs = 
	case MaybeLockArgs of
	    undefined ->
		OldLockArgs;
	    {ok, Term} ->
		Term
	end,
    
    case mrp_alive(OldMrp, OldNode) of
	true ->
	       %% Don't call init function!
	    NewFaultId = check_capacity_decrease(OldCurrInst, NewMaxInst, 
						 OldFaultId, TypeName),
	    UpdRec = OldRec#mesh_type{info         = NewInfo,
				      max_nof_inst = NewMaxInst,
				      admin_state  = NewAdminState,
				      meas_args    = NewLockArgs,
				      fault_id     = NewFaultId},
	    mnesia_write(UpdRec),
	    {{reregistered,TypeName}, NewLockArgs};
	false ->
	    case init_type(CallbackMod, TypeName) of
		{error,Reason} ->
		    {error,Reason};
		{ok, NewMRP} ->
		    start_sup_mrp(NewMRP),
		    NewNode = get_mrp_node(NewMRP),
		    NewFaultId = check_capacity_decrease(OldCurrInst, NewMaxInst,
							 OldFaultId, TypeName),
		    UpdRec = OldRec#mesh_type{info          = NewInfo,
					      max_nof_inst  = NewMaxInst,
					      admin_state   = NewAdminState,
					      meas_args     = NewLockArgs,
					      mrp_init      = NewMRP,
					      mrp_init_node = NewNode,
					      type_failure  = false,
					      fault_id      = NewFaultId},
		    mnesia_write(UpdRec),
		    {{reregistered,TypeName}, NewLockArgs}
	    end
    end.




check_capacity_decrease(Curr, Max, undefined, _Type) when Curr =< Max ->
    undefined;
check_capacity_decrease(Curr, Max, FaultId, _Type) when Curr =< Max ->
    eva:aclear_alarm(FaultId),
    undefined;
check_capacity_decrease(Curr, Max, FaultId, Type) when FaultId /= undefined ->
    eva:asend_alarm(?MEASTYPE_ALARM,         %% Name
		    FaultId,                 %% FaultId
		    mesh_server,             %% Sender
		    {{type,Type},
		     {allowed,Max}, 
		     {currently,Curr}},      %% Cause
		    "Capacity decreased"),   %% Extra
    FaultId;
check_capacity_decrease(Curr, Max, undefined, Type) when Curr > Max ->
    FaultId = eva:get_fault_id(),
    check_capacity_decrease(Curr, Max, FaultId, Type).




watchdog_check(type, State) ->
    #state{max_nof_types     = Max,
	   type_alarm_active = AlarmActive} = State,
    NewAlarmActive = 
	watchdog_check_common(Max, curr_nof_types(), AlarmActive, type),
    State#state{type_alarm_active = NewAlarmActive};
watchdog_check(meas, State) ->
    #state{max_nof_meas      = Max,
	   meas_alarm_active = AlarmActive} = State,
    NewAlarmActive = 
	watchdog_check_common(Max, curr_nof_meas(), AlarmActive, meas),
    State#state{meas_alarm_active = NewAlarmActive}.






watchdog_check_common(Max, Curr, AlarmActive, Notification) ->
    case watchdog_compare(Max, Curr) of
	ok ->
	    case AlarmActive of
		true ->
		    Wd = get_watchdog_rec(),
		    #mesh_type{curr_nof_inst = {OldTypeAlarm,OldMeasAlarm},
			       admin_state   = {OldTypeFaultId,OldMeasFaultId}}  = Wd,
		    {NewAlarmTuple, NewFaultTuple} =
			case Notification of
			    type ->
				eva:aclear_alarm(OldTypeFaultId),
				{{false, OldMeasAlarm}, {undefined,OldMeasFaultId}};
			    meas ->
				eva:aclear_alarm(OldMeasFaultId),
				{{OldTypeAlarm, false}, {OldTypeFaultId,undefined}}
			end,
		    store_watchdog_rec(Wd#mesh_type{curr_nof_inst = NewAlarmTuple});
		false ->
		    done
	    end,
	    false;
	notify ->
	    Wd = get_watchdog_rec(),
	    #mesh_type{curr_nof_inst = {OldTypeAlarm,OldMeasAlarm},
		       admin_state   = {OldTypeFaultId,OldMeasFaultId}}  = Wd,
	    {NewAlarmTuple, NewFaultTuple} =
		case Notification of
		    type ->
			%% May have sent alarm already previously!
			FaultId = case OldTypeFaultId of
				      undefined ->
					  eva:get_fault_id();
				      _Other ->
					  OldTypeFaultId
				  end,
			eva:asend_alarm(?TYPE_ALARM,                  %% Name
					FaultId,                      %% FaultId
					mesh_server,                  %% Sender
					{{allowed,Max},
					 {currently,Curr}},           %% Cause
					""),                          %% Extra
			{{true,OldMeasAlarm}, {FaultId,OldMeasFaultId}};
		    meas ->
			%% May have sent alarm already previously!
			FaultId = case OldMeasFaultId of
				      undefined ->
					  eva:get_fault_id();
				      _Other ->
					  OldMeasFaultId
				  end,
			eva:asend_alarm(?MEAS_ALARM,                 %% Name
					FaultId,                     %% FaultId
					mesh_server,                 %% Sender
					{{allowed,Max},
					 {currently,Curr}},          %% Cause
					""),                         %% Extra
			{{OldTypeAlarm,true}, {OldTypeFaultId,FaultId}}
		end,
	    store_watchdog_rec(Wd#mesh_type{curr_nof_inst = NewAlarmTuple,
					    admin_state   = NewFaultTuple}),
	    true
    end.
    





get_watchdog_rec() ->
    {atomic,Wd} =  
	mnesia:transaction(
	  fun() ->
		  [OldWd] = mnesia:read({mesh_type, 
					 mesh_watchdog}),
		  OldWd
	  end),
    Wd.




store_watchdog_rec(Wd) ->
    {atomic,Result} =
	mnesia:transaction(
	  fun() ->
		  mnesia:write(Wd)
	  end).




watchdog_compare(infinity, _Any) ->
    ok;
watchdog_compare(Max, Curr) when Curr =< Max ->
    ok;
watchdog_compare(Max, Curr) ->
    notify.






type_usage_state(#mesh_type{max_nof_inst=Max,curr_nof_inst=Curr}) when Max =< 0 ->
       %% By definition the type is busy if no instances are allowed!
    busy;
type_usage_state(#mesh_type{max_nof_inst=Max,curr_nof_inst=0}) ->
    idle;
type_usage_state(#mesh_type{max_nof_inst=Max,curr_nof_inst=Curr}) when Curr < Max ->
    active;
type_usage_state(#mesh_type{max_nof_inst=Max,curr_nof_inst=Curr}) ->
    busy.




start_sup_mrp(undefined) ->
    done;
start_sup_mrp(Pid) ->
    link(Pid).



max(N1, N2) when N1 >= N2 ->
    N1;
max(N1, N2) ->
    N2.

    

register_alarms() ->
       %% Register alarms!
    eva:register_alarm(?THRESH_ALARM, ?LOG_STATUS, ?THRESH_CLASS, ?THRESH_SEVERITY),
    eva:register_alarm(?TYPE_ALARM, ?LOG_STATUS, ?TYPE_CLASS, ?TYPE_SEVERITY),
    eva:register_alarm(?MEAS_ALARM, ?LOG_STATUS, ?MEAS_CLASS, ?MEAS_SEVERITY),
    eva:register_alarm(?MEASTYPE_ALARM, ?LOG_STATUS, ?MEASTYPE_CLASS, ?MEASTYPE_SEVERITY).



register_events() ->
       %% Register events!
    eva:register_event(?TYPE_FAILURE_EVENT, ?LOG_STATUS),
    eva:register_event(?MEAS_TERMINATED_EVENT, ?LOG_STATUS),
    eva:register_event(?NODEUP_EVENT, ?LOG_STATUS),
    eva:register_event(?NODEDOWN_EVENT, ?LOG_STATUS),
    eva:register_event(?TYPE_UNCONNECTED_EVENT, ?LOG_STATUS),
    eva:register_event(?MEAS_UNCONNECTED_EVENT, ?LOG_STATUS),
    eva:register_event(?TYPE_CONNECTED_EVENT, ?LOG_STATUS),
    eva:register_event(?MEAS_CONNECTED_EVENT, ?LOG_STATUS),
    eva:register_event(?MEASUREMENT_REPORT, ?LOG_STATUS).
    
    



create_type_table(Nodes) ->
    case catch mnesia:create_table(mesh_type, 
			     [{type, set},
			      {index, [#mesh_type.mrp_init,
				       #mesh_type.mrp_init_node]},
			      {attributes, record_info(fields, mesh_type)},
			      {disc_copies, Nodes}
			     ]) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists,TableName}} ->
	    io:format("WARNING: Table ~p already exists!~n", [TableName]),
	    ok;
	Error ->
	    {error, Error}
    end.



create_meas_table(Nodes) ->
    case catch mnesia:create_table(mesh_meas, 
				   [{type, set},
				    {index, [#mesh_meas.type,
					     #mesh_meas.mrp,
					     #mesh_meas.node]},
				    {attributes, record_info(fields, mesh_meas)},
				    {disc_copies, Nodes}
				   ]) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists,TableName}} ->
	    io:format("WARNING: Table ~p already exists!~n", [TableName]),
	    ok;
	Error ->
	    {error, Error}
    end.
	


call_user(Mod, Fcn, Args) ->
    Pid = spawn_link(?MODULE, executer, [self(),Mod,Fcn,Args]),
    receive
	{Pid, Result} ->
	    receive
		{'EXIT', Pid, {ignore_exit,execution_completed}} ->
		    done
	    end,
	    Result;
	{'EXIT', Pid, Reason} ->
	    {error, Reason}
    after
	5000 ->
	    {error, {function_call_timeout, {Mod, Fcn, Args}}}
    end.
	    


executer(Parent, Mod, Fcn, Args) ->
    Parent ! {self(), catch apply(Mod, Fcn, Args)},
    exit({ignore_exit,execution_completed}).




check_module(Mod) ->
    case code:is_loaded(Mod) of
	{file, _Loaded} ->
	    ok;
	false ->
	    case code:load_file(Mod) of
		{module,Mod} ->
		    ok;
		_Other ->
		    {error, no_module}
	    end
    end.




set_lock_args(TypeName, LockArgs) ->
    {atomic,Result} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:read({mesh_type,TypeName})
	  end),
    case Result of
	[Rec] ->
	    mnesia_write(Rec#mesh_type{meas_args = LockArgs}),
	    {lock_args_set, TypeName};
	[] ->
	    {error, {no_such_type,TypeName}}
    end.
    
