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
%%%*********************************************************************
%%% 
%%%   Description:      This module contains the API functions for the 
%%%                     measurement handler system.
%%%
%%%*********************************************************************

-module(mesh).

-include("mesh.hrl").


   %% Initialisation exports.
-export([create_tables/1
	]).


   %% API exports.
-export([register_type/4, 
	 register_type/5,
	 unregister_type/1,
	 unregister_type/2,
	 list_types/0,
	 lock_type/1,
	 lock_type/2,
	 unlock_type/1,
	 shut_down_type/1,
	 create_measurement/4, 
	 create_measurement/5, 
	 create_measurement/6,
	 delete_measurement/1,
	 delete_measurement/2,
	 measurement_terminated/2,
	 list_measurements/1,
	 revive_measurement/1,
	 start_measurement/1, 
	 start_measurement/2,
	 stop_measurement/1,
	 reset_measurement/1,
	 reset_measurement/2,
	 measurement_report/3,
	 measurement_report/4,
	 get_measurement_report/1,
	 set_upper_threshold/3,
	 set_upper_threshold/4,
	 set_lower_threshold/3,
	 set_lower_threshold/4,
	 remove_threshold/2,
	 remove_thresholds/1,
	 list_thresholds/1,
	 enable_threshold/2,
	 disable_threshold/2,
	 report_tidemarks/1,
	 reset_tidemarks/1,
	 watchdog_setup/2
	]).


   %% Log handling exports.
-export([set_alarm_filter/1,
	 set_event_filter/1,
	 set_measurement_filter/1,
	 reset_alarm_filter/0,
	 reset_event_filter/0,
	 reset_measurement_filter/0
	]).


   %% SNMP exports - only used by SNMP adaption!
-export([check_callback_module/1,
	 register_type/6,
	 set_lock_args/2
	]).


   %% Debug exports.
-export([remove_logs/0, 
	 state/0,
	 alive/0
	]).



%% Timeout for calls to the globally registered mesh_server.
-define(TIME, 10000).




%%%*********************************************************************
%%%  EXPORTED FUNCTIONS
%%%*********************************************************************


%%======================================================================
%% Function:      create_tables/1
%%
%% Return Value:  ok  |  {error, Reason}
%%
%% Description:   Creates the Mnesia tables needed for the MESH application.
%%
%% Parameters:    Nodes  -- The nodes MESH shall be able to run on.
%%======================================================================

create_tables(Nodes) ->
    mesh_server:create_tables(Nodes).



%%======================================================================
%% Function:      register_type
%%
%% Return Value:  {registered, TypeId}    |
%%                {reregistered, TypeId}  | 
%%                {error, Reason}
%%
%% Description:   Registers a new measurement type in the MESH server.
%%
%% Parameters:    TypeId        -- A unique identifier that, from now on,
%%                                 will be used to identify this measure-
%%                                 ment type.
%%                Extra         -- User supplied information telling more
%%                                 about the measurement type.
%%                InterfaceMod  -- Callback module the MESH server will
%%                                 use when operations concerning the 
%%                                 measurement type shall be performed.
%%                NofInst       -- The allowed number of measurement objects
%%                                 belonging to this type.
%%                                 Possible values: infinity | 
%%                                                  <any non-negative integer>
%%                AdminState    -- Tells the administrative state of the
%%                                 measurement type.
%%                                 Possible values:  unlocked | shutting_down | 
%%                                                   locked.
%%                                 Default value: unlocked
%%======================================================================

register_type(TypeId, Extra, InterfaceMod, NofInst) ->
    register_type(TypeId, Extra, InterfaceMod, NofInst, unlocked).

register_type(TypeId, Extra, InterfaceMod, NofInst, AdminState) when NofInst >= 0 ->
    if 
	AdminState /= unlocked, AdminState /= locked, AdminState /= shutting_down ->
	    {error, {bad_admin_state,AdminState}};
	true ->
	    gen_server:call({global,mesh_server}, 
			    {register_type,TypeId,Extra,InterfaceMod,NofInst,AdminState},
			    ?TIME)
    end;
register_type(_TypeId, _Extra, _InterfaceMod, NofInst, _AdminState) ->
    {error, {positive_numbers_required, NofInst}}.



%%======================================================================
%% Function:      unregister_type
%%
%% Return Value:  {unregistered, TypeId}  | 
%%                {error, Reason}
%%
%% Description:   Unregisters a measurement type in the MESH server.
%%
%% Parameters:    TypeId  -- The identifier used to identify the measure-
%%                           ment type.
%%                Args    -- List of arguments that, in the case of created
%%                           measurements, will be passed on to the 
%%                           delete_measurement function in the measurement 
%%                           type interface.
%%======================================================================

unregister_type(TypeId) ->
    unregister_type(TypeId, []).

unregister_type(TypeId, Args) ->
    gen_server:call({global,mesh_server}, 
		    {unregister_type,TypeId,Args},
		    ?TIME).


%%======================================================================
%% Function:      list_types
%%
%% Return Value:  [TypeInfo]  | 
%%                {error, Reason}
%%
%%                TypeInfo = {TypeId, [Info]}
%%                Info = {Tag, term()}
%%
%% Description:   Lists all types currently registered in the MESH server,
%%                as well as info about each type.
%%
%% Parameters:    None.
%%======================================================================

list_types() ->
    gen_server:call({global,mesh_server},
		    list_types,
		    ?TIME).
    

%%======================================================================
%% Function:      lock_type
%%
%% Return Value:  {locked, TypeId}  | 
%%                {error, Reason}
%%
%% Description:   Locks the specified measurement type - this includes 
%%                termination of ongoing associated measurements.
%%
%% Parameters:    TypeId  -- The identifier used to identify the measure-
%%                           ment type.
%%                Args    -- List of arguments that, in the case of created
%%                           measurements, will be passed on to the 
%%                           delete_measurement function in the measurement 
%%                           type interface.
%%======================================================================

lock_type(TypeId) ->
    lock_type(TypeId, []).

lock_type(TypeId, Args) ->
    gen_server:call({global,mesh_server},
		    {lock_type,TypeId,Args},
		    ?TIME).


%%======================================================================
%% Function:      unlock_type
%%
%% Return Value:  {unlocked, TypeId}  | 
%%                {error, Reason}
%%
%% Description:   Unlocks the specified measurement type, thereby making 
%%                it possible to create new associated measurements.
%%
%% Parameters:    TypeId  -- The identifier used to identify the measure-
%%                           ment type.
%%======================================================================

unlock_type(TypeId) ->
    gen_server:call({global,mesh_server},
		    {unlock_type,TypeId},
		    ?TIME).


%%======================================================================
%% Function:      shut_down_type
%%
%% Return Value:  {shutting_down, TypeId}  | 
%%                {locked, TypeId}         |
%%                {error, Reason}
%%
%% Description:   Shuts down the specified measurement type. This means  
%%                that already existing measurements will continue to 
%%                exist, but it will be impossible to create new measurements.
%%                When the last measurement has been deleted, the type will 
%%                be locked. If no measurements exist when the function is
%%                called, the type will be locked immediately.
%%
%% Parameters:    TypeId  -- The identifier used to identify the measure-
%%                           ment type.
%%======================================================================

shut_down_type(TypeId) ->
    gen_server:call({global,mesh_server},
		    {shut_down_type,TypeId},
		    ?TIME).


%%======================================================================
%% Function:      create_measurement
%%
%% Return Value:  {created, MeasId}  | 
%%                {error, Reason}
%%
%% Description:   Creates a new measurement object, belonging to the 
%%                specified measurement type.
%%
%% Parameters:    MeasId      -- The identifier used to identify the 
%%                               measurement object.
%%                TypeId      -- The identifier used to identify the 
%%                               measurement type.
%%                Extra       -- User supplied information telling more 
%%                               about the measurement object.
%%                ResId       -- Identifier(s) of the resource(s) the
%%                               measurement object shall use.
%%                AdminState  -- The initial administrative state of the
%%                               measurement object.
%%                               Possible values: started | stopped
%%                               Default value: started
%%                InitArgs    -- List of initial arguments that will be 
%%                               passed on to the create_measurement function
%%                               in the measurement type interface.
%%                               Default value: []
%%======================================================================

create_measurement(MeasId, TypeId, Extra, ResId) ->
    create_measurement(MeasId, TypeId, Extra, ResId, started, []).

create_measurement(MeasId, TypeId, Extra, ResId, AdminState) ->
    create_measurement(MeasId, TypeId, Extra, ResId, AdminState, []).
    
create_measurement(MeasId, TypeId, Extra, ResId, started, InitArgs) ->
    gen_server:call({global,mesh_server},
		    {create_measurement,MeasId,TypeId,Extra,ResId,started,InitArgs},
		    ?TIME);
create_measurement(MeasId, TypeId, Extra, ResId, stopped, InitArgs) ->
    gen_server:call({global,mesh_server},
		    {create_measurement,MeasId,TypeId,Extra,ResId,stopped,InitArgs},
		    ?TIME);
create_measurement(_MeasId, _TypeId, _Extra, _ResId, AdminState, _InitArgs) ->
    {error, {bad_admin_state,AdminState}}.


%%======================================================================
%% Function:      delete_measurement
%%
%% Return Value:  {deleted, MeasId}  | 
%%                {error, Reason}
%%
%% Description:   Deletes a previously created measurement object, and
%%                also removes all associated information stored.
%%
%% Parameters:    MeasId  -- The identifier used to identify the 
%%                           measurement object.
%%                Args    -- List of arguments that will be passed on to
%%                           the delete_measurement function in the
%%                           measurement type interface.
%%======================================================================

delete_measurement(MeasId) ->
    delete_measurement(MeasId, []).

delete_measurement(MeasId, Args) ->
    gen_server:call({global,mesh_server},
		    {delete_measurement,MeasId,Args},
		    ?TIME).


%%======================================================================
%% Function:      measurement_terminated
%%
%% Return Value:  ok
%%
%% Description:   Tells the measurement handler about terminated measure-
%%                ment objects.
%%
%% Parameters:    MeasId  -- The measurement identifier.
%%                Reason  -- The reason for terminating, as specified by 
%%                           the user.
%%======================================================================

measurement_terminated(MeasId, Reason) ->
    gen_server:cast({global,mesh_server},
		    {measurement_terminated,MeasId,Reason}),
    ok.


%%======================================================================
%% Function:      list_measurements
%%
%% Return Value:  [MeasInfo]  | 
%%                {error, Reason}
%%
%%                MeasInfo = {MeasId, [Info]}
%%                Info = {Tag, term()}
%%
%% Description:   Lists all created measurement objects belonging to the 
%%                specified measurement type, as well as info about each 
%%                measurement.
%%
%% Parameters:    TypeId  -- The identifier used to identify the measure-
%%                           ment type.
%%======================================================================

list_measurements(TypeId) ->
    gen_server:call({global,mesh_server},
		    {list_measurements,TypeId},
		    ?TIME).


%%======================================================================
%% Function:      revive_measurement
%%
%% Return Value:  {revived, MeasId}  | 
%%                {error, Reason}
%%
%% Description:   Tries to recreate a previously started, but later on 
%%                terminated measurement. The last known settings are 
%%                used.
%%
%% Parameters:    MeasId  -- The measurement identifier.
%%======================================================================

revive_measurement(MeasId) ->
    gen_server:call({global,mesh_server},
		    {revive_measurement,MeasId},
		    ?TIME).


%%======================================================================
%% Function:      start_measurement
%%
%% Return Value:  {started, MeasId}  | 
%%                {error, Reason}
%%
%% Description:   Orders the specified measurement object to start running.
%%
%% Parameters:    MeasId     -- The identifier used to identify the measure-
%%                              ment object.
%%                StartArgs  -- List of arguments to be passed on to the
%%                              start_measurement function in the measure-
%%                              ment type interface.
%%======================================================================
    
start_measurement(MeasId) ->
    start_measurement(MeasId, []).

start_measurement(MeasId, StartArgs) ->
    gen_server:call({global, mesh_server},
		    {start_measurement,MeasId,StartArgs},
		    ?TIME).


%%======================================================================
%% Function:      stop_measurement
%%
%% Return Value:  {stopped, MeasId}  | 
%%                {error, Reason}
%%
%% Description:   Orders the specified measurement object to stop running.
%%
%% Parameters:    MeasId   -- The identifier used to identify the measure-
%%                            ment object.
%%======================================================================
    
stop_measurement(MeasId) ->
    gen_server:call({global, mesh_server},
		    {stop_measurement,MeasId},
		    ?TIME).


%%======================================================================
%% Function:      reset_measurement
%%
%% Return Value:  {reset, MeasId}  | 
%%                {error, Reason}
%%
%% Description:   Resets internally a specified measurement, and tells 
%%                the specified user measurement application to also 
%%                reset.
%%
%% Parameters:    MeasId  -- The identifier used to identify the measure-
%%                           ment object.
%%                Args    -- List of arguments that will be passed to 
%%                           the reset function in the measurement type
%%                           interface.
%%======================================================================

reset_measurement(MeasId) ->
    reset_measurement(MeasId, []).

reset_measurement(MeasId, Args) ->
    gen_server:call({global, mesh_server},
		    {reset_measurement,MeasId,Args},
		    ?TIME).


%%======================================================================
%% Function:      measurement_report
%%
%% Return Value:  ok
%%
%% Description:   Measurement value reports, from a measurement to the
%%                measurement handler.
%%
%% Parameters:    MeasId     -- The measurement identifier.
%%                Value      -- The measurement value calculated by the 
%%                              measurement object.
%%                TimeStamp  -- Value associating the measurement value
%%                              with a moment in time.
%%                Extra      -- User supplied extra information concerning
%%                              the measurement report.
%%                              Default value: []
%%======================================================================

measurement_report(MeasId, Value, TimeStamp) ->
    measurement_report(MeasId, Value, TimeStamp, []).

measurement_report(MeasId, Value, TimeStamp, Extra) ->
    gen_server:cast({global, mesh_server},
		    {measurement_report,MeasId,Value,TimeStamp,Extra}),
    ok.


%%======================================================================
%% Function:      get_measurement_report
%%
%% Return Value:  {MeasId, Value, TimeStamp, MeasInfo} |
%%                {error, Reason}
%%
%% Description:   Returns the last measurement value (associated with the 
%%                specified measurement) received by the measurement
%%                handler.
%%
%% Parameters:    MeasId  -- The identifier used to identify the measure-
%%                           ment object.
%%======================================================================

get_measurement_report(MeasId) ->
    gen_server:call({global, mesh_server},
		    {get_measurement_report,MeasId},
		    ?TIME).


%%======================================================================
%% Function:      set_upper_threshold
%%
%% Return Value:  {threshold_set, {MeasId,ThreshId}} | {error, Reason}
%%
%% Description:   Sets an upper threshold associated with the specified 
%%                measurement.
%%
%% Parameters:    MeasId    -- The measurement identifier.
%%                ThreshId  -- The threshold identifier, used to identify 
%%                             the threshold within this measurement.
%%                Value     -- The threshold value.
%%                             Possible values:  integer | {integer, integer}
%%                Status    -- Initial threshold status.
%%                             Possible values:  enabled | disabled
%%                             Default value: enabled
%%======================================================================

set_upper_threshold(MeasId, ThreshId, Value) ->
    set_upper_threshold(MeasId, ThreshId, Value, enabled).

set_upper_threshold(MeasId, ThreshId, Value, Status) when tuple(Value), 
							  size(Value) == 2 ->
    gen_server:call({global, mesh_server},
		    {set_threshold,upper,MeasId,ThreshId,Value,Status},
		    ?TIME);
set_upper_threshold(MeasId, ThreshId, Value, Status) ->
    set_upper_threshold(MeasId, ThreshId, {Value,Value}, Status).


%%======================================================================
%% Function:      set_lower_threshold
%%
%% Return Value:  {threshold_set, {MeasId,ThreshId}} | {error, Reason}
%%
%% Description:   Sets an lower threshold associated with the specified 
%%                measurement.
%%
%% Parameters:    MeasId    -- The measurement identifier.
%%                ThreshId  -- The threshold identifier, used to identify 
%%                             the threshold within this measurement.
%%                Value     -- The threshold value.
%%                             Possible values:  integer | {integer, integer}
%%                Status    -- Initial threshold status.
%%                             Possible values:  enabled | disabled
%%                             Default value: enabled
%%======================================================================

set_lower_threshold(MeasId, ThreshId, Value) ->
    set_lower_threshold(MeasId, ThreshId, Value, enabled).

set_lower_threshold(MeasId, ThreshId, Value, Status) when tuple(Value), 
							  size(Value) == 2 ->
    gen_server:call({global, mesh_server},
		    {set_threshold,lower,MeasId,ThreshId,Value,Status},
		    ?TIME);
set_lower_threshold(MeasId, ThreshId, Value, Status) ->
    set_lower_threshold(MeasId, ThreshId, {Value,Value}, Status).


%%======================================================================
%% Function:      remove_threshold
%%
%% Return Value:  {threshold_removed, {MeasId,ThreshId}} | {error, Reason}
%%
%% Description:   Removes, for a specified measurement, a specified threshold
%%                previously set.
%%
%% Parameters:    MeasId    -- The measurement identifier.
%%                ThreshId  -- The threshold identifier.
%%======================================================================

remove_threshold(MeasId, ThreshId) ->
    gen_server:call({global, mesh_server},
		    {remove_threshold,MeasId,ThreshId},
		    ?TIME).


%%======================================================================
%% Function:      remove_thresholds
%%
%% Return Value:  {thresholds_removed, MeasId} | {error, Reason}
%%
%% Description:   Removes, for a specified measurement, all thresholds
%%                previously set.
%%
%% Parameters:    MeasId    -- The measurement identifier.
%%======================================================================

remove_thresholds(MeasId) ->
    gen_server:call({global, mesh_server},
		    {remove_thresholds,MeasId},
		    ?TIME).


%%======================================================================
%% Function:      list_thresholds
%%
%% Return Value:  {MeasId,{upper_thresholds,[ThreshInfo]},
%%                        {lower_thresholds,[ThreshInfo]}}  | 
%%                {error, Reason}
%%
%% Description:   Lists, for a specified measurement, all thresholds
%%                previously set.
%%
%% Parameters:    MeasId    -- The measurement identifier.
%%======================================================================

list_thresholds(MeasId) ->
    gen_server:call({global, mesh_server},
		    {list_thresholds,MeasId},
		    ?TIME).


%%======================================================================
%% Function:      enable_threshold
%%
%% Return Value:  {threshold_enabled, {MeasId,ThreshId}} | {error, Reason}
%%
%% Description:   Enables a previously defined threshold.
%%
%% Parameters:    MeasId    -- The measurement identifier.
%%                ThreshId  -- The threshold identifier.
%%======================================================================

enable_threshold(MeasId, ThreshId) ->
    gen_server:call({global, mesh_server},
		    {threshold_status,MeasId,ThreshId,enabled},
		    ?TIME).


%%======================================================================
%% Function:      disable_threshold
%%
%% Return Value:  {threshold_disabled, {MeasId,ThreshId}} | {error, Reason}
%%
%% Description:   Disables a previously defined threshold.
%%
%% Parameters:    MeasId    -- The measurement identifier.
%%                ThreshId  -- The threshold identifier.
%%======================================================================

disable_threshold(MeasId, ThreshId) ->
    gen_server:call({global, mesh_server},
		    {threshold_status,MeasId,ThreshId,disabled},
		    ?TIME).


%%======================================================================
%% Function:      report_tidemarks
%%
%% Return Value:  {MeasId, TypeId, MaxTideMark, MinTideMark}  |  {error,Reason}
%%
%% Description:   Returns the tidemarks associated to the specified measurement.
%%
%% Parameters:    MeasId  -- The identifier used to identify the measure-
%%                           ment object.
%%======================================================================

report_tidemarks(MeasId) ->
    gen_server:call({global, mesh_server},
		    {report_tidemarks,MeasId},
		    ?TIME).


%%======================================================================
%% Function:      reset_tidemarks
%%
%% Return Value:  {tidemarks_reset, MeasId}  |  {error, Reason}
%%
%% Description:   Resets tidemarks associated to the specified measurement.
%%
%% Parameters:    MeasId  -- The identifier used to identify the measure-
%%                           ment object.
%%======================================================================

reset_tidemarks(MeasId) ->
    gen_server:call({global, mesh_server},
		    {reset_tidemarks,MeasId},
		    ?TIME).


%%======================================================================
%% Function:      watchdog_setup
%%
%% Return Value:  ok | {error, Reason}
%%
%% Description:   Sets the parameters that governs the watchdog behaviour.
%%
%% Parameters:    NofTypes  -- The number of types allowed before notifi-
%%                             cations are sent to EVA.
%%                NofMeas   -- The number of measurement objects allowed 
%%                             before notifications are sent to EVA.
%%======================================================================

watchdog_setup(NofTypes, NofMeas) when NofTypes >= 0, NofMeas >= 0 ->
    gen_server:call({global, mesh_server},
		    {watchdog_setup,NofTypes,NofMeas},
		    ?TIME);
watchdog_setup(NofTypes, NofMeas) ->
    {error, {positive_numbers_required, {NofTypes, NofMeas}}}.



%%======================================================================
%% Function:      set_alarm_filter
%%
%% Return Value:  
%%
%% Description:   Sets a new alarm log filter.
%%
%% Parameters:    Func  -- {Mod, Fcn, ArgList}
%%======================================================================

set_alarm_filter(Func) ->
    mesh_log:set_filter(alarm, Func).



%%======================================================================
%% Function:      set_event_filter
%%
%% Return Value:  
%%
%% Description:   Sets a new event log filter.
%%
%% Parameters:    Func   -- {Mod, Fcn, ArgList}
%%======================================================================

set_event_filter(Func) ->
    mesh_log:set_filter(event, Func).



%%======================================================================
%% Function:      set_measurement_filter
%%
%% Return Value:  
%%
%% Description:   Sets a new measurement log filter.
%%
%% Parameters:    Func   -- {Mod, Fcn, ArgList}
%%======================================================================

set_measurement_filter(Func) ->
    mesh_log:set_filter(measurement, Func).



%%======================================================================
%% Function:      reset_alarm_filter
%%
%% Return Value:  
%%
%% Description:   Resets the alarm log filter to the original one.
%%
%% Parameters:    None.
%%======================================================================

reset_alarm_filter() ->
    mesh_log:reset_filter(alarm).



%%======================================================================
%% Function:      reset_event_filter
%%
%% Return Value:  
%%
%% Description:   Resets the event log filter to the original one.
%%
%% Parameters:    None.
%%======================================================================

reset_event_filter() ->
    mesh_log:reset_filter(event).



%%======================================================================
%% Function:      reset_measurement_filter
%%
%% Return Value:  
%%
%% Description:   Resets the measurement log filter to the original one.
%%
%% Parameters:    None.
%%======================================================================

reset_measurement_filter() ->
    mesh_log:reset_filter(measurement).




%%======================================================================
%% Function:      state
%%
%% Return Value:  StateRec
%%
%% Description:   Gets the state record the MESH server internally keeps.
%%
%% Parameters:    None.
%%======================================================================
state() ->
    gen_server:call({global,mesh_server},
		    get_state,
		    1000).


%%======================================================================
%% Function:      alive
%%
%% Return Value:  true  |
%%                false
%%
%% Description:   Checks whether or not the MESH server still is alive.
%%
%% Parameters:    None.
%%======================================================================

alive() ->
    case catch gen_server:call({global,mesh_server},
			       alive,
			       1000) of
	true ->
	    true;
	_Other ->
	    false
    end.




remove_logs() ->
    file:delete(?ALARM_LOG ++ ".LOG.1"),
    file:delete(?ALARM_LOG ++ ".LOG.idx"),
    file:delete(?EVENT_LOG ++ ".LOG.1"),
    file:delete(?EVENT_LOG ++ ".LOG.idx"),
    file:delete(?MEASUREMENT_LOG ++ ".LOG.1"),
    file:delete(?MEASUREMENT_LOG ++ ".LOG.idx").





check_callback_module(Mod) ->
    gen_server:call({global, mesh_server},
		    {check_callback_module,Mod},
		    ?TIME).


set_lock_args(TypeName, LockArgs) ->
    gen_server:call({global, mesh_server},
		    {set_lock_args,TypeName,LockArgs},
		    ?TIME).



register_type(TypeId, Extra, InterfaceMod, NofInst, AdminState, LockArgs) ->
    gen_server:call({global,mesh_server}, 
		    {register_type,TypeId,Extra,InterfaceMod,NofInst,AdminState,LockArgs},
		    ?TIME).

