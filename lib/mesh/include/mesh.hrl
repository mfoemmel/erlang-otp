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

-record(mesh_type, {name,
		    info,
		    callback_mod,
		    admin_state,            % locked | unlocked | shutting_down
		    max_nof_inst,
		    curr_nof_inst = 0,
		    mrp_init,               % ID of the MRP returned by MT:init
		    mrp_init_node,
		    meas_args    = [],      % Currently *ONLY* used by SNMP adaption!!!
		    type_failure = false,
		    fault_id                % MEASTYPE_ALARM fault ID
		   }).
		    
		    


-record(mesh_meas, {id,                               % ID of the MO
		    type,                             % ID of the MT
		    info,                             % User supplied info
		    oper_state = enabled,
		    admin_state,                      % started | stopped
		    last_meas_value   = undefined,
		    time_stamp,
		    meas_info         = [],
		    max_tidemark      = #tidemark{},  
		    min_tidemark      = #tidemark{},
		    upper_thresholds  = [],           % List of (upper) thresholds set
		    lower_thresholds  = [],           % List of (lower) thresholds set
		    res_id,                           % ID of the resource to use
		    callback_mod,                     % Callback module to use
		    init_args,
		    mrp,
		    node
		   }).



-record(threshold, {id,           % Id of threshold
		    status,       % enabled | disabled
		    thresh1,      % Value
		    thresh2,      % Value with hysteresis
		    switch,       % The switch is associated with the thresh1
		                  % value. If set to 'on', the thresh1 value
		                  % shall be compared to the measurement value
		                  % received, if set to 'off' it is the thresh2
		                  % value that shall be used.
		    fault_id,     % Id of threshold alarm sent to EVA
		    thresh_type   % 'upper' or 'lower'. ONLY USED BY SNMP-ADAPTION!!!
		   }).




-record(tidemark, {current     = undefined,    %% Current tidemark
		   previous    = undefined,    %% Tidemark prior to last reset
		   reset_time  = undefined     %% Last reset: {Date, Time}
		  }).




-define(OWN_MEAS_TYPES, [mesh_watchdog]).



-define(NEW_WATCHDOG_REC, 
	#mesh_type{name           = mesh_watchdog,
		   info           = "Settings for watchdog.",
		   max_nof_inst   = {infinity, infinity}, %% {type,meas}
		   curr_nof_inst  = {false, false},       %% {type_alarm,meas_alarm}
		   admin_state    = {undefined,undefined} %% {type_fault_id,meas_fault_id}
		  }).




%%%*********************************************************************
%%% ALARMS USED BY MESH
%%%*********************************************************************

-define(THRESH_ALARM, meshThresholdTriggered).
-define(THRESH_CLASS, qos).
-define(THRESH_SEVERITY, indeterminate).

-define(TYPE_ALARM, meshTooManyTypes).
-define(TYPE_CLASS, processing).
-define(TYPE_SEVERITY, warning).

-define(MEAS_ALARM, meshTooManyMeasurements).
-define(MEAS_CLASS, processing).
-define(MEAS_SEVERITY, warning).

-define(MEASTYPE_ALARM, meshTypeCapacityExceeded).
-define(MEASTYPE_CLASS, processing).
-define(MEASTYPE_SEVERITY, warning).

%%%*********************************************************************
%%% EVENTS USED BY MESH
%%%*********************************************************************

-define(TYPE_FAILURE_EVENT, meshTypeFailure).

-define(MEAS_TERMINATED_EVENT, meshMeasurementTerminated).

-define(NODEUP_EVENT, meshNodeUp).

-define(NODEDOWN_EVENT, meshNodeDown).

-define(TYPE_UNCONNECTED_EVENT, meshTypeUnconnected).

-define(MEAS_UNCONNECTED_EVENT, meshMeasurementUnconnected).

-define(TYPE_CONNECTED_EVENT, meshTypeConnected).

-define(MEAS_CONNECTED_EVENT, meshMeasurementConnected).

-define(MEASUREMENT_REPORT, meshMeasurementReport).


-define(LOG_STATUS, true).



-define(ALARM_LOG, "mesh_alarms").
-define(ALARM_LOG_SIZE, {10000,10}).
-define(ALARM_LOG_WRAP_TIME, 3600).

-define(EVENT_LOG, "mesh_events").
-define(EVENT_LOG_SIZE, {10000,10}).
-define(EVENT_LOG_WRAP_TIME, 3600).

-define(MEASUREMENT_LOG, "mesh_measurements").
-define(MEASUREMENT_LOG_SIZE, {100000,10}).
-define(MEASUREMENT_LOG_WRAP_TIME, 3600).


