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
%%%   Description:      Creates log used by the MESH handler, i.e., by
%%%                     EVA, and also defines filter functions.
%%%
%%%*********************************************************************

-module(mesh_log).


-include_lib("eva/include/eva.hrl").
-include("mesh.hrl").


-export([open_log/1,
	 close_log/1,
	 alarm_filter/1,
	 event_filter/1,
	 measurement_filter/1,
	 set_filter/2,
	 reset_filter/1
	]).




open_log(alarm) ->
    open_log(?ALARM_LOG, ?ALARM_LOG_SIZE, alarm_filter, ?ALARM_LOG_WRAP_TIME);
open_log(event) ->
    open_log(?EVENT_LOG, ?EVENT_LOG_SIZE, event_filter, ?EVENT_LOG_WRAP_TIME);
open_log(measurement) ->
    open_log(?MEASUREMENT_LOG, ?MEASUREMENT_LOG_SIZE, measurement_filter, 
	     ?MEASUREMENT_LOG_WRAP_TIME).



open_log(Name, Size, Filter, WrapTime) ->
    case catch disk_log:open([{name,Name}, 
			      {format,internal}, 
			      {type,wrap}, 
			      {size,Size}]) of
	{ok, RetName} ->
	    eva_log:open(Name, {?MODULE, Filter, []}, WrapTime);
	{repaired, RetName, Recovered, NotRecovered} ->
	    eva_log:open(Name, {?MODULE, Filter, []}, WrapTime);
	Other ->
	    Other
    end.




close_log(alarm) ->
    eva_log:close(?ALARM_LOG);
close_log(event) ->
    eva_log:close(?EVENT_LOG);
close_log(measurement) ->
    eva_log:close(?MEASUREMENT_LOG).




alarm_filter(Item) when record(Item, alarm) ->
    case catch lists:prefix("mesh", atom_to_list(Item#alarm.name)) of
	true ->
	    true;
	_Other ->
	    false
    end;
alarm_filter(_Item) ->
    false.
    




event_filter(Item) when record(Item, event) ->
    EventName = Item#event.name,
    case catch lists:prefix("mesh", atom_to_list(EventName)) of
	true ->
	    case EventName of
		?MEASUREMENT_REPORT ->
		    false;
		_OtherEvent ->
		    true
	    end;
	_Other ->
	    false
    end;
event_filter(_Item) ->
    false.



measurement_filter(Item) when record(Item, event) ->
    EventName = Item#event.name,
    case catch lists:prefix("mesh", atom_to_list(EventName)) of
	true ->
	    case EventName of
		?MEASUREMENT_REPORT ->
		    true;
		_OtherEvent ->
		    false
	    end;
	_Other ->
	    false
    end;
measurement_filter(_Item) ->
    false.




set_filter(alarm, Func) ->
    eva_log:set_filter(?ALARM_LOG, Func);
set_filter(event, Func) ->
    eva_log:set_filter(?EVENT_LOG, Func);
set_filter(measurement, Func) ->
    eva_log:set_filter(?MEASUREMENT_LOG, Func).



reset_filter(alarm) ->
    eva_log:set_filter(?ALARM_LOG, {?MODULE,alarm_filter,[]});
reset_filter(event) ->
    eva_log:set_filter(?EVENT_LOG, {?MODULE,event_filter,[]});
reset_filter(measurement) ->
    eva_log:set_filter(?MEASUREMENT_LOG, {?MODULE,measurement_filter,[]}).

