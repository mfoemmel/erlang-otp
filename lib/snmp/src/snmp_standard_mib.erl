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
-module(snmp_standard_mib).

%%%-----------------------------------------------------------------
%%% This module implements the configure- and reinit-functions
%%% for the STANDARD-MIB and SNMPv2-MIB.
%%%-----------------------------------------------------------------
-include("snmp_types.hrl").

-define(enabled, 1).
-define(disabled, 2).

%% External exports
-export([configure/1, reconfigure/1, reset/0, sys_up_time/0, sys_up_time/1,
	 snmp_enable_authen_traps/1, snmp_enable_authen_traps/2,
	 sys_object_id/1, sys_object_id/2, sys_or_table/3,
	 variable_func/1, variable_func/2,
	 inc/1, inc/2]).
-export([dummy/1, snmp_set_serial_no/1, snmp_set_serial_no/2]).
-export([add_agent_caps/2, del_agent_caps/1, get_agent_caps/0]).

%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads the config-files for the standard mib, and
%%          inserts the data.  Persistent data that is already
%%          present is *not* changed!  (use reconfigure for that)
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    Standard = snmp_conf:read_standard(Dir),
    lists:map(fun maybe_create_persistent_var/1, Standard),
    snmp_local_db:variable_set({next_sys_or_index, volatile}, 1),
    snmp_generic:table_func(new, {sysORTable, volatile}),
    ok.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads the config-files for the standard mib, and
%%          inserts the data.  Persistent data that is already
%%          present is deleted.  Makes sure the config file
%%          data is used.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    Standard = snmp_conf:read_standard(Dir),
    lists:map(fun create_persistent_var/1, Standard),
    snmp_local_db:variable_set({next_sys_or_index, volatile}, 1),
    snmp_generic:table_func(new, {sysORTable, volatile}),
    ok.

%%-----------------------------------------------------------------
%% Func: reset/0
%% Purpose: Resets all counters (sets them to 0).
%%-----------------------------------------------------------------
reset() ->
    snmp_mpd:reset().

maybe_create_persistent_var({Var, Val}) ->
    case snmp_generic:variable_get({Var, persistent}) of
	{value, _} -> ok;
	_ -> snmp_generic:variable_set({Var, persistent}, Val)
    end.

create_persistent_var({Var, Val}) ->
    snmp_generic:variable_set({Var, persistent}, Val).

variable_func(Op) -> ok.

variable_func(get, Name) ->
    [{_, Val}] = ets:lookup(snmp_agent_table, Name),
    {value, Val}.
    

%%-----------------------------------------------------------------
%%  inc(VariableName) increments the variable (Counter) in
%%  the local mib. (e.g. snmpInPkts)
%%-----------------------------------------------------------------
inc(Name) -> inc(Name, 1).
inc(Name, N) -> ets:update_counter(snmp_agent_table, Name, N).

%%-----------------------------------------------------------------
%% This is the instrumentation function for sysUpTime.
%%-----------------------------------------------------------------
sys_up_time() ->
    snmp:sys_up_time().

sys_up_time(get) ->
    {value, snmp:sys_up_time()}.

%%-----------------------------------------------------------------
%% This is the instrumentation function for snmpEnableAuthenTraps
%%-----------------------------------------------------------------
snmp_enable_authen_traps(new) ->
    snmp_generic:variable_func(new, {snmpEnableAuthenTraps, persistent});

snmp_enable_authen_traps(delete) ->
    ok;

snmp_enable_authen_traps(get) ->
    snmp_generic:variable_func(get, {snmpEnableAuthenTraps, persistent}).

snmp_enable_authen_traps(set, NewVal) ->
    snmp_generic:variable_func(set, NewVal, {snmpEnableAuthenTraps,persistent}).

%%-----------------------------------------------------------------
%% This is the instrumentation function for sysObjectId
%%-----------------------------------------------------------------
sys_object_id(new) ->
    snmp_generic:variable_func(new, {sysObjectID, persistent});

sys_object_id(delete) ->
    ok;

sys_object_id(get) ->
    snmp_generic:variable_func(get, {sysObjectID, persistent}).

sys_object_id(set, NewVal) ->
    snmp_generic:variable_func(set, NewVal, {sysObjectID, persistent}).

%%-----------------------------------------------------------------
%% This is a dummy instrumentation function for objects like
%% snmpTrapOID, that is accessible-for-notify, with different
%% values each time.  This function will only be called with
%% new/delete.
%%-----------------------------------------------------------------
dummy(Op) -> ok.

%%-----------------------------------------------------------------
%% This is the instrumentation function for snmpSetSerialNo
%%-----------------------------------------------------------------
snmp_set_serial_no(new) ->
    snmp_generic:variable_func(new, {snmpSetSerialNo, volatile}),
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    Val = random:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {snmpSetSerialNo, volatile});

snmp_set_serial_no(delete) ->
    ok;

snmp_set_serial_no(get) ->
    snmp_generic:variable_func(get, {snmpSetSerialNo, volatile}).

snmp_set_serial_no(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {snmpSetSerialNo, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
snmp_set_serial_no(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {snmpSetSerialNo, volatile}).

%%-----------------------------------------------------------------
%% This is the instrumentation function for sysOrTable
%%-----------------------------------------------------------------
sys_or_table(Op, RowIndex, Cols) ->
    snmp_generic:table_func(Op, RowIndex, Cols, {sysORTable, volatile}).

add_agent_caps(Oid, Descr) when list(Oid), list(Descr) ->
    {value, Next} = snmp_local_db:variable_get({next_sys_or_index, volatile}),
    snmp_local_db:variable_set({next_sys_or_index, volatile}, Next+1),
    SysUpTime = sys_up_time(),
    Row = {Next, Oid, Descr, SysUpTime},
    snmp_local_db:table_create_row({sysORTable, volatile}, [Next], Row),
    snmp_local_db:variable_set({sysORLastChange, volatile}, SysUpTime),
    Next.

del_agent_caps(Index) ->
    snmp_local_db:table_delete_row({sysORTable, volatile}, [Index]),
    snmp_local_db:variable_set({sysORLastChange, volatile}, sys_up_time()).

get_agent_caps() ->
    snmp_local_db:match({sysORTable, volatile}, {'$1', '$2', '$3', '$4'}).
