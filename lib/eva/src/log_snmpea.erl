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
-module(log_snmpea).


-include("log_snmp.hrl").
-include("OTP-LOG-MIB.hrl").
-include("OTP-SNMPEA-LOG-MIB.hrl").
-define(log_name, "snmp log").

%% External exports
-export([init/0, stop/0,
	 snmpeaLogDiscriminator/1, snmpeaLogDiscriminator/2,
	 snmpeaLogTransferTable/3,
	 snmpea_log_type/3, snmpea_log_search/3]).

%%%-----------------------------------------------------------------
%%% This module implements SNMP Instrumentation functions for the
%%% OTP-SNMEPA-LOG-MIB.  This MIB contains objects for controlling
%%% the audit trail log in the snmp agent.
%%%-----------------------------------------------------------------

init() ->
    log_snmp:register_type(snmp_log, ?snmpeaLogType,
			   {?MODULE, snmpea_log_type, []}),
    log:open(?log_name, snmp_log, 24*3600),
    MibDir = code:priv_dir(eva) ++ "/mibs",
    snmp:load_mibs(snmp_master_agent, [MibDir ++ "/OTP-SNMPEA-LOG-MIB"]).

stop() ->
    snmp:unload_mibs(snmp_master_agent, ["OTP-SNMPEA-LOG-MIB"]).
    
%%%-----------------------------------------------------------------
%%% Instrumentation functions
%%%-----------------------------------------------------------------
snmpeaLogDiscriminator(get) ->
    case snmp_net_if:get_log_type(get(net_if)) of
	w_log -> {value, ?snmpeaLogDiscriminator_write};
	rw_log -> {value, ?snmpeaLogDiscriminator_readWrite};
	false -> {value, ?snmpeaLogDiscriminator_none};
	_ -> genErr
    end;
snmpeaLogDiscriminator(Op) ->
    ok.
snmpeaLogDiscriminator(is_set_ok, Val) ->
    noError;
snmpeaLogDiscriminator(set, Val) ->
    snmp_net_if:set_log_type(get(net_if), val(Val)),
    noError.

val(?snmpeaLogDiscriminator_none) -> false;
val(?snmpeaLogDiscriminator_readWrite) -> rw_log;
val(?snmpeaLogDiscriminator_write) -> w_log.

snmpeaLogTransferTable(get, [LogIndex, LogTrIndex], 
		       [?snmpeaLogTransferIpAddress]) ->
    case mnesia:dirty_read({logTransferTable, {LogIndex, LogTrIndex}}) of
	[#logTransferTable{type_specific = Ip}] ->
	    [{value, Ip}];
	[] ->
	    [{noValue, noSuchInstance}]
    end;
snmpeaLogTransferTable(get, _, [?snmpeaLogTransferIpAddress]) ->
    [{noValue, noSuchInstance}];
snmpeaLogTransferTable(get_next, RowIndex, [?snmpeaLogTransferIpAddress]) ->
    case mnesia:snmp_get_next_index(logTransferTable, RowIndex) of
	endOfTable ->
	    [endOfTable];
	Index ->
	    case mnesia:snmp_get_row(logTransferTable, Index) of
		{ok, #logTransferTable{type_specific = Ip}} ->
		    [{value, Ip}];
		undefined ->
		    snmpeaLogTransferTable(get_next, RowIndex,
					   [?snmpeaLogTransferIpAddress])
	    end
    end;
snmpeaLogTransferTable(is_set_ok, [LogIndex, LogTrIndex],
		      [{?snmpeaLogTransferIpAddress, Ip}]) ->
    case mnesia:dirty_read({logTransferTable, {LogIndex, LogTrIndex}}) of
	[_Found] -> {noError, 0};
	_ -> {inconsistentName, ?snmpeaLogTransferIpAddress}
    end;
snmpeaLogTransferTable(is_set_ok, _, [?snmpeaLogTransferIpAddress]) ->
    {noCreation, ?snmpeaLogTransferIpAddress};
snmpeaLogTransferTable(set, [LogIndex, LogTrIndex],
		       [{?snmpeaLogTransferIpAddress, Ip}]) ->
    case mnesia:dirty_read({logTransferTable, {LogIndex, LogTrIndex}}) of
	[Tr] ->
	    mnesia:dirty_write(Tr#logTransferTable{type_specific = Ip}),
	    {noError, 0};
	_ -> 
	    {commitFailed, ?snmpeaLogTransferIpAddress}
    end.
    
%%-----------------------------------------------------------------
%% Functions called by LOG when logTable is modified
%%-----------------------------------------------------------------
%% These will never get called, as we don't allow any creation,
%% and it's impossible to delete system-created logs.
% snmpea_log_type(create, Log) ->
% snmpea_log_type(delete, Log) ->


snmpea_log_type(validate_creation, LogIndex, Cols) ->
    false;

%% HERE handle start&stop time
snmpea_log_type(search, LogIndex, LogTrIndex) ->
    [#logTransferTable{type_specific = Ip}] =
	mnesia:dirty_read({logTransferTable, {LogIndex, LogTrIndex}}),
    Info = snmp:info(snmp_master_agent),
    {value, {_, Mibs}} = lists:keysearch(loaded_mibs, 1, Info),
    MibNames = lists:map(fun({_,_,File}) -> File end, Mibs),
    Mib = snmp_misc:make_mini_mib(MibNames),
    {?MODULE, snmpea_log_search, [Mib, Ip]}.

snmpea_log_search(Cont, Mib, Ip) ->
    case disk_log:chunk(?log_name, Cont) of
	eof ->
	    eof;
	{error, R} ->
	    {error, R};
	{NCont, ListOfTerms} ->
	    List = lists:zf(fun(Term) ->
				    format(Term, Ip, Mib)
			     end, ListOfTerms),
	    Bin = list_to_binary(List),
	    {NCont, Bin}
    end.

format(Term, undefined, Mib) ->
    {true, snmp_log:format(Term, Mib)};
format(Term, [A,B,C,D], Mib) ->
    case Term of
	{Packet, {{A,B,C,D}, Port}} ->
	    {true, snmp_log:format(Term, Mib)};
	_ ->
	    false
    end.

	 
