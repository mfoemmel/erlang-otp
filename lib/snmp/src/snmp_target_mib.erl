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
-module(snmp_target_mib).

-export([configure/1, reconfigure/1,
	 snmpTargetSpinLock/1, snmpTargetSpinLock/2,
	 snmpTargetAddrTable/1, snmpTargetAddrTable/3,
	 snmpTargetParamsTable/1, snmpTargetParamsTable/3,
	 table_next/2, get_target_addrs/0, 
	 get_target_engine_id/1, set_target_engine_id/2,
	 is_valid_tag/3, get/3, table_next/2]).
-export([add_addr/10,  delete_addr/1,
	 add_params/5, delete_params/1]).


-include("SNMP-TARGET-MIB.hrl").
-include("SNMPv2-TC.hrl").
-include("SNMPv2-TM.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"TARGET-MIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%% Column not accessible via SNMP - needed when the agent sends informs
-define(snmpTargetAddrEngineId, 10).
%% Extra comlumns for the augmented table snmpTargetAddrExtTable
-define(snmpTargetAddrTMask, 11).
-define(snmpTargetAddrMMS, 12).


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the target tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case snmp_local_db:table_exists(db(snmpTargetParamsTable)) of
	true ->
	    ?vdebug("tables already exist: init vars & cleanup",[]),
	    init_vars(),
	    gc_tabs();
	false ->
	    ?vdebug("no tables: reconfigure",[]),
	    reconfigure(Dir)
    end.


%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the target tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    set_sname(),
    ?vdebug("read target configuration files",[]),
    {Addrs, Params} = snmp_conf:read_target_config_files(Dir),
    ?vdebug("initiate tables",[]),
    init_tabs(Addrs, Params),
    ?vdebug("initiate vars",[]),
    init_vars(),
    ?vdebug("invalidate cache for notification mib",[]),
    snmp_notification_mib:invalidate_cache(),
    ok.

%% maybe_create_table(Name) ->
%%     case snmp_local_db:table_exists(db(Name)) of
%% 	true -> ok;
%% 	_ -> snmp_local_db:table_create(db(Name))
%%     end.

init_tabs(Addrs, Params) ->
    ?vdebug("create target address table",[]),
    snmp_local_db:table_delete(db(snmpTargetAddrTable)),
    snmp_local_db:table_create(db(snmpTargetAddrTable)),
    init_addr_table(Addrs),
    ?vdebug("create target params table",[]),
    snmp_local_db:table_delete(db(snmpTargetParamsTable)),
    snmp_local_db:table_create(db(snmpTargetParamsTable)),
    init_params_table(Params).
    
init_addr_table([Row | T]) ->
    Key = element(1, Row),
    table_create_row(snmpTargetAddrTable, Key, Row),
    init_addr_table(T);
init_addr_table([]) -> true.

init_params_table([Row | T]) ->
    Key = element(1, Row),
    table_create_row(snmpTargetParamsTable, Key, Row),
    init_params_table(T);
init_params_table([]) -> true.

table_create_row(Tab, Key, Row) ->
    ?vtrace("create table ~w row with Key: ~w",[Tab, Key]),
    snmp_local_db:table_create_row(db(Tab), Key, Row).


add_addr(Name, Ip, Port, Timeout, Retry, TagList, 
	 Params, EngineId, TMask, MMS) ->
    Addr = {Name, Ip, Port, Timeout, Retry, TagList, 
	    Params, EngineId, TMask, MMS},
    case (catch snmp_conf:check_target_addr(Addr)) of
	{ok, Row} ->
	    Key = element(1, Row),
	    case table_create_row(snmpTargetAddrTable, Key, Row) of
		true ->
		    {ok, Key};
		false ->
		    {error, create_failed}
	    end;
	Error ->
	    {error, Error}
    end.

delete_addr(Key) ->
    Db = db(snmpTargetAddrTable),
    case snmp_local_db:table_delete_row(Db, Key) of
	true ->
	    ok;
	false ->
	    {error, not_found}
    end.


add_params(Name, MPModel, SecModel, SecName, SecLevel) ->
    Params = {Name, MPModel, SecModel, SecName, SecLevel},
    case (catch snmp_conf:check_target_params(Params)) of
	{ok, Row} ->
	    Key = element(1, Row),
	    case table_create_row(snmpTargetParamsTable, Key, Row) of
		true ->
		    {ok, Key};
		false ->
		    {create_failed}
	    end;
	Error ->
	    {error, Error}
    end.

delete_params(Key) ->
    Db = db(snmpTargetParamsTable),
    case snmp_local_db:table_delete_row(Db, Key) of
	true ->
	    ok;
	false ->
	    {error, not_found}
    end.

    
gc_tabs() ->
    gc_tab(snmpTargetAddrTable),
    gc_tab(snmpTargetParamsTable),
    ok.

gc_tab(Tab) ->
    STC = stc(Tab),
    F = fun(Oid, Row) ->
		case element(STC, Row) of
		    ?'StorageType_volatile' ->
			snmp_local_db:table_delete_row(db(Tab), Oid);
		    _ ->
			ok
		end
	end,
    gc_tab1(F,Tab).
		    

gc_tab1(F,Tab) ->
    case (catch snmp_generic:table_foreach(db(Tab), F)) of
	{'EXIT',{cyclic_db_reference,Oid}} ->
	    %% Remove the row regardless of storage type since this
	    %% is a major error. This row must be removed.
	    case snmp_local_db:table_delete_row(db(Tab), Oid) of
		true -> 
		    ?vlog("deleted cyclic ref row for: ~w",[Oid]),
		    config_err("cyclic reference in table ~w: "
			       "~w -> ~w. Row deleted", [Tab,Oid,Oid]),
		    gc_tab1(F,Tab);
		false ->
		    ?vlog("unable to remove faulty row from table ~w",[Tab]),
		    config_err("failed removing faulty row. "
			       "Giving up on table ~w cleanup", [Tab])
	    end;
	_ ->
	    ok
    end.


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_vars() -> 
    ?vtrace("initiate vars",[]),
    lists:map(fun maybe_create_var/1, vars()).

maybe_create_var(Var) ->
    case ets:lookup(snmp_agent_table, Var) of
	[_] -> ok;
	_ -> init_var(Var)
    end.

init_var(Var) -> ets:insert(snmp_agent_table, {Var, 0}).

vars() ->
    [snmpUnavailableContexts,
     snmpUnknownContexts].

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
is_valid_tag("", _TDomain, _TAddress) ->
    true;
is_valid_tag(Tag, TDomain, TAddress) ->
    is_valid_tag(TDomain, TAddress, Tag, []).

is_valid_tag(TDomain, TAddress, Tag, Key) ->
    case table_next(snmpTargetAddrTable, Key) of
	endOfTable -> 
	    false;
	NextKey -> 
	    case get(snmpTargetAddrTable, NextKey, [?snmpTargetAddrTDomain,
						    ?snmpTargetAddrTAddress,
						    ?snmpTargetAddrTagList,
						    ?snmpTargetAddrTMask]) of
		[{value, TDomain},  % must match exactly
		 {value, TAddress}, % RFC2576: chapters 5.2.1 & 5.3
		 {value, TagList}, 
		 {value, []}] ->
		    case snmp_misc:is_tag_member(Tag, TagList) of
			true ->
			    ?vtrace("is_valid_tag -> exact: "
				    "tag IS member of taglist", []),
			    true;
			false ->
			    ?vtrace("is_valid_tag -> exact: "
				    "tag is NOT member of taglist", []),
			    is_valid_tag(TDomain, TAddress,
					 Tag, NextKey)
		    end;
		[{value, TDomain},   % must match exactly
		 {value, TAddress2},
		 {value, TagList}, 
		 {value, TMask}] when TMask =/= [] ->
		    case snmp_misc:is_tmask_match(TAddress, TAddress2, 
						  TMask) of
			true ->
			    case snmp_misc:is_tag_member(Tag, TagList) of
				true ->
				    ?vtrace("is_valid_tag -> masked: "
					    "tag IS member of taglist", []),
				    true;
				false ->
				    ?vtrace("is_valid_tag -> masked: "
					    "tag is NOT member of taglist",[]),
				    is_valid_tag(TDomain, TAddress,
						 Tag, NextKey)
			    end;
			false ->
			    is_valid_tag(TDomain, TAddress,
					 Tag, NextKey)
		    end;
		_ ->
		    is_valid_tag(TDomain, TAddress, Tag, NextKey)
	    end
    end.
    

%% TargAddrs =
%%     [{TagList, TargetAddr, TargetAddrName, TargetParams, Timeout, Retry}]
%%   TargetAddr = {TDomain, TAddr}; e.g. {?snmpUDPDomain, IpAndUdpPortAsList}
get_target_addrs() ->
    get_target_addrs([], []).

get_target_addrs(Key, Res) ->
    case table_next(snmpTargetAddrTable, Key) of
	endOfTable -> 
	    Res;
	NextKey -> 
	    case snmp_local_db:table_get_row(db(snmpTargetAddrTable),
					     NextKey) of
		{_Key, TDomain, TAddress, Timeout, RetryCount, TagList, Params,
		 _Storage, ?'RowStatus_active', _TargetEngineId,_TMask,_MMS} ->
		    TargetParams = get_target_params(Params),
		    Targ = {TagList, {TDomain, TAddress}, NextKey,
			    TargetParams, Timeout, RetryCount},
		    get_target_addrs(NextKey, [Targ | Res]);
		_ ->
		    get_target_addrs(NextKey, Res)
	    end
    end.

get_target_params(Params) ->
    case snmpTargetParamsTable(get, Params, [?snmpTargetParamsMPModel,
					     ?snmpTargetParamsSecurityModel,
					     ?snmpTargetParamsSecurityName,
					     ?snmpTargetParamsSecurityLevel,
					     ?snmpTargetParamsRowStatus]) of
	[{value, MpModel}, 
	 {value, SecModel}, {value, SecName}, {value, SecLevel},
	 {value, ?'RowStatus_active'}] ->
	    {MpModel, SecModel, SecName, SecLevel};
	_ ->
	    undefined
    end.

get_target_engine_id(TargetAddrName) ->
    case snmp_local_db:table_get_element(db(snmpTargetAddrTable),
					 TargetAddrName,
					 ?snmpTargetAddrEngineId) of
	{value, Val} -> {ok, Val};
	_ ->
	    undefined
    end.
				    
set_target_engine_id(TargetAddrName, EngineId) ->
    snmp_local_db:table_set_elements(db(snmpTargetAddrTable),
				     TargetAddrName,
				     [{?snmpTargetAddrEngineId, EngineId}]).

%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------
snmpTargetSpinLock(new) ->
    snmp_generic:variable_func(new, {snmpTargetSpinLock, volatile}),
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    Val = random:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {snmpTargetSpinLock, volatile});

snmpTargetSpinLock(delete) ->
    ok;

snmpTargetSpinLock(get) ->
    snmp_generic:variable_func(get, {snmpTargetSpinLock, volatile}).

snmpTargetSpinLock(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {snmpTargetSpinLock, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
snmpTargetSpinLock(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {snmpTargetSpinLock, volatile}).


%% Op == new | delete
snmpTargetAddrTable(Op) ->
    snmp_generic:table_func(Op, db(snmpTargetAddrTable)).

%% Op == get | is_set_ok | set | get_next
snmpTargetAddrTable(get, RowIndex, Cols) ->
    get(snmpTargetAddrTable, RowIndex, Cols);
snmpTargetAddrTable(get_next, RowIndex, Cols) ->
    next(snmpTargetAddrTable, RowIndex, Cols);
snmpTargetAddrTable(set, RowIndex, Cols0) ->
    case (catch verify_targetAddrTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    snmp_notification_mib:invalidate_cache(),
	    %% Add columns for augmenting table snmpTargetAddrExtTable and for
	    %% target engine ID.  Target engine ID is set to "".  The function
	    %% get_target_engine_id will return "" unless a value is set using
	    %% set_target_engine_id.  If it is "" Informs can't be sent to the
	    %% target.
	    NCols = Cols ++ [{?snmpTargetAddrEngineId, ""},
			     {?snmpTargetAddrTMask, []},
			     {?snmpTargetAddrMMS, 2048}],
	    snmp_generic:table_func(set, RowIndex, NCols, db(snmpTargetAddrTable));
	Error ->
	    Error
    end;
snmpTargetAddrTable(is_set_ok, RowIndex, Cols0) ->
    case (catch verify_targetAddrTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    %% Add columns for augmenting table snmpTargetAddrExtTable and for
	    %% target engine ID.  Target engine ID is set to "".  The function
	    %% get_target_engine_id will return "" unless a value is set using
	    %% set_target_engine_id.  If it is "" Informs can't be sent to the
	    %% target.
	    NCols = Cols ++ [{?snmpTargetAddrEngineId, ""},
			     {?snmpTargetAddrTMask, []},
			     {?snmpTargetAddrMMS, 2048}],
	    snmp_generic:table_func(is_set_ok, RowIndex, NCols, db(snmpTargetAddrTable));
	Error ->
	    Error
    end;
snmpTargetAddrTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpTargetAddrTable)).

verify_targetAddrTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_targetAddrTable_cols([{Col, Val0}|Cols], Acc) ->
    Val = verify_targetAddrTable_col(Col, Val0),
    verify_targetAddrTable_cols(Cols, [{Col, Val}|Acc]).

verify_targetAddrTable_col(?snmpTargetAddrName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	true ->
	    Name;
	_ ->
	    wrongValue(?snmpTargetAddrName)
    end;
verify_targetAddrTable_col(?snmpTargetAddrTAddress, TAddr) ->
    case (catch snmp_conf:check_ip_udp(TAddr)) of
	true ->
	    TAddr;
	_ ->
	    wrongValue(?snmpTargetAddrTAddress)
    end;
verify_targetAddrTable_col(?snmpTargetAddrTimeout, Timeout) ->
    case (catch snmp_conf:check_integer(Timeout)) of
	true when Timeout >= 0 ->
	    Timeout;
	_ ->
	    wrongValue(?snmpTargetAddrTimeout)
    end;
verify_targetAddrTable_col(?snmpTargetAddrRetryCount, Retry) ->
    case (catch snmp_conf:check_integer(Retry)) of
	true when Retry >= 0 ->
	    Retry;
	_ ->
	    wrongValue(?snmpTargetAddrRetryCount)
    end;
verify_targetAddrTable_col(?snmpTargetAddrTagList, TagList) ->
    case (catch snmp_conf:check_string(TagList)) of
	true ->
	    TagList;
	_ ->
	    wrongValue(?snmpTargetAddrTagList)
    end;
verify_targetAddrTable_col(?snmpTargetAddrParams, Params) ->
    case (catch snmp_conf:check_string(Params)) of
	true ->
	    Params;	
	_ ->
	    wrongValue(?snmpTargetAddrParams)
    end;
verify_targetAddrTable_col(_, Val) ->
    Val.
    

%% Op == new | delete
snmpTargetParamsTable(Op) ->
    snmp_generic:table_func(Op, db(snmpTargetParamsTable)).

%% Op == get | is_set_ok | set | get_next
snmpTargetParamsTable(get, RowIndex, Cols) ->
    get(snmpTargetParamsTable, RowIndex, Cols);
snmpTargetParamsTable(get_next, RowIndex, Cols) ->
    next(snmpTargetParamsTable, RowIndex, Cols);
snmpTargetParamsTable(set, RowIndex, Cols0) ->
    case (catch verify_snmpTargetParamsTable_cols(Cols0, [])) of
	{ok, Cols} ->
	    snmp_notification_mib:invalidate_cache(),
	    snmp_generic:table_func(set, RowIndex, Cols, 
				    db(snmpTargetParamsTable));
	Error ->
	    Error
    end;
snmpTargetParamsTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpTargetParamsTable)).

verify_snmpTargetParamsTable_cols([], Cols) ->
    {ok, lists:reverse(Cols)};
verify_snmpTargetParamsTable_cols([{Col, Val0}|Cols], Acc) ->
    Val = verify_snmpTargetParamsTable_col(Col, Val0),
    verify_snmpTargetParamsTable_cols(Cols, [{Col, Val}|Acc]).

verify_snmpTargetParamsTable_col(?snmpTargetParamsName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	true ->
	    Name;
	_ ->
	    wrongValue(?snmpTargetParamsName)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsMPModel, Model) ->
    case Model of
	v1      -> ?MP_V1;
	v2c     -> ?MP_V2C;
	v3      -> ?MP_V3;
	?MP_V1  -> ?MP_V1;
	?MP_V2C -> ?MP_V2C;
	?MP_V3  -> ?MP_V3;
	_       -> wrongValue(?snmpTargetParamsMPModel)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsSecurityModel, Model) ->
    case Model of
	v1       -> ?SEC_V1;
	v2c      -> ?SEC_V2C;
	usm      -> ?SEC_USM;
	?SEC_V1  -> ?SEC_V1;
	?SEC_V2C -> ?SEC_V2C;
	?SEC_USM -> ?SEC_USM;
	_        -> wrongValue(?snmpTargetParamsSecurityModel)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsSecurityName, Name) ->
    case (catch snmp_conf:check_string(Name)) of
	true ->
	    Name;
	_ ->
	    wrongValue(?snmpTargetParamsSecurityName)
    end;
verify_snmpTargetParamsTable_col(?snmpTargetParamsSecurityLevel, Level) ->
    case Level of
	noAuthNoPriv -> 1;
	authNoPriv   -> 2;
	authPriv     -> 3;
	1            -> 1;
	2            -> 2;
	3            -> 3;
	_            -> wrongValue(?snmpTargetParamsSecurityLevel)
    end;
verify_snmpTargetParamsTable_col(_, Val) ->
    Val.

db(X) -> {X, persistent}.

fa(snmpTargetAddrTable) -> ?snmpTargetAddrTDomain;
fa(snmpTargetParamsTable) -> ?snmpTargetParamsMPModel.
 
foi(snmpTargetAddrTable) -> ?snmpTargetAddrName;
foi(snmpTargetParamsTable) -> ?snmpTargetParamsName.
 
noc(snmpTargetAddrTable) -> 9;
noc(snmpTargetParamsTable) -> 7.

stc(snmpTargetAddrTable) -> ?snmpTargetAddrStorageType;
stc(snmpTargetParamsTable) -> ?snmpTargetParamsStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).

 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).


wrongValue(V) -> throw({wrongValue, V}).

set_sname() ->
    set_sname(get(sname)).

set_sname(undefined) ->
    put(sname,conf);
set_sname(_) -> %% Keep it, if already set.
    ok.
    

config_err(F, A) ->
    snmp_error_report:config_err(F, A).
