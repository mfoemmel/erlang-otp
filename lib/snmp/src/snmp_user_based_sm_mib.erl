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
-module(snmp_user_based_sm_mib).

-export([configure/1, reconfigure/1,
	 usmUserSpinLock/1, usmUserSpinLock/2,
	 usmUserTable/1, usmUserTable/3,
	 table_next/2,
	 is_engine_id_known/1, get_user/2, get_user_from_security_name/2,
	 mk_key_change/3, mk_key_change/5, extract_new_key/3, mk_random/1]).

-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMPv2-TC.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"USM_MIB").
-include("snmp_verbosity.hrl").

%% Columns not accessible via SNMP
-define(usmUserAuthKey, 14).
-define(usmUserPrivKey, 15).

%%%-----------------------------------------------------------------
%%% Implements the instrumentation functions and additional 
%%% functions for the SNMP-USER-BASED-SM-MIB.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the USM tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    case snmp_local_db:table_exists(db(usmUserTable)) of
	true ->
	    init_vars(),
	    gc_tabs();
	false ->
	    reconfigure(Dir)
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the USM tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error) |
%%        exit({unsupported_crypto, Function})
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    Users = snmp_conf:read_usm_config_files(Dir),
    check_users(Users),
    init_tabs(Users),
    init_vars(),
    ok.

%%-----------------------------------------------------------------
%% This function loops through all users, and check that the
%% definition is constistent with the support for crypto on
%% the system.  Thus, it is not possible to define a user that
%% uses authentication if 'crypto' is not started, or a user that
%% uses DES if 'crypto' doesn't support DES.
%%-----------------------------------------------------------------
check_users([User | Users]) ->
    case element(?usmUserAuthProtocol, User) of
	?usmNoAuthProtocol -> ok;
	?usmHMACMD5AuthProtocol ->
	    case is_crypto_supported(md5_mac_96) of
		true -> ok;
		false -> exit({unsupported_crypto, md5_mac_96})
	    end;
	?usmHMACSHAAuthProtocol ->
	    case is_crypto_supported(sha_mac_96) of
		true -> ok;
		false -> exit({unsupported_crypto, sha_mac_96})
	    end
    end,
    case element(?usmUserPrivProtocol, User) of
	?usmNoPrivProtocol -> ok;
	?usmDESPrivProtocol ->
	    case is_crypto_supported(des_cbc_decrypt) of
		true -> ok;
		false -> exit({unsupported_crypto, des_cbc_decrypt})
	    end
    end,
    check_users(Users);
check_users([]) ->
    ok.


maybe_create_table(Name) ->
    case snmp_local_db:table_exists(db(Name)) of
	true -> ok;
	_ -> snmp_local_db:table_create(db(Name))
    end.

init_tabs(Users) ->
    snmp_local_db:table_delete(db(usmUserTable)),
    snmp_local_db:table_create(db(usmUserTable)),
    init_user_table(Users).
    
init_user_table([Row | T]) ->
    Key1 = element(1, Row),
    Key2 = element(2, Row),
    Key = [length(Key1) | Key1] ++ [length(Key2) | Key2],
    snmp_local_db:table_create_row(db(usmUserTable), Key, Row),
    init_user_table(T);
init_user_table([]) -> true.

gc_tabs() ->
    gc_tab(usmUserTable),
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
    snmp_generic:table_foreach(db(Tab), F).

%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_vars() -> lists:map(fun maybe_create_var/1, vars()).

maybe_create_var(Var) ->
    case ets:lookup(snmp_agent_table, Var) of
	[_] -> ok;
	_ -> init_var(Var)
    end.

init_var(Var) -> ets:insert(snmp_agent_table, {Var, 0}).

vars() ->
    [usmStatsUnsupportedSecLevels,
     usmStatsNotInTimeWindows,
     usmStatsUnknownUserNames,
     usmStatsUnknownEngineIDs,
     usmStatsWrongDigests,
     usmStatsDecryptionErrors].

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
is_engine_id_known(EngineID) ->
    EngineKey = [length(EngineID) | EngineID],
    case table_next(usmUserTable, EngineKey) of
	endOfTable -> false;
	Key -> lists:prefix(EngineKey, Key)
    end.

get_user(EngineID, UserName) ->
    Key = [length(EngineID) | EngineID] ++ [length(UserName) | UserName],
    snmp_generic:table_get_row(db(usmUserTable), Key).

get_user_from_security_name(EngineID, SecName) ->
    %% Since the normal mapping between UserName and SecName is the
    %% identityfunction, we first try to use the SecName as UserName,
    %% and check the resulting row.  If it doesn't match, we'll have to
    %% loop through the entire table.
    Key = [length(EngineID) | EngineID] ++ [length(SecName) | SecName],
    case snmp_generic:table_get_row(db(usmUserTable), Key) of
	Row when tuple(Row) ->
	    Row;
	undefined ->
	    F = fun(_, Row) when element(?usmUserEngineID,Row) == EngineID,
				 element(?usmUserSecurityName,Row) == SecName ->
			throw({ok, Row});
		   (_, _) ->
			ok
		end,
	    case catch snmp_generic:table_foreach(db(usmUserTable), F) of
		{ok, Row} ->
		    Row;
		Else ->
		    undefined
	    end
    end.
		

%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------
usmUserSpinLock(new) ->
    snmp_generic:variable_func(new, {usmUserSpinLock, volatile}),
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    Val = random:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {usmUserSpinLock, volatile});

usmUserSpinLock(delete) ->
    ok;

usmUserSpinLock(get) ->
    snmp_generic:variable_func(get, {usmUserSpinLock, volatile}).

usmUserSpinLock(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {usmUserSpinLock, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
usmUserSpinLock(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {usmUserSpinLock, volatile}).


%% Op == new | delete
usmUserTable(Op) ->
    snmp_generic:table_func(Op, db(usmUserTable)).

%% Op == get | is_set_ok | set | get_next
usmUserTable(get, RowIndex, Cols) ->
    get_patch(Cols, get(usmUserTable, RowIndex, Cols));
usmUserTable(get_next, RowIndex, Cols) ->
    next_patch(next(usmUserTable, RowIndex, Cols));
usmUserTable(is_set_ok, RowIndex, Cols) ->
    %% Add a dummy value for securityName; otherwise snmp_generic will
    %% think that a value is missing, so the row can't be created.
    %% Note: this value is only added for is_set_ok, not for set!
    NCols = [{?usmUserSecurityName, ""} | Cols],
    validate_is_set_ok(snmp_generic:table_func(is_set_ok, RowIndex,
					       NCols, db(usmUserTable)),
		       RowIndex, Cols);
usmUserTable(set, RowIndex, Cols) ->
    NCols = pre_set(RowIndex, Cols),
    %% NOTE: The NCols parameter is sent to snmp_generic, but not to
    %% validate_set!  The reason is that the columns from pre_set are
    %% set in snmp_generic, but not used by validate_set.
    validate_set(snmp_generic:table_func(set, RowIndex,
					 NCols, db(usmUserTable)),
		 RowIndex, Cols);
usmUserTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(usmUserTable)).

%% Patch the values stored in the DB with other values for some
%% objects.
get_patch([?usmUserCloneFrom | Cols], [{value, _Val} | Vals]) ->
    [{value, ?zeroDotZero} | get_patch(Cols, Vals)];
get_patch([?usmUserAuthKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([?usmUserOwnAuthKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([?usmUserPrivKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([?usmUserOwnPrivKeyChange | Cols], [{value, _Val} | Vals]) ->
    [{value, ""} | get_patch(Cols, Vals)];
get_patch([_Col | Cols], [Val | Vals]) ->
    [Val | get_patch(Cols, Vals)];
get_patch(_Cols, Result) ->
    Result.

next_patch([{[?usmUserCloneFrom | Idx], _Val} | Vals]) ->
    [{[?usmUserCloneFrom | Idx], ?zeroDotZero} | next_patch(Vals)];
next_patch([{[?usmUserAuthKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserAuthKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([{[?usmUserOwnAuthKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserOwnAuthKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([{[?usmUserPrivKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserPrivKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([{[?usmUserOwnPrivKeyChange | Idx], _Val} | Vals]) ->
    [{[?usmUserOwnPrivKeyChange | Idx], ""} | next_patch(Vals)];
next_patch([Val | Vals]) ->
    [Val | next_patch(Vals)];
next_patch(Result) -> Result.


validate_is_set_ok({noError, 0}, RowIndex, Cols) ->
    validate_clone_from(RowIndex, Cols),
    validate_auth_protocol(RowIndex, Cols),
    validate_auth_key_change(RowIndex, Cols),
    validate_own_auth_key_change(RowIndex, Cols),
    validate_priv_protocol(RowIndex, Cols),
    validate_priv_key_change(RowIndex, Cols),
    validate_own_priv_key_change(RowIndex, Cols),
    {noError, 0};
validate_is_set_ok(Error, _RowIndex, _Cols) ->
    Error.

pre_set(RowIndex, Cols) ->
    %% Possibly initialize the usmUserSecurityName and privacy keys
    case snmp_generic:table_row_exists(db(usmUserTable), RowIndex) of
	true -> Cols;
	false ->
	    SecName = get_user_name(RowIndex),
	    [{?usmUserSecurityName, SecName} | Cols] ++
		[{?usmUserAuthKey, ""},
		 {?usmUserPrivKey, ""}]
    end.

validate_set({noError, 0}, RowIndex, Cols) ->
    %% Now, all is_set_ok validation steps have been executed.  So
    %% everything is ready for the set.
    set_clone_from(RowIndex, Cols),
    set_auth_key_change(RowIndex, Cols),
    set_own_auth_key_change(RowIndex, Cols),
    set_priv_key_change(RowIndex, Cols),
    set_own_priv_key_change(RowIndex, Cols),
    {noError, 0};
validate_set(Error, _RowIndex, _Cols) ->
    Error.

%%-----------------------------------------------------------------
%% Here's the alg: If this is the first time the CloneFrom is written,
%% we must check that the CloneFrom row exists, so we can invoke the
%% clone process in the set phase.  Otherwise, the set succed, with
%% no further checks.
%%-----------------------------------------------------------------
validate_clone_from(RowIndex, Cols) ->
    case lists:keysearch(?usmUserCloneFrom, 1, Cols) of
	{value, {_Col, RowPointer}} ->
	    RowIndex2 = extract_row(RowPointer),
	    OldCloneFrom = snmp_generic:table_get_element(db(usmUserTable),
							  RowIndex,
							  ?usmUserCloneFrom),
	    case OldCloneFrom of
		{value, Val} when Val /= noinit ->
		    %% This means that the cloning is already done...
		    ok;
		_ ->
		    %% Otherwise, we must check the CloneFrom value
		    case snmp_generic:table_get_element(db(usmUserTable),
							RowIndex2,
							?usmUserStatus) of
			{value, ?'RowStatus_active'} -> ok;
			_ -> throw({inconsistentName, ?usmUserCloneFrom})
		    end
	    end;
	false ->
	    ok
    end.


validate_auth_protocol(RowIndex, Cols) ->
    case lists:keysearch(?usmUserAuthProtocol, 1, Cols) of
	{value, {_Col, AuthProtocol}} ->
	    %% Check if the row has been cloned; we can't check the
	    %% old value of authProtocol, because if the row was
	    %% createAndWaited, the default value would have been
	    %% written (usmNoAuthProtocol).
	    OldCloneFrom = snmp_generic:table_get_element(db(usmUserTable),
							  RowIndex,
							  ?usmUserCloneFrom),
	    case OldCloneFrom of
		{value, Val} when Val /= noinit ->
		    %% This means that the cloning is already done; set is ok
		    %% if new protocol is usmNoAuthProtocol
		    case AuthProtocol of
			?usmNoAuthProtocol ->
			    %% Check that the Priv protocl is noPriv
			    case get_priv_proto(RowIndex, Cols) of
				?usmNoPrivProtocol -> ok;
				_ -> throw({inconsistentValue,
					    ?usmUserAuthProtocol})
			    end;
			?usmHMACMD5AuthProtocol ->
			    throw({inconsistentValue, ?usmUserAuthProtocol});
			?usmHMACSHAAuthProtocol ->
			    throw({inconsistentValue, ?usmUserAuthProtocol});
			_ ->
			    throw({wrongValue, ?usmUserAuthProtocol})
		    end;
		_ ->
		    %% Otherwise, check that the new protocol is known,
		    %% and that the system we're running supports the
		    %% hash function.
		    case AuthProtocol of
			?usmNoAuthProtocol ->
			    %% Check that the Priv protocl is noPriv
			    case get_priv_proto(RowIndex, Cols) of
				?usmNoPrivProtocol -> ok;
				_ -> throw({inconsistentValue,
					    ?usmUserAuthProtocol})
			    end;
			?usmHMACMD5AuthProtocol ->
			    case is_crypto_supported(md5_mac_96) of
				true -> ok;
				false ->
				    throw({wrongValue, ?usmUserAuthProtocol})
			    end;
			?usmHMACSHAAuthProtocol ->
			    case is_crypto_supported(sha_mac_96) of
				true -> ok;
				fasle ->
				    throw({wrongValue, ?usmUserAuthProtocol})
			    end;
			_ -> throw({wrongValue, ?usmUserAuthProtocol})
		    end
	    end;
	false ->
	    ok
    end.

validate_auth_key_change(RowIndex, Cols) ->
    validate_key_change(RowIndex, Cols, ?usmUserAuthKeyChange, auth).

validate_own_auth_key_change(RowIndex, Cols) ->
    validate_requester(RowIndex, Cols, ?usmUserOwnAuthKeyChange),
    validate_key_change(RowIndex, Cols, ?usmUserOwnAuthKeyChange, auth).

validate_priv_key_change(RowIndex, Cols) ->
    validate_key_change(RowIndex, Cols, ?usmUserPrivKeyChange, priv).

validate_own_priv_key_change(RowIndex, Cols) ->
    validate_requester(RowIndex, Cols, ?usmUserOwnPrivKeyChange),
    validate_key_change(RowIndex, Cols, ?usmUserOwnPrivKeyChange, priv).

%% Check that the requesting user is the same as the modified user
validate_requester(RowIndex, Cols, KeyChangeCol) ->
    case lists:keysearch(KeyChangeCol, 1, Cols) of
	{value, _} ->
	    case get(sec_model) of % Check the securityModel in the request
		?SEC_USM -> ok;
		_ -> throw({noAccess, KeyChangeCol})
	    end,
	    %% The SecurityName may not be set yet.  First, check if it is set.
	    SecNameForUser = 
		case snmp_generic:table_get_element(db(usmUserTable),
						    RowIndex,
						    ?usmUserSecurityName) of
		    {value, Val} when Val /= noinit -> Val;
		    _ -> get_user_name(RowIndex)
		end,
	    case get(sec_name) of % Check the securityName in the request
		SecNameForUser -> ok;
		_ -> throw({noAccess, KeyChangeCol})
	    end;
	false ->
	    ok
    end.

validate_key_change(RowIndex, Cols, KeyChangeCol, Type) ->
    case lists:keysearch(KeyChangeCol, 1, Cols) of
	{value, {_Col, KeyC}} ->
	    %% Check if the row has been cloned; or if it is cloned in
	    %% this set-operation.
	    OldCloneFrom = snmp_generic:table_get_element(db(usmUserTable),
							  RowIndex,
							  ?usmUserCloneFrom),
	    IsClonePresent = case lists:keysearch(?usmUserCloneFrom, 1, Cols) of
				 {value, _} -> true;
				 false -> false
			     end,
	    %% Set is ok if 1) the user already is created, 2) this is
	    %% a new user, which has been cloned, or is about to be
	    %% cloned.
	    case {OldCloneFrom, IsClonePresent} of
		{{value, Val}, _} when Val /= noinit ->
		    %% The user exists, or has been cloned
		    ok;
		{_, true} ->
		    %% The user is cloned in this operation
		    ok;
		_ ->
		    %% The user doen't exist, or hasn't been cloned,
		    %% and is not cloned in this operation.
		    throw({inconsistentName, KeyChangeCol})
	    end,
	    %% Check that the length makes sense
	    Len = length(KeyC),
	    case Type of
		auth ->
		    case get_auth_proto(RowIndex, Cols) of
			?usmNoAuthProtocol -> ok;
			?usmHMACMD5AuthProtocol when Len == 32 -> ok;
			?usmHMACSHAAuthProtocol when Len == 40 -> ok;
			_ -> throw({wrongValue, KeyChangeCol})
		    end;
		priv ->
		    case get_priv_proto(RowIndex, Cols) of
			?usmNoPrivProtocol -> ok;
			?usmDESPrivProtocol when Len == 32 -> ok;
			_ -> throw({wrongValue, KeyChangeCol})
		    end
	    end;
	false ->
	    ok
    end.

validate_priv_protocol(RowIndex, Cols) ->
    case lists:keysearch(?usmUserPrivProtocol, 1, Cols) of
	{value, {_Col, PrivProtocol}} ->
	    %% Check if the row has been cloned; we can't check the
	    %% old value of privhProtocol, because if the row was
	    %% createAndWaited, the default value would have been
	    %% written (usmNoPrivProtocol).
	    OldCloneFrom = snmp_generic:table_get_element(db(usmUserTable),
							  RowIndex,
							  ?usmUserCloneFrom),
	    case OldCloneFrom of
		{value, Val} when Val /= noinit ->
		    %% This means that the cloning is already done; set is ok
		    %% if new protocol is usmNoPrivProtocol
		    case PrivProtocol of
			?usmNoPrivProtocol ->
			    ok;
			?usmDESPrivProtocol ->
			    throw({inconsistentValue, ?usmUserPrivProtocol});
			_ ->
			    throw({wrongValue, ?usmUserPrivProtocol})
		    end;
		_ ->
		    %% Otherwise, check that the new protocol is known,
		    %% and that the system we're running supports the
		    %% crypto function.
		    case PrivProtocol of
			?usmNoPrivProtocol ->
			    ok;
			?usmDESPrivProtocol ->
			    %% The 'catch' handles the case when 'crypto' is
			    %% not present in the system.
			    case is_crypto_supported(des_cbc_decrypt) of
				true ->
				    case get_auth_proto(RowIndex, Cols) of
					?usmNoAuthProtocol ->
					    throw({inconsistentValue,
						   ?usmUserPrivProtocol});
					_ ->
					    ok
				    end;
				false -> 
				    throw({wrongValue, ?usmUserPrivProtocol})
			    end;
			_ -> throw({wrongValue, ?usmUserPrivProtocol})
		    end
	    end;
	false ->
	    ok
    end.


set_clone_from(RowIndex, Cols) ->
    %% If CloneFrom is modified, do the cloning.
    case lists:keysearch(?usmUserCloneFrom, 1, Cols) of
	{value, {_Col, RowPointer}} ->
	    RowIndex2 = extract_row(RowPointer), % won't fail
	    CloneRow = snmp_generic:table_get_row(db(usmUserTable), RowIndex2),
	    AuthP = element(?usmUserAuthProtocol, CloneRow),
	    PrivP = element(?usmUserPrivProtocol, CloneRow),
	    AuthK = element(?usmUserAuthKey, CloneRow),
	    PrivK = element(?usmUserPrivKey, CloneRow),
	    SCols = [{?usmUserAuthProtocol, AuthP},
		     {?usmUserPrivProtocol, PrivP},
		     {?usmUserAuthKey, AuthK},
		     {?usmUserPrivKey, PrivK}],
	    case snmp_generic:table_set_elements(db(usmUserTable),
						 RowIndex,
						 SCols) of
		true -> ok;
		false -> {commitFailed, ?usmUserCloneFrom}
	    end;
	false ->
	    ok
    end.

set_auth_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserAuthKeyChange, auth).

set_own_auth_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserOwnAuthKeyChange, auth).

set_priv_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserPrivKeyChange, priv).

set_own_priv_key_change(RowIndex, Cols) ->
    set_key_change(RowIndex, Cols, ?usmUserOwnPrivKeyChange, priv).

set_key_change(RowIndex, Cols, KeyChangeCol, Type) ->
    case lists:keysearch(KeyChangeCol, 1, Cols) of
	{value, {_Col, KeyChange}} ->
	    KeyCol = case Type of
			 auth -> ?usmUserAuthKey;
			 priv -> ?usmUserPrivKey
		     end,
	    [AuthP, Key] =
		snmp_generic:table_get_elements(db(usmUserTable),
						RowIndex,
						[?usmUserAuthProtocol,
						 KeyCol]),
	    NewKey = extract_new_key(AuthP, Key, KeyChange),
	    snmp_generic:table_set_element(db(usmUserTable), RowIndex,
					   KeyCol, NewKey);
	false ->
	    ok
    end.

%% Extract the UserName part from a RowIndex.
get_user_name([L1 | Rest])         -> get_user_name(L1, Rest).
get_user_name(0, [_L2 | UserName]) -> UserName;
get_user_name(N, [H | T])          -> get_user_name(N-1, T).

extract_row(RowPtr)                     -> extract_row(?usmUserEntry, RowPtr).
extract_row([H | T], [H | T2])          -> extract_row(T, T2);
extract_row([], [?usmUserSecurityName | T]) -> T;
extract_row(_, _) -> throw({wrongValue, ?usmUserCloneFrom}).

%% Pre: the user exixt
get_auth_proto(RowIndex, Cols) ->
    %% The protocol can be chanegd by the request too, otherwise,
    %% check the stored protocol.
    case lists:keysearch(?usmUserAuthProtocol, 1, Cols) of
	{value, {_, Protocol}} ->
	    Protocol;
	false ->
	    %% OTP-3596
	    case snmp_generic:table_get_element(db(usmUserTable),
						RowIndex,
						?usmUserAuthProtocol) of
		{value, Protocol} ->
		    Protocol;
		_ ->
		    undefined
	    end
    end.

%% Pre: the user exixt
get_priv_proto(RowIndex, Cols) ->
    %% The protocol can be chanegd by the request too, otherwise,
    %% check the stored protocol.
    case lists:keysearch(?usmUserPrivProtocol, 1, Cols) of
	{value, {_, Protocol}} ->
	    Protocol;
	false ->
	    %% OTP-3596
	    case snmp_generic:table_get_element(db(usmUserTable),
						RowIndex,
						?usmUserPrivProtocol) of
		{value, Protocol} ->
		    Protocol;
		_ ->
		    undefined
	    end
    end.


db(X) -> {X, persistent}.

fa(usmUserTable) -> ?usmUserSecurityName.
 
foi(usmUserTable) -> ?usmUserEngineID.
 
noc(usmUserTable) -> 13.

stc(usmUserTable) -> ?usmUserStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).

 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

%%-----------------------------------------------------------------
%% Key change functions.  The KeyChange Texual-Convention is
%% defined in the SNMP-USER-BASED-SM-MIB.
%% Note that this implementation supports md5 and sha, which
%% both have fixed length requirements on the length of the key;
%% thus the implementation can be (and is) simplified.
%%-----------------------------------------------------------------
mk_key_change(Hash, OldKey, NewKey) ->
    KeyLen = length(NewKey),
    Alg = case Hash of
	      ?usmHMACMD5AuthProtocol -> md5;
	      ?usmHMACSHAAuthProtocol -> sha;
	      md5 -> md5;
	      sha -> sha
	  end,
    Random = mk_random(KeyLen),
    mk_key_change(Alg, OldKey, NewKey, KeyLen, Random).

%% This function is only exported for test purposes.  There is a test
%% case in the standard where Random is pre-defined.
mk_key_change(Alg, OldKey, NewKey, KeyLen, Random) ->
    %% OldKey and Random is of length KeyLen...
    Digest = lists:sublist(binary_to_list(crypto:Alg(OldKey++Random)), KeyLen),
    %% ... and so is Digest
    Delta = snmp_misc:str_xor(Digest, NewKey),
    Random ++ Delta.

%% Extracts a new Key from a KeyChange value, sent by a manager.
extract_new_key(?usmNoAuthProtocol, OldKey, KeyChange) ->
    OldKey;
extract_new_key(Hash, OldKey, KeyChange) ->
    KeyLen = length(OldKey),
    Alg = case Hash of
	      ?usmHMACMD5AuthProtocol -> md5;
	      ?usmHMACSHAAuthProtocol -> sha;
	      md5 -> md5;
	      sha -> sha
	  end,
    {Random, Delta} = split(KeyLen, KeyChange, []),
    Digest = lists:sublist(binary_to_list(crypto:Alg(OldKey++Random)), KeyLen),
    NewKey = snmp_misc:str_xor(Digest, Delta),
    NewKey.

-define(i16(Int), (Int bsr 8) band 255, Int band 255.
-define(i8(Int), Int band 255.

mk_random(Len) when Len =< 20 ->
    %% Use of yield():
    %% This will either schedule another process, or fail and invoke
    %% the error_handler (in old versions).  In either case, it is
    %% safe to assume that now, reductions and garbage_collection have
    %% changed in a non-deterministically way.
    {_,_,A} = erlang:now(),
    catch erlang:yield(),
    {_,_,B} = erlang:now(),
    catch erlang:yield(),
    {_,_,C} = erlang:now(),
    {D,_}   = erlang:statistics(reductions),
    {E,_}   = erlang:statistics(runtime),
    {F,_}   = erlang:statistics(wall_clock),
    {G,H,_} = erlang:statistics(garbage_collection),
    catch erlang:yield(),
    {_,_,C2} = erlang:now(),
    {D2,_}   = erlang:statistics(reductions),
    {_,H2,_} = erlang:statistics(garbage_collection),
    %% X(N) means we can use N bits from variable X:
    %% A(16) B(16) C(16) D(16) E(8) F(16) G(8) H(16)
    Rnd20 = [?i16(A),?i16(B),?i16(C),?i16(D),?i8(E),?i16(F),
	     ?i8(G),?i16(H),?i16(C2),?i16(D2),?i16(H2)],
    lists:sublist(Rnd20, Len).
    
split(0, Rest, FirstRev) ->
    {lists:reverse(FirstRev), Rest};
split(N, [H | T], FirstRev) when N > 0 ->
    split(N-1, T, [H | FirstRev]).


is_crypto_supported(Func) ->
    %% The 'catch' handles the case when 'crypto' is
    %% not present in the system (or not started).
    case catch lists:member(Func, crypto:info()) of
	true -> true;
	_ -> false
    end.
    
