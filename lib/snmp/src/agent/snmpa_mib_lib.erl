%%<copyright>
%% <year>2004-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
-module(snmpa_mib_lib).

-export([table_cre_row/3, table_del_row/2]).
-export([gc_tab/3, gc_tab/5]).

-include("SNMPv2-TC.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"MIB-LIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%%-----------------------------------------------------------------
%%%-----------------------------------------------------------------

%% returns: bool()
table_cre_row({Tab, mnesia}, Key, _Row) ->
    ?vtrace("create mnesia table ~w row with Key: ~w",[Tab, Key]),
    {error, mnesia_not_supported};
table_cre_row({Tab, Db} = TabDb, Key, Row) ->
    ?vtrace("create ~w table ~w row with Key: ~w",[Db, Tab, Key]),
    snmpa_local_db:table_create_row(TabDb, Key, Row).

%% returns: bool()
table_del_row({Tab, mnesia}, Key) ->
    ?vtrace("delete mnesia table ~w row with Key: ~w",[Tab, Key]),
    {error, mnesia_not_supported};
table_del_row({Tab, Db} = TabDb, Key) ->
    ?vtrace("delete ~w table ~w row with Key: ~w",[Db, Tab, Key]),
    snmpa_local_db:table_delete_row(TabDb, Key).


%%%-----------------------------------------------------------------
%%% Utility module for the mib-implementation modules (such as
%%% snmp_target_mib).
%%%-----------------------------------------------------------------

gc_tab(TabDb, STC, FOI) ->
    InvalidateRow = fun(_) -> ok end,
    UpdateRow     = fun(_) -> ok end,
    gc_tab(TabDb, STC, FOI, InvalidateRow, UpdateRow).

gc_tab({Tab,mnesia} = TabDb, STC, FOI, InvalidateRow, UpdateRow) ->
    F = fun(RowIndex, Row) ->
		case element(STC, Row) of
                    ?'StorageType_volatile' ->
			snmp_generic_mnesia:table_delete_row(Tab, RowIndex),
                        InvalidateRow(RowIndex);
		    _ ->
			UpdateRow(RowIndex)
		end
	end,
    gc_tab1(F, TabDb, FOI);

gc_tab(TabDb, STC, FOI, InvalidateRow, UpdateRow) ->
    F = fun(RowIndex, Row) ->
		case element(STC, Row) of
 		    ?'StorageType_volatile' ->
			snmpa_local_db:table_delete_row(TabDb, RowIndex),
			InvalidateRow(RowIndex);
		    _ ->
			UpdateRow(RowIndex),
			ok
		end
	end,
    gc_tab1(F, TabDb, FOI).


gc_tab1(F, {Tab,_} = TabDb, FOI) ->
    case (catch snmp_generic:table_foreach(TabDb, F, FOI)) of
	{'EXIT',{cyclic_db_reference,Oid}} ->
	    %% Remove the row regardless of storage type since this
	    %% is a major error. This row must be removed.
	    case table_delete_row(TabDb, Oid) of
		true -> 
		    ?vlog("deleted cyclic ref row for: ~w;~w",
			  [Tab, Oid]),
		    config_err("cyclic reference in table ~w: "
			       "~w -> ~w. Row deleted", 
			       [Tab, Oid, Oid]),
		    gc_tab1(F, TabDb, FOI);
		false ->
		    ?vlog("unable to remove faulty row from table ~w",
			  [Tab]),
		    config_err("failed removing faulty row. "
			       "Giving up on table ~w cleanup", [Tab])
	    end;
	_ ->
	    ok
    end.

table_delete_row({Tab, mnesia}, Oid) ->
    snmp_generic_mnesia:table_delete_row(Tab, Oid),
    true;
table_delete_row(TabDb, Oid) ->
    snmpa_local_db:table_delete_row(TabDb, Oid).

config_err(F, A) ->
    snmpa_error:config_err(F, A).
