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
%%% Purpose : Loads tables from local disc or from remote node

-module(mnesia_loader).

%% Mnesia internal stuff
-export([disc_load_table/2,
	 net_load_table/4,
	 send_table/3]).

-import(mnesia_lib, [set/2, fatal/2, verbose/2, dbg_out/2]).

-include("mnesia.hrl").

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', Reason} -> mnesia_lib:other_val(Var, Reason); 
	Value -> Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load a table from local disc

disc_load_table(Tab, Reason) ->
    Storage =  val({Tab, storage_type}),
    Type = val({Tab, setorbag}),
    dbg_out("Getting table ~p (~p) from disc: ~p~n",
	    [Tab, Storage, Reason]),
    ?eval_debug_fun({?MODULE, do_get_disc_copy},
		    [{tab, Tab},
		     {reason, Reason},
		     {storage, Storage}, 
		     {type, Type}]),
    do_get_disc_copy2(Tab, Reason, Storage, Type).

do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == unknown ->
    verbose("Local table copy of ~p has recently been deleted, ignored.~n",
	    [Tab]),
    {loaded, ok};  %% ?
do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == disc_copies ->
    %% NOW we create the actual table
    Repair = mnesia_monitor:get_env(auto_repair),
    Args = [{keypos, 2}, public, named_table, Type],
    mnesia_monitor:mktab(Tab, Args),
    case Reason of 
	dumper_create_table ->
	    ok = mnesia_log:ets2dcd(Tab);
	_ ->
	    Count = mnesia_log:dcd2ets(Tab, Repair),	    
	    case ets:info(Tab, size) of
		X when X < Count * 4 ->
		    ok = mnesia_log:ets2dcd(Tab); 
		_ ->
		    ignore
	    end
    end, 
    mnesia_index:init_index(Tab, Storage),
    snmpify(Tab, Storage),
    set({Tab, load_node}, node()),
    set({Tab, load_reason}, Reason),
    {loaded, ok};

do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == ram_copies ->
    Args = [{keypos, 2}, public, named_table, Type],
    mnesia_monitor:mktab(Tab, Args),
    Fname = mnesia_lib:tab2dcd(Tab),
    DiscLoad =
	case mnesia_monitor:use_dir() of
	    true ->
		case mnesia_lib:exists(Fname) of
		    true -> true;
		    false -> false
		end;
	    false ->
		false
	end,
    case DiscLoad of
	true ->	    
	    Repair = mnesia_monitor:get_env(auto_repair),
	    mnesia_log:dcd2ets(Tab, Repair),
	    mnesia_index:init_index(Tab, Storage),
	    snmpify(Tab, Storage),
	    set({Tab, load_node}, node()),
	    set({Tab, load_reason}, Reason),
	    {loaded, ok};
	false ->
	    mnesia_index:init_index(Tab, Storage),
	    snmpify(Tab, Storage),
	    set({Tab, load_node}, node()),
	    set({Tab, load_reason}, Reason),
	    {loaded, ok}
    end;

do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == disc_only_copies ->
    Args = [{file, mnesia_lib:tab2dat(Tab)},
	    {type, mnesia_lib:disk_type(Tab, Type)},
	    {keypos, 2},
	    {repair, mnesia_monitor:get_env(auto_repair)}],
    case mnesia_monitor:open_dets(Tab, Args) of
	{ok, _} ->
	    mnesia_index:init_index(Tab, Storage),
	    snmpify(Tab, Storage),
	    set({Tab, load_node}, node()),
	    set({Tab, load_reason}, Reason),
	    {loaded, ok};
	{error, Error} ->
	    {not_loaded, {"Failed to create dets table", Error}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load a table from a remote node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Receiver                             Sender
%% --------                             ------
%% Grab schema lock on table
%%                                      Determine table size
%% Create empty pre-grown table
%%                                      Grab read lock on table
%%                                      Let receiver subscribe on updates done on sender node
%%                                      Disable rehashing of table
%%                                      Release read lock on table
%%                                      Send table to receiver in chunks
%% 
%%                                      Grab read lock on table
%% Block dirty updates
%%                                      Update wherabouts
%% 
%%                                      Cancel the update subscription
%% Process the subscription events
%% Optionally dump to disc
%% Unblock dirty updates
%%                                      Release read lock on table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MAX_TRANSFER_SIZE, 7500). 
-define(MAX_RAM_FILE_SIZE, 1000000).
-define(MAX_RAM_TRANSFERS, (?MAX_RAM_FILE_SIZE div ?MAX_TRANSFER_SIZE) + 1).

net_load_table(Tab, Reason, Ns, Cs)
        when Reason == add_table_copy ->
    try_net_load_table(Tab, Reason, Ns, Cs);
net_load_table(Tab, Reason, Ns, _Cs) ->
    try_net_load_table(Tab, Reason, Ns, val({Tab, cstruct})).

try_net_load_table(Tab, Reason, [], Cs) ->
    verbose("Copy failed. No active replicas of ~p are available.~n", [Tab]),
    {not_loaded, none_active};
try_net_load_table(Tab, Reason, Ns, Cs) ->
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    do_get_network_copy(Tab, Reason, Ns, Storage, Cs).

do_get_network_copy(Tab, Reason, Ns, unknown, Cs) ->
    verbose("Local table copy of ~p has recently been deleted, ignored.~n", [Tab]),
    {not_loaded, storage_unknown};
do_get_network_copy(Tab, Reason, Ns, Storage, Cs) ->
    [Node | Tail] = Ns,
    dbg_out("Getting table ~p (~p) from node ~p: ~p~n",
	    [Tab, Storage, Node, Reason]),
    ?eval_debug_fun({?MODULE, do_get_network_copy},
		    [{tab, Tab}, {reason, Reason}, 
		     {nodes, Ns}, {storage, Storage}]),
    mnesia_controller:start_remote_sender(Node, Tab, self(), Storage),
    case tab_receiver(nopid, Node, Tab, undefined, Storage, Cs, ?MAX_RAM_TRANSFERS, first) of
	ok ->
	    set({Tab, load_node}, Node),
	    set({Tab, load_reason}, Reason),
	    mnesia_controller:i_have_tab(Tab),
	    dbg_out("Table ~p copied from ~p to ~p~n", [Tab, Node, node()]),
	    {loaded, ok};
	down ->
	    try_net_load_table(Tab, Reason, Tail, Cs) 
    end.

snmpify(Tab, Storage) ->
    do_snmpify(Tab, val({Tab, snmp}), Storage).

do_snmpify(Tab, [], _Storage) ->
    ignore;
do_snmpify(Tab, Us, Storage) ->
    Snmp = mnesia_snmp_hook:create_table(Us, Tab, Storage),
    set({Tab, {index, snmp}}, Snmp).

tab_receiver(Pid, Node, Tab, TabRef, Storage, Cs, RamLeft, State) ->
    receive
	{SenderPid, {first, TabSize}} when State == first ->
	    SenderPid ! {self(), more},
	    CreatedTabs = init_table(Tab, TabSize, Storage, Cs),
	    mnesia_tm:block_tab(Tab),
	    tab_receiver(SenderPid, Node, Tab, CreatedTabs, Storage, Cs, RamLeft, more);
	
	{Pid, {more, Recs}} when State == more ->
	    Pid ! {self(), more},
	    insert_records(TabRef, Recs),
	    opt_ram_to_disc(Tab, Storage, Cs, RamLeft),
	    tab_receiver(Pid, Node, Tab, TabRef, Storage, Cs, RamLeft - 1, State);

	{Pid, {no_more, DatBin}} when State == more ->
	    subscr_receiver(Tab, TabRef, Storage, Cs#cstruct.record_name),
	    case handle_last(Tab, Storage, Cs#cstruct.type, DatBin) of
		ok ->
		    mnesia_index:init_index(Tab, Storage),
		    snmpify(Tab, Storage),
		    Pid ! {self(), no_more},
		    mnesia_tm:unblock_tab(Tab),
		    ok;
		{error, Reason} ->
		    Msg = "Failed to handle last",
		    dbg_out("~s: ~p: ~p~n", [Msg, Tab, Reason]),
		    down(Tab, Storage)
	    end;
	
	{copier_done, Node} ->
	    dbg_out("Sender of table ~p crashed on node ~p (~p)~n",
		    [Tab, Node, State]),
	    down(Tab, Storage);
	
	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    tab_receiver(Pid, Node, Tab, TabRef, Storage, Cs, RamLeft, State)
    end.

init_table(Tab, TabSize, Storage, Cs) ->
    case Storage of
	unknown ->
	    no_need;
	disc_only_copies ->
	    mnesia_lib:lock_table(Tab) ,
	    Tmp = mnesia_lib:tab2tmp(Tab),
	    Args = [{file, Tmp},
		    {keypos, 2},
		    {ram_file, true},
		    {estimated_no_objects, TabSize + 256},
		    {repair, mnesia_monitor:get_env(auto_repair)},
		    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)}],
	    file:delete(Tmp),
	    case mnesia_lib:dets_sync_open(Tab, Args) of
		{ok, _} ->
		    {Storage, Tab};
		Else ->
		    fatal("do_get_network_copy open ~w Tab ~w cstruct ~w ~s ~n",
			  [Else, Tab, Cs, Tmp])
	    end;
	_ -> %% ram_copies or disc_copies
	    Args = [{keypos, 2}, public, named_table, Cs#cstruct.type],
	    mnesia_monitor:mktab(Tab, Args), % exits upon failure
	    {Storage, Tab}
    end.

opt_ram_to_disc(Tab, disc_only_copies, Cs, 0) -> 
    dets:close(Tab), % Flush dets table in ram to disc
    Tmp = mnesia_lib:tab2tmp(Tab),
    Args = [{file, Tmp},
	    {keypos, 2},
	    {repair, mnesia_monitor:get_env(auto_repair)},
	    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)}],
    case dets:open_file(Tab, Args) of
	{ok, _} ->
	    ok;
	Else ->
	    fatal("do_get_network_copy reopen ~w Tab ~w cstruct ~w ~s ~n",
		  [Else, Tab, Cs, Tmp])
    end;
opt_ram_to_disc(_, _, _, _) ->  %% Disc table but not time to flush
    ignore. 

subscr_receiver(Tab, TabRef, Storage, RecName) ->
    receive
	{mnesia_table_event, {Op, Val, _Tid}} ->
	    if
		Tab == RecName ->
		    handle_event(TabRef, Op, Val);
		true ->
		    handle_event(TabRef, Op, setelement(1, Val, RecName))
	    end,
	    subscr_receiver(Tab, TabRef, Storage, RecName);

	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    subscr_receiver(Tab, TabRef, Storage, RecName)
    after 0 ->
	    ok
    end.

handle_event(TabRef, write, Rec) ->	    
    db_put(TabRef, Rec);
handle_event(TabRef, delete, {_Tab, Key}) ->
    db_erase(TabRef, Key);
handle_event(TabRef, delete_object, OldRec) ->
    db_match_erase(TabRef, OldRec);
handle_event(TabRef, clear_table, {_Tab, _Key}) ->
    db_match_erase(TabRef, '_').

handle_last(Tab, disc_copies, _Type, nobin) ->
    Ret = mnesia_log:ets2dcd(Tab),
    Fname = mnesia_lib:tab2dat(Tab),  
    case mnesia_lib:exists(Fname) of 
	true ->  %% Remove old .DAT files.
	    file:delete(Fname);
	false ->
	    ok
    end,
    Ret;

handle_last(Tab, disc_only_copies, Type, nobin) ->
    case mnesia_lib:swap_tmp_files([Tab]) of
	[] ->
	    Args = [{file, mnesia_lib:tab2dat(Tab)},
		    {type, mnesia_lib:disk_type(Tab, Type)},
		    {keypos, 2},
		    {repair, mnesia_monitor:get_env(auto_repair)}],
	    mnesia_monitor:open_dets(Tab, Args),
	    ok;
	L when list(L) ->
	    {error, {"Cannot swap tmp files", Tab, L}}
    end;

handle_last(Tab, ram_copies, _Type, nobin) ->
    ok;
handle_last(Tab, ram_copies, _Type, DatBin) ->
    case mnesia_monitor:use_dir() of
	true ->
	    mnesia_lib:lock_table(Tab),
	    Tmp = mnesia_lib:tab2tmp(Tab),
	    ok = file:write_file(Tmp, DatBin),
	    ok = file:rename(Tmp, mnesia_lib:tab2dcd(Tab)),
	    mnesia_lib:unlock_table(Tab),
	    ok;
	false ->
	    ok
    end.

down(Tab, Storage) ->
    %% dbg_out("DOWN ~p (~p)~n", [Tab, Storage]),
    case Storage of
	ram_copies ->
	    catch ?ets_delete_table(Tab);
	disc_copies ->
	    catch ?ets_delete_table(Tab);
	disc_only_copies ->
	    mnesia_lib:cleanup_tmp_files([Tab])
    end,
    mnesia_checkpoint:tm_del_copy(Tab, node()),
    mnesia_controller:sync_del_table_copy_whereabouts(Tab, node()),
    mnesia_tm:unblock_tab(Tab),
    flush_subcrs(),
    down.

flush_subcrs() ->
    receive
	{mnesia_table_event, _} ->
	    flush_subcrs();

	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    flush_subcrs()
    after 0 ->
	    done
    end.

insert_records(TabRef, [Recs | Tail]) ->
    lists:foreach(fun(Rec) -> db_put(TabRef, Rec) end, Recs), %% FIX R8
    insert_records(TabRef, Tail);
insert_records(TabRef, []) ->
    ok.

db_erase({ram_copies, Tab}, Key) ->
    true = ?ets_delete(Tab, Key);
db_erase({disc_copies, Tab}, Key) ->
    true = ?ets_delete(Tab, Key);
db_erase({disc_only_copies, Tab}, Key) ->
    ok = dets:delete(Tab, Key).

db_match_erase({ram_copies, Tab} , Pat) ->
    true = ?ets_match_delete(Tab, Pat);
db_match_erase({disc_copies, Tab} , Pat) ->
    true = ?ets_match_delete(Tab, Pat);
db_match_erase({disc_only_copies, Tab}, Pat) ->
    ok = dets:match_delete(Tab, Pat).

db_put({ram_copies, Tab}, Val) ->
    true = ?ets_insert(Tab, Val);
db_put({disc_copies, Tab}, Val) ->
    true = ?ets_insert(Tab, Val);
db_put({disc_only_copies, Tab}, Val) ->
    ok = dets:insert(Tab, Val).

%% This code executes at the remote site where the data is
%% executes in a special copier process.

send_table(Pid, Tab, RemoteS) ->
    case ?catch_val({Tab, storage_type}) of
	{'EXIT', _} ->
	    {error, {no_exists, Tab}};
	unknown ->
	    {error, {no_exists, Tab}};
	Storage ->
	    %% Send first
	    TabSize = mnesia:table_info(Tab, size),
	    Pid ! {self(), {first, TabSize}},

	    %% Calculate #keys per transfer
	    Key = mnesia_lib:db_first(Storage, Tab),
	    Recs = mnesia_lib:db_get(Storage, Tab, Key),
	    BinSize = size(term_to_binary(Recs)),
	    KeysPerTransfer = (?MAX_TRANSFER_SIZE div BinSize) + 1,
	    
	    SendIt = fun() ->
			     prepare_copy(Pid, Tab, Storage),
			     send_more(Pid, Tab, Storage, 
				       mnesia_lib:db_init_chunk(Storage, Tab,
								KeysPerTransfer)),
			     finish_copy(Pid, Tab, Storage, RemoteS)
		     end,

	    case catch SendIt() of
		receiver_died ->
		    unlink(whereis(mnesia_tm)),
		    ok;
		{_, receiver_died} ->
		    unlink(whereis(mnesia_tm)),
		    ok;
		{atomic, no_more} ->
		    unlink(whereis(mnesia_tm)),
		    ok;
		Reason ->
		    unlink(whereis(mnesia_tm)),
		    cleanup_tab_copier(Pid, Storage, Tab),
		    {error, Reason}
	    end
    end.
		
prepare_copy(Pid, Tab, Storage) ->
    Trans =
	fun() ->
		mnesia:write_lock_table(Tab),
		mnesia_subscr:subscribe(Pid, {table, Tab}),
		update_where_to_write(Tab, node(Pid)),
		mnesia_lib:db_fixtable(Storage, Tab, true),
		ok
	end,
    case mnesia:transaction(Trans) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    exit({tab_copier_prepare, Tab, Reason})
    end.

update_where_to_write(Tab, Node) ->
    case val({Tab, access_mode}) of
	read_only -> 
	    ignore;
	read_write -> 
	    Current = val({current, db_nodes}),
	    Ns = 
		case lists:member(Node, Current) of
		    true -> Current;
		    false -> [Node | Current]
		end,
	    update_where_to_write(Ns, Tab, Node)
    end.

update_where_to_write([], _, _) -> 
    ok;
update_where_to_write([H|T], Tab, AddNode) ->
    case mnesia_monitor:needs_protocol_conversion(H) of
	true ->
	    rpc:call(H,  mnesia_lib, add, [{Tab, where_to_write}, AddNode]);
	false ->
	    rpc:call(H,  mnesia_controller, call, [{update_where_to_write, [add, Tab, AddNode], self()}])
    end,
    update_where_to_write(T, Tab, AddNode).


send_more(Pid, Tab, Storage, DataState) ->
    receive
	{Pid, more} ->
	    case DataState of 
		{Recs, Cont} ->
		    Pid ! {self(), {more, [Recs]}}, %% Build list to handle old mnesia nodes
		    send_more(Pid, Tab, Storage, mnesia_lib:db_chunk(Storage, Cont));
		'$end_of_table' -> 
		    ok
	    end;
	{copier_done, Node} when Node == node(Pid)->
	    verbose("Receiver of table ~p crashed on ~p (more)~n", [Tab, Node]),
	    throw(receiver_died)
    end.

finish_copy(Pid, Tab, Storage, RemoteS) ->
    RecNode = node(Pid),
    DatBin = dat2bin(Tab, Storage, RemoteS),
    Trans =
	fun() ->
		mnesia:read_lock_table(Tab),
		A = val({Tab, access_mode}),
		mnesia_controller:sync_and_block_table_whereabouts(Tab, RecNode, RemoteS, A),
		cleanup_tab_copier(Pid, Storage, Tab),
		ok = mnesia_checkpoint:tm_add_copy(Tab, RecNode),
		Pid ! {self(), {no_more, DatBin}},
		receive
		    {Pid, no_more} -> % Dont bother about the spurious 'more' message
			no_more;
		    {copier_done, Node} when Node == node(Pid)->
			verbose("Tab receiver ~p crashed (more): ~p~n", [Tab, Node]),
			receiver_died
		end
	end,
    mnesia:transaction(Trans).

cleanup_tab_copier(Pid, Storage, Tab) ->
    mnesia_lib:db_fixtable(Storage, Tab, false),
    mnesia_subscr:unsubscribe(Pid, {table, Tab}).

dat2bin(Tab, ram_copies, ram_copies) ->
    mnesia_lib:lock_table(Tab),
    Res = file:read_file(mnesia_lib:tab2dcd(Tab)),
    mnesia_lib:unlock_table(Tab),
    case Res of
	{ok, DatBin} -> DatBin;
	_ -> nobin
    end;
dat2bin(_Tab, _LocalS, RemoteS) ->
    nobin.

handle_exit(Pid, Reason) when node(Pid) == node() ->
    exit(Reason);
handle_exit(Pid, Reason) ->  %% Not from our node, this will be handled by 
    ignore.                  %% mnesia_down soon.
