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
%% Purpose: Handles index functionality in mnesia

-module(mnesia_index).
-export([read/5,
	 add_index/5,
	 delete_index/3,
	 del_object_index/5,
	 clear_index/4,
	 match_object/5,
	 dirty_match_object/3,
	 dirty_read/3,
	 dirty_read2/3,
	 realkeys/3,

	 db_put/2,
	 db_get/2,
	 db_match_erase/2,
	 get_index_table/2,
	 get_index_table/3,
	 
	 tab2filename/2,
	 tab2tmp_filename/2,
	 init_index/2,
	 init_indecies/3,
	 del_transient/2,
	 del_transient/3,
	 del_index_table/3]).

-import(mnesia_lib, [verbose/2]).
-include("mnesia.hrl").

-record(index, {setorbag, pos_list}).

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _ReASoN_} -> mnesia_lib:other_val(Var, _ReASoN_); 
	_VaLuE_ -> _VaLuE_ 
    end.

%% read an object list throuh its index table
%% we assume that table Tab has index on attribute number Pos

read(Tid, Store, Tab, IxKey, Pos) ->
    ResList = mnesia_locker:ixrlock(Tid, Store, Tab, IxKey, Pos),
    %% Remove all tuples which don't include Ixkey, happens when Tab is a bag
    case val({Tab, setorbag}) of
	bag -> 
	    mnesia_lib:key_search_all(IxKey, Pos, ResList);
	_ -> 
	    ResList
    end.

add_index(Index, Tab, Key, Obj, Old) ->    
    add_index2(Index#index.pos_list, Index#index.setorbag, Tab, Key, Obj, Old).

add_index2([{Pos, Ixt} |Tail], bag, Tab, K, Obj, OldRecs) ->
    db_put(Ixt, {element(Pos, Obj), K}),
    add_index2(Tail, bag, Tab, K, Obj, OldRecs);
add_index2([{Pos, Ixt} |Tail], Type, Tab, K, Obj, OldRecs) ->
    %% Remove old tuples in index if Tab is updated
    case OldRecs of 
	undefined -> 
	    Old = mnesia_lib:db_get(Tab, K),
	    del_ixes(Ixt, Old, Pos, K);
	Old -> 
	    del_ixes(Ixt, Old, Pos, K)
    end,
    db_put(Ixt, {element(Pos, Obj), K}),
    add_index2(Tail, Type, Tab, K, Obj, OldRecs);
add_index2([], _, Tab, K, Obj, _) -> ok.

delete_index(Index, Tab, K) ->
    delete_index2(Index#index.pos_list, Tab, K).

delete_index2([{Pos, Ixt} | Tail], Tab, K) ->
    DelObjs = mnesia_lib:db_get(Tab, K), 
    del_ixes(Ixt, DelObjs, Pos, K),
    delete_index2(Tail, Tab, K);
delete_index2([], Tab, K) -> ok.


del_ixes(Ixt, [], Pos, L) -> ok;
del_ixes(Ixt, [Obj | Tail], Pos, Key) ->
    db_match_erase(Ixt, {element(Pos, Obj), Key}),
    del_ixes(Ixt, Tail, Pos, Key).

del_object_index(Index, Tab, K, Obj, Old) ->
    del_object_index2(Index#index.pos_list, Index#index.setorbag, Tab, K, Obj, Old).

del_object_index2([], _, Tab, K, Obj, Old) -> ok;
del_object_index2([{Pos, Ixt} | Tail], SoB, Tab, K, Obj, Old) ->
    case SoB of
	bag -> 
	    del_object_bag(Tab, K, Obj, Pos, Ixt, Old);
	_ -> %% If set remove the tuple in index table
	    del_ixes(Ixt, [Obj], Pos, K)	
    end,
    del_object_index2(Tail, SoB, Tab, K, Obj, Old).

del_object_bag(Tab, Key, Obj, Pos, Ixt, undefined) -> 
    Old = mnesia_lib:db_get(Tab, Key),
    del_object_bag(Tab, Key, Obj, Pos, Ixt, Old);
%% If Tab type is bag we need remove index identifier if Tab 
%% contains less than 2 elements. 
del_object_bag(Tab, Key, Obj, Pos, Ixt, Old) when length(Old) < 2 ->
    del_ixes(Ixt, [Obj], Pos, Key);
del_object_bag(Tab, Key, Obj, Pos, Ixt, Old) -> ok.

clear_index(Index, Tab, K, Obj) ->
    clear_index2(Index#index.pos_list, Tab, K, Obj).

clear_index2([], Tab, K, Obj) -> ok;
clear_index2([{Pos, Ixt} | Tail], Tab, K, Obj) ->
    db_match_erase(Ixt, Obj),
    clear_index2(Tail, Tab, K, Obj).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

realkeys(Tab, Pos, IxKey) ->
    Index = get_index_table(Tab, Pos),
    db_get(Index, IxKey).
    %% a list on the form [{IxKey, RealKey1} , ....
    
%%%% REMOVE after next rel....not used anymore
match_object(Tid, Store, Tab, Pat, Pos) ->
    %% Assume that we are on the node where the replica is
    mnesia_locker:rlock_table(Tid, Store, Tab), %% ignore return value
    dirty_match_object(Tab, Pat, Pos).

dirty_match_object(Tab, Pat, Pos) ->
    %% Assume that we are on the node where the replica is
    case element(2, Pat) of
	'_' ->
	    IxKey = element(Pos, Pat),
	    RealKeys = mnesia_index:realkeys(Tab, Pos, IxKey),
	    merge(RealKeys, Tab, Pat, []);
	Else ->
	    mnesia_lib:db_match_object(Tab, Pat)
    end.

merge([{IxKey, RealKey}|Tail], Tab, Pat0, Ack) ->
    %% Assume that we are on the node where the replica is
    PatternL = setelement(2, Pat0, RealKey),
    L0 = mnesia_lib:db_match_object(Tab, PatternL),
    merge(Tail, Tab, Pat0, L0 ++ Ack);

merge([], _, _, Ack) -> Ack.

dirty_read(Tab, IxKey, Pos) ->
    ResList = mnesia:dirty_rpc(Tab, mnesia_index, dirty_read2,
			       [Tab, IxKey, Pos]),
    case val({Tab, setorbag}) of
	bag ->
	    %% Remove all tuples which don't include Ixkey
	    mnesia_lib:key_search_all(IxKey, Pos, ResList);
	_ -> 
	    ResList
    end.

dirty_read2(Tab, IxKey, Pos) ->
    Ix = get_index_table(Tab, Pos),
    Keys = db_match(Ix, {IxKey, '$1'}),
    r_keys(Keys, Tab, []).

r_keys([[H]|T],Tab,Ack) -> 
    V = mnesia_lib:db_get(Tab, H),
    r_keys(T, Tab, V ++ Ack);
r_keys([], _, Ack) ->
    Ack.
	    

%%%%%%% Creation, Init and deletion routines for index tables
%% We can have several indexes on the same table
%% this can be a fairly costly operation if table is *very* large

tab2filename(Tab, Pos) ->
    mnesia_lib:dir(Tab) ++ "_" ++ integer_to_list(Pos) ++ ".DAT".

tab2tmp_filename(Tab, Pos) ->
    mnesia_lib:dir(Tab) ++ "_" ++ integer_to_list(Pos) ++ ".TMP".
        
init_index(Tab, Storage) ->
    PosList = val({Tab, index}),
    init_indecies(Tab, Storage, PosList).

init_indecies(Tab, Storage, PosList) ->
    case Storage of
	unknown ->
	    ignore;
	disc_only_copies ->
	    init_disc_index(Tab, PosList);
	ram_copies ->
	    make_ram_index(Tab, PosList);
	disc_copies ->
	    make_ram_index(Tab, PosList)
    end.

%% works for both ram and disc indexes

del_index_table(_, unknown, _) ->
    ignore;
del_index_table(Tab, Storage, Pos) ->
    delete_transient_index(Tab, Pos, Storage),
    mnesia_lib:del({Tab, index}, Pos).

del_transient(Tab, Storage) ->
    PosList = val({Tab, index}),
    del_transient(Tab, PosList, Storage).

del_transient(_, [], _) -> done;
del_transient(Tab, [Pos | Tail], Storage) ->
    delete_transient_index(Tab, Pos, Storage),
    del_transient(Tab, Tail, Storage).

delete_transient_index(Tab, Pos, disc_only_copies) ->
    Tag = {Tab, index, Pos},
    mnesia_monitor:unsafe_close_dets(Tag),
    file:delete(tab2filename(Tab, Pos)),
    del_index_info(Tab, Pos), %% Uses val(..)
    mnesia_lib:unset({Tab, {index, Pos}});

delete_transient_index(Tab, Pos, _Storage) ->
    Ixt = val({Tab, {index, Pos}}),
    ?ets_delete_table(Ixt),
    del_index_info(Tab, Pos),
    mnesia_lib:unset({Tab, {index, Pos}}).

%%%%% misc functions for the index create/init/delete functions above

%% assuming that the file exists.
init_disc_index(Tab, []) ->
    done;
init_disc_index(Tab, [Pos | Tail]) when integer(Pos) ->
    Fn = tab2filename(Tab, Pos),
    IxTag = {Tab, index, Pos},
    file:delete(Fn),
    Args = [{file, Fn}, {keypos, 1}, {type, bag}],
    mnesia_monitor:open_dets(IxTag, Args),
    insert_disc_index(Tab, Pos, IxTag, dets:first(Tab)),
    mnesia_lib:set({Tab, {index, Pos}}, IxTag),
    add_index_info(Tab, val({Tab, setorbag}), {Pos, {dets, IxTag}}).

insert_disc_index(Tab, Pos, Ix, '$end_of_table') ->
    done;
insert_disc_index(Tab, Pos, Ix, K) ->
    Vals = dets:lookup(Tab, K),
    insert_disc_index_objects(Ix, Vals, Pos),
    insert_disc_index(Tab, Pos, Ix, mnesia_lib:db_next_key(Tab, K)).

insert_disc_index_objects(Ix, [], Pos) -> 
    done;
insert_disc_index_objects(Ix, [Obj|Tail], Pos) -> 
    O2 = {element(Pos, Obj), element(2, Obj)},
    ok = dets:insert(Ix, O2),
    insert_disc_index_objects(Ix, Tail, Pos).

make_ram_index(_, []) -> 
    done;
make_ram_index(Tab, [Pos | Tail]) ->
    add_ram_index(Tab, Pos),
    make_ram_index(Tab, Tail).

add_ram_index(Tab, Pos) when integer(Pos) ->
    verbose("Creating index for ~w ~n", [Tab]),
    Pat0 = val({Tab, wild_pattern}),
    Pat1 = setelement(2, Pat0, '$1'),
    Pat2 = setelement(Pos, Pat1, '$2'),
    Keys = ?ets_match(Tab, Pat2),
    Index = mnesia_monitor:mktab(mnesia_index, [bag, public]),
    mnesia_lib:set({Tab, {index, Pos}}, Index),
    %%mnesia_lib:add({Tab, index_tables}, {Pos, {ram, Index}}),
    add_index_info(Tab, val({Tab, setorbag}), {Pos, {ram, Index}}),
    insert_ram_index(Index, Keys);
add_ram_index(Tab, snmp) ->
    ok.

insert_ram_index(Index, [[RealKey, IxKey] | Tail]) ->
    ?ets_insert(Index, {IxKey, RealKey}),
    insert_ram_index(Index, Tail);
insert_ram_index(Index, []) -> done.

add_index_info(Tab, Type, IxElem) ->
    Commit = val({Tab, commit_work}),
    case lists:keysearch(index, 1, Commit) of
	false ->
	    Index = #index{setorbag = Type, 
			   pos_list = [IxElem]},
	    %% Check later if mnesia_tm is sensative about the order 
	    mnesia_lib:set({Tab, commit_work}, 
			   mnesia_lib:sort_commit([Index | Commit]));
	{value, Old} ->
	    %% We could check for consistency here
	    Index = Old#index{pos_list = [IxElem | Old#index.pos_list]},
	    NewC = lists:keyreplace(index, 1, Commit, Index),
	    mnesia_lib:set({Tab, commit_work}, 
			   mnesia_lib:sort_commit(NewC))
    end.

del_index_info(Tab, Pos) ->
    Commit = val({Tab, commit_work}),
    case lists:keysearch(index, 1, Commit) of
	false ->
	    %% Something is wrong ignore
	    skip;
	{value, Old} ->
	    case lists:keydelete(Pos, 1, Old#index.pos_list) of
		[] -> 
		    NewC = lists:keydelete(index, 1, Commit),
		    mnesia_lib:set({Tab, commit_work}, 
				   mnesia_lib:sort_commit(NewC));
		New ->
		    Index = Old#index{pos_list = New},
		    NewC = lists:keyreplace(index, 1, Commit, Index),
		    mnesia_lib:set({Tab, commit_work}, 
				   mnesia_lib:sort_commit(NewC))
	    end
    end.

db_put({ram, Ixt}, V) ->
    ?ets_insert(Ixt, V);
db_put({dets, Ixt}, V) ->
    dets:insert(Ixt, V).

db_get({ram, Ixt}, K) ->
    ?ets_lookup(Ixt, K);
db_get({dets, Ixt}, K) ->
    dets:lookup(Ixt, K).

db_match_erase({ram, Ixt}, Pat) ->
    ?ets_match_delete(Ixt, Pat);
db_match_erase({dets, Ixt}, Pat) ->
    dets:match_delete(Ixt, Pat).
    
db_match({ram, Ixt}, Pat) ->
    ?ets_match(Ixt, Pat);
db_match({dets, Ixt}, Pat) ->
    dets:match(Ixt, Pat).
    
get_index_table(Tab, Pos) ->
    get_index_table(Tab,  val({Tab, storage_type}), Pos).

get_index_table(Tab, ram_copies, Pos) ->
    {ram,  val({Tab, {index, Pos}})};
get_index_table(Tab, disc_copies, Pos) ->
    {ram,  val({Tab, {index, Pos}})};
get_index_table(Tab, disc_only_copies, Pos) ->
    {dets, val({Tab, {index, Pos}})};
get_index_table(_Tab, unknown, _Pos) ->
    unknown.

