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
%%%----------------------------------------------------------------------
%%% Purpose : Support tables so large that they need
%%%           to be divided into several fragments.
%%%----------------------------------------------------------------------

%header_doc_include

-module(mnesia_frag).
-behaviour(mnesia_access).

%header_doc_include

%% Callback functions when accessed within an activity
-export([
	 lock/4,
	 write/5, delete/5, delete_object/5,
	 read/5, match_object/5, all_keys/4,
	 index_match_object/6, index_read/6,
	 table_info/4
       ]).

-export([
	 change_table_frag/2,
	 remove_node/2,
	 expand_cstruct/1,
	 lookup_frag_hash/1,
	 lookup_foreigners/1,
	 frag_names/1,
	 set_frag_hash/2
	]).

-include("mnesia.hrl").

%% This record is a table property for base tables only
%% Default record is a pattern when matching for foreigners
-record(frag_hash,
	{foreign_key = {Tab, '_'},
	 n_fragments = '_',
	 next_n_to_split = '_',
	 n_doubles = '_'}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access functions

%impl_doc_include

%% Callback functions which provides transparent
%% access of fragmented tables from any activity
%% access context.

lock(ActivityId, Opaque, {table , Tab}, LockKind) ->
    case frag_names(Tab) of
	[Tab] ->
	    mnesia:lock(ActivityId, Opaque, {table, Tab}, LockKind);	    
	Frags ->
	    DeepNs = [mnesia:lock(ActivityId, Opaque, {table, F}, LockKind) ||
			 F <- Frags],
	    mnesia_lib:uniq(lists:append(DeepNs))
    end;

lock(ActivityId, Opaque, LockItem, LockKind) ->
    mnesia:lock(ActivityId, Opaque, LockItem, LockKind).

write(ActivityId, Opaque, Tab, Rec, LockKind) ->
    Frag = record_to_frag_name(Tab, Rec),
    mnesia:write(ActivityId, Opaque, Frag, Rec, LockKind).

delete(ActivityId, Opaque, Tab, Key, LockKind) ->
    Frag = key_to_frag_name(Tab, Key),
    mnesia:delete(ActivityId, Opaque, Frag, Key, LockKind).

delete_object(ActivityId, Opaque, Tab, Rec, LockKind) ->
    Frag = record_to_frag_name(Tab, Rec),
    mnesia:delete_object(ActivityId, Opaque, Frag, Rec, LockKind).

read(ActivityId, Opaque, Tab, Key, LockKind) ->
    Frag = key_to_frag_name(Tab, Key),
    mnesia:read(ActivityId, Opaque, Frag, Key, LockKind).

match_object(ActivityId, Opaque, Tab, Pat, LockKind) ->
    Match = [mnesia:match_object(ActivityId, Opaque, Frag, Pat, LockKind)
	     || Frag <- frag_names(Tab)],
    lists:flatten(Match).

all_keys(ActivityId, Opaque, Tab, LockKind) ->
    Match = [mnesia:all_keys(ActivityId, Opaque, Frag, LockKind)
	     || Frag <- frag_names(Tab)],
    lists:flatten(Match).

index_match_object(ActivityId, Opaque, Tab, Pat, Attr, LockKind) ->
    Match =
	[mnesia:index_match_object(ActivityId, Opaque, Frag, Pat, Attr, LockKind)
	 || Frag <- frag_names(Tab)],
    lists:flatten(Match).

index_read(ActivityId, Opaque, Tab, Key, Attr, LockKind) ->
    Match =
	[mnesia:index_read(ActivityId, Opaque, Frag, Key, Attr, LockKind)
	     || Frag <- frag_names(Tab)],
    lists:flatten(Match).

table_info(ActivityId, Opaque, {Tab, Key}, Item) ->
    Frag = key_to_frag_name(Tab, Key),
    table_info2(ActivityId, Opaque, Tab, Frag, Item);
table_info(ActivityId, Opaque, Tab, Item) ->
    table_info2(ActivityId, Opaque, Tab, Tab, Item).

table_info2(ActivityId, Opaque, Tab, Frag, Item) ->
    case Item of
	size ->
	    SumFun = fun({_, Size}, Acc) -> Acc + Size end,
	    lists:foldl(SumFun, 0, frag_size(ActivityId, Opaque, Tab));
	memory ->
	    SumFun = fun({_, Size}, Acc) -> Acc + Size end,
	    lists:foldl(SumFun, 0, frag_memory(ActivityId, Opaque, Tab));
	base_table ->
	    lookup_prop(Tab, base_table);
	node_pool ->
	    lookup_prop(Tab, node_pool);
	n_fragments ->
	    FH = lookup_frag_hash(Tab),
	    FH#frag_hash.n_fragments;
	foreign_key ->
	    FH = lookup_frag_hash(Tab),
	    FH#frag_hash.foreign_key;
	foreigners ->
	    lookup_foreigners(Tab);
	n_ram_copies ->
	    length(val({Tab, ram_copies}));
	n_disc_copies ->
	    length(val({Tab, disc_copies}));
	n_disc_only_copies ->
	    length(val({Tab, disc_only_copies}));

	frag_names ->
	    frag_names(Tab);
	frag_dist ->
	    frag_dist(Tab);
	frag_size ->
	    frag_size(ActivityId, Opaque, Tab);
	frag_memory ->
	    frag_memory(ActivityId, Opaque, Tab);
	_ ->
	    mnesia:table_info(ActivityId, Opaque, Frag, Item)
    end.
%impl_doc_include

frag_size(ActivityId, Opaque, Tab) ->
    [{F, remote_table_info(ActivityId, Opaque, F, size)} || F <- frag_names(Tab)].

frag_memory(ActivityId, Opaque, Tab) ->
    [{F, remote_table_info(ActivityId, Opaque, F, memory)} || F <- frag_names(Tab)].

    
  
remote_table_info(ActivityId, Opaque, Tab, Item) ->
    N = val({Tab, where_to_read}),
    case rpc:call(N, mnesia, table_info, [ActivityId, Opaque, Tab, Item]) of
	{badrpc, _} ->
	    mnesia:abort({no_exists, Tab, Item});
	Info ->
	    Info
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns a list of cstructs

expand_cstruct(Cs) ->
    expand_cstruct(Cs, create).
    
expand_cstruct(Cs, Mode) ->
    Tab = Cs#cstruct.name,
    Props = Cs#cstruct.frag_properties,
    mnesia_schema:verify({alt, [nil, list]}, mnesia_lib:etype(Props),
			 {badarg, Tab, Props}), 
    %% Verify keys
    ValidKeys = [foreign_key, n_fragments, node_pool,
		 n_ram_copies, n_disc_copies, n_disc_only_copies],
    Keys = mnesia_schema:check_keys(Tab, Props, ValidKeys),
    mnesia_schema:check_duplicates(Tab, Keys),

    %% Pick fragmentation props
    ForeignKey = mnesia_schema:pick(Tab, foreign_key, Props, undefined),
    {ForeignKey2, N, Pool, DefaultNR, DefaultND, DefaultNDO} =
	pick_props(Tab, Cs, ForeignKey),

    %% Verify node_pool
    BadPool = {bad_type, Tab, {node_pool, Pool}},
    mnesia_schema:verify(list, mnesia_lib:etype(Pool), BadPool),
    NotAtom = fun(A) when atom(A) -> false;
		 (A) -> true
	      end,
    mnesia_schema:verify([], [P || P <- Pool, NotAtom(P)], BadPool),

    FH = #frag_hash{foreign_key = ForeignKey2,
		    n_fragments = 1,
		    next_n_to_split = 1,
		    n_doubles = 0
		   },
    NR = mnesia_schema:pick(Tab, n_ram_copies, Props, 0),
    ND = mnesia_schema:pick(Tab, n_disc_copies, Props, 0),
    NDO = mnesia_schema:pick(Tab, n_disc_only_copies, Props, 0),
    
    PosInt = fun(I) when integer(I), I >= 0 -> true;
		(I) -> false
	     end,
    mnesia_schema:verify(true, PosInt(NR),
			 {bad_type, Tab, {n_ram_copies, NR}}),
    mnesia_schema:verify(true, PosInt(ND),
			 {bad_type, Tab, {n_disc_copies, ND}}),
    mnesia_schema:verify(true, PosInt(NDO),
			 {bad_type, Tab, {n_disc_only_copies, NDO}}),
    
    %% Verify n_fragments
    Cs2 = verify_n_fragments(N, Cs, Mode),
    
    if
	NR == 0, ND == 0, NDO == 0 ->
	    do_expand_cstruct(Cs2, FH, N, Pool, DefaultNR, DefaultND, DefaultNDO, Mode);
	true ->
	    do_expand_cstruct(Cs2, FH, N, Pool, NR, ND, NDO, Mode)
    end.
	    
do_expand_cstruct(Cs, FH, N, Pool, NR, ND, NDO, Mode) ->
    Tab = Cs#cstruct.name,
    
    LC = Cs#cstruct.local_content,
    mnesia_schema:verify(false, LC,
			 {combine_error, Tab, {local_content, LC}}),

    Snmp = Cs#cstruct.snmp,
    mnesia_schema:verify([], Snmp,
			 {combine_error, Tab, {snmp, Snmp}}),

    %% Add empty fragments
    CommonProps = [{base_table, Tab}],
    Cs2 = Cs#cstruct{frag_properties = lists:sort(CommonProps)},
    expand_frag_cstructs(N, NR, ND, NDO, Cs2, Pool, Pool, FH, Mode).

verify_n_fragments(N, Cs, Mode) when integer(N), N > 0 ->
    case Mode of
	create ->
	    Cs#cstruct{ram_copies = [],
		       disc_copies = [],
		       disc_only_copies = []};
	activate  ->
	    Reason = {combine_error, Cs#cstruct.name, {n_fragments, N}},
	    mnesia_schema:verify(1, N, Reason),
	    Cs
    end;
verify_n_fragments(N, Cs, Mode) ->
    mnesia:abort({bad_type, Cs#cstruct.name, {n_fragments, N}}).

pick_props(Tab, Cs, {ForeignTab, Attr}) ->
    mnesia_schema:verify(true, ForeignTab /= Tab,
			 {combine_error, Tab, {ForeignTab, Attr}}),
    Props = Cs#cstruct.frag_properties,
    Attrs = Cs#cstruct.attributes,

    ForeignKey  = lookup_prop(ForeignTab, foreign_key),
    ForeignN    = lookup_prop(ForeignTab, n_fragments),
    ForeignPool = lookup_prop(ForeignTab, node_pool),
    N           = mnesia_schema:pick(Tab, n_fragments, Props,  ForeignN),
    Pool        = mnesia_schema:pick(Tab, node_pool, Props, ForeignPool),
    
    mnesia_schema:verify(ForeignN, N, 
			 {combine_error, Tab, {n_fragments, N},
			  ForeignTab, {n_fragments, ForeignN}}),

    mnesia_schema:verify(ForeignPool, Pool, 
			 {combine_error, Tab, {node_pool, Pool},
			  ForeignTab, {node_pool, ForeignPool}}),

    mnesia_schema:verify(undefined, ForeignKey,
			 {combine_error, Tab,
			  "Multiple levels of foreign_key dependencies",
			  {ForeignTab, Attr}, ForeignKey}),

    Key = {ForeignTab, mnesia_schema:attr_to_pos(Attr, Attrs)},
    DefaultNR = length(val({ForeignTab, ram_copies})), 
    DefaultND = length(val({ForeignTab, disc_copies})), 
    DefaultNDO = length(val({ForeignTab, disc_only_copies})),
    {Key, N, Pool, DefaultNR, DefaultND, DefaultNDO};
pick_props(Tab, Cs, undefined) ->
    Props = Cs#cstruct.frag_properties,
    DefaultN = 1,
    DefaultPool = mnesia:system_info(db_nodes),
    N    = mnesia_schema:pick(Tab, n_fragments, Props,  DefaultN),
    Pool = mnesia_schema:pick(Tab, node_pool, Props, DefaultPool),
    DefaultNR = 1,
    DefaultND = 0,
    DefaultNDO = 0,
    {undefined, N, Pool, DefaultNR, DefaultND, DefaultNDO};
pick_props(Tab, _Cs, BadKey) ->
    mnesia:abort({bad_type, Tab, {foreign_key, BadKey}}).

expand_frag_cstructs(N, NR, ND, NDO, CommonCs, Dist, Pool, FH, Mode)
  when N > 1, Mode == create ->
    Frag = n_to_frag_name(CommonCs#cstruct.name, N),
    Cs = CommonCs#cstruct{name = Frag},
    {Cs2, RevModDist, RestDist} = set_frag_nodes(NR, ND, NDO, Cs, Dist, []),
    ModDist = lists:reverse(RevModDist),
    Dist2 = rearrange_dist(Cs, ModDist, RestDist, Pool),
    FH2 = adjust_before_split(FH), % Adjusts backwards, but it doesn't matter.
    CsList = expand_frag_cstructs(N - 1, NR, ND, NDO, CommonCs, Dist2, Pool, FH2, Mode),
    [Cs2 | CsList];
expand_frag_cstructs(1, NR, ND, NDO, CommonCs, Dist, Pool, FH, Mode) ->
    BaseProps = CommonCs#cstruct.frag_properties ++  
	[{n_fragments, FH#frag_hash.n_fragments},
	 {foreign_key, FH#frag_hash.foreign_key},
	 {next_n_to_split, FH#frag_hash.next_n_to_split},
	 {n_doubles, FH#frag_hash.n_doubles},
	 {node_pool, Pool}
	],
    BaseCs = CommonCs#cstruct{frag_properties = lists:sort(BaseProps)},
    case Mode of
	activate ->
	    [BaseCs];
	create ->
	    {BaseCs2, _, _} = set_frag_nodes(NR, ND, NDO, BaseCs, Dist, []),
	    [BaseCs2]
    end.
    
set_frag_nodes(NR, ND, NDO, Cs, [Head | Tail], Acc) when NR > 0 ->
    Pos = #cstruct.ram_copies,
    {Cs2, Head2} = set_frag_node(Cs, Pos, Head),
    set_frag_nodes(NR - 1, ND, NDO, Cs2, Tail, [Head2 | Acc]); 
set_frag_nodes(NR, ND, NDO, Cs, [Head | Tail], Acc) when ND > 0 ->
    Pos = #cstruct.disc_copies,
    {Cs2, Head2} = set_frag_node(Cs, Pos, Head),
    set_frag_nodes(NR, ND - 1, NDO, Cs2, Tail, [Head2 | Acc]); 
set_frag_nodes(NR, ND, NDO, Cs, [Head | Tail], Acc) when NDO > 0 ->
    Pos = #cstruct.disc_only_copies,
    {Cs2, Head2} = set_frag_node(Cs, Pos, Head),
    set_frag_nodes(NR, ND, NDO - 1, Cs2, Tail, [Head2 | Acc]); 
set_frag_nodes(0, 0, 0, Cs, RestDist, ModDist) ->
    {Cs, ModDist, RestDist};
set_frag_nodes(_, _, _, Cs, [], _) ->
    mnesia:abort({combine_error,  Cs#cstruct.name, "Too few nodes in node_pool"}).

set_frag_node(Cs, Pos, Head) ->
    Ns = element(Pos, Cs),
    {Node, Count2} =  
	case Head of
	    {N, Count} when atom(N), integer(Count), Count >= 0 ->
		{N, Count + 1};
	    N when atom(N) ->
		{N, 1};
	    BadNode ->
		mnesia:abort({bad_type, Cs#cstruct.name, BadNode})
	end,
    Cs2 = setelement(Pos, Cs, [Node | Ns]),
    {Cs2, {Node, Count2}}.

rearrange_dist(Cs, [{Node, Count} | ModDist], Dist, Pool) ->
    Dist2 = insert_dist(Cs, Node, Count, Dist, Pool),
    rearrange_dist(Cs, ModDist, Dist2, Pool);
rearrange_dist(Cs, [], Dist, _) ->
    Dist.

insert_dist(Cs, Node, Count, [Head | Tail], Pool) ->
    case Head of
	{Node2, Count2} when atom(Node2), integer(Count2), Count2 >= 0 ->
	    case node_diff(Node, Count, Node2, Count2, Pool) of
		less ->
		    [{Node, Count}, Head | Tail];
		greater ->
		    [Head | insert_dist(Cs, Node, Count, Tail, Pool)]
	    end;
	Node2 when atom(Node2) -> 
	    insert_dist(Cs, Node, Count, [{Node2, 0} | Tail], Pool);
	BadNode ->
	    mnesia:abort({bad_type, Cs#cstruct.name, BadNode})
    end;
insert_dist(Cs, Node, Count, [], Pool) ->
    [{Node, Count}];
insert_dist(Cs, Node, Count, Dist, Pool) ->
    mnesia:abort({bad_type, Dist}).
    
node_diff(Node, Count, Node2, Count2, Pool) when Count < Count2 ->
    less;
node_diff(Node, Count, Node2, Count2, Pool) when Count == Count2 ->
    Pos = list_pos(Node, Pool, 1),
    Pos2 = list_pos(Node2, Pool, 1),
    if
	Pos < Pos2 ->
	    less;
	Pos > Pos2 ->
	    greater
    end;
node_diff(Node, Count, Node2, Count2, Pool) when Count > Count2 ->
    greater.

%% Returns position of element in list
list_pos(H,  [H | T], Pos) ->
    Pos;
list_pos(E,  [H | T], Pos) ->
    list_pos(E,  T, Pos + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Switch function for changing of table fragmentation
%%
%% Returns a list of lists of schema ops 

change_table_frag(Tab, {activate, FragProps}) ->
    make_activate(Tab, FragProps);
change_table_frag(Tab, deactivate) ->
    make_deactivate(Tab);
change_table_frag(Tab,  {add_frag, SortedNodes}) ->
    make_multi_add_frag(Tab, SortedNodes);
change_table_frag(Tab,  del_frag) ->
    make_multi_del_frag(Tab);
change_table_frag(Tab,  {add_node, Node}) ->
    make_multi_add_node(Tab, Node);
change_table_frag(Tab,  {del_node, Node}) ->
    make_multi_del_node(Tab, Node);
change_table_frag(Tab,  Change) ->
    mnesia:abort({bad_type, Tab, Change}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Turn a normal table into a fragmented table
%% 
%% The storage type must be the same on all nodes

make_activate(Tab, Props) ->
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),
    case Cs#cstruct.frag_properties of
	[] ->
	    Cs2 = Cs#cstruct{frag_properties = Props},
	    [Cs3] = expand_cstruct(Cs2, activate),
	    TabDef = mnesia_schema:cs2list(Cs3),
	    Op = {op, change_table_frag, activate, TabDef},
	    [[Op]];
	BadProps ->
	    mnesia:abort({already_exists, Tab, {frag_properties, BadProps}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Turn a table into a normal defragmented table

make_deactivate(Tab) ->
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),
    Foreigners = lookup_foreigners(Tab),
    BaseTab = lookup_prop(Tab, base_table),
    FH = lookup_frag_hash(Tab),
    if
	BaseTab /= Tab ->
	    mnesia:abort({combine_error, Tab, "Not a base table"});
	Foreigners /= [] ->
	    mnesia:abort({combine_error, Tab, "Too many foreigners", Foreigners});
	FH#frag_hash.n_fragments > 1 ->
	    mnesia:abort({combine_error, Tab, "Too many fragments"});
	true ->
	    Cs2 = Cs#cstruct{frag_properties = []},
	    TabDef = mnesia_schema:cs2list(Cs2),
	    Op = {op, change_table_frag, deactivate, TabDef},
	    [[Op]]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add a fragment to a fragmented table  and fill it with half of
%% the records from one of the old fragments
    
make_multi_add_frag(Tab, SortedNs) when list(SortedNs) ->
    verify_multi(Tab),
    Ops = make_add_frag(Tab, SortedNs),

    %% Propagate to foreigners
    MoreOps = [make_add_frag(T, SortedNs) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps]; 
make_multi_add_frag(Tab, SortedNs) ->
    mnesia:abort({bad_type, Tab, SortedNs}).

verify_multi(Tab) ->
    FH = lookup_frag_hash(Tab),
    ForeignKey = FH#frag_hash.foreign_key,
    mnesia_schema:verify(undefined, ForeignKey, 
			 {combine_error, Tab, 
			  "Op only allowed via foreign table",
			  {foreign_key, ForeignKey}}).
    
make_add_frag(Tab, SortedNs) ->
    FH = lookup_frag_hash(Tab),
    SplitN = FH#frag_hash.next_n_to_split,
    SplitTab = n_to_frag_name(Tab, SplitN),
    mnesia_schema:get_tid_ts_and_lock(SplitTab, write),

    FH2 = adjust_before_split(FH),
    NewN = FH2#frag_hash.n_fragments,
    NewTab = n_to_frag_name(Tab, NewN),
    
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),

    NR = length(Cs#cstruct.ram_copies), 
    ND = length(Cs#cstruct.disc_copies), 
    NDO = length(Cs#cstruct.disc_only_copies),
    Pool = lookup_prop(Tab, node_pool),
    NewCs = Cs#cstruct{name = NewTab,
		       frag_properties = [{base_table, Tab}],
		       ram_copies = [],
		       disc_copies = [],
		       disc_only_copies = []},
    {NewCs2, _, _} = set_frag_nodes(NR, ND, NDO, NewCs, SortedNs, []),
    [NewOp] = mnesia_schema:make_create_table(NewCs2),
    SplitOps = split(Tab, FH2, SplitTab, NewTab, SplitN),

    Cs2 = replace_frag_hash(Cs, FH2),
    TabDef = mnesia_schema:cs2list(Cs2),
    BaseOp = {op, change_table_frag, {add_frag, SortedNs}, TabDef},

    [BaseOp, NewOp | SplitOps].

replace_frag_hash(Cs, FH) ->
    N = FH#frag_hash.n_fragments,
    Next = FH#frag_hash.next_n_to_split,
    Doubles = FH#frag_hash.n_doubles,
    Props = Cs#cstruct.frag_properties,
    Props2 = lists:keyreplace(n_fragments, 1, Props, {n_fragments, N}),
    Props3 = lists:keyreplace(next_n_to_split, 1, Props2, {next_n_to_split, Next}),
    Props4 = lists:keyreplace(n_doubles, 1, Props3, {n_doubles, Doubles}),
    Cs#cstruct{frag_properties = Props4}.

%% Adjust table info before split
adjust_before_split(FH) ->
    P = FH#frag_hash.next_n_to_split + 1,
    L = FH#frag_hash.n_doubles,
    N = FH#frag_hash.n_fragments + 1,
    P2= trunc(math:pow(2, L)) + 1,
    if
	P == P2 ->
	    FH#frag_hash{n_doubles = L + 1,
				    next_n_to_split = 1,
				    n_fragments = N};
	true ->
	    FH#frag_hash{next_n_to_split = P,
				    n_fragments = N}
    end.

split(Tab, FH, SplitFrag, NewFrag, SplitN) ->
    Pat = mnesia:table_info(SplitFrag, wild_pattern),
    {_Mod, Tid, Ts} = mnesia_schema:get_tid_ts_and_lock(Tab, none),
    Recs = mnesia:match_object(Tid, Ts, SplitFrag, Pat, read),
    do_split(FH, SplitFrag,NewFrag, SplitN, Recs).

%% Perform the split of the table
do_split(FH, SplitFrag, NewFrag, SplitN, [Rec | Recs]) ->
    Pos = key_pos(FH),
    HashKey = element(Pos, Rec),
    case key_to_n(FH, HashKey) of
	N when N == SplitN ->
	    do_split(FH, SplitFrag, NewFrag, SplitN, Recs);
	_ ->
	    Key = element(2, Rec),
	    SplitOid = {SplitFrag, Key},
	    [{op, rec, unknown, {{NewFrag, Key}, [Rec], write}}, 
	     {op, rec, unknown, {SplitOid, [SplitOid], delete}} |
	     do_split(FH, SplitFrag, NewFrag, SplitN, Recs)]
    end;
do_split(FH, SplitFrag, NewFrag, SplitN, []) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delete a fragment from a fragmented table
%% and merge its records with an other fragment
    
make_multi_del_frag(Tab) ->
    verify_multi(Tab),
    Ops = make_del_frag(Tab),

    %% Propagate to foreigners
    MoreOps = [make_del_frag(T) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps].

make_del_frag(Tab) ->
    FH = lookup_frag_hash(Tab),
    LastN = FH#frag_hash.n_fragments,
    if
	LastN > 1 ->
	    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
	    mnesia_schema:ensure_active(Cs),
	    LastFrag = n_to_frag_name(Tab, LastN),
	    FH2 = adjust_before_merge(FH),

	    MergeN = FH2#frag_hash.next_n_to_split,
	    MergeFrag = n_to_frag_name(Tab, MergeN),
	    mnesia_schema:get_tid_ts_and_lock(Tab, write),
	    mnesia_schema:get_tid_ts_and_lock(MergeFrag, write),
	    mnesia_schema:get_tid_ts_and_lock(LastFrag, write),

	    MergeOps = merge(Tab, FH2, MergeFrag, LastFrag),
	    [LastOp] = mnesia_schema:make_delete_table(LastFrag, single_frag),
	    Cs2 = replace_frag_hash(Cs, FH2),
	    TabDef = mnesia_schema:cs2list(Cs2),
	    BaseOp = {op, change_table_frag, del_frag, TabDef},
	    [BaseOp, LastOp | MergeOps];
	true ->
	    mnesia:abort({no_exists, Tab})
    end.

%% Adjust tab info before merge
adjust_before_merge(FH) ->
    P = FH#frag_hash.next_n_to_split - 1,
    L = FH#frag_hash.n_doubles,
    N = FH#frag_hash.n_fragments - 1,
    if
	P < 1 ->
	    L2 = L - 1,
	    FH#frag_hash{n_doubles = L2,
				    next_n_to_split = trunc(math:pow(2, L2)),
				    n_fragments = N};
	true ->
	    FH#frag_hash{next_n_to_split = P,
				    n_fragments = N}
    end.

merge(Tab, FH, MergeFrag, LastFrag) ->
    Pat = mnesia:table_info(LastFrag, wild_pattern),
    {_Mod, Tid, Ts} = mnesia_schema:get_tid_ts_and_lock(Tab, none),
    Recs = mnesia:match_object(Tid, Ts, LastFrag, Pat, read),
    do_merge(MergeFrag, LastFrag, Recs).

%% Perform the merge of the table
do_merge(MergeFrag, LastFrag, [Rec | Recs]) ->
    Key = element(2, Rec),
    LastOid = {LastFrag, Key},
    [{op, rec, unknown, {{MergeFrag, Key}, [Rec], write}} |
     do_merge(MergeFrag, LastFrag, Recs)];
do_merge(_MergeFrag, _LastFrag, []) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add a node to the node pool of a fragmented table
    
make_multi_add_node(Tab, Node)  ->
    verify_multi(Tab),
    Ops = make_add_node(Tab, Node),

    %% Propagate to foreigners
    MoreOps = [make_add_node(T, Node) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps].
    
make_add_node(Tab, Node) when atom(Node)  ->
    Pool = lookup_prop(Tab, node_pool),
    case lists:member(Node, Pool) of
	false ->
	    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
	    Pool2 = Pool ++ [Node],
	    Props = Cs#cstruct.frag_properties,
	    Props2 = lists:keyreplace(node_pool, 1, Props, {node_pool, Pool2}),
	    Cs2 = Cs#cstruct{frag_properties = Props2},
	    TabDef = mnesia_schema:cs2list(Cs2),
	    Op = {op, change_table_frag, {add_node, Node}, TabDef},
	    [Op];
	true ->
	    mnesia:abort({already_exists, Tab, Node})
    end;
make_add_node(Tab, Node) ->
    mnesia:abort({bad_type, Tab, Node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delet a node from the node pool of a fragmented table

make_multi_del_node(Tab, Node)  ->
    verify_multi(Tab),
    Ops = make_del_node(Tab, Node),

    %% Propagate to foreigners
    MoreOps = [make_del_node(T, Node) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps].
    
make_del_node(Tab, Node) when atom(Node) ->
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),
    Pool = lookup_prop(Tab, node_pool),
    case lists:member(Node, Pool) of
	true ->
	    Pool2 = Pool -- [Node],
	    Props = lists:keyreplace(node_pool, 1, Cs#cstruct.frag_properties, {node_pool, Pool2}),
	    Cs2 = Cs#cstruct{frag_properties = Props},
	    TabDef = mnesia_schema:cs2list(Cs2),
	    Op = {op, change_table_frag, {del_node, Node}, TabDef},
	    [Op];
	false ->
	    mnesia:abort({no_exists, Tab, Node})
    end;
make_del_node(Tab, Node) ->
    mnesia:abort({bad_type, Tab, Node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special case used to remove all references to a node during 
%% mnesia:del_table_copy(schema, Node)

remove_node(Node, Cs) ->
    Tab = Cs#cstruct.name,
    case is_top_frag(Tab) of
	false ->
	    {Cs, false};
	true -> 
	    Pool = lookup_prop(Tab, node_pool),
	    case lists:member(Node, Pool) of
		true ->
		    Pool2 = Pool -- [Node],
		    Props = lists:keyreplace(node_pool, 1, 
					     Cs#cstruct.frag_properties, 
					     {node_pool, Pool2}),
		    {Cs#cstruct{frag_properties = Props}, true};
		false ->
		    {Cs, false}
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', Reason} -> mnesia_lib:other_val(Var, Reason); 
	Value -> Value 
    end.

set_frag_hash(Tab, Props) ->
    case props_to_frag_hash(Tab, Props) of
	FH when record(FH, frag_hash) ->
	    mnesia_lib:set({Tab, frag_hash}, FH);
	no_hash ->
	    mnesia_lib:unset({Tab, frag_hash})
    end.

props_to_frag_hash(Tab, []) ->
    no_hash;
props_to_frag_hash(Tab, Props) ->
    case catch mnesia_schema:pick(Tab, base_table, Props, must) of
	T when T == Tab ->
	    Foreign = mnesia_schema:pick(Tab, foreign_key, Props, must),
	    N = mnesia_schema:pick(Tab, n_fragments, Props, must),
	    Next = mnesia_schema:pick(Tab, next_n_to_split, Props, must),
	    Doubles = mnesia_schema:pick(Tab, n_doubles, Props, must),
	    #frag_hash{foreign_key = Foreign,
		       n_fragments = N,
		       next_n_to_split = Next,
		       n_doubles = Doubles
		      };
	_ ->
	    no_hash
    end.

lookup_prop(Tab, Prop) ->
    Props = val({Tab, frag_properties}),
    case lists:keysearch(Prop, 1,  Props) of
	{value, {Prop, Val}} ->
	    Val;
	false ->
	    mnesia:abort({no_exists, Tab, Prop, {frag_properties, Props}})
    end.

lookup_frag_hash(Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	FH when record(FH, frag_hash) ->
	    FH;
	{'EXIT', _} ->
	    mnesia:abort({no_exists, Tab, frag_properties, frag_hash})
    end.

is_top_frag(Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    false;
	_ -> 
	    [] == lookup_foreigners(Tab)
    end.

%% Returns a list of tables
lookup_foreigners(Tab) ->
    [T || [T] <- ?ets_match(mnesia_gvar, {{'$1', frag_hash}, #frag_hash{}})].

%% Returns name of fragment table
record_to_frag_name(Tab, Rec) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    Tab;
	FH ->
	    Pos = key_pos(FH),
	    Key = element(Pos, Rec),
	    N = key_to_n(FH, Key),
	    n_to_frag_name(Tab, N)
    end.

key_pos(FH) ->
    case FH#frag_hash.foreign_key of
	undefined -> 2;
	{_ForeignTab, Pos} -> Pos
    end.
    
%% Returns name of fragment table
key_to_frag_name({Tab, ForeignKey}, Key) ->
    FH = val({Tab, frag_hash}),
    case FH#frag_hash.foreign_key of
	{ForeignTab, _Pos} ->
	    N = key_to_n(FH, ForeignKey),
	    n_to_frag_name(Tab, N);
	undefined ->
	    mnesia:abort({combine_error, Tab, frag_properties,
			  {foreign_key, undefined}})
    end;
key_to_frag_name(Tab, Key) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    Tab;
	FH ->
	    N = key_to_n(FH, Key),
	    n_to_frag_name(Tab, N)
    end.

%% Returns name of fragment table
n_to_frag_name(Tab, 1) ->
    Tab;
n_to_frag_name(Tab, N) ->
    list_to_atom(atom_to_list(Tab) ++ "_frag" ++ integer_to_list(N)).

%% Returns fragment number
key_to_n(FH, Key) ->
    L = FH#frag_hash.n_doubles,
    A = erlang:hash(Key, trunc(math:pow(2, L))),
    P = FH#frag_hash.next_n_to_split,
    if
	A < P ->
	    erlang:hash(Key, trunc(math:pow(2, L + 1)));
	true ->
	    A
    end.

%% Returns a list of frament table names
frag_names(Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    [Tab];
	FH ->
	    N = FH#frag_hash.n_fragments,
	    frag_names(Tab, N, [])
    end.

frag_names(Tab, 1, Acc) ->
    [Tab | Acc];
frag_names(Tab, N, Acc) ->
    Frag = n_to_frag_name(Tab, N),
    frag_names(Tab, N - 1, [Frag | Acc]).

%% Returns a list of {Node, FragCount} tuples
%% sorted on FragCounts
frag_dist(Tab) ->
    Pool = lookup_prop(Tab, node_pool),
    Dist = [{good, Node, 0} || Node <- Pool],
    Dist2 = count_frag(frag_names(Tab), Dist),
    sort_dist(Dist2).

count_frag([Frag | Frags], Dist) ->
    Dist2 =  incr_nodes(val({Frag, ram_copies}), Dist),
    Dist3 =  incr_nodes(val({Frag, disc_copies}), Dist2),
    Dist4 =  incr_nodes(val({Frag, disc_only_copies}), Dist3),
    count_frag(Frags, Dist4);
count_frag([], Dist) ->
    Dist.

incr_nodes([Node | Nodes], Dist) ->
    Dist2 = incr_node(Node, Dist),
    incr_nodes(Nodes, Dist2);
incr_nodes([], Dist) ->
    Dist.

incr_node(Node, [{Kind, Node, Count} | Tail]) ->
    [{Kind, Node, Count + 1} | Tail];
incr_node(Node, [Head | Tail]) ->
    [Head | incr_node(Node, Tail)];
incr_node(Node, []) ->
    [{bad, Node, 1}].

%% Sorts dist according in decreasing count order
sort_dist(Dist) -> 
    Dist2 = deep_dist(Dist, []),
    Dist3 = lists:keysort(1, Dist2),
    shallow_dist(Dist3).

deep_dist([Head | Tail], Deep) ->
    {Kind, Node, Count} = Head,
    {Tag, Same, Other} = pick_count(Kind, Count, [Head | Tail]),
    deep_dist(Other, [{Tag, Same} | Deep]);
deep_dist([], Deep) ->
    Deep.

pick_count(Kind, Count, [{Kind2, Node2, Count2} | Tail]) ->
    Head = {Node2, Count2},
    {_, Same, Other} = pick_count(Kind, Count, Tail),
    if
	Kind == bad ->
	    {bad, [Head | Same], Other};
	Kind2 == bad ->
	    {Count, Same, [{Kind2, Node2, Count2} | Other]};
	Count == Count2 ->
	    {Count, [Head | Same], Other};
	true ->
	    {Count, Same, [{Kind2, Node2, Count2} | Other]}
    end;
pick_count(Kind, Count, []) ->
    {Count, [], []}.

shallow_dist([{Tag, Shallow} | Deep]) ->
    Shallow ++ shallow_dist(Deep);
shallow_dist([]) ->
    [].
