%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			HASH TABLES IN ERLANG
%
% These use our declarative vectors.
%
% Provides:
%   init(Xs{,Options}): initializes a hash table with Xs as {Key,Value} pairs.
%      Options is a list of pairs {Option_name,Option_value}.
%      If they are not given, a "good" set of values is used.
%
%   lookup(Key,HT): returns {found,Value} or not_found.
%
%   insert(Key,Value,HT): if Key exists in HT, throw exception.
%      Otherwise, return new hash table.
%
%   update(Key,Value,HT): as insert/3, but replaces old value if
%      Key is present.
%
%   list(HT): returns a (not necessarily ordered) list of {Key,Value} pairs.
%
% Representation of hash table found in hash.hrl.
%
% At present, table is not rehashed when it becomes too full.

-define(vectors,hipe_pure_vectors).

-module(hipe_pure_hash).
-include("hash.hrl").
-export([init/1,init/2,
	 empty/0,empty/1,
	 lookup/2,
	 insert/3,
	 update/3,
	 delete/2,
	 list/1,
	 rehash/1,
	 join/2
	 ]).
-compile(export_all).

% -define(trace,true).
-ifndef(trace).
-define(ENTER(P,N),true).
-else.
-define(ENTER(P,N),io:format('enter ~w/~w~n',[P,N])).
-endif.

init(Xs) ->
    update_all(Xs,empty()).

init(Xs,Opts) ->
    update_all(Xs,empty_w_opts(Opts)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert(K,V,HT) ->
    HT1 = insert_raw(K,V,HT),
    rehash(inc_item_count(1,HT1)).

delete(K,HT) ->
    {Decr,HT1} = delete_raw(K,HT),
    rehash(dec_item_count(Decr,HT1)).

update(K,V,HT) ->
    {Incr,HT1} = update_raw(K,V,HT),
    rehash(inc_item_count(Incr,HT1)).

lookup(K,HT) ->
    Bucket = get_bucket(K,HT),
    search_bucket(K,Bucket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inc_item_count(N,HT) ->
    HT#hashtable{occupancy=HT#hashtable.occupancy+N}.

dec_item_count(N,HT) ->
    inc_item_count(-N,HT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_raw(K,V,HT) ->
    ?ENTER(insert_raw,3),
    Bucket = get_bucket(K,HT),
    NewBucket = insert_bucket(K,V,Bucket),
    set_bucket(K,NewBucket,HT).

update_raw(K,V,HT) ->
    Bucket = get_bucket(K,HT),
    {Incr,NewBucket} = update_bucket(K,V,Bucket),
    {Incr,set_bucket(K,NewBucket,HT)}.

delete_raw(K,HT) ->
    Bucket = get_bucket(K,HT),
    {Decr,NewBucket} = delete_bucket(K,Bucket),
    {Decr,set_bucket(K,NewBucket,HT)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_all([],HT) -> HT;
insert_all([{K,V}|Xs],HT) ->
    insert_all(Xs,insert(K,V,HT)).

update_all([],HT) -> HT;
update_all([{K,V}|Xs],HT) ->
    update_all(Xs,update(K,V,HT)).

lookup_all([],HT) -> HT;
lookup_all([{K,V}|Xs],HT) ->
    lookup_all(Xs,lookup(K,HT)).

delete_all([],HT) -> HT;
delete_all([K|Ks],HT) ->
    delete_all(Ks,delete(K,HT)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_all_raw([],HT) -> HT;
insert_all_raw([{K,V}|Xs],HT) ->
    ?ENTER(insert_all_raw,2),
    insert_all_raw(Xs,insert_raw(K,V,HT)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_bucket(K,HT) ->
    Tab = HT#hashtable.table,
    Size = ?vectors:vsize(Tab),
    HashVal = erlang:phash(K,Size),
    ?vectors:get(Tab,HashVal).

set_bucket(K,Bucket,HT) ->
    Tab = HT#hashtable.table,
    Size = ?vectors:vsize(Tab),
    HashVal = erlang:phash(K,Size),
    NewTab = ?vectors:set(Tab,HashVal,Bucket),
    HT#hashtable{table=NewTab}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_bucket(K,V,[]) -> [{K,V}];
insert_bucket(K,V,[{K0,V0}|Xs]) ->
    if
	K == K0 ->
	    throw({insert_bucket,3,key_present});
	true ->
	    [{K0,V0}|insert_bucket(K,V,Xs)]
    end.

update_bucket(K,V,[]) -> {1,[{K,V}]};
update_bucket(K,V,[{K0,V0}|Xs]) ->
    if
	K == K0 ->
	    {0,[{K,V}|Xs]};
	true ->
	    {Incr,Ys} = update_bucket(K,V,Xs),
	    {Incr,[{K0,V0}|Ys]}
    end.

search_bucket(K,[]) -> not_found;
search_bucket(K,[{K,V}|_]) -> {found,V};
search_bucket(K,[_|Xs]) -> search_bucket(K,Xs).

delete_bucket(K,[]) -> {0,[]};
delete_bucket(K,[{K,V}|Xs]) -> {1,Xs};
delete_bucket(K,[X|Xs]) -> 
    {Incr,Ys} = delete_bucket(K,Xs),
    {Incr,[X|Ys]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(HT) ->
    Tab = HT#hashtable.table,
    Buckets = ?vectors:vector_to_list(Tab),
    lists:append(Buckets).

rehash(HT) ->
    ?ENTER(rehash,1),
    if
	HT#hashtable.occupancy > 
	       HT#hashtable.max_occ ->
	    rehash_table(inc,HT);
	HT#hashtable.occupancy < 
	       HT#hashtable.min_occ ->
	    rehash_table(dec,HT);
	true -> HT
    end.

rehash_table(Direction,HT) ->
    ?ENTER(rehash_table,2),
    Items = list(HT),
    HT0 = insert_all_raw(Items,empty(ideal_size(Direction,HT),
				     HT#hashtable.min_occ_ratio,
				     HT#hashtable.max_occ_ratio)),
    HT0#hashtable{occupancy=length(Items)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The new size depends on:
% - did the table shrink?
% - what is old size?
% - what is est. new size?
% - what is desired occupancy ratio?
%
% The trick is to avoid repeated resizing of the table.
% - table may oscillate between sizes if insert-delete sequences occur
% - table may shrink repeatedly if resized 
%
% Returns {Size,MinOcc,MaxOcc}
%
% *** UNFINISHED ***
% 

ideal_size(inc,HT) ->
    ?ENTER(ideal_size,2),
    Size = HT#hashtable.size,
    MinOcc = HT#hashtable.min_occ,
    MinOccRatio = HT#hashtable.min_occ_ratio,
    MaxOcc = HT#hashtable.max_occ,
    MaxOccRatio = HT#hashtable.max_occ_ratio,
    NewSize = round(2 * HT#hashtable.size),
    NewMinOcc = round(MinOccRatio * NewSize),
    NewMaxOcc = round(MaxOccRatio * NewSize),
    {NewSize,NewMinOcc,NewMaxOcc};
ideal_size(dec,HT) ->
    ?ENTER(ideal_size,2),
    Size = HT#hashtable.size,
    MinOcc = HT#hashtable.min_occ,
    MinOccRatio = HT#hashtable.min_occ_ratio,
    MaxOcc = HT#hashtable.max_occ,
    MaxOccRatio = HT#hashtable.max_occ_ratio,
    NewSize = round(0.5 * HT#hashtable.size),
    NewMinOcc = round(MinOccRatio * NewSize),
    NewMaxOcc = round(MaxOccRatio * NewSize),
    {NewSize,NewMinOcc,NewMaxOcc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty(Size) when integer(Size), Size > 0 ->
    ?ENTER(size,1),
    Tab = ?vectors:empty(Size,[]),
    #hashtable{table=Tab,size=Size}.

empty({Size,MinOcc,MaxOcc},Min,Max) 
       when integer(MinOcc), integer(MaxOcc),
            0 =< Min, Min =< 1, 0 =< Max, Max =< 1 ->
    ?ENTER(empty,3),
    HT = empty(Size),
    HT#hashtable{min_occ=MinOcc,max_occ=MaxOcc,
		 min_occ_ratio=Min,max_occ_ratio=Max}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_w_opts(Opts) ->
    Size = has_opt(size,Opts,64),
    MaxOccRatio = has_opt(max_occupancy_ratio,Opts,0.5),
    MaxOcc = round(MaxOccRatio * Size),
    MinOccRatio = has_opt(min_occupancy_ratio,Opts,0.1), 
    MinOcc = round(MinOccRatio * Size),
    empty({Size,MinOcc,MaxOcc},MinOccRatio,MaxOccRatio).

empty() ->
    empty_w_opts([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stats(HT) ->
    io:format('hashtable statistics~n',[]),
    io:format('size=~w~n',[HT#hashtable.size]),
    io:format('occupancy=~w (~w)~n',[HT#hashtable.occupancy,
			       HT#hashtable.occupancy/HT#hashtable.size]),
    io:format('occupancy limits=(~w,~w)~n',
	      [HT#hashtable.min_occ_ratio,HT#hashtable.max_occ_ratio]),
    io:format('bucket occupancy=~w~n',[bucket_count(HT)]).

bucket_count(HT) ->
    Lst = ?vectors:vector_to_list(HT#hashtable.table),
    list(bucket_length_count(Lst,empty())).

bucket_length_count([],Tab) -> Tab;
bucket_length_count([B|Bs],Tab) ->
    bucket_length_count(Bs,enter_bucket(B,Tab)).

enter_bucket(B,Tab) ->
    N = length(B),
    NewCount = case lookup(N,Tab) of
		   not_found ->
		       1;
		   {found,OldCount} ->
		       OldCount+1
	       end,
    update(N,NewCount,Tab).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_opt(Name,Opts,Dflt) ->
    case lists:keysearch(Name,1,Opts) of
	{value,Val} -> Val;
	false -> Dflt
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ins_stat(M) ->
    ins_stat(1,M,empty()).

ins_stat(M,N,HT) ->
    if
	M > N -> 
	    HT;
	true ->
	    stats(HT),
	    Lst = list(HT),
	    io:format('contents: ~w~n~n',[Lst]),
	    ins_stat(M+1,N,insert(M,M,HT))
    end.

del_stat(M,N,HT) ->
    if
	M > N -> 
	    HT;
	true ->
	    stats(HT),
	    io:format('~n',[]),
	    del_stat(M+1,N,delete(M,HT))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(H1,H2) ->
    join_list(list(H1),H2,[],[]).

join_list([],H2,OnlyH1,Common) ->
    {OnlyH1,list(H2),Common};
join_list([{X,V1}|Xs],H2,OnlyH1,Common) ->
    case lookup(X,H2) of
	{found,V2} ->
	    join_list(Xs,delete(X,H2),OnlyH1,[{X,V1,V2}|Common]);
	not_found ->
	    join_list(Xs,H2,[{X,V1}|OnlyH1],Common)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enter(P,N) ->
    io:format('enter ~w/~w~n',[P,N]).
