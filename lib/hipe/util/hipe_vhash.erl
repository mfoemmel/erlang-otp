%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_vhash.erl
%%  Module   :	hipe_vhash
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-12-15 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: richardc $
%%              $Date: 2002/10/01 12:41:19 $
%%              $Revision: 1.3 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_vhash).
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
-include("hash.hrl").

%-define(trace,true).
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
    % ?ENTER(insert_raw,3),
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

%% insert_all([],HT) -> HT;
%% insert_all([{K,V}|Xs],HT) ->
%%     insert_all(Xs,insert(K,V,HT)).

update_all([],HT) -> HT;
update_all([{K,V}|Xs],HT) ->
    update_all(Xs,update(K,V,HT)).

%% lookup_all([],HT) -> HT;
%% lookup_all([{K,_V}|Xs],HT) ->
%%     lookup_all(Xs,lookup(K,HT)).

%% delete_all([],HT) -> HT;
%% delete_all([K|Ks],HT) ->
%%     delete_all(Ks,delete(K,HT)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_all_raw([],HT) -> HT;
insert_all_raw([{K,V}|Xs],HT) ->
    % ?ENTER(insert_all_raw,2),
    insert_all_raw(Xs,insert_raw(K,V,HT)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_bucket(K,HT) ->
    Tab = HT#hashtable.table,
    Size = hipe_beam_vectors:vsize(Tab),
    HashVal = erlang:phash(K,Size),
    hipe_beam_vectors:get(Tab,HashVal).

set_bucket(K,Bucket,HT) ->
    Tab = HT#hashtable.table,
    Size = hipe_beam_vectors:vsize(Tab),
    HashVal = erlang:phash(K,Size),
    NewTab = hipe_beam_vectors:set(Tab,HashVal,Bucket),
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

search_bucket(_K,[]) -> not_found;
search_bucket(K,[{K,V}|_]) -> {found,V};
search_bucket(K,[_|Xs]) -> search_bucket(K,Xs).

delete_bucket(_K,[]) -> {0,[]};
delete_bucket(K,[{K,_V}|Xs]) -> {1,Xs};
delete_bucket(K,[X|Xs]) -> 
    {Incr,Ys} = delete_bucket(K,Xs),
    {Incr,[X|Ys]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(HT) ->
    Tab = HT#hashtable.table,
    Buckets = hipe_beam_vectors:vector_to_list(Tab),
    conc(Buckets).

rehash(HT) ->
    if
	HT#hashtable.occupancy > 
	       HT#hashtable.max_occ ->
	?ENTER(rehash_up,1),

	    rehash_table(inc,HT);
	HT#hashtable.occupancy < 
	       HT#hashtable.min_occ ->
	?ENTER(rehash_down,1),
	    rehash_table(dec,HT);
	true -> HT
    end.

rehash_table(Direction,HT) ->
    ?ENTER(rehash_table,2),
    Items = list(HT),
    HT0 = insert_all_raw(Items,empty(ideal_size(Direction,HT),
				     HT#hashtable.min_occ_ratio,
				     HT#hashtable.max_occ_ratio)),
    HT0#hashtable{occupancy=HT#hashtable.occupancy}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The new size depends on:
%% - did the table shrink?
%% - what is old size?
%% - what is est. new size?
%% - what is desired occupancy ratio?
%%
%% The trick is to avoid repeated resizing of the table.
%% - table may oscillate between sizes if insert-delete sequences occur
%% - table may shrink repeatedly if resized 
%%
%% Returns {Size,MinOcc,MaxOcc}
%%
%% *** UNFINISHED ***
%% 

ideal_size(inc,HT) ->
    Size = HT#hashtable.size,
    ?ENTER(ideal_size,Size),
    ?ENTER(occupancy,HT#hashtable.occupancy),

    %% MinOcc = HT#hashtable.min_occ,
    MinOccRatio = HT#hashtable.min_occ_ratio,
    %% MaxOcc = HT#hashtable.max_occ,
    MaxOccRatio = HT#hashtable.max_occ_ratio,
    NewSize = round(10 * Size),
    ?ENTER(new_size,NewSize),
    NewMinOcc = round(MinOccRatio * NewSize),
    NewMaxOcc = round(MaxOccRatio * NewSize),
    {NewSize,NewMinOcc,NewMaxOcc};
ideal_size(dec,HT) ->
    ?ENTER(ideal_size,2),
    Size = HT#hashtable.size,
    %% MinOcc = HT#hashtable.min_occ,
    MinOccRatio = HT#hashtable.min_occ_ratio,
    %% MaxOcc = HT#hashtable.max_occ,
    MaxOccRatio = HT#hashtable.max_occ_ratio,
    NewSize = round(0.2 * Size),
    NewMinOcc = round(MinOccRatio * NewSize),
    NewMaxOcc = round(MaxOccRatio * NewSize),
    {NewSize,NewMinOcc,NewMaxOcc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty(Size) when is_integer(Size), Size > 0 ->
    % ?ENTER(size,Size),
    Tab = hipe_beam_vectors:empty(Size,[]),
    #hashtable{table=Tab,size=Size}.

empty({Size,MinOcc,MaxOcc},Min,Max) 
       when is_integer(MinOcc), is_integer(MaxOcc),
            0 =< Min, Min =< 1, 0 =< Max, Max =< 1 ->
    % ?ENTER(empty,3),
    HT = empty(Size),
    HT#hashtable{min_occ=MinOcc,max_occ=MaxOcc,
		 min_occ_ratio=Min,max_occ_ratio=Max}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_w_opts(Opts) ->
    Size = has_opt(size,Opts,100),
    MaxOccRatio = has_opt(max_occupancy_ratio,Opts,0.5),
    MaxOcc = round(MaxOccRatio * Size),
    MinOccRatio = has_opt(min_occupancy_ratio,Opts,0.01), 
    MinOcc = round(MinOccRatio * Size),
    empty({Size,MinOcc,MaxOcc},MinOccRatio,MaxOccRatio).

empty() ->
    empty_w_opts([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_opt(Name,Opts,Dflt) ->
    case lists:keysearch(Name,1,Opts) of
	{value,Val} -> Val;
	false -> Dflt
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

%% enter(P,N) ->
%%     io:format('enter ~w/~w~n',[P,N]).

%% ____________________________________________________________________
%% 

conc([]) -> [];
conc([X|Xs]) -> X ++ conc(Xs).
