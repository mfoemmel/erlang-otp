%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <2001-02-15 20:08:18 richardc>
%% ====================================================================
%%  Filename : 	gb_hash.erl
%%  Module   :	gb_hash
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-03-07 Erik Johansson (happi@csd.uu.se): Created.
%%		* 2000-10-30 richardc@csd.uu.se: Updated.
%% CVS:
%%    $Author: richardc $
%%    $Date: 2001/03/26 18:37:09 $
%%    $Revision: 1.1.1.1 $
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_dict_hash).
-export([]).
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

init(Xs) ->
    dict:from_list(Xs).

init(Xs, _) ->
    dict:from_list(Xs).

empty() ->
    dict:new().

empty(_) ->
    dict:new().

lookup(X,T) ->
    case dict:find(X,T) of
	{ok, V} ->
	    {found, V};
	error ->
	    not_found
    end.

insert(X,V,T) ->
    dict:store(X,V,T).

update(X,V,T) ->
    dict:store(X,V,T).

delete(X,T) ->
    dict:erase(X,T).

list(T) ->
    dict:to_list(T).

rehash(T) ->
    T.

join(T1,T2) ->
    insert_all(list(T2),T1).

insert_all([{K,V}|Xs],HT) ->
    insert_all(Xs,insert(K,V,HT));
insert_all([],HT) -> HT.
