%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_gb_hash.erl
%%  Module   :	hipe_gb_hash
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-03-07 Erik Johansson (happi@csd.uu.se): Created.
%%		* 2000-10-30 richardc@csd.uu.se: Updated.
%% CVS:
%%    $Author: kostis $
%%    $Date: 2002/05/03 13:52:45 $
%%    $Revision: 1.2 $
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_gb_hash).
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
    update_all(Xs,empty()).

init(Xs,_Opts) ->
    update_all(Xs,empty()).

empty() ->
    gb_trees:empty().

empty(_) ->
    gb_trees:empty().

lookup(X,T) ->
    case gb_trees:lookup(X,T) of
	{value, V} ->
	    {found, V};
	none ->
	    not_found
    end.

insert(X,V,T) ->
    gb_trees:insert(X,V,T).

update(X,V,T) ->
    gb_trees:enter(X,V,T).

delete(X,T) ->
    gb_trees:delete(X,T).

list(T) ->
    gb_trees:to_list(T).

rehash(T) ->
    T.


join(T1,T2) ->
    insert_all(list(T2),T1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_all([],HT) -> HT;
insert_all([{K,V}|Xs],HT) ->
    insert_all(Xs,insert(K,V,HT)).

update_all([],HT) -> HT;
update_all([{K,V}|Xs],HT) ->
    update_all(Xs,update(K,V,HT)).

%lookup_all([],HT) -> HT;
%lookup_all([{K,V}|Xs],HT) ->
%    lookup_all(Xs,lookup(K,HT)).
%
%delete_all([],HT) -> HT;
%delete_all([K|Ks],HT) ->
%    delete_all(Ks,delete(K,HT)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
