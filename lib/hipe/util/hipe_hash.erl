%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			HASH TABLES IN ERLANG
%
% Abstract interface to hash(-like) table implementations.
%
% Available implementations:
%  hipe_dict_hash:  uses the stdlib `dict' module (dynamic hashing).
%  hipe_gb_hash:  uses the `gb_trees' module (general balanced trees).
%  hipe_pure_hash:  written in a purely functional style; may copy a lot

%-define(impl,hipe_dict_hash).
-define(impl,hipe_gb_hash).
%-define(impl,hipe_pure_hash).

-module(hipe_hash).

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
    ?impl:init(Xs).

init(A,B) ->
    ?impl:init(A,B).

empty() ->
    ?impl:empty().

empty(X) ->
    ?impl:empty(X).

lookup(X,T) ->
    ?impl:lookup(X,T).

insert(X,V,T) ->
    ?impl:insert(X,V,T).

update(X,V,T) ->
    ?impl:update(X,V,T).

delete(X,T) ->
    ?impl:delete(X,T).

list(T) ->
    ?impl:list(T).

rehash(T) ->
    ?impl:rehash(T).

join(T1,T2) ->
    ?impl:join(T1,T2).
