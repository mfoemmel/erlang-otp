%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%		   ABSTRACT INTERFACE TO UNION-FIND
%%
%% This module provides the interface of the union-find ADT.
%% Also provides some common implementations on top of 'raw' ADT.
%% (e.g., union_list/2)
%%
%%  init(N): initializes N equivalence classes with self as value
%%     (i.e., class N has value N)
%%  list(U): list the {Index,EquivClass} pairs of U
%%  union(X,Y,U): merge equivalence classes X and Y (must be integers!);
%%        returns new U.
%%  union_list(Xs,U): merges all the classes Xs; returns new U
%%  find(X,U): returns {Value,NewU} (performs path compression)
%%  only_find(X,U): returns Index (no path compression is done)

%-define(impl,hipe_pure_ufind).
-define(impl,hipe_vector_ufind).

-module(hipe_ufind).
-export([init/1,
	 list/1,
	 union/3,
	 union_list/2,
	 find/2,
	 only_find/2]).

init(X) ->
    ?impl:init(X).

list(X) ->
    ?impl:list(X).

union(A,B,C) ->
    ?impl:union(A,B,C).

find(X,Y) ->
    ?impl:find(X,Y).

only_find(X,Y) ->
    ?impl:only_find(X,Y).

union_list([],U) -> U;
union_list([X|Xs],U) ->
    union_list(X,Xs,U).

union_list(_X,[],U) -> U;
union_list(X,[Y|Ys],U) ->
    union_list(X,Ys,?impl:union(X,Y,U)).
