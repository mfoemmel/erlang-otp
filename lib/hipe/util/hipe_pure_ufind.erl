%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		       NONDESTRUCTIVE UNION-FIND
%
% Union-find with path compression; requires a fixed number of
% equivalence classes to be merged. This implementation will create
% new versions of the datastructures.
%
%  init(N): initializes N equivalence classes with self as value
%     (i.e., class N has value N)
%  list(U): list the {Index,EquivClass} pairs of U
%  union(X,Y,U): merge equivalence classes X and Y (must be integers!)
%  find(X,U): returns {Value,NewU}
%  only_find(X,U): returns Index (no path compression is done)

-module(hipe_pure_ufind).
-export([init/1,
	 list/1,
	 union/3,
	 find/2,
	 only_find/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(N) ->
    list_to_tuple(mklist(1,N)).

mklist(M,N) when M > N -> [];
mklist(M,N) -> [M|mklist(M+1,N)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(U) ->
    list_all(1,size(U),U).

list_all(M,N,U) when M > N ->
    [];
list_all(M,N,U) -> 
    {V,NewU} = find(M,U),
    [{M,V}|list_all(M+1,N,NewU)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Equivalence classes are represented as a vector of integers.
% Initially, each position i has the value i, but as classes are
% merged, i will get value j, j value k, etc.
%   Find traces such a chain until it finds an index i with value i;
% this is the true value.
%   Union merges two classes by dereferencing both, then
% setting one to point to the other.
%
% Note: there must _never_ be nontrivial circular chains x -> y -> ... -> x
%  or the algorithm will loop!
%   This is the reason for doing 'find' on both elements in union.
%  A second method is given as union2/3, which is slightly more complex
%  but sometimes avoids an extra find-operation.
%
% I haven't measured which one is the fastest in practice.

% FIND:
% - dereference chain of indices until a self-pointer occurs.

find(X,U) ->
    case element(X,U) of
	X ->  % returned self: chain ended
	    {X,U};
	Y ->  % returned other: follow chain
	    {V,NewU} = find(Y,U),
	    {V,setelement(X,NewU,V)}
    end.

only_find(X,U) ->
    case element(X,U) of
	X -> % returned self: end of chain
	    X;
	Y ->
	    only_find(Y,U)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Two implementations of the UNION operation.

union(X,Y,U) ->
    union2(X,Y,U).

% IMPLEMENTATION 1
%
% Idea:
% - Always dereference both arguments. The 'end-points' equivalence
%   classes can then be joined arbitrarily. (Either can point to the other.)
%
% COMMENTED OUT: Currently unused

% union1(X,Y,U) ->
%     {V,NxtU} = find(X,U),
%     {W,NewU} = find(Y,NxtU),
%     setelement(V,NewU,W).

% IMPLEMENTATION 2
%
% Idea:
% - Always set a larger index to point to a smaller one. This avoids
%   nontrivial circular chains and sometimes requires only one find.
% 
% Method:
% - first deref X to V. If V is larger than Y, it can safely point to Y
%   (since Y won't point to anything as large as V)
% - otherwise, deref Y to W and set the larger of V or W to point to the
%   smaller.

union2(X,Y,U) ->
    {V,NxtU} = find(X,U),
    if V > Y ->
	    setelement(V,NxtU,Y);
       V < Y ->
	    {W,NewU} = find(Y,NxtU),
	    if V < W ->
		    setelement(W,NewU,V);
	       V > W ->
		    setelement(NewU,V,W);
	       V == W ->
		    NewU    % V == W
	    end;
       true ->
	    NxtU    % V == Y
    end.
