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
-module(bplus_tree).

-export([empty_tree/0,
	 lookup/2, lookup_first/1, lookup_next/2, lookup_n_next/3,
	 lookup_prev/2, lookup_last/1,
	 insert/3, delete/2]).

-define(max_size, 16).
-define(min_size, 8).

%%%-----------------------------------------------------------------
%%% DO NOT USE THIS MODULE!  It will be replaced by something much
%%% more efficient in the future...
%%%-----------------------------------------------------------------
%%% This module implements a B+ tree.

%%-----------------------------------------------------------------
%% Func: empty_tree/0
%% Purpose: Creates a new empty tree.
%% Returns: tree()
%%-----------------------------------------------------------------
empty_tree() -> void.

%%-----------------------------------------------------------------
%% Func: lookup/2
%% Purpose: Looks for Key in the Tree.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
lookup(void, Key) -> undefined;
lookup(Tree, Key) ->
    case node_type(Tree) of
	leaf ->
	    lookup_leaf(Key, Tree);
	node ->
	    {_, SubTree} = select_sub_tree(Tree, Key),
	    lookup(SubTree, Key)
    end.

%%-----------------------------------------------------------------
%% Func: lookup_first/1
%% Purpose: Finds the smallest key in the entire Tree.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
lookup_first(void) -> undefined;
lookup_first(Tree) ->
    case node_type(Tree) of
	leaf ->
	    % Then it is the leftmost key here.
	    {ok, get_leaf_key_val(Tree, 1)};         
	node ->
	    % Look in the leftmost subtree.
	    lookup_first(get_tree(Tree, 1))
    end.

%%-----------------------------------------------------------------
%% Func: lookup_next/2
%% Purpose: Finds the next key nearest after Key.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
lookup_next(void, _) -> undefined;
lookup_next(Tree, Key) ->
    case node_type(Tree) of
	leaf ->
	    lookup_next_leaf(Key, Tree);
	node ->
	    {Pos, SubTree} = select_sub_tree(Tree, Key),
	    case lookup_next(SubTree, Key) of
		undefined ->
		    S = get_size(Tree),
		    if
			% There is a right brother.
			S > Pos ->                  
			    lookup_first(get_tree(Tree, Pos+1));
			% No there is no right brother.
			true ->
			    undefined
		    end;
		% We ok a next item.
		Result ->                         
		    Result
	    end
    end.

%%-----------------------------------------------------------------
%% Func: lookup_n_next/3
%% Purpose: Finds the N next keys nearest after Key.  Returns a
%%          possibly empty list.
%% Returns: [{Key, Val}]
%%-----------------------------------------------------------------
lookup_n_next(void, _, _) -> [];
lookup_n_next(Tree, Key, N) ->
    case node_type(Tree) of
	leaf ->
	    lookup_n_next_leaf(Key, Tree, N);
	node ->
	    {Pos, SubTree} = select_sub_tree(Tree, Key),
	    Result = lookup_n_next(SubTree, Key, N),
	    L = length(Result),
	    if
		L == N ->                           
		    Result;
		true ->                           
		    % We want to get more since the list is to short.
		    Size = get_size(Tree),
		    lists:append(Result,
				 lookup_n_next_more(Tree, N-L, Pos+1, Size))
	    end
    end.

lookup_n_next_more(_, _, Pos, Size) when Pos > Size -> [];
lookup_n_next_more(Tree, N, Pos, Size) ->
    case node_type(Tree) of
	leaf ->
	    % Return the N first keys, Result can be < N!
	    lists:sublist(leaf_to_list(Tree), 1, N);
	node ->
	    SubTree = get_tree(Tree, Pos),
	    SubSize = get_size(SubTree),
	    Result = lookup_n_next_more(SubTree, N, 1, SubSize),
	    L = length(Result),
	    if
		L >= N ->
		    Result;
		true ->
		    lists:append(Result, 
				 lookup_n_next_more(Tree, N-L, Pos+1, Size))
	    end
    end.


%%-----------------------------------------------------------------
%% Func: lookup_prev/2
%% Purpose: Finds the next key nearest after Key.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
lookup_prev(void, _) -> undefined;
lookup_prev(Tree, Key) ->
    case node_type(Tree) of
	leaf ->
	    lookup_prev_leaf(Key, Tree);
	node ->
	    {Pos, SubTree} = select_sub_tree(Tree, Key),
	    case lookup_prev(SubTree, Key) of
		undefined ->
		    if
			% There is a left brother.
			Pos > 1 ->                  
			    lookup_last(get_tree(Tree, Pos-1));
			% No there is no left brother.
			true ->                   
			    undefined
		    end;
		% We ok a previous item.
		Result ->                         
		    Result
	    end
    end.

%%-----------------------------------------------------------------
%% Func: lookup_last/1
%% Purpose: Finds the greatest key in the entire structure.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
lookup_last(void) -> undefined;
lookup_last(Tree) ->
    case node_type(Tree) of
	leaf ->
	    % Then it is the rightmost key here.
	    {ok, get_leaf_key_val(Tree, get_size(Tree))}; 
	node ->
	    % Look in the rightmost subtree.
	    lookup_last(get_tree(Tree, get_size(Tree))) 
    end.

%%-----------------------------------------------------------------
%% Func: insert/3
%% Purpose: Inserts a new {Key, Value} into the tree.
%% Returns: tree()
%%-----------------------------------------------------------------
insert(void, Key, Val) -> mk_leaf([{Key, Val}]);
insert(Tree, Key, Val) ->
    NewTree = insert_in(Tree, Key, Val),
    case over_full(NewTree) of
	false ->
	    NewTree;
	% If the node is over-full the tree will grow.
	true ->
	    {LTree, DKey, RTree} = 
		case node_type(NewTree) of
		    leaf ->
			split_leaf(NewTree);
		    node ->
			split_node(NewTree)
		end,
	    mk_node([LTree, DKey, RTree])
    end.

%%-----------------------------------------------------------------
%% Func: delete/2
%% Purpose: Deletes a key from the tree (if present).
%% Returns: tree()
%%-----------------------------------------------------------------
delete(void, Key) -> void;
delete(Tree, Key) ->
    NewTree = delete_in(Tree, Key),
    S = get_size(NewTree),
    case node_type(NewTree) of
	leaf ->
	    if
		S == 0 ->
		    void;
		true ->
		    NewTree
	    end;
	node ->
	    if
		S == 1 ->
		    get_tree(NewTree, 1);
		true ->
		    NewTree
	    end
    end.

%%-----------------------------------------------------------------
%% Searches through a leaf until the Key is ok or
%% when it is determined that it does not exist.
%%-----------------------------------------------------------------
lookup_leaf(Key, Leaf) -> lookup_leaf_2(Key, Leaf, get_size(Leaf)).

lookup_leaf_2(_, _, 0) -> undefined;
lookup_leaf_2(Key, Leaf, N) ->
    K = get_leaf_key(Leaf, N),
    if
	K == Key ->
	    {ok, get_leaf_key_val(Leaf, N)};
	true ->
	    lookup_leaf_2(Key, Leaf, N-1)
    end.

%%-----------------------------------------------------------------
%% Returns {ok, NextKey} if there is a key in the leaf which is greater.
%% If there is no such key we return 'undefined' instead.
%% Key does not have to be a key in the structure, just a search value.
%%-----------------------------------------------------------------
lookup_next_leaf(Key, Leaf) -> lookup_next_leaf_2(Key, Leaf, get_size(Leaf), 1).

lookup_next_leaf_2(Key, Leaf, Size, Size) -> 
    % This is the rightmost key.
    K = get_leaf_key(Leaf, Size),          
    if
	K > Key ->
	    {ok, get_leaf_key_val(Leaf, Size)};
	true ->
	    undefined
    end;
lookup_next_leaf_2(Key, Leaf, Size, N) ->
    K = get_leaf_key(Leaf, N),
    if
	Key == K ->
	    % Since this is exact Key it must be the next.
	    {ok, get_leaf_key_val(Leaf, N+1)};
	K > Key ->                        
            % Key was not an exact specification.
	    % It must be K that is next greater.
	    {ok, get_leaf_key_val(Leaf, N)};
	true ->                         
	    % K is still smaller, try next in the leaf.
	    lookup_next_leaf_2(Key, Leaf, Size, N+1)
    end.

%%-----------------------------------------------------------------
%% Returns an N-long list of greater keys, greater than Key. It is
%% possible that the list wouldn't be that long if the leaf ends prior.
%%-----------------------------------------------------------------
lookup_n_next_leaf(Key, Leaf, N) ->
    lookup_n_next_leaf_2(Key, N, Leaf, get_size(Leaf), 1).

lookup_n_next_leaf_2(Key, N, Leaf, Size, Size) ->
    K = get_leaf_key(Leaf, Size),
    if
	K > Key ->
	    [get_leaf_key_val(Leaf, Size)];
	true ->
	    []
    end;
lookup_n_next_leaf_2(Key, N, Leaf, Size, P) ->
    K = get_leaf_key(Leaf, P),
    if
	Key == K ->                       
	    % Since this is exact Key it must be the next.
	    [get_leaf_key_val(Leaf, P+1) |
	     lookup_n_next_leaf_3(N-1, Leaf, Size, P+1)];
	K > Key ->                        
	    % Key was not an exact specification.
	    [get_leaf_key_val(Leaf, P) |
	     lookup_n_next_leaf_3(N-1, Leaf, Size, P)];
	true ->                         
	    % We continue uor search.
	    lookup_n_next_leaf_2(Key, N, Leaf, Size, P+1)
    end.

lookup_n_next_leaf_3(_, Leaf, Size, Size) -> [];
lookup_n_next_leaf_3(0, _, _, _) -> [];
lookup_n_next_leaf_3(N, Leaf, Size, P) ->
    [get_leaf_key_val(Leaf, P+1)|lookup_n_next_leaf_3(N-1, Leaf, Size, P+1)].


%%-----------------------------------------------------------------
%% Returns {ok, NextKey} if there is a key in the leaf which is smaller.
%% If there is no such key we return 'undefined' instead.
%% Key does not have to be a key in the structure, just a search value.
%%-----------------------------------------------------------------
lookup_prev_leaf(Key, Leaf) -> lookup_prev_leaf_2(Key, Leaf, get_size(Leaf)).

lookup_prev_leaf_2(Key, Leaf, 1) ->
    K = get_leaf_key(Leaf, 1), % This is the leftmost key.
    if
	K < Key ->
	    {ok, get_leaf_key_val(Leaf, 1)};
	true ->
	    undefined
    end;
lookup_prev_leaf_2(Key, Leaf, N) ->
    K = get_leaf_key(Leaf, N),
    if
	Key == K ->
	    {ok, get_leaf_key_val(Leaf, N-1)};
	K < Key ->
	    {ok, get_leaf_key_val(Leaf, N)};
	true ->
	    lookup_prev_leaf_2(Key, Leaf, N-1)
    end.

%%% -----------------------
%%% Help function to insert.
%%% -----------------------

insert_in(Tree, Key, Val) ->
    case node_type(Tree) of
	leaf ->
	    insert_in_leaf(Tree, Key, Val);
	node ->
	    {Pos, SubTree} = select_sub_tree(Tree, Key),  
            % Pos = "the position of the subtree".
	    NewSubTree = insert_in(SubTree, Key, Val),
	    case over_full(NewSubTree) of
		false ->
		    put_subtree(Tree, [NewSubTree, Pos]);
		true ->
		    case reorganize_tree_ins(Tree, NewSubTree, Pos) of
			{left, {LeftT, DKey, MiddleT}} ->
			    put_subtree(put_lkey(Tree, DKey, Pos),
					[LeftT, Pos-1, MiddleT, Pos]);
			{right, {MiddleT, DKey, RightT}} ->
			    put_subtree(put_rkey(Tree, DKey, Pos),
					[MiddleT, Pos, RightT, Pos+1]);
			{split, {LeftT, DKey, RightT}} ->
			    extend_tree(Tree, {LeftT, DKey, RightT}, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% Inserts a key in correct position in a leaf.
%%-----------------------------------------------------------------
insert_in_leaf(Leaf, Key, Val) ->
    insert_in_leaf_2(Leaf, Key, Val, get_size(Leaf), []).

insert_in_leaf_2(Leaf, Key, Val, 0, Accum) ->
    insert_in_leaf_3(Leaf, 0, [{Key, Val}|Accum]);
insert_in_leaf_2(Leaf, Key, Val, N, Accum) ->
    {K, V} = get_leaf_key_val(Leaf, N),
    if
	Key < K ->
	    % Not here!
	    insert_in_leaf_2(Leaf, Key, Val, N-1, [{K, V}|Accum]);
	K < Key ->
	    % Insert here.
	    insert_in_leaf_3(Leaf, N-1, [{K, V}, {Key, Val}|Accum]);
	K == Key ->
	    % Replace (?).
	    insert_in_leaf_3(Leaf, N-1, [{Key, Val}|Accum])
    end.

insert_in_leaf_3(Leaf, 0, LeafList) ->
    mk_leaf(LeafList);
insert_in_leaf_3(Leaf, N, LeafList) ->
    insert_in_leaf_3(Leaf, N-1, [get_leaf_key_val(Leaf, N)|LeafList]).


%%% -------------------------
%%% Help functions for delete.
%%% -------------------------

delete_in(Tree, Key) ->
    case node_type(Tree) of
	leaf ->
	    delete_in_leaf(Tree, Key);
	node ->
	    {Pos, SubTree} = select_sub_tree(Tree, Key),  
	    % Pos = "the position of the subtree".
	    NewSubTree = delete_in(SubTree, Key),
	    % Check if it has become to small now
	    case under_filled(NewSubTree) of
		false ->
		    put_subtree(Tree, [NewSubTree, Pos]);
		true ->
		    case reorganize_tree_del(Tree, NewSubTree, Pos) of
			{left, {LeftT, DKey, MiddleT}} ->
			    put_subtree(put_lkey(Tree, DKey, Pos),
					[LeftT, Pos-1, MiddleT, Pos]);
			{right, {MiddleT, DKey, RightT}} ->
			    put_subtree(put_rkey(Tree, DKey, Pos),
					[MiddleT, Pos, RightT, Pos+1]);
			{join_left, JoinedTree} ->
			    joinleft_tree(Tree, JoinedTree, Pos);
			{join_right, JoinedTree} ->
			    joinright_tree(Tree, JoinedTree, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% Deletes a key from the leaf returning a new (smaller) leaf.
%%-----------------------------------------------------------------
delete_in_leaf(Leaf, Key) ->
    delete_in_leaf_2(Leaf, Key, get_size(Leaf), []).

delete_in_leaf_2(Leaf, _, 0, _) -> Leaf;
delete_in_leaf_2(Leaf, Key, N, Accum) ->
    {K, V} = get_leaf_key_val(Leaf, N),
    if
	Key == K ->
            % Remove this one!
	    delete_in_leaf_3(Leaf, N-1, Accum);
	true ->
	    delete_in_leaf_2(Leaf, Key, N-1, [{K, V}|Accum])
    end.

delete_in_leaf_3(Leaf, 0, LeafList) ->
    mk_leaf(LeafList);
delete_in_leaf_3(Leaf, N, LeafList) ->
    delete_in_leaf_3(Leaf, N-1, [get_leaf_key_val(Leaf, N)|LeafList]).



%%-----------------------------------------------------------------
%% Selects and returns which subtree the search should continue in.
%%-----------------------------------------------------------------
select_sub_tree(Tree, Key) ->
    select_sub_tree_2(Tree, Key, get_size(Tree)).

select_sub_tree_2(Tree, Key, 1) -> {1, get_tree(Tree, 1)};
select_sub_tree_2(Tree, Key, N) ->
    K = get_lkey(Tree, N),
    if
	K > Key ->
	    select_sub_tree_2(Tree, Key, N-1);
	K =< Key ->
            % Here it is!
	    {N, get_tree(Tree, N)}
    end.

%%-----------------------------------------------------------------
%% Selects which brother that should take over some of our items.
%% Or if they are both full makes a split.
%%-----------------------------------------------------------------
reorganize_tree_ins(Tree, NewSubTree, 1) ->
    RTree = get_tree(Tree, 2),  % 2 = Pos+1 = 1+1.
    case full(RTree) of
	false ->
	    reorganize_tree_r(Tree, NewSubTree, 1, RTree);
	true ->
            % It is full, we must split this one!
	    reorganize_tree_s(Tree, NewSubTree, 1)
    end;
reorganize_tree_ins(Tree, NewSubTree, Pos) ->
    Size = get_size(Tree),
    if
	Pos == Size ->
            % Pos is the rightmost postion!.
            % Our only chance is the left one.
	    LTree = get_tree(Tree, Pos-1),
 	    case full(LTree) of
		false ->
		    reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
		    % It is full, we must split this one!
		    reorganize_tree_s(Tree, NewSubTree, Pos)
	    end;
	true ->
            % Pos is somewhere inside the node.
	    LTree = get_tree(Tree, Pos-1),
	    RTree = get_tree(Tree, Pos+1),
	    SL = get_size(LTree),
	    SR = get_size(RTree),
	    if
		SL > SR ->
		    reorganize_tree_r(Tree, NewSubTree, Pos, RTree);
		SL < SR ->
		    reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
		    case full(LTree) of
			false ->
			    reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
			true ->
			    reorganize_tree_s(Tree, NewSubTree, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% This function fills over items from brothers to maintain the minimum
%% number of items per node/leaf.
%%-----------------------------------------------------------------
reorganize_tree_del(Tree, NewSubTree, 1) ->
    % The case when Pos is at leftmost position.
    RTree = get_tree(Tree, 2),  % 2 = Pos+1 = 1+1.
    case low_filled(RTree) of
	false ->
	    reorganize_tree_r(Tree, NewSubTree, 1, RTree);
	true ->
            % It is to small, we must join them!
	    reorganize_tree_jr(Tree, NewSubTree, 1, RTree)
    end;
reorganize_tree_del(Tree, NewSubTree, Pos) ->
    Size = get_size(Tree),
    if
	Pos == Size ->
            % Pos is the rightmost postion!.
            % Our only chance is the left one.
	    LTree = get_tree(Tree, Pos-1),
	    case low_filled(LTree) of
		false ->
		    reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
                    % It is to small, we must join this one!
		    reorganize_tree_jl(Tree, NewSubTree, Pos, LTree)
	    end;
	true ->
            % Pos is somewhere inside the node.
	    LTree = get_tree(Tree, Pos-1),
	    RTree = get_tree(Tree, Pos+1),
	    SL = get_size(LTree),
	    SR = get_size(RTree),
	    if
		SL>SR ->
		    reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		SL < SR ->
		    reorganize_tree_r(Tree, NewSubTree, Pos, RTree);
		true ->
		    case low_filled(LTree) of
			false ->
			    reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
			true ->
			    reorganize_tree_jl(Tree, NewSubTree, Pos, LTree)
		    end
	    end
    end.


reorganize_tree_l(Tree, NewSubTree, Pos, LTree) ->
    case node_type(NewSubTree) of
	leaf ->
	    {left, split_leaf(mk_leaf(lists:append(leaf_to_list(LTree),
						   leaf_to_list(NewSubTree))))};
	node ->
	    {left, split_node(mk_node(lists:append([node_to_list(LTree),
						   [get_lkey(Tree, Pos)],
						   node_to_list(NewSubTree)])))}
    end.

reorganize_tree_r(Tree, NewSubTree, Pos, RTree) ->
    case node_type(NewSubTree) of
	leaf ->
	    {right, split_leaf(mk_leaf(lists:append([leaf_to_list(NewSubTree),
						    leaf_to_list(RTree)])))};
	node ->
	    {right, split_node(mk_node(lists:append([node_to_list(NewSubTree),
						    [get_rkey(Tree, Pos)],
						    node_to_list(RTree)])))}
    end.

reorganize_tree_s(Tree, NewSubTree, Pos) ->
    case node_type(NewSubTree) of
	leaf ->
	    {split, split_leaf(NewSubTree)};
	node ->
	    {split, split_node(NewSubTree)}
    end.

reorganize_tree_jl(Tree, NewSubTree, Pos, LTree) ->
    case node_type(NewSubTree) of
	leaf ->
	    {join_left, mk_leaf(lists:append([leaf_to_list(LTree),
					     leaf_to_list(NewSubTree)]))};
	node ->
	    {join_left, mk_node(lists:append([node_to_list(LTree),
					     [get_lkey(Tree, Pos)],
					     node_to_list(NewSubTree)]))}
    end.

reorganize_tree_jr(Tree, NewSubTree, Pos, RTree) ->
    case node_type(NewSubTree) of
	leaf ->
	    {join_right, mk_leaf(lists:append([leaf_to_list(NewSubTree),
					      leaf_to_list(RTree)]))};
	node ->
	    {join_right, mk_node(lists:append([node_to_list(NewSubTree),
					      [get_rkey(Tree, Pos)],
					      node_to_list(RTree)]))}
    end.


%%-----------------------------------------------------------------
%% Takes a leaf and divides it into two equal big leaves.
%% The result is returned in a tuple. The dividing key is also returned.
%%-----------------------------------------------------------------
split_leaf(Leaf) ->
    S = get_size(Leaf),
    split_leaf_2(Leaf, S, trunc(S/2), []).

split_leaf_2(Leaf, Pos, 1, Accum) -> 
    {K, V} = get_leaf_key_val(Leaf, Pos),
    split_leaf_3(Leaf, Pos-1, [], K, [{K,V}|Accum]);
split_leaf_2(Leaf, Pos, N, Accum) ->
    split_leaf_2(Leaf, Pos-1, N-1, [get_leaf_key_val(Leaf, Pos)|Accum]).

split_leaf_3(_, 0, LeftAcc, DKey, RightAcc) ->
    {mk_leaf(LeftAcc), DKey, mk_leaf(RightAcc)};
split_leaf_3(Leaf, Pos, LeftAcc, DKey, RightAcc) ->
    split_leaf_3(Leaf, Pos-1, [get_leaf_key_val(Leaf, Pos)|LeftAcc],
		 DKey, RightAcc).

%%-----------------------------------------------------------------
%% Takes a node and divides it into two equal big nodes.
%% The result is returned in a tuple. The dividing key is also returned.
%%-----------------------------------------------------------------
split_node(Node) ->
    S = get_size(Node),
    split_node_2(Node, S, trunc(S/2), []).

split_node_2(Node, Pos, 1, Accum) ->
    split_node_3(Node, Pos-1, [], get_lkey(Node, Pos),
		 [get_tree(Node, Pos)|Accum]);
split_node_2(Node, Pos, N, Accum) ->
    split_node_2(Node, Pos-1, N-1, [get_lkey(Node, Pos),
				    get_tree(Node, Pos)|Accum]).

split_node_3(Node, 1, LeftAcc, DKey, RightAcc) ->
    {mk_node([get_tree(Node, 1)|LeftAcc]), DKey, mk_node(RightAcc)};
split_node_3(Node, Pos, LeftAcc, DKey, RightAcc) ->
    split_node_3(Node, Pos-1,
		 [get_lkey(Node, Pos), get_tree(Node, Pos)|LeftAcc],
		 DKey, RightAcc).

%%-----------------------------------------------------------------
%% Inserts a joined tree insted of the old one at position Pos and
%% the one nearest left/right brother.
%%-----------------------------------------------------------------
joinleft_tree(Tree, JoinedTree, Pos) ->
    join_tree_2(Tree, JoinedTree, Pos, get_size(Tree), []).
joinright_tree(Tree, JoinedTree, Pos) ->
    join_tree_2(Tree, JoinedTree, Pos+1, get_size(Tree), []).

join_tree_2(Tree, JoinedTree, Pos, Pos, Accum) ->
    join_tree_3(Tree, Pos-2, [JoinedTree|Accum]);
join_tree_2(Tree, JoinedTree, Pos, N, Accum) ->
    join_tree_2(Tree, JoinedTree, Pos, N-1,
		[get_lkey(Tree, N), get_tree(Tree, N)|Accum]).

join_tree_3(Tree, 0, Accum) -> mk_node(Accum);
join_tree_3(Tree, Pos, Accum) ->
    join_tree_3(Tree, Pos-1, [get_tree(Tree, Pos), get_rkey(Tree, Pos)|Accum]).

%%% ---------------------------------
%%% Primitive datastructure functions.
%%% ---------------------------------

%%-----------------------------------------------------------------
%% Constructs a node out of list format.
%%-----------------------------------------------------------------
mk_node(NodeList) -> list_to_tuple([node|NodeList]).

%%-----------------------------------------------------------------
%% Converts the node into list format.
%%-----------------------------------------------------------------
node_to_list(Node) ->
    [_|NodeList] = tuple_to_list(Node),
    NodeList.

%%-----------------------------------------------------------------
%% Constructs a leaf out of list format.
%%-----------------------------------------------------------------
mk_leaf(KeyList) -> list_to_tuple([leaf|KeyList]).

%%-----------------------------------------------------------------
%% Converts a leaf into list format.
%%-----------------------------------------------------------------
leaf_to_list(Leaf) ->
    [_|LeafList] = tuple_to_list(Leaf),
    LeafList.


%%-----------------------------------------------------------------
%% Finds out the type of the node: 'leaf' or 'node'.
%%-----------------------------------------------------------------
node_type(Tree) -> element(1, Tree).

%%-----------------------------------------------------------------
%% Changes subtree "pointers" in a node.
%%-----------------------------------------------------------------
put_subtree(Tree, []) -> Tree;
put_subtree(Tree, [NewSubTree, Pos|Rest]) ->
    put_subtree(setelement(Pos*2, Tree, NewSubTree), Rest).

%%-----------------------------------------------------------------
%% Replaces the tree at position Pos with two new trees.
%%-----------------------------------------------------------------
extend_tree(Tree, Inserts, Pos) ->
    extend_tree_2(Tree, Inserts, Pos, get_size(Tree), []).

extend_tree_2(Tree, {T1, DKey, T2}, Pos, Pos, Accum) ->
    extend_tree_3(Tree, Pos-1, [T1, DKey, T2|Accum]);
extend_tree_2(Tree, Inserts, Pos, N, Accum) ->
    extend_tree_2(Tree, Inserts, Pos, N-1,
		  [get_lkey(Tree, N), get_tree(Tree, N)|Accum]).

extend_tree_3(_, 0, Accum) -> mk_node(Accum);
extend_tree_3(Tree, N, Accum) ->
    extend_tree_3(Tree, N-1, [get_tree(Tree, N), get_rkey(Tree, N)|Accum]).

%%-----------------------------------------------------------------
%% Changes the dividing key between two trees.
%%-----------------------------------------------------------------
put_lkey(Tree, DKey, Pos) -> setelement(Pos*2-1, Tree, DKey).
put_rkey(Tree, DKey, Pos) -> setelement(Pos*2+1, Tree, DKey).

%%-----------------------------------------------------------------
%% Finds out if a node/leaf is full or not.
%%-----------------------------------------------------------------
full(Tree) ->
    S = get_size(Tree),
    if
	S < ?max_size ->
	    false;
	true ->
	    true
    end.

%%-----------------------------------------------------------------
%% Finds out if a node/leaf is filled up over its limit.
%%-----------------------------------------------------------------
over_full(Tree) ->
    S = get_size(Tree),
    if
	S =< ?max_size ->
	    false;
	true ->
	    true
    end.

%%-----------------------------------------------------------------
%% Finds out if a node/leaf has less items then allowed.
%%-----------------------------------------------------------------
under_filled(Tree) ->
    S = get_size(Tree),
    if
	S >= ?min_size ->
	    false;
	true ->
	    true
    end.

%%-----------------------------------------------------------------
%% Finds out if a node/leaf has as few items as minimum allowed.
%%-----------------------------------------------------------------
low_filled(Tree) ->
    S = get_size(Tree),
    if
	S > ?min_size ->
	    false;
	true ->
	    true
    end.

%%-----------------------------------------------------------------
%% Calculates the number of items in a node/leaf.
%%-----------------------------------------------------------------
get_size(Tree) ->
    case node_type(Tree) of
	leaf ->
	    size(Tree)-1;
	node ->
	    trunc(size(Tree)/2)
    end.

%%-----------------------------------------------------------------
%% Returns a tree at position Pos from an internal node.
%%-----------------------------------------------------------------
get_tree(Tree, Pos) -> element(Pos*2, Tree).

%%-----------------------------------------------------------------
%% Returns a key in a leaf at position Pos.
%%-----------------------------------------------------------------
get_leaf_key(Leaf, Pos) -> element(1, element(Pos+1, Leaf)).
get_leaf_key_val(Leaf, Pos) -> element(Pos+1, Leaf).

%%-----------------------------------------------------------------
%% Returns dividing keys, left of or right of a tree.
%%-----------------------------------------------------------------
get_lkey(Tree, Pos) -> element(Pos*2-1, Tree).
get_rkey(Tree, Pos) -> element(Pos*2+1, Tree).












