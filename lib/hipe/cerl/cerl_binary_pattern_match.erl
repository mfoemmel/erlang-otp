%%-------------------------------------------------------------------
%% File    : cerl_bin_.erl
%% Author  : Per Gustafsson <pergu@it.uu.se>
%% Description : 
%%
%% Created : 28 May 2003 by Per Gustafsson <pergu@it.uu.se>
%%-------------------------------------------------------------------

-module(cerl_binary_pattern_match).
-author('pergu@it.uu.se').

-export([add_offset_to_bin/1]).
-export([read_seg_size/1, read_seg_offset/1, read_seg_type/1,
	 read_seg_tag/1, match_tag/1, match_val/1, match_group_tag/1,
	 match_group_vals/1,
	 binary_match_clause_tree/1, binary_match_max_tag/1,
	 clause_tree_instr/1, clause_tree_success/1, clause_tree_fail/1,
	 bin_guard_matches/1, bin_guard_label/1,
	 size_const/1, size_all/1, size_vars/1,size_def_vars/1,
	 instr_type/1, label_name/1, goto_label/1]).

%% Exactly one of these four macros should be defined
%% they decide which heuristic is used to select the next
%% segment

-define(USES_MATCH, true).
%-define(USES_READ, true).
%-define(INDEXNESS, true).
%-define(LEFT, true).



%-define(BIN_PMATCH_DEBUG,true).
%-define(DOT_FILE, '/tmp/out.dot').
-ifdef(BIN_PMATCH_DEBUG).
-export([pp/1, full_size/1, a_height/1, max_height/1, read_seg/4,
	dot/1]).
-endif.
-include("cerl_hipe_primops.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Record definitions and access functions 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @type instr()= size() | read_seg() | match() | label() | goto() | bin_guard() .
%% All these data types can be used as instructions 

%% @type read_seg(). an abstract data type containing type,
%% offset, size and tag data which describes a read segment
-record(read_seg, {type, offset, size, tag}).

%% @spec read_seg_size(read_seg()) -> integer() | var()
%% @doc Returns the size field from a read_seg
read_seg_size(#read_seg{size=X}) -> X.

%% @spec read_seg_offset(read_seg()) -> size()
%% @doc Returns the offset field from a read_seg
read_seg_offset(#read_seg{offset=X}) -> X.

%% @spec read_seg_type(read_seg()) -> {atom(), atom(), atom()}
%% @doc Returns the type field from a read_seg
read_seg_type(#read_seg{type=X}) -> X.

%% @spec read_seg_tag(read_seg()) -> integer()
%% @doc Returns the tag field from a read_seg
read_seg_tag(#read_seg{tag=X}) -> X.

%% @spec read_seg({atom(), atom(), atom()}, size(), integer() | var(), integer()) -> read_seg()
%% @doc Creates a read_seg from a type an offset a size and a tag
read_seg(Type, Offset, Size, Tag)->
  #read_seg{type=Type, offset=Offset, size=Size, tag=Tag}.

%% @type b_clause(). Represents an original binary clause consisting of
%% instructions, matches to be performed at the guard, a guard, the body
%% of the clause and a pointer to the next clause this is only used internally

-record(b_clause, {segments, matches=[], guard, body, next_clause}).
b_clause_segments(#b_clause{segments=X}) -> X.


%% @type match(). Reprsents a matching between a result from a read_seg
%% which is represented as a tag and a term which is called val
-record(match, {val, tag}).
%% @spec match_tag(match()) -> integer()
%% @doc Returns the tag field from a match test
match_tag(#match{tag=X})-> X.
%% @spec match_val(match()) -> term()
%% @doc Returns the val field from a match test
match_val(#match{val=X})-> X.

-record(match_group, {tag, vals}).
match_group_tag(#match_group{tag=X})->X.
match_group_vals(#match_group{vals=X}) ->X.

%% @type binary_match(). Represents an entire binary matching, The structure 
%% of the matching is described in the clause tree and the max tag shows
%% how many reads might be needed
-record(binary_match, {clause_tree, max_tag}).
%% @spec binary_match_clause_tree(binary_match()) -> clause_tree()
%% @doc Returns the clause_tree field from a binary match 
binary_match_clause_tree(#binary_match{clause_tree=X}) -> X.

%% @spec binary_match_max_tag(binary_match()) -> int()
%% @doc Returns themax_tag field from a binary match 
binary_match_max_tag(#binary_match{max_tag=X}) -> X.

%% @type clause_tree(). A clause tree is a tree of instructions
%% each node contains an instruction and a pointer to the success tree
%% and one to the fail tree
-record(clause_tree, {instr, success=[], fail=[]}).
-record(ann_clause_tree, {instr, success=[], fail=[], ann}).
%% @spec clause_tree_instr(binary_match()) -> instr()
%% @doc Returns the instr field from a clause tree 
clause_tree_instr(#clause_tree{instr=X}) -> X.

%% @spec clause_tree_success(clause_tree()) -> clause_tree()
%% @doc Returns the succesful clause tree rooted in this node 
clause_tree_success(#clause_tree{success=X}) -> X.

%% @spec clause_tree_fail(clause_tree()) -> clause_tree()
%% @doc Returns the failure clause tree rooted in this node 
clause_tree_fail(#clause_tree{fail=X}) -> X.

%% @type bin_guard(). Represents a guard for a binary clause
%% containing a set of necessery matches and an outside name
-record(bin_guard, {matches, label}).
%% @spec bin_guard_matches(bin_guard()) -> [match()]
%% @doc Returns the set of matches that need to be performed
%% when this guard is reached 
bin_guard_matches(#bin_guard{matches=X}) -> X.

%% @spec bin_guard_label(bin_guard()) -> [ref()]
%% @doc Returns the outside label name of the guard
bin_guard_label(#bin_guard{label=X}) -> X.

%% @type size(). Represents a size expression containg a
%% constant part, an "operator" part and two different
%% variable parts one for locally defined variables and
%% one for other variables
-record(size, {vars=[],  def_vars=[], const=cerl:c_int(0),  all=cerl:c_atom(false)}).
%% @spec size_const(size()) -> cerl:c_int()
%% @doc Returns the constant part of a size expression
size_const(#size{const=X})-> X.

%% @spec size_all(size()) -> cerl:c_atom(false) | cerl:c_atom(all)
%% @doc Returns cerl:c_atom(all) if the demand on the size is greater than
%% returns cerl:c_atom(false) in other cases
size_all(#size{all=X})-> X.

%% @spec size_vars(size()) -> [cerl:c_var()]
%% @doc Returns a list of the non-locally defined 
%% variables of a size expression
size_vars(#size{vars=X})-> X.

%% @spec size_def_vars(size()) -> [cerl:c_var()]
%% @doc Returns a list of the locally defined 
%% variables of a size expression
size_def_vars(#size{def_vars=X})-> X.

%% @spec size_all_vars(size()) -> [cerl:c_var()]
%% @doc Returns a list of all 
%% variables of a size expression
size_all_vars(#size{vars=X,def_vars=Y})-> X++Y.

%% @type label(). The label data type represents a binary match local label
-record(label, {name}).
%% @spec label_name(label()) -> integer()
%% @doc returns the name of a label
label_name(#label{name=X}) -> X.

%% @type goto(). The goto data type represents a binary match local goto 
%% instruction
-record(goto, {label}).
%% @spec goto_label(goto()) -> integer()
%% @doc returns the name of a label which the goto points to
goto_label(#goto{label=X}) -> X.


-record(hash_cons, {tree=gb_trees:empty(), number=0}).

new_node(Key,Hash=#hash_cons{tree=Tree, number=Number}) ->
  case gb_trees:lookup(Key, Tree) of
    none ->
      LabelName=cerl:c_int(Number),
      {new, LabelName, Hash#hash_cons{tree=gb_trees:insert(Key, LabelName, Tree), number=Number+1}};
    {value, LabelName} ->
      {already_exists, LabelName}
  end.

%% NO DAG OPTIMIZATION

% new_node(Key,Hash=#hash_cons{tree=Tree, number=Number}) ->
%   LabelName=cerl:c_int(Number),
%   {new, LabelName, Hash#hash_cons{tree=gb_trees:enter(Key, LabelName, Tree), number=Number+1}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%End of record definitions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%========================================================================
%% @spec add_offset_to_bin(Binclauses::[clauses()]) -> binary_match()
%% 
%% binary_match() = cerl_binary_pattern_match:binary_match()
%%
%% @doc Turns a list of binclauses into a
%% binary match consisting of a clause tree and a the highest tag value
%% @end
%%========================================================================

add_offset_to_bin(BinClauses) ->
  {BClause0, MaxTag}=iterate_binclauses(BinClauses),
  ITree=create_interference_tree(BClause0),
  %%io:format("~nInterference:~n~p~n", [gb_trees:to_list(ITree)]),
  {ClauseTree, _Hash}=adapt(BClause0, #hash_cons{}, ITree),
  BM = #binary_match{clause_tree=ClauseTree, max_tag=MaxTag},
  %dot(BM),
  BM.

 
%%==========================================================================
%%
%% iterate_binclauses(Clauses) Frees segments from their context and creates
%% tags to show which segments that are similar, returns a b_clause and 
%% the first unused tag
%%
%%==========================================================================


iterate_binclauses(Clauses) ->
  iterate_binclauses(Clauses, 0, gb_trees:empty()).

iterate_binclauses([Clause|Rest], Tag, TagMap) ->
  Guard = cerl:clause_guard(Clause),
  Body = cerl:clause_body(Clause),
  [Bin] = cerl:clause_pats(Clause),
  BinSegs = cerl:binary_segments(Bin),
  {Matches, NewBinSegs, NewTag, NewTagMap} = 
    iterate_binsegs(BinSegs, Tag, TagMap), 
  {Next, FinalTag} = iterate_binclauses(Rest, NewTag, NewTagMap),
  {#b_clause{segments=NewBinSegs, matches=Matches, guard=Guard,  
	    body=Body, next_clause = Next}, FinalTag};
iterate_binclauses([], Tag, _TagMap) ->
  {[], Tag}.

iterate_binsegs(BinSegs, Tag, TagMap) ->
  iterate_binsegs(BinSegs,  Tag, [], [], #size{}, [], [], TagMap).
  
iterate_binsegs([Seg|Rest], Tag, ReadySegs, AccSegs, AccSize, AccMatches, AccDefs, TagMap) ->
  case annotate_seg(Seg, Tag, AccSize, AccDefs, TagMap) of
    {var, {{NewTag, NewTagMap}, ReadNode, Match}, NewAccSize} ->
      iterate_binsegs(Rest, NewTag, ReadySegs, [ReadNode|AccSegs], 
		      NewAccSize, [Match|AccMatches], [Match|AccDefs], NewTagMap);
    {const, {{NewTag, NewTagMap}, ReadNode, MatchNode}, NewAccSize} ->
      iterate_binsegs(Rest, NewTag, ReadySegs, [MatchNode,ReadNode|AccSegs], 
		      NewAccSize, AccMatches, AccDefs, NewTagMap);
    {size, {SizeTest,{var, {{NewTag, NewTagMap}, ReadNode, Match}, NewAccSize}}} ->
      NewReadySegs = ReadySegs ++ [SizeTest|lists:reverse([ReadNode|AccSegs])],
      iterate_binsegs(Rest, NewTag, NewReadySegs, [], NewAccSize, [Match|AccMatches], 
		      [Match|AccDefs], NewTagMap);
    {size, {SizeTest, {const, {{NewTag, NewTagMap}, ReadNode, MatchNode}, NewAccSize}}}->
      NewReadySegs = ReadySegs ++ [SizeTest|lists:reverse([MatchNode,ReadNode|AccSegs])],
      iterate_binsegs(Rest, NewTag, NewReadySegs, [], NewAccSize, AccMatches, 
		      AccDefs, NewTagMap)
  end;
iterate_binsegs([], Tag, ReadySegs, AccSegs, AccSize, AccMatches, _AccDefs, TagMap) ->
  {AccMatches, ReadySegs ++ [AccSize|lists:reverse(AccSegs)], Tag, TagMap}.

annotate_seg(Segment, Tag, Offset, Defs, TagMap) ->
  Type = seg_type(Segment),
  Size = cerl:bitstr_size(Segment),
  Unit = cerl:bitstr_unit(Segment),
  Key=create_key(Type, Size, Unit, Offset),
  {ThisTag, UpdatedTags}=find_tag(Key, Tag, TagMap),
  Val = cerl:bitstr_val(Segment),
  Match = #match{val=Val, tag=ThisTag},
  case size_var_def(Size, Defs) of 
    {true, SizeTag} ->
      NewSize = combine(SizeTag, Unit),
      NextOffset = calc_offset(NewSize,Offset),
      AllOffset = Offset#size{all=cerl:c_atom(all)},
      Read = read_seg(Type, Offset, NewSize, ThisTag),
      Next = case cerl:is_c_var(Val) of
	       true ->
		 {var, {UpdatedTags, Read, Match}, NextOffset};
	       false ->
		 {const, {UpdatedTags, Read, Match}, NextOffset}
	     end,
      {size, {AllOffset, Next}};
    false ->
      NewSize = combine(Size, Unit),
      NextOffset = calc_offset(NewSize,Offset),
      Read = read_seg(Type, Offset, NewSize, ThisTag),
      case cerl:is_c_var(Val) of
	true ->
	  {var, {UpdatedTags, Read, Match}, NextOffset};
	false ->
	  {const, {UpdatedTags, Read, Match}, NextOffset}
      end
  end.

create_key({Sort, Flags}, Size, Unit, Offset) ->
  {cerl:concrete(Sort),
   cerl:concrete(Flags),
   sizekey(Size),
   cerl:concrete(Unit),
   offsetkey(Offset)}.

sizekey(Size) ->
  case cerl:is_c_var(Size) of
    true ->
      cerl:var_name(Size);
    false ->
      cerl:concrete(Size)
  end.

offsetkey(Offset) ->
  {cerl:concrete(size_const(Offset)),
   [{offset_untag(X), cerl:concrete(Y)}||{X,Y} <- size_def_vars(Offset)],
   [{cerl:var_name(X), cerl:concrete(Y)}||{X,Y} <- size_vars(Offset)],
   cerl:concrete(size_all(Offset))}.

offset_untag({tag, Tag}) -> 
  {tag, Tag};
offset_untag(X) ->
  cerl:var_name(X).
 
find_tag(Key, PresTag, TagMap) ->
  case gb_trees:lookup(Key, TagMap) of
    {value, Tag} ->
      {Tag, {PresTag, TagMap}};
    none ->
      {PresTag, {PresTag+1, gb_trees:insert(Key, PresTag, TagMap)}}
  end.

size_var_def(Size, [Def|Rest]) ->
  case same_var(match_val(Def), Size) of
    true ->
      Tag=match_tag(Def),
      {true,{tag, Tag}};
    _ ->
      size_var_def(Size, Rest)
  end;
size_var_def(_Size, []) ->
  false.

same_var(Var1, Var2) ->
  case cerl:is_c_var(Var1) andalso cerl:is_c_var(Var2) of
    true ->
      cerl:var_name(Var1) == cerl:var_name(Var2);
    false ->
      false
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% create_interference_tree(bclause()) -> gb_tree()
%%
%% creates a map from tag pairs to information about how the pair interfere
%% with each other
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_interference_tree(BClause0) ->
  ReadSegs=get_all_unique_read_segments(BClause0, [], []),
  find_interfering(ReadSegs, gb_trees:empty()).

get_all_unique_read_segments(#b_clause{segments=Segs, next_clause=Next}, Acc, Tags) ->
  {NewAcc, NewTags} = it_clause_for_read(Segs, Acc, Tags),
  get_all_unique_read_segments(Next, NewAcc, NewTags);
get_all_unique_read_segments([], Acc, _Tags) ->
  lists:reverse(Acc).

it_clause_for_read([#read_seg{tag=Tag}=Read|Rest], Acc, Tags) ->
  case lists:member(Tag, Tags) of
    true ->
      it_clause_for_read(Rest, Acc, Tags);
    false ->
       it_clause_for_read(Rest, [Read|Acc], [Tag|Tags])
  end;
it_clause_for_read([_|Rest], Acc, Tags) ->
  it_clause_for_read(Rest, Acc, Tags);
it_clause_for_read([], Acc, Tags) ->
  {Acc, Tags}.

find_interfering([ReadSeg|Rest], IntTree) ->
  NewIntTree=find_interfering(Rest, ReadSeg, IntTree),
  find_interfering(Rest, NewIntTree);
find_interfering([], IntTree) ->
  IntTree.

find_interfering([#read_seg{type=Type0, offset=Offset0, size=Size0, tag=Tag0}|Rest],
		 #read_seg{type=Type, offset=Offset, size=Size, tag=Tag}=RSeg,
		 IntTree) ->
  Key=make_search_key(Tag, Tag0),
  Cond  = 
    sameType(Type, Type0) and sameVars(Offset, Offset0) and 
    is_constSize(Size0) and is_constSize(Size),
  if 
    Cond ->
      case interfering(Size, Offset, Tag, Size0, Offset0, Tag0) of
	{true, Inter} ->
	  find_interfering(Rest, RSeg, gb_trees:enter(Key, Inter, IntTree));
	false ->
	  find_interfering(Rest, RSeg, IntTree)
      end;
    true ->
      find_interfering(Rest, RSeg, IntTree)
  end;
find_interfering([], _RSeg, IntTree) ->
  IntTree.

interfering(Size, Offset, Tag, Size0, Offset0, Tag0) ->
  Offs=cerl:concrete(size_const(Offset)),
  Offs0=cerl:concrete(size_const(Offset0)),
  S=cerl:concrete(Size),
  S0=cerl:concrete(Size0),
  if
    S == 0 ; S0 == 0 ->
      false;
    Offs>=Offs0, Offs+S =< Offs0+S0 ->   
      Type = encapsulated,
      Right = Tag,
      Diff = Offs0+S0-Offs-S,
      ISize = S,
      {true, {Type, Right, Diff, ISize}};
    Offs0>=Offs, Offs0+S0 =< Offs+S ->
      Type = encapsulated,
      Right = Tag0,
      Diff = Offs+S-Offs0-S0,
      ISize = S0,
      {true, {Type, Right, Diff, ISize}};
    Offs>=Offs0, Offs+S >= Offs0+S0, Offs0+S0 > Offs  ->
      Type = overlapping,
      Right = Tag,
      Diff = Offs+S-Offs0-S0,
      ISize = Offs0+S0-Offs,
      {true, {Type, Right, Diff, ISize}};
    Offs0>=Offs, Offs0+S0 >= Offs+S, Offs+S > Offs0 ->
      Type = overlapping,
      Right = Tag0,
      Diff = Offs0+S0-Offs-S,
      ISize = Offs+S-Offs0,
      {true, {Type, Right, Diff, ISize}};
    true ->
      false
  end.
  
is_constSize(Size) ->
  cerl:is_c_int(Size).

sameVars(Offset, Offset0) ->
  Dv=[{offset_untag(X), cerl:concrete(Y)}||{X,Y} <- size_def_vars(Offset)],
  Sv=[{cerl:var_name(X), cerl:concrete(Y)}||{X,Y} <- size_vars(Offset)],
  Dv0=[{offset_untag(X), cerl:concrete(Y)}||{X,Y} <- size_def_vars(Offset0)],
  Sv0=[{cerl:var_name(X), cerl:concrete(Y)}||{X,Y} <- size_vars(Offset0)],
  (Dv == Dv0) and (Sv==Sv0).

sameType({Sort, Flags}, {Sort0, Flags0}) ->
  T0=cerl:concrete(Sort0),
  T=cerl:concrete(Sort),
  F0=cerl:concrete(Flags0),
  F=cerl:concrete(Flags),
  case {T,T0,F==F0} of
    {integer, integer, true} ->
      lists:member(unsigned,F) and lists:member(big,F);
    _ ->
      false
  end.
	  
%%========================================================================
%%
%% adapt(BClause, Hash, ITree) creates the clause tree dag by synthesizing
%% a traversal order and pruning all irrelevant segments at each iteration
%%
%%=========================================================================

adapt([], Hash, _ITree)->
  {[], Hash};
adapt(BClause, Hash, ITree) ->
  BClause0=BClause,
  case new_node(BClause0, Hash) of
    {new, NewLabelName, Hash1} ->
      Lbl=#label{name=NewLabelName},
      {Succ, Hash2}  = adapt0(BClause0, Hash1, ITree),
      {#clause_tree{instr=Lbl, success=Succ}, Hash2};
    {already_exists, LabelName} ->
      Goto=#goto{label=LabelName},
      {#clause_tree{instr=Goto}, Hash}
  end.
      
adapt0(#b_clause{segments=[], matches=Matches, guard=Guard, body=Body, next_clause=NextClause}, 
       Hash,  ITree) ->
  Ref = cerl:concrete(get_ref(Guard)),
  NewRef = make_newref(Ref),
  NewGuard = update_guard(Guard, NewRef),
  NewBody = update_body(Body, Ref, NewRef),
  {Fail, Hash1} = adapt(NextClause, Hash, ITree),
  {#clause_tree{instr=#bin_guard{matches=Matches,label=NewGuard}, success=NewBody, fail=Fail}, Hash1};
adapt0(BClause, Hash, ITree) ->
  BinSeg = choose_binseg(BClause),
  case BinSeg of
    #match{} ->
      MatchSet=get_all_similar_match(BinSeg,BClause),
      case gb_sets:size(MatchSet) of
	1 ->
	  NewSuccClause=remove_succ_match(BinSeg, BClause, ITree),
	  {Succ, Hash1} = adapt(NewSuccClause, Hash, ITree),
	   NewFailClause = remove_fail_match(BinSeg, BClause, ITree),
	  {Fail, Hash2} = adapt(NewFailClause, Hash1, ITree),
	  {#clause_tree{instr=BinSeg, success=Succ, fail=Fail}, Hash2};
	_ ->
	  {SuccList,NewInstr,Hash1}=create_success_list(MatchSet, Hash, BClause, ITree),
	  NewFailClause = remove_all_fail_match(MatchSet, BClause, ITree),
	  {Fail, Hash2} = adapt(NewFailClause, Hash1, ITree),
	  {#clause_tree{instr=NewInstr, success=SuccList, fail=Fail}, Hash2}
      end;
    #read_seg{} ->
      NewClause = remove_seg(BinSeg, BClause),
      {Succ, Hash1} = adapt(NewClause, Hash, ITree),
      {#clause_tree{instr=BinSeg, success=Succ}, Hash1};
    #size{} -> 
      {Succ, Hash1} = adapt(prune_compatible_size(BClause, BinSeg), Hash, ITree),
      {Fail, Hash2} = adapt(prune_incompatible_size(BClause, BinSeg), Hash1, ITree),
      {#clause_tree{instr=BinSeg, success=Succ, fail=Fail}, Hash2} 
  end.


get_all_similar_match(Match, BClause) ->
  List=get_all_similar_match(Match, BClause, []),
  gb_sets:from_list(List).

get_all_similar_match(Match, #b_clause{segments=BinSegs, next_clause=Next}, Acc) ->
  NewAcc=get_match(Match, BinSegs, Acc),
  get_all_similar_match(Match, Next, NewAcc);
get_all_similar_match(_Match, [], Acc) ->
  Acc.

get_match(Match1=#match{tag=Tag}, [Match2=#match{tag=Tag}|Rest], Acc) ->
  case have_same_match(Match2, Acc) of
    true ->
      get_match(Match1, Rest, Acc);
    false ->
      get_match(Match1, Rest, [Match2|Acc])
  end;
get_match(Match, [_|Rest], Acc) ->
  get_match(Match, Rest, Acc);
get_match(_Match, [], Acc) ->
  Acc.

have_same_match(Match1, [Match2|Rest]) ->
  Val1 = cerl:concrete(match_val(Match1)),
  Val2 = cerl:concrete(match_val(Match2)),
  case Val1 == Val2 of
    true ->
      true;
    false ->
      have_same_match(Match1, Rest)
  end;
have_same_match(_Match1, []) ->  
  false.
    
create_success_list(MatchSet, Hash, BClause, ITree) ->
  MatchList=gb_sets:to_list(MatchSet),
  Tag=match_tag(hd(MatchList)),
  make_successor_list(MatchList, Hash, BClause, #match_group{tag=Tag, vals=[]}, 
		      gb_trees:empty(), ITree).

make_successor_list([Match=#match{val=Val}|Rest], Hash, BClause, 
		    MatchGroup=#match_group{vals=Acc}, SuccMap, ITree) ->
  NewClause=remove_succ_match(Match, BClause, ITree),
  {Succ, Hash1}=adapt(NewClause, Hash, ITree),
  make_successor_list(Rest, Hash1, BClause,MatchGroup#match_group{vals=[Val|Acc]},
		      gb_trees:insert(Val, Succ, SuccMap), ITree);
make_successor_list([], Hash, _BClause, MatchGroup, SuccMap, _ITree) ->
  {SuccMap, MatchGroup, Hash}.

remove_all_fail_match(MatchSet, BClause, ITree) ->
  MatchList=gb_sets:to_list(MatchSet),
  Fun = fun(X, Y) ->
	    remove_fail_match(X,Y,ITree)
	end,
  lists:foldl(Fun, BClause, MatchList).

%%------------------------------------------------------------------------
%%
%% The following remove functions are used for size pruning
%%
%% remove_tests removes size tests which are true if a certain size test is
%% true
%%
%% remove_clauses removes clauses that contain a size test that is false
%% if a certain size test is false
%%
%%------------------------------------------------------------------------- 

prune_compatible_size(BClause=#b_clause{segments=BinSegs,next_clause=Next}, Size) ->
  case remove_tests(BinSegs, Size, []) of
    fail ->
      prune_compatible_size(Next, Size);
    NewSegs ->
      BClause#b_clause{segments=NewSegs, next_clause=prune_compatible_size(Next, Size)}
  end;
prune_compatible_size([], _Size) ->
  [].


remove_tests([SizeExpr=#size{all=AlliTerm}|Rest], Size=#size{all=AllTerm}, Acc) ->
  Alli = cerl:concrete(AlliTerm),
  All = cerl:concrete(AllTerm),
  Eq = false,
  Geq = all,
  case {All, Alli} of
    {Geq, Geq} ->
      case statically_bigger_or_equal(Size, SizeExpr) of
	true ->
	  remove_tests(Rest, Size, Acc);
	false ->
	  remove_tests(Rest, Size, [SizeExpr|Acc])
      end;
    {Geq, Eq} ->
      case statically_bigger(Size, SizeExpr) of
	true ->
	  fail;
	false ->
	  remove_tests(Rest, Size, [SizeExpr|Acc])
      end;
    {Eq, Geq} ->
      case statically_bigger(SizeExpr, Size) of
	true ->
	  fail;
	false ->
	  case statically_bigger_or_equal(SizeExpr, Size) of
	    true ->
	       remove_tests(Rest, Size, Acc);
	    false ->
	      remove_tests(Rest, Size, [SizeExpr|Acc])
	  end
      end;
    {Eq, Eq} ->
      case statically_different(Size, SizeExpr) of
	true ->
	  fail;
	false ->
	  case statically_equal(Size, SizeExpr) of
	    true ->
	       remove_tests(Rest, Size, Acc);
	    false ->
	      remove_tests(Rest, Size, [SizeExpr|Acc])
	  end
      end
  end;

remove_tests([Seg|Rest], Size, Acc) ->
  remove_tests(Rest, Size, [Seg|Acc]);
remove_tests([], _Size, Acc) ->
  lists:reverse(Acc).

prune_incompatible_size(BClause=#b_clause{segments=BinSegs, next_clause=Next}, Size) ->
  case [X || X <- BinSegs, is_incompatible(X, Size)] of
    [] ->
      BClause#b_clause{next_clause=prune_incompatible_size(Next, Size)};
    _ ->
      prune_incompatible_size(Next, Size)
  end;
prune_incompatible_size([], _Size) ->
  [].

is_incompatible(SizeExpr=#size{all=AlliTerm}, Size=#size{all=AllTerm}) ->
  Alli = cerl:concrete(AlliTerm),
  All = cerl:concrete(AllTerm),
  Eq = false,
  Geq = all,
  case {All, Alli} of
    {Geq, _} ->
      statically_bigger_or_equal(SizeExpr, Size);
    {Eq, Eq} ->
      statically_equal(SizeExpr, Size);
    _ ->
      false
  end;
is_incompatible(_,_) ->
  false.


%%-----------------------------------------------------------------------------
%%
%% remove_seg removes prunes all similar read actions
%%
%%-----------------------------------------------------------------------------

remove_seg(BinSeg, BClause=#b_clause{segments=BinSegs, next_clause=Next}) ->
  NewBinSegs  = remove_same_seg(BinSegs, tag(BinSeg)),
  BClause#b_clause{segments=NewBinSegs, next_clause=remove_seg(BinSeg, Next)};

remove_seg(_BinSeg, []) ->
  [].

remove_same_seg([#read_seg{tag=Tag}|Rest], Tag) ->
  remove_same_seg(Rest, Tag);
remove_same_seg([BinSeg|Rest], Tag) ->
  [BinSeg|remove_same_seg(Rest, Tag)];
remove_same_seg([], _Tag) ->
  [].

%%------------------------------------------------------------------------
%%
%% The following remove functions are used for match pruning
%%
%% remove_succ_match removes matches which are true if a certain match is
%% true and clauses containing a match that is false 
%%
%% remove_fail_match removes clauses that contain a match that is false
%% if a certain match is false
%%
%%------------------------------------------------------------------------- 

remove_succ_match(Match, BClause=#b_clause{segments=BinSegs, next_clause=Next}, ITree) -> 
  case mismatch(Match, BinSegs, ITree) of
    true ->
      remove_succ_match(Match, Next, ITree);
    NewBinSegs ->
      BClause#b_clause{segments=NewBinSegs, next_clause=remove_succ_match(Match, Next, ITree)} 
  end;
remove_succ_match(_Match, [], _ITree) ->
  [].

mismatch(Match, BinSegs, ITree) ->
  mismatch(Match,BinSegs, [], ITree).

mismatch(Match=#match{tag=Tag, val=Val1}, [#match{tag=Tag, val=Val2}|Rest], Acc, ITree) ->
  case cerl:concrete(Val1)==cerl:concrete(Val2) of
    true -> mismatch(Match, Rest, Acc, ITree);
    false -> true
  end;
mismatch(Match=#match{tag=Tag1, val=Val1}, [#match{tag=Tag2, val=Val2}=First|Rest], Acc, ITree) ->
  V1 = cerl:concrete(Val1),
  V2 = cerl:concrete(Val2),
  SearchKey = make_search_key(Tag1, Tag2),
  case gb_trees:lookup(SearchKey, ITree) of
    {value, Inter} ->
      case interference_conclusion_pos(Inter, Tag1, Tag2, V1, V2) of
	fails ->
	  true;
	succeeds ->
	  mismatch(Match, Rest, Acc, ITree);
	no_info -> 
	  mismatch(Match, Rest, [First|Acc], ITree)
      end;
    none ->
      mismatch(Match, Rest, [First|Acc], ITree)
  end;	   
mismatch(Match, [First|Rest], Acc, ITree) ->
  mismatch(Match, Rest, [First|Acc], ITree);
mismatch(_Match, [], Acc, _ITree) ->
  lists:reverse(Acc).

interference_conclusion_pos({encapsulated, Tag1, Diff, Size},
  			    Tag1, _Tag2, V1, V2) ->
    C2 = (V2 bsr Diff) band ((1 bsl Size) - 1), 
    C1 = V1,
    case C1 == C2 of
      true ->
        no_info;
      false ->
        fails
    end;
interference_conclusion_pos({encapsulated, Tag2, Diff, Size},
 			    _Tag1, Tag2, V1, V2) ->
   C2 = V2, 
   C1 = (V1 bsr Diff) band ((1 bsl Size) - 1),
   case C1 == C2 of
     true ->
       succeeds;
     false ->
       fails
   end;
interference_conclusion_pos({overlapping, Tag1, Diff, Size},
 			    Tag1, _Tag2, V1, V2) ->
   C1 = (V1 bsr Diff) band ((1 bsl Size) - 1),
   C2 = V2 band ((1 bsl Size) - 1),
   case C1 == C2 of
     true ->
       no_info;
     false ->
       fails
   end;
interference_conclusion_pos({overlapping, Tag2, Diff, Size},
 			    _Tag1, Tag2, V1, V2) ->
   C2 = (V2 bsr Diff) band ((1 bsl Size) - 1),
   C1 = V1 band ((1 bsl Size) - 1),
   case C1 == C2 of
     true ->
       no_info;
     false ->
       fails
   end;
interference_conclusion_pos(_,_,_,_,_) ->
  no_info.

make_search_key(Tag1, Tag2) ->
  if Tag1 > Tag2 ->
      {Tag2, Tag1};
     true ->
      {Tag1, Tag2}
  end.

remove_fail_match(Match, BClause=#b_clause{segments=BinSegs, next_clause=Next}, ITree) -> 
  case match(Match, BinSegs, ITree) of
    true ->
      remove_fail_match(Match, Next, ITree);
    false ->
      BClause#b_clause{next_clause=remove_fail_match(Match, Next, ITree)} 
  end;
remove_fail_match(_Match, [], _ITree) ->
  [].

match(Match=#match{tag=Tag, val=Val1}, [#match{tag=Tag, val=Val2}|Rest], ITree) ->
  case cerl:concrete(Val1)==cerl:concrete(Val2) of
    true -> true;
    false -> match(Match, Rest, ITree)
  end;
match(Match=#match{tag=Tag1, val=Val1}, [#match{tag=Tag2, val=Val2}|Rest], ITree) ->
   V1 = cerl:concrete(Val1),
  V2 = cerl:concrete(Val2),
  SearchKey = make_search_key(Tag1, Tag2),
  case gb_trees:lookup(SearchKey, ITree) of
    {value, Inter} ->
      case interference_conclusion_neg(Inter, Tag1, Tag2, V1, V2) of
	fails ->
	  true;
	no_info -> 
	  match(Match, Rest, ITree)
      end;
    none ->
      match(Match, Rest, ITree)
  end;	   
match(Match, [_|Rest], ITree) ->
  match(Match, Rest, ITree);
match(_Match, [], _ITree) ->
  false.

interference_conclusion_neg({encapsulated, Tag1, Diff, Size},
			    Tag1, _Tag2, V1, V2) ->
  C1 = V1, 
  C2 = (V2 bsr Diff) band ((1 bsl Size) - 1),
  case C1 == C2 of
    true ->
      fails;
    false ->
      no_info
  end;
interference_conclusion_neg(_Inter, _Tag1, _Tag2, _V1, _V2) ->
  no_info.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% choose_binseg/1 will chose the binseg or match with the 
%% highest count from a list, if there are binsegs and matches 
%% with the same count a binseg is chosen
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_binseg(BClause) ->
  CountTree=simpl_count(BClause),
  BinSegs=b_clause_segments(BClause),
  get_largest(BinSegs, BClause, CountTree).

always_read(BinSegs, CountTree, BClause) ->
  No=no_of_clauses(BClause),
  find_binseg(BinSegs, CountTree, No).
    
find_binseg([BinSeg=#read_seg{}|Rest], CountTree, No) ->
  Tag=tag(BinSeg),
  case gb_trees:lookup(Tag, CountTree) of
    {value, {No, _, _, _}} ->
      {true, BinSeg};
    _ ->
      find_binseg(Rest, CountTree, No)
  end;
find_binseg([_|Rest], CountTree, No) ->
  find_binseg(Rest, CountTree, No);
find_binseg([], _CountTree, _No) ->
  false.

no_of_clauses(#b_clause{next_clause=Next}) ->
  1+no_of_clauses(Next);
no_of_clauses([]) ->
  0.

-ifdef(LEFT).
get_largest(BinSegs=[FirstSeg|_], BClause, CountTree) ->
  Seg=
    case always_read(BinSegs, CountTree, BClause) of
      {true, BinSeg} -> BinSeg;
      false -> FirstSeg
    end,
  legalize(Seg, BinSegs).
-else.
get_largest(BinSegs, BClause, CountTree) ->
  Seg=
    case always_read(BinSegs, CountTree, BClause) of
      {true, BinSeg} -> BinSeg;
      false ->
	get_largest_count(BinSegs, CountTree, {none, 0})
    end,  
  legalize(Seg, BinSegs).
-endif.

-ifdef(INDEXNESS).
get_largest_count([BinSeg|Rest], CountTree, Top={_,Count}) -> 
  NewTop = 
    case BinSeg of
      #match{} ->
	Tag=tag(BinSeg),
	{value, {_, _, IC, _}} = gb_trees:lookup(Tag, CountTree),
	case IC >= Count of
	  true -> {BinSeg, IC};
	false -> Top
	end;
      #read_seg{} ->
	case Count of
	  0 -> {BinSeg, 1};
	  _ -> Top
	end; 
      #size{} ->
	{value, {_, _, IC, _}} = gb_trees:lookup(BinSeg, CountTree),
	case IC >= Count of
	  true -> {BinSeg, IC};
	  false -> Top
	end
    end,
  get_largest_count(Rest, CountTree, NewTop);
get_largest_count([], _CountTree, {BinSeg, _Count}) ->
  BinSeg.
-endif.

-ifdef(USES_MATCH).
get_largest_count([BinSeg|Rest], CountTree, Top={_,Count}) -> 
  NewTop =
    case BinSeg of
      #match{} ->
	Tag=tag(BinSeg),
	{value, {_, MC, _, _}} = gb_trees:lookup(Tag, CountTree),
	case MC >= Count of
	  true -> {BinSeg, MC};
	  false -> Top
	end;
      #read_seg{} ->
	case Count of
	  0 -> {BinSeg, 1};
	  _ -> Top
	end;
      #size{} ->
	{value, {_, MC, _, _}} = gb_trees:lookup(BinSeg, CountTree),
	case MC >= Count of
	  true -> {BinSeg, MC};
	  false -> Top
	end
    end,
  get_largest_count(Rest, CountTree, NewTop);
get_largest_count([], _CountTree, {BinSeg, _Count}) ->
  BinSeg.
-endif.

-ifdef(USES_READ).
get_largest_count([BinSeg|Rest], CountTree, "uses_read", Top={_,Count}) -> 
  NewTop =
    case BinSeg of
      #match{} ->
	Tag=tag(BinSeg),
	{value, {_, MC, _, _}} = gb_trees:lookup(Tag, CountTree),
	NewMC = 1+MC/1000,
	case NewMC>Count of
	  true -> {BinSeg, NewMC};
	  false -> Top
	end;
      #read_seg{} ->
	Tag=tag(BinSeg),
	{value, {RC, _, _, _}} = gb_trees:lookup(Tag, CountTree),
	case RC > Count of
	  true -> {BinSeg, RC};
	  false -> Toptrue 
	end;
    #size{} ->
      {value, {_, MC, _, _}} = gb_trees:lookup(BinSeg, CountTree),
      NewMC = 1+MC/1000,
      case NewMC>Count of
	true -> {BinSeg, NewMC};
	false -> Top
      end;
  end,
  get_largest_count(Rest, CountTree, NewTop);
get_largest_count([], _CountTree, {BinSeg, _Count}) ->
  BinSeg.
-endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utility functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Simpl count creates a mapping between segments and different 
% frequency valued used to synthesize the traversal order
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simpl_count(BinClauses) ->
  simpl_count(BinClauses, gb_trees:empty()).

simpl_count(#b_clause{segments=BinSegs, next_clause=Next}, Tree) ->
  NewTree = simpl_count_segs(BinSegs, Tree),
  simpl_count(Next, NewTree);

simpl_count([], Tree) ->
  Tree.

simpl_count_segs([Seg=#match{}|Rest], Tree) ->
  Tag = match_tag(Seg),
  Val = match_val(Seg),
  case gb_trees:lookup(Tag, Tree) of
    {value, {SC, MC, IC, Set}} ->
      case gb_sets:is_member(Val, Set) of
	false ->
	  simpl_count_segs(Rest, gb_trees:update(Tag, {SC, MC+1, IC+1, gb_sets:add(Val, Set)}, Tree));
	true ->
	  simpl_count_segs(Rest, gb_trees:update(Tag, {SC, MC+1, IC, Set}, Tree))
      end;
    none ->
      simpl_count_segs(Rest, gb_trees:insert(match_tag(Seg), {0,1,1,gb_sets:singleton(Val)}, Tree))
  end;

simpl_count_segs([Seg=#read_seg{}|Rest], Tree) ->
  Tag = read_seg_tag(Seg),
  case gb_trees:lookup(Tag, Tree) of
    {value, {SC, MC, IC, Set}} ->
      simpl_count_segs(Rest, gb_trees:update(Tag, {SC+1, MC, IC, Set}, Tree));
    none ->
      simpl_count_segs(Rest, gb_trees:insert(Tag, {1,0,0,gb_sets:empty()}, Tree))
  end;

simpl_count_segs([Seg=#size{}|Rest], Tree) ->
  case gb_trees:lookup(Seg, Tree) of
    {value, {SC, MC, IC, Set}} ->
      simpl_count_segs(Rest, gb_trees:update(Seg, {SC, MC+1, IC, Set}, Tree));
    none ->
      simpl_count_segs(Rest, gb_trees:insert(Seg, {0,1,1,gb_sets:empty()}, Tree))
  end;

simpl_count_segs([], Tree) ->
  Tree.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% legalize makes sure that a binseg selection is legal, if it is not, a legal 
%% alternative is returned
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


legalize(BinSeg=#match{tag=Tag}, BinSegs) ->
  case has_read(Tag, BinSegs) of
    {true, ReadSeg} ->
      legalize(ReadSeg, BinSegs);
    false ->
      BinSeg
  end;

legalize(BinSeg=#read_seg{}, BinSegs) ->
  case size_precedes_read(BinSegs, BinSeg) of
    {true, Size} ->
      legalize(Size, BinSegs);
    false ->
      BinSeg
  end;
legalize(BinSeg=#size{}, BinSegs) ->
  case size_allowed(BinSegs, BinSeg) of
    {false, Read} ->
      legalize(Read, BinSegs);
    true ->
      BinSeg
  end.

has_read(Tag, [BinSeg=#read_seg{tag=Tag}|_]) ->
  {true,BinSeg};
has_read(Tag, [_|Rest]) ->
  has_read(Tag, Rest);
has_read(_, []) ->
  false.

size_precedes_read([Size=#size{}|_], _BinSeg) ->
  {true, Size};
size_precedes_read([BinSeg|_], BinSeg) ->
  false;
size_precedes_read([_|Rest], BinSeg) ->
  size_precedes_read(Rest, BinSeg).

size_allowed([#read_seg{}=Read|Rest], Size) -> 
  case size_need_read(Size, Read) of
    false ->
      size_allowed(Rest, Size);
    true ->
      {false, Read}
  end;
size_allowed([_|Rest], Size) ->
  size_allowed(Rest, Size);
size_allowed([], _Size) ->
  true.

size_need_read(#size{def_vars=Defs}, #read_seg{tag=Tag}) ->
  case [X || {{tag, X},_} <- Defs, X==Tag] of
    [] ->
      false;
    _ ->
      true
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% combine(Size, Unit) returns a new size value either Size*Unit
%% or if Size is not an integer {Size, Unit}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combine(Size, Unit) ->
  case cerl:is_c_int(Size) of
    true ->
      cerl:c_int(cerl:int_val(Size)*cerl:int_val(Unit));
    false ->
      {Size, Unit}
  end.

calc_offset({{tag, Tag}, Unit}, SizeExpr=#size{def_vars=DefVars}) ->
  SizeExpr#size{def_vars=[{{tag,Tag}, Unit}|DefVars]};

calc_offset({Size, Unit}, SizeExpr=#size{vars=Vars}) ->
  case cerl:is_c_atom(Size) of
    true ->
      all = cerl:atom_val(Size),
      SizeExpr#size{all=Size};
    false ->
      true = cerl:is_c_var(Size),
      SizeExpr#size{vars=[{Size, Unit}|Vars]}
  end;

calc_offset(Size, SizeExpr=#size{const=Const}) ->
  SizeExpr#size{const=cerl:c_int(cerl:int_val(Size)+cerl:int_val(Const))}.
      
seg_type(Segment) ->      
  Type=cerl:bitstr_type(Segment),   
  Flags=cerl:bitstr_flags(Segment),
  {Type, Flags}.

statically_equal(Size1=#size{const=Const1},
		 Size2=#size{const=Const2}) ->
  C1 = cerl:concrete(Const1),
  C2 = cerl:concrete(Const2),
  Var1=size_all_vars(Size1),
  Var2=size_all_vars(Size2),
  same_set(Var1, Var2) and (C1 == C2).

statically_bigger(Size1=#size{const=Const1}, Size2=#size{const=Const2}) ->
  C1 = cerl:concrete(Const1),
  C2 = cerl:concrete(Const2),
  Var1=size_all_vars(Size1),
  Var2=size_all_vars(Size2),
  (C1 > C2) and subset(Var2, Var1). 

statically_bigger_or_equal(Size1=#size{const=Const1},Size2=#size{const=Const2}) ->
  C1 = cerl:concrete(Const1),
  C2 = cerl:concrete(Const2),
  Var1=size_all_vars(Size1),
  Var2=size_all_vars(Size2),
  (C1 >= C2) and subset(Var2, Var1).

statically_different(Size1, Size2) ->
  statically_bigger(Size1, Size2) or statically_bigger(Size2, Size1).

same_set([First|Rest], List) ->
  case lists:member(First, List) of
    true ->
      same_set(Rest, lists:delete(First, List));
    false ->
      false
  end;

same_set([], []) ->
  true;
same_set([], _) ->
  false.

subset([First|Rest], List) ->
  case lists:member(First, List) of
    true ->
      subset(Rest, lists:delete(First, List));
    false ->
      false
  end;

subset([], _List) ->
  true.

tag(T=#read_seg{}) ->
  read_seg_tag(T);
tag(T=#match{}) ->
  match_tag(T);
tag(#size{}) -> 
  [].

%% @spec instr_type(instr()) -> atom()
%% @doc returns the atom describing the instruction type

instr_type(Instr) ->
  case Instr of
    #read_seg{} ->
      read_seg;
    #bin_guard{} ->
      bin_guard;
    #size{} ->
      size;
    #match{} ->
      match;
    #label{} ->
      label;
    #goto{} ->
      goto;
    #match_group{} ->
      match_group
  end.

get_ref(Guard) ->
  [Arg] = cerl:primop_args(Guard),
  Arg.

make_newref(Ref) ->
  case get(joint_unique_ref) of
    undefined ->
      put(joint_unique_ref, 1),
      {Ref, 0};
    N ->
      put(joint_unique_ref, N+1),
      {Ref, N}
  end.

update_guard(Guard, NewRef) ->
  cerl:update_c_primop(Guard,cerl:primop_name(Guard), 
		       [cerl:abstract(NewRef)]).

update_body(Expr, Ref, NewRef) ->
  Fun =
    fun(E) ->
	case cerl:type(E) of
	  primop ->
	    case {cerl:atom_val(cerl:primop_name(E)), cerl:primop_args(E)} of
	      {?PRIMOP_GOTO_LABEL, [Arg]} ->
		case cerl:concrete(Arg) of
		  Ref ->
		    cerl:update_c_primop(E,cerl:primop_name(E), 
					 [cerl:abstract(NewRef)]);
		  _X ->
		    %% io:format("X:~w Ref:~w~n", [_X, Ref]),
		    E
		end;
	      _ ->
		E
	    end;
	  _ ->
	    E
	end
    end,
  cerl_trees:map(Fun, Expr).

-ifdef(BIN_PMATCH_DEBUG).

annotate_clause_tree(#clause_tree{instr=I, success=Succ, fail=Fail}, N) ->
  {ACT1, N1} = annotate_clause_tree(Succ,N+1),
  {ACT2, N2} = annotate_clause_tree(Fail,N1),
  {#ann_clause_tree{instr=I, success=ACT1, fail=ACT2, ann=N}, N2};
annotate_clause_tree(Val,N) ->
  {Val,N}.

remove_fail([{_,"Fail"}|Rest], LabelTree) ->
  remove_fail(Rest,LabelTree);
remove_fail([{X,[{label,Key}]}|Rest], LabelTree) ->
  {value,Name}=gb_trees:lookup(Key, LabelTree),
  [{X,lists:flatten(Name)}|remove_fail(Rest,LabelTree)];
remove_fail([X|Rest], LabelTree) ->
  [X|remove_fail(Rest,LabelTree)];
remove_fail([], _L) ->
  [].

remove_options([{_,"Fail",_}|Rest], LabelTree) -> 
  remove_options(Rest,LabelTree);
remove_options([{X,[{label,Key}],Opt}|Rest], LabelTree) ->
  {value,Name}=gb_trees:lookup(Key, LabelTree),
  [{X,lists:flatten(Name),Opt}|remove_options(Rest,LabelTree)];
remove_options([X|Rest], LabelTree) ->
  [X|remove_options(Rest, LabelTree)];
remove_options([], _LabelTree) ->
  [].

dot(#binary_match{clause_tree=CT}) ->
  {ACT,_} = annotate_clause_tree(CT, 0),
  {LTree, Acc,Opts}=dot(ACT, gb_trees:empty(), [], []),
  Dot=remove_fail(Acc,LTree),
  DotOpts=remove_options(Opts,LTree),
  hipe_dot:translate_list(Dot, ?DOT_FILE, "Stupid_Name", 
			  fun(X)->X end,DotOpts).

dot(#ann_clause_tree{instr=I, success=Succ, fail=Fail}=Self, 
    LabelTree, Acc, Opts) -> 
  case I of
    #label{name=X} ->
      Int=cerl:concrete(X),
      NewLabelTree=gb_trees:insert(Int, get_node_name1(Succ, LabelTree), 
				   LabelTree),
      dot(Succ, NewLabelTree, Acc, Opts);
    #size{} ->
      SelfName=get_node_name(Self, LabelTree),
      SuccName=get_node_name(Succ, LabelTree),
      SuccOpt = make_succ_opt({SelfName,SuccName}),
      {NewLabelTree, NewAcc, NewOpts} = dot(Succ, LabelTree, 
				   [{SelfName,SuccName}|Acc],
				  [SuccOpt|Opts]),
      FailName= get_node_name(Fail, LabelTree),
      FailOpt = make_fail_opt({SelfName,FailName}),
      dot(Fail, NewLabelTree,[{SelfName,FailName}|NewAcc],
	 [FailOpt|NewOpts]);
    #match{} ->
      SelfName=get_node_name(Self, LabelTree),
      SuccName=get_node_name(Succ, LabelTree),
      SuccOpt = make_succ_opt({SelfName,SuccName}),
      {NewLabelTree, NewAcc, NewOpts} = dot(Succ, LabelTree, 
				   [{SelfName,SuccName}|Acc],
				  [SuccOpt|Opts]),
      FailName= get_node_name(Fail, LabelTree),
      FailOpt = make_fail_opt({SelfName,FailName}),
      dot(Fail, NewLabelTree,[{SelfName,FailName}|NewAcc],
	  [FailOpt|NewOpts]);
    #read_seg{} ->
      SelfName=get_node_name(Self, LabelTree),
      SuccName=get_node_name(Succ, LabelTree),
      dot(Succ, LabelTree, [{SelfName,SuccName}|Acc],Opts);
    #bin_guard{} ->
      SelfName=get_node_name(Self, LabelTree),
      BgOpt = make_bin_guard_opt(SelfName),
      {LabelTree, Acc, [BgOpt|Opts]};
      %% dot(Fail, LabelTree,Acc,[BgOpt|Opts]);
    #goto{} ->
      {LabelTree, Acc, Opts}
  end;
dot([], LabelTree, Acc, Opts) ->
  {LabelTree, Acc, Opts}.

make_succ_opt({N1,N2}) ->
  {N1,N2,{color,green}}.

make_fail_opt({N1,N2}) ->
  {N1,N2,{color,red}}.

make_bin_guard_opt(Node) ->
  {Node, {shape,rectangle}}.

get_node_name(CT, LT) ->
  lists:flatten(get_node_name1(CT,LT)).
  
get_node_name1(#ann_clause_tree{instr=I, success=Succ, ann=N}, LabelTree) ->
  case I of
    #label{} ->
      get_node_name1(Succ, LabelTree);
    #size{vars=[], const=X, all=C_atom} ->
      Sign =
	case cerl:concrete(C_atom) of
	  all ->
	    ">=";
	  false ->
	    "="
	end,
      Int=cerl:concrete(X),
      io_lib:format("size ~s ~w, (~w)", [Sign, Int, N]);
    #size{vars=Vars, const=X, all=C_atom} ->
      Sign =
	case cerl:concrete(C_atom) of
	  all ->
	    ">=";
	  false ->
	    "="
	end,
      Int=cerl:concrete(X),
      io_lib:format("size ~s ~w + ~w (~w)", [Sign, Int, Vars,N]);
    #match{val=Val, tag=Tag} ->
      CVal=cerl:concrete(Val),
      io_lib:format("r~w == ~w (~w)", [Tag,CVal,N]);
    #read_seg{tag=Tag, offset=Off, size=Size} ->
      Sz = 
	case cerl:is_c_int(Size) of
	  true -> cerl:concrete(Size);
	  false -> {Size1, Unit} = Size,
		   case cerl:is_c_atom(Size1) of
		     true ->
		       all;
		     _ ->
		       io_lib:format("~w*~w", [Size1,cerl:concrete(Unit)])
		   end	 
	end,
      case Off of
	#size{vars=[], const=X} ->
	  Int=cerl:concrete(X),
	  io_lib:format("r~w = r(~w, ~w) (~w)", [Tag, Int, Sz, N]);
	#size{vars=Vars, const=X} ->
	  Int=cerl:concrete(X),
	  io_lib:format("r~w = read(~w + ~w, ~w) (~w)", [Tag, Vars, Int, Sz, N])
      end;
    #bin_guard{label=Lbl} ->
      [Arg]=cerl:primop_args(Lbl),
      io_lib:format("Match ~w Succeded (~w)", [cerl:concrete(Arg), N]);
    #goto{label=X} ->
      [{label,cerl:concrete(X)}]     
  end;
get_node_name1([], _LabelTree) ->
  io_lib:format("Fail",[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @spec pp(binary_match()|clause_tree())-> atom()
%% @doc pretty prints a clause_tree or the clause_tree of a binary_match
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     

pp(#binary_match{clause_tree=CT}) ->
  pp(CT);

pp(#clause_tree{instr=I, success=Succ, fail=Fail}) -> 
  case I of
    #label{name=X} ->
      Int=cerl:concrete(X),
      io:format("Label: ~w~n", [Int]),
      pp(Succ);
    #size{vars=Vars, const=X, all=C_atom} ->
      Sign =
      case cerl:concrete(C_atom) of
	all ->
	  ">=";
	false ->
	  "="
      end,
      Int=cerl:concrete(X),
      io:format("Case size ~s ~w + ~w ~n", [Sign, Int, Vars]),
      do_split(Succ, Fail);
    #match{val=Val, tag=Tag} ->
      CVal=cerl:concrete(Val),
      io:format("Case r~w == ~w~n", [Tag,CVal]),
      do_split(Succ, Fail);
    #match_group{vals=Vals, tag=Tag} ->
      io:format("Case r~w of~n", [Tag]),
      Succs = do_split_match_group(Vals, Succ, Fail),
      [pp(X)||X<-Succs];
    #read_seg{tag=Tag, offset=Off, size=Size} ->
      Sz = 
	case cerl:is_c_int(Size) of
	  true -> cerl:concrete(Size);
	  false -> Size
	end,
      #size{vars=Vars, const=X} = Off,
      Int=cerl:concrete(X),
      io:format("r~w = read(~w + ~w, ~w)~n", [Tag, Vars, Int, Sz]),
      Lbl = get_label(Succ),
      io:format("goto ~w~n~n", [Lbl]),
      pp(Succ);
    #bin_guard{label=Lbl} ->
      io:format("Matched pattern: ~w~n~n", [Lbl]);
    #goto{label=X} ->
      Lbl=cerl:concrete(X),
      io:format("goto ~w~n~n", [Lbl])
  end.

do_split_match_group([Val|Vals], SuccMap, Fail) ->
  Succ=gb_trees:get(Val, SuccMap),
  CVal = cerl:concrete(Val),
  SL = get_label(Succ),
  io:format("    ~w -> ~w~n", [CVal, SL]),
  [Succ|do_split_match_group(Vals, SuccMap, Fail)];
do_split_match_group([], _SuccMap, Fail) ->
  case get_label(Fail) of
    fail ->
      io:format("    _ -> fail~n~n", []),
      [];
    FL ->
      io:format("    _ -> ~w~n~n", [FL]),
      [Fail]
  end.

do_split(Succ, Fail) ->
  SL = get_label(Succ),
  io:format("    true -> ~w~n", [SL]),
  case get_label(Fail) of
    fail ->
      io:format("    fail -> fail~n~n", []),
      pp(Succ);
    FL ->
      io:format("    fail -> ~w~n~n", [FL]),
      pp(Succ),
      pp(Fail)
  end.

get_label(#clause_tree{instr=I}) ->
  case I of
    #label{name=X} ->
      cerl:concrete(X);
    #goto{label=X} ->
      cerl:concrete(X)
  end;
get_label([]) ->
  fail.

%% @spec full_size(binary_match())-> integer()
%% @doc measures the number of nodes of the clause_tree in 
%% a binary_match
full_size(#binary_match{clause_tree=CT}) ->
    full_size(CT, 0) + 1.
full_size(#clause_tree{instr=I, success=Succ, fail=Fail}, Acc) ->
    Acc1 = 
	case I of 
	    #label{name=X} ->
		max(cerl:concrete(X), Acc);
	    _ ->
		Acc
	end,
    Acc2 = full_size(Succ, Acc1),
    full_size(Fail, Acc2);

full_size([], Acc) ->
    Acc.

max(A, B) when A>B ->
    A;
max(_A,B) ->
    B.

%% @spec a_height(binary_match())-> float()
%% @doc measures the average height of the clause_tree in 
%% a binary_match
a_height(#binary_match{clause_tree=CT}) ->
    Heights=a_height(CT, CT, [], 0),
    Sorted=lists:keysort(2,Heights),
    Added=calculate(Sorted),
    lists:sum(Added)/length(Added).

%% @spec max_height(binary_match())-> integer() 
%% @doc measures the maximum height of the clause_tree in 
%% a binary_match
max_height(#binary_match{clause_tree=CT}) ->
    Heights=a_height(CT, CT, [], 0),
    [{High, _}|_]=lists:reverse(lists:keysort(1,Heights)),
    High.

calculate([{X,Token}|Rest]) -> 
    calculate(Rest, [X], Token, []).

calculate([{X,Token}|Rest], Acc, Token, AccList) ->
    calculate(Rest, [X|Acc], Token, AccList);
calculate([{X,NewToken}|Rest], Acc, _Token, AccList) ->
    Val=lists:sum(Acc)/length(Acc),
    calculate(Rest, [X], NewToken, [Val|AccList]);
calculate([], Acc, _Token, AccList) ->
    Val=lists:sum(Acc)/length(Acc),
    [Val|AccList].

a_height(#clause_tree{instr=I,success=Succ, fail=Fail}, OT, AccList, Acc) ->
    case I of
	#bin_guard{label=X} ->
	    [{Acc+1, X}|AccList];
	#label{} ->
	    a_height(Succ, OT, AccList, Acc);
	#goto{label=Label} ->
	    {value,OtherSucc}=get_succ(OT, Label),
	    a_height(OtherSucc, OT, AccList, Acc);
	_ ->
	    NewList=a_height(Succ, OT, AccList, Acc+1),
	    a_height(Fail, OT, NewList, Acc+1)
    end;

a_height([], _OT, AccList,_Acc) ->
    AccList.

get_succ(#clause_tree{instr=I,success=Succ, fail=Fail}, Label) ->
    case I of
	#label{name=Label} ->
	    {value, Succ};
	_ ->
	    case get_succ(Succ, Label) of
		none ->
		    get_succ(Fail, Label);
		Val ->
		    Val
	    end
    end;
get_succ([], _Label) ->
    none.

-endif.
%% @clear  No warnings if not ifdef(BIN_PMATCH_DEBUG)
