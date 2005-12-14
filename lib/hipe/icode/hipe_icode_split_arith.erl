%%%-------------------------------------------------------------------
%%% File    : hipe_icode_split_arith.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : 
%%%
%%% Created : 12 Nov 2003 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_icode_split_arith).

-export([cfg/2]).

-include("hipe_icode.hrl").

-define(MIN_RATIO, 0.005).

%%-define(UNSAFE, true).
-define(UNSAFE, false).

cfg(Cfg, _Fun)->
  case preprocess(Cfg) of
    {do_not_split, _Ratio} -> 
      %%io:format("split(Cfg): NOT Reasonable to split ~w.Ratio: ~.3f\n", 
      %%		[_Fun, _Ratio]),
      Cfg;
    {split, _Ratio, NewCfg} ->
      %%hipe_icode_cfg:pp(Cfg),
      %%io:format("split(Cfg):Reasonable to split ~w. Ratio: ~.2f\n", 
      %%		[_Fun, _Ratio]),
      NewCfg0 = split(NewCfg),
      NewCfg1 = cleanup(NewCfg0),
      %%hipe_icode_cfg:pp(NewCfg1),
      NewCfg1
  end.

cleanup(Cfg) ->
  Icode=hipe_icode_cfg:cfg_to_linear(Cfg),
  LinearCode = hipe_icode:icode_code(Icode),
  NewLinearCode=cleanup_code(LinearCode),
  NewIcode = hipe_icode:icode_code_update(Icode, NewLinearCode),
  hipe_icode_cfg:linear_to_cfg(NewIcode).

cleanup_code([I|Is]) ->
  case I of
    #call{} ->
      case hipe_icode:call_fail_label(I) of
	[] ->
	  [I|cleanup_code(Is)];
	_ ->
	  case hipe_icode:call_continuation(I) of
	    [] ->
	      NewLabel = hipe_icode:mk_new_label(),
	      NewLabelName = hipe_icode:label_name(NewLabel),
	      NewI=hipe_icode:call_set_continuation(I, NewLabelName),
	      [NewI, NewLabel|cleanup_code(Is)];
	    _ ->
	      [I|cleanup_code(Is)]
	  end
      end;
    _ ->
      [I|cleanup_code(Is)]
  end;
cleanup_code([]) -> [].

preprocess(Cfg)->
  Icode = hipe_icode_cfg:cfg_to_linear(Cfg),
  LinearCode = hipe_icode:icode_code(Icode),
  {NofArith, NofIns, NewLinearCode} = preprocess_code(LinearCode),
  case NofArith / NofIns of
    X when X >= ?MIN_RATIO ->
      NewIcode = hipe_icode:icode_code_update(Icode, NewLinearCode),
      {split, X, hipe_icode_cfg:linear_to_cfg(NewIcode)};
    Y ->
      {do_not_split, Y}
  end.

preprocess_code([H|Code])->
  preprocess_code(Code, 0, 0, [H]).

preprocess_code([I|Left], NofArith, NofIns,CodeAcc = [HdCode|TlCodeAcc])->
  case I of
    #call{} ->
      case is_arith(I) of
	true ->
	  case hipe_icode:is_label(HdCode) of
	    true ->
	      preprocess_code(Left, NofArith + 1, NofIns + 1,[I|CodeAcc]);
	    false ->
	      NewLabel = hipe_icode:mk_new_label(),
	      NewLabelName = hipe_icode:label_name(NewLabel),
	      NewCodeAcc = 
		case hipe_icode:is_call(HdCode) of
		  true -> 
		    [I,
		     NewLabel,
		     hipe_icode:call_set_continuation(HdCode, NewLabelName)|
		     TlCodeAcc];
		  false ->
		    [I,
		     NewLabel,
		     hipe_icode:mk_goto(hipe_icode:label_name(NewLabel))|
		     CodeAcc]
		end,
	      preprocess_code(Left, NofArith + 1, NofIns + 1, NewCodeAcc)
	  end;
	false ->
	  case hipe_icode:is_label(I) of % Don't count labels as intructions.
	    true ->
	      preprocess_code(Left, NofArith, NofIns, [I|CodeAcc]);
	    false ->
	      preprocess_code(Left, NofArith, NofIns + 1, [I|CodeAcc])
	  end
      end;
    _ ->
      preprocess_code(Left, NofArith, NofIns + 1, [I|CodeAcc])
  end;
preprocess_code([], NofArith, NofIns, CodeAcc) ->
  {NofArith, NofIns, lists:reverse(CodeAcc)}.
   

split(Cfg)->
  AllLabels = hipe_icode_cfg:reverse_postorder(Cfg),
  {OldToNewMap, NewToOldMap}  = new_label_maps(AllLabels),
  %%io:format("split(Cfg): Adding fixnum trace ...\n", []),
  NewCfg = add_fixnum_trace(AllLabels, OldToNewMap, Cfg),
  %%io:format("split(Cfg): Adding fixnum trace: Done\n", []),
  %%io:format("split(Cfg): Inserting tests\n", []),
  case ?UNSAFE of
    true ->
      Start = hipe_icode_cfg:start_label(NewCfg),
      NewStart = gb_trees:get(Start, OldToNewMap),
      hipe_icode_cfg:start_label_update(NewCfg, NewStart);
    false ->
      NewCfg2 = 
	insert_tests(NewCfg, [gb_trees:get(X, OldToNewMap)||X<-AllLabels], 
		     NewToOldMap, OldToNewMap),
      %%io:format("split(Cfg): Inserting testsL Done\n", []),
      NewCfg2
  end.

add_fixnum_trace([Lbl|Left], LabelMap, Cfg)->
  NewLbl = gb_trees:get(Lbl, LabelMap),
  Code = hipe_bb:code(hipe_icode_cfg:bb(Cfg, Lbl)),
  NewCode = map_code(Code, Lbl, LabelMap),
  NewBB = hipe_bb:mk_bb(NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, NewLbl, NewBB),
  add_fixnum_trace(Left, LabelMap, NewCfg);
add_fixnum_trace([], _LabelMap, Cfg) ->
  Cfg.

map_code(Ins, ArithFail, LabelMap) ->
  map_code(Ins, ArithFail, LabelMap, []).

map_code([I|Left], ArithFail, LabelMap, Acc) ->
  case I of
    #call{} ->
      case is_arith(I) of
	true ->
	  case hipe_icode:defines(I) of
	    []-> 
	      map_code(Left, ArithFail, LabelMap, [redirect(I, LabelMap)|Acc]);
	    _ ->
	      NewOp = arithop_to_unsafe(hipe_icode:call_fun(I)),
	      NewI1 = hipe_icode:call_fun_update(I, NewOp),
	      NewI2 = redirect(NewI1, LabelMap),
	      NewI3 =
		case hipe_icode:call_fail_label(NewI2) of
		  [] ->
		    case ?UNSAFE of
		      true -> NewI2;
		      false -> hipe_icode:call_set_fail_label(NewI2, ArithFail)
		    end;
		  _ -> NewI2
		end,
	      map_code(Left, ArithFail, LabelMap, [NewI3|Acc])
	  end;
	false ->
	  map_code(Left, ArithFail, LabelMap, [redirect(I, LabelMap)|Acc])
      end;
    _ -> 
      map_code(Left, ArithFail, LabelMap, [redirect(I, LabelMap)|Acc])
  end;
map_code([], _ArithFail, _LabelMap, Acc) ->
  lists:reverse(Acc).

insert_tests(Cfg, Labels,NewToOldMap, OldToNewMap) ->
  InfoMap = infomap_init(Labels),
  %%io:format("insert_tests/3: Finding testpoints ...\n", []),
  NewInfoMap = find_testpoints(Cfg, Labels, InfoMap),
  %%io:format("insert_tests/3: Finding testpoints: Done\n", []),
  %%io:format("insert_tests/3: Infomap: ~w\n", [gb_trees:to_list(NewInfoMap)]),
  make_tests(Cfg, NewInfoMap, NewToOldMap, OldToNewMap).

find_testpoints(Cfg, Labels, InfoMap) ->
  case find_testpoints(Labels, InfoMap, Cfg, false) of
    {dirty, NewInfoMap} -> 
      %%io:format("find_testpoints/3: Looping\n", []),
      find_testpoints(Cfg, Labels, NewInfoMap);
    fixpoint ->
      InfoMap 
  end.

find_testpoints([Lbl|Left], InfoMap, Cfg, Dirty)->
  Code = hipe_bb:code(hipe_icode_cfg:bb(Cfg, Lbl)),
  InfoOut = join_info(hipe_icode_cfg:succ(Cfg, Lbl), InfoMap),  
  OldInfoIn = infomap_get_all(Lbl, InfoMap),
  NewInfoIn = traverse_code(lists:reverse(Code), InfoOut),
  case (gb_sets:is_subset(OldInfoIn, NewInfoIn) andalso 
	gb_sets:is_subset(NewInfoIn, OldInfoIn)) of
    true ->
      find_testpoints(Left, InfoMap, Cfg, Dirty);
    false ->
      %%io:format("find_testpoints/4: Label: ~w: OldMap ~w\nNewMap: ~w\n", 
      %%	 [Lbl, gb_sets:to_list(OldInfoIn), gb_sets:to_list(NewInfoIn)]),
      NewInfoMap = gb_trees:update(Lbl, NewInfoIn, InfoMap),
      find_testpoints(Left, NewInfoMap, Cfg, true)
  end;
find_testpoints([], InfoMap, _Cfg, Dirty) ->
  if Dirty -> {dirty, InfoMap};
     true -> fixpoint
  end.
      
traverse_code([I|Left], Info)->
  NewInfo = kill_defines(I, Info),
  case I of
    #call{} ->
      case is_unsafe_arith(I) of
	true ->
	  %% The dst is sure to be a fixnum. Remove the 'killed' mark.
	  Dst = hd(hipe_icode:call_dstlist(I)),
	  NewInfo1 = gb_sets:delete_any({killed, Dst}, NewInfo),
	  NewInfo2 = 
	    gb_sets:union(NewInfo1, gb_sets:from_list(hipe_icode:uses(I))),
	  traverse_code(Left, NewInfo2);
	false ->
	  traverse_code(Left, NewInfo)
      end;
    #move{} ->
      Dst = hipe_icode:move_dst(I),
      case gb_sets:is_member(Dst, Info) of 
	true -> 
	  %% The dst is an argument to an arith op. Transfer the test
	  %% to the src and remove the 'killed' mark from the dst.
	  NewInfo1 = gb_sets:delete({killed, Dst}, NewInfo),
	  Src = hipe_icode:move_src(I),
	  case hipe_icode:is_const(Src) of
	    true ->
	      traverse_code(Left, NewInfo1);
	    false ->
	      NewInfo2 = gb_sets:add(Src, NewInfo1),
	      traverse_code(Left, NewInfo2)
	  end;
	false ->
	  traverse_code(Left, NewInfo)
      end;
    _ ->
      traverse_code(Left, NewInfo)
  end;
traverse_code([], Info) ->
  Info.

kill_defines(I, Info)->  
  Defines = hipe_icode:defines(I),
  case [X||X<-Defines, gb_sets:is_member(X, Info)] of
    List when length(List)>0 ->
      TmpInfo = gb_sets:difference(Info, gb_sets:from_list(List)),
      gb_sets:union(gb_sets:from_list([{killed, X}||X<-List]), TmpInfo);
    [] ->
      Info
  end.

make_tests(Cfg, InfoMap, NewToOldMap, OldToNewMap)->
  %%io:format("make_tests 0:\n",[]),
  WorkList = make_worklist(gb_trees:keys(NewToOldMap), InfoMap, 
			   NewToOldMap, Cfg, []),
  %%io:format("make_tests 1:Worklist: ~w\n",[WorkList]),
  NewCfg = make_tests(WorkList, Cfg),
  %%io:format("make_tests 2\n",[]),
  %% If the arguments to this function are used in unsafe arith
  %% they should be marked as killed by a new start block.
  Args = hipe_icode_cfg:params(NewCfg),
  Start = hipe_icode_cfg:start_label(NewCfg),
  AltStart = gb_trees:get(Start, OldToNewMap),
  UnsafeIn = gb_sets:to_list(infomap_get(AltStart, InfoMap)),
  case [X || X<-UnsafeIn, Y<-Args, X=:=Y] of
    [] -> 
      hipe_icode_cfg:start_label_update(NewCfg, AltStart);
    KilledArgs ->
      NewStart = hipe_icode:label_name(hipe_icode:mk_new_label()),
      NewCfg1 = insert_test_block(NewStart, AltStart, 
				  Start,
				  KilledArgs, NewCfg),
      hipe_icode_cfg:start_label_update(NewCfg1, NewStart)
  end.



make_worklist([Lbl|Left], InfoMap, LabelMap, Cfg, Acc)->
  case infomap_get_killed(Lbl, InfoMap) of
    [] -> make_worklist(Left, InfoMap, LabelMap, Cfg, Acc);
    Vars ->
      %%io:format("make_worklist 1 ~w\n",[Vars]),      
      NewAcc = 
	[{Lbl, Succ, gb_trees:get(Succ, LabelMap), Vars}
	 || Succ<-hipe_icode_cfg:succ(Cfg, Lbl)] ++ Acc,
      %%io:format("make_worklist 2\n",[]),
      make_worklist(Left, InfoMap, LabelMap, Cfg, NewAcc)
  end;
make_worklist([], _InfoMap, _LabelMap, _Cfg, Acc) ->
  Acc.

make_tests([{FromLbl, ToLbl, FailLbl, Vars}|Left], Cfg)->
  NewLbl = hipe_icode:label_name(hipe_icode:mk_new_label()),
  TmpCfg = insert_test_block(NewLbl, ToLbl, FailLbl, Vars, Cfg),  
  NewCfg = hipe_icode_cfg:redirect(TmpCfg, FromLbl, ToLbl, NewLbl),
  make_tests(Left, NewCfg);
make_tests([], Cfg) ->
  Cfg.

insert_test_block(NewLbl, Succ, FailLbl, Vars, Cfg)->
  Code = [hipe_icode:mk_type(Vars, fixnum, Succ, FailLbl, 0.99)],
  BB = hipe_bb:mk_bb(Code),
  hipe_icode_cfg:bb_add(Cfg, NewLbl, BB).



infomap_init(Labels)->
  infomap_init(Labels, gb_trees:empty()).

infomap_init([Lbl|Left], Map)->
  infomap_init(Left, gb_trees:insert(Lbl, gb_sets:empty(), Map));
infomap_init([], Map) ->
  Map.

join_info(Labels, Map)->
  join_info(Labels, Map, gb_sets:empty()).

join_info([Lbl|Left], Map, Set) ->  
  join_info(Left, Map, gb_sets:union(Set, infomap_get(Lbl, Map)));
join_info([], _Map, Set) ->
  Set.


infomap_get(Lbl, Map)->
  case gb_trees:lookup(Lbl, Map) of
    none -> gb_sets:empty();
    {value, Val} -> 
      gb_sets:filter(fun(X)->case X of 
			       {killed, _}->false;
			       _->true
			     end
		     end, 
		     Val)
  end.

infomap_get_all(Lbl, Map)->
  case gb_trees:lookup(Lbl, Map) of
    none -> gb_sets:empty();
    {value, Val} -> Val
  end.

infomap_get_killed(Lbl, Map)->
  case gb_trees:lookup(Lbl, Map) of
    none -> [];
    {value, Val} -> 
      Fun = fun(X, Acc) ->
		case X of
		  {killed, Var} -> [Var|Acc];
		  _ -> Acc
		end
	    end,
      lists:foldl(Fun, [], gb_sets:to_list(Val))
  end.

is_arith(I)->
  case hipe_icode:call_fun(I) of
    '+' -> true;
    '-' -> true;
    'band' -> true;
    'bor' -> true;
    'bxor' -> true;
    'bnot' -> true;
    _ -> false
  end.

is_unsafe_arith(I)->
  case hipe_icode:call_fun(I) of
    unsafe_add -> true;
    unsafe_sub -> true;
    unsafe_band -> true;
    unsafe_bor -> true;
    unsafe_bxor -> true;
    unsafe_bnot -> true;
    _ -> false
  end.

arithop_to_unsafe(Op)->
  case Op of
    '+' -> unsafe_add;
    '-' -> unsafe_sub;
    'band' -> unsafe_band;
    'bor' -> unsafe_bor;
    'bxor' -> unsafe_bxor;
    'bnot' -> unsafe_bnot  
  end.

redirect(I, LabelMap)->
  case hipe_icode:successors(I) of
    [] -> I;
    [[]] -> I;
    Succ ->
      RedirectMap = [{X, gb_trees:get(X, LabelMap)}||X<-Succ],
      redirect_1(RedirectMap, I)
  end.
	  
redirect_1([{From, To}|Left], I)->
  redirect_1(Left, hipe_icode:redirect_jmp(I, From, To));
redirect_1([], I) ->
  I.

new_label_maps(Labels)->
  new_label_maps(Labels, gb_trees:empty(), gb_trees:empty()).

new_label_maps([Lbl|Left], Map1, Map2)->
  NewLabel = hipe_icode:label_name(hipe_icode:mk_new_label()),
  NewMap1 = gb_trees:insert(Lbl, NewLabel, Map1),
  NewMap2 = gb_trees:insert(NewLabel, Lbl, Map2),
  new_label_maps(Left, NewMap1, NewMap2);
new_label_maps([], Map1, Map2) ->
  {Map1, Map2}.
