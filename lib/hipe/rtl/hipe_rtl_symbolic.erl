%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
%% File        : hipe_rtl_symbolic.erl
%% Author      : Per Gustafsson <pergu@it.uu.se>
%% Description : Optimization and expansion of symbolic instructions.
%%
%% Created     : 18 May 2004 by Per Gustafsson <pergu@it.uu.se>
%%-------------------------------------------------------------------

-module(hipe_rtl_symbolic).

-export([find_and_replace/1, expand/1]).

-include("hipe_rtl.hrl").
-include("hipe_literals.hrl").
-include("../icode/hipe_icode_primops.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_and_replace(Cfg::cfg()) -> cfg()
%%
%% Optimizes uses of symbolic instructions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_and_replace(Cfg) ->
  FindFuns = [{fnums, fun fixnum_rules/2},
	      {bbase, fun binbase_rules/2}],
  ReplaceFuns = [{fnums, fun fixnum_replace_rules/5},
		 {bbase, fun binbase_replace_rules/5}],
  AvOut=find(FindFuns, Cfg),
  replace(Cfg, ReplaceFuns, AvOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find([{Key::atom(), Fun::fun()}], Cfg::cfg()) ->
%%     AvOut::gb_tree()
%%
%% Find framework which extracts info from a Cfg. What info that is
%% extracted is defined by the Funs. All info is about what is
%% available out of a basic block and is stored in a gb_tree which is
%% indexed by label and key pairs where the key describes what kind of
%% info the value contains
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find(Funs, Cfg) ->
  find(new_worklist(Cfg), Funs, Cfg, gb_trees:empty()).
 
find(WorkList, [{Key, Fun}|Rest]=Funs, Cfg, AvOut) ->
  case get_work(WorkList) of
    none ->
      find(new_worklist(Cfg), Rest, Cfg, AvOut);
    {Label, NewWorkList} ->
      Preds=hipe_rtl_cfg:pred(hipe_rtl_cfg:pred_map(Cfg), Label),
      AvIn=available_in(Preds, Key,  AvOut),
      BB=hipe_rtl_cfg:bb(Cfg, Label),
      Code=hipe_bb:code(BB),
      LocalAvOut=do_code(Code, Fun, AvIn),
      case update_av_out({Key, Label}, LocalAvOut, AvOut) of
	AvOut ->
	  find(NewWorkList, Funs, Cfg, AvOut); 
	NewAvOut ->
	  Succs=hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(Cfg), Label),
	  find(add_work(NewWorkList, Succs), Funs, Cfg, NewAvOut)
      end
  end;
find(_WorkList, [], _Cfg, AvOut) ->
  AvOut.

do_code([Instr|Rest], Fun, AvIn) ->
  Defs= hipe_rtl:defines(Instr),
  AvList=gb_sets:to_list(AvIn),
  NewAvList=[X||X <- AvList, possible(X, Defs)],
  NewAvIn = Fun(Instr, NewAvList),
  do_code(Rest, Fun, NewAvIn);
do_code([], _Fun, AvIn) ->
  AvIn.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace(Cfg::cfg(), [{Key::atom(), Fun::fun()}],  AvOut::gb_tree()) ->
%%     cfg()
%%
%% Replace framework which uses the info in AvOut to update the cfg
%% according to the rules laid out in the key-fun pairs.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace(Cfg, Funs, AvOut) ->
  Labels=hipe_rtl_cfg:reverse_postorder(Cfg),
  replace(Labels, Cfg, Funs, AvOut, 0).

replace([Label|Labels], Cfg, [{Key,Fun}|_]=Funs, AvOut, N) ->
  Preds=hipe_rtl_cfg:pred(hipe_rtl_cfg:pred_map(Cfg), Label),
  AvIn=available_in(Preds, Key, AvOut),
  BB=hipe_rtl_cfg:bb(Cfg, Label),
  Code=hipe_bb:code(BB),
  {NewCode, NewN}=update_code(Code, Fun, AvIn, N),
  UpdBB= hipe_bb:code_update(BB, NewCode),
  NewCfg=hipe_rtl_cfg:bb_add(Cfg, Label, UpdBB), 
  replace(Labels, NewCfg, Funs, AvOut, NewN);
replace([], Cfg, [_KeyFun|Rest], AvOut, N) ->
  Labels=hipe_rtl_cfg:reverse_postorder(Cfg),
  replace(Labels, Cfg, Rest, AvOut, N);
replace(_, Cfg, [], _AvOut, _N) ->
  Cfg.

update_code(Code, Fun,  AvIn, N) ->
  {BwdCode, _AvOut, _Fun, N1}=lists:foldl(fun update_code_av_in/2, 
					  {[],AvIn,Fun,N}, Code),
  {lists:reverse(BwdCode), N1}.

update_code_av_in(Instr, {Acc,AvIn,Fun,N}) ->
  Defs= hipe_rtl:defines(Instr),
  AvList=gb_sets:to_list(AvIn),
  NewAvList=[X||X <- AvList, possible(X, Defs)],
  Fun(Instr, Fun, NewAvList, Acc, N).

%%====================================================================
%%
%% Utility functions for dealing with the AvOut datastructure
%%
%%====================================================================

available_in(Preds, Key, AvOut) ->
  List=[gb_trees:lookup({Key,Label}, AvOut)||Label <- Preds], 
  case [X || X <- List, X==none] of
    [] ->
      SetList=[Value||{value, Value} <- List],
      case SetList of
	[] -> 
	  gb_sets:empty();
	_ ->
	  gb_sets:intersection(SetList)
      end;
    _ ->
      gb_sets:empty()
  end.

possible(Instr, Defs) -> 
  List=hipe_rtl:uses(Instr) ++ hipe_rtl:defines(Instr),
  case [X||X <- Defs, lists:member(X, List)] of
    [] ->
      true;
    _ ->
     false
  end.

update_av_out(Label, LocalAvOut, AvOut) ->
  case gb_trees:lookup(Label, AvOut) of
    {value, LocalAvOut} ->
      AvOut;
    {value, _} ->
      gb_trees:update(Label, LocalAvOut, AvOut);
    none ->
      gb_trees:insert(Label, LocalAvOut, AvOut)
  end.

%%================================================================
%%
%% Function definitions to use in funs for find
%%
%%================================================================

binbase_rules(Instr, AvList) ->
  case Instr of
    #binbase{} ->
      add_instr(Instr, AvList);
    #gctest{} ->
      gb_sets:empty();
    #call{} ->
      case cerl_bs_call(hipe_rtl:call_fun(Instr)) of
	true ->
	  gb_sets:from_list(AvList);
	false ->
	  gb_sets:empty()
      end;
    _ ->
      gb_sets:from_list(AvList)
  end.

fixnum_rules(Instr, AvList) ->
  case Instr of
    #fixnumop{} ->
      add_instr(Instr, AvList);
    _ ->
      gb_sets:from_list(AvList)
  end.

add_instr(Instr, AvList) -> 
  case [X||X <- AvList, has_equal_args(X, Instr)] of
    [] ->
      gb_sets:from_list([Instr|AvList]);
    _ ->
      gb_sets:from_list(AvList)
  end.

%%================================================================
%%
%% Function definitions to use in funs for replace
%%
%%================================================================

%% XXX: Is this first clause needed? - Kostis
binbase_replace_rules(#gc_test{} = Instr, Fun, _AvList, Acc, N) ->
  {[Instr|Acc],gb_sets:empty(),Fun,N};
binbase_replace_rules(Instr, Fun, AvList, Acc, N) ->
  case Instr of
    #binbase{} ->
      add_instr_and_new_code_to_av(Instr, Fun, Acc, AvList, N);
    #call{} ->
      case cerl_bs_call(hipe_rtl:call_fun(Instr)) of
	true ->
	  {[Instr|Acc],gb_sets:from_list(AvList),Fun,N};
	false ->
	  {[Instr|Acc],gb_sets:empty(),Fun,N}
      end;
    _ ->
	{[Instr|Acc],gb_sets:from_list(AvList),Fun,N}
  end. 

fixnum_replace_rules(Instr, Fun, AvList, Acc, N) ->
  case Instr of
    #fixnumop{} ->
      add_fixnum_instr_and_new_code_to_av(Instr, Fun, Acc, AvList, N);
    _ ->
      {[Instr|Acc],gb_sets:from_list(AvList),Fun,N}
  end.

add_instr_and_new_code_to_av(Instr, Fun, Acc, AvList, N) ->
  case [X||X <- AvList, has_equal_args(X, Instr)] of
    [] ->                     
      {[Instr|Acc],gb_sets:from_list([Instr|AvList]), Fun, N};
    [OldInstr] ->
      [Src]=hipe_rtl:defines(OldInstr),
      [Dst]=hipe_rtl:defines(Instr),
      NewI=hipe_rtl:mk_move(Dst, Src),
      {[NewI|Acc], gb_sets:from_list(AvList), Fun, N+1}
  end.

add_fixnum_instr_and_new_code_to_av(Instr, Fun, Acc, AvList, N) ->
  case [X||X <- AvList, has_equal_args(X, Instr)] of
    [] ->
      case [X||X <- AvList, has_arg_as_def(X, Instr)] of
	[] ->
	  {[Instr|Acc],gb_sets:from_list([Instr|AvList]), Fun, N};
	[OldInstr|_] ->
	  %%io:format("Instr:~w~nOldInstr:~w~n~n", [Instr,OldInstr]),
	  [Src]=hipe_rtl:args(OldInstr),
	  [Dst]=hipe_rtl:defines(Instr),
	  NewI=hipe_rtl:mk_move(Dst, Src),
	  {[NewI|Acc], gb_sets:from_list(AvList), Fun, N+1}
      end;
    [OldInstr] ->
      [Src]=hipe_rtl:defines(OldInstr),
      [Dst]=hipe_rtl:defines(Instr),
      NewI=hipe_rtl:mk_move(Dst, Src),
      {[NewI|Acc], gb_sets:from_list(AvList), Fun, N+1}
  end.

%%====================================================================
%%
%% Utility functions for funs
%% 
%%====================================================================

cerl_bs_call(bs_get_integer) -> true;
cerl_bs_call(bs_get_binary) -> true;
cerl_bs_call(bs_get_float) -> true;
cerl_bs_call(_) -> false.
  

has_equal_args(X, Instr) ->
  hipe_rtl:args(X) == hipe_rtl:args(Instr).

has_arg_as_def(X, Instr) ->
  hipe_rtl:defines(X) == hipe_rtl:args(Instr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expand(Cfg:cfg()) -> cfg()
%%
%% Expansion of symbolic instructions to real rtl instructions done on
%% linear form
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand(Cfg) ->
  Linear=hipe_rtl_cfg:linearize(Cfg),
  Code=hipe_rtl:rtl_code(Linear),
  NonFlatCode=[expand_instr(Instr)||Instr<-Code],
  NewCode=lists:flatten(NonFlatCode),
  Linear1=hipe_rtl:rtl_code_update(Linear, NewCode),
  hipe_rtl_cfg:init(Linear1).

expand_instr(Instr) ->
  case Instr of
    #binbase{} ->
      expand_binbase(Instr);
    #fixnumop{} ->
     expand_fixnumop(Instr);
    #gctest{} ->
      expand_gctest(Instr);
    _ ->
      Instr
  end.

expand_binbase(Instr) ->
  Base=hipe_rtl:binbase_dst(Instr),
  Orig=hipe_rtl:binbase_orig(Instr),
  OrigOffset=hipe_rtl:binbase_offset(Instr),
  Tmp1=hipe_rtl:mk_new_reg_gcsafe(),
  HeapLbl=hipe_rtl:mk_new_label(),
  REFCLbl=hipe_rtl:mk_new_label(),
  JoinLbl=hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_heap_binary(Orig, hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Base, Orig, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
   REFCLbl,
   hipe_rtl:mk_load(Base, Orig, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
   JoinLbl,
   hipe_tagscheme:realuntag_fixnum(Tmp1, OrigOffset),
   hipe_rtl:mk_alu(Base, Base, add, Tmp1)].

expand_fixnumop(Instr) ->
  case hipe_rtl:fixnumop_type(Instr) of
    untag ->
      Dst = hipe_rtl:fixnumop_dst(Instr),
      Src = hipe_rtl:fixnumop_src(Instr),
      hipe_tagscheme:realuntag_fixnum(Dst, Src);
    tag ->
      Dst = hipe_rtl:fixnumop_dst(Instr),
      Src = hipe_rtl:fixnumop_src(Instr),
      hipe_tagscheme:realtag_fixnum(Dst, Src)
  end.

expand_gctest(Instr) ->
   HeapNeed = hipe_rtl:gctest_words(Instr),
  {GetHPInsn, HP, _PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  {GetHLIMITInsn, H_LIMIT} = hipe_rtl_arch:heap_limit(),
  ContLabel = hipe_rtl:mk_new_label(),
  GCLabel = hipe_rtl:mk_new_label(),
  ContLabelName = hipe_rtl:label_name(ContLabel),
  GCLabelName = hipe_rtl:label_name(GCLabel),
  Tmp = hipe_rtl:mk_new_reg(), % diff between two gc-unsafe pointers
  StartCode = 
    [GetHPInsn, 
     GetHLIMITInsn,
     hipe_rtl:mk_alu(Tmp, H_LIMIT, 'sub', HP)],
  {SeparateCode, GCAmount, HPAmount} =
    case hipe_rtl:is_reg(HeapNeed) of	
      true ->
	GA = hipe_rtl:mk_new_reg_gcsafe(),
	HA = hipe_rtl:mk_new_reg_gcsafe(),
	{[hipe_rtl:mk_alu(HA, HeapNeed, sll, 
			  hipe_rtl:mk_imm(hipe_rtl_arch:
					  log2_word_size()))|
	  hipe_tagscheme:realtag_fixnum(GA, HeapNeed)], GA, HA};
      false ->
	WordsNeeded = hipe_rtl:imm_value(HeapNeed),
	GA = hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(WordsNeeded)),
	HA = hipe_rtl:mk_imm(WordsNeeded*hipe_rtl_arch:word_size()),
	{[], GA, HA}
    end,
  EndCode = 
    [hipe_rtl:mk_branch(Tmp, 'lt', HPAmount, GCLabelName, ContLabelName, 0.01),
     GCLabel,
     hipe_rtl:mk_call([], gc_1, [GCAmount], ContLabelName, [], not_remote),
     ContLabel],
  StartCode ++ SeparateCode ++ EndCode.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Worklist datatype implemented by Richard originally
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_worklist(Cfg)->
  Start = hipe_icode_cfg:start_label(Cfg),
  {[Start], [], gb_sets:insert(Start, gb_sets:empty())}.

get_work({[Label|Left], List, Set})->
  {Label, {Left, List, gb_sets:delete(Label, Set)}};
get_work({[], [], _Set}) ->
  none;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work({List1, List2, Set}, [Label|Left])->
  case gb_sets:is_member(Label, Set) of
    true -> 
      add_work({List1, List2, Set}, Left);
    false -> 
      %%io:format("Added work: ~w\n", [Label]),
      NewSet = gb_sets:insert(Label, Set),
      add_work({List1, [Label|List2], NewSet}, Left)
  end;
add_work(WorkList, []) ->
  WorkList.



