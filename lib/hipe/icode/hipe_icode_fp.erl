%%%-------------------------------------------------------------------
%%% File    : hipe_icode_fp.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : One pass analysis to find floating point values. 
%%%               Mapping to fp variables and creation of fp ebbs.
%%%
%%% Created : 23 Apr 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_icode_fp).

-export([cfg/1,
	 cfg/2]).

-export([print_state_info/1]).

-import(erl_types, [
		    t_any/0,
		    t_float/0,
		    t_from_term/1,
		    t_inf/2,
		    t_is_any/1,
		    t_is_float/1,
		    t_sup/2,
		    t_to_string/1,
		    t_undefined/0]).

cfg(Cfg) ->
  %%hipe_icode_cfg:pp(Cfg),
  State = analyse(Cfg),
  %%print_state_info(State),
  NewState = place_fp_blocks(State),
  NewCfg = state__cfg(NewState),
  %%hipe_icode_cfg:pp(NewCfg),
  NewCfg.

cfg(Cfg, InfoMap) ->
  State = new_state(Cfg, InfoMap),
  NewState = place_fp_blocks(State),
  NewCfg = state__cfg(NewState),
  %%hipe_icode_cfg:pp(NewCfg),
  NewCfg.

%%____________________________________________________________
%%
%% Single pass analysis that only focus on floats.
%%

analyse(Cfg)->
  State = new_state(Cfg),
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  analyse_blocks(Labels, State).

analyse_blocks(Labels, State)->
  analyse_blocks(Labels, Labels, State).

analyse_blocks(Labels, [Label|Left], State)->
  Info = state__info_in(State, Label),
  NewState = analyse_block(Label, Info, State),
  analyse_blocks(Labels, Left, NewState);
analyse_blocks(_Labels, [], State) ->
  State.

analyse_block(Label, InfoIn, State)->
  %%io:format("Handling ~w\n", [Label]),
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Last = hipe_bb:last(BB),
  NewInfoIn = analyse_insns(Code, InfoIn),
  NewState = state__info_out_update(State, Label, NewInfoIn),

  case hipe_icode:type(Last) of
    type ->
      UpdateInfo = do_type(Last, NewInfoIn),
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo);
    _ ->
      UpdateInfo = [{X, NewInfoIn}||X<-state__succ(NewState, Label)],
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo)
  end.

analyse_insns([I|Insns], Info)->
  NewInfo = 
    case hipe_icode:type(I) of
      mov ->
	do_mov(I, Info);
      call ->
	do_call(I, Info);
      phi ->
	Type = join_list(uses(I), Info),
	enter_defines(I, Type, Info);
      _ ->
	enter_defines(I, t_any(), Info)
    end,
  analyse_insns(Insns, NewInfo);  
analyse_insns([], Info) ->
  Info.

do_mov(I, Info)->
  %% Can't use uses/1 since we must keep constants.
  Src = hipe_icode:mov_src(I), 
  case const_type(Src) of
    not_a_constant ->
      %% Make the destination point to the source.
      enter_defines(I, Src, Info);
    ConstType ->
      enter_defines(I, ConstType, Info)
  end.

do_call(I, Info)->
  case hipe_icode:call_type(I) of
    remote ->
      case hipe_icode:call_fun(I) of
	{M, F, A} ->
	  ArgTypes = lookup_type_list(uses(I), Info),
	  Type = erl_bif_types:type(M, F, A, ArgTypes),
	  enter_defines(I, Type, Info);
	_ ->
	  Info
      end;
    local ->
      case lists:keysearch(dst_type, 1, hipe_icode:info(I)) of
	false ->
	  enter_defines(I, t_any(), Info);
	{value,{_, [Type]}}->
	  enter_defines(I, Type, Info)
      end;
    primop ->
      Fun = hipe_icode:call_fun(I),
      ArgType = lookup_type_list(uses(I), Info),
      DstType = hipe_rtl_primops:type(Fun, ArgType),
      enter_defines(I, DstType, Info)
  end.

do_type(I, Info)->
  [Var] = uses(I),
  VarInfo = lookup_type(I, Info),
  TrueLab = hipe_icode:type_true_label(I),
  FalseLab = hipe_icode:type_false_label(I),
  
  case hipe_icode:type_type(I) of
    float ->
      TrueType = t_inf(t_float(), VarInfo),
      TrueInfo = enter(Var, TrueType, Info),
      FalseInfo = enter(Var, t_any(), Info),
      [{TrueLab, TrueInfo}, {FalseLab, FalseInfo}];
    _ ->
      [{TrueLab, Info}, {FalseLab, Info}]
  end.

do_updates(State, [{Label, Info}|Tail])->
  case state__info_in_update(State, Label, Info) of
    fixpoint ->
      do_updates(State, Tail);
    NewState ->
      do_updates(NewState, Tail)
  end;
do_updates(State, []) ->
  State.

%%____________________________________________________________
%%
%% Make float ebbs
%%

place_fp_blocks(State)->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  transform_block(Labels, gb_sets:empty(), State).

transform_block([Label|Left], BackEdgeSucc, State)->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Info = state__info_out(State, Label),
  {Prelude, Map} = do_prelude(State, Label),
  InBlock = state__in_block(State, Label),
  {NewMap, NewCode, NewInBlock} = 
    transform_instrs(Code, Map, Info, InBlock, []),
  NewBB = hipe_bb:code_update(BB, Prelude++NewCode),
  NewState0 = state__bb_update(State, Label, NewBB),
  NewState1 = state__map_update(NewState0, Label, NewMap, NewInBlock),
  case state__succ(NewState1, Label) of
    [] ->
      transform_block(Left, BackEdgeSucc, NewState1);
    Succ ->
      BackEdge = [X||X<-Succ, not lists:member(X, Left)],
      NewBackEdgeSucc = lists:foldl(fun(X, Acc)->gb_sets:add(X, Acc)end,
				    BackEdgeSucc, BackEdge),
      transform_block(Left, NewBackEdgeSucc, NewState1)
  end;
transform_block([], BackEdgeSucc, State) ->
  handle_back_edges(gb_sets:to_list(BackEdgeSucc), State).

transform_instrs([I|Left], Map, Info, InBlock, Acc)->
  case is_fop_cand(I, Info) of
    false ->
      {NewMap, NewAcc, NewInBlock} = end_block(I, Map, Acc, InBlock),
      transform_instrs(Left, NewMap, Info, NewInBlock, NewAcc);
    true ->
      Uses = uses(I),
      Defines = defines(I),
      Convs =  [X||X <- remove_duplicates(Uses), lookup(X, Map)==none],
      NewMap0 = add_new_bindings_assigned(Convs, Map),
      NewMap = add_new_bindings_unassigned(Defines, NewMap0),
      ConvIns = get_conv_instrs(Convs, NewMap, Info),
      {Op, Cont, Fail} = get_info(I),
      NewI = hipe_icode:mk_primop(lookup_list(Defines, NewMap), Op,
				  lookup_list(Uses, NewMap), Cont, Fail),
      case InBlock of
	{true, Fail} -> %% We can continue the block
	  transform_instrs(Left, NewMap, Info, InBlock, [NewI|ConvIns]++Acc);
	{true, _NewFail} -> %% Must end previous block and start a new one.
	  %% TODO: Find out if this ever happens. If so, handle it!
	  exit('Different catches');
	false ->
	  BlockStart = hipe_icode:mk_primop([], fclearerror, []),
	  transform_instrs(Left, NewMap, Info, {true, Fail}, 
			   [NewI, BlockStart|ConvIns]++Acc)
      end
  end;
transform_instrs([], Map, _Info, InBlock, Acc) ->
  {Map, lists:reverse(Acc), InBlock}.

end_block(I, Map, Code, InBlock)->
  %% If there is instructions that need to operate on tagged values
  %% that currently are untagged, we must end the block (if necessary)
  %% and tag the values .
  case hipe_icode:type(I) of
    phi ->
      Uses = uses(I),
      case [{X, Y}||X<-Uses, (Y=lookup(X, Map))/=none] of
	[] ->
	  {Map, [I|Code], InBlock};
	Subst0 ->
	  Defines = defines(I),
	  NewMap = add_new_bindings_assigned(Defines, Map),
	  Dst = hd(Defines),
	  Subst = [{Dst, lookup(Dst, NewMap)}|Subst0],
	  NewI = hipe_icode:subst(Subst, I),
	  {NewMap, [NewI|Code], InBlock}
      end;
    Other ->
      LocalCallEndsBlock =
	case Other of
	  call ->
	    case hipe_icode:call_type(I) of
	      local -> InBlock;
	      _ ->false
	    end;
	  _ ->
	    false
	end,
      case lists:filter(fun(X)->must_be_tagged(X, Map)end, uses(I)) of
	[] ->
	  case LocalCallEndsBlock of
	    {true, FailLab}->
	      Fcheck = hipe_icode:mk_primop([], fcheckerror, [],
					    [], FailLab),      
	      {Map, [I, Fcheck|Code], false};
	    false ->
	      {Map, [I|Code], InBlock}
	  end;
	Tag ->
	  TagIntrs = mk_tags(Tag, Map),
	  NewMap = lists:foldl(fun(X,Tree)->gb_trees:delete(X, Tree)end,
			       Map, Tag),
	  case InBlock of
	    {true, FailLab} ->
	      Fcheck = hipe_icode:mk_primop([], fcheckerror, [],
					    [], FailLab),      
	      {NewMap, [I|TagIntrs]++[Fcheck|Code], false};
	    _ ->
	      {NewMap, [I|TagIntrs]++Code, InBlock}
	  end
      end
  end.

mk_tags(Tag, Map)->
  [hipe_icode:mk_primop([Dst], unsafe_tag_float, [gb_trees:get(Dst, Map)])||
    Dst<-Tag].

%% We make a difference between values that has been assigned as
%% tagged variables and those that has got a 'virtual' binding.

add_new_bindings_unassigned([Var|Left], Map)->
  FVar = hipe_icode:mk_new_fvar(),
  add_new_bindings_unassigned(Left, gb_trees:insert(Var, FVar, Map));
add_new_bindings_unassigned([], Map) ->
  Map.

add_new_bindings_assigned([Var|Left], Map)->
  case lookup(Var, Map) of
    none ->
      FVar = hipe_icode:mk_new_fvar(),
      NewMap = gb_trees:insert(Var, {assigned, FVar}, Map),
      add_new_bindings_assigned(Left, NewMap);
    _ ->
      add_new_bindings_assigned(Left, Map)
  end;
add_new_bindings_assigned([], Map) ->
  Map.


get_conv_instrs(Vars, Map, Info)->
  get_conv_instrs(Vars, Map, Info, []).

get_conv_instrs([Var|Left], Map, Info, Acc)->
  {_, Dst} = gb_trees:get(Var, Map),
  NewI = 
    case t_is_float(lookup_type(Var, Info)) of
      true ->
	hipe_icode:mk_primop([Dst],unsafe_untag_float,[Var]);
      _ ->
	hipe_icode:mk_primop([Dst],conv_to_float,[Var]) 
    end,
  get_conv_instrs(Left, Map, Info, [NewI|Acc]);
get_conv_instrs([], _, _, Acc) ->
  Acc.

do_prelude(State, Label)->  
  %% Add phi nodes for untagged fp values.
  Map = state__map(State, Label),
  case state__pred(State, Label) of
    List when length(List)>1 ->
      {Ins, NewMap} = lists:foldl(fun(X, Acc)->get_phi(X, List, Acc)end,
				  {[], Map}, gb_trees:to_list(Map)),
      {Ins, init_map(NewMap)};
    _ -> {[], init_map(Map)}
  end.

get_phi({Var, PredList}, Preds, {InsAcc, Map})->
  case all_args_equal(PredList) of
    true ->
      {InsAcc, Map};
    false ->
      FVar = hipe_icode:mk_new_fvar(),
      NewMap = gb_trees:enter(Var, FVar, Map),
      Phi0 = hipe_icode:mk_phi(FVar, Preds),
      Phi1 = lists:foldl(fun(X, Ins)->
			     case X of
			       {Pred, {assigned, Val}}->
				 hipe_icode:subst_phi_arg(Ins, Pred, Val);
			       {Pred, Val} ->
				 hipe_icode:subst_phi_arg(Ins, Pred, Val)
			     end
			 end,
			 Phi0, PredList),
      {[Phi1|InsAcc], NewMap}
  end.

all_args_equal([{_, FVar}|Left])->
  all_args_equal(Left, FVar).

all_args_equal([{_, FVar1}|Left], FVar2) when FVar1 == FVar2 ->
  all_args_equal(Left, FVar2);
all_args_equal([], _) ->
  true;
all_args_equal(_, _) ->
  false.
  
handle_back_edges([Label|Left], State)->
  %% When there is a back edge we must make sure that any phi nodes
  %% has the rigth arguments since untagging might have occured after
  %% the block was processed.
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Map = init_map(state__map(State, Label)),
  NewCode = lists:map(fun(X)->subst_phi_uses(X, Map)end, Code),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewState = state__bb_update(State, Label, NewBB),
  handle_back_edges(Left, NewState);
handle_back_edges([], State) ->
  State.

subst_phi_uses(I, Map)->
  case hipe_icode:type(I) of
    phi ->
      Uses = uses(I),
      case [{X, Y}||X<-Uses, (Y=lookup(X, Map))/=none] of
	[] ->
	  I;
	Subst ->
	  hipe_icode:subst(Subst, I)
      end;
    _ ->
      I
  end.


%%____________________________________________________________
%%
%% Information handling help functions
%%

lookup(Key, Tree)->
  case gb_trees:lookup(Key, Tree) of
    none -> none;
    {value, {assigned, Val}} -> Val;
    {value, Val} -> Val
  end.

lookup_type(Var, Tree)->
  case gb_trees:lookup(Var, Tree) of
    none ->
      t_any();
    {value, Val} ->
      case hipe_icode:is_var(Val) of
	true ->
	  lookup(Val, Tree);
	_ ->
	  Val
      end
  end.

lookup_type_list(List, Info)->
  lookup_list(List, fun lookup_type/2, Info, []).

lookup_list(List, Info)->
  lookup_list(List, fun lookup/2, Info, []).

lookup_list([H|T], Fun, Info, Acc)->
  lookup_list(T, Fun, Info, [Fun(H, Info)|Acc]);
lookup_list([], _,  _, Acc) ->
  lists:reverse(Acc).

enter([Key], Value, Tree)->
  enter(Key, Value, Tree);
enter(Key, Value, Tree)->
  case t_is_any(Value) of
    true ->
      Tree;
    _ ->
      enter_to_leaf(Key, Value, Tree)
  end.

enter_to_leaf(Key, Value, Tree)->
  case gb_trees:lookup(Key, Tree) of
    {value, Value} ->
      Tree;
    {value, Val} ->
      case hipe_icode:is_var(Val) of
	true->
	  enter_to_leaf(Val, Value, Tree);
	_ ->
	  gb_trees:enter(Key, Value, Tree)
      end;
    _ ->
      gb_trees:insert(Key, Value, Tree)
  end.

enter_defines(I, Types, Info)when is_list(Types)->
  case defines(I) of
    []-> Info;
    Def->
      {NewInfo, _} =
	lists:foldl(fun(X, {Info, [Type|Tail]})->
			{enter(X,Type,Info), Tail}end,
		    {Info, Types}, Def),
      NewInfo
  end;
enter_defines(I, Type, Info)->
  case defines(I) of
    []-> Info;
    Def->
      lists:foldl(fun(X, Acc)->enter(X, Type, Acc)end, Info, Def)
  end.

join_list(List, Info)->
  join_list(List, Info, t_undefined()).
 
join_list([H|T], Info, Acc)->
  Type = t_sup(lookup_type(H, Info), Acc),
  join_list(T, Info, Type);
join_list([], _, Acc) ->
  Acc.

join_info_in([{Var, Type}|Tail], InfoIn)->
  case gb_trees:lookup(Var, InfoIn) of
    none ->
      join_info_in(Tail, enter(Var, Type, InfoIn));
    {value, Type} ->
      join_info_in(Tail, InfoIn);
    {value, OldType}->
      join_info_in(Tail, enter(Var, t_sup(OldType, Type), InfoIn))
  end;
join_info_in([], InfoIn) ->
  InfoIn.

join_maps(Pred, BlockMap)->
  join_maps(Pred, BlockMap, empty()).

join_maps([Pred|Left], BlockMap, Map)->
  case gb_trees:lookup(Pred, BlockMap) of
    none ->
      join_maps(Left, BlockMap, Map);
    {value, OldMap} ->
      NewMap = join_maps0(gb_trees:to_list(OldMap), Pred, Map),
      join_maps(Left, BlockMap, NewMap)
  end;
join_maps([], _, Map) ->
  Map.

join_maps0([{Var, FVar}|Tail], Pred,  Map)->
  case lookup(Var, Map) of
    none ->
      join_maps0(Tail, Pred, gb_trees:enter(Var, [{Pred, FVar}], Map));
    Val ->
      join_maps0(Tail, Pred, gb_trees:enter(Var, [{Pred, FVar}|Val], Map))
  end;
join_maps0([], _, Map) ->
  Map.

%% The map has information about from which predecessor a particular
%% binding comes from. When this information has been used we strip
%% the map of it.
init_map(Map)->
  init_map(gb_trees:to_list(Map), empty()).

init_map([{Var, [{_, FVar}|_]}|Left], Acc)->
  init_map(Left, gb_trees:insert(Var, FVar, Acc));
init_map([{Var, FVar}|Left], Acc)->
  init_map(Left, gb_trees:insert(Var, FVar, Acc));
init_map([], Acc) ->
  Acc.

defines(I)->
  hipe_icode:defines(I).

uses(I)->
  hipe_icode:uses(I).

remove_duplicates(List)->
  remove_duplicates(List, []).
remove_duplicates([X|Left], Acc)->
  case lists:member(X, Acc) of
    true ->
      remove_duplicates(Left, Acc);
    false ->
      remove_duplicates(Left, [X|Acc])
  end;
remove_duplicates([], Acc) ->
  Acc.

print_state_info(State)->
  Labels = hipe_icode_cfg:reverse_postorder(state__cfg(State)),
  lists:foreach(
    fun(X)-> 
	io:format("Label ~w:\n", [X]),
	lists:foreach(
	  fun({Y, Z})->
	      io:format("\t~w: ~s\n", [Y, t_to_string(Z)])
	  end,
	  gb_trees:to_list(state__info_out(State, X)))
    end,
    Labels),
  io:format("=========================\n", []),
  ok.

const_type(Var)->
  case hipe_icode:is_const(Var) of
    false ->
      not_a_constant;
    true ->
      t_from_term(hipe_icode:const_value(Var))
  end.

is_fop_cand(I, Info)->
  case get_info(I) of
    false -> false;
    {false, _, _} -> false;
    _ ->
      case [X||X<-uses(I), t_is_float(lookup_type(X, Info))] of
	[] -> false;
	_ -> true
      end
  end.

get_info(I)->
  case hipe_icode:type(I) of
    call ->
      Fail = hipe_icode:call_fail(I),
      Cont = hipe_icode:call_continuation(I),
      Op = fun_to_fop(hipe_icode:call_fun(I)), 
      {Op, Cont, Fail};
    enter ->
      {fun_to_fop(hipe_icode:enter_fun(I)), [], []};
    _ ->
      false
  end.

fun_to_fop(Fun)->
  case Fun of
    '+' -> fp_add;
    '-' -> fp_sub;
    {erlang, '/', 2} -> fp_div;
    {erlang, '*', 2} -> fp_mul;
    _ ->false
  end.

must_be_tagged(Var, Map)->
  %% If there is a tagged version of this variable available we don't
  %% have to tag the untagged version.
  case gb_trees:lookup(Var, Map) of
    none -> false;
    {value, {assigned, _}} -> false; 
    _ ->true
  end.

%% _________________________________________________________________
%%
%% Handling the state
%%

-record(state, {info, info_map, block_map, cfg}).

new_state(Cfg)->
  Start = hipe_icode_cfg:start(Cfg),  
  Info = case lists:keysearch(arg_type, 1, hipe_icode_cfg:info(Cfg)) of
	   false ->
	     Any = t_any(),
	     lists:foldl(fun(X, Tree)->gb_trees:insert(X, Any, Tree)end,
			 empty(), hipe_icode_cfg:params(Cfg));
	   {value,{_, ArgType}}->
	     add_arg_types(hipe_icode_cfg:params(Cfg), ArgType)
	 end,
  InfoMap = gb_trees:insert({Start, in}, Info, empty()),
  new_state(Cfg, InfoMap).

new_state(Cfg, InfoMap)->
  Start = hipe_icode_cfg:start(Cfg),  
  Block_map = gb_trees:insert({Start, inblock}, false, empty()),
  #state{info_map=InfoMap, cfg=Cfg, block_map=Block_map}.

add_arg_types(Args,Types)->
  add_arg_types(Args, Types, empty()).

add_arg_types([Arg|Args],[Type|Types], Acc)->
  add_arg_types(Args,Types, enter(Arg, Type, Acc));
add_arg_types([],[],Acc) ->
  Acc;
add_arg_types(A,B,_) ->
  exit({wrong_number_of_arguments, {A, B}}).

empty()->
  gb_trees:empty().

state__cfg(#state{cfg=Cfg})->
  Cfg.

state__succ(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:succ(hipe_icode_cfg:succ_map(Cfg), Label).

state__pred(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:pred(hipe_icode_cfg:pred_map(Cfg), Label).

state__bb(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_update(S=#state{cfg=Cfg}, Label, BB)->
  NewCfg = hipe_icode_cfg:bb_update(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__info_in(S, Label)->
  state__info(S, {Label, in}).

state__info_out(S, Label)->
  state__info(S, {Label, out}).

state__info(#state{info_map=IM}, Label)->
  case gb_trees:lookup(Label, IM) of
    {value, Info}-> Info;
    _ -> empty()
  end.

state__info_in_update(S=#state{info_map=IM}, Label, Info)->
  case gb_trees:lookup({Label, in}, IM) of
    none ->
      S#state{info_map=gb_trees:enter({Label, in}, Info, IM)};
    {value, Info} ->
      fixpoint;
    {value, OldInfo} ->
      NewInfo = join_info_in(gb_trees:to_list(OldInfo), Info),
      S#state{info_map=gb_trees:enter({Label, in}, NewInfo, IM)}
  end.

state__info_out_update(S=#state{info_map=IM}, Label, Info)->
  S#state{info_map=gb_trees:enter({Label, out}, Info, IM)}.

state__map(S=#state{block_map=BM}, Label)->
  join_maps(state__pred(S, Label), BM).

state__map_update(S=#state{block_map=BM}, Label, Map, InBlock)->
  NewBM0 = gb_trees:enter(Label, Map, BM),
  Succ = state__succ(S, Label),
  Fun = fun(X, Acc)->gb_trees:enter({X, inblock}, InBlock, Acc)end,
  NewBM = lists:foldl(Fun, NewBM0, Succ),
  S#state{block_map=NewBM}.  

state__in_block(#state{block_map=BM}, Label)->
  case gb_trees:lookup({Label, inblock}, BM) of
    {value, Ans}->
      Ans
  end.
