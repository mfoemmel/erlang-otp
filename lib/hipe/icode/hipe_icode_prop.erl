%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Semi-local copy propagation, constant propagation, constant folding,
%% and dead code removal.
%%
%% Works on extended basic blocks. No iteration is done at this point.
%%
%% The environment binds (icode) variables to:
%%    - constants
%%    - variables
%%
-include("../main/hipe.hrl").
-module(hipe_icode_prop).
-export([cfg/1,remove_dead_code/1]).

cfg(CFG) ->
  %% io:format("Icode prop\n",[]),
  %% Code = hipe_icode:icode_code(hipe_icode_cfg:linearize(CFG)),
  %% ActualVmax = hipe_icode:highest_var(Code),
  %% ActualLmax = hipe_icode:highest_label(Code),
  ?opt_start_timer("Icode prop init"),
  {_VMin,VMax} = hipe_icode_cfg:var_range(CFG),
  {_LMin,LMax} = hipe_icode_cfg:label_range(CFG),
  hipe_gensym:set_label(icode,LMax+1),
  hipe_gensym:set_var(icode,VMax+1),
  ?opt_stop_timer("Icode prop init"),
  %% io:format("~w - ~w\n",[VMax,ActualVmax]),
  %% io:format("~w - ~w\n",[LMax,ActualLmax]),
  ?opt_start_timer("Icode prop EBB"),
  EBBs = hipe_icode_ebb:cfg(CFG),
  ?opt_stop_timer("Icode prop EBB"),
  ?opt_start_timer("Icode prop consts"),
  CFG0 = prop_ebbs(EBBs, CFG),
  ?opt_stop_timer("Icode prop consts"),
  CFG1 = hipe_icode_cfg:var_range_update(CFG0, {0,hipe_gensym:get_var(icode)}),
  CFG2 = hipe_icode_cfg:label_range_update(CFG1, {0,hipe_gensym:get_label(icode)}),
  %% io:format("~w - ~w\n",[VMax,ActualVmax]),
  %% hipe_icode_cfg:pp(CFG1),
  ?opt_start_timer("Icode prop DeadCode"),
  CFG3 = dead_code(CFG2),
  ?opt_stop_timer("Icode prop DeadCode"),
  CFG4 = hipe_icode_cfg:var_range_update(CFG3, {0,hipe_gensym:get_var(icode)}),
  CFG5 = hipe_icode_cfg:label_range_update(CFG4, {0,hipe_gensym:get_label(icode)}),
  CFG5.

remove_dead_code(CFG) ->
   ?opt_start_timer("Icode DeadCode init"),
  {_VMin,VMax} = hipe_icode_cfg:var_range(CFG),
  {_LMin,LMax} = hipe_icode_cfg:label_range(CFG),
  hipe_gensym:set_label(icode,LMax+1),
  hipe_gensym:set_var(icode,VMax+1),
  ?opt_stop_timer("Icode DeadCode init"),
  ?opt_start_timer("Icode DeadCode"),
  CFG2 = dead_code(CFG),
  ?opt_stop_timer("Icode DeadCode"),
  CFG3 = hipe_icode_cfg:var_range_update(CFG2, {0,hipe_gensym:get_var(icode)}),
  CFG4 = hipe_icode_cfg:label_range_update(CFG3, {0,hipe_gensym:get_label(icode)}),
  CFG4. 

%%
%% Iterate over the extended basic blocks of a cfg.
%%

prop_ebbs([], CFG) ->
  CFG;
prop_ebbs([Ebb|Ebbs], CFG) ->
  CFG0 = prop_ebb(Ebb, new_env(), CFG),
  prop_ebbs(Ebbs, CFG0).


%%
%% Propagate info through a block and down to its successors.
%%

prop_ebb(Ebb, Env, CFG) ->
  case hipe_icode_ebb:type(Ebb) of
    node ->
      Lbl = hipe_icode_ebb:node_label(Ebb),
      BB = hipe_icode_cfg:bb(CFG, Lbl),
      {NewCode, NewEnv} = prop_instrs(hipe_bb:code(BB), Env),
      NewBB = hipe_bb:code_update(BB, NewCode),
      NewCFG = hipe_icode_cfg:bb_update(CFG, Lbl, NewBB),
      Succ = hipe_icode_ebb:node_successors(Ebb),
      prop_succ(Succ, NewEnv, NewCFG);
    leaf ->
      CFG
  end.


prop_succ([], _Env, CFG) ->
  CFG;
prop_succ([Ebb|Ebbs], Env, CFG) ->
  NewCFG = prop_ebb(Ebb, Env, CFG),
  prop_succ(Ebbs, Env, NewCFG).

prop_instrs(Is, Env) ->
  prop_instrs(Is,Env,[]).

%% Acc is a flat reversed list of instructions. 
prop_instrs([], Env, Acc) ->
  {lists:reverse(Acc), Env};
prop_instrs([I|Is], Env, Acc) ->
  {NewI, Env0} = prop_instr(I, Env),
  %% io:format("To  ~w\n",[NewI]),
  if is_list(NewI) ->
      prop_instrs(Is, Env0, reverse_append(NewI,Acc));
     true ->
      prop_instrs(Is, Env0, [NewI|Acc])
  end.


%% reverse_append(List,Acc)
%% Flatten and reverses the list List and then appends Acc.
%% This is fixes instruction sequences such as:
%%  List = [5,[6,7],8,[9,[10,11]]],   Acc =[4,3,2,1]
%% To [11,10,9,8,7,6,5,4,3,2,1]
reverse_append([],L) ->
  L;
reverse_append([H|T], L) when is_list(H) ->
  reverse_append(T, reverse_append(H,L));
reverse_append([H|T], L) ->
  reverse_append(T, [H|L]).


%%
%% Propagate copies and constants for one instruction
%%  I in the environment Env.
%% Returns a tuple:
%%  {NewInstruction, NewEnv}
%%  The instuction may expand to a deep-list of instructions.

prop_instr(I, Env) ->
  %% io:format("Prop I ~w\n",[I]),
  NewEnv = unbind(hipe_icode:defines(I), Env),
  case hipe_icode:is_mov(I) of
    true ->
      Src = lookup(hipe_icode:mov_src(I), Env),
      %% Uncomment this and only constants will be propagated
      %%case hipe_icode:is_const(Src) of
      %%   true ->
      NewI = hipe_icode:mov_src_update(I, Src),
      %%io:format("Propagates ~w to ~w\n",[hipe_icode:mov_src(I), Src]),
      {NewI, bind(NewEnv, hipe_icode:mov_dst(I), Src)};
    %%   false ->
    %%      {I, NewEnv}
    %%end;
    false ->
      Uses = hipe_icode:args(I),
      VarMap = map_vars(Uses, Env),
      Instr = hipe_icode:subst_uses(VarMap, I),
      ValUses = lookup_all(Uses, Env),
      case all_const(ValUses) of
	true ->
	  %% all arguments are constant
	  ConstMap = map_consts(Uses, Env),
	  eval(Instr, hipe_icode:subst_uses(ConstMap, Instr), NewEnv);
	false ->
	  {Instr, NewEnv}
      end
  end.


map_vars([], _Env) ->
  [];
map_vars([V|Vs], Env) ->
  [{V, lookup_var(V, Env)} | map_vars(Vs, Env)].


map_consts([], _Env) ->
  [];
map_consts([C|Cs], Env) -> 
  case hipe_icode:is_var_or_fvar_or_reg(C) of
    true ->
      [{C, hipe_icode:const_value(lookup(C, Env))} | map_consts(Cs, Env)];
    false ->
      %% io:format("C: ~w, ~w\n",[C, hipe_icode:const_value(C)]),
      [{C, hipe_icode:const_value(C)} | map_consts(Cs, Env)]
      %%  map_consts(Cs, Env)
  end.

lookup_all([], _Env) ->
  [];
lookup_all([V|Vs], Env) ->
  [lookup(V, Env) | lookup_all(Vs, Env)].


all_const([]) ->
  true;
all_const([V|Vs]) ->
  case hipe_icode:is_const(V) of
    true ->
      all_const(Vs);
    false ->
      false
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Evaluate an instruction. All arguments are untagged constants.
%% Returns {NewI, NewEnv}.
%%

eval(OrgI, I, Env) ->
  case hipe_icode:type(I) of
    call ->
      eval_call(OrgI, I, Env);
    enter ->
      eval_enter(OrgI, I, Env);
    'if' ->
       %% io:format("~w\n", [hipe_icode:if_args(I)]),
      case eval_if_cond(hipe_icode:if_op(I), hipe_icode:if_args(I)) of
	true ->
	  {hipe_icode:mk_goto(hipe_icode:if_true_label(I)), Env};
	false ->
	  {hipe_icode:mk_goto(hipe_icode:if_false_label(I)), Env};
	unknown ->
	  {OrgI, Env}
      end;
    type -> 
       %% io:format("~w\n", [I]),
      case eval_type_cond(hipe_icode:type_type(I),hipe_icode:type_var(I)) of
	true ->
	  {hipe_icode:mk_goto(hipe_icode:type_true_label(I)), Env};
	false ->
	  {hipe_icode:mk_goto(hipe_icode:type_false_label(I)), Env};
	unknown ->
	  {OrgI, Env}
      end;

    _ ->
      {OrgI, Env}
  end.

eval_call(OrgI, I, Env) ->
  %% Evaluate an op with constant arguments.
  %% XXX: Arithmetic on f.p. numbers may fail.
  %% For example, 3.23e133*3.57e257 throws {EXIT,{badarith,_}}.
  %%
  %% Consider breaking up this into a dispatch function...

  Args = hipe_icode:call_args(I),
  Dst = hipe_icode:call_dst(I),
  Cont = hipe_icode:call_continuation(OrgI),
  Goto =
    case Cont of
      [] -> %% Just a fallthrough.
	hipe_icode:mk_comment("Removed call");
      _ ->
	hipe_icode:mk_goto(Cont)
    end,

  case hipe_icode:call_fun(I) of
    cons ->
      [Hd, Tl] = Args,
      [Dst0] = Dst,
      Const = hipe_icode:mk_const([Hd|Tl]),
      {[hipe_icode:mk_mov(Dst0, Const),
	Goto],
       bind(Env, Dst0, Const)};
    mktuple ->
      [Dst0] = Dst,
      Const = hipe_icode:mk_const(list_to_tuple(Args)),
      {[hipe_icode:mk_mov(Dst0, Const),
	Goto], bind(Env, Dst0, Const)};

    {erlang,hd,1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      case Arg of
	[Hd|_] ->
	  Const = hipe_icode:mk_const(Hd),
	  {[hipe_icode:mk_mov(Dst0, Const),
	    Goto],
	   bind(Env, Dst0, Const)};
	_ -> %% This will fail at runtime...
	  {OrgI, Env}
      end;
    {erlang,tl,1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      case Arg of
	[_|Tl] ->
	  Const = hipe_icode:mk_const(Tl),
	  {[hipe_icode:mk_mov(Dst0, Const),
	    Goto],
	   bind(Env, Dst0, Const)};
	_ -> %% This will fail at runtime...
	  {OrgI, Env}
      end;

    '+' ->
      [Arg1, Arg2] = Args,
      [Dst0] = Dst,
      if is_number(Arg1), is_number(Arg2) ->
	  arith(catch (Arg1 + Arg2), Dst0, Goto, OrgI, Env);
	 true -> {OrgI, Env}
      end;
    '-' ->
      [Arg1, Arg2] = Args,
      [Dst0] = Dst,
      if is_number(Arg1), is_number(Arg2) ->
	  arith(catch (Arg1 - Arg2), Dst0, Goto, OrgI, Env);
	 true -> {OrgI, Env}
      end;
    '*' ->
      [Arg1, Arg2] = Args,
      [Dst0] = Dst,
      if is_number(Arg1), is_number(Arg2) ->
	  arith(catch (Arg1 * Arg2), Dst0, Goto, OrgI, Env);
	 true -> {OrgI, Env}
      end;
    '/' ->
      [Arg1, Arg2] = Args,
      [Dst0] = Dst,
      if is_number(Arg1), is_number(Arg2) ->
	  arith(catch (Arg1 / Arg2), Dst0, Goto, OrgI, Env);
	 true -> {OrgI, Env}
      end;
    {unsafe_element,N} ->
      [Arg1] = Args,
      [Dst0] = Dst,    
      if is_tuple(Arg1),N =< size(Arg1) ->
	  Const = hipe_icode:mk_const(element(N, Arg1)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	 true ->{OrgI, Env}
      end;
	  
    {erlang,element,2} ->
      [Arg1, Arg2] = Args,
      [Dst0] = Dst,
      if is_integer(Arg1), is_tuple(Arg2), Arg1 > 0, Arg1 =< size(Arg2) ->
	  Const = hipe_icode:mk_const(element(Arg1, Arg2)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	 true ->
	  Fail = hipe_icode:call_fail(OrgI),
	  case Fail of 
	    [] -> 
	      R = hipe_icode:mk_new_var(),
	      Const = hipe_icode:mk_const(badarg),
	      MoveI = hipe_icode:mk_mov(R, Const),
	      FailI = hipe_icode:mk_fail([R], exit),
	      case hipe_icode:call_continuation(OrgI) of
		[] ->
		  LI = hipe_icode:mk_new_label(),
		  {[MoveI, FailI, LI], Env};
		Cont ->
		  {[MoveI, FailI], Env}
	      end;
	    _ -> {OrgI, Env}
	  end
      end;
    {erlang, setelement, 3} ->
      [Arg1, Arg2, Arg3] = Args,
      [Dst0] = Dst,
      if is_integer(Arg1), is_tuple(Arg2), Arg1 > 0, Arg1 =< size(Arg2) ->
	  Const = hipe_icode:mk_const(setelement(Arg1, Arg2, Arg3)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const),Goto], Env1};
	 true ->
	  {OrgI, Env}
      end;
    {erlang, atom_to_list, 1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      if is_atom(Arg) ->
	  Const = hipe_icode:mk_const(atom_to_list(Arg)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const),Goto], Env1};
	 true ->
	  {OrgI, Env}
      end;
    {erlang, list_to_atom, 1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      if is_list(Arg) ->
	  Const = hipe_icode:mk_const(list_to_atom(Arg)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	 true ->
	  {OrgI, Env}
      end;
    {erlang, tuple_to_list, 1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      if is_tuple(Arg) ->
	  Const = hipe_icode:mk_const(tuple_to_list(Arg)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	 true ->
	  {OrgI, Env}
      end;
    {erlang, list_to_tuple, 1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      if is_list(Arg) ->
	  case catch hipe_icode:mk_const(list_to_tuple(Arg)) of
	    {'EXIT',_} -> {OrgI, Env};
	    Const  ->
	      Env1 = bind(Env, Dst0, Const),
	      {[hipe_icode:mk_mov(Dst0, Const),Goto], Env1}
	  end;
	 true ->
	  {OrgI, Env}
      end;
    {erlang, length, 1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      if is_list(Arg) ->
	  Const = hipe_icode:mk_const(length(Arg)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	 true ->
	  {OrgI, Env}
      end;
    {erlang, size, 1} ->
      [Arg] = Args,
      [Dst0] = Dst,
      if is_tuple(Arg) ->
	  Const = hipe_icode:mk_const(size(Arg)),
	  Env1 = bind(Env, Dst0, Const),
	  {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	 true ->
	  {OrgI, Env}
      end;
    {erlang,abs,1} ->
      [Src] = Args,
      [Dst0] = Dst,
      if is_number(Src) ->
	  case catch(abs(Src)) of
	    {'EXIT', _ } ->
	      {OrgI, Env};
	    Res ->
	      Const = hipe_icode:mk_const(Res),
	      Env1 = bind(Env, Dst0, Const),
	      {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1}
	  end;
	 true ->
	  {OrgI, Env}
      end;
    {mkfun, {_Mod, _Fun, _Arity}, _Unique, _Index} ->
      {OrgI, Env};
    %% Not yet implemented!
    %% [Dst0] = Dst,
    %% Const = hipe_icode:mk_const_fun({Mod, Fun, Arity}, Unique, Index, Args),
    %% Env1 = bind(Env, Dst0, Const),
    %% {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};

    conv_to_float ->
      [Src] = Args,
      [Dst0] = Dst,
      if is_number(Src) ->
	  case catch(float(Src)) of
	    {'EXIT', _ } ->
	      {OrgI, Env};
	    Res ->
	      Src2 = hipe_icode:mk_const(Res),
	      {[hipe_icode:mk_primop([Dst0], unsafe_untag_float, [Src2]), 
		Goto], Env}
	  end;
	 true ->
	  {OrgI, Env}
      end;

    _NotHandled ->
      %% io:format("Not handled ~w\n",[NotHandled]),
      {OrgI, Env}
  end.

eval_enter(OrgI, _I, Env) ->
  %% Consider optimisations of apply etc
  {OrgI, Env}.



arith(Val, Dst, Goto, _, Env) when is_number(Val) ->
  Const = hipe_icode:mk_const(Val), 
  {[hipe_icode:mk_mov(Dst, Const), Goto], 
   bind(Env, Dst, Const)};
arith(_, _, _, OrgI, Env) ->
  {OrgI, Env}.


%%
%% Evaluate an if condition. Returns 'true', 'false' or 'unknown'
%%

eval_if_cond(Cond, [Arg1, Arg2]) -> 
  case Cond of
    '=:=' ->
      Arg1 =:= Arg2;
    '==' ->
      Arg1 == Arg2;
    '=/=' ->
      Arg1 =/= Arg2;
    '/=' ->
      Arg1 /= Arg2;
    '<' ->
      Arg1 < Arg2;
    '>=' ->
      Arg1 >= Arg2;
    '=<' ->
      Arg1 =< Arg2;
    '>' ->
      Arg1 > Arg2;
    _ ->
      unknown
  end;
eval_if_cond(_Cond, _Args) ->
  unknown.


%%
%% Evaluate a type test. Returns 'true', 'false' or 'unknown'
%%

eval_type_cond(Type, Arg) ->
  case {Type, Arg} of
    {nil, []} -> true;
    {nil, _} -> false;
    {cons, [_|_]} -> true;
    {cons, _} -> false;
    {{tuple,N},T} when is_tuple(T), size(T) == N ->
      true;
    _ ->
      unknown
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Dead code elimination
%%

dead_code(CFG) ->
  EBBs = hipe_icode_ebb:cfg(CFG),
  dead_code_ebbs(EBBs, CFG).


dead_code_ebbs(EBBs, CFG) ->
  Live = hipe_icode_liveness:analyze(CFG),
  {CFG0, Changed} = dead_code_ebbs(EBBs, CFG, Live, false),
  case Changed of 
    true -> dead_code(CFG0);
    false -> CFG0
  end.


dead_code_ebbs([], CFG, _Live, Changed) ->
  {CFG, Changed};
dead_code_ebbs([EBB|EBBs], CFG, Live, Changed) ->
  {CFG0, _, Changed0} = dead_code_ebb(EBB, CFG, Live),
  dead_code_ebbs(EBBs, CFG0, Live, Changed or Changed0).


dead_code_ebb(EBB, CFG, Live) ->
  case hipe_icode_ebb:type(EBB) of
    node ->
      Lbl = hipe_icode_ebb:node_label(EBB),
      Succ = hipe_icode_ebb:node_successors(EBB),
      {CFG0, LiveOut, Changed0} = dead_code_succ(Succ, CFG, Live),
      BB = hipe_icode_cfg:bb(CFG0, Lbl),

      {NewCode, LiveIn, Changed1} = dead_code_instrs(hipe_bb:code(BB), LiveOut),

      NewBB = hipe_bb:code_update(BB, NewCode),
      NewCFG = hipe_icode_cfg:bb_update(CFG0, Lbl, NewBB),
      {NewCFG, LiveIn, Changed0 or Changed1};
    leaf ->
      Lbl = hipe_icode_ebb:leaf_next(EBB),
      {CFG, hipe_icode_liveness:livein(Live, Lbl), false}
  end.


dead_code_succ([], CFG, _Live) ->
  {CFG, ordsets:new(), false};
dead_code_succ([EBB|EBBs], CFG, Live) ->
  {CFG0, LiveOut0, Changed0} = dead_code_ebb(EBB, CFG, Live),
  {NewCFG, LiveOut1, Changed1} = dead_code_succ(EBBs, CFG0, Live),
  {NewCFG, ordsets:union(LiveOut0, LiveOut1), Changed0 or Changed1}.


dead_code_instrs([], LiveOut) ->
  {[], LiveOut, false};
dead_code_instrs([I|Is], LiveOut) ->
  {NewIs, LiveOut0, Changed} = dead_code_instrs(Is, LiveOut),
  Def = ordsets:from_list(hipe_icode:defines(I)),
  Dead = ordsets:intersection(LiveOut0, Def) =:= ordsets:new(),
  case {hipe_icode:is_pure(I), Dead} of

    {true, true} ->
      case hipe_icode:is_call(I) of
	true ->
	  case hipe_icode:call_continuation(I) of
	    [] -> 
	      {NewIs, LiveOut0, true};
	    CC ->
	      {[hipe_icode:mk_goto(CC)|NewIs],
	       LiveOut0, true}
	  end;
	false ->
	  {NewIs, LiveOut0, true}
      end;
    {false,true} ->
      case hipe_icode:is_call(I) of
	true ->
	  case hipe_icode:call_dst(I) of
	    [_Dst] -> 
	      %% This Call uptdates a dead var
	      %% Remove the update.
	      Use = ordsets:from_list(hipe_icode:uses(I)),
	      LiveIn = ordsets:union(Use,LiveOut0),
	      {[hipe_icode:call_dst_update(I,[])|NewIs],
	       LiveIn, true};
	    _ -> %% Multiple return values, 
	      %% Hard to do anything nice here.
	      Use = ordsets:from_list(hipe_icode:uses(I)),
	      LiveIn = ordsets:union(Use, ordsets:subtract(LiveOut0, Def)),
	      {[I|NewIs], LiveIn, Changed}
	  end;
	_ -> %% Not a call, and not pure...
	  Use = ordsets:from_list(hipe_icode:uses(I)),
	  LiveIn = ordsets:union(Use, ordsets:subtract(LiveOut0, Def)),
	  {[I|NewIs], LiveIn, Changed}
      end;
    _ ->
      case dead_move(I) of
	true ->
	  {NewIs, LiveOut0, true};
	false ->
	  Use = ordsets:from_list(hipe_icode:uses(I)),
	  LiveIn = ordsets:union(Use, ordsets:subtract(LiveOut0, Def)),
	  {[I|NewIs], LiveIn, Changed}
      end
  end.


%%
%% Identity moves and fmoves that can be safely deleted.
%%

dead_move(X) ->
  Temp1 = hipe_icode:is_mov(X) 
    and (hipe_icode:mov_src(X) =:= hipe_icode:mov_dst(X)),
  Temp2 = hipe_icode:is_fmov(X)
    and (hipe_icode:fmov_src(X) =:= hipe_icode:fmov_dst(X)),
  Temp1 or Temp2.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The environment. Rewrite if we go global.
%%

%% An environment has two mappings:
%% If x is bound to y then map 1 contains {x,y} and map 2 contains {y,[x|_]}.
%%
new_env() ->
  {gb_trees:empty(),gb_trees:empty()}.

%%
%% Find what X is bound to (as a last resort variables are bound to
%% themselves).
%%

lookup_var(X, Map) ->
  Y = lookup(X,Map),
  case hipe_icode:is_var_or_fvar_or_reg(Y) of
    true -> Y;
    false -> X
  end.

lookup(X,{Map,_}) ->
  case gb_trees:lookup(X,Map) of
    {value, Y} -> Y;
    none -> X
  end.


%%
%% Bind X to Y in Map
%%
bind({Map1,Map2}, X, Y) -> 
  NewMap2 =
    case gb_trees:lookup(Y,Map2) of
      none -> gb_trees:enter(Y,[X],Map2);
      {value,Ys} -> gb_trees:enter(Y,[X|Ys],Map2)
    end, 
  {gb_trees:enter(X,Y,Map1),NewMap2}.


%%
%% Kill bindings with references to X
%%
kill(X,M = {Map1,Map2}) ->
  %% First check if anyting is bound to X.
  M1 = {M11,M12} =
    case gb_trees:lookup(X,Map2) of
      none -> M;
      {value,Y1s} -> %% All Y1s bound to X
	{lists:foldl(
	   fun (Y1,MapAcc) ->
	       gb_trees:delete_any(Y1,MapAcc)
	   end,Map1,Y1s),
	 gb_trees:delete(X,Map2)}
    end,
  %% Now check if X is bound to anything.
  case gb_trees:lookup(X,M11) of
    {value,Y2} -> %% X bound to Y2
      {gb_trees:delete(X,M11),
       case gb_trees:lookup(Y2,M12) of
	 none -> M12;
       {value,Y2s} ->
	 gb_trees:enter(Y2,lists:delete(X,Y2s),M12)
       end}; 
    none -> 
      M1
  end.	     


unbind([], Map) ->
  Map;
unbind([V|Vs], Map) ->
  unbind(Vs, kill(V, Map)).
