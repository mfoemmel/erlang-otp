%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Semi-local copy propagation, constant propagation, constant folding
%% and dead code removal.
%%
%% Works on extended basic blocks. No iteration is done at this point.
%%
%% The environment binds (icode) variables to:
%%    - constants
%%    - variables
%%

-module(hipe_icode_prop).
-export([cfg/1]).

cfg(CFG) ->
  %% io:format("Icode prop\n",[]),
  Code = hipe_icode:icode_code(hipe_icode_cfg:linearize(CFG)),
  ActualVmax = hipe_icode:highest_var(Code),
  ActualLmax = hipe_icode:highest_label(Code),

  hipe_gensym:set_label(icode,ActualLmax+1),
  hipe_gensym:set_var(icode,ActualVmax+1),

  %% io:format("~w - ~w\n",[VMax,ActualVmax0]),
  EBBs = hipe_icode_ebb:cfg(CFG),
  CFG0 = prop_ebbs(EBBs, CFG),
  CFG1 = hipe_icode_cfg:var_range_update(CFG0, {0,hipe_gensym:get_var(icode)}),
  CFG2 = hipe_icode_cfg:label_range_update(CFG1, {0,hipe_gensym:get_label(icode)}),
  %%io:format("~w - ~w\n",[VMax,ActualVmax]),
  %% hipe_icode_cfg:pp(CFG1),
  CFG3 = dead_code(CFG2),
  CFG4 = hipe_icode_cfg:var_range_update(CFG3, {0,hipe_gensym:get_var(icode)}),
  CFG5 = hipe_icode_cfg:label_range_update(CFG4, {0,hipe_gensym:get_label(icode)}),
  CFG5.


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
	 NewBB = hipe_bb:code_update(BB, lists:flatten(NewCode)),
	 NewCFG = hipe_icode_cfg:bb_update(CFG, Lbl, NewBB),
	 Succ = hipe_icode_ebb:node_successors(Ebb),
	 prop_succ(Succ, NewEnv, NewCFG);
      leaf ->
	 CFG
   end.


prop_succ([], Env, CFG) ->
   CFG;
prop_succ([Ebb|Ebbs], Env, CFG) ->
   NewCFG = prop_ebb(Ebb, Env, CFG),
   prop_succ(Ebbs, Env, NewCFG).


prop_instrs([], Env) ->
   {[], Env};
prop_instrs([I|Is], Env) ->
   {NewI, Env0} = prop_instr(I, Env),
   %% io:format("To  ~w\n",[NewI]),
   {NewIs, NewEnv} = prop_instrs(Is, Env0),
   {[NewI|NewIs], NewEnv}.


%%
%% Propagate copies and constants for one instruction.
%%

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


map_vars([], Env) ->
   [];
map_vars([V|Vs], Env) ->
   [{V, lookup_var(V, Env)} | map_vars(Vs, Env)].


map_consts([], Env) ->
   [];
map_consts([C|Cs], Env) ->
    case hipe_icode:is_var(C) of 
	true ->
	    [{C, hipe_icode:const_value(lookup(C, Env))} | map_consts(Cs, Env)];
	false ->
	    %% io:format("C: ~w, ~w\n",[C, hipe_icode:const_value(C)]),
	    [{C, hipe_icode:const_value(C)} | map_consts(Cs, Env)]
	  %%  map_consts(Cs, Env)
    end.

lookup_all([], Env) ->
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
  Dest = hipe_icode:call_fun(I),
						
  %% Evaluate an op with constant arguments.
  %% XXX: Arithmetic on f.p. numbers may fail.
  %% For example, 3.23e133*3.57e257 throws {EXIT,{badarith,_}}.

  %% io:format("Eval ~w\n",[I]),
  Args = hipe_icode:call_args(I),
  Dst = hipe_icode:call_dst(I),
  Cont = hipe_icode:call_continuation(OrgI),
  Goto = hipe_icode:mk_goto(Cont),
   %% io:format("~w ~w\n", [I,Args]),
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
      '+' ->
	 [Arg1, Arg2] = Args,
	 [Dst0] = Dst,
	 if number(Arg1), number(Arg2) ->
	     arith(catch (Arg1 + Arg2), Dst0, Goto, OrgI, Env);
	    true -> {OrgI, Env}
	 end;
      '-' ->
	 [Arg1, Arg2] = Args,
	 [Dst0] = Dst,
	 if number(Arg1), number(Arg2) ->
	     arith(catch (Arg1 - Arg2), Dst0, Goto, OrgI, Env);
	    true -> {OrgI, Env}
	 end;
      '*' ->
	 [Arg1, Arg2] = Args,
	 [Dst0] = Dst,
	 if number(Arg1), number(Arg2) ->
	     arith(catch (Arg1 * Arg2), Dst0, Goto, OrgI, Env);
	    true -> {OrgI, Env}
	 end;
      '/' ->
	 [Arg1, Arg2] = Args,
	 [Dst0] = Dst,
	 if number(Arg1), number(Arg2) ->
	     arith(catch (Arg1 / Arg2), Dst0, Goto, OrgI, Env);
	    true -> {I, Env}
	 end;

      {erlang,element,2} ->
	 [Arg1, Arg2] = Args,
	 [Dst0] = Dst,
	 if integer(Arg1), tuple(Arg2), Arg1 > 0, Arg1 =< size(Arg2) ->
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
		   FailI = hipe_icode:mk_fail(R,exit),
		   {[MoveI, FailI], Env};
		 _ -> {OrgI, Env}
	       end
	 end;
      {erlang, setelement, 3} ->
	 [Arg1, Arg2, Arg3] = Args,
	 [Dst0] = Dst,
	 if integer(Arg1), tuple(Arg2), Arg1 > 0, Arg1 =< size(Arg2) ->
	       Const = hipe_icode:mk_const(setelement(Arg1, Arg2, Arg3)),
	       Env1 = bind(Env, Dst0, Const),
	       {[hipe_icode:mk_mov(Dst0, Const),Goto], Env1};
	    true ->
	       {OrgI, Env}
	 end;
      {erlang, atom_to_list, 1} ->
	 [Arg] = Args,
	 [Dst0] = Dst,
	 if atom(Arg) ->
	       Const = hipe_icode:mk_const(atom_to_list(Arg)),
	       Env1 = bind(Env, Dst0, Const),
	       {[hipe_icode:mk_mov(Dst0, Const),Goto], Env1};
	    true ->
	       {OrgI, Env}
	 end;
      {erlang, list_to_atom, 1} ->
	 [Arg] = Args,
	 [Dst0] = Dst,
	 if list(Arg) ->
	       Const = hipe_icode:mk_const(list_to_atom(Arg)),
	       Env1 = bind(Env, Dst0, Const),
	       {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	    true ->
	       {OrgI, Env}
	 end;
      {erlang, tuple_to_list, 1} ->
	 [Arg] = Args,
	 [Dst0] = Dst,
	 if tuple(Arg) ->
	       Const = hipe_icode:mk_const(tuple_to_list(Arg)),
	       Env1 = bind(Env, Dst0, Const),
	       {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	    true ->
	       {OrgI, Env}
	 end;
      {erlang, list_to_tuple, 1} ->
	 [Arg] = Args,
	 [Dst0] = Dst,
	 if list(Arg) ->
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
	 if list(Arg) ->
	       Const = hipe_icode:mk_const(length(Arg)),
	       Env1 = bind(Env, Dst0, Const),
	       {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	    true ->
	       {OrgI, Env}
	 end;
      {erlang, size, 1} ->
	 [Arg] = Args,
	 [Dst0] = Dst,
	 if tuple(Arg) ->
	       Const = hipe_icode:mk_const(size(Arg)),
	       Env1 = bind(Env, Dst0, Const),
	       {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
	    true ->
	       {OrgI, Env}
	 end;
     {mkfun, {Mod, Fun, Arity}, Unique, Index} ->
       {OrgI, Env};
       %% Not yet implemented!
       %% [Dst0] = Dst,
       %% Const = hipe_icode:mk_const_fun({Mod, Fun, Arity}, Unique, Index, Args),
       %% Env1 = bind(Env, Dst0, Const),
       %% {[hipe_icode:mk_mov(Dst0, Const), Goto], Env1};
      NotHandled ->
      %% io:format("Not handled ~w\n",[NotHandled]),
	 {OrgI, Env}
   end.


eval_enter(OrgI, I, Env) ->
  Dest = hipe_icode:enter_fun(I),
  %% Consider optimisations of apply etc
  {OrgI, Env}.
  


arith(Val, Dst, Goto, _, Env) when number(Val) ->
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
eval_if_cond(Cons, Args) ->
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
   CFG0.


dead_code_ebbs([], CFG, Live, Changed) ->
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


dead_code_succ([], CFG, Live) ->
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
	   {[hipe_icode:mk_goto(hipe_icode:call_continuation(I))|NewIs],
	    LiveOut0, true};
	 false ->
	   {NewIs, LiveOut0, true}
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
%% Identity moves can be safely deleted.
%%

dead_move(X) ->
   hipe_icode:is_mov(X) and (hipe_icode:mov_src(X) =:= hipe_icode:mov_dst(X)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The environment, Rewrite if we go global.
%%

new_env() ->
   [].

%%
%% Find what X is bound to (as a last restort varaibles are bound 
%% to themself).
%%

lookup(X, []) ->
   X;
lookup(X, [{X, Y}|_]) ->
   Y;
lookup(X, [_|Map]) ->
   lookup(X, Map).

%%
%% Find the variable X is bound to.
%%

lookup_var(X, []) ->
   X;
lookup_var(X, [{X, Y}|_]) ->
   case hipe_icode:is_var(Y) of
      true -> Y;
      false -> X
   end;
lookup_var(X, [_|Map]) ->
   lookup_var(X, Map).

%%
%% Bind X to Y in Map
%%

bind(Map, X, Y) -> 
   [{X, Y} | Map].

%%
%% Kill bindings with references to X
%%

kill(X, []) ->
   [];
kill(X, [{X,_}|Xs]) ->
   kill(X, Xs);
kill(X, [{_,X}|Xs]) ->
   kill(X, Xs);
kill(X, [D|Xs]) ->
   [D | kill(X,Xs)].


unbind([], Map) ->
   Map;
unbind([V|Vs], Map) ->
   unbind(Vs, kill(V, Map)).
