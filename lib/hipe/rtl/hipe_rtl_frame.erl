%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RTL_FRAME
%%

-module(hipe_rtl_frame).
-export([add_save_restore/1]).

-include("hipe_literals.hrl").

add_save_restore(PreCFG) ->
   CFG = PreCFG, %%split_cfg(PreCFG),
   Liveness = hipe_rtl_liveness:analyze(CFG),
   Labels = hipe_rtl_cfg:labels(CFG),
   PredMap = hipe_rtl_cfg:pred_map(CFG),
   InitSaves = [{L, new_sset()} || L <- Labels],
   Saves0 = hash:init(InitSaves),
   Saves = saves_loop(CFG, Saves0, Labels, Liveness),
   StacksOut0 = hash:init([{L, new_stack()} || L <- Labels]),
   StacksOut1 = stacks_loop(CFG, PredMap, StacksOut0, Saves, Labels, Liveness),

   StacksIn0 = hash:init([{L, new_stack()} || L <- Labels]),
   StacksIn1 = init_stacksin(Labels, CFG, PredMap, StacksOut1, StacksIn0),

   %% io:format("StacksOut1 ~p~n~n", [hash:list(StacksOut1)]),
   %% io:format("StacksIn1 ~p~n~n", [hash:list(StacksIn1)]),

   {StacksIn, StacksOut} = 
      bw_stacks_loop(CFG, PredMap, StacksIn1, StacksOut1, Saves, Liveness,
		     Labels),
   
   %% io:format("StacksIn ~p~n~n", [hash:list(StacksIn)]),
   %% io:format("StacksOut ~p~n~n", [hash:list(StacksOut)]),

  CFG0 = add_save_restore(Labels, CFG, StacksOut, StacksIn, Liveness, 0, 0,[]),
  %% hipe_rtl_cfg:pp(CFG0),
  CFG0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 
%

split_cfg(CFG) ->
   {LowLbl, HighLbl} = hipe_rtl_cfg:label_range(CFG),
   hipe_gensym:set_label(rtl,HighLbl),
   {LowVar, HighVar} = hipe_rtl_cfg:var_range(CFG),
   hipe_gensym:set_var(rtl,HighVar),
   Labels = hipe_rtl_cfg:labels(CFG),
   CFG1 = split_cfg(Labels, CFG),
   hipe_rtl_cfg:label_range_update(CFG1, {LowLbl, hipe_gensym:get_label(rtl)}).


split_cfg([], CFG) ->
   CFG;
split_cfg([L|Ls], CFG) ->
   BB = hipe_rtl_cfg:bb(CFG, L),
   Code = hipe_bb:code(BB),
   {NewCode, CFG0} = bb_split_cfg(Code, CFG, false),
   CFG1 = hipe_rtl_cfg:bb_update(CFG0, L, hipe_bb:code_update(BB, NewCode)),
   split_cfg(Ls, CFG1).


bb_split_cfg([], CFG, Call) ->
   {[], CFG};
bb_split_cfg([I|Is], CFG, Call) ->
   case hipe_rtl:type(I) of
      call ->
	 case Call of
	    true -> % Split this block
	       % io:format("**** Splitting: ~w *****~n~n", [I]),
	       ContLabel = hipe_rtl:label_name(hipe_rtl:mk_new_label()),
	       {CodeRest, CFG0} = bb_split_cfg([I|Is], CFG, false),
	       CFG1 = hipe_rtl_cfg:bb_add(CFG0, ContLabel, hipe_bb:mk_bb(CodeRest)),
	       {[hipe_rtl:mk_goto(ContLabel)], CFG1};
	    false ->
	       {NewIs, CFG0} = bb_split_cfg(Is, CFG, true),
	       {[I|NewIs], CFG0}
	 end;
      _ ->
	 {NewIs, CFG0} = bb_split_cfg(Is, CFG, Call),
	 {[I|NewIs], CFG0}
   end.

   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

saves_loop(CFG, Saves, Labels, Liveness) ->
   case once(Labels, Saves, CFG, Liveness, false) of
      {Saves0, true} ->
	 saves_loop(CFG, Saves0, Labels, Liveness);
      {Saves0, false} ->
	 Saves0
   end.


once([], Saves, CFG, Liveness, Changed) ->
   {Saves, Changed};
once([L|Ls], Saves, CFG, Liveness, Changed) ->
   Succ = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), L),
   SaveOut = union_saveins(Succ, Saves),
   LiveOut = hipe_rtl_liveness:liveout(Liveness, L),
   Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, L)),
   SaveIn = lazy_instrs(Code, SaveOut, LiveOut),
   OldSaveIn = savein(Saves, L),
   Saves0 = set_savein(Saves, L, SaveIn),
   Changed0 = not(saves_equal(SaveIn, OldSaveIn)) or Changed,
   once(Ls, Saves0, CFG, Liveness, Changed0).


union_saveins([], Saves) ->
   [];
union_saveins([S|Ss], Saves) ->
   sset_union(savein(Saves, S), union_saveins(Ss, Saves)).

intersect_saveins([], Saves) ->
   new_sset();
intersect_saveins([S], Saves) ->
   savein(Saves, S);
intersect_saveins([S|Ss], Saves) ->
   sset_intersect(savein(Saves, S), intersect_saveins(Ss, Saves)).


saves_equal([], []) ->
   true;
saves_equal([], _) ->
   false;
saves_equal(_, []) ->
   false;
saves_equal([{X, _}|Xs], [{X,_}|Ys]) ->
   saves_equal(Xs, Ys);
saves_equal([_|Xs], [_|Ys]) ->
   saves_equal(Xs, Ys).



lazy_instrs([], SaveOut, LiveOut) ->
   SaveOut;
lazy_instrs([I|Is], SaveOut, LiveOut) ->
   SaveOut0 = lazy_instrs(Is, SaveOut, LiveOut),
   case hipe_rtl:type(I) of
      call ->
	 Live0 = hipe_rtl_liveness:list(Is, LiveOut),
	 Live = ordsets:subtract(Live0, ordsets:from_list(hipe_rtl:call_dst(I))),
	 ToSave = ordsets:from_list(ordsets:subtract(Live, do_not_save())),
	 NewSaves = [{X, 0} || X <- ToSave],
	 SaveOut1 = sset_inc_max(SaveOut0),
	 sset_remove(hipe_rtl:defines(I), 
		     sset_remove(hipe_rtl:uses(I), sset_union(NewSaves, SaveOut1)));
      _ ->
	 sset_remove(hipe_rtl:defines(I), sset_remove(hipe_rtl:uses(I), SaveOut0))
   end.


new_sset() ->
   [].


sset_union([], Ys) -> 
   Ys;
sset_union(Xs, []) ->
   Xs;
sset_union([{X,Xmax}|Xs], [{Y,Ymax}|Ys]) when X < Y ->
   [{X,Xmax} | sset_union(Xs, [{Y,Ymax}|Ys])];
sset_union([{X,Xmax}|Xs], [{Y,Ymax}|Ys]) when X =:= Y ->
   [{X, max(Xmax, Ymax)} | sset_union(Xs, Ys)];
sset_union([X|Xs], [Y|Ys]) ->
   [Y | sset_union([X|Xs], Ys)].


sset_intersect([], Ys) ->
   [];
sset_intersect(Xs, []) ->
   [];
sset_intersect([{X, Xmax}|Xs], [{Y, Ymax}|Ys]) when X < Y ->
   sset_intersect(Xs, [Y|Ys]);
sset_intersect([{X, Xmax}|Xs], [{Y, Ymax}|Ys]) when X =:= Y ->
   [{X, max(Xmax, Ymax)} | sset_intersect(Xs, Ys)];
sset_intersect([X|Xs], [Y|Ys]) ->
   sset_intersect([X|Xs], Ys).


sset_inc_max([]) ->
   [];
sset_inc_max([{X, Xmax}|Xs]) ->
   [{X, Xmax+1} | sset_inc_max(Xs)].


sset_remove(Xs, Set) ->
   sset_remove0(ordsets:from_list(Xs), Set).

sset_remove0([], Set) ->
   Set;
sset_remove0(_, []) ->
   [];
sset_remove0([X|Xs], [{X, _}|Ss]) ->
   sset_remove0(Xs, Ss);
sset_remove0([X|Xs], [{Y, Yt}|Ss]) when X < Y ->
   sset_remove0(Xs, [{Y, Yt}|Ss]);
sset_remove0(Xs, [S|Ss]) ->
   [S | sset_remove0(Xs, Ss)].



savein(Saves, Lbl) ->
   {found, SaveIn} = hash:lookup(Lbl, Saves),
   SaveIn.


set_savein(Saves, Lbl, SaveIn) ->
   hash:update(Lbl, SaveIn, Saves).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

stacks_loop(CFG, PredMap, StacksOut, Saves, Labels, Liveness) ->
   case stacks_once(Labels, StacksOut, Saves, Liveness, CFG, PredMap, false) of
      {StacksOut0, true} ->
	 stacks_loop(CFG, PredMap, StacksOut0, Saves, Labels, Liveness);
      {StacksOut0, false} ->
	 StacksOut0
   end.


stacks_once([], StacksOut, Saves, Liveness, CFG, PredMap, Changed) ->
   {StacksOut, Changed};
stacks_once([L|Ls], StacksOut,  Saves, Liveness, CFG, PredMap, Changed) ->
   StackIn = calc_stackin(StacksOut, CFG, PredMap, L),
   Succ = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), L),
   SaveOut = union_saveins(Succ, Saves),
   LiveOut = hipe_rtl_liveness:liveout(Liveness, L),
   Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, L)),
   StackOut = stack_instrs(Code, StackIn, SaveOut, LiveOut),
   OldStackOut = stack(StacksOut, L),
   StacksOut0 = set_stack(StacksOut, L, StackOut),
   Changed0 = if StackOut =/= OldStackOut ->
		    true;
		 true ->
		    Changed or false
	      end,
   stacks_once(Ls, StacksOut0, Saves, Liveness, CFG, PredMap, Changed0).



intersect_stacks2([], StacksOut) ->
   new_stack();
intersect_stacks2([P], StacksOut) ->
   stack(StacksOut, P);
intersect_stacks2([P|Ps], StacksOut) ->
   intersect_stacks(stack(StacksOut, P), intersect_stacks2(Ps, StacksOut)).



stack_instrs([], StackIn, SaveOut, LiveOut) ->
   StackIn;
stack_instrs([I|Is], StackIn, SaveOut, LiveOut) ->
   case hipe_rtl:type(I) of
      call ->
	 SaveOut17 = lazy_instrs(Is, SaveOut, LiveOut),
	 SaveOut0 = 
	    sset_remove(hipe_rtl:defines(I), sset_remove(hipe_rtl:uses(I), SaveOut17)),
	 Live0 = hipe_rtl_liveness:list(Is, LiveOut),
	 Live = ordsets:subtract(Live0, ordsets:from_list(hipe_rtl:call_dst(I))),
	 ToSave = ordsets:from_list(ordsets:subtract(Live, do_not_save())),
	 
	 NewSaves = order_saves(ToSave -- StackIn, SaveOut0),
	 case ordsets:subtract(ordsets:from_list(ToSave), 
			ordsets:from_list(NewSaves ++ StackIn)) of
	    [] ->
	       yes;
	    _ ->
	       throw({inconsistent_stacks, NewSaves, StackIn, ToSave})
	 end,
	 StackOut = trim_stack(strip_dist(SaveOut0), StackIn ++ NewSaves),
	 stack_instrs(Is, StackOut, SaveOut, LiveOut);
      _ ->
	 stack_instrs(Is, StackIn, SaveOut, LiveOut)
   end.


new_stack() ->
   [].

stack(Stacks, Lbl) ->
   {found, Stack} = hash:lookup(Lbl, Stacks),
   Stack.

set_stack(Stacks, Lbl, Stack) ->
   hash:update(Lbl, Stack, Stacks).

calc_stackout(StacksIn, CFG, L) ->
   Succ = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), L),
   intersect_stacks2(Succ, StacksIn).

calc_stackin(StacksOut, CFG, PredMap, L) ->
   case hipe_rtl_cfg:is_entry(L, CFG) of
      true ->
	 new_stack();
      false ->
	 Pred = hipe_rtl_cfg:pred(PredMap, L),
	 intersect_stacks2(Pred, StacksOut)
   end.


intersect_stacks(X, any) ->
   X;
intersect_stacks(any, Y) ->
   Y;
intersect_stacks(X, []) ->
   [];
intersect_stacks([], Y) ->
   [];
intersect_stacks([X|Xs], [X|Ys]) ->
   [X | intersect_stacks(Xs, Ys)];
intersect_stacks([X|Xs], [Y|Ys]) ->
   [].


order_saves(Saves, Ranking) ->
   Ranking0 = lists:reverse(lists:keysort(2, lists:sort(Ranking))),
   order_saves0(Ranking0, Saves).

order_saves0([], Saves) ->
   Saves;
order_saves0([{X,_}|Rs], Saves) ->
   case lists:member(X, Saves) of
      true ->
	 [X | order_saves0(Rs, lists:delete(X, Saves))];
      false ->
	 order_saves0(Rs, Saves)
   end.



trim_stack(Keep, []) ->
   [];
trim_stack(Keep, [X|Xs]) ->
   case lists:member(X, Keep) of
      true ->
	 [X | trim_stack(Keep, Xs)];
      false ->
	 []
   end.

trim_stack1(Remove, []) ->
   [];
trim_stack1(Remove, [X|Xs]) ->
   case lists:member(X, Remove) of
      true ->
	 [];
      false ->
	 [X | trim_stack1(Remove, Xs)]
   end.


strip_dist([]) ->
   [];
strip_dist([{X, _}|Xs]) ->
   [X|strip_dist(Xs)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Stacks backwards
%

init_stacksin([], CFG, PredMap, StacksOut, StacksIn) ->
   StacksIn;
init_stacksin([L|Ls], CFG, PredMap, StacksOut, StacksIn) ->
   StackIn = calc_stackin(StacksOut, CFG, PredMap, L),
   StacksIn0 = set_stack(StacksIn, L, StackIn),
   init_stacksin(Ls, CFG, PredMap, StacksOut, StacksIn0).


bw_stacks_loop(CFG, PredMap, StacksIn, StacksOut, Saves, Liveness, Labels) ->
   case bw_stacks_once(Labels, CFG, PredMap, StacksIn, StacksOut, Saves, 
		       Liveness, false) of
      {StacksIn0, StacksOut0, true} ->
	 bw_stacks_loop(CFG, PredMap, StacksIn0, StacksOut0, Saves, Liveness, 
			Labels);
      {StacksIn0, StacksOut0, false} ->
	 {StacksIn0, StacksOut0}
   end.


bw_stacks_once([], CFG, PredMap, StacksIn, StacksOut, Saves, Liveness, 
	       Changed) ->
   {StacksIn, StacksOut, Changed};
bw_stacks_once([L|Ls], CFG, PredMap, StacksIn, StacksOut, Saves, Liveness, 
	       Changed) ->
   StackOut = stack(StacksOut, L),
   CalcStackOut = calc_stackout(StacksIn, CFG, L),
   case StackOut =:= CalcStackOut of
      true ->
	 Changed0 = Changed,
	 StacksOut0 = StacksOut,
	 StackOut0 = StackOut;
      false ->
	 StackOut0 = intersect_stacks(StackOut, CalcStackOut),
	 StacksOut0 = set_stack(StacksOut, L, StackOut0),
	 %io:format("out (~w): ~w /\\ ~w -> ~w~n", 
	%	   [L, StackOut, CalcStackOut, StackOut0]),
	 Changed0 = true
   end,
   Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, L)),
   {StackIn, Remove} = bw_stack_instrs(Code, StackOut0, ordsets:new()),
   OldStackIn = stack(StacksIn, L),
   CalcStackIn = calc_stackin(StacksOut, CFG, PredMap, L),
   NewStackIn = trim_stack1(Remove, intersect_stacks(StackIn, OldStackIn)),
   case (NewStackIn =:= OldStackIn) and (NewStackIn =:= CalcStackIn) of
      true ->
	 Changed1 = Changed0,
	 StacksOut1 = StacksOut0,
	 StacksIn0 = StacksIn;
      false ->
	 Changed1 = true,
	 StackIn0 = intersect_stacks(NewStackIn, CalcStackIn),
	 
       %% io:format("~n(~w) StackIn: ~w~n", [L, StackIn]),
       %% io:format("(~w) OldStackIn: ~w~n", [L, OldStackIn]),
       %% io:format("(~w) CalcStackIn: ~w~n", [L, CalcStackIn]),
       %% io:format("(~w) Remove: ~w~n", [L, Remove]),
       %% io:format("(~w) NewStackIn: ~w~n", [L, NewStackIn]),
       %% io:format("(~w) StackIn0: ~w~n", [L, StackIn0]),
       %% io:format("in (~w): ~w -> ~w~n~n", 
       %% 		 [L, OldStackIn, StackIn0]),
	 StacksIn0 = set_stack(StacksIn, L, StackIn0),
	 Succ = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), L),
	 SaveOut = union_saveins(Succ, Saves),
	 LiveOut = hipe_rtl_liveness:liveout(Liveness, L),
	 StackOut42 = stack_instrs(Code, StackIn0, SaveOut, LiveOut),
	 StacksOut1 = set_stack(StacksOut0, L, StackOut42)
   end,
   bw_stacks_once(Ls, CFG, PredMap, StacksIn0, StacksOut1, Saves, Liveness, 
		  Changed1).



bw_stack_instrs([], StackOut, PleaseRemove) ->
   {StackOut, PleaseRemove};
bw_stack_instrs([I|Is], StackOut, PleaseRemove) ->
   {StackOut0, PleaseRemove0} = bw_stack_instrs(Is, StackOut, PleaseRemove),
   NewRemoves = ordsets:union(hipe_rtl:uses(I), hipe_rtl:defines(I)),
  case
    lists:member(hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
		 hipe_rtl:defines(I)) 
    of
    true -> %% This might be a little bit paranoid.
            %% Should realy check how the stack pointer is changed
            %%   and if it changed back before the previous save.
      io:format("Frame-merging canceled because of catch frame:\n ~w \n",[I]),
      {[],ordsets:union(NewRemoves, PleaseRemove0)};
    _ -> 
      case hipe_rtl:type(I) of
	call ->
	  {any, NewRemoves};
	enter ->
	  {any, ordsets:new()};
						%case hipe_rtl:esr_fun(I) of
						%   bif_exit_1 ->
						%      {any, PleaseRemove1};
						%   _ ->
						%      {StackOut0, PleaseRemove1}
						%end;
	return ->
	  {any, ordsets:new()};
	_ ->
	  {StackOut0, ordsets:union(NewRemoves, PleaseRemove0)}
      end
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%


add_save_restore([L|Ls], CFG, StacksOut, StacksIn, Liveness, MaxF, SumC, Restores) ->
  io:format("L: ~w\n",[L]),
   BB = hipe_rtl_cfg:bb(CFG, L),
   Code = hipe_bb:code(BB),

   StackIn = stack(StacksIn, L),
   StackOut = stack(StacksOut, L),
   LiveOut = hipe_rtl_liveness:liveout(Liveness, L),
   {NewCode, MaxFb, SumCb, Restore} = 
    block_add_save_restore(Code, StackIn, StackOut, LiveOut),

   NewCFG = hipe_rtl_cfg:bb_update(CFG, L, hipe_bb:code_update(BB, NewCode)),
   add_save_restore(Ls, NewCFG, StacksOut, StacksIn, Liveness, 
		    max(MaxF,MaxFb), SumC+SumCb,
		    [Restore|Restores]);
add_save_restore([], CFG, StacksOut, StacksIn, Liveness, MaxF, SumC, Restores) ->
  CFG1 = add_restores(Restores, CFG),
  %% Check if we really need a stack test
  Leaf = lists:member(leaf, hipe_rtl_cfg:info(CFG1)),
  
  CFG5 = 
    if (MaxF+SumC) =:= 0 -> CFG1;
       (MaxF+SumC) < ?HIPE_SPARC_LEAF_WORDS, Leaf =:= true -> CFG1;
       true ->
	Start = hipe_rtl_cfg:start_label(CFG1),
	 %% The variables we need to save across the call to stack_inc
	 Live = hipe_rtl_liveness:livein(Liveness, Start),
	 ToSave = ordsets:from_list(ordsets:subtract(Live, do_not_save())),
	 %% The maximum amount of stack this function needs
	 %% the ?HIPE_SPARC_LEAF_WORDS is somewhat ugly, but signifies the amount of stack
	 %% a leaf function can use without a stacktest
	 StackNeed = max(MaxF, length(ToSave)) + SumC +
	    hipe_sparc_registers:number_of_physical() + ?HIPE_SPARC_LEAF_WORDS,
	 %% Code to call function that extends the stack
	 OverflowLbl = hipe_rtl:mk_new_label(),
	 RetLbl = hipe_rtl:mk_new_label(),
	 OC = [hipe_rtl:mk_stackneed(length(ToSave)),
	       hipe_rtl:mk_save_frame(ToSave),
	       hipe_rtl:mk_call([], inc_stack_0, [], c, 
			   hipe_rtl:label_name(RetLbl), []), 
	       RetLbl,
	       hipe_rtl:mk_restore_frame(ToSave),
	       hipe_rtl:mk_pop_frame(length(ToSave)),
	       hipe_rtl:mk_goto(Start)],
	 OCb = hipe_bb:mk_bb(OC),
	 %% Code to test if we need more stack
	 NewStart = hipe_rtl:mk_new_label(),
	 SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	 SL = hipe_rtl:mk_reg(hipe_sparc_registers:stack_limit()),
	 Tmp = hipe_rtl:mk_new_reg(),
	 TC = [hipe_rtl:mk_alu(Tmp, SP, add, hipe_rtl:mk_imm(StackNeed*4)),
	       hipe_rtl:mk_branch(Tmp, lt, SL, Start, hipe_rtl:label_name(OverflowLbl), 
			     0.99)],
	 TCb = hipe_bb:mk_bb(TC),
	 %% Update the cfg
	 CFG2 = hipe_rtl_cfg:bb_add(CFG1, hipe_rtl:label_name(NewStart), TCb),
	 CFG3 = hipe_rtl_cfg:start_label_update(CFG2, hipe_rtl:label_name(NewStart)),
	CFG4 = hipe_rtl_cfg:bb_add(CFG3, hipe_rtl:label_name(OverflowLbl), OCb)
    end,
  {VLow, _} = hipe_rtl_cfg:var_range(CFG5),
  CFG6 = hipe_rtl_cfg:var_range_update(CFG5, {VLow, hipe_gensym:get_var(rtl)}),
  {LLow, _} = hipe_rtl_cfg:label_range(CFG6),
  hipe_rtl_cfg:label_range_update(CFG6, {LLow, hipe_gensym:get_label(rtl)}).



block_add_save_restore([], StackIn, StackOut, LiveOut) ->
  {[], 0, 0, []};
block_add_save_restore([I], StackIn, StackOut, LiveOut) ->
  case hipe_rtl:type(I) of
    call ->
      Live0 = LiveOut,
      Live1 = ordsets:subtract(Live0, ordsets:from_list(hipe_rtl:call_dst(I))),
      Live = ordsets:from_list(ordsets:subtract(Live1, do_not_save())),
      PreOrderedSaves = (StackOut -- StackIn),
      LocalSaves = Live -- (StackIn++PreOrderedSaves),
      CallStack = StackIn ++ PreOrderedSaves ++ LocalSaves,
      Push = PreOrderedSaves++LocalSaves,
      Pop = CallStack -- StackOut,	 
      check_saves(Push),
      ToSave = Push,
      Need = length(Live),
      Cont = hipe_rtl:call_continuation(I),
      NewCont = hipe_rtl:label_name(hipe_rtl:mk_new_label()),
      Fail = hipe_rtl:call_fail(I),
      case Fail of
	[] -> %% Not in a catch
	  NewI = hipe_rtl:call_continuation_update(
		   I,
		   NewCont),
	  {[hipe_rtl:mk_stackneed(Need),
	    hipe_rtl:mk_save_frame(ToSave),
	    NewI], 
	   Need, 0, {Pop,{Cont, NewCont}}};
	_ -> %% In a catch
	  NFail = hipe_rtl:label_name(hipe_rtl:mk_new_label()),
	  NewI = hipe_rtl:call_continuation_update(
		   hipe_rtl:call_fail_update(I, NFail),
		   NewCont),
	  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	  IncStack = hipe_rtl:mk_alu(SP, SP, add, hipe_rtl:mk_imm((Need+1)*4)),
	  FrameCode = save_vars(Pop, SP, 0, 
				hipe_tagscheme:write_catch_frame(SP, Need*4, NFail)++
				[IncStack,NewI]),
	  {FrameCode,
	   Need+1, 0, {ToSave,{Cont, NewCont},
		       {Fail,NFail,hipe_rtl:call_dst(NewI), Need}}}
      end;
    enter ->
      case StackIn of
	[] ->
	  {[I], 0, 0, []};
	_ ->
	  {[hipe_rtl:mk_restore_frame(StackIn),
	    hipe_rtl:mk_pop_frame(length(StackIn)),
	    I], 0, 0, []}
      end;
    return ->
      case StackIn of
	[] ->{[I], 0, 0, []};
	_ ->
	  {[hipe_rtl:mk_restore_frame(StackIn),
	    hipe_rtl:mk_pop_frame(length(StackIn)),
	    I], 0, 0, []}
      end;
    _ ->
      {[I], 0, 0, []}
  end;
block_add_save_restore([I|Is], StackIn, StackOut, LiveOut) ->
  case hipe_rtl:type(I) of
    call ->
      exit({I,Is});
    stackneed ->
      {NewIs, MaxF, SumC, Restores} = 
	block_add_save_restore(Is, StackIn, StackOut, LiveOut),
      {[I|NewIs], MaxF, SumC+hipe_rtl:stackneed_words(I),
       Restores};
    _ ->
      {NewIs, MaxF, SumC, Restores} = 
	block_add_save_restore(Is, StackIn, StackOut, LiveOut),
      {[I|NewIs], MaxF, SumC, Restores}
  end.



save_vars([], _, _, Code) -> Code;
save_vars([Var | Vars], SP, I, Code) ->
   save_vars(Vars, SP, I+4, save_var(Var, SP, hipe_rtl:mk_imm(I), Code)).
save_var(Var, SP, Off, Code) ->
   case is_CP(Var) of
      true -> hipe_tagscheme:save_CP(Var, SP, Off, Code);
      false -> [hipe_rtl:mk_store(SP, Off, Var) | Code]
   end.
%%
%% Test if a save/restore variable is CP. We make sure that CP
%% always is an rtl var, not reg, to simplify the test.
%%
%% The JAM compiler sometimes creates unused variables (with "alloc").
%% IX isn't able to eliminate these from saved frames, but it is able
%% to propagate their initial values (nil) to all use sites.
%% Therefore, save_frame var lists sometimes contain rtl immediates.
%% At the moment, this has only been observed in save_frames constructed
%% for JAM pushCatch instructions.
%%
is_CP(Var) ->
   case hipe_rtl:is_var(Var) of
      true -> hipe_rtl:var_name(Var) == hipe_sparc_registers:return_address();
      false -> false
  end.


add_restores([{ToRestore,{Cont,NewC}}|Restores], CFG) ->
  %% Not in a catch.
  Code =
    [hipe_rtl:mk_restore_frame(ToRestore),
     hipe_rtl:mk_pop_frame(length(ToRestore)),
     hipe_rtl:mk_goto(Cont)],
  NewCFG = hipe_rtl_cfg:bb_add(CFG, NewC, hipe_bb:mk_bb(Code)),
  add_restores(Restores, NewCFG);
add_restores([{ToRestore,{Cont,NewC},{Fail,NewF,Dst, Need}}|Restores], CFG) ->
  %% In a catch.
  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  R = hipe_rtl:mk_new_reg(),
  CCode =
    [hipe_rtl:mk_alu(SP, SP, sub, hipe_rtl:mk_imm(4)),
     hipe_rtl:mk_restore_frame(ToRestore),
     hipe_rtl:mk_pop_frame(Need),
     hipe_rtl:mk_goto(Cont)],
  CCFG = hipe_rtl_cfg:bb_add(CFG, NewC, hipe_bb:mk_bb(CCode)),
  FCode =
%%    move_args_to_vars(Dst) ++
    [hipe_rtl:mk_restore_frame(ToRestore),
     hipe_rtl:mk_pop_frame(Need),
     hipe_rtl:mk_goto(Fail)],
  FCFG = hipe_rtl_cfg:bb_add(CCFG, NewF, hipe_bb:mk_bb(FCode)),
  add_restores(Restores, FCFG);
add_restores([[]|Restores], CFG) ->
  add_restores(Restores,CFG);
add_restores([],CFG) -> CFG.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This procedure keeps track of the largest possible frame and adds
% a stacktest at the start of the function.
%

add_stacktest([Lbl|Lbls], CFG, Liveness, MaxF, SumC) ->
   Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Lbl)),
   {MaxFb, SumCb} = block_add_stacktest(Code),
   add_stacktest(Lbls, CFG, Liveness, max(MaxF,MaxFb), SumC+SumCb);
add_stacktest([], CFG, Liveness, MaxF, SumC) ->
   %% Check if we really need a stack test
   Leaf = lists:member(leaf, hipe_rtl_cfg:info(CFG)),
   if (MaxF+SumC) =:= 0 ->
	 CFG;
      (MaxF+SumC) < ?HIPE_SPARC_LEAF_WORDS, Leaf =:= true ->
	 CFG;
      true ->
	 {LLow, LHigh} = hipe_rtl_cfg:label_range(CFG),
	 hipe_gensym:set_label(rtl,LHigh),
	 {VLow, VHigh} = hipe_rtl_cfg:var_range(CFG),
	 hipe_gensym:set_var(rtl,VHigh),
	 Start = hipe_rtl_cfg:start_label(CFG),
	 %% The variables we need to save across the call to stack_inc
	 Live = hipe_rtl_liveness:liveout(Liveness, Start),
	 ToSave = ordsets:from_list(ordsets:subtract(Live, do_not_save())),
	 %% The maximum amount of stack this function needs
	 %% the HIPE_SPARC_LEAF_WORDS is somewhat ugly, but signifies the amount of stack
	 %% a leaf function can use without a stacktest
	 StackNeed = max(MaxF, length(ToSave)) + SumC +
	    hipe_sparc_registers:number_of_physical() + ?HIPE_SPARC_LEAF_WORDS,
	 %% Code to call function that extends the stack
	 OverflowLbl = hipe_rtl:mk_new_label(),
	 OC = [hipe_rtl:mk_stackneed(length(ToSave)),
	       hipe_rtl:mk_save_frame(ToSave),
	       hipe_rtl:mk_call([], inc_stack_0, [], c), 
	       hipe_rtl:mk_restore_frame(ToSave),
	       hipe_rtl:mk_pop_frame(length(ToSave)),
	       hipe_rtl:mk_goto(Start)],
	 OCb = hipe_bb:mk_bb(OC),
	 %% Code to test if we need more stack
	 NewStart = hipe_rtl:mk_new_label(),
	 SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	 SL = hipe_rtl:mk_reg(hipe_sparc_registers:stack_limit()),
	 Tmp = hipe_rtl:mk_new_reg(),
	 TC = [hipe_rtl:mk_alu(Tmp, SP, add, hipe_rtl:mk_imm(StackNeed*4)),
	       hipe_rtl:mk_branch(Tmp, lt, SL, Start, hipe_rtl:label_name(OverflowLbl), 
			     0.99)],
	 TCb = hipe_bb:mk_bb(TC),
	 %% Update the cfg
	 CFG0 = hipe_rtl_cfg:bb_add(CFG, hipe_rtl:label_name(NewStart), TCb),
	 CFG1 = hipe_rtl_cfg:start_label_update(CFG0, hipe_rtl:label_name(NewStart)),
	 CFG2 = hipe_rtl_cfg:bb_add(CFG1, hipe_rtl:label_name(OverflowLbl), OCb),
	 CFG3 = hipe_rtl_cfg:var_range_update(CFG2, {VLow, hipe_gensym:get_var(rtl)}),
	 hipe_rtl_cfg:label_range_update(CFG3, {LLow, hipe_gensym:get_label(rtl)})
   end.


%%
%% Returns {MaxFrameSize, SumOfCatchFrameSizes}
%%

block_add_stacktest([]) ->
   {0, 0};
block_add_stacktest([I|Is]) ->
   case hipe_rtl:type(I) of
      stackneed ->
	 {MaxF, SumC} = block_add_stacktest(Is),
	 case lists:member('catch', hipe_rtl:info(I)) of
	    true ->
	       {MaxF, SumC+hipe_rtl:stackneed_words(I)};
	    false ->
	       {max(hipe_rtl:stackneed_words(I), MaxF), SumC}
	 end;
      _ ->
	 block_add_stacktest(Is)
   end.


check_saves([]) ->
   ok;
check_saves([S|Ss]) ->
   case hipe_rtl:is_var(S) of
      true ->
	 check_saves(Ss);
      false ->
       case hipe_rtl:is_reg(S) of
	 true ->
	   case hipe_rtl:reg_name(S) ==
	     hipe_sparc_registers:return_address() of
	     true ->
	        check_saves(Ss);
	     false ->
	       exit({rtl_frame, {"illegal save", S}})
	   end;
	 false ->
	   exit({rtl_frame, {"illegal save", S}})
       end
   end.

%%
%% The set of registers that is not to be saved in a frame
%%

do_not_save() ->
   ordsets:from_list(lists:map(fun (R) -> hipe_rtl:mk_reg(R) end, hipe_sparc_registers:global())).

%% Just a max function.

max(X, Y) when X < Y -> Y;
max(X, Y) -> X.
