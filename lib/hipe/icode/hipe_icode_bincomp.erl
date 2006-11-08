%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : hipe_icode_bincomp.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created : 12 Sep 2005 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------

-module(hipe_icode_bincomp).

-export([cfg/1]).

cfg(Cfg) ->
  {_,FunName,_} =  hipe_icode_cfg:function(Cfg),
  case bincomp_fun(atom_to_list(FunName)) of
    from_binary ->
      %%hipe_icode_cfg:pp(Cfg),
      {MaxIterCode,MaxIterVar} = find_matches(Cfg),
      {Init, Cfg1}  = remove_init(Cfg),
      {Cfg2, SelfRecLbl, RetVal} = 
	add_new_start(Cfg1, Init, MaxIterVar, MaxIterCode),
      %%hipe_icode_cfg:pp(Cfg2),
      Cfg3 = rewrite_cfg(Cfg2, SelfRecLbl, hipe_icode_cfg:function(Cfg), {get_offset(Init),RetVal}),
       %%hipe_icode_cfg:pp(Cfg3),
      cfg2(Cfg3);
    from_list ->
      {MaxIterCode,MaxIterVar} = get_max_iter(Cfg),
      {Init, Cfg1}  = remove_init(hipe_icode_cfg:start_label(Cfg),Cfg),
      {Cfg2, SelfRecLbl, RetVal} = 
	add_new_start2(Cfg1, Init, MaxIterVar, MaxIterCode),
      rewrite_cfg(Cfg2, SelfRecLbl, hipe_icode_cfg:function(Cfg), RetVal);
    false ->
      case contains_tail_call(Cfg) of
	true ->
	  case find_bs_start_match_labels2(Cfg) of
	    failed ->
	      Cfg;
	    {Lbl, SuccLbl} ->
	      {Cfg1, Succs, Flag} = remove_unnecessary([SuccLbl],Cfg,[],[SuccLbl],false),
	      Cfg2 = 
		if Flag -> rewrite_bs_gets([SuccLbl], Cfg1, []);
		   true -> Cfg1
		end, 
	      Cfg3 = unique_state_name(Lbl, Cfg2),
	       %%hipe_icode_cfg:pp(Cfg3),
	      rewrite_gotos(Succs, Cfg3, Lbl, SuccLbl)
	  end;
	false ->
	  Cfg
      end
  end.

get_offset(Init) ->
  [_,_,Offset] = hipe_icode:call_dstlist(Init),
  Offset.

get_max_iter(Cfg) ->
  [List]=hipe_icode_cfg:params(Cfg),
  IterVar = hipe_icode:mk_new_var(),
  {[hipe_icode:mk_call([IterVar], erlang,  length, [List], remote)],
   IterVar}.

contains_tail_call(Cfg) ->
  Start = hipe_icode_cfg:start_label(Cfg),
  Lbls = hipe_icode_cfg:labels(Cfg),
  contains_tail_call(Lbls, Cfg, Start).

contains_tail_call([Lbl|Lbls], Cfg, Start) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  Last = hipe_bb:last(BB),
  case hipe_icode:is_goto(Last) andalso 
    hipe_icode:goto_label(Last) == Start of
    true ->
      true;
    false ->
      contains_tail_call(Lbls, Cfg, Start)
  end;
contains_tail_call([], _Cfg, _Start) ->
  false.

cfg2(Cfg) ->
  {Lbl, SuccLbl} = find_bs_start_match_labels(Cfg),
  {Cfg1, Succs, Flag} = remove_unnecessary([SuccLbl],Cfg,[],[SuccLbl],false),
  Cfg2 = 
    if Flag -> rewrite_bs_gets([SuccLbl], Cfg1, []);
       true -> Cfg1
    end,
  Cfg3 = unique_state_name(Lbl, Cfg2),
  %%hipe_icode_cfg:pp(Cfg3),
  rewrite_gotos(Succs, Cfg3, Lbl, SuccLbl).

unique_state_name(Lbl, Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  StartMatch = hipe_bb:last(BB),
  [State] = hipe_icode:call_dstlist(StartMatch),
  NewState = hipe_icode:mk_new_var(),
  Next = hipe_icode:call_continuation(StartMatch),
  NewStartMatch = hipe_icode:call_dstlist_update(StartMatch,[NewState]),
  NewCode = hipe_bb:butlast(BB) ++ [NewStartMatch],
  NewBB = hipe_bb:mk_bb(NewCode),
  Cfg1 = hipe_icode_cfg:bb_add(Cfg, Lbl, NewBB),
  unique_state_name([Next],Cfg1,State,NewState,[]).

unique_state_name([Lbl|Rest],Cfg,OldState,NewState,Visited) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  NewCode = usn_straightline(hipe_bb:code(BB),OldState,NewState),
  NewBB = hipe_bb:mk_bb(NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Lbl, NewBB),
  Last = hipe_bb:last(NewBB),
  case ending(Last) of
    true ->
      Nexts=[hipe_icode:call_fail_label(Last)],
      NewNexts = [X || X <- Nexts, not(lists:member(X,Visited))],
      unique_state_name(Rest++NewNexts,NewCfg,OldState,NewState,Visited++NewNexts);
    false -> 
      Nexts=hipe_icode_cfg:succ(Cfg,Lbl),
      NewNexts = [X || X <- Nexts, not(lists:member(X,Visited))],
      unique_state_name(Rest++NewNexts,NewCfg,OldState,NewState,Visited++NewNexts) 
  end;
unique_state_name([],Cfg,_,_,_) ->
  Cfg.

usn_straightline(Code,OS,NS) ->
  [usn_instr(Instr,OS,NS)||Instr<-Code].

usn_instr(I,OS,NS) ->
  case get_state_from_bs(I) of
    OS -> change_bs_state(I,NS);
    _ -> I
  end.

ending(I) ->
  case hipe_icode:is_call(I) of
    true ->
      case hipe_icode:call_fun(I) of
	{hipe_bs_primop2,{bs_get_binary_all_2,_Flags}} ->
	  true;
	{hipe_bs_primop2,{bs_test_tail_2,_NumBits}} ->
	  true;
	{hipe_bs_primop2,{bs_skip_bits_all_2,_Flags}} ->
	  true;
	_ ->
	  false
      end;
    _ ->
      false
  end.

get_state_from_bs(I) ->
  case hipe_icode:is_call(I) of
    true ->
      case hipe_icode:call_fun(I) of
	{hipe_bs_primop2,Name} ->
	  case matching_bs(Name) of
	    true ->
	      [MS|_] = hipe_icode:args(I),
	      MS;
	    _ ->
	      no_state
	  end;
	_ ->
	  no_state
      end;
    _ ->
      no_state
  end.

change_bs_state(I,NS) ->
  [_OldMS|RestArgs] = hipe_icode:call_args(I),
  [_OldMS|RestDst] =lists:reverse(hipe_icode:call_dstlist(I)),
  I1 = hipe_icode:call_args_update(I,[NS|RestArgs]),
  hipe_icode:call_dstlist_update(I1,lists:reverse([NS|RestDst])).

matching_bs({bs_get_integer_2,_Size,_Flags}) -> true;
matching_bs({bs_get_float_2,_Size,_Flags}) -> true; 
matching_bs({bs_get_binary_2,_Size,_Flags}) -> true; 
matching_bs({bs_get_binary_all_2,_Flags}) -> true; 
matching_bs({bs_test_tail_2,_NumBits}) -> true; 
matching_bs({bs_restore_2, _Index}) -> true;
matching_bs({bs_save_2, _Index}) -> true; 
matching_bs({bs_skip_bits_all_2, _Flags}) -> true;
matching_bs({bs_skip_bits_2, _Unit}) -> true;
matching_bs(_) -> false.
		  
find_bs_start_match_labels(Cfg) ->
  Start = hipe_icode_cfg:start_label(Cfg),
  [StartMatchLbl]=hipe_icode_cfg:succ(Cfg,Start),
  BB = hipe_icode_cfg:bb(Cfg, StartMatchLbl),
  StartMatch = hipe_bb:last(BB),
  true = is_start_match(StartMatch),
  Next = hipe_icode:call_continuation(StartMatch),
  {StartMatchLbl, Next}.

find_bs_start_match_labels2(Cfg) ->
  Start = hipe_icode_cfg:start_label(Cfg),
  BB = hipe_icode_cfg:bb(Cfg, Start),
  StartMatch = hipe_bb:last(BB),
  case is_start_match(StartMatch) of
    true ->
      Next = hipe_icode:call_continuation(StartMatch),
      {Start, Next};
    false ->
      failed
  end.

rewrite_bs_gets([Lbl|Rest], Cfg, Visited) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  Last = hipe_bb:last(BB),
  NewCfg = 
    case rewrite_bs_get(Last) of
      Last -> Cfg;
      NewLast -> 
	NewCode = hipe_bb:butlast(BB) ++ [NewLast],
	NewBB = hipe_bb:mk_bb(NewCode),
	hipe_icode_cfg:bb_add(Cfg, Lbl, NewBB)
    end,
  case is_bs_test_tail(Last) of
    true ->
      rewrite_bs_gets(Rest,NewCfg,Visited);
    false ->
      Nexts=hipe_icode_cfg:succ(Cfg,Lbl),
      NewNexts = [X || X <- Nexts, not(lists:member(X,Visited))],
      rewrite_bs_gets(Rest++NewNexts,NewCfg,Visited++NewNexts)
  end;
rewrite_bs_gets([], Cfg, _Visited) ->
  Cfg.
  
rewrite_bs_get(Instr) ->
  case hipe_icode:is_call(Instr) of
    true ->
      FunName = 
	case hipe_icode:call_fun(Instr) of
	  {hipe_bs_primop2,Name} ->
	    NewName = 
	      case Name of
		{bs_get_integer_2,Size,Flags} -> {bs_get_integer_2,Size,Flags band 6};
		{bs_get_float_2,Size,Flags} -> {bs_get_float_2,Size,Flags band 6};
		{bs_get_binary_2,Size,Flags} -> {bs_get_binary_2,Size,Flags band 6};
		Old -> Old
	      end,
	    {hipe_bs_primop2,NewName};
	  OldFun ->
	    OldFun
	end,
      hipe_icode:call_fun_update(Instr,FunName);
    false ->
      Instr
  end.


remove_unnecessary([SuccLbl|Rest],Cfg,Succs,Visited,Flag) ->
  BB = hipe_icode_cfg:bb(Cfg, SuccLbl),
  Last = hipe_bb:last(BB),
  case is_bs_get_binary_all(Last) of
    true ->
      Succ = hipe_icode:call_continuation(Last),
      Fail = hipe_icode:call_fail_label(Last),
      NewRest = 
	case lists:member(Fail,Visited) of
	  true -> Rest;
	  false -> [Fail|Rest]
	end,
      case res_used_in_start_match([Succ],Cfg,
				   hd(hipe_icode:call_dstlist(Last)),[]) of
	true ->
	  io:format("hej\n",[]),
	  [Bin|_] = hipe_icode:call_dstlist(Last), 
	  NewFlag = is_not_aligned(hipe_icode:call_fun(Last)), 
	  NewLast = hipe_icode:mk_goto(Succ),
	  NewCode = hipe_bb:butlast(BB) ++ [NewLast],
	  NewBB = hipe_bb:mk_bb(NewCode),
	  NewCfg = hipe_icode_cfg:bb_add(Cfg, SuccLbl, NewBB),
	  {NewCfg2, NewSuccs} = {NewCfg, [{Succ,Bin}|Succs]},
	  %%	remove_bs_test_tail(Succ, NewCfg, Bin, Succs),
	  remove_unnecessary(NewRest, NewCfg2, NewSuccs, Visited, NewFlag or Flag);
	false ->
	  remove_unnecessary(NewRest, Cfg, Succs, Visited, Flag)
      end;
    false ->
      case is_bs_test_tail(Last) of
	true ->
	  Fail = hipe_icode:call_fail_label(Last),
	  NewRest = 
	    case lists:member(Fail,Visited) of
	      true -> Rest;
	      false -> [Fail|Rest]
	    end,
	  remove_unnecessary(NewRest, Cfg, Succs, Visited, Flag);
	false ->
	  Nexts=hipe_icode_cfg:succ(Cfg,SuccLbl),
	  NewNexts = [X || X <- Nexts, not(lists:member(X,Visited))],
	  remove_unnecessary(Rest++NewNexts,Cfg,Succs,
			       Visited++NewNexts, Flag)
      end
  end;
remove_unnecessary([],Cfg,Succs,_Visited,Flag) ->
  {Cfg,Succs,Flag}.

is_not_aligned(BsGetAll) ->
  case BsGetAll of
    {hipe_bs_primop2,{bs_get_binary_all_2,Flags}} ->
      (Flags band 1) == 0
  end.
	
res_used_in_start_match([Lbl|Rest],Cfg,Var0,Visited) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  ButLast = hipe_bb:butlast(BB),
  case lists:foldl(fun moving_var/2, Var0, ButLast) of
    none -> false;
    Var ->
      Last = hipe_bb:last(BB),
      case is_start_match(Last) of
	true ->
	  case hipe_icode:call_args(Last) of
	    [Var] ->
	      true;
	    _ ->
	      Nexts=hipe_icode_cfg:succ(Cfg,Lbl),
	      NewNexts = [X || X <- Nexts, not(lists:member(X,Visited))],
	      res_used_in_start_match(Rest++NewNexts,Cfg,Var, Visited++NewNexts)
	  end;
	false ->
	  Nexts=hipe_icode_cfg:succ(Cfg,Lbl),
	  NewNexts = [X || X <- Nexts, not(lists:member(X,Visited))],
	  res_used_in_start_match(Rest++NewNexts,Cfg,Var, Visited++NewNexts)
      end
  end;
res_used_in_start_match([],_Cfg,_Var,_Visited) ->
  false.

moving_var(I,Var) ->
  case hipe_icode:is_move(I) andalso (hipe_icode:move_src(I) == Var) of
    true ->
      hipe_icode:move_dst(I);
    false ->
      case lists:member(Var,hipe_icode:uses(I))  of
	true -> none;
	false -> Var
      end
  end.

%% remove_bs_test_tail(SuccLbl, Cfg, Bin, Succs) ->
%%   BB = hipe_icode_cfg:bb(Cfg, SuccLbl),
%%   Last = hipe_bb:last(BB),
%%   true = is_bs_test_tail(Last),   
%%   Succ = hipe_icode:call_continuation(Last),
%%   NewLast = hipe_icode:mk_goto(Succ),
%%   NewCode = hipe_bb:butlast(BB) ++ [NewLast],
%%   NewBB = hipe_bb:mk_bb(NewCode),
%%   NewCfg = hipe_icode_cfg:bb_add(Cfg, SuccLbl, NewBB), 
%%   {NewCfg, [{Succ, Bin}|Succs]}.

rewrite_gotos([{Next,Bin}|Rest],Cfg, Lbl, SuccLbl) ->
  {_NewVisited,Cfg1} = rewrite_goto(Cfg,Next,Lbl,SuccLbl,[Bin],[]),
  rewrite_gotos(Rest,Cfg1,Lbl, SuccLbl);
rewrite_gotos([],Cfg,_L1,_L2) ->
  Cfg.

rewrite_goto(Cfg, NextLbl, Lbl, SuccLbl, Bins, Visited) ->
  BB = hipe_icode_cfg:bb(Cfg, NextLbl),
  OldCode = hipe_bb:butlast(BB),
  {NewCode,Bins2} = rewrite_oldcode(OldCode, Bins,[]),
  Last = hipe_bb:last(BB),
  NewBins = Bins2 -- hipe_icode:defines(Last),
  case is_correct_goto(Last, Lbl) of
    true ->
      NewLast = hipe_icode:mk_goto(SuccLbl),
      NewBB = hipe_bb:mk_bb(NewCode++[NewLast]),
      {Visited,hipe_icode_cfg:bb_add(Cfg, NextLbl, NewBB)};
    false ->
      NewBB = hipe_bb:mk_bb(NewCode++[Last]),
      NewCfg = hipe_icode_cfg:bb_add(Cfg, NextLbl, NewBB),
      Nexts = hipe_icode_cfg:succ(Cfg,NextLbl),
      NewNexts = [X || X <- Nexts, not(lists:member(X,Visited))],
      rewrite_gotos(NewNexts, NewCfg, Lbl, SuccLbl, 
		    NewBins, NewNexts++Visited)
  end.

rewrite_gotos([NextLbl|Rest], Cfg,  Lbl, SuccLbl, Bins, Visited) ->
  {NewVisited,NewCfg} = rewrite_goto(Cfg, NextLbl, Lbl, SuccLbl, 
				     Bins, Visited),
  rewrite_gotos(Rest, NewCfg, Lbl, SuccLbl, Bins, NewVisited);
rewrite_gotos([], Cfg, _Lbl, _SuccLbl, _Bins, Visited) ->
  {Visited,Cfg}.

rewrite_oldcode([I|Is], Bins, Acc) ->
  NewBins = 
    [X|| X <- Bins, not(lists:member(X,hipe_icode:defines(I)))],
  case [X|| X <- hipe_icode:uses(I),lists:member(X,Bins)] of
    [] ->
      rewrite_oldcode(Is, NewBins, [I|Acc]);
    _ ->
      rewrite_oldcode(Is, hipe_icode:defines(I)++NewBins, Acc)
  end;
rewrite_oldcode([], Bins, Acc) ->
  {lists:reverse(Acc), Bins}.

is_correct_goto(Last, Lbl) ->
  hipe_icode:is_goto(Last) andalso hipe_icode:goto_label(Last) == Lbl.

is_bs_test_tail(Last) ->
  hipe_icode:is_call(Last) andalso correct_testtail(hipe_icode:call_fun(Last)).

correct_testtail({hipe_bs_primop2, {bs_test_tail_2,_}}) ->
  true;
correct_testtail(_) ->
  false.

is_bs_final2(Instr) ->
   hipe_icode:is_call(Instr) andalso  correct_bs_final2(hipe_icode:call_fun(Instr)).

correct_bs_final2({hipe_bs_primop, bs_final2}) ->
  true;
correct_bs_final2(_) ->
  false.

is_bs_get_binary_all(Last) ->
  hipe_icode:is_call(Last) andalso correct_binall(hipe_icode:call_fun(Last)).

correct_binall({hipe_bs_primop2, {bs_get_binary_all_2,_}}) ->
  true;
correct_binall(_) ->
  false.

rewrite_cfg(Cfg, SelfRecLbl, FunName, RetVal) ->
  Labels = hipe_icode_cfg:labels(Cfg),
  rewrite_cfg(Labels, Cfg, SelfRecLbl, FunName, RetVal).

rewrite_cfg([Label|Rest], Cfg, SelfRecLbl, FunName, {Offset,RetVal}) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  Args = hipe_icode_cfg:params(Cfg),
  Start = hipe_icode_cfg:start_label(Cfg),
  Code1 = remove_bs_final(Code),
  NewCode =
    case rewrite_tailcall(Code1, Args, Start, SelfRecLbl, FunName) of
      Code1 ->
	Last = hipe_bb:last(BB),
	case hipe_icode:is_return(Last) of
	  true -> 
	    hipe_bb:butlast(BB) ++
	      [hipe_icode:mk_primop([RetVal],{hipe_bs_primop, bs_final2},
				    [RetVal,Offset]), 
	       hipe_icode:mk_return([RetVal])];
	  false ->
	    Code
	end;
      New ->
	New
    end,
  NewBB = hipe_bb:mk_bb(NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB), 
  rewrite_cfg(Rest, NewCfg, SelfRecLbl, FunName, {Offset,RetVal});
rewrite_cfg([], Cfg, _, _, _) ->
  Cfg.

remove_bs_final(Code) ->
  [Instr || Instr <- Code, not(is_bs_final2(Instr))].

rewrite_tailcall([Instr|Rest], Args, Start, SelfRecLbl, FunName) ->
  case is_selfrec_call(Instr, FunName, Start) of
    {true,call} ->
      ArgCopies = copy_args(Args, hipe_icode:call_args(Instr)),
      Goto = hipe_icode:mk_goto(SelfRecLbl),
      ArgCopies ++ [Goto];
     {true,goto} ->
       Goto = hipe_icode:mk_goto(SelfRecLbl),
      [Goto];
    false ->
      [Instr|rewrite_tailcall(Rest, Args, Start, SelfRecLbl, FunName)]
  end;
rewrite_tailcall([], _Args, _Start, _SelfRecLbl, _FunName) ->
  [].

is_selfrec_call(Instr, FunName, Start) ->
  case hipe_icode:is_call(Instr) of
    true ->
      case hipe_icode:call_fun(Instr) of
	FunName ->
	  {true,call};
	_ ->
	  false
      end;
    false ->
      case hipe_icode:is_goto(Instr) of
	true ->
	  case hipe_icode:goto_label(Instr) of
	    Start -> {true,goto};
	    _ -> false
	  end;
	false -> false
      end
  end.

copy_args([Arg|Args], [IArg|IArgs]) ->
  [hipe_icode:mk_move(Arg, IArg)|copy_args(Args, IArgs)];
copy_args([], []) ->
  [].

add_new_start2(Cfg, Init, MaxIterVar, MaxIterCode) ->
  RetVal = hipe_icode:mk_new_var(),
  SelfRecLbl = hipe_icode:label_name(hipe_icode:mk_new_label()),
  Start = hipe_icode_cfg:start_label(Cfg),
  BB = hipe_icode_cfg:bb(Cfg, Start),
  Code = hipe_bb:code(BB),
  NewInitCode = update_init(Init, RetVal, MaxIterVar),
  {Code1, Code2} = divide_on_redtest(Code),
  NewBB = hipe_bb:mk_bb(Code2),
  io:format("Labels: ~w ~w ~n", [SelfRecLbl, Start]),
  Cfg1 = hipe_icode_cfg:bb_add(Cfg, SelfRecLbl, NewBB),
  NewStartCode = 
    Code1 ++ MaxIterCode ++ NewInitCode ++ 
    [hipe_icode:mk_goto(SelfRecLbl)],
  NewStartBB = hipe_bb:mk_bb(NewStartCode),
  Cfg2 = hipe_icode_cfg:bb_add(Cfg1, Start, NewStartBB),
  {Cfg2, SelfRecLbl, RetVal}.

divide_on_redtest([Instr|Rest]) ->
  {[Instr], Rest}.



add_new_start(Cfg, Init, MaxIterVar, MaxIterCode) ->
  RetVal = hipe_icode:mk_new_var(),
  SelfRecLbl = hipe_icode:label_name(hipe_icode:mk_new_label()),
  Start = hipe_icode_cfg:start_label(Cfg),
  BB = hipe_icode_cfg:bb(Cfg, Start),
  Code = hipe_bb:code(BB),
  NewInitCode = update_init(Init, RetVal, MaxIterVar),
  {Code1, Code2} = divide_on_redtest(Code),
  NewBB = hipe_bb:mk_bb(Code2),
  io:format("Labels: ~w ~w ~n", [SelfRecLbl, Start]),
  Cfg1 = hipe_icode_cfg:bb_add(Cfg, SelfRecLbl, NewBB),
  NewStartCode = 
    Code1 ++ MaxIterCode ++ NewInitCode ++ 
    [hipe_icode:mk_goto(SelfRecLbl)],
  NewStartBB = hipe_bb:mk_bb(NewStartCode),
  Cfg2 = hipe_icode_cfg:bb_add(Cfg1, Start, NewStartBB),
  {Cfg2, SelfRecLbl, RetVal}.
  
    
update_init(Init, RetVal, MaxIterVar) ->
  Dst = hipe_icode:mk_new_var(),
  [_OldRet|Rest] = hipe_icode:call_dstlist(Init),
  Init0 = hipe_icode:call_dstlist_update(Init, [RetVal|Rest]),
  {hipe_bs_primop, {bs_init2, Size, Flags}} = 
    hipe_icode:call_fun(Init0), 
  Init1 = hipe_icode:call_fun_update(Init0,{hipe_bs_primop, {bs_init2, Flags}}),
  Init2 = hipe_icode:call_args_update(Init1,[Dst]),
   [hipe_icode:mk_primop([Dst],{hipe_bs_primop,{bs_add,Size}},
			 [hipe_icode:mk_const(0), MaxIterVar]),
    Init2].

%divide_on_bs_create_space(Code) ->
%  divide_on_bs_create_space(Code, []).

%divide_on_bs_create_space([Instr|Rest], Acc) ->
%  case is_create_space(Instr) of
%    true ->
%      {lists:reverse(Acc), [Instr|Rest]};
%    false ->
%      divide_on_bs_create_space(Rest, [Instr|Acc])
%  end.

%is_create_space(Instr) ->
%  case hipe_icode:is_call(Instr) of
%    true ->
%      case hipe_icode:call_fun(Instr) of
%	{hipe_bs_primop, {bs_create_space, _, _}} ->
%	  true;
%	_ ->
%	  false
%      end;
%    false ->
%      false
%  end.

%% divide_on_bs_start_match(Code) ->
%%   divide_on_bs_start_match(Code, []).
%% 
%% divide_on_bs_start_match([Instr|Rest], Acc) ->
%%   case is_start_match(Instr) of
%%     true ->
%%       {lists:reverse(Acc), [Instr|Rest]};
%%     false ->
%%       divide_on_bs_start_match(Rest, [Instr|Acc])
%%   end.

is_start_match(Instr) ->
  case hipe_icode:is_call(Instr) of
    true ->
      case hipe_icode:call_fun(Instr) of
	{hipe_bs_primop2, {bs_start_match_2,_}} ->
	  true;
	_ ->
	  false
      end;
    false ->
      false
  end.  

bincomp_fun("-bc$" ++ _) ->
  from_binary;
bincomp_fun("-lbc$" ++ _) ->
  from_list;
bincomp_fun([]) ->
  false;
bincomp_fun([_|Rest]) ->
  bincomp_fun(Rest).

find_matches(Cfg) ->
  Start = hipe_icode_cfg:start_label(Cfg),
  BB = hipe_icode_cfg:bb(Cfg, Start),
  StartMatch = hipe_bb:last(BB),
  true = is_start_match(StartMatch),
  [Arg|_] = hipe_icode:call_args(StartMatch),
  Next = hipe_icode:call_continuation(StartMatch),
  Consum = calculate_consumption(Next, Cfg, 0),
  calc_maxiter(Consum, Arg).

calculate_consumption(Label, Cfg, Acc) -> 
  BB = hipe_icode_cfg:bb(Cfg, Label),
  MatchCall = hipe_bb:last(BB),
  case hipe_icode:is_call(MatchCall) of
    true ->
      Arity = length(hipe_icode:call_args(MatchCall)),
      Name = hipe_icode:call_fun(MatchCall),
      case get_consumption(Name, Arity) of
	X when is_integer(X) ->
	  Next = hipe_icode:call_continuation(MatchCall),
	  calculate_consumption(Next, Cfg, Acc+X);
	done ->
	  Acc
      end
  end.
	  

get_consumption({hipe_bs_primop2,Name}, Arity) ->					 
  case {Name, Arity} of
    {{bs_get_integer_2,Size,_Flags}, 1} ->
      Size;
    {{bs_get_float_2,Size,_Flags}, 1} ->
      Size;
    {{bs_get_binary_2,Size,_Flags}, 1} ->
      Size;
    {{bs_get_binary_all_2, _Flags}, 1} ->
      done;
    {bs_test_tail_2, _NumBits} ->
      done
  end.
					 
calc_maxiter(Consum, Arg) ->
  Dst = hipe_icode:mk_new_var(),
  {[hipe_icode:mk_call([Dst], erlang, bitsize, [Arg], remote),
    hipe_icode:mk_primop([Dst],'div',[Dst,hipe_icode:mk_const(Consum)])],
    Dst}. 

remove_init(Cfg) ->
  Start = hipe_icode_cfg:start_label(Cfg),
  BB = hipe_icode_cfg:bb(Cfg, Start),
  StartMatch = hipe_bb:last(BB),
  true = is_start_match(StartMatch),
  Next = hipe_icode:call_continuation(StartMatch),
  Cont = find_bs_test_tail_cont(Next, Cfg),
  remove_init(Cont, Cfg).

remove_init(Cont, Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Cont),
  Code = hipe_bb:code(BB),
  case find_bs_init(Code) of
    {Init, NewCode} ->   
      NewBB = hipe_bb:code_update(BB, NewCode),
      {Init, hipe_icode_cfg:bb_add(Cfg, Cont, NewBB)};
    no_bs_init ->
      Last = hipe_bb:last(BB),
      case element(1,Last) of
	'if' ->
	  remove_init(hipe_icode:if_true_label(Last), Cfg);
	call ->
	  remove_init(hipe_icode:call_continuation(Last), Cfg);
	goto ->
	  remove_init(hipe_icode:goto_label(Last),Cfg);
	type ->
	  remove_init(hipe_icode:type_true_label(Last),Cfg)
      end
  end.

find_bs_init(Code) ->
  find_bs_init(Code, []).

find_bs_init([Instr|Rest], Acc) ->
  case hipe_icode:is_call(Instr) of
    true ->
      case hipe_icode:call_fun(Instr) of
	{hipe_bs_primop, {bs_init2, _, _}} ->
	  {Instr, lists:reverse(Acc)++
	   remove_spurious_move(Rest, hd(hipe_icode:call_dstlist(Instr)))};
	_ ->
	  find_bs_init(Rest, [Instr|Acc])
      end;
    false ->
      find_bs_init(Rest, [Instr|Acc])
  end;
find_bs_init([], _Acc) ->
  no_bs_init.

remove_spurious_move([I|Rest], Var) -> 
  case lists:member(Var,hipe_icode:uses(I)) of
    true ->
      remove_spurious_move(Rest,Var);
    false ->
      case lists:member(Var,hipe_icode:defines(I)) of
	true -> [I|Rest];
	false -> [I|remove_spurious_move(Rest,Var)]
      end
  end;
remove_spurious_move([], _Var) -> []. 

find_bs_test_tail_cont(Next, Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Next),
  MatchCall = hipe_bb:last(BB),
  case hipe_icode:is_call(MatchCall) of
    true ->
      Cont = hipe_icode:call_continuation(MatchCall),
      case hipe_icode:call_fun(MatchCall) of
	{hipe_bs_primop2, {bs_test_tail_2, _}} ->
	  Cont;
	{hipe_bs_primop2, {bs_get_binary_all_2, _}} ->
	  Cont;
	_ ->
	  find_bs_test_tail_cont(Cont, Cfg)
      end
  end.



