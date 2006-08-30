%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% File    : hipe_icode_binary_pass.erl
%% Author  : Per Gustafsson <pergu@it.uu.se>
%% Description : 
%%
%% Created : 13 Mar 2003 by Per Gustafsson <pergu@it.uu.se>
%%--------------------------------------------------------------------

-module(hipe_icode_binary_pass).

-export([make_pass/1, remove_save_restore/1]).

-include("../rtl/hipe_literals.hrl").

%%--------------------------------------------------------------------

%% @spec make_pass(IcodeCFG::icode_cfg()) -> icode_cfg()
%%
%% @type icode_cfg() = term()
%%
%% @doc Makes a pass over an IcodeCFG so as to create a list of basic
%% blocks that are members of a binary match. Then it calculates the
%% maximal heap need of this binary match. Finally, it creates a new
%% bit syntax operation which makes sure that there is enough space on
%% the heap for each binary match.
%%
%% <p> The lists of basic blocks which are members of binary matches
%% are also used to give all bit syntax operations the state
%% parameters as arguments and destinations. </p>
%%

make_pass(IcodeCFG0) ->
  CFG = make_bs_ops_end_basic_blocks(IcodeCFG0),
  StartLabel = hipe_icode_cfg:start_label(CFG),
  Vis0 = hipe_icode_cfg:none_visited(),
  EmptyBinChunks = [],
  FinalBinChunks = make_pass(CFG, [StartLabel], Vis0, EmptyBinChunks),
  CFG1 = add_code(FinalBinChunks, CFG),
  copy_state(FinalBinChunks, CFG1).

make_bs_ops_end_basic_blocks(CFG) ->
  LinIcode = hipe_icode_cfg:cfg_to_linear(CFG),
  Code = hipe_icode:icode_code(LinIcode),
  NewCode = lists:foldr(fun break_block/2, [], Code),
  NewLinIcode = hipe_icode:icode_code_update(LinIcode, NewCode),
  hipe_icode_cfg:linear_to_cfg(NewLinIcode).

break_block(Instr, Acc) ->
  case hipe_icode:is_call(Instr) of 
    true ->
      case hipe_icode:call_fun(Instr) of
	{hipe_bs_primop,_} ->
	  case hipe_icode:call_continuation(Instr) of
	    [] ->
	      ContLbl = hipe_icode:mk_new_label(),
	      NewInstr = hipe_icode:call_set_continuation(Instr, hipe_icode:label_name(ContLbl)),
	      [NewInstr, ContLbl|Acc];
	    _ ->
	      [Instr|Acc]
	  end;
	_ ->
	  [Instr|Acc]
      end;
    false ->
      [Instr|Acc]
  end.

make_pass(CFG, [Label | Labels], Visited, BinChunks) ->
  case hipe_icode_cfg:is_visited(Label, Visited) of 
    true ->
      make_pass(CFG, Labels, Visited, BinChunks); 
    false ->
      NewVisited = hipe_icode_cfg:visit(Label, Visited),
      CurrentBB = hipe_icode_cfg:bb(CFG, Label),
      Instr = hipe_bb:last(CurrentBB),
      NewBinChunks=test_instr(Instr, CFG, Label, BinChunks), 
      NewLabels = Labels ++ hipe_icode_cfg:succ(CFG, Label),
      make_pass(CFG, NewLabels, NewVisited, NewBinChunks)
  end;
make_pass(_CFG, [], _Visited, BinChunks) ->
  BinChunks.

test_instr(Instr, CFG, Label, Acc) ->
  case hipe_icode:is_call(Instr) of
    true ->
      case hipe_icode:call_fun(Instr) of
	{_, bs_start_match} ->	
	 [{match, extract_binary_matches(CFG, Label)}|Acc];
	{_, {bs_init,_,_}} ->
	 [{create, extract_binary_creation(CFG, Label)}|Acc];
	_ ->
	  Acc
      end;
    false ->
     Acc
  end.

extract_binary_creation(CFG, Label) ->
  CurrentBB = hipe_icode_cfg:bb(CFG, Label),
  LastInstr = hipe_bb:last(CurrentBB),
  FailLbl = hipe_icode:call_fail_label(LastInstr),
  Next = hipe_icode:call_continuation(LastInstr),
  extract_binary_creation(CFG, Next, Label, [], [], gb_trees:empty(), FailLbl).

extract_binary_creation(CFG, Label, Start, Labels, Sizes, VarMap, FailLbl) ->
  CurrentBB = hipe_icode_cfg:bb(CFG, Label),
  LastInstr = hipe_bb:last(CurrentBB),
  OtherCode = hipe_bb:butlast(CurrentBB),
  NewVarMap = update_varmap(OtherCode, VarMap),
  case hipe_icode:is_call(LastInstr) of
    true ->
      case hipe_icode:call_fun(LastInstr) of
	{hipe_bs_primop, bs_final} ->
	  {Start, Sizes, [Label|Labels], FailLbl};
	{hipe_bs_primop, Type} ->
	  case Type of
	    {bs_put_float, Size, _,  _} ->
	      [];
	    {bs_put_binary, Size, _} ->
	      [];
	    {bs_put_binary_all, _} ->
	      Size = all;
	    {bs_put_integer, Size, _, _} ->
	      [];
	    {bs_put_string, _, SizeInBytes} ->
	      Size = SizeInBytes * 8;
	    {bs_put_string, _, SizeInBytes, _} ->
	      Size = SizeInBytes * 8
	  end,
	  Next = hipe_icode:call_continuation(LastInstr),
	  case {hipe_icode:call_args(LastInstr), Size} of
	    {[Arg],all} -> 
	      NewFailLbl = hipe_icode:call_fail_label(LastInstr),
	      case acceptable(CurrentBB, Arg) of
		{true, Arg1} ->
		  extract_binary_creation(CFG, Next, Start, [Label|Labels], [{all, Arg1}|Sizes],NewVarMap, NewFailLbl);
		false ->
		  extract_binary_creation(CFG, Next, Start, [Label|Labels], [fail|Sizes],NewVarMap, FailLbl)
	      end;
	    {[_],_} ->
	      extract_binary_creation(CFG, Next, Start, [Label|Labels], [{const, Size}|Sizes],NewVarMap, FailLbl);
	    {[],_} ->
	      extract_binary_creation(CFG, Next, Start, [Label|Labels], [{const, Size}|Sizes],NewVarMap, FailLbl);
	    {[_,SizeVar],_} ->
	      RealSizeVar = translate_sizevar(SizeVar, NewVarMap),
	      NewFailLbl = hipe_icode:call_fail_label(LastInstr),
	      extract_binary_creation(CFG, Next, Start, [Label|Labels], [{Size, RealSizeVar}|Sizes], NewVarMap, NewFailLbl)
	  end
      end
  end.      

update_varmap([Instr|Rest], VarMap) ->
  case hipe_icode:is_move(Instr) of
    true ->
      Src = hipe_icode:move_src(Instr),
      Dst = hipe_icode:move_dst(Instr),
      case hipe_icode:is_var(Src) andalso hipe_icode:is_var(Dst) of
	true ->
	  case gb_trees:lookup(Src, VarMap) of
	    {value,Val} ->
	      update_varmap(Rest, gb_trees:insert(Dst, Val, VarMap));
	    none ->
	      update_varmap(Rest, gb_trees:insert(Dst, Src, VarMap))
	  end;
	false ->
	  update_varmap(Rest, VarMap)
      end;
    false ->
      update_varmap(Rest, VarMap)
  end;
update_varmap([], VarMap) ->
  VarMap.

translate_sizevar(SizeVar, VarMap) ->
  case gb_trees:lookup(SizeVar, VarMap) of
    {value, Val} ->
      Val;
    none ->
      SizeVar
  end.

%% Extract binaries creates the lists of basic blocks that belong to
%% each binary match

acceptable(BB, Arg) ->
  Code = hipe_bb:butlast(BB),
  case Code of
    [] ->
      {true, Arg};
    [_|_] ->
      Instr = lists:last(Code),
      case hipe_icode:is_move(Instr) of
	true ->
	  case hipe_icode:move_dst(Instr) of
	    Arg ->
	      Src = hipe_icode:move_src(Instr),
	      case hipe_icode:is_var(Src) of
		true ->
		  {true, Src};
		false ->
		  false
	      end;
	    _ ->
	      {true, Arg}
	  end;
	_ ->
	  {true, Arg}
      end
  end.

extract_binary_matches(CFG, Label) ->
  CurrentBB = hipe_icode_cfg:bb(CFG, Label),
  Exit_set0 = hipe_icode_cfg:none_visited(),
  Bin_match_call = hipe_bb:last(CurrentBB),
  Exit_set = hipe_icode_cfg:visit(pass_by_begin_handler(hipe_icode:call_fail_label(Bin_match_call),CFG), Exit_set0),
  Chunk = [{Label, 0, Successors = [hipe_icode:call_continuation(Bin_match_call)] }],
  {Label, extract_binary_matches(CFG, Successors, Exit_set, Chunk)}.

extract_binary_matches(CFG, [Label | Labels], Exit_set0, Chunk0) ->
  {Exit_set, Chunk} =
    case lists:keysearch(Label, 1, Chunk0) of
      {value, _} ->
	{Exit_set0, Chunk0};
      false ->
	LastInstr = hipe_bb:last(hipe_icode_cfg:bb(CFG,Label)),
	Size = heap_need(LastInstr), 
	Exit_set1 =
	  case hipe_icode:is_call(LastInstr) of
	    true ->
	      case hipe_icode:call_fun(LastInstr) of
		{_, {bs_test_tail,_}} ->
		  hipe_icode_cfg:visit(hipe_icode:call_continuation(LastInstr), Exit_set0);
		_ ->
		  Exit_set0
	      end;
	    false ->
	      Exit_set0
	  end,
	PossibleLabels = pass_by_begin_handler(hipe_icode_cfg:succ(CFG, Label), CFG, []), 
	AcceptableLabels = remove_members(PossibleLabels, Exit_set1),
	Chunk1 = [{Label, Size, AcceptableLabels} | Chunk0],
	extract_binary_matches(CFG, AcceptableLabels, Exit_set1, Chunk1)
    end,
  extract_binary_matches(CFG, Labels, Exit_set, Chunk); 
extract_binary_matches(_CFG, [], Exit_set0, Chunk0) ->    
  {Exit_set0, Chunk0}.

pass_by_begin_handler([Label| Labels], CFG, Acc) -> 
  pass_by_begin_handler(Labels, CFG, [pass_by_begin_handler(Label, CFG)|Acc]);
pass_by_begin_handler([], _CFG, Acc) ->
  Acc.

pass_by_begin_handler(Label, CFG) -> 
  CurrentBB = hipe_icode_cfg:bb(CFG, Label), 
  [First|_] = hipe_bb:code(CurrentBB),
  case hipe_icode:is_begin_handler(First) of 
    true ->  
      hipe_icode:goto_label(hipe_bb:last(CurrentBB)); 
    false -> Label 
  end.

heap_need(Instruction) ->
  case hipe_icode:is_call(Instruction) of
    true ->
      case hipe_icode:call_fun(Instruction) of
	{hipe_bs_primop, Name} ->
	  case Name of
	    {bs_get_integer,Size,_} ->
	      case hipe_icode:call_args(Instruction) of
	        [] ->
	          case Size < 28 of
		    true ->
		      0;
		    false ->
		      ((Size+31) div 32)+2
	          end;
	        _ ->
	          0
	      end;
	    {bs_get_float,_,_} ->
	      3;
	    {bs_get_binary,_Size,Flag} ->
	      case Flag band 1 of
		1 ->
		  4;
		0 ->
		  trunc(?MAX_HEAP_BIN_SIZE/4) +2
	      end;
	    {bs_get_binary_all, _} ->
	      4;
	    _ ->
	      0
	  end;
        _ ->
          0
      end;
    false ->
      0
  end.

remove_members(Labels, Set) ->
  remove_members(Labels, Set, []).

remove_members([Label | Labels], Set, Ok) ->
  case hipe_icode_cfg:is_visited(Label, Set) of
    true ->
      remove_members(Labels, Set, Ok);
    false ->
      remove_members(Labels, Set, [Label|Ok])
  end;
remove_members([], _Set, Ok) ->
  Ok.

calculate_need({Chunk, Hash}, Start) ->
  case gb_trees:lookup(Start, Hash) of
    {value, Val} ->
      {Val, {Chunk, Hash}};
    none ->
      {value,{Start,Need,Succ}}=lists:keysearch(Start,1,Chunk),
      {NewValue, {Chunk, NewHash}} =calculate_need(Chunk, Succ, Need, Hash),
      NewerHash=gb_trees:enter(Start, NewValue, NewHash),
      {NewValue, {Chunk, NewerHash}}
  end;
calculate_need(Chunk, Start) ->
  {Need,_} = calculate_need({Chunk, gb_trees:empty()}, Start),
  Need.

calculate_need(Chunk, [], Need, Hash) ->
  {Need, {Chunk, Hash}};

calculate_need(Chunk, Succ, Need, Hash) ->
  {Resultlist,NewAccu} = lists:mapfoldl(fun(X, Ch)-> calculate_need(Ch, X) end,
					{Chunk,Hash}, Succ),
  {lists:max(Resultlist)+Need, NewAccu}.

add_code([{match,{Start,{_Exitset,Chunk}}}|Rest], CFG) ->
  Need = calculate_need(Chunk, Start),
  {Shifts,Args} = runtime_effects(Chunk, CFG),
  StartBlock = hipe_icode_cfg:bb(CFG,Start),
  ResultBB = hipe_bb:code_update(StartBlock, hipe_bb:butlast(StartBlock) ++ [hipe_icode:mk_primop([],{hipe_bs_primop,{bs_create_space, Need, Shifts}},Args), hipe_bb:last(StartBlock)]),
  CFG1 = hipe_icode_cfg:bb_add(CFG, Start, ResultBB),
  add_code(Rest,CFG1);
add_code([{create,{Start, Sizes, Labels, FailLbl}}|Rest], CFG) ->
  StartBlock = hipe_icode_cfg:bb(CFG,Start),
  Init = hipe_bb:last(StartBlock),
  {NewBlock,State} = update_init(Init, StartBlock, Sizes, FailLbl),
  CFG1 = hipe_icode_cfg:bb_add(CFG, Start, NewBlock),
  CFG2 = add_state(Labels, State, CFG1),
  add_code(Rest, CFG2);
add_code([], CFG) ->
  CFG.

update_init(Init, StartBlock, Sizes, FailLbl) ->		       
  Base = hipe_icode:mk_new_reg(),
  Offset = hipe_icode:mk_new_reg(),
  NewInit = 
    case condense_sizes(Sizes) of
      {Const, Units, SizeArgs} ->
	Init1 = hipe_icode:call_fun_update(Init, {hipe_bs_primop,{bs_init,{Const, Units}}}),
	Init2 = hipe_icode:call_dstlist_update(Init1, [Base, Offset]),
	Init3 = hipe_icode:call_args_update(Init2, SizeArgs),
	hipe_icode:call_set_fail_label(Init3, FailLbl);
      fail ->
	Init1 = hipe_icode:call_fun_update(Init, {hipe_bs_primop,{bs_init,fail}}),
	hipe_icode:call_dstlist_update(Init1, [Base, Offset])
    end,
  NewBlock = hipe_bb:code_update(StartBlock, hipe_bb:butlast(StartBlock) ++ [NewInit]),
  {NewBlock,[Base,Offset]}.

add_state([Label|Rest], State, CFG) ->
  Block = hipe_icode_cfg:bb(CFG, Label),
  Instr = hipe_bb:last(Block),
  Instr1 = add_state_to_instr(Instr, State),
  NewBB = hipe_bb:code_update(Block, hipe_bb:butlast(Block)++[Instr1]),
  CFG1 = hipe_icode_cfg:bb_add(CFG, Label, NewBB),
  add_state(Rest, State, CFG1);
add_state([], _State, CFG) ->
  CFG.

add_state_to_instr(Instruction, State=[_Base, Offset]) ->
  case hipe_icode:is_call(Instruction) of
    true ->
      case hipe_icode:call_fun(Instruction) of
	{hipe_bs_primop, Name} ->
	  OldArgs = hipe_icode:call_args(Instruction),
	  OldDsts = hipe_icode:call_dstlist(Instruction),
	  {NewArgs, NewDsts} =
	    case Name of
	      bs_init ->
		{OldArgs, State};
	      {bs_put_string, _, _} ->
		{OldArgs++State, [Offset]};
	      {bs_put_string, _, _, _} ->
		{OldArgs++State, [Offset]};
	      {bs_put_integer, _, _, _} ->
		{OldArgs++State, [Offset]};
	      {bs_put_float, _, _, _} ->
		{OldArgs++State, [Offset]};
	      {bs_put_binary, _, _} ->
		{OldArgs++State, [Offset]};
	      {bs_put_binary_all, _} ->
		{OldArgs++State, [Offset]};
	      bs_final ->
		{OldArgs++State, OldDsts}
	    end,
	  Instruction1 = hipe_icode:call_args_update(Instruction, NewArgs),
	  hipe_icode:call_dstlist_update(Instruction1, NewDsts);
	_ ->
	  Instruction
      end;
    _ ->
      Instruction
  end.

copy_state([{match,{_Start,{_Exitset,Chunk}}}|Rest],CFG) ->
  State = [hipe_icode:mk_new_reg(),
	   hipe_icode:mk_new_reg(),
	   hipe_icode:mk_new_reg(),
	   hipe_icode:mk_new_reg(),
	   hipe_icode:mk_new_reg()],
  NewCFG = add_state_to_bs_primops(Chunk, CFG, State),
  copy_state(Rest, NewCFG); 
copy_state([_|Rest], CFG) ->
  copy_state(Rest, CFG);
copy_state([],CFG) ->
  CFG.

add_state_to_bs_primops([{Label,_,_}|Rest], CFG,
			State=[BinSize,Base,Offset,Orig,OrigOffset]) ->
  OldBB = hipe_icode_cfg:bb(CFG, Label),
  Instruction = hipe_bb:last(OldBB),
  CFG1 =
    case hipe_icode:is_call(Instruction) of
      true ->
	case hipe_icode:call_fun(Instruction) of
	  {hipe_bs_primop, Name} ->
	    OldArgs = hipe_icode:call_args(Instruction),
	    OldDsts = hipe_icode:call_dstlist(Instruction),
	    NewName = skip_bits(Name, OldDsts),
	    {NewArgs, NewDsts} =
	      case NewName of
		bs_start_match ->
		  {OldArgs, State};
		{bs_get_integer, _, _} ->
		  {OldArgs++[BinSize, Base, Offset, Orig], OldDsts++[Offset]};
		{bs_get_float, _, _} ->
		  {OldArgs++[BinSize, Base, Offset, Orig], OldDsts++[Offset]};
		{bs_get_binary, _, _} ->
		  {OldArgs++State, OldDsts++[Offset]};
		{bs_get_binary_all, _} ->
		  {OldArgs++[BinSize, Offset, Orig, OrigOffset], OldDsts++[Offset]};
		{bs_skip_bits, _} ->
		  {OldArgs++[BinSize, Offset], [Offset]};
		{bs_skip_bits_all, _} ->
		  {OldArgs++[BinSize, Offset], [Offset]};
		{bs_test_tail, _} ->
		  {OldArgs++[BinSize, Offset], []};
		{bs_save, _} ->
		  {OldArgs++State, OldDsts};
		{bs_restore, _} ->
		  {OldArgs, OldDsts++State}
	      end,
	    Instruction1 = hipe_icode:call_args_update(Instruction, NewArgs),
	    Instruction2 = hipe_icode:call_dstlist_update(Instruction1, NewDsts),
	    NewInstruction = hipe_icode:call_fun_update(Instruction2, {hipe_bs_primop, NewName}),
	    NewBB = hipe_bb:code_update(OldBB, hipe_bb:butlast(OldBB)++[NewInstruction]),
	    hipe_icode_cfg:bb_add(CFG, Label, NewBB);	
	  _ ->
	    CFG
	end;
      _ ->
	CFG
    end,
  add_state_to_bs_primops(Rest, CFG1, State);
add_state_to_bs_primops([], CFG, _State) ->
  CFG.

get_local_mapping(Code) ->
  get_local_mapping(Code, gb_trees:empty()).

get_local_mapping([Instr|Instrs], Map) ->
  case hipe_icode:is_move(Instr) of
    true ->
      Dst = hipe_icode:move_dst(Instr), 
      Src = hipe_icode:move_src(Instr),
      get_local_mapping(Instrs, gb_trees:enter(Dst, Src, Map)); 
    false ->
      get_local_mapping(Instrs, Map)
  end;
get_local_mapping([], Map) ->
  Map.

get_alias(Arg, Map) ->
  case gb_trees:lookup(Arg, Map) of
    {value, Val} -> Val;
    none -> Arg
  end.       

runtime_effects(Chunk, CFG) ->
  {Vars, Bin}=find_results(Chunk, CFG),
  runtime_effects(Chunk, CFG, [], [], Vars, Bin).

runtime_effects([{Label,_,_}|Rest], CFG, Shifts, Args, Vars, Bin) ->
  BB = hipe_icode_cfg:bb(CFG, Label),
  LocalMapping = get_local_mapping(hipe_bb:code(BB)),
  Instruction=hipe_bb:last(BB),
  case hipe_icode:is_call(Instruction) of
    true ->
      case hipe_icode:call_fun(Instruction) of
	{hipe_bs_primop, {bs_get_integer, Size,_}} ->
	  {Shifts1, Args1} =
	    case hipe_icode:call_args(Instruction) of
	      [Arg] ->
		RealArg  = get_alias(Arg, LocalMapping),
		case lists:member(RealArg, Vars) of
		  true ->
		    {[size|Shifts], [Bin|Args]};
		  false ->
		    {[rooflog2(Size)|Shifts], [RealArg|Args]}
		end;
	      _ ->
		{Shifts,Args}
	    end,
	  runtime_effects(Rest, CFG, Shifts1, Args1, Vars, Bin);
	_ ->
	  runtime_effects(Rest, CFG, Shifts, Args, Vars, Bin)
      end;
    _ ->
      runtime_effects(Rest, CFG, Shifts, Args, Vars, Bin)
  end;
runtime_effects([], _CFG, Shifts, Args, _Vars, _Bin) ->
  {Shifts, Args}.

find_results(Chunk, CFG) ->
  find_results(Chunk, CFG, [], []).

find_results([{Label,_,_}|Rest], CFG, Vars, Bin) ->
  Instruction=hipe_bb:last(hipe_icode_cfg:bb(CFG, Label)),
  {NewVars, NewBin} =
    case hipe_icode:is_call(Instruction) of
      true ->
	case hipe_icode:call_fun(Instruction) of
	  {hipe_bs_primop, bs_start_match} ->
	    [Arg] = hipe_icode:call_args(Instruction),
	    {Vars, Arg};
	  {hipe_bs_primop, {bs_get_integer, _Size,_Flags}} ->
	    [Dst] = hipe_icode:call_dstlist(Instruction),
	    {[Dst|Vars], Bin};
	  _ ->
	    {Vars,Bin}
	end;
      _ ->
	{Vars, Bin}
    end,
  find_results(Rest, CFG, NewVars, NewBin);
find_results([], _CFG, NewVars, NewBin) ->
  {NewVars, NewBin}.

rooflog2(X) ->
  round(0.5+math:log(X)/math:log(2)).

condense_sizes(Sizes) ->
  condense_sizes(Sizes, 0, {[],[]}, []).

condense_sizes([{const, N}|Rest], Consts, Vars, Alls) -> 
  condense_sizes(Rest, Consts+N, Vars, Alls);
condense_sizes([{all, Bin}|Rest],  Consts, Vars, Alls) -> 
  condense_sizes(Rest, Consts, Vars, [Bin|Alls]);
condense_sizes([{Unit, Var}|Rest],  Consts, {Units,Vars}, Alls) -> 
  condense_sizes(Rest, Consts, {[Unit|Units], [Var|Vars]}, Alls);
condense_sizes([fail|_Rest], _Consts, _Vars, _Alls) ->
  fail;
condense_sizes([], Consts, {Units,Vars}, Alls) -> 
  {Consts, Units, Vars ++ Alls}.

%% @spec remove_save_restore(IcodeCFG::icode_cfg()) -> icode_cfg()
%%
%% @doc Given an IcodeCFG, returns a NewIcodeCFG where all the bs_save
%% and bs_restore primops are removed.
%%
remove_save_restore(IcodeCFG) ->
  Labels = hipe_icode_cfg:reverse_postorder(IcodeCFG),
  iterate_blocks(Labels, IcodeCFG, gb_trees:empty()).

iterate_blocks([Label|Rest], CFG, Info) ->
  CurrentBB = hipe_icode_cfg:bb(CFG, Label),
  Code = hipe_bb:code(CurrentBB),
  {NewCode,NewInfo} = iterate_instructions(Code, Info, []),
  NewBB = hipe_bb:code_update(CurrentBB, NewCode),
  NewCFG = hipe_icode_cfg:bb_add(CFG, Label, NewBB),
  iterate_blocks(Rest, NewCFG, NewInfo);
iterate_blocks([], CFG, _Info) ->
  CFG.

iterate_instructions([Instruction|Rest], Info, Acc) ->
  case hipe_icode:is_call(Instruction) of
    true ->
      case hipe_icode:call_fun(Instruction) of
	{hipe_bs_primop, {bs_save, I}} ->
	  Args = hipe_icode:call_args(Instruction),
	  NewInfo = gb_trees:enter(I, Args, Info),
	  case hipe_icode:call_continuation(Instruction) of
	    [] ->
	      iterate_instructions(Rest, NewInfo, Acc);
	    Lbl ->
	      iterate_instructions(Rest, NewInfo, [hipe_icode:mk_goto(Lbl)|Acc])
	  end;
	{hipe_bs_primop, {bs_restore, I}} ->
	  {value, Args} = gb_trees:lookup(I, Info),
	  Dsts = hipe_icode:call_dstlist(Instruction),
	  Moves = hipe_icode:mk_moves(Dsts, Args),
	  case hipe_icode:call_continuation(Instruction) of
	    [] ->
	      iterate_instructions(Rest, Info, Moves++Acc);
	    Lbl ->
	      iterate_instructions(Rest, Info, [hipe_icode:mk_goto(Lbl)|Moves] ++ Acc)
	  end;
	_ ->
	   iterate_instructions(Rest, Info, [Instruction|Acc])
      end;
    _ ->
      iterate_instructions(Rest, Info, [Instruction|Acc])
  end;
iterate_instructions([], Info, Acc) ->
  {lists:reverse(Acc), Info}.

skip_bits(Name, []) ->
  case Name of
    {bs_get_integer, Unit, _Flags} ->
      {bs_skip_bits, Unit};
    {bs_get_float, Unit, _Flags} ->
      {bs_skip_bits, Unit};
    {bs_get_binary, Unit, _Flags} ->
      {bs_skip_bits, Unit};
    {bs_get_binary_all, Flags} ->
      {bs_skip_bits_all, Flags};
    _ ->
      Name
  end;
skip_bits(Name, _) ->
  Name.
