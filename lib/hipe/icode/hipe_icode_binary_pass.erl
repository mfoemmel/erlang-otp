%% -*- erlang-indent-level: 2 -*-


-module(hipe_icode_binary_pass).

-export([make_pass/1]).

-include("../rtl/hipe_literals.hrl").


%% make_pass starts a pass over the an icodecfg. The purpose of the pass
%% It creates a list of basic blocks that are members of a binary match
%% Then it calculates the maximal heap need of this binary match. Finally
%% it creates a new bit syntax operation create space that makes 
%% sure that there is enough space on the heap for each binary match
%%
%% The lists of basic blocks that are members of binary matches are also used 
%% to give all bit syntax operations the state parameters as arguments and destinations. 
make_pass(CFG) ->
  StartLabel = hipe_icode_cfg:start(CFG),
  Vis0 = hipe_icode_cfg:none_visited(CFG),   
  EmptyBinChunks = [],
  FinalBinChunks = make_pass(CFG, [StartLabel], Vis0, EmptyBinChunks),
  CFG1=add_code(FinalBinChunks,CFG),
  copy_state(FinalBinChunks,CFG1).

make_pass(CFG, [Label | Labels], Visited, BinChunks) ->
  case hipe_icode_cfg:visited(Label, Visited) of 
    true ->
      make_pass(CFG, Labels, Visited, BinChunks); 
    false ->
      NewVisited = hipe_icode_cfg:visit(Label, Visited),
      CurrentBB = hipe_icode_cfg:bb(CFG, Label),
      case hipe_icode:is_call(hipe_bb:last(CurrentBB)) of
	true ->
	  case hipe_icode:call_fun(hipe_bb:last(CurrentBB)) of
	    {_, bs_start_match} ->	
	      NewBinChunks = BinChunks ++ [extract_binary_matches(CFG, Label)];
	    _ ->
	      NewBinChunks = BinChunks
	  end;
	false ->
	  NewBinChunks = BinChunks
      end,
      NewLabels = Labels ++ hipe_icode_cfg:succ(CFG, Label),
      make_pass(CFG, NewLabels, NewVisited, NewBinChunks)
  end;

make_pass(_CFG, [], _Visited, BinChunks) ->
  BinChunks.

%%Extract binaries creates the lists of basic blocks that belongs to each binary match

extract_binary_matches(CFG, Label) ->
  CurrentBB = hipe_icode_cfg:bb(CFG, Label),
  Exit_set0 = hipe_icode_cfg:none_visited(CFG),
  Bin_match_call = hipe_bb:last(CurrentBB),
  Exit_set = hipe_icode_cfg:visit(pass_by_restore_catch(hipe_icode:call_fail(Bin_match_call),CFG), Exit_set0),
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
	PossibleLabels = pass_by_restore_catch(hipe_icode_cfg:succ(CFG, Label), CFG, []), 
	AcceptableLabels = remove_members(PossibleLabels, Exit_set1),
	Chunk1 = [{Label, Size, AcceptableLabels} | Chunk0],
	extract_binary_matches(CFG, AcceptableLabels, Exit_set1, Chunk1)
    end,
  extract_binary_matches(CFG, Labels, Exit_set, Chunk); 

extract_binary_matches(_CFG, [], Exit_set0, Chunk0) ->    
  {Exit_set0, Chunk0}.

pass_by_restore_catch([Label| Labels], CFG, Acc) -> 
  pass_by_restore_catch(Labels, CFG, [pass_by_restore_catch(Label, CFG)|Acc]);
pass_by_restore_catch([],_CFG, Acc) ->
  Acc.
pass_by_restore_catch(Label, CFG) -> 
  CurrentBB = hipe_icode_cfg:bb(CFG, Label), 
  [First | _] = hipe_bb:code(CurrentBB),
  case hipe_icode:is_restore_catch(First) of 
    true ->  
      hipe_icode:goto_label(hipe_bb:last(CurrentBB)); 
    false -> Label 
  end.

heap_need(Instruction) ->
  case {hipe_icode:is_call(Instruction), hipe_icode:call_fun(Instruction)} of
    {true, {hipe_bs_primop, Name}} ->
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
  end.


remove_members(Labels, Set) ->
  remove_members(Labels, Set, []).

remove_members([Label | Labels], Set, Ok) ->
  case hipe_icode_cfg:visited(Label, Set) of
    true ->
      remove_members(Labels, Set, Ok);
    false ->
      remove_members(Labels, Set, [Label |Ok])
  end;

remove_members([], _Set, Ok) ->		    
  Ok.

calculate_need(Chunk, Start) ->
  {value,{Start,Need,Succ}}=lists:keysearch(Start,1,Chunk),
  calculate_need(Chunk, Succ, Need).

calculate_need(_Chunk, [], Need) ->
  Need;

calculate_need(Chunk, Succ, Need) ->
  {Resultlist, _} =	lists:mapfoldl(fun(X, Ch)->{calculate_need(Ch,X), Ch} end, Chunk, Succ),
  lists:max(Resultlist)+Need.


add_code([{Start,{_Exitset,Chunk}}|Rest],CFG) ->
  Need=calculate_need(Chunk, Start),
  {Shifts, Args}= runtime_effects(Chunk, CFG),
  StartBlock=hipe_icode_cfg:bb(CFG,Start),
  ResultBB=hipe_bb:code_update(StartBlock, hipe_bb:butlast(StartBlock) ++ [hipe_icode:mk_primop([],{hipe_bs_primop,{bs_create_space, Need, Shifts}},Args), hipe_bb:last(StartBlock)]),
  CFG1=hipe_icode_cfg:bb_update(CFG, Start, ResultBB),
  add_code(Rest,CFG1);

add_code([],CFG) ->
  CFG.

copy_state([{Start,{_Exitset,Chunk}}|Rest],CFG) ->
  StartBlock=hipe_icode_cfg:bb(CFG,Start),
  State = hipe_icode:call_dst(hipe_bb:last(StartBlock)),
  NewCFG = add_state_to_bs_primops(Chunk, CFG, State),
  copy_state(Rest, NewCFG); 

copy_state([],CFG) ->
  CFG.

add_state_to_bs_primops([{Label,_,_}|Rest], CFG, State) ->
  OldBB=hipe_icode_cfg:bb(CFG, Label),
  Instruction=hipe_bb:last(OldBB),
  CFG1= 
    case hipe_icode:is_call(Instruction) of
      true ->
	case hipe_icode:call_fun(Instruction) of

	  {hipe_bs_primop, bs_start_match} ->
	    CFG;

	  {hipe_bs_primop, _} ->
	    OldArgs = hipe_icode:call_args(Instruction),
	    Instruction1 = hipe_icode:call_args_update(Instruction, OldArgs++State),
	    OldDsts = hipe_icode:call_dst(Instruction1),
	    NewInstruction = hipe_icode:call_dst_update(Instruction1, OldDsts++State),
	    NewBB = hipe_bb:code_update(OldBB, hipe_bb:butlast(OldBB)++[NewInstruction]),
	    hipe_icode_cfg:bb_update(CFG, Label, NewBB);	
	  _ ->
	    CFG
	end;
      _ ->
	CFG
    end,
  add_state_to_bs_primops(Rest, CFG1, State);

add_state_to_bs_primops([], CFG, _State) ->
  CFG.

runtime_effects(Chunk, CFG) ->
  runtime_effects(Chunk, CFG, [], []).

runtime_effects([{Label,_,_}|Rest], CFG, Shifts, Args) ->
  Instruction=hipe_bb:last(hipe_icode_cfg:bb(CFG, Label)),

  {Shifts1, Args1} =
    case hipe_icode:is_call(Instruction) of
      true ->
	case hipe_icode:call_fun(Instruction) of
	  {hipe_bs_primop, {bs_get_integer, Size,_}} ->
	    case hipe_icode:call_args(Instruction) of
	      [Arg] ->
		{[rooflog2(Size)|Shifts], [Arg|Args]};
	      _ ->
		{Shifts,Args}
	    end;
	  _ ->
	    {Shifts,Args}
	end;
      _ ->
	{Shifts,Args}
    end,
  runtime_effects(Rest, CFG, Shifts1, Args1);

runtime_effects([], _CFG, Shifts, Args) ->
  {Shifts, Args}.

rooflog2(X) ->
  round(0.5+math:log(X)/math:log(2)).

