-module(hipe_hot_cold).

-export([finalize/3,get_hotness/1,split_constants/1,pp/2,pp/3]).


is_hot(none,Hotness) -> 
    false;
is_hot(Label,{Treshold,Blocks}) -> 
    Val = find_block(Label,Blocks),
    Val > Treshold.

find_block(L,[{L,Val}|_]) -> Val;
find_block(L,[_|Rest]) ->
    find_block(L,Rest);
find_block(L,[]) -> 0. 


get_hotness(MFA) ->
  {-1,[]}.
%%    {0.5,absolute_to_rel(hipe_profile:read_counters(MFA))}.

%absolute_to_rel([{Block,Value}|Rest]) ->
%    if 
%	Value == 0 ->
%	    [{Block,0}|absolute_to_rel(Rest)];
%	true ->
%	    [{Block,1}|absolute_to_rel(Rest)]
%    end;
%absolute_to_rel([]) -> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Does the final layout of the code and fills delay slots. After
% this pass the code can't be converted back to a CFG.
%
%new_finalize(CFG,Hotness,Options) ->
%  
%   [{hot,add_nops(jmp_peephole(peephole(hipe_sparc:sparc_code(hipe_sparc_cfg:linearize(CFG)))))},
%    []].
%
%add_nops([I|Is]) ->
%   case hipe_sparc:is_any_branch(I) of
%     true ->
%       [I,hipe_sparc:nop_create([])|add_nops(Is)];
%     false ->
%       [I|add_nops(Is)]
%   end;
%add_nops([]) -> [].

%jmp_peephole([I1, I2 |Is]) ->
%  case hipe_sparc:is_goto(I1) of
%    true ->
%      case hipe_sparc:goto_label(I1) =:= hipe_sparc:label_name(I2) of
%	true ->
%	  [I2 | jmp_peephole(Is)];
%	false ->
%	  [I1, I2 | jmp_peephole(Is)]
%      end;
%    false ->
%      [I1 | jmp_peephole([I2 | Is])]
%  end;
%jmp_peephole([I]) -> [I];
%jmp_peephole([]) -> [].

finalize(CFG,Hotness,Options) ->
   Start = hipe_sparc_cfg:start(CFG),
   Vis = none_visited(),
   {Vis0, NestedHotCode,NestedColdCode} =
	finalize_succ(Start, CFG, Vis,Hotness, Options),
   {AllHotCode,AllColdCode} =
	finalize_fail_entries(NestedHotCode,NestedColdCode, CFG,
			      hipe_sparc_cfg:other_entrypoints(CFG),
			      Vis0,Hotness, Options),
    case is_hot(Start,Hotness) of
	true ->
	    [{hot,
	      opt(lists:flatten(AllHotCode))},[{cold,opt(lists:flatten(AllColdCode))}]];
	false ->
	    [{cold,
	      opt(lists:flatten(AllColdCode))},[{hot,opt(lists:flatten(AllHotCode))}]]
    end.

opt(Code) ->
  lists:reverse(opt(Code,[])).

opt([G,L|Code], Acc) ->
  case hipe_sparc:is_goto(G) of
    true ->
      case hipe_sparc:is_label(L) of
	true ->
	  case hipe_sparc:goto_label(G) =:= hipe_sparc:label_name(L) of
	    true ->
	      opt(Code,[L|Acc]);
	    false ->
	      opt(Code,[L,G|Acc])
	  end;
	false ->
	  opt([L|Code], [G|Acc])
      end;
    false ->
      opt([L|Code], [G|Acc])
  end;
opt([I],Acc) ->[I|Acc];
opt([],Acc) -> Acc.

finalize_succ(none, CFG, Vis,Hotness, Options) ->
    {Vis,[],[]};
finalize_succ(Label, CFG, Vis,Hotness, Options) ->
  case visited(Label, Vis) of
      true ->
      {Vis, [],[]};      % already visited
    false ->
      Vis0 = visit(Label, Vis),
      BB = hipe_sparc_cfg:bb(CFG, Label),
      Fallthrough = hipe_sparc_cfg:fallthrough(CFG, Label),
      Cond = hipe_sparc_cfg:cond(CFG, Label),
      %% If Label got only one successor thats not been visited we can
      %% remove the jump.
      Merge = 
	case {Fallthrough, Cond} of
	  {Lbl, none} when Lbl =/= none -> 
	    case visited(Lbl, Vis0) of
	      true -> false;
	      false -> 
		is_hot(Lbl,Hotness) =:= is_hot(Label,Hotness) 
	    end;
	  _ -> false
	end,
      if Merge =:= true ->
	  LblInstr = hipe_sparc:label_create(Label, hipe_bb:annot(BB)),
	  Jmp = hipe_bb:last(BB),

	  case hipe_sparc:is_call_link(Jmp) of 
	    false ->
	      Code = hipe_bb:butlast(BB),
	      {Vis1, Code1,ColdCode1} = finalize_succ(Fallthrough, CFG,
						      Vis0,Hotness, Options),
	      case is_hot(Label,Hotness) of
		true ->
		  {Vis1, [[LblInstr|fill_delay(Code, Options)], Code1],ColdCode1};
		false ->
		  {Vis1, Code1, [[LblInstr|fill_delay(Code, Options)], ColdCode1]}
	      end;
	    true -> %% We have a jsr with no fail continuation.
	      Code = hipe_bb:code(BB),
	      {Vis1, Code1,ColdCode1} = finalize_succ(Fallthrough, CFG,
						      Vis0,Hotness, Options),
	      case is_hot(Label,Hotness) of
		true ->
		  {Vis1, [[LblInstr|fill_delay(Code, Options)], Code1],ColdCode1};
		false ->
		  {Vis1, Code1, [[LblInstr|fill_delay(Code, Options)], ColdCode1]}
	      end
	  end;
	 true ->
	  LblInstr = hipe_sparc:label_create(Label, hipe_bb:annot(BB)),
	  Code = hipe_bb:code(BB),
	  {Vis1, Code1,ColdCode1} = finalize_succ(Fallthrough, CFG,
						  Vis0,Hotness, Options),
	  {Vis2, Code2,ColdCode2} =
	    if list(Cond) -> %% This is a jumptable handled by entrypoints
		{Vis1, Code1,ColdCode1};
	       true ->
		finalize_succ(Cond, CFG,
			      Vis1,Hotness, Options)
	    end,
	  if 
	    Fallthrough == none -> %% There is no code to fall through to
	      Code1 = ColdCode1 = [], %% Assertion !!!!!!!!!!!!!
	      case is_hot(Label,Hotness) of
		true -> % Code is hot. 
		  {Vis2, 
		   [[LblInstr|fill_delay(Code, Options)],Code2],
		   [ColdCode2]};
		false -> % Code is cold, no fallthrough
		  {Vis2, 
		   [Code2],
		   [[LblInstr|fill_delay(Code, Options)],ColdCode2]}
	      end;
	    true -> %% There is fallthroughcode
	      if 
		Cond == none -> %% There is no code to jump to -> ba(Fallthrough)
		  Code2 = ColdCode2 = [], %% Assertion !!!!!!!!!!!!!
		  case is_hot(Label,Hotness) of
		    true -> % Code is hot. 
		      {Vis2, 
		       [[LblInstr|fill_delay(Code, Options)],Code1],
		       [ColdCode1]};
		    false -> % Code is cold, no fallthrough
		      {Vis2, 
		       [Code1],
		       [[LblInstr|fill_delay(Code, Options)],ColdCode1]}
		  end;  
		true -> 
		  case is_hot(Label,Hotness) of
		    true -> % Code is hot.
		      case is_hot(Fallthrough,Hotness) of
			true -> % Fallthrough is also hot so no jump needed
			  {Vis2, 
			   [[LblInstr|fill_delay(Code, Options)], Code1, Code2],
			   [ColdCode1,ColdCode2]};
			false -> % Code hot, Fallthrough cold a jump is needed
			  {Vis2, 
			   [[LblInstr|fill_delay(Code++[hipe_sparc:goto_create(Fallthrough,[])], Options)],
			    Code1, Code2],
			   [ColdCode1,ColdCode2]}
		      end;
		    false -> % Code is cold.
		      case is_hot(Fallthrough,Hotness) of
			false -> % Fallthrough is also cold so no jump needed
			  {Vis2, 
			   [Code1, Code2],
			   [[LblInstr|fill_delay(Code, Options)],ColdCode1,ColdCode2]};
			true -> % Code cold, Fallthrough hot a jump is needed
			  {Vis2, 
			   [Code1, Code2],
			   [[LblInstr|fill_delay(Code++[hipe_sparc:goto_create(Fallthrough,[])], Options)],
			    ColdCode1,ColdCode2]}
		      end % falltrough is hot
		  end % Label is hot
	      end % Cond == none
	  end % Fallthrough == none
      end % if Merge
  end. % Visited



finalize_fail_entries(HotCode,ColdCode, CFG, [], Vis,Hotness, Options) ->
    {HotCode,ColdCode};
finalize_fail_entries(HotCode,ColdCode, CFG, [E|Es], Vis,Hotness, Options) ->
    {Vis0, MoreHotCode,MoreColdCode} = finalize_succ(E, CFG,
						     Vis,Hotness, Options),
    finalize_fail_entries([HotCode, MoreHotCode],
			  [ColdCode,MoreColdCode], 
			  CFG, Es, Vis0,Hotness, Options).


%
% Code is a list of instructions (from one basic block).
%

fill_delay(Code, Options) ->
    Code0 = 
	case property_lists:get_bool(sparc_peephole,Options) of
	    true  -> peephole(Code);
	    false -> Code
    end,
    Codes =  split_at_branch(Code0),
    case property_lists:get_bool(fill_delayslot,Options) of
	true  -> lists:map(fun fill_delay0/1, Codes);
	false -> lists:map(fun nofill_delay/1, Codes)
    end.

nofill_delay(Code) ->
  [Code | [hipe_sparc:nop_create([])]].


%
% Code is a list of instructions where a branch/jump 
%

fill_delay0(Code) ->
   case catch find_delay(Code) of
      no_branch ->
	 Code;
      {NewCode, _, _} ->
	 [NewCode | [hipe_sparc:nop_create([])]];
      {NewCode, Delay} ->
	 [NewCode | [Delay]]
   end.


%
% Extracts a delay instruction from a list 
%

find_delay([Jmp]) ->
   case hipe_sparc:is_any_branch(Jmp) of
      true ->
	 {[Jmp], 
	  ordsets:from_list(hipe_sparc:uses(Jmp)), 
	  ordsets:from_list(hipe_sparc:defines(Jmp))};
      false ->
	 throw(no_branch)
   end;
find_delay([I|Is]) ->
   case find_delay(Is) of
      {NewIs, Uses, Defs} ->
	 IUses = ordsets:from_list(hipe_sparc:uses(I)),
	 IDefs = ordsets:from_list(hipe_sparc:defines(I)),
	 NewUses = ordsets:union(Uses, IUses),
	 NewDefs = ordsets:union(Defs, IDefs),
	 case is_delay_instr(I) of
	    true ->
	       %% Make sure the instruction isn't defining a reg that is 
	       %% used later or uses a reg that is defined later or
	       %% defines a reg that is defined later
	       X = {ordsets:intersection(Uses, IDefs), 
		    ordsets:intersection(Defs, IUses),
		    ordsets:intersection(Defs, IDefs)},
	       case X of
		  {[], [], []} ->  %% No conflicts, found a delay instr.
		     {NewIs, I};
		  _ ->
		     {[I|NewIs], NewUses, NewDefs}
	       end;
	    false ->
	       {[I|NewIs], NewUses, NewDefs}
	 end;
      {NewIs, Delay} ->
	 {[I|NewIs], Delay}
   end.


%
% true if I is an instruction that can be moved to a delay slot
%

is_delay_instr(I) ->
%%  false.
%%
%% foo(I)->
    case hipe_sparc:type(I) of
	comment -> false;
	load_address -> false;
	load_atom -> false;
	load_word_index -> false;
	%% (Happi) Tests have indicated that puting loads 
	%%         in the delayslot can slow down code...
	%%         ... but it can also speed up code.
	%%         the impact is about 10 - 20 % on small bms
	%%         on the average you loose 1-2 % by not putting
	%%         loads in delayslots
	%% load -> false;
	_ -> true
    end.

%
% Split a list of instructions to a list of lists of instructions
% Where each sublist ends with a branch.
%

split_at_branch([]) ->
   [];
split_at_branch([I]) ->
   [[I]];
split_at_branch([I|Is]) ->
   case hipe_sparc:is_any_branch(I) of
      true ->
	 [[I] | split_at_branch(Is)];
      false ->
	 [Same|Lists] = split_at_branch(Is),
	 [[I|Same]|Lists]
   end.

%
% 
%


peephole([]) ->
   [];
peephole([I|Is]) ->
   case hipe_sparc:type(I) of
      move ->
	 case hipe_sparc:move_src(I) =:= hipe_sparc:move_dest(I) of
	    true ->
	       peephole(Is);
	    false ->
	       [I | peephole(Is)]
	 end;
      _ ->
	 [I | peephole(Is)]
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


none_visited() ->
   hipe_hash:empty().

visit(X, Vis) -> 
   hipe_hash:update(X, visited, Vis).

visited(X, Vis) ->
   case hipe_hash:lookup(X, Vis) of
      not_found -> false;
      {found,_} -> true
   end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Replaces big immediates with registers that are defined with
% a 'sethi' and an 'or'. 
%
% NEWSFLASH! This also checks if arg1 to an alu operation is an immediate
% and rectifies that situation.
%

split_constants(CFG) ->
   Labels = hipe_sparc_cfg:labels(CFG),
   {Low, High} = hipe_sparc_cfg:var_range(CFG),
   hipe_gensym:set_var(High),
   NewCFG = split_bbs(Labels, CFG),
   hipe_sparc_cfg:var_range_update(NewCFG, {Low, hipe_gensym:get_var()}).


split_bbs([], CFG) ->
   CFG;
split_bbs([Lbl|Lbls], CFG) ->
   BB = hipe_sparc_cfg:bb(CFG, Lbl),
   Code = hipe_bb:code(BB),
   case split_instrs(Code, [], unchanged) of
      unchanged ->
	 split_bbs(Lbls, CFG);
      NewCode ->
	 NewCFG = hipe_sparc_cfg:bb_update(CFG, Lbl, hipe_bb:code_update(BB, NewCode)),
	 split_bbs(Lbls, NewCFG)
   end.


split_instrs([], RevCode, unchanged) ->
   unchanged;
split_instrs([], RevCode, changed) ->
   lists:reverse(RevCode);
split_instrs([I|Is], RevCode, Status) ->
   case fuck_left_immediate(I) of
      unchanged ->
	 case split_instr(I) of
	    unchanged ->
	       split_instrs(Is, [I|RevCode], Status);
	    NewCode ->
	       split_instrs(Is, NewCode++RevCode, changed)
	 end;
      NewCode ->
	 split_instrs(NewCode++Is, RevCode, changed)
   end.


fuck_left_immediate(I) ->
   case {hipe_sparc:is_alu(I), hipe_sparc:is_alu_cc(I)} of
      {true, _} ->
	 Src1 = hipe_sparc:alu_src1(I),
	 case hipe_sparc:is_imm(Src1) of
	    true ->
	       case is_commutative(hipe_sparc:alu_operator(I)) of
		  true ->
		     Src2 = hipe_sparc:alu_src2(I),
		     I0 = hipe_sparc:alu_src1_update(I, Src2),
		     [hipe_sparc:alu_src2_update(I0, Src1)];
		  false ->
		     Tmp = hipe_sparc:mk_new_reg(),
		     Mov = hipe_sparc:move_create(Tmp, Src1, []),
		     NewI = hipe_sparc:alu_cc_src1_update(I, Tmp),
		     [Mov, NewI]
	       end;
	    false ->
	       unchanged
	 end;
      {_, true} ->
	 Src1 = hipe_sparc:alu_cc_src1(I),
	 case hipe_sparc:is_imm(Src1) of
	    true ->
	       case is_commutative(hipe_sparc:alu_cc_operator(I)) of
		  true ->
		     Src2 = hipe_sparc:alu_cc_src2(I),
		     I0 = hipe_sparc:alu_cc_src1_update(I, Src2),
		     [hipe_sparc:alu_cc_src2_update(I0, Src1)];
		  false ->
		     Tmp = hipe_sparc:mk_new_reg(),
		     Mov = hipe_sparc:move_create(Tmp, Src1, []),
		     NewI = hipe_sparc:alu_cc_src1_update(I, Tmp),
		     [Mov, NewI]
	       end;
	    false ->
	       unchanged
	 end;
      {false, false} ->
	 unchanged
   end.


is_commutative(Op) ->
   case Op of
      '+' -> true;
      'or' -> true;
      'and' -> true;
      'xor' -> true;
      _ -> false
   end.


split_instr(I) ->
   Uses = hipe_sparc:imm_uses(I),
   case big_constants(Uses) of
      [] -> unchanged;
      {Code, Subst} -> [hipe_sparc:subst(I, Subst) | Code]
   end.

big_constants([]) ->
    {[], []};
big_constants([V|Vs]) ->
    C = hipe_sparc:imm_value(V),
    case is_big(C) of
	true ->
	    NewVar = hipe_sparc:mk_new_reg(),
	    Low = low10(C),
	    Code =
		if Low =:= 0 ->
			[hipe_sparc:sethi_create(NewVar,hipe_sparc:mk_imm(high22(C)),[])];
		   true ->	     
			[hipe_sparc:alu_create(NewVar, NewVar, 'or', 
					  hipe_sparc:mk_imm(Low), []),
			 hipe_sparc:sethi_create(NewVar,hipe_sparc:mk_imm(high22(C)), 
					    [])]
		end,
	    {MoreCode, MoreSubst} = big_constants(Vs),
	    {Code++MoreCode, [{V, NewVar} | MoreSubst]};
	false ->
	    big_constants(Vs)
    end.

is_big(X) ->
   if X > 16#1fff ->
	 true;
      true ->
	 false
   end.

high22(X) -> X bsr 10.
low10(X) -> X band 16#3ff.


pp(Code,Fun) ->
  pp(Code,standard_io,Fun).

pp([{ET,EC},Rest],Dev,Fun) ->
  {M, F, A} = Fun,
  Fname = atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
  io:format(Dev, ".section    \".text\"~n", []),
  io:format(Dev, "    .align 4~n", []),
  io:format(Dev, "    .global ", []),
  io:format(Dev, "~s~n", [Fname]),
  io:format(Dev, "~s:~n", [Fname]),
  io:format(Dev, "! Entry (~w) ~n", [ET]),
  hipe_sparc:pp_instrs(EC, Dev, Fname),
  pp_rest(Rest,Fname,Dev),
  io:format(Dev, "~n~n", []).

pp_rest([{Type,Code}|Rest],Fname,Dev) ->
  io:format(Dev, "~n ! (~w) ~n~n", [Type]),
  hipe_sparc:pp_instrs(Code, Dev, Fname),
  pp_rest(Rest,Fname,Dev);
pp_rest([],_,_) -> true.
  
%
% A couple of functions that gives a common interface to 
% both b- and br- branches
%

%is_cond(I) ->
%   case hipe_sparc:type(I) of
%      br -> true;
%      b -> true;
%      _ -> false
%   end.


%cond_pred(I) ->
%   case hipe_sparc:type(I) of
%      br -> hipe_sparc:br_pred(I);
%      b -> hipe_sparc:b_pred(I)
%   end.
   

%cond_true_label(B) ->
%   case hipe_sparc:type(B) of
%      br -> hipe_sparc:br_true_label(B);
%      b -> hipe_sparc:b_true_label(B)
%   end.


%cond_false_label(B) ->
%   case hipe_sparc:type(B) of
%      br -> hipe_sparc:br_false_label(B);
%      b -> hipe_sparc:b_false_label(B)
%   end.
