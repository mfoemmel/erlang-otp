%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface for register allocating SPARC code.  Uses hipe_regalloc.erl
%%

-module(hipe_sparc_ra_ls).

-export([alloc/2, cols2tuple/1, rewrite/2]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

%%-----------------------------------------------------------------------

%
% Calls regalloc, rewrite the code after register allocation.
%
% Coloring are given as a list of {Reg, {reg, NewReg}} or 
% {Reg, {spill, SpillIndex}}.
%

alloc(SparcCfg, Options) ->
  T0 = hipe_sparc_registers:temp0(),       
  T1 = hipe_sparc_registers:temp1(),
  {Coloring, NewSpillIndex} = 
    hipe_ls_regalloc:regalloc(SparcCfg,
			      hipe_sparc_registers:allocatable() -- 
			      %% The temps are used for spills
			      ([T0,T1] ++
			       hipe_sparc_registers:fixed()),
			      [hipe_sparc_cfg:start(SparcCfg)] ++ 
			      hipe_sparc_cfg:fail_entrypoints(SparcCfg),
			      0,
			      hipe_sparc_specific:number_of_temporaries(SparcCfg), 
			      Options,
			      hipe_sparc_specific),

  case NewSpillIndex > 0 of
    false ->
      ColTuple = cols2tuple(Coloring),
      Labels = hipe_sparc_cfg:labels(SparcCfg),
      rewrite(SparcCfg,ColTuple);
    true ->
      ?add_spills(Options, NewSpillIndex),

      ColTuple = cols2tuple(Coloring),
      %% io:format("Coloring ~w\n",[ColTuple]),

      Labels = hipe_sparc_cfg:labels(SparcCfg),
      {NewConstTab, SpillArea} = 
	hipe_consttab:insert_block(hipe_sparc_cfg:data(SparcCfg), 4, word, 
				   hipe_consttab:repeat(NewSpillIndex+1,0)),
      Spills = spill_regs(Coloring),
      SpillCfg = spill_rewrite_bbs(
		   Labels, 
		   hipe_sparc_cfg:update_data(SparcCfg,NewConstTab),
		   Spills, 
		   SpillArea, ColTuple),



      rewrite(SpillCfg, ColTuple)
  end.

rewrite(SpillCfg, ColTuple) ->
  Labels = hipe_sparc_cfg:labels(SpillCfg),
  CFG0 = rewrite_bbs(Labels, SpillCfg, ColTuple),
  NewCFG = hipe_sparc_cfg:var_range_update(CFG0, {0, 31}),
  NewCFG.


%
% Returns a list of {SpilledReg, {Tmp, LoadInstr, StoreInstr}}
%

spill_regs([]) ->
   [];
spill_regs([{RegNr, {reg, _}} | Colors]) ->
   spill_regs(Colors);
spill_regs([{RegNr, {spill, SpillIndex}} | Colors]) ->
   SpillIndexImm = hipe_sparc:mk_imm(SpillIndex*4),
   Info = {hipe_sparc:mk_reg(RegNr), SpillIndexImm},
   [Info | spill_regs(Colors)].



%
% Rewrite a cfg where spills occured
%

spill_rewrite_bbs([], CFG, Spills, SpillArea, Coloring) ->
  CFG;
spill_rewrite_bbs([Lbl|Lbls], CFG, Spills, SpillArea, Coloring) ->
  BB = hipe_sparc_cfg:bb(CFG, Lbl),
  Code = hipe_bb:code(BB),
  NewCode = spill_rewrite_instrs(Code, Spills,
				  SpillArea, [], Coloring),
  NewCFG = hipe_sparc_cfg:bb_update(CFG, Lbl, hipe_bb:code_update(BB, NewCode)),
  spill_rewrite_bbs(Lbls, NewCFG, Spills, SpillArea, Coloring).

spill_rewrite_instrs([], Spills, SpillArea,Acc, Coloring) ->
  lists:reverse(Acc);
spill_rewrite_instrs([I|Is], Spills, SpillArea, Acc, Coloring) ->
  case hipe_sparc:type(I) of 
    multimove ->
      {Defs, Uses} = hipe_sparc:def_use(I),
      SpillUses = get_spills(Uses, Spills, Spills),
      SpillDefs = get_spills(Defs, Spills, Spills),
      case SpillUses++SpillDefs of
	[] ->
	  spill_rewrite_instrs(Is, Spills, SpillArea,[I|Acc], Coloring);
	Spilled ->
	  NewIs = hipe_sparc_multimove:rewrite_mm(I, Coloring, SpillArea, 
			     hipe_sparc:mk_reg(hipe_sparc_registers:temp0()),
			     hipe_sparc:mk_reg(hipe_sparc_registers:temp1()),
			     Acc),
	  spill_rewrite_instrs(Is, Spills, SpillArea, NewIs, Coloring)
      end;

    _ -> 
      NewIs = spill_rewrite_instr(I, Spills, SpillArea,Acc),
      spill_rewrite_instrs(Is, Spills, SpillArea, NewIs, Coloring)
  end.






spill_rewrite_instr(I, Spills, SpillArea,Acc) ->
  {Defs, Uses} = hipe_sparc:def_use(I),
  SpillUses = get_spills(Uses, Spills, Spills),
  SpillDefs = get_spills(Defs, Spills, Spills),
  case SpillUses++SpillDefs of
    [] ->
      [I|Acc];
    _ ->
      
      SpillAreaReg = hipe_sparc:mk_reg(hipe_sparc_registers:temp0()),
      LA = hipe_sparc:load_address_create(SpillAreaReg, SpillArea, constant, []),
      {Loads,
       UseSubst,
       SpillAreaRegOwerwritten} = 
	case SpillUses of
	  [] -> {[],[],false};
	[{R1u, Offset1u}] ->
	  Tmp1u = hipe_sparc:mk_reg(hipe_sparc_registers:temp1()),
	    {[hipe_sparc:load_create(Tmp1u, uw, SpillAreaReg, Offset1u, [])],
	     [{R1u, Tmp1u}],
	     false};
	[{R1u, Offset1u}, {R2u, Offset2u}] ->
	  Tmp1u = hipe_sparc:mk_reg(hipe_sparc_registers:temp1()),
	  Tmp2u = SpillAreaReg,
	   
	    {[hipe_sparc:load_create(Tmp2u, uw, SpillAreaReg, Offset2u, []),
	      hipe_sparc:load_create(Tmp1u, uw, SpillAreaReg, Offset1u, [])
	     ],
	     [{R1u, Tmp1u}, {R2u, Tmp2u}],
	     true}
      end,
      {Stores,DefSubst} =
	case SpillDefs of
	  [] ->
	    {[], []};
	[{R1d, Offset1d}] ->
	  
	  Tmp1d = hipe_sparc:mk_reg(hipe_sparc_registers:temp1()),
	    {[hipe_sparc:store_create(SpillAreaReg, Offset1d, uw, Tmp1d,
				      []) |
	      if SpillAreaRegOwerwritten =:= true ->
		  [LA];
		 true ->
		  []
	      end],
	     [{R1d, Tmp1d}]}
	end,
      C1 = hipe_sparc:comment_create('** SPILL START **', []),
      C2 = hipe_sparc:comment_create('** SPILL END **', []),
      NewI = hipe_sparc:subst_defines(hipe_sparc:subst_uses(I, UseSubst), DefSubst),
      


      %% We reverse the list of instructions in the end, so lets create
      %%  them reversed now.
      Stores ++ [NewI|Loads] ++ [LA, C1] ++ Acc
  end.



get_spills([], _, _) ->
   [];
get_spills([R|Rs], [], Spills) ->
   get_spills(Rs, Spills, Spills);
get_spills([R|Rs], [{R, Info}|_], Spills) ->
   [{R, Info} | get_spills(Rs, Spills, Spills)];
get_spills(Rs, [R|Ss], Spills) ->

   get_spills(Rs, Ss, Spills).



cols2tuple(Map) ->
  hipe_temp_map:cols2tuple(Map,hipe_sparc_specific).

%
% Rewrite a cfg to use the allocated registers
%

rewrite_bbs([], CFG, ColTuple) ->
   CFG;
rewrite_bbs([Lbl|Lbls], CFG, ColTuple) ->
   BB = hipe_sparc_cfg:bb(CFG, Lbl),
   Code = hipe_bb:code(BB),
   NewCode = rewrite_instrs(Code, ColTuple),
   NewCFG = hipe_sparc_cfg:bb_update(CFG, Lbl, hipe_bb:code_update(BB, NewCode)),
   rewrite_bbs(Lbls, NewCFG, ColTuple).


rewrite_instrs([], ColTuple) ->
   [];
rewrite_instrs([I|Is], ColTuple) ->
   [rewrite_instr(I, ColTuple) | rewrite_instrs(Is, ColTuple)].


rewrite_instr(Ins, ColTuple) ->
   case hipe_sparc:type(Ins) of
      label -> Ins;
      nop -> Ins;
      block -> Ins;
      align -> Ins;
      comment -> Ins;
      b -> Ins;
      goto -> Ins;
      move ->
	 NewDst = color_arg(hipe_sparc:move_dest(Ins), ColTuple),
	 NewSrc = color_arg(hipe_sparc:move_src(Ins), ColTuple),
	 hipe_sparc:move_dest_update(hipe_sparc:move_src_update(Ins, NewSrc), NewDst);
      cmov_cc ->
	 NewDst = color_arg(hipe_sparc:cmov_cc_dest(Ins), ColTuple),
	 NewSrc = color_arg(hipe_sparc:cmov_cc_src(Ins), ColTuple),
	 I0 = hipe_sparc:cmov_cc_src_update(Ins, NewSrc),
	 hipe_sparc:cmov_cc_dest_update(I0, NewDst);
      cmov_r ->
	 NewDst = color_arg(hipe_sparc:cmov_r_dest(Ins), ColTuple),
	 NewSrc = color_arg(hipe_sparc:cmov_r_src(Ins), ColTuple),
	 NewReg = color_arg(hipe_sparc:cmov_r_reg(Ins), ColTuple),
	 I0 = hipe_sparc:cmov_r_dest_update(Ins, NewDst),
	 I1 = hipe_sparc:cmov_r_src_update(I0, NewSrc),
	 hipe_sparc:cmov_r_reg_update(I1, NewReg);
      alu ->
	 NewSrc1 = color_arg(hipe_sparc:alu_src1(Ins), ColTuple),
	 NewSrc2 = color_arg(hipe_sparc:alu_src2(Ins), ColTuple),
	 NewDst = color_arg(hipe_sparc:alu_dest(Ins), ColTuple),
	 I0 = hipe_sparc:alu_src1_update(Ins, NewSrc1),
	 I1 = hipe_sparc:alu_src2_update(I0, NewSrc2),
	 hipe_sparc:alu_dest_update(I1, NewDst);
      alu_cc ->
	 NewSrc1 = color_arg(hipe_sparc:alu_cc_src1(Ins), ColTuple),
	 NewSrc2 = color_arg(hipe_sparc:alu_cc_src2(Ins), ColTuple),
	 NewDst = color_arg(hipe_sparc:alu_cc_dest(Ins), ColTuple),
	 I0 = hipe_sparc:alu_cc_src1_update(Ins, NewSrc1),
	 I1 = hipe_sparc:alu_cc_src2_update(I0, NewSrc2),
	 hipe_sparc:alu_cc_dest_update(I1, NewDst);
      sethi ->
	 NewDest = color_arg(hipe_sparc:sethi_dest(Ins), ColTuple),
	 hipe_sparc:sethi_dest_update(Ins, NewDest);

      load ->
	 NewDest = color_arg(hipe_sparc:load_dest(Ins), ColTuple),
	 NewSrc = color_arg(hipe_sparc:load_src(Ins), ColTuple),
	 NewOff = color_arg(hipe_sparc:load_off(Ins), ColTuple),
	 I0 = hipe_sparc:load_dest_update(Ins, NewDest),
	 I1 = hipe_sparc:load_src_update(I0, NewSrc),
	 hipe_sparc:load_off_update(I1, NewOff);
      store ->
	 NewDest = color_arg(hipe_sparc:store_dest(Ins), ColTuple),
	 NewSrc = color_arg(hipe_sparc:store_src(Ins), ColTuple),
	 NewOff = color_arg(hipe_sparc:store_off(Ins), ColTuple),
	 I0 = hipe_sparc:store_dest_update(Ins, NewDest),
	 I1 = hipe_sparc:store_src_update(I0, NewSrc),
	 hipe_sparc:store_off_update(I1, NewOff);
      br ->
	 NewReg = color_arg(hipe_sparc:br_reg(Ins), ColTuple),
	 hipe_sparc:br_reg_update(Ins, NewReg);
      %% Warning, not complete
      jmp_link ->
	 NewTarget = color_arg(hipe_sparc:jmp_link_target(Ins), ColTuple),
	 NewOff = color_arg(hipe_sparc:jmp_link_off(Ins), ColTuple),
	 I0 = hipe_sparc:jmp_link_target_update(Ins, NewTarget),
	 hipe_sparc:jmp_link_off_update(I0, NewOff);
      jmp ->
	 NewTarget = color_arg(hipe_sparc:jmp_target(Ins), ColTuple),
	 NewOff = color_arg(hipe_sparc:jmp_off(Ins), ColTuple),
	 I0 = hipe_sparc:jmp_target_update(Ins, NewTarget),
	 hipe_sparc:jmp_off_update(I0, NewOff);
      call_link ->
         Ins1 =
         case hipe_sparc:call_link_type(Ins) of
	   closure ->
	     NewTarget = color_arg(hipe_sparc:call_link_target(Ins), ColTuple),
	     hipe_sparc:call_link_target_update(Ins, NewTarget);
	   _ -> Ins
	 end,
	 NewLink = color_arg(hipe_sparc:call_link_link(Ins1), ColTuple),
	 hipe_sparc:call_link_link_update(Ins1, NewLink);
      %% end warning
      load_atom ->
	 NewDest = color_arg(hipe_sparc:load_atom_dest(Ins), ColTuple),

	 hipe_sparc:load_atom_dest_update(Ins, NewDest);
      load_address ->
	 NewDest = color_arg(hipe_sparc:load_address_dest(Ins), ColTuple),
	 hipe_sparc:load_address_dest_update(Ins, NewDest);
      multimove ->
	 NewDst = color_all_args(hipe_sparc:move_dest(Ins), ColTuple),
	 NewSrc = color_all_args(hipe_sparc:move_src(Ins), ColTuple),
	 hipe_sparc:multimove_dest_update(hipe_sparc:multimove_src_update(Ins, NewSrc), NewDst);
      _ -> 
       %% If we don't know how to handle this instruction do a generic substitution
       hipe_sparc:subst(Ins, hipe_temp_map:to_substlist(ColTuple))

   end.



color_arg(Arg, ColTuple) ->
   case hipe_sparc:is_reg(Arg) of
      true ->
	 case hipe_temp_map:find(hipe_sparc:reg_nr(Arg), ColTuple) of
	    {reg, NewRgNr} ->
	       hipe_sparc:mk_reg(NewRgNr);
	    {spill, SpillIndex} ->
	       exit({sparc, spilled})
	 end;
      false ->
	 Arg
   end.

color_all_args([Arg|Args], ColTuple) ->
  [color_arg(Arg, ColTuple)|color_all_args(Args, ColTuple)];
color_all_args([], _ ) -> [].




