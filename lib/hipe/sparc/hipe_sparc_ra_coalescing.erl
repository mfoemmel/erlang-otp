%% -*- erlang-indent-level: 2 -*-
%%========================================================================
%%
%% Interface for register allocating SPARC code.  Uses hipe_regalloc.erl
%%
%%========================================================================

-module(hipe_sparc_ra_coalescing).

-export([alloc/2, lf_alloc/2]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").


lf_alloc(SparcCfg, Options) ->
   {_, SpillLimit} = hipe_sparc_cfg:var_range(SparcCfg),
   lf_alloc(SparcCfg, SpillLimit, Options).

lf_alloc(SparcCfg, SpillLimit, Options) ->
  ?inc_counter(ra_iteration_counter,1), 
 
   {Map, _NewSpillIndex} = 
    hipe_coalescing_regalloc:regalloc(SparcCfg, 0,
				      SpillLimit, hipe_sparc_specific),

  TempMap = hipe_temp_map:cols2tuple(Map, hipe_sparc_specific),

  {NewCfg, DontSpill} =
    hipe_sparc_ra_postconditions:rewrite(
      SparcCfg, TempMap, [], Options),

  case DontSpill of
    [] -> 
      %% Code to minimize stack size by allocation of temps to spillpositions
      {TempMap2, NewSpillIndex2} = 
	hipe_spill_minimize:stackalloc(NewCfg, [], 
				       0, Options, 
				       hipe_sparc_specific, 
				       TempMap),
      TempMap3 = hipe_spill_minimize:mapmerge(hipe_temp_map:to_substlist(TempMap), 
					      TempMap2),
      TempMap4 = hipe_temp_map:cols2tuple( TempMap3, hipe_sparc_specific),
          
      ?add_spills(Options, NewSpillIndex2),
      %%{SparcCfg2, NextPos} = hipe_sparc_caller_saves:rewrite(
      %%NewCfg, TempMap, NewSpillIndex, Options),
      %%{SparcCfg2, TempMap, NextPos};
      {NewCfg, TempMap4, NewSpillIndex2};
    _ -> 
      %% Since SpillLimit is used as a low-water-mark
      %% the list of temps not to spill is uninteresting.
      lf_alloc( NewCfg, SpillLimit, Options)
  end.



%%
%% Calls regalloc to rewrite the code after register allocation.
%%
%% Coloring is given as a list of tuples of the form:
%%     {Reg,{reg,NewReg}} or {Reg,{spill,SpillIndex}}.
%%

alloc(SparcCfg, Options) ->
  {_, SpillLimit} = hipe_sparc_cfg:var_range(SparcCfg),
  alloc(SparcCfg, unspilled, 0, SpillLimit, 0, Options).

alloc(SparcCfg, PrevSpillArea, SpillIndex, SpillLimit, Locs, Options) ->
  %% io:format("Allocating\n"),
  {Coloring, NewSpillIndex} = 
    hipe_coalescing_regalloc:regalloc(SparcCfg, SpillIndex,
				      SpillLimit, hipe_sparc_specific),
  %% io:format("Coloring ~p\n",[Coloring]),
  case SpillIndex < NewSpillIndex of
    false ->
      ColTuple = cols2tuple(Coloring),
      Labels = hipe_sparc_cfg:labels(SparcCfg),
      CFG0 = rewrite_bbs(Labels, SparcCfg, ColTuple),
      NewCFG = hipe_sparc_cfg:var_range_update(CFG0, {0, 31}),
      case SpillIndex  of
	0 ->
	  NewCFG;
	_ ->
	  ?add_spills(Options, NewSpillIndex),
	  NewConstTab =
	    hipe_consttab:update_block(hipe_sparc_cfg:data(NewCFG),
				       PrevSpillArea,
				       4,
				       word,
				       hipe_consttab:repeat(SpillIndex, 0)),
	  hipe_sparc_cfg:update_data(NewCFG, NewConstTab)
      end;
    true ->
      Labels = hipe_sparc_cfg:labels(SparcCfg),
      {Low, High} = hipe_sparc_cfg:var_range(SparcCfg),
      hipe_gensym:set_var(sparc,High),
      {CFG1, SpillArea} = 
	case PrevSpillArea of
	  unspilled ->
	    {NewCTab, SpillA} = 
	      hipe_consttab:insert_block(hipe_sparc_cfg:data(SparcCfg),
					 4, word, [0]),
	    {hipe_sparc_cfg:update_data(SparcCfg, NewCTab), SpillA};
	  SpillA ->
	    {SparcCfg, SpillA}
	end,
      
      Spills = spill_regs(Coloring),
      {CFG2, Locs0} = spill_rewrite_bbs(Labels, CFG1, Spills, SpillArea, Locs),
      CFG3 = hipe_sparc_cfg:var_range_update(CFG2,
					     {Low,hipe_gensym:get_var(sparc)}),
      alloc(CFG3, SpillArea, NewSpillIndex, SpillLimit,Locs0, Options)
  end.


%%
%% Returns a list of {SpilledReg, {Tmp, LoadInstr, StoreInstr}}
%%

spill_regs([]) ->
  [];
spill_regs([{_RegNr, {reg, _}} | Colors]) ->
  spill_regs(Colors);
spill_regs([{RegNr, {spill, SpillIndex}} | Colors]) ->
  SpillIndexImm = hipe_sparc:mk_imm(SpillIndex*4),
  Info = {hipe_sparc:mk_reg(RegNr), SpillIndexImm},
  [Info | spill_regs(Colors)].

%%
%% Rewrites a cfg where spills occured
%%

spill_rewrite_bbs([], CFG, _Spills, _SpillArea, Locs) ->
  {CFG, Locs};
spill_rewrite_bbs([Lbl|Lbls], CFG, Spills, SpillArea, Locs) ->
  BB = hipe_sparc_cfg:bb(CFG, Lbl),
  Code = hipe_bb:code(BB),
  {NewCode, Locs0} = spill_rewrite_instrs(Code, Spills, SpillArea),
  NewCFG = hipe_sparc_cfg:bb_update(CFG, Lbl, hipe_bb:code_update(BB, NewCode)),
  spill_rewrite_bbs(Lbls, NewCFG, Spills, SpillArea, Locs+Locs0).

spill_rewrite_instrs([], _Spills, _SpillArea) ->
  {[], 0};
spill_rewrite_instrs([I|Is], Spills, SpillArea) ->
  {NewI, Locs0} = spill_rewrite_instr(I, Spills, SpillArea),
  {NewIs, Locs1} = spill_rewrite_instrs(Is, Spills, SpillArea),
  {NewI++NewIs, Locs0+Locs1}.

spill_rewrite_instr(I, Spills, SpillArea) ->
  {Defs, Uses} = hipe_sparc:def_use(I),
  SpillUses = get_spills(Uses, Spills, Spills),
  SpillDefs = get_spills(Defs, Spills, Spills),
  case SpillUses++SpillDefs of
    [] ->
      {[I], 0};
	_ ->
      SpillAreaReg = hipe_sparc:mk_new_reg(),
      LA = hipe_sparc:load_address_create(SpillAreaReg,
					  SpillArea, constant, []),
      {Loads,UseSubst} = 
	case SpillUses of
	  [] ->
	    {[],[]};
	  [{R1u, Offset1u}] ->
	    Tmp1u = hipe_sparc:mk_new_reg(),
	    {[hipe_sparc:load_create(Tmp1u, uw, SpillAreaReg, Offset1u, [])],
	     [{R1u, Tmp1u}]};
	  [{R1u, Offset1u}, {R2u, Offset2u}] ->
	    Tmp1u = hipe_sparc:mk_new_reg(),
	    Tmp2u = hipe_sparc:mk_new_reg(),
	    {[hipe_sparc:load_create(Tmp1u, uw, SpillAreaReg, Offset1u, []),
	      hipe_sparc:load_create(Tmp2u, uw, SpillAreaReg, Offset2u, [])],
	     [{R1u, Tmp1u}, {R2u, Tmp2u}]}
	end,
      {Stores,DefSubst} = 
	case SpillDefs of
	  [] ->
	    {[],[]};
	  [{R1d, Offset1d}] ->
	    Tmp1d = hipe_sparc:mk_new_reg(),
	    {[hipe_sparc:store_create(SpillAreaReg, Offset1d, Tmp1d)],
	     [{R1d, Tmp1d}]}
	end,

      C1 = hipe_sparc:comment_create('** SPILL START **', []),
      C2 = hipe_sparc:comment_create('** SPILL END **', []),
      NewI = hipe_sparc:subst_defines(hipe_sparc:subst_uses(I, UseSubst), DefSubst),
      {[C1, LA] ++ Loads ++ [NewI] ++ Stores ++ [C2],
       length(UseSubst)+length(DefSubst)}
  end.


get_spills([], _, _) ->
  [];
get_spills([_|Rs], [], Spills) ->
  get_spills(Rs, Spills, Spills);
get_spills([R|Rs], [{R, Info}|_], Spills) ->
  [{R, Info} | get_spills(Rs, Spills, Spills)];
get_spills(Rs, [_|Ss], Spills) ->
  get_spills(Rs, Ss, Spills).


%%
%% Converts a list of [{R1, C1}, {R2, C2}, ...} to a tuple {C17, C23, ...}.
%%
%% The N's must be unique but do not have to be sorted and they can be sparse.
%%

cols2tuple(Map) ->
  hipe_temp_map:cols2tuple(Map, hipe_sparc_specific).


%%
%% Rewrite a cfg to use the allocated registers
%%

rewrite_bbs([], CFG, _ColTuple) ->
  CFG;
rewrite_bbs([Lbl|Lbls], CFG, ColTuple) ->
  BB = hipe_sparc_cfg:bb(CFG, Lbl),
  Code = hipe_bb:code(BB),
  NewCode = rewrite_instrs(Code, ColTuple),
  NewCFG = hipe_sparc_cfg:bb_update(CFG, Lbl, hipe_bb:code_update(BB,NewCode)),
  rewrite_bbs(Lbls, NewCFG, ColTuple).

rewrite_instrs([], _ColTuple) ->
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
    _ -> Ins
  end.

color_arg(Arg, ColTuple) ->
  %% io:format("Color_arg: ~p ~p\n", [Arg,ColTuple]),
   case hipe_sparc:is_reg(Arg) of
     true ->
       case hipe_temp_map:find(hipe_sparc:reg_nr(Arg), ColTuple) of
	 {reg, NewRgNr} ->
	   hipe_sparc:mk_reg(NewRgNr);
	 {spill, _SpillIndex} ->
	   exit({sparc, spilled})
       end;
     false ->
       Arg
   end.
