%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%%
%%% Expand virtual gc tests to actual code.

-module(hipe_rtl_gctests).
-export([expand/1]).

expand(CFG) ->
   {LowLbl, HighLbl} = hipe_rtl_cfg:label_range(CFG),
   hipe_gensym:set_label(rtl,HighLbl),
   {LowVar, HighVar} = hipe_rtl_cfg:var_range(CFG),
   hipe_gensym:set_var(rtl,HighVar),
   Labels = hipe_rtl_cfg:labels(CFG),
   CFG1 = expand_gctests(Labels, CFG),
   CFG2 = hipe_rtl_cfg:label_range_update(CFG1, {LowLbl, hipe_gensym:get_label(rtl)}),
   hipe_rtl_cfg:var_range_update(CFG2, {LowVar, hipe_gensym:get_var(rtl)}).

expand_gctests([], CFG) ->
   CFG;
expand_gctests([L|Ls], CFG) ->
   BB = hipe_rtl_cfg:bb(CFG, L),
   Code = hipe_bb:code(BB),
   {NewCode, CFG0} = expand_gc(Code, CFG),
   CFG1 = hipe_rtl_cfg:bb_update(CFG0, L, hipe_bb:code_update(BB, NewCode)),
   expand_gctests(Ls, CFG1).

expand_gc([], CFG) ->
  {[], CFG};
expand_gc([I|Is], CFG) ->
  {CodeRest, CFG0} = expand_gc(Is, CFG),
  case hipe_rtl:type(I) of
    gctest ->  %% Two new blocks, The rest of this one, and one for the gc
      %% One
      ContLabel = hipe_rtl:label_name(hipe_rtl:mk_new_label()),
      CFG1 = hipe_rtl_cfg:bb_add(CFG0, ContLabel, hipe_bb:mk_bb(CodeRest)),
      %% Two
      %% Note: the pointer arithmetic is done in bytes,
      %% but the gc parameter is in words (tagged as fixnum).
      WordsNeeded = hipe_rtl:gctest_words(I),
      Tmp = hipe_rtl:mk_new_reg(),
      HP = hipe_rtl:mk_reg(hipe_rtl_arch:heap_pointer_reg()),
      H_LIMIT = hipe_rtl:mk_reg(hipe_rtl_arch:heap_limit_reg()),
      GCLabel = hipe_rtl:label_name(hipe_rtl:mk_new_label()),	 
   
      case hipe_rtl:is_reg(WordsNeeded) of
	
	true -> %Creates a gctest for a number of words known at runtime
	  HPAmount = hipe_rtl:mk_new_reg(),
	  GCAmount = hipe_rtl:mk_new_var(),
	  Code = [hipe_rtl:mk_alu(HPAmount, WordsNeeded, sll, hipe_rtl:mk_imm(2)),
		  hipe_rtl:mk_alu(Tmp, H_LIMIT, 'sub', HP),
		  hipe_rtl:mk_alu(GCAmount, WordsNeeded, sll, hipe_rtl:mk_imm(4)),
		  hipe_rtl:mk_alu(GCAmount, GCAmount, 'add', hipe_rtl:mk_imm(15)), 
		  hipe_rtl:mk_branch(Tmp, 'lt', HPAmount, GCLabel, ContLabel, 0.01)];
	false -> %Creates a gctest for a fixed number of words
	  GCAmount = hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(WordsNeeded)),
	  HPAmount = hipe_rtl:mk_imm(WordsNeeded*4),
	  %% Le grande finale
	  %% Do the GC overflow test. In the old version,
	  %%	if( (Tmp = HP+HPAmount) >= H_LIMIT ) goto GCLabel,
	  %% Tmp could wrap around. The new version below should be safe.
	  Code = [hipe_rtl:mk_alu(Tmp, H_LIMIT, 'sub', HP),
		  hipe_rtl:mk_branch(Tmp, 'lt', HPAmount, GCLabel, ContLabel, 0.01)]
      end,
      GCCode = [hipe_rtl:mk_call([], gc_1, [GCAmount], c, ContLabel, [])],
      CFG2 = hipe_rtl_cfg:bb_add(CFG1, GCLabel, hipe_bb:mk_bb(GCCode)),

      {Code, CFG2};
    _ ->
      {[I|CodeRest], CFG0}
  end.
