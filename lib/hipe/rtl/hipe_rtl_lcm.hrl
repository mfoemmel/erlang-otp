%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% GENERAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Returns true if the list of registers only contains virtual registers and
%% no machine registers. 
no_machine_regs([]) ->
  true;
no_machine_regs([{rtl_reg, N}|Regs]) ->
  (N >= hipe_rtl_arch:first_virtual_reg()) andalso no_machine_regs(Regs);
no_machine_regs([{rtl_fpreg, N}|Regs]) ->
  (N >= hipe_rtl_arch:first_virtual_reg()) andalso no_machine_regs(Regs);
no_machine_regs([_|Regs]) ->
  no_machine_regs(Regs).

%%=============================================================================
%% Returns true if an RTL instruction is an expression.
%%
is_expr(I) ->
  Defines = hipe_rtl:defines(I),
  Uses = hipe_rtl:uses(I),
  
  %% We don't cosider something that doesn't define anything as an expression.
  %% Also we don't consider machine registers to be expressions.
  case length(Defines) > 0 andalso no_machine_regs(Defines)
    andalso no_machine_regs(Uses) of 
    true ->
      case hipe_rtl:type(I) of
        alu -> true;
%   	alu ->
%    	  Dst = hipe_rtl:alu_dst(I),
%    	  Src1 = hipe_rtl:alu_src1(I),
%    	  Src2 = hipe_rtl:alu_src2(I),

   	  %% Check if dest updates src
%    	  case Dst == Src1 orelse Dst == Src2 of
%    	    true ->
%    	      false;
%    	    false ->
%    	      true
%    	  end;

	  %% Check if alu expression is untagging of boxed (rX <- vX sub 2)
% 	  case hipe_rtl:is_reg(Dst) andalso hipe_rtl:is_var(Src1) andalso 
% 	    (hipe_rtl:alu_op(I) == sub) andalso hipe_rtl:is_imm(Src2) of
% 	    true ->
% 	      case hipe_rtl:imm_value(Src2) of
% 		2 -> false; %% Tag for boxed. TODO: Should not be hardcoded...
% 		_ -> true
% 	      end;
% 	    false ->
% 	      true
% 	  end;
	       
        alub -> false; %% TODO: Split instruction to consider alu expression?
	binbase -> false;
        branch -> false;
        call -> false; %% We cannot prove that a call has no side-effects
        comment -> false;
        enter -> false;
        %fail_to -> false; %% Deprecated?
        fconv -> true;
        fixnumop -> true;
        fload -> true;
        fmove -> false;
        fp -> true;
        fp_unop -> true;
        fstore -> false;
        goto -> false;
        goto_index -> false;
        gctest -> false;
        label -> false;
        load -> true;
        load_address ->
	  case hipe_rtl:load_address_type(I) of
	    c_const -> false;
	    closure -> false;	%% not sure whether safe to move; 
	                        %% also probably not worth it
	    constant -> true
	  end;
        load_atom -> true;
        load_word_index -> true;
        move -> false;
        multimove -> false;
        phi -> false;
        %restore_catch -> false; %% Deprecated?
        return -> false;
        store -> false;
        switch -> false
      end;
    false->
      false
  end.


%%=============================================================================
%% Replaces destination of rtl expression with empty list.
%% 
expr_set_dst(I, [Dst|Dsts]) ->
  case hipe_rtl:type(I) of
    alu -> hipe_rtl:alu_dst_update(I, Dst);
    call -> hipe_rtl:call_dstlist_update(I, [Dst|Dsts]);
    fconv -> hipe_rtl:fconv_dst_update(I, Dst);
    fixnumop -> hipe_rtl:fixnumop_dst_update(I, Dst);
    fload -> hipe_rtl:fload_dst_update(I, Dst);
    %% fmove -> hipe_rtl:fmove_dst_update(I, Dst);
    fp -> hipe_rtl:fp_dst_update(I, Dst);
    fp_unop -> hipe_rtl:fp_unop_dst_update(I, Dst);
    load -> hipe_rtl:load_dst_update(I, Dst);
    load_address -> hipe_rtl:load_address_dst_update(I, Dst);
    load_atom -> hipe_rtl:load_atom_dst_update(I, Dst);
    load_word_index -> hipe_rtl:load_word_index_dst_update(I, Dst);
    %% move -> hipe_rtl:move_dst_update(I, Dst);
    _ -> exit({?MODULE,expr_set_dst, {"bad expression"}})
  end.

%%=============================================================================
%% Replaces destination of rtl expression with empty list.
%% 
expr_clear_dst(I) ->
  case hipe_rtl:type(I) of
    alu -> hipe_rtl:alu_dst_update(I, nil);
    call -> hipe_rtl:call_dstlist_update(I, nil);
    fconv -> hipe_rtl:fconv_dst_update(I, nil);
    fixnumop -> hipe_rtl:fixnumop_dst_update(I, nil);
    fload -> hipe_rtl:fload_dst_update(I, nil);
    %% fmove -> hipe_rtl:fmove_dst_update(I, nil);
    fp -> hipe_rtl:fp_dst_update(I, nil);
    fp_unop -> hipe_rtl:fp_unop_dst_update(I, nil);
    load -> hipe_rtl:load_dst_update(I, nil);
    load_address -> hipe_rtl:load_address_dst_update(I, nil);
    load_atom -> hipe_rtl:load_atom_dst_update(I, nil);
    load_word_index -> hipe_rtl:load_word_index_dst_update(I, nil);
    %% move -> hipe_rtl:move_dst_update(I, nil);
    _ -> exit({?MODULE,expr_clear_dst, {"bad expression"}})

  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% PRECALC FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Pre-calculates the flow analysis and puts the calculated sets in maps for 
%% easy access later.
lcm_precalc(CFG, Options) ->
  %% Calculate use map and expression map.
  ?option_time({ExprMap, IdMap} = mk_expr_map(CFG), 
	       "RTL LCM mk_expr_map", Options),
  ?option_time(UseMap = mk_use_map(CFG, ExprMap), 
	       "RTL LCM mk_use_map", Options),
  %% Labels = hipe_rtl_cfg:reverse_postorder(CFG),
  Labels = hipe_rtl_cfg:labels(CFG),
  %% StartLabel = hipe_rtl_cfg:start_label(CFG),
  %% AllExpr = all_exprs(CFG, Labels),
  AllExpr = ?SETS:from_list(gb_trees:keys(IdMap)),

  %% Calculate the data sets.
  ?option_time(NodeInfo0 = mk_node_info(Labels), "RTL LCM mk_node_info", 
	       Options),
  %% ?option_time(EdgeInfo0 = mk_edge_info(), "RTL LCM mk_edge_info", 
  %%  	          Options),
  EdgeInfo0 = mk_edge_info(),
  ?option_time(NodeInfo1 = calc_up_exp(CFG, ExprMap, NodeInfo0, Labels), 
	       "RTL LCM calc_up_exp", Options),
  ?option_time(NodeInfo2 = calc_down_exp(CFG, ExprMap, NodeInfo1, Labels), 
	       "RTL LCM calc_down_exp", Options),
  ?option_time(NodeInfo3 = calc_killed_expr(CFG, NodeInfo2, UseMap, AllExpr, 
					   Labels), 
	       "RTL LCM calc_killed_exp", Options),
  ?option_time(NodeInfo4 = calc_avail(CFG, NodeInfo3), 
	       "RTL LCM calc_avail", Options),
  ?option_time(NodeInfo5 = calc_antic(CFG, NodeInfo4, AllExpr), 
	       "RTL LCM calc_antic", Options),

  %% Earliest is calculated when needed during calc_later instead.
  ?option_time(EdgeInfo1 = calc_earliest(CFG, NodeInfo5, EdgeInfo0, 
  					 AllExpr, Labels), 
  	       "RTL LCM calc_earliest", Options),
  %%EdgeInfo1 = EdgeInfo0,

  ?option_time({NodeInfo6, EdgeInfo2} = calc_later(CFG, NodeInfo5, EdgeInfo1,
						   AllExpr), 
	       "RTL LCM calc_later", Options),
  ?option_time(NodeInfo7 = calc_delete(CFG, NodeInfo6, Labels), 
	       "RTL LCM calc_delete", Options),
  {NodeInfo7, EdgeInfo2, AllExpr, ExprMap, IdMap, Labels}.

%%%%%%%%%%%%%%%%%%% AVAILABLE IN/OUT FLOW ANALYSIS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fixpoint calculation of anticipated in/out sets.
%% Uses a worklist algorithm.
%% Performs the avail in/out flow analysis.

%%=============================================================================
%% Calculates the available in/out sets, and returns an updated NodeInfo.

calc_avail(CFG, NodeInfo) ->
  StartLabel = hipe_rtl_cfg:start_label(CFG),
  Work = init_work([StartLabel]),
  %% Initialize start node
  NewNodeInfo = set_avail_in(NodeInfo, StartLabel, ?SETS:new()),
  calc_avail_fixpoint(Work, CFG, NewNodeInfo).

calc_avail_fixpoint(Work, CFG, NodeInfo) ->
  case get_work(Work) of
    fixpoint ->
      NodeInfo;
    {Label, NewWork} ->
      {NewNodeInfo, NewLabels} = calc_avail_node(Label, CFG, NodeInfo),
      NewWork2 = add_work(NewWork, NewLabels),
      calc_avail_fixpoint(NewWork2, CFG, NewNodeInfo)
  end.

calc_avail_node(Label, CFG, NodeInfo) ->
  %% Get avail in
  AvailIn = avail_in(NodeInfo, Label),

  %% Calculate avail out
  AvailOut = ?SETS:union(down_exp(NodeInfo, Label),
                           ?SETS:subtract(AvailIn,
					      killed_expr(NodeInfo, Label))),

  {Changed, NodeInfo2} = 
    case avail_out(NodeInfo, Label) of
      none ->
	%% If there weren't any old avail out we use this one.
	{true, set_avail_out(NodeInfo, Label, AvailOut)};
      OldAvailOut ->
	%% Check if the avail outs are equal.
	case AvailOut == OldAvailOut of
	  true ->
	    {false, NodeInfo};
	  false ->
	    {true, set_avail_out(NodeInfo, Label, AvailOut)}
	end
    end,

  case Changed of
    true ->
      %% Update AvailIn-sets of successors and add them to worklist
      Succs = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), Label),
      NodeInfo3 =
	lists:foldl
	  (fun(Succ, NewNodeInfo) ->
	       case avail_in(NewNodeInfo, Succ) of
		 none ->
		   %% Initialize avail in to all expressions
		   set_avail_in(NewNodeInfo, Succ, AvailOut);

		 OldAvailIn ->
		   set_avail_in(NewNodeInfo, Succ, 
				?SETS:intersection(OldAvailIn, AvailOut))
	       end
	   end,
	   NodeInfo2, Succs),
      {NodeInfo3, Succs};

    false ->
      {NodeInfo2, []}
  end.

%%%%%%%%%%%%%%%%%% ANTICIPATED IN/OUT FLOW ANALYSIS  %%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fixpoint calculation of anticipated in/out sets.
%% Uses a worklist algorithm.

%%=============================================================================
%% Calculates the anicipated in/out sets, and returns an updated NodeInfo.
calc_antic(CFG, NodeInfo, AllExpr) ->
  % Initialize worklist with all nodes in postorder
  Labels = hipe_rtl_cfg:postorder(CFG),
  Work = init_work(Labels),

  %% Initialize start node
%  NewNodeInfo = set_antic_in(NodeInfo, StartLabel, ?SETS:new()),
%  calc_antic_fixpoint(Work, CFG, NewNodeInfo, AllExpr).
  calc_antic_fixpoint(Work, CFG, NodeInfo, AllExpr).

calc_antic_fixpoint(Work, CFG, NodeInfo, AllExpr) ->
  case get_work(Work) of
    fixpoint ->
      NodeInfo;
    {Label, NewWork} ->
      {NewNodeInfo, NewLabels} = calc_antic_node(Label, CFG, NodeInfo, AllExpr),
      NewWork2 = add_work(NewWork, NewLabels),
      calc_antic_fixpoint(NewWork2, CFG, NewNodeInfo, AllExpr)
  end.

calc_antic_node(Label, CFG, NodeInfo, AllExpr) ->
  %% Get antic out
  AnticOut = 
    case antic_out(NodeInfo, Label) of
      none -> 
	case is_exit_label(CFG, Label) of 
	  true ->
	    ?SETS:new();
	  false ->
	    AllExpr
	end;
      
      AnticOutTemp -> AnticOutTemp
    end,

  %% Calculate antic in
  AnticIn = ?SETS:union(up_exp(NodeInfo, Label),
			  ?SETS:subtract(AnticOut,
					     killed_expr(NodeInfo, Label))),

  {Changed, NodeInfo2} = 
    case antic_in(NodeInfo, Label) of
      %% If there weren't any old antic in we use this one.
      none ->
	{true, set_antic_in(NodeInfo, Label, AnticIn)};

      OldAnticIn ->
	%% Check if the antic in:s are equal.
	case AnticIn == OldAnticIn of
	  true ->
	    {false, NodeInfo};
	  false ->
	    {true, 
	     set_antic_in(NodeInfo, Label, AnticIn)}
	end
    end,

  case Changed of
    true ->
      %% Update AnticOut-sets of predecessors and add them to worklist
      Preds = hipe_rtl_cfg:pred(hipe_rtl_cfg:pred_map(CFG), Label),
      NodeInfo3 =
	lists:foldl
	  (fun(Pred, NewNodeInfo) ->
	       case antic_out(NewNodeInfo, Pred) of
		 none ->
		   %% Initialize antic out to all expressions
		   set_antic_out(NewNodeInfo, Pred, AnticIn);

		 OldAnticOut ->
		   set_antic_out(NewNodeInfo, Pred, 
				 ?SETS:intersection(OldAnticOut, AnticIn))
	       end
	   end,
	   NodeInfo2, Preds),
      {NodeInfo3, Preds};

    false ->
      {NodeInfo2, []}
  end.

%%%%%%%%%%%%%%%%%%%%% LATER / LATER IN FLOW ANALYSIS %%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fixpoint calculations of Later and LaterIn sets. 
%% Uses a worklist algorithm.
%% Note that the Later set is calculated on edges.

%%=============================================================================
%% Calculates the Later and LaterIn sets, and returns updates of both 
%% NodeInfo (with LaterIn sets) and EdgeInfo (with Later sets).
calc_later(CFG, NodeInfo, EdgeInfo, AllExpr) ->
  StartLabel = hipe_rtl_cfg:start_label(CFG),
  Work = init_work([{node, StartLabel}]),

  %% Initialize start node
  NewNodeInfo = set_later_in(NodeInfo, StartLabel, ?SETS:new()), 

  calc_later_fixpoint(Work, CFG, NewNodeInfo, EdgeInfo, AllExpr).

calc_later_fixpoint(Work, CFG, NodeInfo, EdgeInfo, AllExpr) ->
  case get_work(Work) of
    {{edge, From, To}, Work2} ->
      {NewNodeInfo, NewEdgeInfo, AddWork} = 
	calc_later_edge(From, To, CFG, NodeInfo, EdgeInfo, AllExpr),
      Work3 = add_work(Work2, AddWork),
      calc_later_fixpoint(Work3, CFG, NewNodeInfo, NewEdgeInfo, AllExpr);

    {{node, Label}, Work2} ->
      AddWork = calc_later_node(Label, CFG),
      Work3 = add_work(Work2, AddWork),
      calc_later_fixpoint(Work3, CFG, NodeInfo, EdgeInfo, AllExpr);

    fixpoint ->
      {NodeInfo, EdgeInfo}
  end.

calc_later_node(Label, CFG) ->
  Succs = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), Label),
  lists:map((fun(Succ) -> {edge, Label, Succ} end), Succs).

calc_later_edge(From, To, _CFG, NodeInfo, EdgeInfo, AllExpr) ->  
  %% Instead of pre-calculaing earliest, we calculate it when needed here
%%  {Earliest, EdgeInfo1} = 
%%    case lookup_earliest(EdgeInfo, {From, To}) of
%%      none ->
%%	IsStartLabel = (From == hipe_rtl_cfg:start_label(_CFG)),
%%	CalcEarliest = calc_earliest_edge(NodeInfo, AllExpr, IsStartLabel,
%%					  From, To),
%%	{CalcEarliest, set_earliest(EdgeInfo, {From, To}, CalcEarliest)};
%%
%%      TempEarliest ->
%%	{TempEarliest, EdgeInfo}
%%    end,
  Earliest = earliest(EdgeInfo, {From, To}),
  EdgeInfo1 = EdgeInfo,
  
  LaterIn = later_in(NodeInfo, From),
  InvUpExp = ?SETS:subtract(AllExpr, up_exp(NodeInfo, From)),
  Later = ?SETS:union(Earliest, ?SETS:intersection(LaterIn, InvUpExp)),
  
  {Changed, EdgeInfo2} =
    case lookup_later(EdgeInfo1, {From, To}) of 
      none ->
	{true, set_later(EdgeInfo1, {From, To}, Later)};

      %% FIXME use cases "Later -> " and "OldLater ->" instead of == ?
      OldLater ->
	case Later == OldLater of
	  true ->
	    {false, EdgeInfo1};
	  false ->
	    {true, set_later(EdgeInfo1, {From, To}, Later)}
	end
    end,
  
  case Changed of 
    true ->
      %% Update later in set of To-node
      case lookup_later_in(NodeInfo, To) of
	%% If the data isn't set initialize to all expressions
	none ->
 	  {set_later_in(NodeInfo, To, Later),
	   EdgeInfo2, [{node, To}]};

	OldLaterIn ->
	  NewLaterIn = ?SETS:intersection(OldLaterIn, Later),
	  %% Check if something changed
	  %% FIXME: Implement faster equality test?
	  case NewLaterIn == OldLaterIn of 
	    true ->
	      {NodeInfo, EdgeInfo2, []};
	    false ->
	      {set_later_in(NodeInfo, To, NewLaterIn),
	       EdgeInfo2, [{node, To}]}
	  end
      end;

    false ->
      {NodeInfo, EdgeInfo2, []}
  end.

%%%%%%%%%%%%%%%%%% UPWARDS/DOWNWARDS EXPOSED EXPRESSIONS %%%%%%%%%%%%%%%%%%%%%%
%% Calculates upwards and downwards exposed expressions.

%%=============================================================================
%% Calculates the downwards exposed expression sets for the given labels in
%% the CFG.
calc_down_exp(_, _, NodeInfo, []) ->
  NodeInfo;
calc_down_exp(CFG, ExprMap, NodeInfo, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
%  Data = ?SETS:from_list(lists:map(fun expr_clear_dst/1, exp_work(Code))),
  Data = ?SETS:from_list(get_expr_ids(ExprMap, exp_work(Code))),
  NewNodeInfo = set_down_exp(NodeInfo, Label, Data),
  calc_down_exp(CFG, ExprMap, NewNodeInfo, Labels).
  
%%=============================================================================
%% Calculates the upwards exposed expressions sets for the given labels in 
%% the CFG.
calc_up_exp(_, _, NodeInfo, []) ->
  NodeInfo;
calc_up_exp(CFG, ExprMap, NodeInfo, [Label|Labels]) ->
  BB = hipe_rtl_cfg:bb(CFG, Label),
  RevCode = lists:reverse(hipe_bb:code(BB)),
  %% FIXME: Implement ExprId here 
%%  Data = ?SETS:from_list(lists:map(fun expr_clear_dst/1, exp_work(RevCode))),
  Data = ?SETS:from_list(get_expr_ids(ExprMap, exp_work(RevCode))),
  NewNodeInfo = set_up_exp(NodeInfo, Label, Data),
  calc_up_exp(CFG, ExprMap, NewNodeInfo, Labels).

%%=============================================================================
%% Given a list of expression instructions, gets a list of expression ids
%% from an expression map.
get_expr_ids(ExprMap, Instrs) ->
  lists:map(fun(Instr) -> 
		expr_map_get_id(ExprMap, expr_clear_dst(Instr))
	    end, Instrs).

%%=============================================================================
%% Does the work of the calc_*_exp functions.
exp_work(Code) ->
  exp_work([], Code).

exp_work([], [Instr|Instrs]) ->
  case is_expr(Instr) of
    true->
      exp_work([Instr], Instrs);
    false ->
      exp_work([], Instrs)
  end;
exp_work(Exprs, []) ->
  Exprs;
exp_work(Exprs, [Instr|Instrs]) ->
  case is_expr(Instr) of
    true ->
      NewExprs = exp_kill_expr(Instr, [Instr|Exprs]),
      exp_work(NewExprs, Instrs);

    false ->
      NewExprs = exp_kill_expr(Instr, Exprs),
      exp_work(NewExprs, Instrs)
  end.

%%=============================================================================
%% Checks if the given instruction redefines any operands of 
%% instructions in the instruction list. 
%% It returns the list of expressions with those instructions that has
%% operands redefined removed.
exp_kill_expr(_Instr, []) ->
  [];
exp_kill_expr(Instr, [CheckedExpr|Exprs]) ->
  %% Calls, gctests and stores potentially clobber everything
  case hipe_rtl:type(Instr) of
    call -> [];
    gctest -> [];
    store -> [];     %% FIXME: Only regs and vars clobbered, not fregs...
    fstore -> 
      %% fstore potentially clobber float expressions
      [ExprDefine|_] = hipe_rtl:defines(CheckedExpr),
      case hipe_rtl:is_fpreg(ExprDefine) of
	true ->
	  exp_kill_expr(Instr, Exprs);
	false ->
	  [CheckedExpr | exp_kill_expr(Instr, Exprs)]
      end;
    _ ->
      InstrDefines  = hipe_rtl:defines(Instr),
      ExprUses      = hipe_rtl:uses(CheckedExpr),
      Diff          = ExprUses -- InstrDefines,

      case length(Diff) < length(ExprUses) of  
	true->
	  exp_kill_expr(Instr, Exprs);
	false ->
	  [CheckedExpr | exp_kill_expr(Instr, Exprs)]
      end
  end.
     
%%%%%%%%%%%%%%%%%%%%%%%% KILLED EXPRESSIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Calculates the killed expression sets for all given labels.
calc_killed_expr(_, NodeInfo, _, _, []) ->
  NodeInfo;
calc_killed_expr(CFG, NodeInfo, UseMap, AllExpr, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
  KilledExprs = calc_killed_expr_bb(Code, UseMap, AllExpr, ?SETS:new()),
  NewNodeInfo = set_killed_expr(NodeInfo, Label, KilledExprs),
  calc_killed_expr(CFG, NewNodeInfo, UseMap, AllExpr, Labels).

%%=============================================================================
%% Calculates the killed expressions set for one basic block.
calc_killed_expr_bb([], _UseMap, _AllExpr, KilledExprs) ->
  KilledExprs;
calc_killed_expr_bb([Instr|Instrs], UseMap, AllExpr, KilledExprs) ->
  %% Calls, gctests and stores potentially clobber everything
  case hipe_rtl:type(Instr) of
    call -> AllExpr;
    gctest -> AllExpr;
    store -> AllExpr;     %% FIXME: Only regs and vars clobbered, not fregs...
    fstore -> 
      %% Kill all float expressions
      %% FIXME: Make separate function is_fp_expr
      ?SETS:from_list
	(lists:foldl(fun(Expr, Fexprs) ->
			 [Define|_] = hipe_rtl:defines(Expr),
			 case hipe_rtl:is_fpreg(Define) of
			   true ->
			     [Expr|Fexprs];
			   false ->
			     Fexprs
			 end
		     end, [], ?SETS:to_list(AllExpr)));
    _ ->
      case hipe_rtl:defines(Instr) of
	[] ->
	  calc_killed_expr_bb(Instrs, UseMap, AllExpr, KilledExprs);
	[Define|_] ->
	  NewKilledExprs = use_map_get_expr_uses(UseMap, Define),
	  calc_killed_expr_bb(Instrs, UseMap, AllExpr,
			      ?SETS:union(NewKilledExprs, KilledExprs))
      end
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%% EARLIEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Calculates the earliest set for all edges in the CFG.
calc_earliest(_, _, EdgeInfo, _, []) ->
  EdgeInfo;
calc_earliest(CFG, NodeInfo, EdgeInfo, AllExpr, [From|Labels]) ->
  IsStartLabel = (From == hipe_rtl_cfg:start_label(CFG)),
  NewEdgeInfo = 
    lists:foldl(fun(To, EdgeInfoAcc) ->
		    Earliest = 
		      calc_earliest_edge(NodeInfo, AllExpr, 
					 IsStartLabel, From, To),
		    set_earliest(EdgeInfoAcc, {From, To}, Earliest)
		end, 
		EdgeInfo,
		hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), From)),
  calc_earliest(CFG, NodeInfo, NewEdgeInfo, AllExpr, Labels).
  
%%=============================================================================
%% Calculates the earliest set for one edge.
calc_earliest_edge(NodeInfo, AllExpr, IsStartLabel, From, To) ->
  AnticIn = antic_in(NodeInfo, To),
  AvailOut = avail_out(NodeInfo, From),

  case IsStartLabel of
    true->
      ?SETS:subtract(AnticIn, AvailOut);

    false ->
      AnticOut = antic_out(NodeInfo, From),
      ExprKill = killed_expr(NodeInfo, From),
      ?SETS:intersection(?SETS:subtract(AnticIn, AvailOut),
			 ?SETS:union(ExprKill, 
				     ?SETS:subtract(AllExpr, 
						    AnticOut)))
  end.

%%%%%%%%%%%%%%%%%%%%%%%% INSERT / DELETE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Calculates the insert set for one edge and returns the resulting set.
%% NOTE This does not modify the EdgeInfo set, since the resulting set is 
%%      returned and used immediately, instead of being pre-calculated as are 
%%      the other sets.
calc_insert_edge(NodeInfo, EdgeInfo, From, To) ->
  Later = later(EdgeInfo, {From, To}),
  LaterIn = later_in(NodeInfo, To),	      
  ?SETS:subtract(Later, LaterIn).

%%=============================================================================
%% Calculates the delete set for all given labels in a CFG.
calc_delete(_, NodeInfo, []) ->
  NodeInfo;
calc_delete(CFG, NodeInfo, [Label|Labels]) ->
  case Label == hipe_rtl_cfg:start_label(CFG) of
    true -> 
      NewNodeInfo = set_delete(NodeInfo, Label, ?SETS:new());
    false ->
      UpExp = up_exp(NodeInfo, Label),
      LaterIn = later_in(NodeInfo, Label),
      Delete = ?SETS:subtract(UpExp, LaterIn),
      NewNodeInfo = set_delete(NodeInfo, Label, Delete)
  end,
  calc_delete(CFG, NewNodeInfo, Labels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% FIXPOINT FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Worklist used by the fixpoint calculations.
%% 
%% We use gb_sets here, which is optimized for continuous inserts and
%% membership tests.

init_work(Labels) ->
  {Labels, [], gb_sets:from_list(Labels)}.

get_work({[Label|Left], List, Set})->
  NewWork = {Left, List, gb_sets:delete(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set},[Label|Left])->
  case gb_sets:is_member(Label, Set) of
    true ->
      add_work(Work, Left);
    false ->
      %%io:format("Adding work: ~w\n", [Label]),
      add_work({List1, [Label|List2], gb_sets:insert(Label, Set)}, Left)
  end;
add_work(Work, []) ->
  Work.

%%=============================================================================
%% Calculates the labels that are the exit labels.
%% FIXME We do not detect dead-end loops spanning more than one block.
%%       This could potentially cause a bug in the future...
%% exit_labels(CFG) ->
%%   Labels = hipe_rtl_cfg:labels(CFG),
%%   lists:foldl(fun(Label, ExitLabels) ->
%%                   Succs = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), 
%%                                             Label),
%%                   case Succs of
%% 		    [] ->
%%                       [Label|ExitLabels];
%% 		    [Label] -> %% Count single bb dead-end loops as exit labels
%%                       [Label|ExitLabels];		      
%%                     _ ->
%%                       ExitLabels
%%                   end
%%               end, [], Labels ).

%%=============================================================================
%% Return true if label is an exit label,
%% i.e. its bb has no successors or itself as only successor.
is_exit_label(CFG, Label) ->
  case hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), Label) of
    [] -> true;
    [Label] -> true;
    _ -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% DATASET FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The dataset is a collection of data about the CFG. 
%% It is divided into two parts, NodeInfo and EdgeInfo.
%% The pre-calculation step stores the calculated sets here.

-record(node_data, {up_exp      = none,
		    down_exp    = none,
		    killed_expr = none,
		    avail_in    = none,
		    avail_out   = none,
		    antic_in    = none,
		    antic_out   = none,
		    later_in    = none,
		    delete      = none}).

-record(edge_data, {earliest    = none,
		    later       = none,
		    insert      = none}).

%%=============================================================================
%% Creates a node info from a CFG (one entry for each Label).
mk_node_info(Labels) ->
  lists:foldl(fun(Label, DataTree) ->
		  gb_trees:insert(Label, #node_data{}, DataTree)
		  %%gb_trees:enter(Label, #node_data{}, DataTree)
	      end, 
	      gb_trees:empty(), Labels).

%%mk_edge_info(Labels) ->
%%  FIXME Should we traverse cfg and initialize edges?
mk_edge_info() ->
  gb_trees:empty().

%%=============================================================================
%% Get methods
up_exp(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, up_exp, {"Node data for label not found"}}),
      none;
    {value, #node_data{up_exp = Data}} ->
      Data
  end.

down_exp(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, down_exp, {"Node info not found"}}),
      none;
    {value, #node_data{down_exp = Data}} ->
      Data
  end.

killed_expr(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, killed_expr, {"Node info not found"}}),
      none;
    {value, #node_data{killed_expr = Data}} ->
      Data
  end.

avail_in(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, avail_in, {"Node info not found"}}),
      none;
    {value, #node_data{avail_in = Data}} ->
      Data
  end.

avail_out(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, avail_out, {"Node info not found"}}),
      none;
    {value, #node_data{avail_out = Data}} ->
      Data
  end.

antic_in(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, antic_in, {"Node info not found"}}),
      none;
    {value, #node_data{antic_in = Data}} ->
      Data
  end.

antic_out(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, antic_out, {"Node info not found"}}),
      none;
    {value, #node_data{antic_out = Data}} ->
      Data
  end.

later_in(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, later_in, {"Node info not found"}}),
      none;
    {value, #node_data{later_in = Data}} ->
      Data
  end.

lookup_later_in(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      none;
    {value, #node_data{later_in = Data}} ->
      Data
  end.

delete(NodeInfo, Label) ->
  case gb_trees:lookup(Label, NodeInfo) of
    none -> 
      exit({?MODULE, delete, {"Node info not found"}}),
      none;
    {value, #node_data{delete = Data}} ->
      Data
  end.

earliest(EdgeInfo, Edge) ->
  case gb_trees:lookup(Edge, EdgeInfo) of
    none -> 
      exit({?MODULE, earliest, {"Edge info not found"}}),
      none;
    {value, #edge_data{earliest = Data}} ->
      Data
  end.

-ifdef(LOOKUP_EARLIEST_NEEDED).
lookup_earliest(EdgeInfo, Edge) ->
  case gb_trees:lookup(Edge, EdgeInfo) of
    none -> 
      none;
    {value, #edge_data{earliest = Data}} ->
      Data
  end.
-endif.

later(EdgeInfo, Edge) ->
  case gb_trees:lookup(Edge, EdgeInfo) of
    none -> 
      exit({?MODULE, later, {"Edge info not found"}}),
      none;
    {value, #edge_data{later = Data}} ->
      Data
  end.

lookup_later(EdgeInfo, Edge) ->
  case gb_trees:lookup(Edge, EdgeInfo) of
    none -> 
      none;
    {value, #edge_data{later = Data}} ->
      Data
  end.

%% insert(EdgeInfo, Edge) ->
%%   case gb_trees:lookup(Edge, EdgeInfo) of
%%     none -> 
%%       exit({?MODULE, insert, {"Edge info not found"}}),
%%       none;
%%     {value, #edge_data{insert = Data}} ->
%%       Data
%%   end.

%%=============================================================================
%% Set methods
set_up_exp(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{up_exp = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{up_exp = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_down_exp(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{down_exp = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{down_exp = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_killed_expr(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{killed_expr = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{killed_expr = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_avail_in(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{avail_in = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{avail_in = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_avail_out(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{avail_out = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{avail_out = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_antic_in(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{antic_in = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{antic_in = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_antic_out(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{antic_out = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{antic_out = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_later_in(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{later_in = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{later_in = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_delete(NodeInfo, Label, Data) ->
  NodeData =
    case gb_trees:lookup(Label, NodeInfo) of
      none -> 
	#node_data{delete = Data};
      {value, OldNodeData} ->
	OldNodeData#node_data{delete = Data}
    end,
  gb_trees:enter(Label, NodeData, NodeInfo).

set_earliest(EdgeInfo, Edge, Data) ->
  EdgeData =
    case gb_trees:lookup(Edge, EdgeInfo) of
      none -> 
	#edge_data{earliest = Data};
      {value, OldEdgeData} ->
	OldEdgeData#edge_data{earliest = Data}
    end,
  gb_trees:enter(Edge, EdgeData, EdgeInfo).

set_later(EdgeInfo, Edge, Data) ->
  EdgeData =
    case gb_trees:lookup(Edge, EdgeInfo) of
      none -> 
	#edge_data{later = Data};
      {value, OldEdgeData} ->
	OldEdgeData#edge_data{later = Data}
    end,
  gb_trees:enter(Edge, EdgeData, EdgeInfo).

%% set_insert(EdgeInfo, Edge, Data) ->
%%   EdgeData =
%%     case gb_trees:lookup(Edge, EdgeInfo) of
%%       none -> 
%% 	#edge_data{insert = Data};
%%       {value, OldEdgeData} ->
%% 	OldEdgeData#edge_data{insert = Data}
%%     end,
%%   gb_trees:enter(Edge, EdgeData, EdgeInfo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% USE MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The use map is a mapping from "use" (which is an rtl register/variable) 
%% to a set of expressions (IDs) where that register/variable is used.
%% It is used by calc_killed_expr to know what expressions are affected by
%% a definition.

%%=============================================================================
%% Creates and calculates the use map for a CFG.
%% It uses ExprMap to lookup the expression IDs.
mk_use_map(CFG, ExprMap) ->
  Labels = hipe_rtl_cfg:reverse_postorder(CFG),
  NewMap = mk_use_map(gb_trees:empty(), CFG, ExprMap, Labels),
  gb_trees:balance(NewMap).

mk_use_map(Map, _, _, []) ->
  Map;
mk_use_map(Map, CFG, ExprMap, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
  NewMap = mk_use_map_bb(Map, ExprMap, Code),
  mk_use_map(NewMap, CFG, ExprMap, Labels).

mk_use_map_bb(UseMap, _, []) ->
  UseMap;
mk_use_map_bb(UseMap, ExprMap, [Instr|Instrs]) ->
  case is_expr(Instr) of
    true ->
      Uses = hipe_rtl:uses(Instr),
      ExprId = expr_map_get_id(ExprMap, expr_clear_dst(Instr)),
      NewUseMap = mk_use_map_insert_uses(UseMap, ExprId, Uses),
      mk_use_map_bb(NewUseMap, ExprMap, Instrs);
    false ->
      mk_use_map_bb(UseMap, ExprMap, Instrs)
  end.

%%=============================================================================
%% Worker function for mk_use_map that inserts the expression id for every 
%% rtl register the expression uses in a use map.
mk_use_map_insert_uses(Map, _, []) ->
  Map;
mk_use_map_insert_uses(Map, Expr, [Use|Uses]) ->
  case gb_trees:lookup(Use, Map) of
    {value, UseSet} ->
      NewUseSet = ?SETS:add_element(Expr, UseSet),
      mk_use_map_insert_uses(gb_trees:update(Use, NewUseSet, Map), Expr, Uses);
    none ->
      UseSet = ?SETS:new(),
      NewUseSet = ?SETS:add_element(Expr, UseSet),
      mk_use_map_insert_uses(gb_trees:insert(Use, NewUseSet, Map), Expr, Uses)
  end.

%%=============================================================================
%% Gets a set of expressions where the given rtl register is used.
use_map_get_expr_uses(Map, Reg) ->
  case gb_trees:lookup(Reg, Map) of
    {value, UseSet} ->
      UseSet;
    none ->
      ?SETS:new()
  end. 

%%%%%%%%%%%%%%%%%%%%%% EXPRESSION MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The expression map is a mapping from expression to 
%% (1) Expression Id (Integer used to speed up set operations)
%% (2) List of definitions (labels where the expression is defined and the 
%%     list of registers or variables defined by an instruction in that label,
%%     represented as a tuple {Label, Defines})
%% (3) The list of replacement registers created for the expression

%%=============================================================================
%% Creates and calculates the expression map for a CFG.
mk_expr_map(CFG) ->
  init_expr_id(),
  Labels = hipe_rtl_cfg:reverse_postorder(CFG),
  {ExprMap, IdMap} = mk_expr_map(gb_trees:empty(), gb_trees:empty(), 
				 CFG, Labels),
  {gb_trees:balance(ExprMap), gb_trees:balance(IdMap)}.

mk_expr_map(ExprMap, IdMap, _, []) ->
  {ExprMap, IdMap};
mk_expr_map(ExprMap, IdMap, CFG, [Label|Labels]) ->
  Code = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
  {NewExprMap, NewIdMap} = mk_expr_map_bb(ExprMap, IdMap, Label, Code),
  mk_expr_map(NewExprMap, NewIdMap, CFG, Labels).

mk_expr_map_bb(ExprMap, IdMap, _, []) ->
  {ExprMap, IdMap};
mk_expr_map_bb(ExprMap, IdMap, Label, [Instr|Instrs]) ->
  case is_expr(Instr) of
    true ->
      Expr = expr_clear_dst(Instr),
      Defines = hipe_rtl:defines(Instr),
      case gb_trees:lookup(Expr, ExprMap) of
        {value, {ExprId, DefinesList, ReplRegs}} ->
          NewExprMap = gb_trees:update(Expr, {ExprId, 
					  [{Label, Defines}|DefinesList], 
					  ReplRegs}, ExprMap),
	  mk_expr_map_bb(NewExprMap, IdMap, Label, Instrs);
        none ->
	  NewExprId = new_expr_id(),
          NewReplRegs = mk_replacement_regs(Defines),
          NewExprMap = gb_trees:insert(Expr, {NewExprId, 
					  [{Label, Defines}], 
					  NewReplRegs}, ExprMap),
          NewIdMap = gb_trees:insert(NewExprId, Expr, IdMap),
	  mk_expr_map_bb(NewExprMap, NewIdMap, Label, Instrs)
      end;
    false ->
      mk_expr_map_bb(ExprMap, IdMap, Label, Instrs)
  end.

%%=============================================================================
%% Creates new temporaries to replace defines in moved expressions.
mk_replacement_regs([]) ->
  [];
mk_replacement_regs(Defines) ->
  mk_replacement_regs(Defines, []).

mk_replacement_regs([], NewRegs) ->
  lists:reverse(NewRegs);
mk_replacement_regs([Define|Defines], NewRegs) ->
  case hipe_rtl:is_reg(Define) of
    true ->
      mk_replacement_regs(Defines, [hipe_rtl:mk_new_reg()|NewRegs]);
    false ->
      case hipe_rtl:is_var(Define) of
	true ->
	  mk_replacement_regs(Defines, [hipe_rtl:mk_new_var()|NewRegs]);
	false ->
	  case hipe_rtl:is_fpreg(Define) of
	    true ->
	      mk_replacement_regs(Defines, [hipe_rtl:mk_new_fpreg()|NewRegs])
	  end
      end
  end.
  
%%=============================================================================
%% Performs a lookup, which returns a tuple
%% {expression ID, list of definitions, list of replacement registers}
expr_map_lookup(Map, Expr) ->
  gb_trees:lookup(Expr, Map).

%%=============================================================================
%% Gets the actual rtl instruction to be generated for insertions of an 
%% expression.
expr_map_get_instr(Map, Expr) ->
  case gb_trees:lookup(Expr, Map) of
    {value, {_, _, Regs}} ->
      expr_set_dst(Expr, Regs);
    none ->
      exit({?MODULE,expr_map_get_instr, {"expression missing"}})
  end.

%%=============================================================================
%% Gets expression id.
expr_map_get_id(Map, Expr) ->
  case gb_trees:lookup(Expr, Map) of
    {value, {ExprId, _, _}} ->
      ExprId;
    none ->
      exit({?MODULE,expr_map_get_instr, {"expression missing"}})
  end.

%%=============================================================================
%% Creates an rtl instruction that moves a value
mk_expr_move_instr([Reg], [Define]) ->
  case hipe_rtl:is_fpreg(Reg) of
    true ->
      hipe_rtl:mk_fmove(Reg, Define);
    false ->
      %% FIXME Check is_var() orelse is_reg() ?
      hipe_rtl:mk_move(Reg, Define)
  end;
mk_expr_move_instr([Reg|Regs], Defines) ->
  %% FIXME Does this really work? What about floats...
  %% (Multiple defines does not seem to be used by any of the 
  %%  instructions considered by rtl_lcm at the moment so this is pretty much
  %%  untested/unused.)
  hipe_rtl:mk_multimove([Reg|Regs], Defines);
mk_expr_move_instr(_, []) ->
  exit({?MODULE,mk_expr_move_instr, {"bad match"}}).

%%=============================================================================
%% Returns a set of all expressions in the code.
%% all_exprs(_CFG, []) ->
%%   ?SETS:new();
%% all_exprs(CFG, [Label|Labels]) ->
%%   BB = hipe_rtl_cfg:bb(CFG, Label),
%%   Code = hipe_bb:code(BB),
%%   ?SETS:union(all_exprs_bb(Code),
%% 	      all_exprs(CFG, Labels)).

%%=============================================================================
%% Returns a set of expressions in a basic block.
%% all_exprs_bb([]) ->
%%   ?SETS:new();
%% all_exprs_bb([Instr|Instrs]) ->
%%   case is_expr(Instr) of
%%     true->
%%       Expr = expr_clear_dst(Instr),
%%       ExprSet = all_exprs_bb(Instrs),
%%       ?SETS:add_element(Expr, ExprSet);
%%     false ->
%%       all_exprs_bb(Instrs)
%%   end.

%%%%%%%%%%%%%%%%%% EXPRESSION ID -> EXPRESSION MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map from expression IDs to expressions.
%%=============================================================================
%% mk_expr_id_map() ->
%%   gb_trees:empty().

%% expr_id_map_insert(Map, ExprId, Expr) ->
%%   gb_trees:insert(ExprId, Expr, Map).

%% expr_id_map_lookup(Map, ExprId) ->
%%   gb_trees:lookup(ExprId, Map).

%%=============================================================================
%% Given expression id, gets expression.
expr_id_map_get_expr(Map, ExprId) ->
  case gb_trees:lookup(ExprId, Map) of
    {value, Expr} ->
      Expr;
    none ->
      exit({?MODULE,expr_id_map_get_expr, {"expression id missing"}})
  end.

%%=============================================================================
%% Expression ID counter
init_expr_id() ->
  put({rtl_lcm,expr_id_count}, 0).

new_expr_id() ->
  V = get({rtl_lcm,expr_id_count}),
  put({rtl_lcm,expr_id_count}, V+1),
  V.

%%%%%%%%%%%%%%%%%% EDGE BB (INSERT BETWEEN) MAP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map from edges to labels.
%% This is used by insert_expr_between to remember what new bbs it has created
%% for insertions on edges, and thus for multiple insertions on the same edge
%% to end up in the same bb.
%%=============================================================================
mk_edge_bb_map() ->
  gb_trees:empty().

edge_bb_map_insert(Map, Edge, Label) ->
  gb_trees:enter(Edge, Label, Map).

edge_bb_map_lookup(Map, Edge) ->
  gb_trees:lookup(Edge, Map).
