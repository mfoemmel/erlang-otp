%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File        : hipe_rtl_lcm.erl
%% Author      : Henrik Nyman and Erik Cedheim
%% Description : Performs Lazy Code Motion on RTL.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%
%% This module implements Lazy Code Motion on RTL.
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_lcm).
-export([rtl_lcm/2]).

-define(SETS, ordsets).   %% Which set implementation module to use
                          %% We have tried gb_sets, sets and ordsets and
                          %% ordsets seems to be a lot faster according to 
                          %% our test runs.

-include("../main/hipe.hrl").
-include("hipe_rtl_lcm.hrl").

%%-define(LCM_DEBUG, true). %% When defined and true, produces debug printouts

%%=============================================================================
%% @spec rtl_lcm(CFG::cfg(), Options::options()) -> cfg()
%%
%% @doc Performs Lazy Code Motion on RTL.
%%
rtl_lcm(CFG, Options) ->
  %% Perform precalculation of the data sets.
%  {NodeInfo, EdgeInfo, AllExpr, ExprMap, Labels} = lcm_precalc(CFG),
  ?opt_start_timer("RTL LCM precalc"),
  {NodeInfo, EdgeInfo, AllExpr, ExprMap, IdMap, Labels} = 
    lcm_precalc(CFG, Options),
  ?opt_stop_timer("RTL LCM precalc"),
%  {NodeInfo, EdgeInfo, AllExpr, ExprMap, Labels} = 
%    ?option_time(lcm_precalc(CFG, Options), "RTL LCM precalc", Options),
  
  pp_debug("-------------------------------------------------~n",[]),%DEBUG
  %% pp_debug( "~w~n", [MFA]),
  
  %% A check if we should pretty print the result.
  case proplists:get_bool(pp_rtl_lcm, Options) of
    true->
      pp_debug("-------------------------------------------------~n",[]),%DEBUG
      %% pp_debug("AllExpr:  ~w~n", [AllExpr]),
      pp_debug("AllExpr:~n", []),
      pp_exprs(ExprMap, IdMap, ?SETS:to_list(AllExpr)),
      %% pp_sets(ExprMap, NodeInfo, EdgeInfo, AllExpr, CFG2<-ERROR!, Labels); 
      pp_sets(ExprMap, IdMap, NodeInfo, EdgeInfo, AllExpr, CFG, Labels);
    _ ->
      ok
  end,
  
  pp_debug("-------------------------------------------------~n",[]),%DEBUG
%   {CFG1, MoveSet} = 
%     perform_lcm(CFG, NodeInfo, EdgeInfo, ExprMap, IdMap,
% 		AllExpr, ?SETS:new(), Labels),
  ?option_time({CFG1, MoveSet} = perform_lcm(CFG, NodeInfo, EdgeInfo, ExprMap, 
					     IdMap, AllExpr, mk_edge_bb_map(), 
					     ?SETS:new(), Labels),
	       "RTL_LCM perform_lcm", Options),

  %% Scan through list of moved expressions and replace their 
  %% assignments with the new temporary created for that expression
  MoveList = ?SETS:to_list(MoveSet),
  ?option_time(CFG2 = moved_expr_replace_assignments(CFG1, ExprMap, IdMap, 
						     MoveList),
	       "RTL_LCM moved_expr_replace_assignments", Options),
  pp_debug("-------------------------------------------------~n~n",[]),%DEBUG
  
  CFG2.

%%=============================================================================
%% Performs lazy code motion given the pre-calculated data sets.
perform_lcm(CFG, _, _, _, _, _, _, MoveSet, []) ->
  {CFG, MoveSet};
perform_lcm(CFG0, NodeInfo, EdgeInfo, ExprMap, IdMap, AllExp, BetweenMap, 
	    MoveSet0, [Label|Labels]) ->
  Code0 = hipe_bb:code(hipe_rtl_cfg:bb(CFG0, Label)),
  DeleteSet = delete(NodeInfo, Label),
  
  %% Check if something should be deleted from this block.
  {CFG1, MoveSet1} = 
    case ?SETS:size(DeleteSet) > 0 of
      true ->
        pp_debug("Label ~w: Expressions Deleted: ~n", %DEBUG
		 [Label]), %DEBUG
        Code1 = delete_exprs(Code0, ExprMap, IdMap, ?SETS:to_list(DeleteSet)),
        BB = hipe_bb:mk_bb(Code1),
        {hipe_rtl_cfg:bb_add(CFG0, Label, BB), 
         ?SETS:union(MoveSet0, DeleteSet)};
      false ->
        {CFG0, MoveSet0}
    end,
  
  Succs = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG1), Label),  
  
  %% Go through the list of successors and insert expression where needed.
  %% Also collect a list of expressions that are inserted somewhere
  {CFG2, NewBetweenMap, MoveSet2} = 
    lists:foldl(fun(Succ, {CFG, BtwMap, MoveSet}) ->
		    InsertSet = calc_insert_edge(NodeInfo, EdgeInfo, 
						 Label, Succ),
		    %% Check if something should be inserted on this edge.
		    case ?SETS:size(InsertSet) > 0 of
		      true ->
			pp_debug("Label ~w: Expressions Inserted for Successor: ~w~n", [Label, Succ]), %DEBUG
			InsertList = ?SETS:to_list(InsertSet),
			{NewCFG, NewBtwMap} =
			  insert_exprs(CFG, Label, Succ, ExprMap, IdMap, 
				       BtwMap, InsertList),
			{NewCFG, NewBtwMap, ?SETS:union(MoveSet, InsertSet)};

		      false ->
			{CFG, BtwMap, MoveSet}
		    end
		end, 
		{CFG1, BetweenMap, MoveSet1}, Succs),
  
  perform_lcm(CFG2, NodeInfo, EdgeInfo, ExprMap, IdMap, AllExp, NewBetweenMap, 
	      MoveSet2, Labels).

%%=============================================================================
%% Scan through list of moved expressions and replace their 
%% assignments with the new temporary created for that expression.
moved_expr_replace_assignments(CFG, _, _, []) ->
  CFG;
moved_expr_replace_assignments(CFG0, ExprMap, IdMap, [ExprId|Exprs]) ->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  case expr_map_lookup(ExprMap, Expr) of
    {value, {_, ReplaceList, NewReg}} -> 
      CFG1 = lists:foldl(fun({Label, Reg}, CFG) ->
                      %% Find and replace expression in block
		      pp_debug("Label ~w: Expressions Replaced:~n", [Label]), %DEBUG
		      Code0 = hipe_bb:code(hipe_rtl_cfg:bb(CFG, Label)),
                      Code1 = 
                        moved_expr_do_replacement(expr_set_dst(Expr, Reg), 
                                                  Reg, NewReg, Code0),
                      hipe_rtl_cfg:bb_add(CFG, Label, hipe_bb:mk_bb(Code1))
                  end, CFG0, ReplaceList),
      moved_expr_replace_assignments(CFG1, ExprMap, IdMap, Exprs);
    none ->
      moved_expr_replace_assignments(CFG0, ExprMap, IdMap, Exprs)
  end.

moved_expr_do_replacement(_, _, _, []) ->
  [];
moved_expr_do_replacement(Expr, Reg, NewReg, [Expr|Instrs]) ->
  NewExpr = expr_set_dst(Expr, NewReg),
  Move = mk_expr_move_instr(Reg, NewReg),

  pp_debug("  Replacing:~n", []), %DEBUG
  pp_debug_instr(Expr), %DEBUG
  pp_debug("  With:~n", []), %DEBUG
  pp_debug_instr(NewExpr), %DEBUG
  pp_debug_instr(Move), %DEBUG
  [NewExpr, Move | moved_expr_do_replacement(Expr, Reg, NewReg, Instrs)];
moved_expr_do_replacement(Expr, Reg, NewReg, [Instr|Instrs]) ->
  [Instr | moved_expr_do_replacement(Expr, Reg, NewReg, Instrs)].

%%=============================================================================
%% Goes through the given list of expressions and deletes them from the code.
%% NOTE We do not actually delete an expression, but instead we replace it 
%%      with an assignment from the new temporary containing the result of the 
%%      expressions which is guaranteed to have been calculated earlier in 
%%      the code.
delete_exprs(Code, _, _, []) ->
  Code;
delete_exprs(Code, ExprMap, IdMap, [ExprId|Exprs]) ->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  %% Perform a foldl that goes through the code and delete all occurances of
  %% the expression.
  NewCode =
    lists:reverse
      (lists:foldl(fun(CodeExpr, Acc) ->
                     case is_expr(CodeExpr) of
                       true ->
                         case expr_clear_dst(CodeExpr) == Expr of
                           true ->
                             pp_debug("  Deleting:         ", []),%DEBUG
                             pp_debug_instr(CodeExpr),%DEBUG
                            
                             %% Lookup expression entry.
                             Defines = 
                               case expr_map_lookup(ExprMap, Expr) of
                                 {value, {_, _, Defs}} -> 
				   Defs;
                                 none -> 
                                   exit({?MODULE,expr_map_lookup,
                                         {"expression missing"}})
                               end,
                                 
                             MoveCode = 
                               mk_expr_move_instr
                                 (hipe_rtl:defines(CodeExpr), Defines),

                             pp_debug("    Replacing with: ", []),%DEBUG
                             pp_debug_instr(MoveCode),%DEBUG
                             
                             %Acc ++ [];
                             [MoveCode|Acc];
                           false ->
                             %Acc ++ [CodeExpr]
                             [CodeExpr|Acc]
                         end;
                     false ->
                             [CodeExpr|Acc]
                     end
                   end, 
		   [], Code)),
  delete_exprs(NewCode, ExprMap, IdMap, Exprs).

%%=============================================================================
%% Goes through the given list of expressions and inserts them at 
%% appropriate places in the code.
insert_exprs(CFG, _, _, _, _, BetweenMap, [])->
  {CFG, BetweenMap};
insert_exprs(CFG, Pred, Succ, ExprMap, IdMap, BetweenMap, [ExprId|Exprs])->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  Instr = expr_map_get_instr(ExprMap, Expr),
  Succs = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), Pred),
  case length(Succs) == 1 of
    true->
      pp_debug("  Inserted last: ", []),%DEBUG
      pp_debug_instr(Instr),%DEBUG
      NewCFG = insert_expr_last(CFG, Pred, Instr),
      insert_exprs(NewCFG, Pred, Succ, ExprMap, IdMap, BetweenMap, Exprs);

    false ->
      Preds = hipe_rtl_cfg:pred(hipe_rtl_cfg:pred_map(CFG), Succ),
      case length(Preds) == 1 of
        true->
	  pp_debug("  Inserted first: ", []),%DEBUG
	  pp_debug_instr(Instr),%DEBUG
          NewCFG = insert_expr_first(CFG, Succ, Instr),
          insert_exprs(NewCFG, Pred, Succ, ExprMap, IdMap, BetweenMap, Exprs);

        false ->
	  pp_debug("  Inserted between: ", []),%DEBUG
	  pp_debug_instr(Instr),%DEBUG
          {NewCFG, NewBetweenMap} = 
	    insert_expr_between(CFG, BetweenMap, Pred, Succ, Instr),
          insert_exprs(NewCFG, Pred, Succ, ExprMap, IdMap, NewBetweenMap, Exprs)
      end 
  end. 

%%=============================================================================
%% Recursively goes through the code in a block and returns a new block
%% with the new code inserted second to last (assuming the last expression is
%% a branch operation.
insert_expr_last(CFG0, Label, Instr) ->
  Code0 = hipe_bb:code(hipe_rtl_cfg:bb(CFG0, Label)),
  %% FIXME Use hipe_bb:butlast() instead?
  Code1 = insert_expr_last_work(Label, Instr, Code0),
  hipe_rtl_cfg:bb_add(CFG0, Label, hipe_bb:mk_bb(Code1)).

%%=============================================================================
%% Recursively goes through the code in a block and returns a new block
%% with the new code inserted second to last (assuming the last expression is
%% a branch operation.
insert_expr_last_work(_, Instr, []) ->
  %% This case should not happen since this means that block was completely 
  %% empty when the function was called. For compability we insert it last.
  [Instr];
insert_expr_last_work(_, Instr, [Code1]) ->
  %% We insert the code next to last.
  [Instr, Code1];
insert_expr_last_work(Label, Instr, [Code|Codes]) ->
  [Code|insert_expr_last_work(Label, Instr, Codes)].

%%=============================================================================
%% Inserts expression first in the block for the given label.
insert_expr_first(CFG0, Label, Instr) ->
  %% The first instruction is always a label
  [Lbl|Code0] = hipe_bb:code(hipe_rtl_cfg:bb(CFG0, Label)),
  Code1 = [Lbl, Instr | Code0],
  hipe_rtl_cfg:bb_add(CFG0, Label, hipe_bb:mk_bb(Code1)).

%%=============================================================================
%% Inserts an expression on and edge between two existing blocks.
%% It creates a new basic block to hold the expression. 
%% Created bbs are inserted into BetweenMap to be able to reuse them for 
%% multiple inserts on the same edge.
%% NOTE Currently creates multiple blocks for identical expression with the
%%      same successor. Since the new bb usually contains very few instructions
%%      this should not be a problem.
insert_expr_between(CFG0, BetweenMap, Pred, Succ, Instr) ->
  case edge_bb_map_lookup(BetweenMap, {Pred, Succ}) of
    none ->
      NewLabel = hipe_rtl:mk_new_label(),
      NewLabelName = hipe_rtl:label_name(NewLabel),
      pp_debug("    Creating new bb ~w~n", [NewLabel]),
      Code = [Instr, hipe_rtl:mk_goto(Succ)],
      CFG1 = hipe_rtl_cfg:bb_add(CFG0, NewLabelName, hipe_bb:mk_bb(Code)),
      CFG2 = hipe_rtl_cfg:redirect(CFG1, Pred, Succ, NewLabelName),
      NewBetweenMap = edge_bb_map_insert(BetweenMap, {Pred, Succ}, 
					NewLabelName),
      pp_debug("    Mapping edge (~w,~w) to label ~w~n", 
	       [Pred, Succ, NewLabelName]),
      {CFG2, NewBetweenMap};

    {value, Label} ->
      pp_debug("    Using existing new bb for edge (~w,~w) with label ~w~n", 
	       [Pred, Succ, Label]),
      {insert_expr_last(CFG0, Label, Instr), BetweenMap}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRETTY-PRINTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Prints debug messages.
-ifdef(LCM_DEBUG).

pp_debug(Str, Args) ->
  case ?LCM_DEBUG of
    true ->
      io:format(standard_io, Str, Args);
    false ->
      ok
  end.

pp_debug_instr(Instr) ->
  case ?LCM_DEBUG of
    true ->
      hipe_rtl:pp_instr(standard_io, Instr);
    false ->
      ok
  end.

-else.

pp_debug(_, _) ->  
  ok.

pp_debug_instr(_) ->  
  ok.

-endif.	%% DEBUG

%%=============================================================================
%% Pretty-prints the calculated sets for the lazy code motion.
pp_sets(_, _, _, _, _, _, []) ->
  ok; 
pp_sets(ExprMap, IdMap, NodeInfo, EdgeInfo, AllExpr, CFG, [Label|Labels]) ->
  Preds = hipe_rtl_cfg:pred(hipe_rtl_cfg:pred_map(CFG), Label),
  Succs = hipe_rtl_cfg:succ(hipe_rtl_cfg:succ_map(CFG), Label),

  io:format(standard_io, "Label ~w~n", [Label]),
  io:format(standard_io, "  Preds:    ~w~n", [Preds]),
  io:format(standard_io, "  Succs:    ~w~n", [Succs]),

  case up_exp(NodeInfo, Label) of
    none -> ok;
    UpExp -> 
      case ?SETS:size(UpExp) == 0 of
	false ->
	  io:format(standard_io, "  UEExpr: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(UpExp));
	true -> ok
      end
  end,
  case down_exp(NodeInfo, Label) of
    none -> ok;
    DownExp -> 
      case ?SETS:size(DownExp) == 0 of
	false ->
	  io:format(standard_io, "  DEExpr: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(DownExp));
	true -> ok
      end
  end,
  case killed_expr(NodeInfo, Label) of
    none -> ok;
    KilledExpr -> 
      case ?SETS:size(KilledExpr) == 0 of
	false ->
	  io:format(standard_io, "  ExprKill: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(KilledExpr));
	true -> ok
      end
  end,
  case avail_in(NodeInfo, Label) of
    none -> ok;
    AvailIn ->
      case ?SETS:size(AvailIn) == 0 of
	false ->
	  io:format(standard_io, "  AvailIn:  ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AvailIn));
	true -> ok
      end
  end,
  case avail_out(NodeInfo, Label) of
    none -> ok;
    AvailOut ->
      case ?SETS:size(AvailOut) == 0 of
	false ->
	  io:format(standard_io, "  AvailOut: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AvailOut));
	true -> ok
      end
  end,
  case antic_in(NodeInfo, Label) of
    none -> ok;
    AnticIn ->
      case ?SETS:size(AnticIn) == 0 of
	false ->
	  io:format(standard_io, "  AnticIn:  ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AnticIn));
	true -> ok
      end
  end,
  case antic_out(NodeInfo, Label) of
    none -> ok;
    AnticOut ->
      case ?SETS:size(AnticOut) == 0 of
	false ->
	  io:format(standard_io, "  AnticOut: ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(AnticOut));
	true -> ok
      end
  end,
  case later_in(NodeInfo, Label) of
    none -> ok;
    LaterIn ->
      case ?SETS:size(LaterIn) == 0 of
	false ->
	  io:format(standard_io, "  LaterIn:  ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(LaterIn));
	true -> ok
      end
  end,  

  pp_earliest(ExprMap, IdMap, EdgeInfo, Label, Succs),
  pp_later(ExprMap, IdMap, EdgeInfo, Label, Succs),

  case delete(NodeInfo, Label) of
    none -> ok;
    Delete ->
      case ?SETS:size(Delete) == 0 of
	false ->
	  io:format(standard_io, "  Delete:   ~n", []),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(Delete));
	true -> ok
      end
  end,
  pp_sets(ExprMap, IdMap, NodeInfo, EdgeInfo, AllExpr, CFG, Labels).

%%=============================================================================
%% Pretty-prints the later set.
pp_later(_, _, _, _, []) ->
  ok;
pp_later(ExprMap, IdMap, EdgeInfo, Pred, [Succ|Succs]) ->
  case later(EdgeInfo, {Pred, Succ}) of
    none -> ok;
    Later ->
      case ?SETS:size(Later) == 0 of
	false ->
	  io:format(standard_io, "  Later(~w->~w): ~n", [Pred,Succ]),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(Later));
	true -> ok
      end
  end,
  pp_later(ExprMap, IdMap, EdgeInfo, Pred, Succs).

%%=============================================================================
%% Pretty-prints the earliest set.
pp_earliest(_, _, _, _, []) ->
  ok;
pp_earliest(ExprMap, IdMap, EdgeInfo, Pred, [Succ|Succs]) ->
  case earliest(EdgeInfo, {Pred, Succ}) of
    none -> ok;
    Earliest ->
      case ?SETS:size(Earliest) == 0 of
	false ->
	  io:format(standard_io, "  Earliest(~w->~w): ~n", [Pred,Succ]),
	  pp_exprs(ExprMap, IdMap, ?SETS:to_list(Earliest));
	true -> ok
      end
  end,
  pp_earliest(ExprMap, IdMap, EdgeInfo, Pred, Succs).

%%=============================================================================
%% Pretty-prints an expression
pp_expr(ExprMap, IdMap, ExprId) ->
  Expr = expr_id_map_get_expr(IdMap, ExprId),
  hipe_rtl:pp_instr(standard_io, expr_map_get_instr(ExprMap, Expr)).

pp_exprs(_, _, []) ->
  ok;
pp_exprs(ExprMap, IdMap, [E|Es]) ->
  pp_expr(ExprMap, IdMap, E),
  pp_exprs(ExprMap, IdMap, Es).
