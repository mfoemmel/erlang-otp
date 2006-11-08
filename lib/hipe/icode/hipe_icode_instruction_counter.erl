%%%-------------------------------------------------------------------
%%% File    : icode_instruction_counter.erl
%%% Author  : Andreas Hasselberg <anha0825@student.uu.se>
%%% Description : This module counts the number of different 
%%% instructions in a function. It is useful when you want to know 
%%% if your icode analysis/specialization is good, bad or unlucky :)
%%%
%%% Created :  2 Oct 2006 by Andreas Hasselberg <anha0825@student.uu.se>
%%%-------------------------------------------------------------------

-module(hipe_icode_instruction_counter).

-include("hipe_icode.hrl").

-export([cfg/3, compare/3]).

%%%-------------------------------------------------------------------
%%% A general Cfg instruction walktrough
%%%-------------------------------------------------------------------

cfg(Cfg, _IcodeFun, _Options) ->
    Labels = hipe_icode_cfg:labels(Cfg),
    %% Your Info init function goes here
    InitInfo = counter__init_info(),
    Info = lists:foldl(
	     fun(Label, InfoAcc) ->
		     BB = hipe_icode_cfg:bb(Cfg, Label),
		     Code = hipe_bb:code(BB),
		     walktrough_bb(Code, InfoAcc)
	     end,
	     InitInfo,
	     Labels),
    %counter__output_info(IcodeFun, Info),
    Info.


walktrough_bb(BB, Info) ->
    lists:foldl(
      fun(Insn, InfoAcc) ->
	      %% Your analysis function here
	      counter__analys_insn(Insn, InfoAcc)
      end,
      Info,
      BB).

%%%-------------------------------------------------------------------
%%% The counter specific functions
%%%-------------------------------------------------------------------

compare(Name, Old, New) ->
    NewList = gb_trees:to_list(New),
    OldList = gb_trees:to_list(Old),
    Temptree =  compare_one_way(NewList, Old, added, gb_trees:empty()),
    Difftree = compare_one_way(OldList, New, removed, Temptree),
    DiffList = gb_trees:to_list(Difftree),
    if DiffList =:= [] ->
	    ok;
       true ->
	    io:format("~p: ~p ~n", [Name, DiffList])
    end,
    Difftree.
    


    
compare_one_way(List, Tree, Key, Fold_tree) ->
    lists:foldl(
      fun({Insn, ListCount}, DiffAcc) ->
	      DiffCount = 
	      case gb_trees:lookup(Insn, Tree) of
		  {value, TreeCount} ->
		      ListCount - TreeCount;
		  none ->
		      ListCount
	      end,
	      if DiffCount > 0 ->
		      gb_trees:insert({Key, Insn}, DiffCount, DiffAcc);
		 true ->
		      DiffAcc
	      end
      end,
      Fold_tree,
      List).
      
      
      

counter__init_info() ->
    gb_trees:empty().

counter__analys_insn(Insn, Info) ->
    Key = counter__insn_get_key(Insn),
    counter__increase_key(Key, Info).

counter__insn_get_key(If = #'if'{}) -> {'if', hipe_icode:if_op(If)};
counter__insn_get_key(Call = #call{}) -> {call, hipe_icode:call_fun(Call)};
counter__insn_get_key(#enter{}) -> enter;
counter__insn_get_key(#return{}) -> return;
counter__insn_get_key(#type{}) -> type;
counter__insn_get_key(#switch_val{}) -> switch_val;
counter__insn_get_key(#switch_tuple_arity{}) -> switch_tuple_arity;
counter__insn_get_key(#goto{}) -> goto;
counter__insn_get_key(#move{}) -> move;
counter__insn_get_key(#fmove{}) -> fmove;
counter__insn_get_key(#phi{}) -> phi;
counter__insn_get_key(#begin_try{}) -> begin_try;
counter__insn_get_key(#end_try{}) -> end_try;
counter__insn_get_key(#begin_handler{}) -> begin_handler;
counter__insn_get_key(#fail{}) -> fail;
counter__insn_get_key(#comment{}) -> comment.

counter__increase_key(Key, Info) ->
    NewCounter = 
    case gb_trees:lookup(Key, Info) of
	{value, Counter} when is_integer(Counter) ->
	    Counter + 1;
	none ->
	    1
    end,
    gb_trees:enter(Key, NewCounter, Info).

%counter__output_info(IcodeFun, Info) ->
%    InfoList = gb_trees:to_list(Info),
%    io:format("~p instructions : ~p ~n", [IcodeFun, InfoList]).
