%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/08/01 21:42:44 happi>
%% ====================================================================
%%  Filename : 	hipe_icode_heap_test.erl
%%  Module   :	hipe_icode_heap_test
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-11-07 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2001/10/01 07:53:22 $
%%              $Revision: 1.3 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_heap_test).
-export([cfg/1]).

%-ifndef(DEBUG).
%-define(DEBUG,1).
%-endif.
-define(DO_ASSERT,true).
-include("../main/hipe.hrl").
%-------------------------------------------------------------------------
-include("../rtl/hipe_literals.hrl").
%-------------------------------------------------------------------------


cfg(CFG) ->
  Icode = hipe_icode_cfg:linearize(CFG),
  Code = hipe_icode:icode_code(Icode),
  ActualVmax = hipe_icode:highest_var(Code),
  ActualLmax = hipe_icode:highest_label(Code),

  hipe_gensym:set_label(icode,ActualLmax+1),
  hipe_gensym:set_var(icode,ActualVmax+1),

  EBBs = hipe_icode_ebb:cfg(CFG),
  Start = hipe_icode_cfg:start_label(CFG),

  {EBBcode,Visited} = ebbs(EBBs,[], CFG),
  %% io:format("Visited ~w\n",[Visited]),
  %% io:format("EBBs ~w\n",[EBBs]),
  NewCode = add_gc_tests(EBBcode),
  NewIcode = hipe_icode:icode_code_update(Icode,NewCode),
  %% hipe_icode:pp(NewIcode),
  hipe_icode_cfg:init(NewIcode).

ebbs([EBB|EBBs], Visited, CFG) ->
  case hipe_icode_ebb:type(EBB) of
    node ->
      L = hipe_icode_ebb:node_label(EBB),
      case visited(L, Visited) of
	true ->
	  ebbs(EBBs, Visited, CFG);
	false ->
	  EBBCode = hipe_bb:code(hipe_icode_cfg:bb(CFG, L)),
	  case hipe_icode_ebb:node_successors(EBB) of
	    [Succ|Succs]  ->
	      {[SuccCode|More], Visited1} = 
		ebbs([Succ] , [L|Visited], CFG),
	      {[OtherCode|MoreOther], Visited2} = 
		ebbs(Succs ++ EBBs, Visited1, CFG),
	      {[[hipe_icode:mk_label(L)|EBBCode] ++ SuccCode|
		More] ++ [OtherCode|MoreOther],
	       Visited2};
	    [] ->
	      {OtherCode, Visited1} = ebbs(EBBs, [L|Visited], CFG),
	      {[[hipe_icode:mk_label(L)|EBBCode] | OtherCode], Visited1}
	  end
      end;
    leaf ->
      ebbs(EBBs, Visited, CFG)
  end;
ebbs([],Visited,_) ->
  {[[]],Visited}.


visited(L, Visited) ->
  lists:member(L,Visited).

add_gc_tests([[]|EBBCodes]) -> add_gc_tests(EBBCodes);
add_gc_tests([EBBCode|EBBCodes]) ->
  case need(EBBCode,0, []) of
    {Need, RestCode, [Lbl|Code]}  ->
      if Need > 0 ->
	  [Lbl] ++ gc_test(Need) ++ Code ++ add_gc_tests([RestCode|EBBCodes]);
	 true ->
	  [Lbl|Code] ++ add_gc_tests([RestCode|EBBCodes])
      end;
    {0, RestCode, []} ->
      add_gc_tests([RestCode|EBBCodes])
  end;
add_gc_tests([]) -> [].

  
need([I|Is] , Need, Code) ->
  case split(I) of 
    true -> 
      {Need + need(I), Is,  lists:reverse([I|Code])};
    false ->
      need(Is, Need + need(I), [I|Code])
  end;
need([], Need, Code) ->
  {Need, [], lists:reverse(Code)}.

need(I) ->
  case hipe_icode:type(I) of 
    call ->
      primop_need(I);
    _ -> 
      0
  end.
	      
primop_need(I) ->
  case hipe_icode:call_fun(I) of
    cons ->
      2;
    mktuple ->
      length(hipe_icode:call_args(I)) + 1;
    {mkfun,MFA,MagicNum,Index} ->
      NumFree = length(hipe_icode:call_args(I)),
      ?ERL_FUN_SIZE + NumFree;
    _ ->
      0
  end.


gc_test(Need) ->
  L = hipe_icode:mk_new_label(),
  [hipe_icode:mk_primop([],{gc_test,Need},[],
		   hipe_icode:label_name(L),hipe_icode:label_name(L)),
   L].

split(I) ->
  case hipe_icode:type(I) of
    call -> split_primop(hipe_icode:call_fun(I));
    enter -> split_primop(hipe_icode:enter_fun(I));
    _ -> false
  end.

split_primop(Primop) ->
  not hipe_bif:known_heap_need(Primop).

