%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <02/05/13 14:57:54 happi>
%% ====================================================================
%%  Filename : 	hipe_icode_cleanup.erl
%%  Module   :	hipe_icode_cleanup
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-11-07 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2002/05/13 16:51:07 $
%%              $Revision: 1.4 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_cleanup).
-export([code/1]).

code(Icode) ->
  {_LMin,LMax} = hipe_icode:icode_label_range(Icode),
  hipe_gensym:set_label(icode,LMax+1),
  {_VMin,VMax} = hipe_icode:icode_var_range(Icode),
  hipe_gensym:set_var(icode,VMax+1),
  Code = hipe_icode:icode_code(Icode),
  NewCode = cleanup_code(Code),
  hipe_icode:icode_code_update(Icode,NewCode).


cleanup_code(Code) ->
  Catches = find_catch_entries(Code),
  cleanup_code(Code, [], Catches).

cleanup_code([I|Is], Acc, Catches) ->
  cleanup_code(Is, cleanup_instr(I, Catches) ++ Acc, Catches);
cleanup_code([], Acc, _) -> lists:reverse(Acc).


cleanup_instr(I, Catches) ->
  case hipe_icode:type(I) of
    call ->
      call_to_primop(I);
    enter ->
      enter_to_primop(I);
    label ->
      case lists:member(hipe_icode:label_name(I), Catches) of
	true -> 
	  case lists:member(entry,hipe_icode:info(I)) of
	    true -> [I];
	    false -> [hipe_icode:info_add(I, entry)]
	  end;
	false -> [I]
      end;
    _Other -> [I]
  end.


call_to_primop(I) ->
  Fun =  hipe_icode:call_fun(I), 
  case Fun of
    {erlang,exit,1} ->
      Args = hipe_icode:call_args(I),
      [hipe_icode:mk_fail(Args, exit)];
    {erlang,throw,1} ->
      Args = hipe_icode:call_args(I),
      [hipe_icode:mk_fail(Args, throw)];
    {erlang,fault,1} ->
      Args = hipe_icode:call_args(I),
      [hipe_icode:mk_fail(Args, fault)];
    {erlang,fault,2} ->
      Args = hipe_icode:call_args(I),
      [hipe_icode:mk_fail(Args, fault2)];
    _ -> 
      [I]
  end.

enter_to_primop(I) ->
  Fun = hipe_icode:enter_fun(I), 
  case Fun of
    {erlang,exit,1} ->
      Args = hipe_icode:enter_args(I),
      [hipe_icode:mk_fail(Args, exit)];
    {erlang,throw,1} ->
      Args = hipe_icode:enter_args(I),
      [hipe_icode:mk_fail(Args, throw)];
    {erlang,fault,1} ->
      Args = hipe_icode:enter_args(I),
      [hipe_icode:mk_fail(Args, fault)];
    {erlang,fault,2} ->
      Args = hipe_icode:enter_args(I),
      [hipe_icode:mk_fail(Args, fault2)];
    _ ->
      [I]
  end.


find_catch_entries(Code) ->
  find_catch_entries(Code,[]).

find_catch_entries([I|Is], Acc) ->
  case hipe_icode:type(I) of
    pushcatch ->
      find_catch_entries(Is, [hipe_icode:pushcatch_label(I)|Acc]);
    _ ->
      find_catch_entries(Is, Acc)
  end;
find_catch_entries([],Acc) -> Acc.
