%%% -*- Erlang -*-
%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : hipe_ssa_copy_prop.inc
%%% Author  : Tobias Lindahl <tobiasl@fan.it.uu.se>
%%% Description : Copy propagation on ssa form.
%%%
%%% Created :  4 Apr 2003 by Tobias Lindahl <tobiasl@fan.it.uu.se>
%%%-------------------------------------------------------------------

-export([cfg/1]).

%%--------------------------------------------------------------------
%% Two passes through the code choosing the blocks in reverse
%% postorder. The first pass binds all destinations of copying moves
%% to the sources and propagates the copies, the second propagates the
%% copies and removes the copying moves. 
%%
%% Copies must not be propagated across the point of redefinition of
%% the source variable. If the original value is used after the
%% redefinition we must use this value and cannot remove the copying
%% move.
%% %%--------------------------------------------------------------------

cfg(Cfg)->
  Labels = ?cfg:reverse_postorder(Cfg),
  {Info, NewCfg1} = propagate(Labels, Cfg, gb_trees:empty(), fun first_pass/3),
  {_, NewCfg2} = propagate(Labels, NewCfg1, Info, fun second_pass/3),
  NewCfg2.

propagate([Label|Left], Cfg, Info, Fun)->
  BB = ?cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  {NewInfo, NewCode} = Fun(Code, Info, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  propagate(Left, ?cfg:bb_update(Cfg, Label, NewBB), NewInfo, Fun);
propagate([], Cfg, Info, _Fun) ->
  {Info, Cfg}.

first_pass([I|Left], Info, Acc)->
  case ?code:type(I) of
    mov ->
      NewInfo = get_info_mov_or_fmov(I, Info),
      first_pass(Left, NewInfo, [I|Acc]);
    fmov ->
      NewInfo = get_info_mov_or_fmov(I, Info),
      first_pass(Left, NewInfo, [I|Acc]);
    _ ->
      {_, NewI} = propagate_instr(I, Info),
      first_pass(Left, Info, [NewI|Acc])
  end;
first_pass([], Info, Acc) ->
  {Info, lists:reverse(Acc)}.

get_info_mov_or_fmov(I, Info)->
  case ?code:uses(I) of
    [] -> %% Constant.
      Info;
    [Src] ->
      add_binding(?code:defines(I), Src, Info)
  end.

second_pass([I|Left], Info, Acc)->
  case ?code:type(I) of
    mov ->
      NewI = propagate_mov_or_fmov(I, Info),
      second_pass(Left, Info, NewI++Acc);
    fmov ->
      NewI = propagate_mov_or_fmov(I, Info),
      second_pass(Left, Info, NewI++Acc);
    _ ->      
      {NewInfo1, NewI} = propagate_instr(I, Info),
      NewInfo2 = add_binding(?code:defines(I), 'redefined', NewInfo1),
      second_pass(Left, NewInfo2, [NewI|Acc])
  end;
second_pass([], Info, Acc) ->
  {Info, lists:reverse(Acc)}.

propagate_mov_or_fmov(I, Info)->
  case ?code:uses(I) of
    [] ->%% Constant
      [I];
    _ ->
      case gb_trees:lookup(hd(?code:defines(I)), Info) of
	none -> %% We must keep this instruction.
	  [I];
	_ ->
	  []
      end
  end.

propagate_instr(I, Info)->
  propagate_instr0(I, ?code:uses(I), Info, []).

propagate_instr0(I, [Key|Left], Info, UpdateInfo)->
  case gb_trees:lookup(Key, Info) of
    {value, 'redefined'} ->
      propagate_instr0(I, Left, Info, UpdateInfo);
    {value, Val} ->
      case  gb_trees:lookup(Val, Info) of
	{value, 'redefined'} ->
	  %%Remove the binding to show that the copying move cannot be removed.
	  propagate_instr0(I, Left, gb_trees:delete(Key, Info), UpdateInfo);
	_ ->
	  propagate_instr0(I, Left, Info, [{Key, Val}|UpdateInfo])
      end;
    _ ->
      propagate_instr0(I, Left, Info, UpdateInfo)
  end;
propagate_instr0(I, [], Info, UpdateInfo)->
  {Info, ?code:subst(UpdateInfo, I)}.

add_binding([Key|Left], Val, Info)->
  %% Make sure the key is bound to the end of any copy-chains.
  NewInfo = 
    case gb_trees:lookup(Val, Info) of
      {value, NewVal} ->
	gb_trees:insert(Key, NewVal, Info);
      none ->
	gb_trees:insert(Key, Val, Info)
    end,
  add_binding(Left, Val, NewInfo);
add_binding([], _, Info) ->
  Info.
