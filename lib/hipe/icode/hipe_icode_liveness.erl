%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ICODE LIVENESS ANALYSIS
%%

-module(hipe_icode_liveness).

-define(PRETTY_PRINT,true).
-include("../flow/liveness.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to CFG and icode.
%%

cfg_bb(CFG, L) ->
  hipe_icode_cfg:bb(CFG, L).

cfg_postorder(CFG) ->
  hipe_icode_cfg:postorder(CFG).

cfg_succ_map(CFG) ->
  hipe_icode_cfg:succ_map(CFG).

cfg_succ(CFG, L) ->
  hipe_icode_cfg:succ(CFG, L).

uses(Instr) ->
  hipe_icode:uses(Instr).

defines(Instr) ->
  hipe_icode:defines(Instr).

%%
%% This is the list of registers that are live at exit from a function
%%
cfg_labels(CFG) ->
  hipe_icode_cfg:labels(CFG).

liveout_no_succ() ->
  ordsets:new().

pp_liveness_info(LiveList) ->
 print_live_list(LiveList).

print_live_list([]) ->
  io:format(" none~n", []);
print_live_list([Last]) ->
  io:format(" ", []),
  print_var(Last),
  io:format("~n", []);
print_live_list([Var|Rest]) ->
  io:format(" ", []),
  print_var(Var),
  io:format(",", []), 
  print_live_list(Rest).

pp_block(Label, CFG) ->
  BB=hipe_icode_cfg:bb(CFG, Label),
  Code=hipe_bb:code(BB),
  hipe_icode_pp:pp_block(Code).

print_var({var, V, T}) ->
  case erl_types:t_is_none(T) of
    true->
      io:format("v~p", [V]);
    _ ->
      io:format("v~p (~s)", [V, erl_types:t_to_string(T)])
  end;
print_var({var, V}) ->
  io:format("v~p", [V]);
print_var({fvar, V}) ->
  io:format("fv~p", [V]);
print_var({reg, V}) -> 
  io:format("r~p", [V]).

%%
%% The following are used only if annotation of the code is requested.
%%
-ifdef(DEBUG_LIVENESS).
cfg_bb_add(CFG, L, NewBB) ->
  hipe_icode_cfg:bb_add(CFG, L, NewBB).

mk_comment(Text) ->
  hipe_icode:mk_comment(Text).
-endif.
