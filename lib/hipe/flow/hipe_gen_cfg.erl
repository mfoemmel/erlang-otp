-module(hipe_gen_cfg).

-export([function/1,
	 params/1,
	 params_update/2,
	 pp/2,
	 linearize/1]).

%% To avoid warnings...
-export([find_new_label/2]).

%%-define(DO_ASSERT, true).
-include("../main/hipe.hrl").
-include("../flow/cfg.inc").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface to generic code
%

init(_) ->
  ?error_msg("Can not create a generic CFG",[]),
  ?EXIT(dont_call_this_function).

is_label(_Instr) ->
   exit(nyi).

label_name(_Instr) ->
  exit(nyi).

label_annot(_Lbl) ->
    exit(nyi).

mk_label(_Name, _Annot) ->
  exit(nyi).

mk_goto(_Name) ->
  exit(nyi).

branch_successors(_Instr) ->
  exit(nyi).
is_comment(_Instr) ->
  exit(nyi).
is_goto(_Instr) ->
  exit(nyi).
is_branch(_Instr) ->
  exit(nyi).
redirect_jmp(_Jmp, _ToOld, _ToNew) ->
  exit(nyi).
redirect_ops(_,_CFG,_) -> 
  exit(nyi).
pp(_CFG) ->
  exit(nyi).
pp(_Dev, _CFG) ->
  exit(nyi).
linearize(_CFG) ->
  exit(nyi).



