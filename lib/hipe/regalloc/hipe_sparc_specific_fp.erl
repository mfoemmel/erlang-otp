%%----------------------------------------------------------------------
%% File    : hipe_sparc_specific_fp.erl
%% Author  : Ingemar Åberg <d95ina@it.uu.se>
%% Purpose : Provide target specific functions to the register allocator
%% Created :  2 Apr 2000 by Ingemar Åberg <d95ina@it.uu.se>
%%----------------------------------------------------------------------

-module(hipe_sparc_specific_fp).
-author('d95ina@it.uu.se').

-export([allocatable/0,
	 is_precoloured/1,
	 %% var_range/1,
	 %% def_use/1,
	 %% is_fixed/1,
	 %% non_alloc/1,

	 %% predictionorder/1,
	 %% is_move/1,
	 number_of_temporaries/1
	]).

%% The following exports are used as M:F(...) calls from other modules;
%% e.g. hipe_x86_ra_ls.
-export([analyze/1,
	 bb/2,
	 args/1,
	 labels/1,
	 livein/2,
	 liveout/2,
	 succ_map/1,
	 uses/1,
	 defines/1,
	 is_arg/1,
	 is_global/1,
	 reg_nr/1,
	 physical_name/1,
	 new_spill_index/1,
	 breadthorder/1,
	 postorder/1,
	 reverse_postorder/1]).

%% Liveness stuff

analyze(CFG) ->
  hipe_sparc_liveness:analyze(CFG).

liveout(BB_in_out_liveness,Label) ->
  hipe_sparc_liveness:liveout(BB_in_out_liveness,Label).

livein(Liveness,L) ->
  hipe_sparc_liveness:livein(Liveness,L).

%% Registers stuff

allocatable() ->
  [4,6,8,10,12,14,16,18,20,22,24,26,28,30].

all_precoloured() -> %% Is this correct?
  lists:seq(0, 31).

is_precoloured(Reg) ->
  lists:member(Reg, all_precoloured()).

physical_name(Reg) ->
  Reg.

is_global(_R) -> %% A fp_reg can't be global.
  false.

%% is_fixed(_R) -> %% A fp_reg can't be fixed.
%%   false.

is_arg(_R) ->
  false.

%% CFG stuff

%% Return registers that are used to pass arguments to the CFG.
args(_CFG) ->
  [].
%%  Arity = arity(CFG),
%%  arg_vars(Arity).

%% non_alloc(_CFG) ->
%%   [].
%% 
%% arg_vars(N, Acc) when N >= 0 ->
%%   arg_vars(N-1, [arg_var(N)|Acc]);
%% arg_vars(_, Acc) -> Acc.
%%
%% arg_vars(N) ->
%%   case N >= hipe_sparc_registers:register_args() of
%%     false ->
%%       arg_vars(N-1,[]);
%%     true ->
%%       arg_vars(hipe_sparc_registers:register_args()-1,[])
%%   end.
%% 
%% arg_var(X) ->
%%   hipe_sparc_registers:arg(X).
%%
%% arity(CFG) ->
%%   {_,_,Arity} = hipe_sparc_cfg:function(CFG). 

labels(CFG) ->
  hipe_sparc_cfg:labels(CFG).

reverse_postorder(CFG) ->
  hipe_sparc_cfg:reverse_postorder(CFG).

breadthorder(CFG) ->
  hipe_sparc_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_sparc_cfg:postorder(CFG).

%% predictionorder(CFG) ->
%%   hipe_sparc_cfg:predictionorder(CFG).


%% var_range(_CFG) ->
%%   hipe_gensym:get_var(sparc).

number_of_temporaries(_CFG) ->
  Highest_temporary = hipe_gensym:get_var(sparc),
  %% Since we can have temps from 0 to Max adjust by +1.
  %% (Well, on sparc this is not entirely true, but lets pretend...)
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_sparc_cfg:bb(CFG,L).

succ_map(CFG) ->
  hipe_sparc_cfg:succ_map(CFG).

uses(I) ->
  hipe_sparc:fp_reg_uses(I).

defines(I) ->
  hipe_sparc:fp_reg_defines(I).

%% def_use(Instruction) ->
%%   hipe_sparc:fp_reg_def_use(Instruction).

%% is_move(Instruction) ->
%%     hipe_sparc:is_move(Instruction).

reg_nr(Reg) ->
  hipe_sparc:fpreg_nr(Reg).

new_spill_index(SpillIndex)->
  SpillIndex + 2.
