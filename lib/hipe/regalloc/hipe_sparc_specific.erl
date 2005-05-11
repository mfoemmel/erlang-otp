%%----------------------------------------------------------------------
%% File    : hipe_sparc_specific.erl
%% Author  : Ingemar Åberg <d95ina@it.uu.se>
%% Purpose : Provide target specific functions to the register allocator
%% Created :  2 Apr 2000 by Ingemar Åberg <d95ina@it.uu.se>
%%----------------------------------------------------------------------

-module(hipe_sparc_specific).
-author('d95ina@it.uu.se').

-export([number_of_temporaries/1]).

%% The following exports are (also) used as M:F(...) calls from other
%% modules; e.g. hipe_x86_ra_ls.
-export([analyze/1,
	 bb/2,
	 args/1,
	 labels/1,
	 livein/2,
	 liveout/2,
	 succ_map/1,
	 uses/1,
	 defines/1,
	 def_use/1,
	 is_arg/1,      %% used by hipe_ls_regalloc
	 is_move/1,
	 is_fixed/1,    %% used by hipe_graph_coloring_regalloc
	 is_global/1,
	 is_precoloured/1,
	 reg_nr/1,
	 non_alloc/1,
	 allocatable/0,
	 physical_name/1,
	 all_precoloured/0,
	 new_spill_index/1,     %% used by hipe_ls_regalloc
	 var_range/1,
	 breadthorder/1,
	 preorder/1,
	 postorder/1,
	 inorder/1,
	 reverse_inorder/1,
	 predictionorder/1,
	 reverse_postorder/1]).


%% ---------------- Liveness stuff ----------------

analyze(CFG) ->
    hipe_sparc_liveness:analyze(CFG).

liveout(BB_in_out_liveness,Label) ->
    hipe_sparc_liveness:liveout(BB_in_out_liveness,Label).

livein(Liveness,L) ->
    hipe_sparc_liveness:livein(Liveness,L).

%% ---------------- Registers stuff ----------------

allocatable() ->
    hipe_sparc_registers:allocatable().

all_precoloured() ->
    hipe_sparc_registers:all_precoloured().

is_precoloured(Reg) ->
    hipe_sparc_registers:is_precoloured(Reg).

physical_name(Reg) ->
    hipe_sparc_registers:physical_name(Reg).

is_global(R) ->
  not lists:member(R,hipe_sparc_registers:allocatable()).

is_fixed(R) ->
  hipe_sparc_registers:is_fixed(R).

all_args() ->
  [hipe_sparc_registers:arg(0),
   hipe_sparc_registers:arg(1),
   hipe_sparc_registers:arg(2),
   hipe_sparc_registers:arg(3),
   hipe_sparc_registers:arg(4),
   hipe_sparc_registers:arg(5),
   hipe_sparc_registers:arg(6),
   hipe_sparc_registers:arg(7),
   hipe_sparc_registers:arg(8),
   hipe_sparc_registers:arg(9),
   hipe_sparc_registers:arg(10),
   hipe_sparc_registers:arg(11),
   hipe_sparc_registers:arg(12),
   hipe_sparc_registers:arg(13),
   hipe_sparc_registers:arg(14),
   hipe_sparc_registers:arg(15)].

is_arg(R) ->
  lists:member(R,all_args()).

%% CFG stuff

%% Return registers that are used to pass arguments to the CFG.
args(CFG) ->
  Arity = arity(CFG),
  arg_vars(Arity).

non_alloc(_CFG) ->
  [].

arg_vars(N, Acc) when N >= 0 ->
  arg_vars(N-1, [arg_var(N)|Acc]);
arg_vars(_, Acc) -> Acc.

arg_vars(N) ->
  case N >= hipe_sparc_registers:register_args() of
    false ->
      arg_vars(N-1,[]);
    true ->
      arg_vars(hipe_sparc_registers:register_args()-1,[])
  end.

arg_var(X) ->
  hipe_sparc_registers:arg(X).

arity(_CFG) ->
  %% XXX: This really should retrieve the arity from the MFA or
  %% the CFG's arity field (extra), but both alternatives break
  %% linear scan on SPARC due to some unknown bug. Using the
  %% maximum number of argument registers is a kludge which
  %% forces linear scan to behave exactly as it did before.
  hipe_sparc_registers:register_args().

labels(CFG) ->
  hipe_sparc_cfg:labels(CFG).

reverse_postorder(CFG) ->
  hipe_sparc_cfg:reverse_postorder(CFG).

breadthorder(CFG) ->
  hipe_sparc_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_sparc_cfg:postorder(CFG).

predictionorder(CFG) ->
  hipe_sparc_cfg:predictionorder(CFG).

inorder(CFG) ->
  hipe_sparc_cfg:inorder(CFG).

reverse_inorder(CFG) ->
  hipe_sparc_cfg:reverse_inorder(CFG).

preorder(CFG) ->
  hipe_sparc_cfg:preorder(CFG).


var_range(_CFG) ->
  hipe_gensym:var_range(sparc).

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
  hipe_sparc:keep_registers(hipe_sparc:uses(I)).

defines(I) ->
  hipe_sparc:keep_registers(hipe_sparc:defines(I)).

def_use(Instruction) ->
  {D,U} = hipe_sparc:def_use(Instruction),
  {hipe_sparc:keep_registers(D),hipe_sparc:keep_registers(U)}.

is_move(Instruction) ->
  hipe_sparc:is_move(Instruction).

reg_nr(Reg) ->
  hipe_sparc:reg_nr(Reg).

new_spill_index(SpillIndex)->
  SpillIndex + 1.
