%% $Id$
%% hipe_x86_specific
%% This module defines interface to the x86 backend
%% Copyright (C) Ulf Magnusson
%% Email: ulf.magnusson@ubm-computing.com

-module(hipe_x86_specific).

-export([analyze/1,
	 liveout/2,
	 livein/2,
	 allocatable/0,
	 all_precolored/0,
	 is_precolored/1,
	 physical_name/1,
	 labels/1,
	 var_range/1,
	 number_of_temporaries/1,
	 bb/2,
	 def_use/1,
	 is_move/1,
	 reg_nr/1,
	 is_global/1,
	 is_fixed/1,
	 is_arg/1,
	 args/1,
	 uses/1,
	 defines/1,
	 succ_map/1,
	 non_alloc/1,
	 reverse_postorder/1,
	 breadthorder/1,
	 postorder/1,
	 new_spill_index/1]).

reverse_postorder(CFG) ->
  hipe_x86_cfg:reverse_postorder(CFG).

breadthorder(CFG) ->
  hipe_x86_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_x86_cfg:postorder(CFG).

is_global(R) ->
  hipe_x86_registers:is_fixed(R).

is_fixed(R) ->
  hipe_x86_registers:is_fixed(R).

is_arg(R) ->
  hipe_x86_registers:is_arg(R).

args(CFG) ->
  hipe_x86_registers:args(hipe_x86_cfg:arity(CFG)).

non_alloc(CFG) ->
  non_alloc(hipe_x86_registers:nr_args(), hipe_x86_cfg:params(CFG)).

non_alloc(N, [_|Rest]) when N > 0 -> non_alloc(N-1, Rest);
non_alloc(_, Params) -> Params.

%% Liveness stuff

analyze(CFG) ->
  hipe_x86_liveness:analyze(CFG).

livein(Liveness,L) ->
    [X ||
	X <- hipe_x86_liveness:livein(Liveness,L),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_reg(X) /= hipe_x86_registers:fcalls(),
	hipe_x86:temp_reg(X) /= hipe_x86_registers:heap_limit(),
	hipe_x86:temp_type(X) /= 'double'].

liveout(BB_in_out_liveness,Label) ->
    [X ||
	X <- hipe_x86_liveness:liveout(BB_in_out_liveness,Label),
	hipe_x86:temp_is_allocatable(X),
	hipe_x86:temp_reg(X) /= hipe_x86_registers:fcalls(),
	hipe_x86:temp_reg(X) /= hipe_x86_registers:heap_limit(),
	hipe_x86:temp_type(X) /= 'double'].

%% Registers stuff

allocatable() ->
  hipe_x86_registers:allocatable().

all_precolored() ->
  hipe_x86_registers:all_precoloured().

is_precolored(Reg) ->
  hipe_x86_registers:is_precoloured(Reg).
%% or hipe_x86_registers:is_spill(Reg).

physical_name(Reg) ->
  Reg.

%% CFG stuff

succ_map(CFG) ->
  hipe_x86_cfg:succ_map(CFG).

labels(CFG) ->
  hipe_x86_cfg:labels(CFG).

var_range(CFG) ->
  {Min,Max} = hipe_x86_cfg:var_range(CFG),
  {Min,Max}.

number_of_temporaries(CFG) ->
  {_, Highest_temporary} = hipe_x86_cfg:var_range(CFG),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_x86_cfg:bb(CFG,L).

%% X86 stuff

def_use(Instruction) ->
    {[X || X <- hipe_x86_defuse:insn_def(Instruction), 
	   hipe_x86:temp_is_allocatable(X),
	   hipe_x86:temp_type(X) /= 'double'],
     [X || X <- hipe_x86_defuse:insn_use(Instruction), 
	   hipe_x86:temp_is_allocatable(X),
	   hipe_x86:temp_type(X) /= 'double']
    }.

uses(I) ->
    [X || X <- hipe_x86_defuse:insn_use(I),
	  hipe_x86:temp_is_allocatable(X),
	  hipe_x86:temp_type(X) /= 'double'].
defines(I) ->
    [X || X <- hipe_x86_defuse:insn_def(I),
	  hipe_x86:temp_is_allocatable(X),
	  hipe_x86:temp_type(X) /= 'double'].

is_move(Instruction) ->
  case hipe_x86:is_move(Instruction) of
    true ->
      Src = hipe_x86:move_src(Instruction),
      Dst = hipe_x86:move_dst(Instruction),
      case hipe_x86:is_temp(Src) of
	true ->
	  case hipe_x86:temp_is_allocatable(Src) of
	    true ->
	      case hipe_x86:is_temp(Dst) of
		true ->
		  hipe_x86:temp_is_allocatable(Dst);
		false -> false
	      end;
	    false -> false
	  end;
	false -> false
      end;
    false -> false
  end.

reg_nr(Reg) ->
  hipe_x86:temp_reg(Reg).

new_spill_index(SpillIndex)->
  SpillIndex+1.
