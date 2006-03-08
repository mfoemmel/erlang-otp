%% -*- erlang-indent-level: 2 -*-
%% $Id$
%% 
%% File: hipe_amd64_specific
%% This module defines interface to the amd64 backend
%% Copyright (C) Ulf Magnusson
%% Email: ulf.magnusson@ubm-computing.com

-module(hipe_amd64_specific).

-export([number_of_temporaries/1]).

% The following exports are used as M:F(...) calls from other modules;
%% e.g. hipe_amd64_ra_ls.
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
	 is_arg/1,	%% used by hipe_ls_regalloc
	 is_move/1,
	 is_fixed/1,	%% used by hipe_graph_coloring_regalloc
         is_global/1,
	 is_precoloured/1,
         reg_nr/1,
	 non_alloc/1,
	 allocatable/0,
         physical_name/1,
	 all_precoloured/0,
	 new_spill_index/1,	%% used by hipe_ls_regalloc
	 var_range/1,
         breadthorder/1,
         postorder/1,
         reverse_postorder/1]).

%% callbacks for hipe_regalloc_loop
-export([defun_to_cfg/1,
	 check_and_rewrite/2]).

defun_to_cfg(Defun) ->
  hipe_x86_cfg:init(Defun).

check_and_rewrite(Defun, Coloring) ->
  hipe_amd64_ra_postconditions:check_and_rewrite(Defun, Coloring, 'normal').

reverse_postorder(CFG) ->
  hipe_x86_cfg:reverse_postorder(CFG).

breadthorder(CFG) ->
  hipe_x86_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_x86_cfg:postorder(CFG).

%% Globally defined registers for linear scan
is_global(R) ->
  hipe_amd64_registers:temp1() =:= R orelse 
  hipe_amd64_registers:temp0() =:= R orelse
  hipe_amd64_registers:is_fixed(R).
 
is_fixed(R) ->
  hipe_amd64_registers:is_fixed(R).

is_arg(R) ->
  hipe_amd64_registers:is_arg(R).

args(CFG) ->
  hipe_amd64_registers:args(hipe_x86_cfg:arity(CFG)).
 
non_alloc(CFG) ->
  non_alloc(hipe_amd64_registers:nr_args(), hipe_x86_cfg:params(CFG)).

non_alloc(N, [_|Rest]) when N > 0 -> non_alloc(N-1, Rest);
non_alloc(_, Params) -> Params.

%% Liveness stuff

analyze(CFG) ->
  hipe_amd64_liveness:analyze(CFG).

livein(Liveness,L) ->
  [X || X <- hipe_amd64_liveness:livein(Liveness,L),
 	     hipe_x86:temp_is_allocatable(X),
 	     hipe_x86:temp_reg(X) /= hipe_amd64_registers:fcalls(),
 	     hipe_x86:temp_reg(X) /= hipe_amd64_registers:heap_limit(),
	     hipe_x86:temp_type(X) /= 'double'].

liveout(BB_in_out_liveness,Label) ->
  [X || X <- hipe_amd64_liveness:liveout(BB_in_out_liveness,Label),
 	     hipe_x86:temp_is_allocatable(X),
	     hipe_x86:temp_reg(X) /= hipe_amd64_registers:fcalls(),
	     hipe_x86:temp_reg(X) /= hipe_amd64_registers:heap_limit(),
	     hipe_x86:temp_type(X) /= 'double'].

%% Registers stuff

allocatable() ->
  hipe_amd64_registers:allocatable().

all_precoloured() ->
  hipe_amd64_registers:all_precoloured().

is_precoloured(Reg) ->
  hipe_amd64_registers:is_precoloured(Reg).

physical_name(Reg) ->
  Reg.

%% CFG stuff

succ_map(CFG) ->
  hipe_x86_cfg:succ_map(CFG).

labels(CFG) ->
  hipe_x86_cfg:labels(CFG).

var_range(_CFG) ->
  hipe_gensym:var_range(x86).

number_of_temporaries(_CFG) ->
  Highest_temporary = hipe_gensym:get_var(x86),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_x86_cfg:bb(CFG,L).

%% AMD64 stuff

def_use(Instruction) ->
  {[X || X <- hipe_amd64_defuse:insn_def(Instruction), 
 	   hipe_x86:temp_is_allocatable(X),
 	   hipe_x86:temp_type(X) /= 'double'],
   [X || X <- hipe_amd64_defuse:insn_use(Instruction), 
 	   hipe_x86:temp_is_allocatable(X),
	   hipe_x86:temp_type(X) /= 'double']
  }.

uses(I) ->
  [X || X <- hipe_amd64_defuse:insn_use(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     hipe_x86:temp_type(X) /= 'double'].

defines(I) ->
  [X || X <- hipe_amd64_defuse:insn_def(I),
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
