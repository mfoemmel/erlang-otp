%% $Id$
%%
%% File   : hipe_x86_specific_fp.erl
%% Purpose: This module defines interface to the x86 backend
%%

-module(hipe_x86_specific_fp).
-export([allocatable/0,
	 is_precoloured/1,
	 %% var_range/1,
	 %% def_use/1,
	 %% is_fixed/1,
	 %% is_arg/1,
	 %% non_alloc/1,
	 %% new_spill_index/1,
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
	 is_global/1,
	 reg_nr/1,
	 physical_name/1,
	 breadthorder/1,
	 postorder/1,
 	 reverse_postorder/1]).

breadthorder(CFG) ->
  hipe_x86_cfg:breadthorder(CFG).
postorder(CFG) ->
  hipe_x86_cfg:postorder(CFG).
reverse_postorder(CFG) ->
  hipe_x86_cfg:reverse_postorder(CFG).

is_global(_) ->
  false.

%% is_fixed(_) ->
%%   false.
%% 
%% is_arg(_) ->
%%   false.

args(_) ->
  [].

%% non_alloc(_) ->
%%   [].

%% Liveness stuff

analyze(CFG) ->
  hipe_x86_liveness:analyze(CFG).

livein(Liveness,L) ->
  [X || X <- hipe_x86_liveness:livein(Liveness,L),
 	     hipe_x86:temp_is_allocatable(X),
 	     hipe_x86:temp_type(X)=='double'].

liveout(BB_in_out_liveness,Label) ->
  [X || X <- hipe_x86_liveness:liveout(BB_in_out_liveness,Label),
	     hipe_x86:temp_is_allocatable(X),
	     hipe_x86:temp_type(X) == 'double'].

%% Registers stuff

allocatable() ->
    [0,1,2,3,4,5,6].

all_precoloured() ->
     allocatable().

is_precoloured(X) ->
    lists:member(X,all_precoloured()).

physical_name(Reg) ->
  Reg.

%% CFG stuff

succ_map(CFG) ->
  hipe_x86_cfg:succ_map(CFG).

labels(CFG) ->
  hipe_x86_cfg:labels(CFG).

%% var_range(_CFG) ->
%%   {Min,Max} = hipe_gensym:var_range(x86),
%%   %% io:format("Var_range: ~w\n",[{Min,Max}]),
%%   {Min,Max}.

number_of_temporaries(_CFG) ->
  Highest_temporary = hipe_gensym:get_var(x86),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_x86_cfg:bb(CFG,L).

%% X86 stuff

%% def_use(Instruction) ->
%%     {[X || X <- hipe_x86_defuse:insn_def(Instruction), 
%% 	   hipe_x86:temp_is_allocatable(X),
%% 	   temp_is_double(X)],
%%      [X || X <- hipe_x86_defuse:insn_use(Instruction), 
%% 	   hipe_x86:temp_is_allocatable(X),
%% 	   temp_is_double(X)]
%%     }.
%% 
uses(I) ->
  [X || X <- hipe_x86_defuse:insn_use(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     temp_is_double(X)].
 
defines(I) ->
  [X || X <- hipe_x86_defuse:insn_def(I),
 	     hipe_x86:temp_is_allocatable(X),
 	     temp_is_double(X)].
 
temp_is_double(Temp)->
  hipe_x86:temp_type(Temp) == 'double'.

reg_nr(Reg) ->
  hipe_x86:temp_reg(Reg).
 
%% new_spill_index(SpillIndex)->
%%   SpillIndex+1.
