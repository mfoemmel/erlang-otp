%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_spillcost).

-export([new/1,
	 inc_costs/2,
	 ref_in_bb/2,
	 spill_cost/2,
	 nr_of_use/2]). %% only for debugging

-record(spill_cost,
	{uses		% number of uses of each temp
	 ,bb_uses	% number of basic blocks each temp occurs in
	}).

new(NrTemps) ->
  #spill_cost{uses = hipe_bifs:array(NrTemps, 0),
	      bb_uses = hipe_bifs:array(NrTemps, 0)}.

%%----------------------------------------------------------------------
%% Function:    inc_costs
%%
%% Description: Registers usage of a list of temporaries (for spill_cost)
%%----------------------------------------------------------------------

inc_costs(Temps, SC) ->
  Uses = SC#spill_cost.uses,
  inc_uses(Temps, Uses),
  SC. % updated via side-effects

inc_uses([], _Uses) -> [];
inc_uses([T|Ts], Uses) ->
  inc_use(T, Uses),
  inc_uses(Ts, Uses).

inc_use(Temp, Uses) ->
  hipe_bifs:array_update(Uses, Temp, get_uses(Temp, Uses) + 1).

nr_of_use(Temp, SC) ->
  get_uses(Temp, SC#spill_cost.uses).

get_uses(Temp, Uses) ->
  hipe_bifs:array_sub(Uses, Temp).

%%----------------------------------------------------------------------
%% Function:    ref_in_bb
%%
%% Description: Registers that a set of temporaries are used in one basic
%%              block; should be done exactly once per basic block
%%----------------------------------------------------------------------

ref_in_bb(Temps, SC) ->
  BBUses = SC#spill_cost.bb_uses,
  inc_bb_uses(Temps, BBUses),
  SC. % updated via side-effects

inc_bb_uses([], _BBUses) -> [];
inc_bb_uses([T|Ts], BBUses) ->
  inc_bb_use(T, BBUses),
  inc_bb_uses(Ts, BBUses).

inc_bb_use(Temp, BBUses) ->
  hipe_bifs:array_update(BBUses, Temp, get_bb_uses(Temp, BBUses) + 1).

bb_use(Temp, SC) ->
  get_bb_uses(Temp, SC#spill_cost.bb_uses).

get_bb_uses(Temp, BBUses) ->
  hipe_bifs:array_sub(BBUses, Temp).

%%----------------------------------------------------------------------
%% Function:    spill_cost
%%
%% Description: Computes a spill cost for a temporary
%%   
%% Returns:
%%   Spill cost (a real number -- higher means worse to spill)
%%----------------------------------------------------------------------

spill_cost(Temp, SC) ->
  nr_of_use(Temp, SC) / bb_use(Temp, SC).
