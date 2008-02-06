%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_spillcost).

-export([new/1,
	 inc_costs/2,
	 ref_in_bb/2,
	 spill_cost/2]).
%% The following is exported only for debugging purposes.
-ifdef(DEBUG_PRINTOUTS).
-export([nr_of_use/2]).
-endif.

-type(hipe_array() :: integer()).

-record(spill_cost,
	{uses	 :: hipe_array(),	% number of uses of each temp
	 bb_uses :: hipe_array()	% number of basic blocks each temp occurs in
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
  lists:foreach(fun (T) -> inc_use(T, Uses) end, Temps),
  SC. % updated via side-effects

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
  lists:foreach(fun (T) -> inc_bb_use(T, BBUses) end, Temps),
  SC. % updated via side-effects

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
