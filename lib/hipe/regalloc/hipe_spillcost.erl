%%----------------------------------------------------------------------
%% File    : hipe_spillcost.erl
%% Author  : Andreas Wallin <d96awa@ida.dis.uu.se>, Thorild Selén
%% Purpose : Be the biggest the best, better than the rest.
%% Created : 31 Mar 2000 by Andreas Wallin <d96awa@ida.dis.uu.se>
%%----------------------------------------------------------------------

-module(hipe_spillcost).
-author(['d96awa@ida.dis.uu.se', 'd95ths@csd.uu.se']).

-export([new/1,
	 inc_cost/2,
	 inc_costs/2,
	 ref_in_bb/2,
	 spill_cost/2 
	]).

-record(spill_cost,
	{uses,       % Vector noting, for each temporary, how many times it is used
	 bb_uses     % Vector saying in how many basic blocks each temporary is
                     %  referred to
	}).

%%----------------------------------------------------------------------
%% Function:    new
%%
%% Description: Creates new structure for spill cost computation
%%
%% Parameters:
%%   No_temporaries     -- Number of temporaries
%%   
%% Returns:
%%   Empty spill_cost structure
%%
%%----------------------------------------------------------------------

new(No_temporaries) ->
    #spill_cost{uses = hipe_vectors_wrapper:empty(No_temporaries, 0),
		bb_uses = hipe_vectors_wrapper:empty(No_temporaries, 0)}.

%%----------------------------------------------------------------------
%% Function:    inc_costs
%%
%% Description: Registers usage of a list of temporaries (for spill_cost)
%%
%% Parameters:
%%   [T|Ts]              -- List of temporaries
%%   IG                  -- Inference graph
%%   
%% Returns:
%%   Inference graph with updated spill cost information
%%
%%----------------------------------------------------------------------

inc_costs([], IG) -> IG;
inc_costs([T|Ts], IG) ->
    inc_costs(Ts, inc_cost(T, IG)).    

%%----------------------------------------------------------------------
%% Function:    inc_cost
%%
%% Description: Registers usage of a temporary (for spill_cost)
%%
%% Parameters:
%%   Temporary           -- Temporary
%%   IG                  -- Inference graph
%%   
%% Returns:
%%   Inference graph with updated spill cost information
%%
%%----------------------------------------------------------------------

inc_cost(Temporary, IG) ->
    Spill_costs = hipe_ig:spill_costs(IG),
    Spill_costs0 = set_nr_of_use(Temporary,
				 nr_of_use(Temporary, Spill_costs) + 1,
				 Spill_costs),
    hipe_ig:set_spill_costs(Spill_costs0, IG).

%%----------------------------------------------------------------------
%% Function:    ref_in_bb
%%
%% Description: Registers that a set of temporaries are used in one basic
%%              block; should be done exactly once per basic block
%%
%% Parameters:
%%   Ref                 -- Set of temporaries used in the basic block
%%   IG                  -- Inference graph
%%   
%% Returns:
%%   Inference graph with updated spill cost information
%%
%%----------------------------------------------------------------------

ref_in_bb(Ref, IG) ->
    hipe_ig:set_spill_costs(ref_in_bb(reflist, Ref,
				      hipe_ig:spill_costs(IG)),
			    IG).
%% The symbol reflist means that we've converted the set to a list now
%% (and that we've extracted the spill cost info from the IG)
ref_in_bb(reflist, [], Spill_costs) -> Spill_costs;
ref_in_bb(reflist, [R|Rs], Spill_costs) ->
    New_spill_costs = inc_bb_use(R, Spill_costs),
    ref_in_bb(reflist, Rs, New_spill_costs).

%%----------------------------------------------------------------------
%% Function:    nr_of_use
%%
%% Description: Returns the number of uses of a temporary
%%
%% Parameters:
%%   Temporary           -- The temporary
%%   Spill_costs         -- Spill cost information
%%   
%% Returns:
%%   Number of uses of the temporary
%%
%%----------------------------------------------------------------------

nr_of_use(Temporary, Spill_costs) ->
    hipe_vectors_wrapper:get(Spill_costs#spill_cost.uses, Temporary).

%%----------------------------------------------------------------------
%% Function:    set_nr_of_use
%%
%% Description: Records the number of uses of a temporary
%%
%% Parameters:
%%   Temporary           -- The temporary
%%   Nr_of_use           -- Number of uses
%%   Spill_costs         -- Old spill cost information
%%   
%% Returns:
%%   New spill cost information
%%
%%----------------------------------------------------------------------

set_nr_of_use(Temporary, Nr_of_use, Spill_costs) ->
    Spill_costs#spill_cost{uses = hipe_vectors_wrapper:set(Spill_costs#spill_cost.uses,
						   Temporary, Nr_of_use)}.

%%----------------------------------------------------------------------
%% Function:    inc_bb_use
%%
%% Description: Increases the number of basic blocks using a temporary
%%
%% Parameters:
%%   Temporary           -- The temporary
%%   Spill_costs         -- Old spill cost information
%%   
%% Returns:
%%   New spill cost information
%%
%%----------------------------------------------------------------------

inc_bb_use(Temporary, Spill_costs) ->
    Old_bb_use = hipe_vectors_wrapper:get(Spill_costs#spill_cost.bb_uses, Temporary),
    Spill_costs#spill_cost{bb_uses =
			   hipe_vectors_wrapper:set(Spill_costs#spill_cost.bb_uses,
					    Temporary, Old_bb_use + 1)}.

%%----------------------------------------------------------------------
%% Function:    inc_bb_use
%%
%% Description: Gives basic block usage information for a temporary
%%
%% Parameters:
%%   Temporary           -- The temporary
%%   Spill_costs         -- Spill cost information
%%   
%% Returns:
%%   The number of basic blocks where the temporary is used
%%
%%----------------------------------------------------------------------

bb_use(Temporary, Spill_costs) ->
    hipe_vectors_wrapper:get(Spill_costs#spill_cost.bb_uses, Temporary).

%%----------------------------------------------------------------------
%% Function:    spill_cost
%%
%% Description: Computes a spill cost for a temporary
%%
%% Parameters:
%%   Temporary           -- The temporary
%%   IG                  -- Inference graph
%%   
%% Returns:
%%   Spill cost (a real number -- higher means worse to spill)
%%
%%----------------------------------------------------------------------

spill_cost(Temporary, IG) ->
%    Degree      = hipe_ig:degree(IG),
    Spill_costs = hipe_ig:spill_costs(IG),
%    Adj_list    = hipe_ig:adj_list(IG),
%    Neighbours_degree = count_neighbours_degree(adj_list:edges(Temporary, Adj_list),
%						Degree),
%    (nr_of_use(Temporary, Spill_costs) + Neighbours_degree) / (bb_use(Temporary, Spill_costs) + hipe_degree:degree(Temporary, Degree)).
%    nr_of_use(Temporary, Spill_costs) / hipe_degree:degree(Temporary, Degree).
    nr_of_use(Temporary, Spill_costs) / bb_use(Temporary, Spill_costs).

%%----------------------------------------------------------------------
%% Function:    count_neighbours_degree
%%
%% Description: Computes the sum of degrees of neighbours of a node
%%
%% Parameters:
%%   Neighbours          -- List of neighbours of the node
%%   Degree              -- Information about degrees of nodes
%%   
%% Returns:
%%   
%%----------------------------------------------------------------------

%count_neighbours_degree(Neighbours, Degree) ->
%    count_neighbours_degree(Neighbours, Degree, 0).
%count_neighbours_degree([], _, Count) -> Count;
%count_neighbours_degree([N|Ns], Degree, Count) ->
%    case hipe_degree:degree(N, Degree) of
%	inf ->
%	    count_neighbours_degree(Ns, Degree, Count + 10000);
%	D ->
%	    count_neighbours_degree(Ns, Degree,
%				    Count + hipe_degree:degree(N, Degree))
%    end.
