%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% File    : hipe_digraph.hrl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Description : Record for a simple implementation of a directed graph.
%%
%% Created : 22 Mar 2005 by Kostis Sagonas <kostis@it.uu.se>
%%-----------------------------------------------------------------------

-record(hipe_digraph, {edges     = dict:new()    :: dict(),
		       rev_edges = dict:new()    :: dict(),
		       leaves    = ordsets:new() :: ordset(_), % ???
		       nodes     = sets:new()    :: set()}).

%%-----------------------------------------------------------------------
