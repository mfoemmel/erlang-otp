%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
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
