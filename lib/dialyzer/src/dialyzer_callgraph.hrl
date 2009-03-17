%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

%%-----------------------------------------------------------------------------
%% File    : dialyzer_callgraph.hrl
%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%           Kostis Sagonas <kostis@it.uu.se>
%% Description : Header file for Dialyzer's call graph module.
%%
%% Created : 23 Nov 2007 by Kostis Sagonas <kostis@it.uu.se>
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% A callgraph is a directed graph where the nodes are functions and a
%% call between two functions is an edge from the caller to the callee.
%% 
%% calls	-  A mapping from call site (and apply site) labels
%%		   to the possible functions that can be called.
%% digraph	-  A digraph representing the callgraph. 
%%		   Nodes are represented as MFAs or labels.
%% esc		-  A set of all escaping functions as reported by dialyzer_dep.
%% postorder	-  A list of strongly connected components of the callgraph
%%		   sorted in a topological bottom-up order.
%%		   This is produced by calling finalize/1.
%% name_map	-  A mapping from label to MFA.
%% rev_name_map	-  A reverse mapping of the name_map.
%% rec_var_map	-  A dict mapping from letrec bound labels to function names.
%%		   Only for top level functions (from module defs).
%% self_rec	-  A set containing all self recursive functions.
%%		   Note that this contains MFAs for named functions and labels
%%		   whenever applicable.
%%-----------------------------------------------------------------------------

%% in the following type, integers represent labels of funs
-type mfa_or_funlbl() :: integer() | mfa().
-type scc()	      :: [mfa_or_funlbl()].

-record(dialyzer_callgraph, {digraph		     :: digraph(),
			     esc		     :: set(),
			     name_map		     :: dict(),
			     rev_name_map	     :: dict(),
			     postorder = []	     :: [scc()],
			     rec_var_map	     :: dict(),
			     self_rec		     :: set(),
			     calls		     :: dict(),
			     module_local_calls = [] :: [_],
			     inter_module_calls	= [] :: [_]}).
