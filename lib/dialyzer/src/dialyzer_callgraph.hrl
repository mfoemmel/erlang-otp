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

%%-----------------------------------------------------------------------

-define(no_arg, no_arg).
-define(no_label, no_label).

%%-----------------------------------------------------------------------

-type mfa_or_funlbl() :: label() | mfa().
-type scc()	      :: [mfa_or_funlbl()].
-type mfa_calls()     :: [{mfa_or_funlbl(), mfa_or_funlbl()}].

%%-----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% Basic types used in the race analysis
%%-----------------------------------------------------------------------

-type label_type() :: label() | [label()] | {label()} | ?no_label.
-type args()       :: 'empty' | [label_type() | [string()]].
-type core_vars()  :: core_tree() | ?no_arg.
-type var_to_map() :: core_vars() | [core_tree()].
-type dep_calls()  :: 'whereis' | 'ets_lookup' | 'mnesia_dirty_read'.
-type warn_calls() :: 'register' | 'ets_insert' | 'mnesia_dirty_write'.
-type race_tag()   :: 'whereis_register' | 'ets_lookup_insert'
                    | 'mnesia_dirty_read_write'.

-record(beg_clause, {arg        :: var_to_map(),
                     pats       :: var_to_map(),
                     guard      :: core_tree()}).
-record(end_clause, {arg        :: var_to_map(),
                     pats       :: var_to_map(),
                     guard      :: core_tree()}).
-record(end_case,   {clauses    :: [#end_clause{}]}).
-record(curr_fun,   {status     :: 'in' | 'out',
                     mfa        :: mfa_or_funlbl(),
                     label      :: label(),
                     def_vars   :: [core_vars()],
                     arg_types  :: [erl_types:erl_type()],
                     call_vars  :: [core_vars()],
                     var_map    :: dict()}).
-record(dep_call,   {call_name  :: dep_calls(),
                     args       :: args(),
                     arg_types  :: [erl_types:erl_type()],
                     vars       :: [core_vars()],
                     state      :: _,
                     file_line  :: file_line(),
                     var_map    :: dict()}).
-record(fun_call,   {caller     :: mfa_or_funlbl(),
                     callee     :: mfa_or_funlbl(),
                     arg_types  :: [erl_types:erl_type()],
                     vars       :: [core_vars()]}).
-record(let_tag,    {var        :: var_to_map(),
                     arg        :: var_to_map()}).
-record(warn_call,  {call_name  :: warn_calls(),
                     args       :: args(),
                     var_map    :: dict()}).

-type case_tags()  :: 'beg_case' | #beg_clause{} | #end_clause{} | #end_case{}.
-type code()       :: [#dep_call{} | #warn_call{} | #fun_call{} |
                       #curr_fun{} | #let_tag{} | case_tags() | race_tag()]
                    | 'empty'.

%%----------------------------------------------------------------------
%% Record declarations used in various files
%%----------------------------------------------------------------------

-record(dialyzer_callgraph, {digraph        = digraph:new() :: digraph(),
			     esc	    = sets:new()    :: set(),
			     name_map	    = dict:new()    :: dict(),
			     rev_name_map   = dict:new()    :: dict(),
			     postorder      = []	    :: [scc()],
			     rec_var_map    = dict:new()    :: dict(),
			     self_rec	    = sets:new()    :: set(),
			     calls          = dict:new()    :: dict(),
                             race_code      = dict:new()    :: dict(),
                             public_tables  = []            :: [label()],
                             named_tables   = []            :: [string()],
                             race_detection = false         :: boolean()}).
