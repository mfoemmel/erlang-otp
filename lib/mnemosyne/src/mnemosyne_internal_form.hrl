%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-define(UNKNOWN, []).

-define(NO_MODULE, []).

-define(default_min, 10).
-define(default_max, 100).
-define(default_prefetch, 1).

-record(pred_sym, {module	= ?NO_MODULE,
		   functor,
		   line		= 0,
		   type		= ?UNKNOWN,	% table, rule
		   recursive	= ?UNKNOWN,	% recursive, non_recursive
		   args,
		   original_args_vars = ordsets:new(),
		   record_type  = ?UNKNOWN,
		   record_def   = ?UNKNOWN,
		   defvars	= ordsets:new(),
		   singelvars	= ordsets:new(),
		   pattern,
		   back_pattern,
		   idx_method	= no_idx,	% {dyn_idx,Pos}
						% {stat_idx,Pos}
						% {key.Pos}
		   rec_count = 0
		  }).


-record(constraint, {exprL,
		     exprR,
		     op,
		     line	= 0
		    }).


-record(rec_f, {var,
		line	= 0,
		name = ?UNKNOWN,
		field}).

-record(rec_c, {line = 0,
		name = ?UNKNOWN,
		fields = []}).


%% The "code" produced by the optimizer is a list of the following record

-record(disj_alt, {bs		= mnemosyne_unify:empty_bindings(),
		   constraints	= [],
		   alias	= [],
		   conj		= [],
		   fncalls	= [],
		   size_estimate= 0,
		   sign		= ordsets:new(),
		   rec_count = 0
		  }).
	
-record(fn, {alias_var,
	     fndef,
	     rec_count = 0
	    }).

-record(erl_expr, {alias_var,
		   expr,
		   rec_count = 0
		  }).

-record(optimizer_result, {how_to_eval = query_eval, 
			   %% mnesia_match | mnesia_match_build
			   pattern,
			   common_bs = mnemosyne_unify:empty_bindings(),
			   code = []}).

