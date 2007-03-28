%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : dialyzer.hrl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : Header file for the Dialyzer.
%%%
%%% Created : 1 Oct 2004 by Kostis Sagonas <kostis@it.uu.se>
%%%-------------------------------------------------------------------

-define(RET_NOTHING_SUSPICIOUS, 0).
-define(RET_INTERNAL_ERROR, 1).
-define(RET_DISCREPANCIES_FOUND, 2).

-define(WARN_CALLGRAPH, warn_callgraph).

-define(SRC_COMPILE_OPTS, 
	[to_core, binary, report_errors, no_inline, strict_record_tests]).
-define(HIPE_DEF_OPTS, 
	[no_inline_fp, {pmatch, no_duplicates}, {target, x86}]).

-record(analysis, {analysis_pid, core_transform=dataflow,
		   defines=[], doc_plt,
		   files, fixpoint, granularity, include_dirs=[],
		   init_plt, mcg=none, plt_info=none, 
		   supress_inline, start_from, user_plt}).

-record(options, {files=[],
		  files_rec=[],
		  core_transform=core_warnings,
		  defines=[],
		  from=byte_code, %% default is to start from byte code	  
		  init_plt,
		  include_dirs=[],
		  output_plt,
		  legal_warnings,
		  quiet=false,
		  erlang_mode=false,
		  supress_inline=false,
		  output_file=""}).

%% Warning classification.

-define(WARN_RETURN_NO_RETURN, warn_return_no_exit).
-define(WARN_RETURN_ONLY_EXIT, warn_return_only_exit).
-define(WARN_NOT_CALLED, warn_not_called).
-define(WARN_NON_PROPER_LIST, warn_non_proper_list).
-define(WARN_TUPLE_AS_FUN, warn_tuple_as_fun).
-define(WARN_FUN_APP, warn_fun_app).
-define(WARN_MATCHING, warn_matching).
-define(WARN_COMP, warn_comp).
-define(WARN_GUARDS, warn_guards).
-define(WARN_OLD_BEAM, warn_old_beam).
-define(WARN_FAILING_CALL, warn_failing_call).
