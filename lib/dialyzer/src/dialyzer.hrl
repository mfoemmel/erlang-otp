%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : dialyzer.hrl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : Header file for Dialyzer.
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

%%--------------------------------------------------------------------
%% Warning classification
%%--------------------------------------------------------------------

-define(WARN_RETURN_NO_RETURN, warn_return_no_exit).
-define(WARN_RETURN_ONLY_EXIT, warn_return_only_exit).
-define(WARN_NOT_CALLED, warn_not_called).
-define(WARN_NON_PROPER_LIST, warn_non_proper_list).
-define(WARN_COMP, warn_comp).
-define(WARN_FUN_APP, warn_fun_app).
-define(WARN_TUPLE_AS_FUN, warn_tuple_as_fun).
-define(WARN_MATCHING, warn_matching).
-define(WARN_GUARDS, warn_guards).
-define(WARN_OLD_BEAM, warn_old_beam).
-define(WARN_FAILING_CALL, warn_failing_call).
-define(WARN_CONTRACT_TYPES, warn_contract_types).
-define(WARN_CONTRACT_SYNTAX, warn_contract_syntax).
-define(WARN_CONTRACT_NOT_EQUAL, warn_contract_not_equal).
-define(WARN_CONTRACT_SUBTYPE, warn_contract_subtype).
-define(WARN_CONTRACT_SUPERTYPE, warn_contract_supertype).

%% Mostly for debugging
-define(WARN_TERM_COMP, warn_term_comp).

-type(dial_warning() :: ?WARN_RETURN_NO_RETURN | ?WARN_RETURN_ONLY_EXIT
                      | ?WARN_NOT_CALLED | ?WARN_NON_PROPER_LIST | ?WARN_COMP
                      | ?WARN_TUPLE_AS_FUN | ?WARN_MATCHING | ?WARN_FUN_APP
                      | ?WARN_GUARDS | ?WARN_OLD_BEAM | ?WARN_FAILING_CALL
                      | ?WARN_CONTRACT_TYPES | ?WARN_CONTRACT_SYNTAX
                      | ?WARN_CONTRACT_NOT_EQUAL | ?WARN_CONTRACT_SUBTYPE
                      | ?WARN_CONTRACT_SUPERTYPE | ?WARN_TERM_COMP).

%%--------------------------------------------------------------------

-type(dict()       :: tuple()). %% XXX: temporarily
%-type(ordset(T)    :: [T]). %% XXX: temporarily

-type(anal_type()  :: 'dataflow' | 'succ_typings' | 'old_style' | 'plt_build').
-type(start_from() :: 'byte_code' | 'src_code').
-type(define()     :: {atom(), any()}).
-type(md5()        :: [{atom(), binary()}]).

%%--------------------------------------------------------------------

-record(dialyzer_plt, {info       = dict:new()      :: dict(),
		       contracts  = dict:new()      :: dict()}).

-record(analysis, {analysis_pid			    :: pid(),
		   type		  = succ_typings    :: anal_type(),
		   defines	  = []		    :: [define()],
		   doc_plt                          :: #dialyzer_plt{},
		   files          = []		    :: [string()],
		   include_dirs	  = []		    :: [string()],
		   supress_inline = false	    :: bool(),
		   start_from     = byte_code	    :: start_from(),
		   plt                              :: #dialyzer_plt{}}).

-record(options, {files           = []		    :: [string()],
		  files_rec       = []		    :: [string()],
		  analysis_type   = succ_typings    :: anal_type(),
		  defines         = []		    :: [define()],
		  from            = byte_code	    :: start_from(),
		  init_plt        = ""		    :: string(),
		  include_dirs    = []		    :: [string()],
		  output_plt,
		  legal_warnings  = ordsets:new()   :: [dial_warning()], % XXX: ordset(dial_warning())
		  report_mode     = normal    :: 'quiet' | 'normal' | 'verbose',
		  erlang_mode     = false	    :: bool(),
		  supress_inline  = false	    :: bool(),
		  output_file     = ""		    :: string()}).

-record(contract, {contracts	  = []		    :: [_],        % ???
		   args		  = []		    :: [_],        % ???
		   forms	  = []		    :: [{_, _}]}). % ???
