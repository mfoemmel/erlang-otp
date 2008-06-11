%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : dialyzer.hrl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : Header file for Dialyzer.
%%%
%%% Created : 1 Oct 2004 by Kostis Sagonas <kostis@it.uu.se>
%%%-------------------------------------------------------------------

-define(RET_NOTHING_SUSPICIOUS, 0).
-define(RET_INTERNAL_ERROR, 1).
-define(RET_DISCREPANCIES, 2).

-type(dial_ret() :: ?RET_NOTHING_SUSPICIOUS
                  | ?RET_INTERNAL_ERROR 
                  | ?RET_DISCREPANCIES).

-define(SRC_COMPILE_OPTS, 
	[no_copt, to_core, binary, return_errors, 
	 no_inline, strict_record_tests, strict_record_updates]).

%%--------------------------------------------------------------------
%% Warning classification
%%--------------------------------------------------------------------

-define(WARN_RETURN_NO_RETURN, warn_return_no_exit).
-define(WARN_RETURN_ONLY_EXIT, warn_return_only_exit).
-define(WARN_NOT_CALLED, warn_not_called).
-define(WARN_NON_PROPER_LIST, warn_non_proper_list).
-define(WARN_FUN_APP, warn_fun_app).
-define(WARN_MATCHING, warn_matching).
-define(WARN_FAILING_CALL, warn_failing_call).
-define(WARN_CONTRACT_TYPES, warn_contract_types).
-define(WARN_CONTRACT_SYNTAX, warn_contract_syntax).
-define(WARN_CONTRACT_NOT_EQUAL, warn_contract_not_equal).
-define(WARN_CONTRACT_SUBTYPE, warn_contract_subtype).
-define(WARN_CONTRACT_SUPERTYPE, warn_contract_supertype).
-define(WARN_CALLGRAPH, warn_callgraph).
-define(WARN_UNMATCHED_RETURN, warn_umatched_return).
%% Mostly for debugging
-define(WARN_TERM_COMP, warn_term_comp).

%%
%% The following type has double role:
%%   1. It is the set of warnings that will be collected.
%%   2. It is also the set of tags for warnings that will be returned.
%%
-type(dial_warn_tag() :: ?WARN_RETURN_NO_RETURN | ?WARN_RETURN_ONLY_EXIT
                       | ?WARN_NOT_CALLED | ?WARN_NON_PROPER_LIST
                       | ?WARN_MATCHING | ?WARN_FUN_APP
                       | ?WARN_FAILING_CALL | ?WARN_CALLGRAPH
                       | ?WARN_CONTRACT_TYPES | ?WARN_CONTRACT_SYNTAX
                       | ?WARN_CONTRACT_NOT_EQUAL | ?WARN_CONTRACT_SUBTYPE
                       | ?WARN_CONTRACT_SUPERTYPE | ?WARN_TERM_COMP
                       | ?WARN_UNMATCHED_RETURN).

%%
%% This is the representation of each warning as they will be returned
%% to dialyzer's callers
%%
-type(file_line()    :: {string(), non_neg_integer()}).
-type(dial_warning() :: {dial_warn_tag(), file_line(), {atom(),[_]}}).

%%
%% This is the representation of dialyzer's internal errors
%%
-type(dial_error()   :: any()).    %% XXX: underspecified

%%--------------------------------------------------------------------
%% THESE TYPES SHOULD ONE DAY DISAPPEAR -- THEY DO NOT BELONG HERE
%%--------------------------------------------------------------------
 
-type(dict()         :: tuple()).  %% XXX: temporarily
-type(orddict()      :: [{_, _}]). %% XXX: temporarily
-type(set()          :: tuple()).  %% XXX: temporarily
-type(ordset(T)      :: [T]).      %% XXX: temporarily
-type(core_module()  :: {'module',_,_,_,_,_}). % XXX: belongs in 'cerl*'
-type(core_tree()    :: tuple()).  %% XXX: belongs in 'cerl*'
-type(core_records() :: tuple()).  %% XXX: belongs in 'cerl*'
-type(erl_type()     :: any()).    %% XXX: belongs to 'erl_types'

%%--------------------------------------------------------------------
%% Basic types used either in the record definitions below or in other
%% parts of the application
%%--------------------------------------------------------------------

-type(anal_type()    :: 'succ_typings' | 'plt_build').
-type(anal_type1()   :: anal_type() | 'plt_add' | 'plt_check' | 'plt_remove').
-type(start_from()   :: 'byte_code' | 'src_code').
-type(define()       :: {atom(), any()}).
-type(md5()          :: [{string(), binary()}]).
-type(rep_mode()     :: 'quiet' | 'normal' | 'verbose').
-type(dial_option()  :: {atom(), any()}).
-type(dial_options() :: [dial_option()]).

%%--------------------------------------------------------------------
%% Record declarations used by various files
%%--------------------------------------------------------------------

-record(dialyzer_plt, {info       = dict:new()      :: dict(),
		       contracts  = dict:new()      :: dict()}).

-record(dialyzer_codeserver, {table           :: pid(),
                              exports         :: set(), % set(mfa())
                              next_core_label :: non_neg_integer(),
                              records         :: dict(),
                              contracts       :: dict()}).

-record(analysis, {analysis_pid			    :: pid(),
		   type		  = succ_typings    :: anal_type(),
		   defines	  = []		    :: [define()],
		   doc_plt                          :: #dialyzer_plt{},
		   files          = []		    :: [string()],
		   include_dirs	  = []		    :: [string()],
		   start_from     = byte_code	    :: start_from(),
		   plt                              :: #dialyzer_plt{},
		   use_contracts  = true            :: bool()}).

-record(options, {files           = []		    :: [string()],
		  files_rec       = []		    :: [string()],
		  analysis_type   = succ_typings    :: anal_type1(),
		  defines         = []		    :: [define()],
		  from            = byte_code	    :: start_from(),
		  get_warnings    = maybe           :: bool() | 'maybe',
		  init_plt        = none	    :: 'none' | string(),
		  include_dirs    = []		    :: [string()],
		  output_plt      = none            :: 'none' | string(),
		  legal_warnings  = ordsets:new()   :: ordset(dial_warn_tag()),
		  report_mode     = normal	    :: rep_mode(),
		  erlang_mode     = false	    :: bool(),
		  use_contracts   = true            :: bool(),
		  output_file     = none	    :: 'none' | string(),
		  output_format   = formatted       :: 'raw' | 'formatted'}).

-record(contract, {contracts	  = []		    :: [_],        % ???
		   args		  = []		    :: [erl_type()],
		   forms	  = []		    :: [{_, _}]}).

%%--------------------------------------------------------------------
