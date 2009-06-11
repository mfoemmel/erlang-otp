%%% This is an -*- Erlang -*- file.
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
%%% 
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved online at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%% %CopyrightEnd%
%%%
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

-type dial_ret() :: ?RET_NOTHING_SUSPICIOUS
                  | ?RET_INTERNAL_ERROR 
                  | ?RET_DISCREPANCIES.

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
-define(WARN_OPAQUE, warn_opaque).
-define(WARN_FAILING_CALL, warn_failing_call).
-define(WARN_BIN_CONSTRUCTION, warn_bin_construction).
-define(WARN_CONTRACT_TYPES, warn_contract_types).
-define(WARN_CONTRACT_SYNTAX, warn_contract_syntax).
-define(WARN_CONTRACT_NOT_EQUAL, warn_contract_not_equal).
-define(WARN_CONTRACT_SUBTYPE, warn_contract_subtype).
-define(WARN_CONTRACT_SUPERTYPE, warn_contract_supertype).
-define(WARN_CALLGRAPH, warn_callgraph).
-define(WARN_UNMATCHED_RETURN, warn_umatched_return).
-define(WARN_POSSIBLE_RACE, warn_possible_race).

%%
%% The following type has double role:
%%   1. It is the set of warnings that will be collected.
%%   2. It is also the set of tags for warnings that will be returned.
%%
-type dial_warn_tag() :: ?WARN_RETURN_NO_RETURN | ?WARN_RETURN_ONLY_EXIT
                       | ?WARN_NOT_CALLED | ?WARN_NON_PROPER_LIST
                       | ?WARN_MATCHING | ?WARN_OPAQUE | ?WARN_FUN_APP
                       | ?WARN_FAILING_CALL | ?WARN_BIN_CONSTRUCTION
                       | ?WARN_CONTRACT_TYPES | ?WARN_CONTRACT_SYNTAX
                       | ?WARN_CONTRACT_NOT_EQUAL | ?WARN_CONTRACT_SUBTYPE
                       | ?WARN_CONTRACT_SUPERTYPE | ?WARN_CALLGRAPH
                       | ?WARN_UNMATCHED_RETURN | ?WARN_POSSIBLE_RACE.

%%
%% This is the representation of each warning as they will be returned
%% to dialyzer's callers
%%
-type filename()     :: string().
-type file_line()    :: {filename(), non_neg_integer()}.
-type dial_warning() :: {dial_warn_tag(), file_line(), {atom(), [term()]}}.

%%
%% This is the representation of dialyzer's internal errors
%%
-type dial_error()   :: any().    %% XXX: underspecified

%%--------------------------------------------------------------------
%% THESE TYPES SHOULD ONE DAY DISAPPEAR -- THEY DO NOT BELONG HERE
%%--------------------------------------------------------------------
 
-type ordset(T)      :: [T] .      %% XXX: temporarily
-type core_literal() :: {'c_literal',_,_}.        %% XXX: belongs in 'cerl*'
-type core_binary()  :: {'c_binary',_,_}.         %% XXX: belongs in 'cerl*'
-type core_bitstr()  :: {'c_bitstr',_,_,_,_,_,_}. %% XXX: belongs in 'cerl*'
-type core_cons()    :: {'c_cons',_,_,_}.         %% XXX: belongs in 'cerl*'
-type core_tuple()   :: {'c_tuple',_,_}.          %% XXX: belongs in 'cerl*'
-type core_var()     :: {'c_var',_,_}.            %% XXX: belongs in 'cerl*'
-type core_values()  :: {'c_values',_,_}.         %% XXX: belongs in 'cerl*'
-type core_fun()     :: {'c_fun',_,_,_}.          %% XXX: belongs in 'cerl*'
-type core_seq()     :: {'c_seq',_,_,_}.          %% XXX: belongs in 'cerl*'
-type core_let()     :: {'c_let',_,_,_,_}.        %% XXX: belongs in 'cerl*'
-type core_letrec()  :: {'c_letrec',_,_,_}.       %% XXX: belongs in 'cerl*'
-type core_case()    :: {'c_case',_,_,_}.         %% XXX: belongs in 'cerl*'
-type core_clause()  :: {'c_clause',_,_,_,_}.     %% XXX: belongs in 'cerl*'
-type core_alias()   :: {'c_alias',_,_,_}.        %% XXX: belongs in 'cerl*'
-type core_receive() :: {'c_receive',_,_,_,_}.    %% XXX: belongs in 'cerl*'
-type core_apply()   :: {'c_apply',_,_,_}.        %% XXX: belongs in 'cerl*'
-type core_call()    :: {'c_call',_,_,_,_}.       %% XXX: belongs in 'cerl*'
-type core_primop()  :: {'c_primop',_,_,_}.       %% XXX: belongs in 'cerl*'
-type core_try()     :: {'c_try',_,_,_,_,_,_}.    %% XXX: belongs in 'cerl*'
-type core_catch()   :: {'c_catch',_,_}.          %% XXX: belongs in 'cerl*'
-type core_module()  :: {'c_module',_,_,_,_,_}.   %% XXX: belongs in 'cerl*'
-type core_records() :: core_module().            %% XXX: belongs in 'cerl*'
-type core_tree()    :: core_literal() | core_binary() | core_bitstr()
                      | core_cons()    | core_tuple()  | core_var()
                      | core_values()  | core_fun()    | core_seq()
                      | core_let()     | core_letrec() | core_case()
                      | core_clause()  | core_alias()  | core_receive()
                      | core_apply()   | core_call()   | core_primop()
                      | core_try()     | core_catch()  | core_module(). %% XXX: belongs in 'cerl*'
-type erl_type()     :: 'any' | 'none' | 'unit' | {'c',atom(),_,_}. %% XXX: belongs to 'erl_types'

%%--------------------------------------------------------------------
%% Basic types used either in the record definitions below or in other
%% parts of the application
%%--------------------------------------------------------------------

-type anal_type()    :: 'succ_typings' | 'plt_build'.
-type anal_type1()   :: anal_type() | 'plt_add' | 'plt_check' | 'plt_remove'.
-type contr_constr() :: {'subtype', erl_type(), erl_type()}.
-type contract()     :: {erl_type(), [contr_constr()]}.
-type dial_define()  :: {atom(), term()}.
-type dial_option()  :: {atom(), any()}.
-type dial_options() :: [dial_option()].
-type label()	     :: non_neg_integer().
-type md5()          :: [{filename(), binary()}].
-type rep_mode()     :: 'quiet' | 'normal' | 'verbose'.
-type start_from()   :: 'byte_code' | 'src_code'.

%%--------------------------------------------------------------------
%% Record declarations used by various files
%%--------------------------------------------------------------------

-record(dialyzer_plt, {info       = dict:new()      :: dict(),
		       contracts  = dict:new()      :: dict()}).

-record(dialyzer_codeserver, {table_pid		     :: pid(),
                              exports   = sets:new() :: set(), % set(mfa())
                              next_core_label = 0    :: label(),
                              records   = dict:new() :: dict(),
                              contracts = dict:new() :: dict()}).

-record(analysis, {analysis_pid			    :: pid(),
		   type		  = succ_typings    :: anal_type(),
		   defines	  = []		    :: [dial_define()],
		   doc_plt                          :: #dialyzer_plt{},
		   files          = []		    :: [filename()],
		   include_dirs	  = []		    :: [filename()],
		   start_from     = byte_code	    :: start_from(),
		   plt                              :: #dialyzer_plt{},
		   use_contracts  = true            :: bool(),
		   race_detection = false	    :: bool(),
		   callgraph_file = ""              :: filename()}).

-record(options, {files           = []		    :: [filename()],
		  files_rec       = []		    :: [filename()],
		  analysis_type   = succ_typings    :: anal_type1(),
		  defines         = []		    :: [dial_define()],
		  from            = byte_code	    :: start_from(),
		  get_warnings    = maybe           :: bool() | 'maybe',
		  init_plt        = none	    :: 'none' | filename(),
		  include_dirs    = []		    :: [filename()],
		  output_plt      = none            :: 'none' | filename(),
		  legal_warnings  = ordsets:new()   :: ordset(dial_warn_tag()),
		  report_mode     = normal	    :: rep_mode(),
		  erlang_mode     = false	    :: bool(),
		  use_contracts   = true            :: bool(),
		  output_file     = none	    :: 'none' | filename(),
		  output_format   = formatted       :: 'raw' | 'formatted',
		  callgraph_file  = ""              :: filename(),
		  check_plt       = true            :: bool()
		 }).

-record(contract, {contracts	  = []		    :: [contract()],
		   args		  = []		    :: [erl_type()],
		   forms	  = []		    :: [{_, _}]}).

%%--------------------------------------------------------------------

-type plt_contracts() :: [{mfa(), #contract{}}]. % actually, an orddict()

%%--------------------------------------------------------------------
