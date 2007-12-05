%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% File    : typer.hrl
%% Author  : Bingwen He <Bingwen.He@gmail.com>
%% Description : 
%%--------------------------------------------------------------------

-define(SRC_COMP_OPTS, [no_copt, binary, no_inline,
			strict_record_test]).

-record(args, {analyze=[]        :: [string()],
	       analyzed_dir_r=[] :: [string()],
	       trust=[]        	 :: [string()]}).

-record(analysis, {
	  %% Common use:
	  mode		:: atom(),
	  macros=[]	:: [{atom(),_}], % {macro_name, value}
	  includes=[]	:: [string()],

	  %% Esp for Dialyzer
	  %% ----------------------
	  code_server = dialyzer_codeserver:new(),
	  callgraph   = dialyzer_callgraph:new(),
	  ana_files	:: [string()],   % absolute filenames

	  %% Esp for TypEr
	  %% ----------------------
	  t_files=[]	:: [string()], 

	  %% For choosing between contracts or comments
	  contracts=true :: bool(),

	  %% Any file in 'final_files' is compilable.
	  %% And we need to keep it as {FileName,ModuleName}
	  %% in case filename does NOT match with moduleName
	  final_files=[],  
	  needed_inc_files,

	  ex_func=typer_map:new(),	%% dict
	  record=typer_map:new(),	%% dict

	  %% Functions: the line number of the function 
	  %%            should be kept as well
	  func=typer_map:new(),	%% dict
	  inc_func=typer_map:new(),	%% dict
	  trust_plt=dialyzer_plt:new()}).
