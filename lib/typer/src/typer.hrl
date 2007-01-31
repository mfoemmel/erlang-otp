%%%----------------------------------------------
%%% File    : typer.hrl
%%% Author  : He, Bingwen <Bingwen.He@gmail.com>
%%% Description : 
%%%
%%%----------------------------------------------

-define(SRC_COMP_OPTS, [binary,no_inline,strict_record_test,typed_record]).

-record(args, {analyze=[]        :: [string()],
	       analyzed_dir_r=[] :: [string()],
	       trust=[]        	 :: [string()]}).

-record(analysis, {
	  %% Common use:
	  mode=[]	:: string(), %% TODO: change to atom
	  macros=[]	:: [string()],
	  includes=[]	:: [string()],

	  %% Esp for Dialyzer
	  %% ----------------------
	  code_server=dialyzer_codeserver:new(),
	  callgraph=dialyzer_callgraph:new(),
	  ana_files	:: [string()], %% ABS filenames

	  %% Esp for TypEr
	  %% ----------------------
	  t_files=[]	:: [string()], 

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
	  trust_plt=dialyzer_plt:new(trusted_plt)}).
