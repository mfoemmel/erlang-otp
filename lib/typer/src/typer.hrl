%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% File        : typer.hrl
%% Author      : Bingwen He <Bingwen.He@gmail.com>
%% Description : Header file for the typer application
%%--------------------------------------------------------------------

%%
%% The following are needed for some dialyzer records below
%%
-include("dialyzer.hrl").
-include("dialyzer_callgraph.hrl").

%% XXX: The following appear here only temporarily
%-type(dict()                :: tuple()).
-type(dialyzer_plt()        :: tuple()).
-type(dialyzer_codeserver() :: tuple()).

-record(typer_analysis,
	{mode		 :: atom(),
	 macros=[]	 :: [{atom(),_}], % {macro_name, value}
	 includes=[]	 :: [string()],
	 
	 %% Esp for Dialyzer
	 %% ----------------------
	 code_server = dialyzer_codeserver:new() :: dialyzer_codeserver(),
	 callgraph   = dialyzer_callgraph:new()  :: #dialyzer_callgraph{},
	 ana_files	 :: [string()],   % absolute filenames
	 plt         = none                      :: 'none' | string(),
	 
	 %% Esp for TypEr
	 %% ----------------------
	 t_files=[]	 :: [string()], 
	 
	 %% For choosing between contracts or comments
	 contracts=true :: bool(),
	 
	 %% Any file in 'final_files' is compilable.
	 %% And we need to keep it as {FileName,ModuleName}
	 %% in case filename does NOT match with moduleName
	 final_files=[] :: [{string(),atom()}],  
	 
	 ex_func=typer_map:new()      :: dict(),
	 record=typer_map:new()       :: dict(),
	 
	 %% Functions: the line number of the function 
	 %%            should be kept as well
	 func=typer_map:new()         :: dict(),
	 inc_func=typer_map:new()     :: dict(),
	 trust_plt=dialyzer_plt:new() :: #dialyzer_plt{}}).

-record(args,
	{analyze=[]        :: [string()],
	 analyzed_dir_r=[] :: [string()],
	 trust=[]          :: [string()]}).
