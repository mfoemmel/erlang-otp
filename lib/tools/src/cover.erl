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
-module(cover).
-behaviour(gen_server).

%% External exports
-export([start/0,
	 compile/1, compile/2, compile_module/1, compile_module/2,
	 compile_directory/0, compile_directory/1, compile_directory/2,
	 analyse/1, analyse/2, analyse/3, analyze/1, analyze/2, analyze/3,
	 analyse_to_file/1, analyse_to_file/2,
	 analyze_to_file/1, analyze_to_file/2,
	 modules/0, is_compiled/1,
	 reset/1, reset/0,
	 stop/0]).
-export([bump/5]).
-export([transform/3]). % for test purposes

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {compiled=[],                    % [{Module,File}]
		stopper}).

-record(bump, {module   = '_',                  % atom()
	       function = '_',                  % atom()
	       arity    = '_',                  % integer()
	       clause   = '_',                  % integer()
	       line     = '_'                   % integer()
	      }).

-record(vars, {module,                          % atom() Module name
	       vsn,                             % atom()
	       
	       init_info=[],                    % [{M,F,A,C,L}]

	       function,                        % atom()
	       arity,                           % int()
	       clause,                          % int()
	       lines,                           % [int()]
	       depth,                           % int()
	       is_guard=false                   % boolean
	      }).

-define(TABLE, 'cover_internal_data_table').
-define(TAG, cover_compiled).

%%%----------------------------------------------------------------------
%%% External exports
%%%----------------------------------------------------------------------

%% start() -> {ok,Pid} | {error,Reason}
%%   Pid = pid()
%%   Reason = {already_started,Pid} | term()
start() ->
    gen_server:start({local, cover_server}, ?MODULE, [], []).

%% compile(ModFile) ->
%% compile(ModFile, Options) ->
%% compile_module(ModFile) -> Result
%% compile_module(ModFile, Options) -> Result
%%   ModFile = Module | File
%%     Module = atom()
%%     File = string()
%%   Options = [Option]
%%     Option = {i,Dir} | {d,Macro} | {d,Macro,Value}
%%   Result = {ok,Module} | {error,File}
compile(ModFile) ->
    compile_module(ModFile, []).
compile(ModFile, Options) ->
    compile_module(ModFile, Options).
compile_module(ModFile) when atom(ModFile);
			     list(ModFile) ->
    compile_module(ModFile, []).
compile_module(Module, Options) when atom(Module), list(Options) ->
    compile_module(atom_to_list(Module), Options);
compile_module(File, Options) when list(File), list(Options) ->
    WithExt = case filename:extension(File) of
		  ".erl" ->
		      File;
		  _ ->
		      File++".erl"
	      end,
    AbsFile = filename:absname(WithExt),
    compile_module2(AbsFile, Options).

%% compile_directory() ->
%% compile_directory(Dir) ->
%% compile_directory(Dir, Options) -> [Result] | {error,Reason}
%%   Dir = string()
%%   Options - see compile/1
%%   Result - see compile/1
%%   Reason = eacces | enoent
compile_directory() ->
    case file:get_cwd() of
	{ok, Dir} ->
	    compile_directory(Dir, []);
	Error ->
	    Error
    end.
compile_directory(Dir) when list(Dir) ->
    compile_directory(Dir, []).
compile_directory(Dir, Options) when list(Dir), list(Options) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    
	    %% Filter out all erl files (except cover.erl)
	    ErlFileNames =
		lists:filter(fun("cover.erl") ->
				     false;
				(File) ->
				     case filename:extension(File) of
					 ".erl" -> true;
					 _ -> false
				     end
			     end,
			     Files),

	    %% Create a list of .erl file names (incl path) and call
	    %% compile_module2/1 for each such file name
	    ErlFiles = lists:map(fun(ErlFileName) ->
					 filename:join(Dir, ErlFileName)
				 end,
				 ErlFileNames),
	    lists:map(fun(ErlFile) -> compile_module2(ErlFile, Options) end,
		      ErlFiles);
	Error ->
	    Error
    end.

compile_module2(File, Options) ->
    Options2 = lists:filter(fun(Option) ->
				    case Option of
					{i, Dir} when list(Dir) -> true;
					{d, Macro} -> true;
					{d, Macro, Value} -> true;
					_ -> false
				    end
			    end,
			    Options),
    call({compile, File, Options2}).

%% analyse(Module) ->
%% analyse(Module, Analysis) ->
%% analyse(Module, Level) ->
%% analyse(Module, Analysis, Level) -> {ok,Answer} | {error,Error}
%%   Module = atom()
%%   Analysis = coverage | calls
%%   Level = line | clause | function | module
%%   Answer = {Module,Value} | [{Item,Value}]
%%     Item = Line | Clause | Function
%%      Line = {M,N}
%%      Clause = {M,F,A,C}
%%      Function = {M,F,A}
%%        M = F = atom()
%%        N = A = C = integer()
%%     Value = {Cov,NotCov} | Calls
%%       Cov = NotCov = Calls = integer()
%%   Error = {not_cover_compiled,Module}
analyse(Module) ->
    analyse(Module, coverage).
analyse(Module, Analysis) when Analysis==coverage; Analysis==calls ->
    analyse(Module, Analysis, function);
analyse(Module, Level) when Level==line; Level==clause; Level==function;
			    Level==module ->
    analyse(Module, coverage, Level).
analyse(Module, Analysis, Level) when atom(Module),
				      Analysis==coverage; Analysis==calls,
				      Level==line; Level==clause;
				      Level==function; Level==module ->
    call({{analyse, Analysis, Level}, Module}).

analyze(Module) -> analyse(Module).
analyze(Module, Analysis) -> analyse(Module, Analysis).
analyze(Module, Analysis, Level) -> analyse(Module, Analysis, Level).

%% analyse_to_file(Module) ->
%% analyse_to_file(Module, OutFile) -> {ok,OutFile} | {error,Error}
%%   Module = atom()
%%   OutFile = string()
%%   Error = {not_cover_compiled,Module} | {file,File,Reason}
%%     File = string()
%%     Reason = term()
analyse_to_file(Module) ->
    analyse_to_file(Module, atom_to_list(Module)++".COVER.out").
analyse_to_file(Module, OutFile) when atom(Module), list(OutFile) ->
    call({{analyse_to_file, OutFile}, Module}).

analyze_to_file(Module) -> analyse_to_file(Module).
analyze_to_file(Module, OutFile) -> analyse_to_file(Module, OutFile).

%% modules() -> [Module]
%%   Module = atom()
modules() ->
   call(modules).

%% is_compiled(Module) -> {file,File} | false
%%   Module = atom()
%%   File = string()
is_compiled(Module) when atom(Module) ->
    call({is_compiled, Module}).

%% reset(Module) -> ok | {error,Error}
%% reset() -> ok
%%   Module = atom()
%%   Error = {not_cover_compiled,Module}
reset(Module) when atom(Module) ->
    call({reset, Module}).
reset() ->
    call(reset).

%% stop() -> ok
stop() ->
    call(stop).

%% bump(Module, Function, Arity, Clause, Line)
%%   Module = Function = atom()
%%   Arity = Clause = Line = integer()
%% This function is inserted into Cover compiles modules, once for each
%% executable line.
bump(Module, Function, Arity, Clause, Line) ->
    Key = #bump{module=Module, function=Function, arity=Arity, clause=Clause,
		line=Line},
    ets:update_counter(?TABLE, Key, 1).

call(Request) ->
    case catch gen_server:call(cover_server, Request, infinity) of
	{'EXIT', {noproc, _Call}} ->
	    start(),
	    catch gen_server:call(cover_server, Request, infinity);
	Reply ->
	    Reply
    end.


%%%----------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------

init(_Args) ->
    ets:new(?TABLE, [set, public, named_table]),
    {ok, #state{}}.

handle_call({compile, File, Options}, _From, State) ->
    case do_compile(File, Options) of
	{ok, Module} ->
	    Compiled = add_compiled(Module, File, State#state.compiled),
	    {reply, {ok, Module}, State#state{compiled=Compiled}};
	error ->
	    {reply, {error, File}, State}
    end;

handle_call(modules, _From, State) ->

    %% Find all Cover compiled modules which are still loaded
    CompiledModules = get_modules(State#state.compiled),
    LoadedModules = lists:filter(fun(Module) ->
					 case code:which(Module) of
					     ?TAG -> true;
					     _ -> false
					 end
				 end,
				 CompiledModules),

    %% If some Cover compiled modules have been unloaded, update State
    %% and the database.
    UnloadedModules = CompiledModules--LoadedModules,
    Compiled = if
		   UnloadedModules==[] ->
		       State#state.compiled;
		   true ->
		       lists:foreach(fun(Module) -> do_clear(Module) end,
				     UnloadedModules),
		       update_compiled(UnloadedModules, State#state.compiled)
	       end,

    {reply, LoadedModules, State#state{compiled=Compiled}};

handle_call(reset, _From, State) ->
    Compiled = get_modules(State#state.compiled),
    lists:foreach(fun(Module) -> do_reset(Module) end, Compiled),
    {reply, ok, State};

handle_call({Request, Module}, _From, State) ->
    case is_loaded(Module, State#state.compiled) of
	{loaded, File} ->
	    Reply = case Request of
			{analyse, Analysis, Level} ->
			    [{Module,Clauses}] = ets:lookup(?TABLE, Module),
			    do_analyse(Module, Analysis, Level, Clauses);

			{analyse_to_file, OutFile} ->
			    do_analyse_to_file(Module, OutFile, File);

			is_compiled ->
			    {file, File};

			reset ->
			    do_reset(Module)
		    end,
	    {reply, Reply, State};

	NotLoaded ->
	    Reply = case Request of
			is_compiled ->
			    false;
			_ ->
			    {error, {not_cover_compiled,Module}}
		    end,
	    Compiled = case NotLoaded of
			   unloaded ->
			       do_clear(Module),
			       update_compiled([Module],
					       State#state.compiled);
			   false ->
			       State#state.compiled
		       end,
	    {reply, Reply, State#state{compiled=Compiled}}
    end;
handle_call(stop, From, State) ->
    %% Answer the client from terminate/2 to make sure all Cover compiled
    %% code is unloaded before cover:stop/0 returns.
    {stop, normal, State#state{stopper=From}}.

handle_cast(null, State) ->
    {noreply, State}.

handle_info(null, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    Compiled = get_modules(State#state.compiled),
    lists:foreach(fun(Module) ->
			  case code:which(Module) of
			      ?TAG ->
				  code:purge(Module),
				  code:delete(Module);
			      _ ->
				  ignore
			  end
		  end,
		  Compiled),
    case State#state.stopper of
	undefined ->
	    ignore;
	From ->
	    gen_server:reply(From, ok)
    end.

code_change(OldVsn, State, Extra) ->
    {ok, State}.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%--Handling of modules state data--------------------------------------

%% Adds information to the list of compiled modules, preserving time order
%% and without adding duplicate entries.
add_compiled(Module, File1, [{Module,File2}|Compiled]) ->
    [{Module,File1}|Compiled];
add_compiled(Module, File, [H|Compiled]) ->
    [H|add_compiled(Module, File, Compiled)];
add_compiled(Module, File, []) ->
    [{Module,File}].

is_loaded(Module, Compiled) ->
    case get_file(Module, Compiled) of
	{ok, File} ->
	    case code:which(Module) of
		?TAG -> {loaded, File};
		_ -> unloaded
	    end;
	false ->
	    false
    end.

get_file(Module, [{Module, File}|_T]) ->
    {ok, File};
get_file(Module, [_H|T]) ->
    get_file(Module, T);
get_file(Module, []) ->
    false.

get_modules(Compiled) ->
    lists:map(fun({Module, File}) -> Module end, Compiled).

update_compiled([Module|Modules], [{Module,File}|Compiled]) ->
    update_compiled(Modules, Compiled);
update_compiled(Modules, [H|Compiled]) ->
    [H|update_compiled(Modules, Compiled)];
update_compiled(Modules, []) ->
    [].

%%%--Compilation---------------------------------------------------------

%% do_compile(File, Options) -> {ok,Module} | {error,Error}
do_compile(File, Options) ->
    CoverOptions = [debug_info,binary,report_errors,report_warnings],
    case compile:file(File, CoverOptions++Options) of
	{ok, Module, Binary1} ->

	    %% Clear database
	    do_clear(Module),

	    %% Extract the abstract format and insert calls to bump/6 at
	    %% every executable line and, as a side effect, initiate
	    %% the database
	    {ok, {Module, [{abstract_code, {Vsn, Forms}}]}} =
		beam_lib:chunks(Binary1, [abstract_code]),
	    Vars0 = #vars{module=Module, vsn=Vsn},
	    {ok, MungedForms, Vars} = transform(Forms, [], Vars0),

	    %% Add module and export information to the munged forms
	    %% Information about module_info must be removed as this function
	    %% is added at compilation
	    {ok, {Module, [{exports,Exports1}]}} =
		beam_lib:chunks(Binary1, [exports]),
	    Exports2 = lists:filter(fun(Export) ->
					    case Export of
						{module_info,_} -> false;
						_ -> true
					    end
				    end,
				    Exports1),
	    NewForms = [{attribute,1,module,Module},
			{attribute,2,export,Exports2}]++ MungedForms,

	    %% Compile and load the result
	    %% It's necessary to check the result of loading since it may
	    %% fail, for example if Module resides in a sticky directory
	    {ok, Module, Binary2} = compile:forms(NewForms, []),
	    case code:load_binary(Module, ?TAG, Binary2) of
		{module, Module} ->

		    %% Store info about all function clauses in database
		    InitInfo = lists:reverse(Vars#vars.init_info),
		    ets:insert(?TABLE, {Module, InitInfo}),

		    {ok, Module};

		Error ->
		    do_clear(Module),
		    error
	    end;

	error ->
	    error
    end.

transform([Form|Forms], MungedForms, Vars) ->
    case munge(Form, Vars) of
	ignore ->
	    transform(Forms, MungedForms, Vars);
	{MungedForm, Vars2} ->
	    transform(Forms, [MungedForm|MungedForms], Vars2)
    end;
transform([], MungedForms, Vars) ->
    {ok, lists:reverse(MungedForms), Vars}.

%% This code traverses the abstract code, stored as the abstract_code
%% chunk in the BEAM file, as described in absform(3) for Erlang/OTP R8B
%% (Vsn=abstract_v2).
%% The abstract format after preprocessing differs slightly from the abstract
%% format given eg using epp:parse_form, this has been noted in comments.
munge({function,0,module_info,_Arity,_Clauses}, Vars) ->
    ignore; % module_info will be added again when the forms are recompiled
munge({function,Line,Function,Arity,Clauses}, Vars) ->
    Vars2 = Vars#vars{function=Function,
		      arity=Arity,
		      clause=1,
		      lines=[],
		      depth=1},
    {MungedClauses, Vars3} = munge_clauses(Clauses, Vars2, []),
    {{function,Line,Function,Arity,MungedClauses}, Vars3};
munge(Form, Vars) -> % attributes
    {Form, Vars}.

munge_clauses([{clause,Line,Pattern,Guards,Body}|Clauses], Vars, MClauses) ->
    {MungedGuards, _Vars} = munge_exprs(Guards, Vars#vars{is_guard=true},[]),

    case Vars#vars.depth of
	1 -> % function clause
	    {MungedBody, Vars2} = munge_body(Body, Vars#vars{depth=2}, []),
	    ClauseInfo = {Vars2#vars.module,
			  Vars2#vars.function,
			  Vars2#vars.arity,
			  Vars2#vars.clause,
			  length(Vars2#vars.lines)},
	    InitInfo = [ClauseInfo | Vars2#vars.init_info],
	    Vars3 = Vars2#vars{init_info=InitInfo,
			       clause=(Vars2#vars.clause)+1,
			       lines=[],
			       depth=1},
	    munge_clauses(Clauses, Vars3,
			  [{clause,Line,Pattern,MungedGuards,MungedBody}|
			   MClauses]);

	2 -> % receive-,  case- or if clause
	    {MungedBody, Vars2} = munge_body(Body, Vars, []),
	    munge_clauses(Clauses, Vars2,
			  [{clause,Line,Pattern,MungedGuards,MungedBody}|
			   MClauses])
    end;
munge_clauses([], Vars, MungedClauses) -> 
    {lists:reverse(MungedClauses), Vars}.

munge_body([Expr|Body], Vars, MungedBody) ->
    %% Here is the place to add a call to cover:bump/6!
    Line = element(2, Expr),
    case Vars#vars.lines of
	[Line|_Lines] -> % already a bump at this line!
	    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
	    munge_body(Body, Vars2, [MungedExpr|MungedBody]);
	Lines ->
	    ets:insert(?TABLE, {#bump{module   = Vars#vars.module,
				      function = Vars#vars.function,
				      arity    = Vars#vars.arity,
				      clause   = Vars#vars.clause,
				      line     = Line},
				0}),
	    Bump = {call, 0, {remote, 0, {atom,0,cover}, {atom,0,bump}},
		    [{atom, 0, Vars#vars.module},
		     {atom, 0, Vars#vars.function},
		     {integer, 0, Vars#vars.arity},
		     {integer, 0, Vars#vars.clause},
		     {integer, 0, Line}]},
	    Lines2 = [Line|Lines],

	    {MungedExpr, Vars2} = munge_expr(Expr, Vars#vars{lines=Lines2}),
	    munge_body(Body, Vars2, [MungedExpr,Bump|MungedBody])
    end;
munge_body([], Vars, MungedBody) ->
    {lists:reverse(MungedBody), Vars}.

munge_expr({atom,Line,Value}, Vars) when Vars#vars.is_guard==true ->
    %% Difference in abstract format after preprocessing: BIF names in
    %% guards are changed
    {{atom,Line,fix_guard(Value)}, Vars};
munge_expr({match,Line,ExprL,ExprR}, Vars) ->
    {MungedExprL, Vars2} = munge_expr(ExprL, Vars),
    {MungedExprR, Vars3} = munge_expr(ExprR, Vars2),
    {{match,Line,MungedExprL,MungedExprR}, Vars3};
munge_expr({tuple,Line,Exprs}, Vars) ->
    {MungedExprs, Vars2} = munge_exprs(Exprs, Vars, []),
    {{tuple,Line,MungedExprs}, Vars2};
munge_expr({cons,Line,ExprH,ExprT}, Vars) ->
    {MungedExprH, Vars2} = munge_expr(ExprH, Vars),
    {MungedExprT, Vars3} = munge_expr(ExprT, Vars2),
    {{cons,Line,MungedExprH,MungedExprT}, Vars3};
munge_expr({op,Line,Op,ExprL,ExprR}, Vars) ->
    {MungedExprL, Vars2} = munge_expr(ExprL, Vars),
    {MungedExprR, Vars3} = munge_expr(ExprR, Vars2),
    {{op,Line,Op,MungedExprL,MungedExprR}, Vars3};
munge_expr({op,Line,Op,Expr}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {{op,Line,Op,MungedExpr}, Vars2};
munge_expr({'catch',Line,Expr}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {{'catch',Line,MungedExpr}, Vars2};
munge_expr({call,Line1,{remote,Line2,ExprM,ExprF},Exprs},
	   Vars) when Vars#vars.is_guard==false->
    {MungedExprM, Vars2} = munge_expr(ExprM, Vars),
    {MungedExprF, Vars3} = munge_expr(ExprF, Vars2),
    {MungedExprs, Vars4} = munge_exprs(Exprs, Vars3, []),
    {{call,Line1,{remote,Line2,MungedExprM,MungedExprF},MungedExprs}, Vars4};
munge_expr({call,Line1,{remote,Line2,ExprM,ExprF},Exprs},
	   Vars) when Vars#vars.is_guard==true ->
    %% Difference in abstract format after preprocessing: BIF calls in guards
    %% are translated to {remote,...} (which is not allowed as source form)
    munge_expr({call,Line1,ExprF,Exprs}, Vars);
munge_expr({call,Line,Expr,Exprs}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {MungedExprs, Vars3} = munge_exprs(Exprs, Vars2, []),
    {{call,Line,MungedExpr,MungedExprs}, Vars3};
munge_expr({lc,Line,Expr,LC}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {MungedLC, Vars3} = munge_lc(LC, Vars2, []),
    {{lc,Line,MungedExpr,MungedLC}, Vars3};
munge_expr({block,Line,Body}, Vars) ->
    {MungedBody, Vars2} = munge_body(Body, Vars, []),
    {{block,Line,MungedBody}, Vars2};
munge_expr({'if',Line,Clauses}, Vars) -> 
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars, []),
    {{'if',Line,MungedClauses}, Vars2};
munge_expr({'case',Line,Expr,Clauses}, Vars) ->
    {MungedExpr,Vars2} = munge_expr(Expr,Vars),
    {MungedClauses,Vars3} = munge_clauses(Clauses, Vars2, []),
    {{'case',Line,MungedExpr,MungedClauses}, Vars3};
munge_expr({'receive',Line,Clauses}, Vars) -> 
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars, []),
    {{'receive',Line,MungedClauses}, Vars2};
munge_expr({'receive',Line,Clauses,Expr,Body}, Vars) ->
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars, []),
    {MungedExpr, Vars3} = munge_expr(Expr, Vars2),
    {MungedBody, Vars4} = munge_body(Body, Vars3, []),
    {{'receive',Line,MungedClauses,MungedExpr,MungedBody}, Vars4};
%% Difference in abstract format after preprocessing: Funs get an extra
%% element Extra.
munge_expr({'fun',Line,{function,Name,Arity},Extra}, Vars) ->
    {{'fun',Line,{function,Name,Arity}}, Vars};
munge_expr({'fun',Line,{clauses,Clauses},Extra}, Vars) ->
    {MungedClauses,Vars2}=munge_clauses(Clauses, Vars, []),
    {{'fun',Line,{clauses,MungedClauses}}, Vars2};
munge_expr(Form, Vars) -> % var|char|integer|float|string|atom|nil|bin
    {Form, Vars}.

munge_exprs([Expr|Exprs], Vars, MungedExprs) when Vars#vars.is_guard==true,
						  list(Expr) ->
    {MungedExpr, _Vars} = munge_exprs(Expr, Vars, []),
    munge_exprs(Exprs, Vars, [MungedExpr|MungedExprs]);
munge_exprs([Expr|Exprs], Vars, MungedExprs) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_exprs(Exprs, Vars2, [MungedExpr|MungedExprs]);
munge_exprs([], Vars, MungedExprs) ->
    {lists:reverse(MungedExprs), Vars}.

munge_lc([{generate,Line,Pattern,Expr}|LC], Vars, MungedLC) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_lc(LC, Vars2, [{generate,Line,Pattern,MungedExpr}|MungedLC]);
munge_lc([Expr|LC], Vars, MungedLC) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_lc(LC, Vars2, [MungedExpr|MungedLC]);
munge_lc([], Vars, MungedLC) ->
    {lists:reverse(MungedLC), Vars}.

fix_guard(is_atom)      -> atom;
fix_guard(is_binary)    -> binary;
fix_guard(is_constant)  -> constant;
fix_guard(is_float)     -> float;
fix_guard(is_function)  -> function;
fix_guard(is_integer)   -> integer;
fix_guard(is_list)      -> list;
fix_guard(is_number)    -> number;
fix_guard(is_pid)       -> pid; 
fix_guard(is_port)      -> port; 
fix_guard(is_reference) -> reference;
fix_guard(is_tuple)     -> tuple;
fix_guard(Name) -> Name.


%%%--Analysis------------------------------------------------------------

%% do_analyse(Module, Analysis, Level, Clauses)-> {ok,Answer} | {error,Error}
%%   Clauses = [{Module,Function,Arity,Clause,Lines}]
do_analyse(Module, Analysis, line, _Clauses) ->
    Pattern = {#bump{module=Module},'_'},
    Bumps = ets:match_object(?TABLE, Pattern),
    Fun = case Analysis of
	      coverage ->
		  fun({#bump{line=L}, 0}) ->
			  {{Module,L}, {0,1}};
		     ({#bump{line=L}, N}) ->
			  {{Module,L}, {1,0}}
		  end;
	      calls ->
		  fun({#bump{line=L}, N}) ->
			  {{Module,L}, N}
		  end
	  end,
    Answer = lists:keysort(1, lists:map(Fun, Bumps)),
    {ok, Answer};
do_analyse(Module, Analysis, clause, Clauses) ->
    Fun = case Analysis of
	      coverage ->
		  fun({M,F,A,C,Ls}) ->
			  Pattern = {#bump{module=M,function=F,arity=A,
					   clause=C},0},
			  Bumps = ets:match_object(?TABLE, Pattern),
			  NotCov = length(Bumps),
			  {{M,F,A,C}, {Ls-NotCov, NotCov}}
		  end;
	      calls ->
		  fun({M,F,A,C,Ls}) ->
			  Pattern = {#bump{module=M,function=F,arity=A,
					   clause=C},'_'},
			  Bumps = ets:match_object(?TABLE, Pattern),
			  {_Bump, Calls} = hd(lists:keysort(1, Bumps)),
			  {{M,F,A,C}, Calls}
		  end
	  end,
    Answer = lists:map(Fun, Clauses),
    {ok, Answer};
do_analyse(Module, Analysis, function, Clauses) ->
    {ok, ClauseResult} = do_analyse(Module, Analysis, clause, Clauses),
    Result = merge_clauses(ClauseResult, merge_fun(Analysis)),
    {ok, Result};
do_analyse(Module, Analysis, module, Clauses) ->
    {ok, FunctionResult} = do_analyse(Module, Analysis, function, Clauses),
    Result = merge_functions(FunctionResult, merge_fun(Analysis)),
    {ok, {Module,Result}}.

merge_fun(coverage) ->
    fun({Cov1,NotCov1}, {Cov2,NotCov2}) ->
	    {Cov1+Cov2, NotCov1+NotCov2}
    end;
merge_fun(calls) ->
    fun(Calls1, Calls2) ->
	    Calls1+Calls2
    end.

merge_clauses(Clauses, MFun) -> merge_clauses(Clauses, MFun, []).
merge_clauses([{{M,F,A,C1},R1},{{M,F,A,C2},R2}|Clauses], MFun, Result) ->
    merge_clauses([{{M,F,A,C2},MFun(R1,R2)}|Clauses], MFun, Result);
merge_clauses([{{M,F,A,C},R}|Clauses], MFun, Result) ->
    merge_clauses(Clauses, MFun, [{{M,F,A},R}|Result]);
merge_clauses([], _Fun, Result) ->
    lists:reverse(Result).

merge_functions([{MFA,R}|Functions], MFun) ->
    merge_functions(Functions, MFun, R).
merge_functions([{MFA,R}|Functions], MFun, Result) ->
    merge_functions(Functions, MFun, MFun(Result, R));
merge_functions([], MFun, Result) ->
    Result.

%% do_analyse_to_file(Module,OutFile,ErlFile) -> {ok,OutFile} | {error,Error}
%%   Module = atom()
%%   OutFile = ErlFile = string()
do_analyse_to_file(Module, OutFile, ErlFile) ->
    case file:open(ErlFile, read) of
	{ok, InFd} ->
	    case file:open(OutFile, write) of
		{ok, OutFd} ->
		    
		    %% Write some initial information to the output file
		    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
		    io:format(OutFd, "File generated from ~s by COVER "
			             "~p-~s-~s at ~s:~s:~s~n",
			      [ErlFile,
			       Y,
			       string:right(integer_to_list(Mo), 2, $0),
			       string:right(integer_to_list(D),  2, $0),
			       string:right(integer_to_list(H),  2, $0),
			       string:right(integer_to_list(Mi), 2, $0),
			       string:right(integer_to_list(S),  2, $0)]),
		    io:format(OutFd, "~n"
			             "**************************************"
			             "**************************************"
			             "~n~n", []),

		    print_lines(Module, InFd, OutFd, 1),
		    
		    file:close(OutFd),
		    file:close(InFd),

		    {ok, OutFile};

		{error, Reason} ->
		    {error, {file, OutFile, Reason}}
	    end;

	{error, Reason} ->
	    {error, {file, ErlFile, Reason}}
    end.

print_lines(Module, InFd, OutFd, L) ->
    case io:get_line(InFd, '') of
	eof ->
	    ignore;
	Line ->
	    case ets:match(?TABLE, {#bump{module=Module, line=L}, '$1'}) of
		[] ->
		    io:format(OutFd, "~s", [tab()++Line]);
		Ns ->
		    N = lists:foldl(fun([Ni], Nacc) -> Nacc+Ni end, 0, Ns),
		    if
			N<1000000 ->
			    Str = string:right(integer_to_list(N), 6, 32),
			    io:format(OutFd, "~s", [Str++fill1()++Line]);
			N<10000000 ->
			    Str = integer_to_list(N),
			    io:format(OutFd, "~s", [Str++fill2()++Line]);
			true ->
			    Str = integer_to_list(N),
			    io:format(OutFd, "~s", [Str++fill3()++Line])
		    end
	    end,
	    print_lines(Module, InFd, OutFd, L+1)
    end.

tab() ->  "        |  ".
fill1() ->      "..|  ".
fill2() ->       ".|  ".
fill3() ->        "|  ".


%%%--Reset---------------------------------------------------------------

%% do_reset(Module) -> ok
%% The reset is done on a per-clause basis to avoid building
%% long lists in the case of very large modules
do_reset(Module) ->
    [{Module,Clauses}] = ets:lookup(?TABLE, Module),
    do_reset2(Clauses).

do_reset2([{M,F,A,C,L}|Clauses]) ->
    Pattern = {#bump{module=M, function=F, arity=A, clause=C}, '_'},
    Bumps = ets:match_object(?TABLE, Pattern),
    lists:foreach(fun({Bump,_N}) ->
			  ets:insert(?TABLE, {Bump,0})
		  end,
		  Bumps),
    do_reset2(Clauses);
do_reset2([]) ->
    ok.    

do_clear(Module) ->
    ets:match_delete(?TABLE, {Module,'_'}),
    ets:match_delete(?TABLE, {#bump{module=Module},'_'}).
