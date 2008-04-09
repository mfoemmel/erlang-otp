%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
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
%% Copyright 2006-2008, Tobias Lindahl and Kostis Sagonas
%% 
%% $Id$
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_cl.erl
%%% Authors : Tobias Lindahl <tobiasl@csd.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : The command line interface for the Dialyzer tool.
%%%
%%% Created : 27 Apr 2004 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer_cl).

-export([start/1, check_init_plt/2]).

-include("dialyzer.hrl").

-type(io_device() :: pid()).	% XXX: This should be imported from 'file'

-record(cl_state,
	{backend_pid                            :: pid(),
	 erlang_mode = false                    :: bool(),
	 external_calls = []                    :: string(),
	 legal_warnings  = ordsets:new()        :: [dial_warn_tag()],
	 mod_deps                               :: dict(),
	 output          = standard_io		:: 'standard_io' | io_device(),
	 output_plt      = none                 :: 'none' | string(),
	 plt_info                               :: {md5(), 
						    [atom()],
						    {mergable | non_mergable, 
						     dict()}},
	 report_mode     = normal               :: rep_mode(),
	 return_status= ?RET_NOTHING_SUSPICIOUS	:: dial_ret(),
	 stored_errors   = []                   :: [dial_error()],
	 stored_warnings = []                   :: [dial_warning()]
	}).

%%--------------------------------------------------------------------

-spec(start/1 :: (#options{}) -> dial_ret()
			       | {dial_ret(),[dial_warning()],[dial_error()]}).

start(#options{} = DialyzerOptions) ->
  process_flag(trap_exit, true),
  {InitAnalysis, PltInfo} = build_analysis_record(DialyzerOptions),
  State = new_state(),
  NewState1 = init_output(State, DialyzerOptions),  
  NewState2 = 
    NewState1#cl_state{legal_warnings=DialyzerOptions#options.legal_warnings,
		       output_plt=DialyzerOptions#options.output_plt,
		       plt_info=PltInfo,
		       erlang_mode=DialyzerOptions#options.erlang_mode,
		       report_mode=DialyzerOptions#options.report_mode},
  NewState3 = run_analysis(NewState2, InitAnalysis),
  cl_loop(NewState3).

%%--------------------------------------------------------------------

-spec(check_init_plt/2 :: (#options{}, bool()) -> ?RET_NOTHING_SUSPICIOUS).

check_init_plt(Opts, Force) ->
  process_flag(trap_exit, true),
  ReportMode = Opts#options.report_mode,
  case dialyzer_plt:check_init_plt(Opts#options.init_plt) of
    {fail, MD5, DiffMd5, Libs, InitPlt} ->      
      case ReportMode of
	quiet -> ok;
	normal -> io:format(" no\n", []);
	verbose -> print_md5_diff(DiffMd5)
      end,
      case (not Force) andalso check_if_installed() of
	true -> 
	  Msg = "    The initial PLT is not up-to-date.\n"
	    "    Since Dialyzer is installed no new PLT will be built.\n"
	    "    To force the rebuilding use the option --check_init_plt\n",
	  error(Msg);
	false ->
	  Msg = "    Creating initial PLT"
	    " (will take several minutes; please be patient)",
	  io:format("~s", [Msg]),
	  InclDirs = Opts#options.include_dirs,
	  {T1, _} = statistics(wall_clock),
	  Ret = 
	    case create_init_plt(MD5, DiffMd5, Libs, InitPlt, InclDirs) of
	      ?RET_INTERNAL_ERROR ->
		error("Problems during consistency check of initial PLT");
	      ?RET_NOTHING_SUSPICIOUS -> ?RET_NOTHING_SUSPICIOUS;
	      ?RET_DISCREPANCIES -> ?RET_NOTHING_SUSPICIOUS
	    end,
	  {T2, _} = statistics(wall_clock),
	  if ReportMode =:= quiet -> ok;
	     true -> print_elapsed_time(T1, T2)
	  end,
	  Ret
      end;
    {ok, _InitPlt} ->
      if ReportMode =:= quiet -> ok;
	 true  -> io:format(" yes\n")
      end,
      ?RET_NOTHING_SUSPICIOUS;
    {error, Msg} ->
      if ReportMode =:= quiet -> ok;
	 true  -> io:format(" no\n")
      end,
      error(Msg)
  end.

%%--------------------------------------------------------------------

print_elapsed_time(T1, T2) ->
  ElapsedTime = T2 - T1,
  Mins = ElapsedTime div 60000,
  Secs = (ElapsedTime rem 60000) / 1000,
  io:format("    Done building PLT in ~wm~.2fs\n", [Mins, Secs]).

print_md5_diff(none) ->
  io:format("    Could not find the old PLT information\n", []);
print_md5_diff(List) ->
  io:format("    The PLT information is not up to date:\n", []),
  case [Mod || {new, Mod} <- List] of
    [] -> ok;
    NewMods -> io:format("    New modules: ~p\n", [NewMods])
  end,
  case [Mod || {removed, Mod} <- List] of
    [] -> ok;
    RemovedMods -> io:format("    Removed modules: ~p\n", [RemovedMods])
  end,
  case [Mod || {diff, Mod} <- List] of
    [] -> ok;
    ChangedMods -> io:format("    Changed modules: ~p\n", [ChangedMods])
  end.

check_if_installed() ->
  case filename:basename(code:lib_dir(dialyzer)) of
    "dialyzer" -> false;
    "dialyzer-" ++ _Version -> true
  end.  

create_init_plt(MD5, none, Libs, InitPltFile, IncludeDirs) ->
  State = new_state(),
  State1 = State#cl_state{output_plt=InitPltFile,
			  plt_info={MD5, Libs, {mergable, dict:new()}}},
  Dirs = [filename:join(code:lib_dir(Lib), "ebin")|| Lib <- Libs],
  Files1 = [{D, file:list_dir(D)} || D <- Dirs],
  Files2 = [[filename:join(D, F2) || F2 <- F] || {D, {ok, F}} <- Files1],
  Files3 = lists:append(Files2),
  BeamFiles = [F || F <- Files3, filename:extension(F) =:= ".beam"],
  InitPlt = dialyzer_plt:new(),
  create_init_plt_common(State1, BeamFiles, IncludeDirs, InitPlt);
create_init_plt(MD5, DiffMd5, Libs, InitPltFile, IncludeDirs) ->
  ChangedMods = ordsets:from_list([M || {diff, M} <- DiffMd5]),
  AddedMods = ordsets:from_list([M || {new, M} <- DiffMd5]),
  RemovedMods = ordsets:from_list([M || {removed, M} <- DiffMd5]),
  ModDeps = dialyzer_plt:get_mod_deps(InitPltFile),
  AnalyzeMods = expand_dependent_module(ChangedMods, AddedMods, 
					RemovedMods, ModDeps),
  Dirs = [filename:join(code:lib_dir(Lib), "ebin")|| Lib <- Libs],
  Files1 = [{D, file:list_dir(D)} || D <- Dirs],
  Files2 = [[filename:join(D, F2) || F2 <- F] || {D, {ok, F}} <- Files1],
  Files3 = lists:append(Files2),
  BeamFiles = [{F, list_to_atom(filename:basename(F, ".beam"))} 
	       || F <- Files3, filename:extension(F) =:= ".beam"],
  AnalFiles = [F || {F, Mod} <- BeamFiles, 
		    ordsets:is_element(Mod, AnalyzeMods)],
  %% Clean the plt and the mod deps from the removed modules.
  InitPlt = dialyzer_plt:from_file(InitPltFile),
  {NewInitPlt, NewModDeps} = 
    lists:foldl(fun(M, {AccPlt, AccModDeps}) -> 
		    {dialyzer_plt:delete_module(AccPlt, M),
		     dict:erase(M, AccModDeps)}
		end, {InitPlt, ModDeps}, RemovedMods),
  State = new_state(),
  State1 = State#cl_state{output_plt=InitPltFile,
			  plt_info={MD5, Libs, {mergable, NewModDeps}}},
  create_init_plt_common(State1, AnalFiles, IncludeDirs, NewInitPlt).

create_init_plt_common(State, Files, IncludeDirs, InitPlt) ->
  %%io:format("Files: ~p\n", [Files]),
  case Files =:= [] of
    true ->
      %% We have only removed files with no remaining dependencies.
      return_value(State, InitPlt);
    false ->
      hipe_compile(Files),
      Analysis = #analysis{files=Files, 
			   plt=InitPlt,
			   include_dirs=IncludeDirs,
			   start_from=byte_code,
			   type=plt_build},
      cl_loop(run_analysis(State, Analysis))
  end.

expand_dependent_module(ChangedMods, AddedMods, RemovedMods, ModDeps) ->
  BigSet = lists:sort(ChangedMods++AddedMods++RemovedMods),
  ExpandedSet = expand_dependent_module(BigSet, BigSet, ModDeps),
  ordsets:subtract(ExpandedSet, RemovedMods).
			
expand_dependent_module([Mod|Left], Included, ModDeps) ->
  case dict:find(Mod, ModDeps) of
    {ok, Deps} ->
      case ordsets:subtract(Deps, Included) of
	[] -> 
	  expand_dependent_module(Left, Included, ModDeps);
	NewDeps -> 
	  NewIncluded = ordsets:union(Included, NewDeps),
	  expand_dependent_module(NewDeps ++ Left, NewIncluded, ModDeps)
      end;
    error ->
      expand_dependent_module(Left, Included, ModDeps)
  end;
expand_dependent_module([], Included, _ModDeps) ->
  Included.

-define(MIN_FILES_FOR_NATIVE_COMPILE, 20).

-spec(hipe_compile/1 :: ([string()]) -> 'ok').

hipe_compile(Files) when length(Files) > ?MIN_FILES_FOR_NATIVE_COMPILE ->
  case erlang:system_info(hipe_architecture) of
    undefined -> ok;
    _ ->
      {ok, lists}                       = hipe:c(lists),
      {ok, dict}                        = hipe:c(dict),
      {ok, gb_trees}                    = hipe:c(gb_trees),
      {ok, dialyzer_succ_typings}       = hipe:c(dialyzer_succ_typings),
      {ok, dialyzer_analysis_callgraph} = hipe:c(dialyzer_analysis_callgraph),
      {ok, dialyzer_typesig}            = hipe:c(dialyzer_typesig),
      {ok, dialyzer_dataflow}           = hipe:c(dialyzer_dataflow),
      {ok, dialyzer_codeserver}         = hipe:c(dialyzer_codeserver),
      {ok, erl_types}                   = hipe:c(erl_types),
      {ok, erl_bif_types}               = hipe:c(erl_bif_types),
      {ok, cerl}                        = hipe:c(cerl, [no_concurrent_comp]),
      ok
  end;
hipe_compile(_Files) ->
  ok.

new_state() ->
  #cl_state{mod_deps=dict:new()}.

init_output(State, DialyzerOptions) ->
  case DialyzerOptions#options.output_file of
    "" ->
      State;
    OutputFile ->
      case file:open(OutputFile, [write]) of
	{ok, File} ->
	  State#cl_state{output=File};
	{error, Reason} ->
	  Msg = io_lib:format("Could not open output file ~p, Reason: ~p\n",
			      [OutputFile, Reason]),
	  error(State, lists:flatten(Msg))
      end
  end.

-spec(maybe_close_output_file/1 :: (#cl_state{}) -> 'ok').
maybe_close_output_file(State) ->
  case State#cl_state.output of
    standard_io -> ok;
    File -> ok = file:close(File)
  end.

%% ----------------------------------------------------------------
%%
%%  Main Loop
%%

-define(LOG_CACHE_SIZE, 10).

%%-spec(cl_loop/1 :: (#cl_state{}) -> 
cl_loop(State) ->
  cl_loop(State, []).

cl_loop(State, LogCache) ->
  BackendPid = State#cl_state.backend_pid,
  receive
    {BackendPid, log, LogMsg} ->
      %%io:format(State#cl_state.output ,"Log: ~s\n", [LogMsg]),
      cl_loop(State, lists:sublist([LogMsg|LogCache], ?LOG_CACHE_SIZE));
    {BackendPid, warnings, Warnings} ->
      NewState = store_warnings(State, Warnings),
      cl_loop(NewState, LogCache);
    {BackendPid, error, Msg} ->
      State1 = store_error(State, Msg),
      cl_loop(State1#cl_state{return_status=?RET_INTERNAL_ERROR}, LogCache);
    {BackendPid, done, NewPlt, _NewDocPlt} ->
      return_value(State, NewPlt);
    {BackendPid, ext_calls, ExtCalls} ->
      Msg = lists:flatten(io_lib:format("Unknown functions: ~p\n", [ExtCalls])),
      cl_loop(State#cl_state{external_calls=Msg}, LogCache);
    {BackendPid, mod_deps, ModDeps} ->
      NewState = State#cl_state{mod_deps=ModDeps},
      cl_loop(NewState, LogCache);
    {'EXIT', BackendPid, {error, Reason}} ->
      Msg = failed_anal_msg(Reason, LogCache),
      error(State, Msg);
    {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
      Msg = failed_anal_msg(io_lib:format("~P", [Reason, 12]), LogCache),
      error(State, Msg);
    _Other ->
      %% io:format("Received ~p\n", [_Other]),
      cl_loop(State, LogCache)
  end.

-spec(failed_anal_msg/2 :: (string(), [_]) -> string()).

failed_anal_msg(Reason, LogCache) when length(Reason) > 0 ->
  io_lib:format("Analysis failed with error: ~s\n"
		"Last messages in log cache: ~p\n", 
		[Reason, lists:reverse(LogCache)]).

store_error(State = #cl_state{stored_errors=StoredErrors, 
			      erlang_mode=ErlangMode}, Msg) ->
  case ErlangMode of
    true -> 
      NewStoredErrors = StoredErrors ++ [Msg],
      State#cl_state{stored_errors=NewStoredErrors};
    false ->
      error(State, Msg)
  end.

-spec(store_warnings/2 :: (#cl_state{}, [dial_warning()]) -> #cl_state{}).

store_warnings(State = #cl_state{stored_warnings=StoredWarnings}, Warnings) ->
  State#cl_state{stored_warnings=StoredWarnings ++ Warnings}.

-spec(error/1 :: (string()) -> no_return()).

error(Msg) ->
  throw({dialyzer_error, Msg}).

-spec(error/2 :: (#cl_state{}, string()) -> no_return()).

error(State, Msg) ->
  case State#cl_state.output of
    standard_io -> ok;
    Outfile -> io:format(Outfile, "\n~s\n", [Msg])
  end,
  maybe_close_output_file(State),
  throw({dialyzer_error, Msg}).

return_value(State = #cl_state{erlang_mode=ErlangMode,
			       mod_deps=ModDeps,
			       output_plt=OutputPlt,
			       plt_info=PltInfo,
			       stored_errors=StoredErrors,
			       stored_warnings=StoredWarnings},
	     Plt) ->
  RetValue =
    case State#cl_state.return_status of
      ?RET_INTERNAL_ERROR -> ?RET_INTERNAL_ERROR;
      _ ->      
	case OutputPlt =:= none of
	  true -> ok;
	  false -> dialyzer_plt:to_file(OutputPlt, Plt, ModDeps, PltInfo)
	end,
	case StoredWarnings =:= [] of
	  true -> ?RET_NOTHING_SUSPICIOUS;
	  false -> ?RET_DISCREPANCIES
	end
    end,
  case ErlangMode of
    false ->
      print_warnings(State),
      print_ext_calls(State),
      maybe_close_output_file(State),
      RetValue;
    true -> 
      {RetValue, process_warnings(StoredWarnings), StoredErrors}
  end.

print_ext_calls(#cl_state{report_mode=quiet}) ->
  ok;
print_ext_calls(#cl_state{output=Output, external_calls=Msg, 
			  stored_warnings=Warnings}) ->
  case Warnings =:= [] of
    true -> io:format(Output, "\n~s", [Msg]); %% Need to do a newline first
    false -> io:format(Output, "~s", [Msg])
  end.

print_warnings(#cl_state{stored_warnings=[]}) ->
  ok;
print_warnings(#cl_state{output=Output, stored_warnings=Warnings}) ->
  Warnings1 = process_warnings(Warnings),
  String = lists:flatten([dialyzer:format_warning(W) || W <- Warnings1]),
  io:format(Output, "\n~s", [String]).

process_warnings(Warnings) ->
  Warnings1 = lists:keysort(2, Warnings), %% Sort on file/line
  remove_duplicate_warnings(Warnings1, []).

remove_duplicate_warnings([Duplicate, Duplicate|Left], Acc) ->
  remove_duplicate_warnings([Duplicate|Left], Acc);
remove_duplicate_warnings([NotDuplicate|Left], Acc) ->
  remove_duplicate_warnings(Left, [NotDuplicate|Acc]);
remove_duplicate_warnings([], Acc) ->
  lists:reverse(Acc).

%% ----------------------------------------------------------------
%%
%%  Run the analysis
%%

build_analysis_record(DialyzerOptions) ->
  Files0 = ordsets:from_list(DialyzerOptions#options.files),
  Files1 = ordsets:from_list(lists:concat([filelib:wildcard(F) 
					   || F <- Files0])),
  From = DialyzerOptions#options.from,
  Files2 = add_files_rec(DialyzerOptions#options.files_rec, From),  
  Files = ordsets:union(Files1, Files2),
  {InitPlt, PltInfo} = 
    dialyzer_plt:plt_and_info_from_file(DialyzerOptions#options.init_plt),
  {#analysis{type=DialyzerOptions#options.analysis_type,
	     defines=DialyzerOptions#options.defines,
	     include_dirs=DialyzerOptions#options.include_dirs,
	     plt=InitPlt,
	     files=Files, 
	     start_from=From, 
	     use_contracts=DialyzerOptions#options.use_contracts},
   PltInfo}.

add_files_rec(Files, From) ->
  Files1 = ordsets:from_list(Files), 
  Dirs1 = ordsets:filter(fun(X) -> filelib:is_dir(X) end, Files1),  
  Dirs2 = ordsets:union(Dirs1, all_subdirs(Dirs1)),  
  FinalFiles = ordsets:union(Files1, Dirs2),
  case From of
    byte_code -> filter_files(FinalFiles, ".beam");
    src_code -> filter_files(FinalFiles, ".erl")
  end.

-spec(all_subdirs/1 :: ([string()]) -> [string()]).
all_subdirs(Dirs) ->
  all_subdirs(Dirs, []).

-spec(all_subdirs/2 :: ([string()], [string()]) -> [string()]).
all_subdirs([Dir|T], Acc) ->
  {ok, Files} = file:list_dir(Dir),
  SubDirs = lists:zf(fun(F) ->
                       SubDir = filename:join(Dir, F),
                       case filelib:is_dir(SubDir) of
                         true -> {true, SubDir};
                         false -> false
                       end
		     end, Files),
  NewAcc = ordsets:union(ordsets:from_list(SubDirs), Acc),
  all_subdirs(T++SubDirs, NewAcc);
all_subdirs([], Acc) ->
  Acc.

-spec(filter_files/2 :: ([string()], string()) -> [string()]).
filter_files(Files, Extension) ->
  Fun = fun(X) -> 
	    filename:extension(X) =:= Extension
	      orelse 
		(filelib:is_dir(X) andalso contains_files(X, Extension))
	end,
  lists:filter(Fun, Files).

-spec(contains_files/2 :: (string(), string()) -> bool()).
contains_files(Dir, Extension) ->
  {ok, Files} = file:list_dir(Dir),
  lists:any(fun(X) -> filename:extension(X) =:= Extension end, Files).

-spec(run_analysis/2 :: (#cl_state{}, #analysis{}) -> #cl_state{}).
run_analysis(State, Analysis) ->
  Self = self(),
  LegalWarnings = State#cl_state.legal_warnings,
  Fun = fun() -> 
	    dialyzer_analysis_callgraph:start(Self, LegalWarnings, Analysis)
	end,
  BackendPid = spawn_link(Fun),
  State#cl_state{backend_pid=BackendPid}.
