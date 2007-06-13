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
%% Copyright 2006, Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
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

-record(cl_state, {backend_pid,
		   init_plt,
		   legal_warnings=[],
		   nof_warnings=0::integer(),
		   output=standard_io,
		   output_plt,
		   quiet::bool(),
		   return_status=?RET_NOTHING_SUSPICIOUS::integer(),
		   stored_errors=do_not_store,
		   stored_warnings=do_not_store,
		   user_plt
		  }).

start(#options{} = DialyzerOptions) ->
  process_flag(trap_exit, true),
  StoreWarningsAndErrors =
    case DialyzerOptions#options.erlang_mode of
      true -> [];
      false -> do_not_store
    end,
  State = new_state(DialyzerOptions#options.init_plt),
  NewState1 = init_output(State, DialyzerOptions),  
  NewState2 = 
    NewState1#cl_state{legal_warnings=DialyzerOptions#options.legal_warnings,
		       output_plt=DialyzerOptions#options.output_plt,
		       stored_errors=StoreWarningsAndErrors,
		       stored_warnings=StoreWarningsAndErrors,
		       quiet=DialyzerOptions#options.quiet},
  InitAnalysis = build_analysis_record(NewState2, DialyzerOptions),
  NewState3 = run_analysis(NewState2, InitAnalysis),
  cl_loop(NewState3).

check_init_plt(Opts, Force) ->
  process_flag(trap_exit, true),
  Quiet = Opts#options.quiet,
  case dialyzer_plt:check_init_plt(Opts#options.init_plt) of
    {fail, MD5, DiffMd5, Libs, InitPlt} ->      
      if Quiet -> ok;
	 true  -> 
	  io:format(" no\n", []),
	  case Quiet =:= verbose of
	    true -> print_md5_diff(DiffMd5);
	    false -> ok
	  end
      end,
      case (not Force) andalso check_if_installed() of
	true -> 
	  Msg = "    The initial PLT is not up-to-date.\n"
	    "    Since Dialyzer is installed no new PLT will be built.\n"
	    "    To force the rebuilding use the option --check_init_plt\n",
	  error(Msg);
	false ->
	  Msg = "    Creating initial PLT"
	    " (will take several minutes; please be patient)\n",
	  io:format("~s", [Msg]),
	  InclDirs = Opts#options.include_dirs,
	  {T1, _} = statistics(wall_clock),
	  Ret =
	    case create_init_plt(MD5, Libs, InitPlt, InclDirs) of
	      ?RET_INTERNAL_ERROR ->
		error("Problems during consistency check of initial PLT");
	      ?RET_NOTHING_SUSPICIOUS -> ?RET_NOTHING_SUSPICIOUS;
	      ?RET_DISCREPANCIES_FOUND -> ?RET_NOTHING_SUSPICIOUS
	    end,
	  {T2, _} = statistics(wall_clock),
	  if Quiet -> ok;
	     true -> print_elapsed_time(T1, T2)
	  end,
	  Ret
      end;
    {ok, _InitPlt} ->
      if Quiet -> ok;
	 true  -> io:format(" yes\n")
      end,
      ?RET_NOTHING_SUSPICIOUS;
    {error, Msg} ->
      if Quiet -> ok;
	 true  -> io:format(" no\n")
      end,
      error(Msg)
  end.

print_elapsed_time(T1, T2) ->
  ElapsedTime = T2 - T1,
  Mins = ElapsedTime div 60000,
  Secs = (ElapsedTime rem 60000) / 1000,
  io:format("  Done building PLT in ~wm~.2fs\n", [Mins, Secs]).


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

create_init_plt(MD5, Libs, InitPlt, IncludeDirs) ->  
  hipe_compile(),
  State = new_state_no_init(),
  State1 = State#cl_state{output_plt=InitPlt},
  Files = [filename:join(code:lib_dir(Lib), "ebin")|| Lib <- Libs],
  Analysis = #analysis{files=Files, 
		       init_plt=dialyzer_plt:new(dialyzer_empty_plt),
		       include_dirs=IncludeDirs,
		       plt_info={MD5, Libs},
		       start_from=byte_code,
		       type=plt_build,
		       user_plt=State1#cl_state.user_plt,
		       supress_inline=true},
  cl_loop(run_analysis(State1, Analysis)).

hipe_compile() ->
  case erlang:system_info(hipe_architecture) of
    undefined -> ok;
    ultrasparc -> ok;
    _ ->
      {ok, lists}                       = hipe:c(lists),
      {ok, dict}                        = hipe:c(dict),
      {ok, dialyzer_succ_typings}       = hipe:c(dialyzer_succ_typings),
      {ok, dialyzer_analysis_callgraph} = hipe:c(dialyzer_analysis_callgraph),
      {ok, dialyzer_typesig}            = hipe:c(dialyzer_typesig),
      {ok, dialyzer_dataflow}           = hipe:c(dialyzer_dataflow),
      {ok, dialyzer_codeserver}         = hipe:c(dialyzer_codeserver),
      {ok, erl_types}                   = hipe:c(erl_types),
      {ok, erl_bif_types}               = hipe:c(erl_bif_types),
      {ok, cerl}                        = hipe:c(cerl, [no_concurrent_comp]),
      ok
  end.

new_state(InitPlt) ->
  NewInitPlt = dialyzer_plt:from_file(dialyzer_init_plt, InitPlt),
  new_state1(NewInitPlt).

new_state_no_init() ->
  new_state1(none).

new_state1(InitPlt) ->
  UserPLT = dialyzer_plt:new(dialyzer_user_plt),
  #cl_state{user_plt=UserPLT, init_plt=InitPlt}.

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

maybe_close_output_file(State) ->
  case State#cl_state.output of
    standard_io -> ok;
    File -> file:close(File)
  end.

%% ----------------------------------------------------------------
%%
%%  Main Loop
%%

cl_loop(State) ->
  BackendPid = State#cl_state.backend_pid,
  receive
    {BackendPid, log, _LogMsg} ->
      %% io:format(State#cl_state.output ,"Log: ~s", [_LogMsg]),
      cl_loop(State);
    {BackendPid, warnings, Warnings} ->
      NewState = store_warnings(State, Warnings),
      cl_loop(NewState);
    {BackendPid, error, Msg} ->
      State1 = store_error(State, Msg),
      cl_loop(State1#cl_state{return_status=?RET_INTERNAL_ERROR});
    {BackendPid, done} ->
      return_value(State);
    {BackendPid, ext_calls, ExtCalls} ->
      Msg = io_lib:format("Unknown functions: ~p\n", [ExtCalls]),
      NewState = print_ext_calls(State, Msg),
      cl_loop(NewState);
    {'EXIT', BackendPid, {error, Reason}} ->
      Msg = failed_anal_msg(Reason),
      error(State, Msg);
    {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
      Msg = failed_anal_msg(Reason),
      maybe_close_output_file(State),
      error(State, Msg);
    _Other ->
      %% io:format("Received ~p\n", [_Other]),
      cl_loop(State)
  end.

failed_anal_msg(Reason) ->
  io_lib:format("Analysis failed with error report:\n\t~P", [Reason, 12]).

print_ext_calls(State = #cl_state{quiet=true}, _Msg) ->
  State;
print_ext_calls(State = #cl_state{}, Msg) ->
  print_warning_string(State, Msg),
  State.

store_error(State = #cl_state{stored_errors=StoredErrors}, Msg) ->
  NewStoredErrors =
    case StoredErrors =:= do_not_store of
      true ->
	error(State, Msg);
      false ->
	StoredErrors ++ [Msg]
    end,
  State#cl_state{stored_errors=NewStoredErrors}.

store_warnings(State = #cl_state{nof_warnings=NofOldWarnings,
				 stored_warnings=StoredWarnings}, Warnings) ->
  NewStoredWarnings =
    case StoredWarnings =:= do_not_store of
      true ->
	WarningString = format_warnings(Warnings),
	print_warning_string(State, WarningString),
	StoredWarnings;
      false ->
	StoredWarnings ++ Warnings
    end,
  NofNewWarning = length(Warnings),
  State#cl_state{nof_warnings=NofOldWarnings+NofNewWarning, 
		 stored_warnings=NewStoredWarnings}.

format_warnings(Warnings) ->
  MapFun =
    fun({{M, F, A} = MFA, Msg}) when is_atom(M), is_atom(F), is_integer(A)-> 
	io_lib:format("~w: ~s", [MFA, Msg]);
       ({{File, Line}, Msg}) when is_list(File), is_integer(Line) ->
	BaseName = filename:basename(File),
	io_lib:format("~s:~w: ~s", [BaseName, Line, Msg])
    end,
  lists:flatten(lists:map(MapFun, lists:keysort(1, Warnings))).

print_warning_string(#cl_state{nof_warnings=NofWarn, output=Output}, String) ->
  case NofWarn of
    0 -> io:format(Output, "\n", []); %% warnings are just starting to appear
    _ -> ok
  end,
  io:format(Output, "~s", [String]).

error(Msg) ->
  throw({dialyzer_error, Msg}).

error(State, Msg) ->
  case State#cl_state.output of
    standard_io -> ok;
    Outfile -> io:format(Outfile, "\n~s\n", [Msg])
  end,
  throw({dialyzer_error, Msg}).

return_value(State = #cl_state{nof_warnings=NofWarnings, output_plt=OutputPlt,
			       stored_errors=StoredErrors,
			       stored_warnings=StoredWarnings,
			       user_plt=UserPlt, init_plt=InitPlt}) ->
  maybe_close_output_file(State),
  RetValue =
    case State#cl_state.return_status of
      ?RET_INTERNAL_ERROR -> ?RET_INTERNAL_ERROR;
      _ ->      
	case OutputPlt =:= undefined of
	  true -> ok;
	  false ->
	    dialyzer_plt:merge_and_write_file([InitPlt, UserPlt], OutputPlt)
	end,
	if NofWarnings =:= 0 -> ?RET_NOTHING_SUSPICIOUS;
	   true              -> ?RET_DISCREPANCIES_FOUND
	end
    end,
  case StoredWarnings =:= do_not_store of
    true ->
      %% Assert
      do_not_store = StoredErrors,
      RetValue;
    false ->
      %% Assert
      false = StoredErrors =:= do_not_store,
      {RetValue, StoredWarnings, StoredErrors}
  end.


%% ----------------------------------------------------------------
%%
%%  Run the analysis
%%

build_analysis_record(State, DialyzerOptions) ->
  PLT = State#cl_state.user_plt,
  From = DialyzerOptions#options.from,
  IncludeDirs = DialyzerOptions#options.include_dirs,
  Defines = DialyzerOptions#options.defines,
  SupressInline = DialyzerOptions#options.supress_inline,
  Files0 = ordsets:from_list(DialyzerOptions#options.files),
  Files1 = ordsets:from_list(lists:concat([filelib:wildcard(F) 
					   || F <- Files0])),
  Files2 = add_files_rec(DialyzerOptions#options.files_rec, From),  
  Files = ordsets:union(Files1, Files2),
  AnalType = DialyzerOptions#options.analysis_type,
  InitPlt = State#cl_state.init_plt,
  #analysis{type=AnalType,
	    defines=Defines,
	    include_dirs=IncludeDirs, init_plt=InitPlt, user_plt=PLT, 
	    files=Files, start_from=From, supress_inline=SupressInline}.

add_files_rec(Files, From) ->
  Files1 = ordsets:from_list(Files), 
  Dirs1 = ordsets:filter(fun(X) -> filelib:is_dir(X) end, Files1),  
  Dirs2 = ordsets:union(Dirs1, all_subdirs(Dirs1)),  
  FinalFiles = ordsets:union(Files1, Dirs2),
  case From of
    byte_code -> filter_files(FinalFiles, ".beam");
    src_code -> filter_files(FinalFiles, ".erl")
  end.

all_subdirs(Dirs) ->
  all_subdirs(Dirs, []).

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

filter_files(Files, Extension) ->
  Fun = fun(X) -> 
	    filename:extension(X) =:= Extension
	      orelse 
		(filelib:is_dir(X) andalso contains_files(X, Extension))
	end,
  lists:filter(Fun, Files).

contains_files(Dir, Extension) ->
  {ok, Files} = file:list_dir(Dir),
  lists:any(fun(X) -> filename:extension(X) =:= Extension end, Files).


run_analysis(State, Analysis) ->
  Self = self(),
  LegalWarnings = State#cl_state.legal_warnings,
  Fun = fun() -> 
	    dialyzer_analysis_callgraph:start(Self, LegalWarnings, Analysis)
	end,
  BackendPid = spawn_link(Fun),
  State#cl_state{backend_pid=BackendPid}.
