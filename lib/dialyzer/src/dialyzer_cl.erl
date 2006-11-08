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

-export([start/1, check_init_plt/1]).

-include("dialyzer.hrl").	  %% file is automatically generated
-include("hipe_icode_type.hrl").

-record(cl_state, {backend_pid,
		   legal_warnings=[],
		   init_plt,
		   output=standard_io,
		   output_plt,
		   user_plt,
		   nof_warnings=0::integer(),
		   return_status=0::integer()
		  }).

start(Options) when is_list(Options) ->
  start(dialyzer_options:build(Options));
start(#options{} = DialyzerOptions) ->
  process_flag(trap_exit, true),
  State = new_state(DialyzerOptions#options.init_plt),
  NewState1 = init_output(State, DialyzerOptions),
  NewState2 = 
    NewState1#cl_state{legal_warnings=DialyzerOptions#options.legal_warnings,
		       output_plt=DialyzerOptions#options.output_plt},
  InitAnalysis = build_analysis_record(NewState2, DialyzerOptions),
  NewState3 = run_analysis(NewState2, InitAnalysis),
  cl_loop(NewState3).

check_init_plt(Opts) ->
  process_flag(trap_exit, true),
  Quiet = get(dialyzer_options_quiet),
  case dialyzer_plt:check_init_plt(Opts#options.plt_libs,
				   Opts#options.init_plt) of
    {fail, MD5, Libs, InitPlt} ->      
      if Quiet -> ok;
	 true  -> io:format(" no\n", [])
      end,
      case check_if_installed() of
	true -> 
	  Msg = "    The initial PLT is not up-to-date.\n"
	    "    Since Dialyzer is installed no new PLT will be built.\n"
	    "    Please refer to the manual.\n",
	  io:format("~s", [Msg]),
	  error;
	false ->
	  Msg = "    Creating initial PLT"
	    " (will take several minutes; please be patient)\n",
	  io:format("~s", [Msg]),
	  case create_init_plt(MD5, Libs, InitPlt, Opts#options.include_dirs) of
	    ?RET_INTERNAL_ERROR -> error;
	    ?RET_NOTHING_SUSPICIOUS -> ok;
	    ?RET_DISCREPANCIES_FOUND -> ok
	  end
      end;
    {ok, _InitPlt} ->
      if Quiet -> ok;
	 true  -> io:format(" yes\n")
      end,
      ok;
    {error, Msg} ->
      io:format(" no\n~s\n", [Msg]),
      error
  end.

check_if_installed() ->
  case filename:basename(code:lib_dir(dialyzer)) of
    "dialyzer" -> false;
    "dialyzer-" ++ _Version -> true
  end.  

create_init_plt(MD5, Libs, InitPlt, IncludeDirs) ->  
  State = new_state_no_init(),
  State1 = State#cl_state{output_plt=InitPlt},
  Files = [filename:join(code:lib_dir(Lib), "ebin")|| Lib <- Libs],
  Analysis = #analysis{fixpoint=first,
		       files=Files, 
		       granularity=all, 
		       init_plt=dialyzer_plt:new(dialyzer_empty_plt),
		       include_dirs=IncludeDirs,
		       plt_info={MD5, Libs},
		       start_from=byte_code,
		       core_transform=succ_typings,
		       user_plt=State1#cl_state.user_plt,
		       supress_inline=true},
  cl_loop(run_analysis(State1, Analysis)).

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
	  io:format("Could not open output file ~p, Reason ~p\n",
		    [OutputFile, Reason]),
	  exit(error)
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
  Output = State#cl_state.output,
  receive
    {BackendPid, log, _LogMsg} ->
      %io:format(Output,"Log: ~s", [_LogMsg]),
      cl_loop(State);
    {BackendPid, warnings, Warnings} ->
      NewState = print_warnings(State, Warnings),
      cl_loop(NewState);
    {BackendPid, error, Msg} ->
      io:format(Output, "~s", [Msg]),
      cl_loop(State#cl_state{return_status=-1});
    {BackendPid, done} ->
      return_value(State);
    {BackendPid, ext_calls, ExtCalls} ->
      Quiet = get(dialyzer_options_quiet),
      if Quiet -> ok;
	 true  -> io:format("\nUnknown functions: ~p\n", [ExtCalls])
      end,
      cl_loop(State);
    {'EXIT', BackendPid, {error, Reason}} ->
      Msg = failed_anal_msg(Reason),
      error(State, Msg);
    {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
      Msg = failed_anal_msg(Reason),
      maybe_close_output_file(State),
      error(State, Msg);
    _Other ->
      %% io:format(Output, "Received ~p\n", [Other]),
      cl_loop(State)
  end.

failed_anal_msg(Reason) ->
  io_lib:format("Analysis failed with error report: ~p\n", [Reason]).

print_warnings(State = #cl_state{output=Output}, Warnings) ->
  NofOldWarnings = State#cl_state.nof_warnings,
  case NofOldWarnings of
    0 ->
      io:format(Output, "\n", []); %% warnings are just starting to appaer
    _ ->
      ok
  end,
  io:format(Output, "~s", [Warnings]),
  NofNewWarning = length(string:tokens(lists:flatten(Warnings), "\n")),
  State#cl_state{nof_warnings=NofOldWarnings+NofNewWarning}.

error(State, Msg) ->
  io:format(State#cl_state.output, "~s", [Msg]),
  return_value(State#cl_state{return_status=-1}).

return_value(State = #cl_state{return_status=-1}) ->
  maybe_close_output_file(State),
  ?RET_INTERNAL_ERROR;
return_value(State = #cl_state{nof_warnings=NofWarnings, output_plt=OutputPlt,
			       user_plt=UserPlt, init_plt=InitPlt}) ->
  if OutputPlt =/= undefined ->
      case dialyzer_plt:merge_and_write_file([InitPlt, UserPlt], OutputPlt) of
	ok -> ok;
	{error, What} -> 
	  error(State, io_lib:format("Error while writing plt to file: ~w\n", 
				     [What]))
      end;
     true ->
      ok
  end,
  maybe_close_output_file(State),
  if NofWarnings =:= 0 ->
      ?RET_NOTHING_SUSPICIOUS;
     true ->
      ?RET_DISCREPANCIES_FOUND
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
  Files1 = ordsets:from_list(lists:concat([filelib:wildcard(F) || F <- Files0])),
  Files2 = add_files_rec(DialyzerOptions#options.files_rec, From),  
  Files = ordsets:union(Files1, Files2),
  CoreTransform = DialyzerOptions#options.core_transform,
  InitPlt = State#cl_state.init_plt,
  #analysis{fixpoint=first, core_transform=CoreTransform,
	    defines=Defines, granularity=all,
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
