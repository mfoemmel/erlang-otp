%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
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
%% Copyright 2006, 2007 Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

%%%-------------------------------------------------------------------
%%% File        : dialyzer.erl
%%% Authors     : Tobias Lindahl <tobiasl@it.uu.se>
%%%               Kostis Sagonas <kostis@it.uu.se>
%%% Description : This is the interface for the Dialyzer tool.
%%%
%%% Created     : 27 Apr 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer).

%%%-------------------------------------------------------------------
%%% NOTE: Only functions exported by this module are available to
%%%       other applications.
%%%-------------------------------------------------------------------
-export([plain_cl/0, 
	 run/1, 
	 gui/0,
	 gui/1,
	 format_warning/1]).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------
%% Interfaces:
%%  - plain_cl/0 :      to be used ONLY by the dialyzer C program.
%%  - run/1:            Erlang interface for a command line-like analysis
%%  - gui/0/1:          Erlang interface for the gui.
%%  - format_warning/1: Get the string representation of a warning.
%%--------------------------------------------------------------------

-spec(plain_cl/0 :: () -> no_return()).

plain_cl() ->
  case dialyzer_cl_parse:start() of
    {check_init, Opts} -> 
      cl_halt(cl_check_init(Opts, true), Opts);
    {gui, Opts} ->
      case cl_check_init(Opts) of
	{error, _} = Error -> gui_halt(Error, Opts);
	{ok, ?RET_NOTHING_SUSPICIOUS} ->
	  if Opts#options.report_mode =:= quiet -> ok;
	     true -> io:put_chars("  Proceeding with startup...\n")
	  end,
	  gui_halt(internal_gui(Opts), Opts)
      end;
    {cl, Opts} -> 
      case cl_check_init(Opts) of
	{error, _} = Error -> cl_halt(Error, Opts);
	{ok, ?RET_NOTHING_SUSPICIOUS} ->
	  if Opts#options.report_mode =:= quiet -> ok;
	     true  -> io:put_chars("  Proceeding with analysis... ")
	  end,
	  cl_halt(cl(Opts), Opts)
      end;
    {error, Msg} -> 
      cl_error(Msg)
  end.

cl_check_init(Opts) ->
  cl_check_init(Opts, false).

cl_check_init(Opts, Force) ->
  if Opts#options.report_mode =:= quiet -> ok;
     true -> io:put_chars("  Checking whether the initial "
			  "PLT exists and is up-to-date...")
  end,
  F = fun() ->
	  dialyzer_cl:check_init_plt(Opts, Force)
      end,
  doit(F).

cl(Opts) ->
  F = fun() ->
	  dialyzer_cl:start(Opts)
      end,
  doit(F).

-spec(run/1 :: (dial_options()) ->
	 {'ok',[dial_warning()]} | {'error',[dial_warning()],[dial_error()]}).

run(Opts) when length(Opts) > 0 ->
  try
    case dialyzer_options:build([{report_mode, quiet}, 
				 {erlang_mode, true}|Opts]) of
      {error, Msg} -> throw({dialyzer_error, Msg});
      OptsRecord ->
	case cl_check_init(OptsRecord) of
	  {ok, ?RET_NOTHING_SUSPICIOUS} ->
	    case dialyzer_cl:start(OptsRecord) of
	      {?RET_DISCREPANCIES, Warnings, []}      -> {ok, Warnings};
	      {?RET_NOTHING_SUSPICIOUS, [], []}       -> {ok, []};
	      {?RET_INTERNAL_ERROR, Warnings, Errors} -> {error, Warnings, 
							  Errors}
	    end;
	  {error, ErrorMsg1} ->
	    throw({dialyzer_error, ErrorMsg1})
	end
    end
  catch
    throw:{dialyzer_error, ErrorMsg} -> 
      erlang:error({dialyzer_error, lists:flatten(ErrorMsg)})
  end.

internal_gui(OptsRecord) ->
  F = fun() ->
	  dialyzer_gui:start(OptsRecord),
	  ?RET_NOTHING_SUSPICIOUS
      end,
  doit(F).

-spec(gui/0 :: () -> 'ok').

gui() ->
  gui([]).

-spec(gui/1 :: (dial_options()) -> 'ok').

gui(Opts) ->
  try
    case dialyzer_options:build([{report_mode, quiet}|Opts]) of
      {error, Msg} -> throw({dialyzer_error, Msg});
      OptsRecord ->
	case cl_check_init(OptsRecord) of
	  {ok, ?RET_NOTHING_SUSPICIOUS} ->
	    F = fun() ->
		    dialyzer_gui:start(OptsRecord)
		end,
	    case doit(F) of
	      {ok, _} -> ok;
	      {error, Msg} -> throw({dialyzer_error, Msg})
	    end;
	  {error, ErrorMsg1} ->
	    throw({dialyzer_error, ErrorMsg1})
	end
    end
  catch
    throw:{dialyzer_error, ErrorMsg} ->
      erlang:error({dialyzer_error, lists:flatten(ErrorMsg)})
  end.

%%-----------
%% Machinery
%%-----------

doit(F) ->
  try 
    {ok, F()}
  catch
    throw:{dialyzer_error, Msg} ->
      {error, lists:flatten(Msg)}
  end.

cl_error(Msg) ->
  cl_halt({error, Msg}, #options{}).

gui_halt(R, Opts) ->
  cl_halt(R, Opts#options{report_mode=quiet}).

-spec(cl_halt/2 ::
      ({'ok',atom()} | {'error',string()}, #options{}) -> no_return()).

cl_halt({ok, R = ?RET_NOTHING_SUSPICIOUS},  #options{report_mode=quiet}) -> 
  halt(R);
cl_halt({ok, R = ?RET_DISCREPANCIES}, #options{report_mode=quiet}) -> 
  halt(R);
cl_halt({ok, R = ?RET_NOTHING_SUSPICIOUS},  #options{}) ->
  io:put_chars("done (passed successfully)\n"),
  halt(R);
cl_halt({ok, R = ?RET_DISCREPANCIES},  #options{output_file=Output}) ->
  io:put_chars("done (warnings were emitted)\n"),
  cl_check_log(Output),
  halt(R);
cl_halt({ok, R = ?RET_INTERNAL_ERROR},  #options{output_file=Output}) ->
  Msg = "dialyzer: Internal problems were encountered in the analysis.",
  io:format("~s\n", [Msg]),
  cl_check_log(Output),
  halt(R);  
cl_halt({error, Msg1}, #options{output_file=Output}) ->
  Msg2 = "dialyzer: Internal problems were encountered in the analysis.",
  io:format("\n~s\n~s\n", [Msg1, Msg2]),
  cl_check_log(Output),
  halt(?RET_INTERNAL_ERROR).

-spec(cl_check_log/1 :: (string()) -> 'ok').

cl_check_log("") ->
  ok;
cl_check_log(Output) ->
  io:format("  Check output file `~s' for details\n", [Output]).

-spec(format_warning/1 :: (dial_warning()) -> string()).

format_warning({_Tag, {File, Line}, Msg}) when is_list(File), 
					       is_integer(Line) ->
  BaseName = filename:basename(File),
  String = lists:flatten(message_to_string(Msg)),
  lists:flatten(io_lib:format("~s:~w: ~s", [BaseName, Line, String])).

message_to_string({binary_construction, [Size, Seg, Type]}) ->
  io_lib:format("Binary construction will fail since the size field ~s in "
		"binary segment ~s has type ~s\n",
		[Size, Seg, Type]);
message_to_string({fun_app_no_fun, [Op, Type]}) ->
  io_lib:format("Fun application will fail since ~s :: ~s is not a function\n",
		[Op, Type]);
message_to_string({fun_app_args, [Args, Type]}) ->
  io_lib:format("Fun application with arguments ~s will fail "
		"since the function has type ~s\n",
		 [Args, Type]);
message_to_string({call, [M, F, Args, ArgNs, FailReason, 
			  SigArgs, SigRet, Contract]}) ->
  io_lib:format("The call ~w:~w~s ", [M, F, Args]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({apply, [Args, ArgNs, FailReason,
			   SigArgs, SigRet, Contract]}) ->
  io_lib:format("Fun application with arguments ~s ", [Args]) ++
    call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({exact_eq, [Type1, Type2]}) ->
  io_lib:format("~s =:= ~s can never evaluate to 'true'\n", [Type1, Type2]);
message_to_string({improper_list_constr, [TlType]}) ->
  io_lib:format("Cons will produce an improper list since its "
		"2nd argument is ~s\n", [TlType]);
message_to_string({record_constr, [Types, Name]}) ->
  io_lib:format("Record construction ~s violates the "
		"declared type for #~w{}\n", [Types, Name]);
message_to_string({record_constr, [Name, Field, Type]}) ->
  io_lib:format("Record construction violates the declared type for #~w{}"
		" since ~s cannot be of type ~s\n", 
		[Name, Field, Type]);
message_to_string({pattern_match_cov, [Pat, Type]}) ->
  io_lib:format("The ~s can never match since previous"
		" clauses completely covered the type ~s\n",
		[Pat, Type]);
message_to_string({pattern_match, [Pat, Type]}) ->
  io_lib:format("The ~s can never match the type ~s\n", [Pat, Type]);
message_to_string({guard_fail, []}) ->
  "Clause guard cannot succeed.\n";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}) ->
  io_lib:format("Guard test ~s ~s ~s can never succeed.\n",
		[Arg1, Infix, Arg2]);
message_to_string({guard_fail, [Guard, Args]}) ->
  io_lib:format("Guard test ~w~s can never succeed\n", [Guard, Args]);
message_to_string({guard_fail_pat, [Pat, Type]}) ->
  io_lib:format("Clause guard cannot succeed. The ~s was matched"
		" against the type ~s\n", [Pat, Type]);
message_to_string({unused_fun, []}) ->
  io_lib:format("Function will never be called\n", []);
message_to_string({unused_fun, [F, A]}) ->
  io_lib:format("Function ~w/~w will never be called\n", [F, A]);
message_to_string({no_return, [Type|Name]}) ->
  NameString =
    case Name of
      [] -> "Function ";
      [F, A] -> io_lib:format("Function ~w/~w ", [F, A])
    end,
  case Type of
    only_explicit -> NameString ++ "only terminates with explicit exception\n";
    only_normal -> NameString ++ "has no local return\n";
    both -> NameString ++ "has no local return\n"
  end;
message_to_string({spec_missing_fun, [M, F, A]}) ->
  io_lib:format("Contract for function that does not exist: ~w:~w/~w\n",
		[M,F,A]);
message_to_string({invalid_contract, [M, F, A, Sig]}) ->
  io_lib:format("Invalid type specification for function ~w:~w/~w. "
		"The success typing is ~s\n", 
		[M, F, A, Sig]);
message_to_string({overlapping_contract, []}) ->
  "Overloaded contract has overlapping domains;"
    " such contracts are currently unsupported and are simply ignored \n";
message_to_string({contract_subtype, [M, F, A, Contract, Sig]}) ->
  io_lib:format("Type specification ~w:~w/~w :: ~s "
		"is a subtype of the success typing: ~s\n", 
		[M, F, A, Contract, Sig]);
message_to_string({contract_supertype, [M, F, A, Contract, Sig]}) ->
  io_lib:format("Type specification ~w:~w/~w :: ~s "
		"is a supertype of the success typing: ~s\n", 
		[M, F, A, Contract, Sig]);
message_to_string({contract_diff, [M, F, A, Contract, Sig]}) ->
  io_lib:format("Type specification ~w:~w/~w :: ~s "
		"is not equal to the success typing: ~s\n", 
		[M, F, A, Contract, Sig]);
message_to_string({call_to_missing, [M, F, A]}) ->
  io_lib:format("Call to missing or unexported function ~w:~w/~w\n", [M, F, A]);
message_to_string({unmatched_return, [Type]}) ->
  io_lib:format("Expression produces a value of type ~s, "
		"but this value is unmatched\n", [Type]).

call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, 
			{IsOverloaded, Contract}) ->
  PositionString =
    case ArgNs of
      [] -> [];
      [N1] -> io_lib:format("position ~w", [N1]);
      [_|_] -> 
	" and"++ArgString = lists:flatten([io_lib:format(" and ~w", [N]) 
					   || N <- ArgNs]),
	"positions" ++ ArgString
    end,
  case FailReason of
    only_sig ->
      case ArgNs =:= [] of
	true -> 
	  %% We do not know which arguments that caused the failure. 
	  io_lib:format("will fail since the success typing arguments"
			" are ~s\n", [SigArgs]);
	false ->
	  io_lib:format("will fail since it differs in argument" 
			" ~s from the success typing arguments: ~s\n", 
			[PositionString, SigArgs])
      end;
    only_contract -> 
      case (ArgNs =:= []) orelse IsOverloaded of
	true ->
	  %% We do not know which arguments that caused the failure. 
	  io_lib:format("breaks the contract ~s\n", [Contract]);
	false ->
	  io_lib:format("breaks the contract ~s in argument ~s\n",
			[Contract, PositionString])
      end;
    both  ->
      io_lib:format("will fail since the success typing is ~s -> ~s and "
		    "the contract is ~s\n", [SigArgs, SigRet, Contract])
  end.
