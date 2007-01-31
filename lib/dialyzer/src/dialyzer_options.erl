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

%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : dialyzer_options.erl
%%% Authors : Richard Carlsson <richardc@csd.uu.se>
%%% Description : Provides a better way to start Dialyzer from a script.
%%%
%%% Created : 17 Oct 2004 by Richard Carlsson <richardc@csd.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_options).

-export([build/1]).

-include("dialyzer.hrl").
-include("hipe_icode_type.hrl").

build(Opts) ->
  DefaultWarns = [?WARN_RETURN_NO_RETURN,
		  ?WARN_NOT_CALLED,
		  ?WARN_NON_PROPER_LIST,
		  ?WARN_TUPLE_AS_FUN,
		  ?WARN_FUN_APP,
		  ?WARN_MATCHING,
		  ?WARN_CALLGRAPH,
		  ?WARN_COMP,
		  ?WARN_GUARDS,
		  ?WARN_OLD_BEAM,
		  ?WARN_FAILING_CALL,
		  ?WARN_CALLGRAPH],
  DefaultWarns1 = ordsets:from_list(DefaultWarns),
  InitPlt=filename:join([code:lib_dir(dialyzer),"plt","dialyzer_init_plt"]),
  DefaultOpts = #options{},
  DefaultOpts1 = DefaultOpts#options{legal_warnings=DefaultWarns1,
				      init_plt=InitPlt},
  try 
    build_options(Opts, DefaultOpts1)
  catch
    throw:{dialyzer_options_error, Msg} -> {error, Msg}
  end.

build_options([{_OptionName, undefined}|Rest], Options) ->
  build_options(Rest, Options);
build_options([Term = {OptionName, Value}|Rest], Options) ->
  case OptionName of
    files ->
      assert_filenames(Term, Value),
      build_options(Rest, Options#options{files=Value});
    files_rec ->
      assert_filenames(Term, Value),
      build_options(Rest, Options#options{files_rec=Value});
    core_transform ->
      build_options(Rest, Options#options{core_transform=Value});
    defines ->
      assert_defines(Term, Value),
      OldVal = Options#options.defines,
      NewVal = ordsets:union(ordsets:from_list(Value), OldVal),
      build_options(Rest, Options#options{defines=NewVal});
    from when Value =:= byte_code; Value =:= src_code ->
      build_options(Rest, Options#options{from=Value});
    init_plt ->
      assert_filenames([Term], [Value]),
      build_options(Rest, Options#options{init_plt=Value});
    include_dirs ->
      assert_filenames(Term, Value),
      OldVal = Options#options.include_dirs,
      NewVal = ordsets:union(ordsets:from_list(Value), OldVal),
      build_options(Rest, Options#options{include_dirs=NewVal});
    output_file ->
      assert_filenames([Term], [Value]),
      build_options(Rest, Options#options{output_file=Value});
    output_plt ->
      assert_filenames([Term], [Value]),
      build_options(Rest, Options#options{output_plt=Value});
    quiet ->
      build_options(Rest, Options#options{quiet=Value});
    erlang_mode ->
      build_options(Rest, Options#options{erlang_mode=true});
    supress_inline ->
      build_options(Rest, Options#options{supress_inline=Value});
    warnings ->
      NewWarnings = build_warnings(Value, Options#options.legal_warnings),
      build_options(Rest, Options#options{legal_warnings=NewWarnings});
    _ ->
      bad_option(Term)
  end;
build_options([Term|_Rest], _Options) ->
  bad_option(Term);
build_options([], Options) ->
  Options.

assert_filenames(Term, [FileName|Left]) when length(FileName) >= 0 ->
  assert_filenames(Term, Left);
assert_filenames(_Term, []) ->
  ok;
assert_filenames(Term, [_|_]) ->
  bad_option(Term).

assert_defines(Term, [{Macro, _Value}|Left]) when is_atom(Macro) ->
  assert_defines(Term, Left);
assert_defines(_Term, []) ->
  ok;
assert_defines(Term, [_|_]) ->
  bad_option(Term).


bad_option(Term) ->
  Msg = io_lib:format("Illegal dialyzer option: ~P.\n",[Term,15]),
  throw({dialyzer_options_error, Msg}).


build_warnings([Opt|Left], Warnings) ->
  NewWarnings =
    case Opt of
      no_return ->
	ordsets:del_element(?WARN_RETURN_NO_RETURN, Warnings);
      no_unused ->
	ordsets:del_element(?WARN_NOT_CALLED, Warnings);
      no_improper_lists ->
	ordsets:del_element(?WARN_NON_PROPER_LIST, Warnings);
      no_tuple_as_fun ->
	ordsets:del_element(?WARN_TUPLE_AS_FUN, Warnings);
      no_fun_app ->
	ordsets:del_element(?WARN_FUN_APP, Warnings);
      no_match ->
	ordsets:del_element(?WARN_MATCHING, Warnings);
      no_comp ->
	ordsets:del_element(?WARN_COMP, Warnings);
      no_guards ->
	ordsets:del_element(?WARN_GUARDS, Warnings);
      no_unsafe_beam ->
	ordsets:del_element(?WARN_OLD_BEAM, Warnings);
      no_fail_call ->
	ordsets:del_element(?WARN_FAILING_CALL, Warnings);
      error_handling ->
	ordsets:add_element(?WARN_RETURN_ONLY_EXIT, Warnings);
      Other ->
	bad_option(Other)
    end,
  build_warnings(Left, NewWarnings);
build_warnings([], Warnings) ->
  Warnings.
