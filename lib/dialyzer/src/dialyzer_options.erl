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
		  ?WARN_COMP,
		  ?WARN_GUARDS,
		  ?WARN_OLD_BEAM,
		  ?WARN_FAILING_CALL],
  build_options(Opts, #options{legal_warnings=ordsets:from_list(DefaultWarns)}).

build_options([Term={OptionName,Value}|Rest], Options) ->
  case OptionName of
    files ->
      build_options(Rest, Options#options{files=Value});
    files_rec ->
      build_options(Rest, Options#options{files_rec=Value});
    core_transform ->
      build_options(Rest, Options#options{core_transform=Value});
    defines ->
      build_options(Rest, Options#options{defines=ordsets:from_list(Value)});
    from ->
      build_options(Rest, Options#options{from=Value});
    init_plt ->
      build_options(Rest, Options#options{init_plt=Value});
    include_dirs ->
      build_options(Rest, Options#options{include_dirs=Value});
    output_file ->
      build_options(Rest, Options#options{output_file=Value});
    output_plt ->
      build_options(Rest, Options#options{output_plt=Value});
    plt_libs ->
      case Value of
	[] -> build_options(Rest, Options);
	_  -> build_options(Rest, Options#options{plt_libs=Value})
      end;
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

bad_option(Term) ->
  report("error building dialyzer options: ~P.\n",[Term,15]),
  exit(error).


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

report(S, As) ->
  io:nl(),
  io:fwrite(S, As),
  io:nl().
