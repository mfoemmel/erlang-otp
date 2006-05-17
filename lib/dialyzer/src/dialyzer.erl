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
-export([plain_cl/0, cl/1, run/1, gui/1, cl_check_init_plt/1]).

-include("dialyzer.hrl").	  %% file is automatically generated

%%--------------------------------------------------------------------
%% Interfaces:
%%  - cl/1 : to be run from the OS' command line
%%  - run/1: to be run from within the Erlang shell
%%  - gui/1: to be used by the GUI version
%%  - cl_check_init_plt/1: Auxiliary function for the command line
%%--------------------------------------------------------------------

plain_cl() ->
  dialyzer_cl_parse:start().

cl(Args) ->
  F = fun () ->
	  case parse_args(Args) of
	    [Opts] when is_list(Opts) ->
	      OptsRecord = dialyzer_options:build(Opts),
	      dialyzer_cl:start(OptsRecord);
	    Other ->
	      invalid_args("dialyzer:cl/1", Other)
	  end
      end,
  R = doit(F),
  halt(R).   %% halt with result as a return value

run(Opts) ->
  F = fun () ->
	  OptsRecord = dialyzer_options:build(Opts),
	  dialyzer_cl:start(OptsRecord)
      end,
  doit(F).  %% return result to the caller (in Erlang)

gui(Args) ->
  F = fun () ->
	  case parse_args(Args) of
	    [Opts] when is_list(Opts) ->
	      OptsRecord = dialyzer_options:build(Opts),
	      dialyzer_gui:start(OptsRecord);
	    Other ->
	      invalid_args("dialyzer:gui/1", Other)
	  end
      end,
  doit(F).

cl_check_init_plt(Args) ->  
  F = fun () -> 
	  case parse_args(Args) of
	    [Opts] when is_list(Opts) ->
	      OptsRecord = dialyzer_options:build(Opts),
	      dialyzer_cl:check_init_plt(OptsRecord);
	    Other ->
	      invalid_args("dialyzer:gui/1", Other)
	  end
      end,
  doit(F).

%%-----------
%% Machinery
%%-----------

doit(F) ->
  wait_init(),
  case catch {ok, F()} of
    {ok, Result} ->
      Result;
    {'EXIT', E} ->
      report("terminated abnormally: ~P.", [E, 10]),
      ?RET_INTERNAL_ERROR;
    Thrown ->
      report("throw without catch: ~P.", [Thrown, 15]),
      ?RET_INTERNAL_ERROR
  end.

wait_init() ->
  case erlang:whereis(code_server) of
    undefined ->
      erlang:yield(),
      wait_init();
    _ ->
      ok
  end.

invalid_args(Where, Args) ->
  report("invalid arguments to ~s: ~w.", [Where, Args]),
  exit(error).

parse_args([A | As]) when atom(A) ->
  [parse_arg(atom_to_list(A)) | parse_args(As)];
parse_args([A | As]) ->
  [parse_arg(A) | parse_args(As)];
parse_args([]) ->
  [].

parse_arg(A) ->
  case catch {ok, parse(A)} of
    {ok, Expr} ->
      case catch erl_parse:normalise(Expr) of
	{'EXIT', _} ->
	  report("bad argument: '~s':", [A]),
	  exit(error);
	Term ->
	  Term
      end;
    {error, S} ->
      report("error parsing argument '~s'", [A]),
      report(S, []),
      exit(error)
  end.

parse(S) ->
  case erl_scan:string(S ++ ".", 1) of
    {ok, Ts, _} ->
      case erl_parse:parse_exprs(Ts) of
	{ok, [Expr]} ->
	  Expr;
	{error, E} ->
	  parse_error(E)
      end;
    {error, E, _} ->
      parse_error(E)
  end.

parse_error({_L, M, D}) ->
  throw({error,M:format_error(D)});
parse_error(E) ->
  %% Just in case we get some other error descriptor.
  S = io_lib:fwrite("error parsing expression: ~P.",[E,15]),
  throw({error,S}).

report(S, As) ->
  io:fwrite(S, As),
  io:nl().
