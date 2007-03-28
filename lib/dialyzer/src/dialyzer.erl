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
%% Copyright 2006, Tobias Lindahl and Kostis Sagonas
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
	 gui/1]).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------
%% Interfaces:
%%  - plain_cl/0 : to be used ONLY by the dialyzer C program.
%%  - run/1:       Erlang interface for a command line-like analysis
%%  - gui/0/1:     Erlang interface for the gui.
%%--------------------------------------------------------------------

plain_cl() ->
  case dialyzer_cl_parse:start() of
    {check_init, Opts} -> 
      cl_return(cl_check_init(Opts, true), Opts);
    {gui, Opts} ->
      case cl_check_init(Opts) of
	{error, _} = Error -> gui_return(Error, Opts);
	{ok, ?RET_NOTHING_SUSPICIOUS} ->
	  if Opts#options.quiet -> ok;
	     true  -> io:put_chars("  Proceeding with startup...\n")
	  end,
	  gui_return(internal_gui(Opts), Opts)
      end;
    {cl, Opts} -> 
      case cl_check_init(Opts) of
	{error, _} = Error -> cl_return(Error, Opts);
	{ok, ?RET_NOTHING_SUSPICIOUS} ->
	  if Opts#options.quiet -> ok;
	     true  -> io:put_chars("  Proceeding with analysis... ")
	  end,
	  cl_return(cl(Opts), Opts)
      end;
    {error, Msg} -> 
      cl_error(Msg)
  end.

cl_check_init(Opts) ->
  cl_check_init(Opts, false).

cl_check_init(Opts, Force) ->
  if Opts#options.quiet -> ok;
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

run(Opts) when length(Opts) > 0 ->
  try
    case dialyzer_options:build([{quiet, true}, {erlang_mode, true}|Opts]) of
      {error, Msg} -> throw({dialyzer_error, Msg});
      OptsRecord ->
	case cl_check_init(OptsRecord) of
	  {ok, ?RET_NOTHING_SUSPICIOUS} ->
	    case dialyzer_cl:start(OptsRecord) of
	      {?RET_DISCREPANCIES_FOUND, Warnings, []} -> {ok, Warnings};
	      {?RET_NOTHING_SUSPICIOUS, [], []}    -> {ok, []};
	      {?RET_INTERNAL_ERROR, Warnings, Errors} -> {error, Warnings, 
							  Errors}
	    end;
	  {error, ErrorMsg1} ->
	    throw({dialyzer_error, ErrorMsg1})
	end
    end
  catch
    throw:{dialyzer_error, ErrorMsg} -> 
      erlang:fault({dialyzer_error, lists:flatten(ErrorMsg)})
  end.

internal_gui(OptsRecord) ->
  F = fun() ->
	  dialyzer_gui:start(OptsRecord),
	  ?RET_NOTHING_SUSPICIOUS
      end,
  doit(F).

gui() ->
  gui([]).

gui(Opts) ->  
  try
    case dialyzer_options:build([{quiet, true}|Opts]) of
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
      erlang:fault({dialyzer_error, lists:flatten(ErrorMsg)})
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
  cl_return({error, Msg}, #options{}).

gui_return(R, Opts) ->
  cl_return(R, Opts#options{quiet=true}).

cl_return({ok, R = ?RET_NOTHING_SUSPICIOUS},  #options{quiet=true}) -> halt(R);
cl_return({ok, R = ?RET_DISCREPANCIES_FOUND}, #options{quiet=true}) -> halt(R);
cl_return({ok, R = ?RET_NOTHING_SUSPICIOUS},  #options{}) ->
  io:put_chars("done (passed successfully)\n"),
  halt(R);
cl_return({ok, R = ?RET_DISCREPANCIES_FOUND},  #options{output_file=Output}) ->
  io:put_chars("done (warnings were emitted)\n"),
  cl_check_log(Output),
  halt(R);
cl_return({ok, R = ?RET_INTERNAL_ERROR},  #options{output_file=Output}) ->
  Msg = "dialyzer: Internal problems were encountered in the analysis.",
  io:format("~s\n", [Msg]),
  cl_check_log(Output),
  halt(R);  
cl_return({error, Msg1}, #options{output_file=Output}) ->
  Msg2 = "dialyzer: Internal problems were encountered in the analysis.",
  io:format("\n~s\n~s\n", [Msg1, Msg2]),
  cl_check_log(Output),
  halt(?RET_INTERNAL_ERROR).

cl_check_log("") ->
  ok;
cl_check_log(Output) ->
  io:format("  Check output file `~s' for details\n", [Output]).
