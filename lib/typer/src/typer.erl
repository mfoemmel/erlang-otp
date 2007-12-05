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
%% Copyright 2006, Bingwen He, Tobias Lindahl, and Kostis Sagonas
%% 
%%     $Id$
%%

%%--------------------------------------------------------------------
%% File        : typer.erl
%% Author      : Bingwen He <Bingwen.He@gmail.com>
%% Description : The main driver of the TypEr application
%%--------------------------------------------------------------------

-module(typer).

-export([start/0]).
-export([error/1, compile_error/1]).	% for error reporting

-include("typer.hrl").

%%--------------------------------------------------------------------

-spec(start/0 :: () -> no_return()).

start() ->
  {Args, Analysis} = typer_options:process(),
  %% io:format("Args: ~p\n", [Args]),
  %% io:format("Analysis: ~p\n", [Analysis]),
  
  TrustedFiles = typer_preprocess:get_all_files(Args, trust),
  %% io:format("TrustedFiles: ~p\n",[TrustedFiles]),
  Analysis1 = Analysis#analysis{t_files=TrustedFiles},
  Analysis2 = extract(Analysis1),  

  All_Files = typer_preprocess:get_all_files(Args, analysis),
  %% io:format("All_Files: ~p\n", [All_Files]),
  Analysis3 = Analysis2#analysis{ana_files=All_Files},
  Analysis4 = typer_info:collect(Analysis3),
  %% io:format("Final: ~p\n", [Analysis4#analysis.final_files]),
  
  TypeInfo = get_type_info(Analysis4),

  typer_generator:generate_result(TypeInfo),
  %% io:format("\nTyper analysis finished\n"),
  erlang:halt(0).

%%--------------------------------------------------------------------

extract(Analysis) ->
  %% io:format("--- Extracting trusted typer_info... "),
  Ds = [{d,Name,Value} || {Name,Value} <- Analysis#analysis.macros],
  Fun =
    fun(File) ->
	%% We should include not the upper level, but one more level above
	%% E.g, for /home/tests/typer_ann/test.ann.erl, we should include
	%% /home/tests/ rather than /home/tests/typer_ann/
	Includes = [filename:dirname(filename:dirname(File))
		    |Analysis#analysis.includes],
	Is = [{i,Dir} || Dir <- Includes],
	CompOpts = ?SRC_COMP_OPTS++Is++Ds,
	Records = 
	  case dialyzer_utils:get_abstract_code_from_src(File, CompOpts) of
	    {error, Reason} -> compile_error(Reason);
	    AbstractCode -> 
	      case dialyzer_utils:get_record_and_type_info(AbstractCode) of
		{error, Reason} -> compile_error(Reason);
		{ok, Ans} -> Ans
	      end
	  end,
	ModuleString = filename:basename(File,".ann.erl"),
	Module = list_to_atom(ModuleString),
	Comments = erl_comment_scan:file(File),
	Plt = Analysis#analysis.trust_plt,
	scan_comments(Comments, Module, Plt, File, Records)
    end,
  lists:foreach(Fun, Analysis#analysis.t_files),
  %% io:format("done\n"),
  Analysis.

scan_comments([], _, _, _, _) -> ok;
scan_comments([{_,_,_,Comment}|RestComments], Module, Plt, File, Records) ->
  Fun =
    fun(Line, AccPlt) ->
	case Line of
	  "% @typer_spec " ++ F_A_TypeInfo ->
	    case erl_scan:string(F_A_TypeInfo++".") of
	      {ok,Tokens,_} ->
		%%io:format("Tokens : ~p\n",[Tokens]),
		case Tokens of %% remove '::'
		  [{atom,_,FName},{'/',_},{integer,_,Arity},_|TypeTokens] ->
		    case typer_parse:parse(TypeTokens) of
		      {ok,TypeForm} ->
			try
			  Type = erl_types:t_from_form(TypeForm, Records),
			  FuncObject = {{Module,FName,Arity},
					erl_types:t_fun_range(Type),
					erl_types:t_fun_args(Type)},
			  dialyzer_plt:insert(AccPlt, FuncObject)
			catch
			  throw:{error, What} ->
			    typer_info_error(What, File)
			end;
		      {error, Msg} -> 
			Msg1 = typer_parse:format_error(Msg),
			typer_info_error(Msg1, File)
		    end;
		  _ -> typer_info_error(F_A_TypeInfo, File)
		end;
	      _ -> typer_info_error(F_A_TypeInfo, File)
	    end;
	  _ -> AccPlt
	end
    end,
  NewPlt = lists:foldl(Fun, Plt, Comment),
  scan_comments(RestComments, Module, NewPlt, File, Records).

typer_info_error(String, File) ->
  io:format("\n          **Warning**: in \"~p\"\n      ", [File]),
  io:format("                 Invalid typer_info \"~s\"", [String]).

%%--------------------------------------------------------------------

get_type_info(Analysis) ->
  StrippedCallGraph = remove_external(Analysis#analysis.callgraph, 
				      Analysis#analysis.trust_plt),
  %% io:format("--- Analyzing callgraph... "),  
  try 
    NewPlt = 
      dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph, 
					      Analysis#analysis.trust_plt,
					      Analysis#analysis.code_server),
    Analysis#analysis{callgraph=StrippedCallGraph, trust_plt=NewPlt}
  catch
    error:What -> 
      error(io_lib:format("Analysis failed with message: ~p", 
			  [{What, erlang:get_stacktrace()}]));
    throw:{dialyzer_succ_typing_error, Msg} ->
      error(io_lib:format("analysis failed with message: ~s", [Msg]))
  end.

remove_external(CallGraph, PLT) ->
  {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
  StrippedCG = dialyzer_callgraph:finalize(StrippedCG0),
  case get_external(Ext, PLT) of
    [] -> ok; 
    Externals -> io:format(" Unknown functions: ~p\n", [lists:usort(Externals)])
  end,
  StrippedCG.

get_external(Exts, Plt) ->
  Fun = fun ({_From, To = {M, F, A}}, Acc) ->
	    case dialyzer_plt:contains_mfa(Plt, To) of
	      false ->
		case erl_bif_types:is_known(M, F, A) of
		  true -> Acc;
		  false -> [To|Acc]
		end;
	      true -> Acc
	    end
	end,
  lists:foldl(Fun, [], Exts).

%%--------------------------------------------------------------------

-spec(error/1 :: (string()) -> no_return()).

error(Slogan) ->
  io:format("typer: ~s\n", [Slogan]),
  erlang:halt(1).

compile_error([]) ->
  error("cannot compile some input file(s)");
compile_error([{_, _}|_] = Reason) ->
  Msg = io_lib:format("analysis failed with error report:\n~s",
		      [lists:flatten(format_compile_errors(Reason))]),
  error(Msg);
compile_error(Reason) when is_list(Reason) ->
  Msg = io_lib:format("analysis failed with error report:\n~s",
		      [Reason]),
  error(Msg).

format_compile_errors([{Mod, Errors}|Left]) ->
  FormatedError = 
    [io_lib:format("~s:~w: ~s\n", [Mod, Line, M:format_error(Desc)])
     || {Line, M, Desc} <- Errors],
  [lists:flatten(FormatedError) | format_compile_errors(Left)];
format_compile_errors([]) ->
  [].

%%--------------------------------------------------------------------
