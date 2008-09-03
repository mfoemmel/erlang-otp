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

-spec start() -> no_return().

start() ->
  {Args, Analysis} = typer_options:process(),
  %% io:format("Args: ~p\n", [Args]),
  %% io:format("Analysis: ~p\n", [Analysis]),
  
  TrustedFiles = typer_preprocess:get_all_files(Args, trust),
  Analysis1 = Analysis#typer_analysis{t_files=TrustedFiles},
  Analysis2 = extract(Analysis1),  

  All_Files = typer_preprocess:get_all_files(Args, analysis),
  %% io:format("All_Files: ~p\n", [All_Files]),
  Analysis3 = Analysis2#typer_analysis{ana_files=All_Files},
  Analysis4 = typer_info:collect(Analysis3),
  %% io:format("Final: ~p\n", [Analysis4#typer_analysis.final_files]),
  
  TypeInfo = get_type_info(Analysis4),

  typer_annotator:annotate(TypeInfo),
  %% io:format("\nTyper analysis finished\n"),
  erlang:halt(0).

%%--------------------------------------------------------------------

-spec extract(#typer_analysis{}) -> #typer_analysis{}.

extract(Analysis) ->
  %% io:format("--- Extracting trusted typer_info... "),
  Ds = [{d,Name,Value} || {Name,Value} <- Analysis#typer_analysis.macros],
  Fun =
    fun(File, TmpPlt) ->
	%% We should include not the upper level, but one more level above
	%% E.g, for /home/tests/typer_ann/test.ann.erl, we should include
	%% /home/tests/ rather than /home/tests/typer_ann/
	Includes = [filename:dirname(filename:dirname(File))
		    |Analysis#typer_analysis.includes],
	Is = [{i,Dir} || Dir <- Includes],
	CompOpts = ?SRC_COMPILE_OPTS++Is++Ds,
	SpecInfo = 
	  case dialyzer_utils:get_abstract_code_from_src(File, CompOpts) of
	    {error, Reason} -> compile_error(Reason);
	    {ok, AbstractCode} -> 
	      case dialyzer_utils:get_record_and_type_info(AbstractCode) of
		{error, Reason} -> compile_error([Reason]);
		{ok, RecDict} -> 
		  case dialyzer_utils:get_spec_info(AbstractCode, RecDict) of
		    {error, Reason} -> compile_error([Reason]);
		    {ok, Ans} -> Ans
		  end
	      end
	  end,
	SpecList = [{MFA, Contract} 
		    || {MFA, {_FileLine, Contract}} <- dict:to_list(SpecInfo)],
	dialyzer_plt:insert_contract_list(TmpPlt, SpecList)
    end,
  TrustPlt = lists:foldl(Fun, Analysis#typer_analysis.trust_plt, 
			 Analysis#typer_analysis.t_files),
  Analysis#typer_analysis{trust_plt=TrustPlt}.

%%--------------------------------------------------------------------

-spec get_type_info(#typer_analysis{}) -> #typer_analysis{}.

get_type_info(Analysis) ->
  StrippedCallGraph = remove_external(Analysis#typer_analysis.callgraph, 
				      Analysis#typer_analysis.trust_plt),
  %% io:format("--- Analyzing callgraph... "),  
  try 
    NewPlt = 
      dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph, 
					      Analysis#typer_analysis.trust_plt,
					      Analysis#typer_analysis.code_server),
    Analysis#typer_analysis{callgraph=StrippedCallGraph, trust_plt=NewPlt}
  catch
    error:What -> 
      error(io_lib:format("Analysis failed with message: ~p", 
			  [{What, erlang:get_stacktrace()}]));
    throw:{dialyzer_succ_typing_error, Msg} ->
      error(io_lib:format("analysis failed with message: ~s", [Msg]))
  end.

-spec remove_external(#dialyzer_callgraph{}, #dialyzer_plt{}) -> #dialyzer_callgraph{}.

remove_external(CallGraph, PLT) ->
  {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
  StrippedCG = dialyzer_callgraph:finalize(StrippedCG0),
  case get_external(Ext, PLT) of
    [] -> ok; 
    Externals -> io:format(" Unknown functions: ~p\n", [lists:usort(Externals)])
  end,
  StrippedCG.

-spec get_external([{mfa(), mfa()}], #dialyzer_plt{}) -> [mfa()].

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

-spec error(string()) -> no_return().

error(Slogan) ->
  io:format("typer: ~s\n", [Slogan]),
  erlang:halt(1).

%%--------------------------------------------------------------------

-spec compile_error([string()]) -> no_return().

compile_error(Reason) ->
  JoinedString = lists:flatten([X ++ "\n" || X <- Reason]),
  Msg = io_lib:format("analysis failed with error report:\n~s",
		      [JoinedString]),
  error(Msg).

%%--------------------------------------------------------------------
