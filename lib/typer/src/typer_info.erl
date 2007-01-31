%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% File        : typer_info.erl
%% Author      : Bingwen He <Bingwen.He@gmail.com>
%% Description : 
%%--------------------------------------------------------------------

-module(typer_info).

-export([collect/1]).

-record(tmpAcc,{file,
		module,
		funcAcc=[],
		incFuncAcc=[],
		dialyzerObj=[]}).

-include("typer.hrl").

collect(Analysis) ->
  try get_dialyzer_plt() of
      DialyzerPlt ->
      dialyzer_plt:merge_plts([Analysis#analysis.trust_plt, DialyzerPlt])
  catch
    throw:{dialyzer_error,_Reason} ->
      typer:error("No dialyzer_init_plt found under lib/dialyzer/plt/\n"++
                  "       Please check your Erlang/OTP installation");
    exit:Term ->
      io:format("Error: ~p\n", [Term])
  end,
  lists:foldl(fun collect_one_file_info/2, Analysis, Analysis#analysis.ana_files).

collect_one_file_info(File, Analysis) ->
  Ds = [{d,Name,Val} || {Name,Val} <- Analysis#analysis.macros],
  %% Current directory should also be included in "Includes".
  Includes = [filename:dirname(File)|Analysis#analysis.includes],
  Is = [{i,Dir} || Dir <- Includes],
  Options = ?SRC_COMP_OPTS ++Is ++Ds,
  case dialyzer_utils:get_abstract_code_from_src(File, Options) of
    {error, Reason} ->
      %% io:format("File=~p\n,Options=~p\n,Error=~p\n", [File,Options,Reason]),
      typer:compile_error(Reason);
    AbstractCode ->
      case dialyzer_utils:get_core_from_abstract_code(AbstractCode) of
	error -> typer:compile_error("Could not get core erlang for "++File);
	Core ->
	  case dialyzer_utils:get_record_info(AbstractCode) of
	    {error, Reason} -> typer:compile_error(Reason);
	    {ok, Records} -> analyze_core_tree(Core, Records, Analysis, File)
	  end
      end
  end.

analyze_core_tree(Core, Records, Analysis, File) ->
  Module = list_to_atom(filename:basename(File, ".erl")),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#analysis.code_server,
  NextLabel = dialyzer_codeserver:next_core_label(CS1),
  {Tree,NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert([{Module, Tree}], core, CS1),
  CS3 = dialyzer_codeserver:update_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_records(Module, Records, CS3),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  TmpCG = Analysis#analysis.callgraph,
  CG = dialyzer_callgraph:scan_core_tree(Tree, TmpCG),

  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file=File,module=Module}, All_Defs),
  Exported_FuncMap = typer_map:insert({File, Ex_Funcs},
				      Analysis#analysis.ex_func),
  %% NOTE: we *MUST* sort here all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = typer_map:insert({File,Sorted_Functions},
			     Analysis#analysis.func),
  %% NOTE: However so far we do *NOT* need to sort functions
  %% which are imported from included files.
  IncFuncMap = typer_map:insert({File, Acc#tmpAcc.incFuncAcc}, 
				Analysis#analysis.inc_func),
  
  Final_Files = Analysis#analysis.final_files++[{File, Module}],
  RecordMap = typer_map:insert({File,Records}, Analysis#analysis.record),
  Analysis#analysis{final_files=Final_Files,
		    callgraph=CG,
		    code_server=CS4,
		    ex_func=Exported_FuncMap,
		    inc_func=IncFuncMap,
		    record=RecordMap,
		    func=FuncMap}.

analyze_one_function({Var,FunBody}, Acc) ->
  F = cerl:fname_id(Var),
  A = cerl:fname_arity(Var),
  TmpDialyzerObj = {{Acc#tmpAcc.module,F,A},{Var,FunBody}},
  NewDialyzerObj = Acc#tmpAcc.dialyzerObj++[TmpDialyzerObj],  
  [_,LineNo,{file,FileName}] = cerl:get_ann(FunBody),
  %% Note!: If you run TypEr on files that are right at 
  %%        the current level of directory, you will have 
  %%        'FileName' like './FileName', so there will be
  %%        no match, that is why we have 'AlterName'
  case FileName of
    "./"++AlterName -> ok;
    _ -> AlterName = FileName
  end,
  FuncInfo = {LineNo,F,A},
  OrginalName = Acc#tmpAcc.file,
  case (FileName =:= OrginalName) orelse (AlterName =:= OrginalName) of
    true -> %% Coming from original file
      %% io:format("Added function ~p\n",[{LineNo, F,A}]),
      FuncAcc    = Acc#tmpAcc.funcAcc++[FuncInfo], 
      IncFuncAcc = Acc#tmpAcc.incFuncAcc;
    false ->
      %% Coming from other sourses, including:
      %%     -- .yrl (yecc-generated file)
      %%     -- yeccpre.hrl (yecc-generated file)
      %%     -- other cases
      FuncAcc    = Acc#tmpAcc.funcAcc,
      IncFuncAcc = Acc#tmpAcc.incFuncAcc++[{FileName,FuncInfo}]
  end,
  Acc#tmpAcc{funcAcc=FuncAcc,
	     incFuncAcc=IncFuncAcc,
	     dialyzerObj=NewDialyzerObj}.

get_dialyzer_plt() ->
  DialyzerDir = code:lib_dir(dialyzer),
  Dialyzer_Init_Plt = filename:join([DialyzerDir,"plt","dialyzer_init_plt"]),
  dialyzer_plt:from_file(typer_plt, Dialyzer_Init_Plt).
