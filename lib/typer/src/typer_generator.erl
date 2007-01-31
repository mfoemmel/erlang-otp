%% -*- erlang-indent-level: 2 -*-
%%%-----------------------------------------------------------
%%% File    : typer_generator.erl
%%% Author  : He, Bingwen <hebingwen@hotmail.com>
%%% Description : 
%%    If file 'FILENAME' has been analyzed, then the output of
%%    command "diff -B FILENAME.erl typer_ann/FILENAME.ann.erl"
%%    should be exactly what TypEr has added, namely type info.
%%%
%%%-----------------------------------------------------------

-module(typer_generator).
-export([generate_result/1]).

-include("typer.hrl").
-define(TYPER_ANN_DIR,typer_ann).
-record(info,{recMap = typer_map:new(),
	      funcs = [],
	      typeMap}).
-record(inc, {map = typer_map:new(),
	      filter = []}).
	    

generate_result(Analysis) ->
  Mode = Analysis#analysis.mode,
  case is_show_only(Analysis,Mode) of
    job_done      -> ok;
    generate_file -> generate_file(Analysis,Mode)
  end.

is_show_only(Analysis,Mode) ->
  Flag = case typer_options:lookup(show_exported,Mode) of
	   true  -> show_ex;
	   false ->
	     case typer_options:lookup(show,Mode) of
	       true  -> show;
	       false -> none
	     end
	 end,
  case Flag of
    none -> generate_file;
    _ ->
      Fun=fun({File,Module}) -> 
	      Info = get_final_info(File,Module,Analysis,Flag),
	      show_type_info_only(File,Info)
	  end,
      lists:foreach(Fun, Analysis#analysis.final_files),
      job_done
  end.

generate_file(Analysis,Mode) ->
  case typer_options:lookup(annotate_inc_files,Mode) of
    false ->
      [] = Mode,
      Fun = fun({File,Module}) ->
		Info = get_final_info(File,Module,Analysis,func),
		write_typed_file(File, Info)
	    end,
      lists:foreach(Fun, Analysis#analysis.final_files);
    true -> 
      IncInfo = write_and_collect_inc_info(Analysis),
      write_inc_files(IncInfo)
  end.

write_and_collect_inc_info(Analysis)->
  Fun = fun({File,Module},Inc) ->
	    Info = get_final_info(File,Module,Analysis,func),
	    write_typed_file(File, Info),
	    IncFuns = get_function(File,Analysis,inc_func),
	    collect_imported_funcs(IncFuns,Info#info.typeMap,Inc)
	end,
  NewInc = lists:foldl(Fun,#inc{},Analysis#analysis.final_files),
  clean_inc(NewInc).

write_inc_files(Inc) ->
  Fun =
    fun (File) ->
	Val = typer_map:lookup(File,Inc#inc.map),
	%% Val is function with its type info
	%% in form [{{Line,F,A},Type}]
	Functions = [Key || {Key,_} <- Val],
	Val1 = [{{F,A},Type} || {{_Line,F,A},Type} <- Val],
	Info = #info{typeMap = typer_map:from_list(Val1),
		     recMap = typer_map:new(),
		     %% Note we need to sort functions here!
		     funcs = lists:keysort(1,Functions)},
	%% io:format("TypeMap ~p\n",[Info#info.typeMap]),
	%% io:format("Funcs ~p\n",[Info#info.funcs]),
	%% io:format("RecMap ~p\n",[Info#info.recMap]),
	write_typed_file(File,Info)
    end,
  lists:foreach(Fun, dict:fetch_keys(Inc#inc.map)).

get_final_info(File,Module,Analysis,Mode) ->
  RecMap = get_recMap(File,Analysis),
  TypeMap = get_typeMap(Module,Analysis),
  Functions = get_function(File,Analysis,Mode),
  #info{recMap=RecMap, funcs=Functions, typeMap=TypeMap}.

collect_imported_funcs(Funcs,TypeMap,TmpInc) ->
  %% Coming from other sourses, including:
  %% FIXME: How to deal with yecc-generated file????
  %%     --.yrl (yecc-generated file)???
  %%     -- yeccpre.hrl (yecc-generated file)???
  %%     -- other cases
  Fun = fun({File,_}=Obj,Inc) ->
	    case is_yecc_file(File,Inc) of
	      {yecc_generated,NewInc} -> NewInc;
	      {not_yecc,NewInc} ->
		check_imported_funcs(Obj,NewInc,TypeMap)
	    end
	end,
  lists:foldl(Fun, TmpInc, Funcs).

is_yecc_file(File, Inc) ->
  case lists:member(File,Inc#inc.filter) of
    true -> {yecc_generated,Inc};
    false ->
      case filename:extension(File) of
	".yrl" -> 
	  Rootname = filename:rootname(File,".yrl"),
	  Obj = lists:concat([Rootname,".erl"]),
	  case lists:member(Obj,Inc#inc.filter) of
	    true -> {yecc_generated,Inc};
	    false ->
	      NewFilter = [Obj|Inc#inc.filter],
	      NewInc = Inc#inc{filter=NewFilter},
	      {yecc_generated,NewInc}
	  end;
	_ ->
	  case filename:basename(File) of
	    "yeccpre.hrl" -> {yecc_generated,Inc};
	    _ -> {not_yecc,Inc}
	  end
      end
  end.

check_imported_funcs({File,{Line,F,A}},Inc,TypeMap) ->
  IncMap = Inc#inc.map,
  Type = get_type_info({F,A},TypeMap),
  case typer_map:lookup(File,IncMap) of
    none -> %% File is not added. Add it
      Obj = {File,[{{F,A},{Line,Type}}]},
      NewMap = typer_map:insert(Obj,IncMap),
      Inc#inc{map=NewMap};
    Val -> %% File is already in. Check.
      case lists:keysearch({F,A},1,Val) of
	false -> 
	  %% Function is not in. Good. Add.
	  Obj = {File,Val++[{{F,A},{Line,Type}}]},
	  NewMap = typer_map:insert(Obj,IncMap),
	  Inc#inc{map=NewMap};
	{_,Type} -> 
	  %% Function is in and with same type
	  Inc;
	{_,_} ->
	  %% Function is in but with diff type
	  inc_warning({F,A},File),
	  Elem = lists:keydelete({F,A},1,Val),
	  NewMap = case Elem of
		     [] -> 
		       typer_map:remove(File,IncMap);
		     _  ->
		       Obj = {File,Elem},
		       typer_map:insert(Obj,IncMap)
		   end,
	  Inc#inc{map=NewMap}
      end
  end.

inc_warning({F,A},File) ->	    
  io:format("      ***Warning: Skip function ~p/~p ",[F,A]),
  io:format("in file ~p because of inconsistent type\n",[File]).

clean_inc(Inc) ->
  Inc1 = remove_yecc_generated_file(Inc),
  normalize_obj(Inc1).

remove_yecc_generated_file(TmpInc) ->
  Fun = fun(Key,Inc) ->
	    NewMap = typer_map:remove(Key,Inc#inc.map),
	    Inc#inc{map=NewMap}
	end,
  lists:foldl(Fun,TmpInc,TmpInc#inc.filter).
  
normalize_obj(TmpInc) ->
  Fun = fun(Key,Val,Inc) ->
	    NewVal = [{{Line,F,A},Type} || {{F,A},{Line,Type}} <- Val],
	    typer_map:insert({Key,NewVal},Inc)
	end,
  NewMap = typer_map:fold(Fun, typer_map:new(), TmpInc#inc.map),
  TmpInc#inc{map=NewMap}.
  
get_recMap(File, Analysis) ->
  typer_map:lookup(File, Analysis#analysis.record).

get_typeMap(Module, Analysis) ->
  TypeInfoPlt = Analysis#analysis.trust_plt,
  TypeInfo = case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
	       none -> [];
	       {value, List} -> List
	     end,
  TypeInfoList = [{{F,A},{Range,Arg}} ||{{_M,F,A},Range,Arg} <- TypeInfo],
  typer_map:from_list(TypeInfoList).

get_function(File,Analysis,show_ex) ->
  Ex_Funcs = typer_map:lookup(File,Analysis#analysis.ex_func),
  remove_useless_func(Ex_Funcs);
get_function(File,Analysis,show) ->
  Funcs = typer_map:lookup(File,Analysis#analysis.func),
  Inc_Funcs = typer_map:lookup(File,Analysis#analysis.inc_func),
  remove_useless_func(Funcs)++normalize_incFuncs(Inc_Funcs);
get_function(File,Analysis,func) ->
  Funcs = typer_map:lookup(File,Analysis#analysis.func),
  remove_useless_func(Funcs);
get_function(File,Analysis,inc_func) ->
  typer_map:lookup(File,Analysis#analysis.inc_func).

normalize_incFuncs(Funcs) ->
  [FuncInfo || {_FileName,FuncInfo} <- Funcs].

remove_useless_func(Funcs) ->
  Fun = fun 
	  ({_,module_info,0}) -> false;
	  ({_,module_info,1}) -> false;
	  ({_,_,_}) -> true
	end,
  lists:filter(Fun,Funcs).

write_typed_file(File, Info) ->
  io:format("      Processing file: ~p\n",[File]),
  Dir = filename:dirname(File),
  RootName = filename:basename(filename:rootname(File)),
  Ext = filename:extension(File),
  TyperAnnDir = filename:join(Dir, ?TYPER_ANN_DIR),
  TmpNewFilename = lists:concat([RootName,".ann",Ext]),
  NewFileName = filename:join(TyperAnnDir,TmpNewFilename),
  case file:make_dir(TyperAnnDir) of
    {error,Reason} ->
      case Reason of
	eexist -> %% TypEr dir exists,remove old typer files
	  file:delete(NewFileName),
	  write_typed_file(File, Info, NewFileName);
	enospc -> io:format("  No enough space in ~p\n",[Dir]);
	eacces -> io:format("  No write permission in ~p\n",[Dir]);
	_ ->
	  io:format("Unknown error when writing ~p\n",[Dir]),
	  halt()
      end;
    ok -> %% Typer dir does NOT exist
      write_typed_file(File, Info, NewFileName)
  end.

write_typed_file(File, Info, NewFileName) ->
  {ok,Binary} = file:read_file(File),
  List = binary_to_list(Binary),
  write_typed_file(List, NewFileName, Info, 1, []),
  io:format("             Saved as: ~p\n", [NewFileName]).

write_typed_file(Rest, File, #info{funcs=[]}, _LNo, _Acc) ->
  file:write_file(File, list_to_binary(Rest), [append]), 
  {ok,done};
write_typed_file([First|RestCh], File, Info, LineNo, Acc) ->
  [{Line,F,A}|RestFuncs] = Info#info.funcs,
  case Line of 
    1 -> %% This will happen only for inc files
      raw_write(F,A,Info,File,[]),
      NewInfo = Info#info{funcs=RestFuncs},
      NewAcc = [],
      write_typed_file([First|RestCh],File,NewInfo,Line,NewAcc);
    _ ->
      case First of
	10 ->
	  NewLineNo = LineNo + 1,
	  case NewLineNo of
	    Line ->
	      raw_write(F,A,Info,File,[First|Acc]),
	      NewInfo = Info#info{funcs=RestFuncs},
	      NewAcc = [];
	    _ ->
	      NewInfo = Info,
	      NewAcc = [First|Acc]
	  end,
	  write_typed_file(RestCh,File,NewInfo,NewLineNo,NewAcc);
	_ ->
	  write_typed_file(RestCh,File,Info,LineNo,[First|Acc])
      end
  end.

raw_write(F,A,Info,File,Content) ->  
  TypeInfo = get_type_string(F,A,Info),
  ContentList = lists:reverse(Content)++TypeInfo++"\n",
  ContentBin = list_to_binary(ContentList),
  file:write_file(File,ContentBin,[append]).

get_type_string(F,A,Info)->
  {RetType,ArgType} = get_type_info({F,A},Info#info.typeMap),
  Type = erl_types:t_fun(ArgType,RetType),
  Prefix = lists:concat(["%% @typer_spec ",F,"/",A," :: "]),
  TypeStr = erl_types:t_to_string(Type,Info#info.recMap),
  lists:concat([Prefix, TypeStr]).

show_type_info_only(File, Info) ->
  io:format("\n%% File: ~p\n%% ", [File]),
  OutputString = lists:concat(["~.",length(File)+8,"c~n"]),
  io:fwrite(OutputString,[$-]),
  Fun = 
    fun ({_LineNo,F,A}) ->
	TypeInfo = get_type_string(F,A,Info),
	io:format("~s\n", [TypeInfo])
    end,
  lists:foreach(Fun, Info#info.funcs).

get_type_info(Func, TypeMap) ->
  case typer_map:lookup(Func, TypeMap) of
    none ->
      %% Note: Typeinfo of any function should exist in
      %% the result offered by Dialyzer, otherwise there 
      %% *MUST* be something wrong with the analysis
      io:format("No type info for function!!!: ~p\n", [Func]),
      halt();
    {RetType, ArgType} -> {RetType, ArgType} 
  end.
