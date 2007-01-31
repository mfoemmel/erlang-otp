%% -*- erlang-indent-level: 2 -*-
%%%----------------------------------------------
%%% File    : typer_preprocess.erl
%%% Author  : Bingwen He <hebingwen@hotmail.com>
%%% Description : 
%%%----------------------------------------------

-module(typer_preprocess).

-export([get_all_files/2]).

-include("typer.hrl").

get_all_files(Args, analysis) ->
  case internal_get_all_files(Args#args.analyze,
			      Args#args.analyzed_dir_r,
			      fun test_erl_file/1) of
    [] -> typer:error("no file(s) to analyze");
    AllFiles -> AllFiles
  end;
get_all_files(Args, trust) -> 
  internal_get_all_files(Args#args.trust,[],
			 fun test_ann_erl_file/1).

test_erl_file(File) ->
  %% Get ".erl" files without ".ann.erl" files
  case filename:extension(File) of
    ".erl" -> %% Exclude files ending with ".ann.erl"
      case regexp:matches(File, "[\.](ann)[\.](erl)$") of
	{match, []} -> true;
	{match, _ } -> false
      end;
    _  -> false
  end.

test_ann_erl_file(File) ->
  %% Get ".ann.erl" files
  case regexp:matches(File, "[\.](ann)[\.](erl)$") of
    {match, []} -> false;
    {match, _ } -> true
  end.

internal_get_all_files(File_Dir, Dir_R, Fun) ->
  All_File_1 = process_file_and_dir(File_Dir, Fun),
  All_File_2 = process_dir_recursively(Dir_R, Fun),
  remove_dup(All_File_1 ++ All_File_2).

process_file_and_dir(File_Dir,TestFun) ->
  Fun = 
    fun (Elem, Acc) ->
	case filelib:is_regular(Elem) of
	  true  -> process_file(Elem,TestFun,Acc);
	  false -> check_dir(Elem,non_recursive,Acc,TestFun)
	end
    end,
  lists:foldl(Fun,[],File_Dir).

process_dir_recursively(Dirs,TestFun) ->
  Fun = fun (Dir, Acc) ->
	    check_dir(Dir,recursive,Acc,TestFun)
	end,
  lists:foldl(Fun, [], Dirs).

check_dir(Dir, Mode, Acc, Fun) ->
  case file:list_dir(Dir) of
    {ok, Files} ->
      {TmpDirs, TmpFiles} = split_dirs_and_files(Files,Dir),
      case Mode of
	non_recursive ->
	  FinalFiles = process_file_and_dir(TmpFiles,Fun),
	  Acc++FinalFiles;
	recursive ->
	  TmpAcc1 = process_file_and_dir(TmpFiles,Fun),
	  TmpAcc2 = process_dir_recursively(TmpDirs,Fun),
	  Acc++TmpAcc1++TmpAcc2
      end;
    {error, eacces} ->
      typer:error("no access permission to dir \""++Dir++"\"");
    {error, enoent} ->
      typer:error("\""++Dir++"\" is not a valid file or directory");
    {error, _Reason} -> 
      typer:error("error involving a use of file:list_dir/1")
  end.

%% Same order as the input list
process_file(File,TestFun,Acc) ->
  case TestFun(File) of
    true  -> Acc++[File];
    false -> Acc
  end.

%% Same order as the input list
split_dirs_and_files(Elems,Dir) ->
  Test_Fun = 
    fun (Elem, {DirAcc,FileAcc}) ->
	File = filename:join(Dir,Elem),
	case filelib:is_regular(File) of
	  false -> {[File|DirAcc], FileAcc}; 
	  true  -> {DirAcc, [File|FileAcc]}
	end
    end,
  {Dirs,Files} = lists:foldl(Test_Fun, {[],[]}, Elems),
  {lists:reverse(Dirs), lists:reverse(Files)}.  

%%-------------------------------------------------------------
%% utils
%%-------------------------------------------------------------

%% Same order as the input list
%% Remove duplicate filenames
remove_dup(Files) ->
  Test_Dup = fun (File, Acc) ->
		 case lists:member(File,Acc) of
		   true  -> Acc;
		   false -> [File|Acc]
		 end
	     end,
  Reversed_Elems = lists:foldl(Test_Dup, [], Files),
  lists:reverse(Reversed_Elems).
