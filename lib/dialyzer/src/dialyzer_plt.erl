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
%%% File    : dialyzer_plt.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : Interface to display information in the persistent 
%%%               lookup tables.
%%%
%%% Created : 23 Jul 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_plt).

-include("dialyzer.hrl").	  %% file is automatically generated

-export([
	 check_init_plt/2,
	 copy/2,
	 contains_mfa/2,
	 delete/1,
	 delete_list/2,
	 delete_module/2,
	 from_file/2,
	 insert/2,
	 lookup/2,
	 lookup_module/2,
	 merge_and_write_file/2,
	 merge_plts/1,
	 new/1,
	 strip_non_member_mfas/2,
	 to_edoc/1,
	 to_edoc/4
	]).

-record(dialyzer_plt, {version, libs, md5, tab}).

new(Name) when is_atom(Name) ->
  ets:new(Name, [set, public]).

delete(Name) ->
  ets:delete(Name).

delete_module(Plt, Mod) ->
  ets:match_delete(Plt, {{Mod, '_', '_'}, '_', '_'}).

delete_list(Plt, [H|T]) ->
  ets:delete(Plt, H),
  delete_list(Plt, T);
delete_list(_Plt, []) ->
  ok.

copy(From, To) ->
  List = ets:match_object(From, '_'),
  ets:delete_all_objects(To),
  ets:insert_new(To, List).

insert(Plt, Object) ->
  ets:insert(Plt, Object).

lookup(Plt, MFA={M, F, A}) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
  lookup_1(Plt, MFA);
lookup(Plt, Label) when is_integer(Label) ->
  lookup_1(Plt, Label).

lookup_1(Plt, Obj) ->
  case ets:lookup(Plt, Obj) of
    [] -> none;
    [{Obj, Ret, Arg}] -> {value, {Ret, Arg}}
  end.

lookup_module(Plt, M) when is_atom(M) ->
  case ets:match_object(Plt, {{M, '_', '_'}, '_', '_'}) of
    [] -> none;
    [_|_] = List -> {value, List}
  end.
  
from_file(Name, FileName) ->
  case get_record_from_file(FileName) of
    {ok, Rec} ->
      case check_version(Rec) of
	{error, _Vsn} -> erlang:fault(old_plt);
	ok -> 
	  Plt = new(Name),
	  insert(Plt, Rec#dialyzer_plt.tab),
	  Plt
      end;
    {error, Reason} ->
      erlang:fault(Reason)
  end.

check_version(#dialyzer_plt{version=?VSN}) -> ok;
check_version(#dialyzer_plt{version=Vsn}) -> {error, Vsn}.

get_record_from_file(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      case catch binary_to_term(Bin) of
	Rec = #dialyzer_plt{} -> {ok, Rec};
	_ -> 
	  %% Lets see if this is a dets, i.e., an old type of plt.
	  case dets:is_dets_file(FileName) of
	    true -> {error, dets_plt};
	    false -> {error, not_valid}
	  end
      end;
    {error, enoent} ->
      {error, no_such_file};
    {error, _} -> 
      {error, read_error}
  end.

merge_and_write_file(PltList, File) ->
  NewPlt = merge_plts(PltList--[none]),
  to_file(NewPlt, File).

merge_plts([Plt]) ->
  Plt;
merge_plts([Plt1, Plt2|Left]) ->
  ets:foldl(fun(Obj, _) -> insert(Plt1, Obj) end, [], Plt2),
  ets:delete(Plt2),
  merge_plts([Plt1|Left]).

to_file(Plt, FileName) ->
  MD5 = case ets:lookup(Plt, md5) of
	  [] -> none;
	  [{md5, Val1}] -> Val1
	end,
  Libs = case ets:lookup(Plt, libs) of
	   [] -> none;
	   [{libs, Val2}] -> Val2
	 end,
  Record = #dialyzer_plt{version=?VSN, md5=MD5, 
			 libs=Libs, tab=ets:tab2list(Plt)},
  Bin = term_to_binary(Record),
  case file:write_file(FileName, Bin) of
    ok -> ok;
    {error, Reason} ->
      Msg = 
	io_lib:format("Could not write plt file ~s: ~w\n", [FileName, Reason]),
      erlang:fault(Msg)
  end.

check_init_plt(InputLibs, FileName) ->
  case get_record_from_file(FileName) of
    {ok, Rec = #dialyzer_plt{libs=FileLibs, md5=Md5}} ->
      Libs = case InputLibs =:= none of
	       true -> 
		 case FileLibs =:= none of
		   true -> ?DEFAULT_LIBS;
		   false -> FileLibs
		 end;
	       false -> InputLibs
	     end,
      case check_version(Rec) of
	ok -> 
	  case compute_md5(Libs) of
	    Md5 -> {ok, FileName};
	    NewMd5 -> {fail, NewMd5, Libs, FileName}
	  end;
	{error, Vsn} ->
	  case Md5 =:= none of
	    true ->
	      %% This is a user defined plt. No md5 check.
	      Msg = io_lib:format("    The plt ~s was built with Dialyzer ~s\n"
				  "    Please rebuild it using the current "
				  "version (~s)\n", [FileName, Vsn, ?VSN]),
	      {error, Msg};
	    false ->
	      NewMd5 = compute_md5(Libs),
	      {fail, NewMd5, Libs, FileName}
	  end
      end;
    {error, dets_plt} ->
      %% If this is a user-defined plt we need to fail with an error
      %% message. On the other hand, if it was built from some libs
      %% we can fail so that a new one is built.
      {ok, Dets} = dets:open_file(FileName, [{access, read}]),
      case dets:lookup(Dets, md5) of
	[{md5, _}] -> 
	  case InputLibs =:= none of
	    true ->
	      [{libs, PltLibs}] = dets:lookup(Dets, libs),
	      ok = dets:close(Dets),
	      {fail, compute_md5(PltLibs), PltLibs, FileName};
	    false ->
	      ok = dets:close(Dets),
	      {fail, compute_md5(InputLibs), InputLibs, FileName}
	  end;
	[] ->
	  %% This is a user-defined plt.
	  ok = dets:close(Dets),
	  Msg = io_lib:format("    The file ~s is an old type of plt\n"
			      "    Please rebuild it with the current "
			      "Dialyzer\n", [FileName]),
	  {error, Msg}
      end;
    {error, not_valid} ->
      Msg = io_lib:format("    The file ~s is not a plt file\n", [FileName]),
      {error, Msg};
    {error, no_such_file} ->
      case InputLibs =:= none of
	true -> {fail, compute_md5(?DEFAULT_LIBS), ?DEFAULT_LIBS, FileName};
	false -> {fail, compute_md5(InputLibs), InputLibs, FileName}
      end;
    {error, read_error} ->
      Msg = io_lib:format("    Could not read the file ~s\n", [FileName]),
      {error, Msg}
  end.

compute_md5(Libs) ->
  LibDirs = [code:lib_dir(L) || L <- Libs],
  Dirs = [filename:join(L, "ebin") || L <- LibDirs],
  case list_dirs(Dirs) of
    {error, List} ->
      Msg = lists:flatten(io_lib:format("Invalid libraries: ~w\n", [List])),
      erlang:fault(Msg);
    {ok, List} ->
      BeamFiles = [filename:join(Dir, X) 
		   || {Dir, X} <- List, filename:extension(X)==".beam"],
      Context = erlang:md5_init(),
      Fun = fun(X, Acc) ->
		compute_md5_from_file(X, Acc)
	    end,
      FinalContext = lists:foldl(Fun, Context, lists:sort(BeamFiles)),
      erlang:md5_final(FinalContext)
  end.

compute_md5_from_file(File, Acc) ->
  %% Avoid adding stuff like compile time etc.
  ChunkNames = sets:from_list(["Atom", "Code", "FunT", "StrT", "ImpT", "ExpT"]),
  {ok, _, Chunks} = beam_lib:all_chunks(File),
  Fun = fun({Name, Content}, FunAcc) ->
	    case sets:is_element(Name, ChunkNames) of
	      true -> erlang:md5_update(FunAcc, Content);
	      false -> FunAcc
	    end
	end,
  lists:foldl(Fun, Acc, Chunks).

list_dirs(Dirs) ->
  list_dirs(Dirs, [], []).

list_dirs([Dir|Left], Error, Acc) ->
  case file:list_dir(Dir) of
    {ok, List} -> list_dirs(Left, Error, [{Dir, List}|Acc]);
    {error, _} -> list_dirs(Left, [Dir|Error], Acc)
  end;
list_dirs([], [], Acc) -> 
  {ok, lists:sort(lists:flatten([[{Dir, X} || X <- List] || {Dir, List} <- Acc]))};
list_dirs([], Error, _Acc) ->
  {error, lists:flatten(Error)}.

contains_mfa(Plt, MFA) ->
  ets:lookup(Plt, MFA) =/= [].

to_edoc(PLT) ->
  to_edoc(PLT, '_', '_', '_').

to_edoc(PLT, M, F, A) when is_atom(M), is_atom(F) ->
  List = ets:match_object(PLT, {{M, F, A}, '_', '_'}),
  SortedList = lists:keysort(1, List),
  lists:flatten(expand_edoc(SortedList, [])).

expand_edoc([{{M, F, A}, ReturnType, ArgTypes}|Left], M) ->
  case erl_types:t_is_any(ArgTypes) of
    true ->
      [io_lib:format("%% @spec ~w(~s) -> ~s\n", 
		     [F, expand_args_any(A),
		      erl_types:t_to_string(ReturnType)])
       | expand_edoc(Left, M)];
    false ->
      [io_lib:format("%% @spec ~w(~s) -> ~s\n", 
		     [F, expand_args(ArgTypes), 
		      erl_types:t_to_string(ReturnType)])
       | expand_edoc(Left, M)]
  end;
expand_edoc(List = [{{M1, _F, _A}, _ReturnType, _ArgTypes}| _], _M) ->
  [io_lib:format("\n\n%% -------  Module: ~w -------\n\n", [M1]) | 
   expand_edoc(List, M1)];
expand_edoc([], _) ->
  [].

expand_args_any(0) ->
  [];
expand_args_any(1) ->
  ["_"];
expand_args_any(X) ->
  ["_,"|expand_args_any(X-1)].

expand_args([]) ->
  [];
expand_args([ArgType]) ->
  case erl_types:t_is_any(ArgType) of
    true -> ["_"];
    false -> [erl_types:t_to_string(ArgType)]
  end;
expand_args([ArgType|Left]) ->
  [case erl_types:t_is_any(ArgType) of
     true -> "_";
     false -> erl_types:t_to_string(ArgType)
   end ++
   ","|expand_args(Left)].

strip_non_member_mfas(Plt, Set) ->
  Fun = fun({{_, _, _} = MFA, _, _}, Acc) ->
	    case sets:is_element(MFA, Set) of
	      true -> Acc;
	      false -> [MFA|Acc]
	    end;
	   (_, Acc) ->
	    Acc
	end,
  ets:safe_fixtable(Plt, true),
  Delete = ets:foldl(Fun, [], Plt),
  ets:safe_fixtable(Plt, false),
  delete_list(Plt, Delete).
