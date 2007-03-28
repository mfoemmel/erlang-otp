%% -*- erlang-indent-level: 2 -*-
%%----------------------------------------------------------------------
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

-export([
	 check_init_plt/1,
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

%% Debug utilities
-export([pp_non_returning/0, pp_mod/1]).

-include("dialyzer.hrl").

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
  List = ets:tab2list(From),
  ets:delete_all_objects(To),
  ets:insert_new(To, List).

insert(Plt, Object) ->
  ets:insert(Plt, Object).

lookup(Plt, MFA={M, F, A}) when is_atom(M), is_atom(F),
				is_integer(A), 0 =< A, A =< 255 ->
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
      error(io_lib:format("Could not read plt file ~s: ~p\n", [Name, Reason]))
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
      Msg = io_lib:format("Could not write plt file ~s: ~w\n", 
			  [FileName, Reason]),
      throw({dialyzer_error, Msg})
  end.

check_init_plt(FileName) ->
  case get_record_from_file(FileName) of
    {ok, Rec = #dialyzer_plt{libs=FileLibs, md5=Md5}} ->
      case (FileLibs =:= ?DEFAULT_LIBS) or (Md5 =:= none) of
	false -> 
	  {fail, compute_md5(?DEFAULT_LIBS), none, ?DEFAULT_LIBS, FileName};
	true -> 
	  Libs = ?DEFAULT_LIBS,
	  case check_version(Rec) of
	    ok -> 
	      case compute_md5(Libs) of
		Md5 -> {ok, FileName};
		NewMd5 ->
		  DiffMd5 = 
		    if is_list(Md5) -> find_diffs_in_md5(NewMd5, Md5);
		       %% The Md5 was calculated in the old fashion.
		       true -> none
		    end,
		  {fail, NewMd5, DiffMd5, Libs, FileName}
	      end;
	    {error, Vsn} ->
	      case Md5 =:= none of
		true ->
		  %% This is a user defined plt. No md5 check.
		  Msg = io_lib:format("    The plt ~s was built with "
				      "Dialyzer ~s\n"
				      "    Please rebuild it using the current "
				      "version (~s)\n", [FileName, Vsn, ?VSN]),
		  {error, Msg};
		false ->
		  NewMd5 = compute_md5(Libs),
		  {fail, NewMd5, none, Libs, FileName}
	      end
	  end
      end;
    {error, dets_plt} ->
      %% If this is a user-defined plt we need to fail with an error
      %% message. On the other hand, if it was built from some libs
      %% we can fail so that a new one is built.
      {ok, Dets} = dets:open_file(FileName, [{access, read}]),
      case dets:lookup(Dets, md5) of
	[{md5, _}] -> 
	  ok = dets:close(Dets),
	  {fail, compute_md5(?DEFAULT_LIBS), none, ?DEFAULT_LIBS, FileName};
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
      {fail, compute_md5(?DEFAULT_LIBS), none, ?DEFAULT_LIBS, FileName};
    {error, read_error} ->
      Msg = io_lib:format("    Could not read the file ~s\n", [FileName]),
      {error, Msg}
  end.

compute_md5(Libs) ->
  LibDirs = [code:lib_dir(L) || L <- Libs],
  Dirs = [filename:join(L, "ebin") || L <- LibDirs],
  case list_dirs(Dirs) of
    {error, List} ->
      error(io_lib:format("Invalid libraries: ~w\n", [List]));
    {ok, List} ->
      BeamFiles = [filename:join(Dir, X) 
		   || {Dir, X} <- List, filename:extension(X)==".beam"],
      [compute_md5_from_file(F) || F <- lists:sort(BeamFiles)]
  end.

compute_md5_from_file(File) ->
  case beam_lib:md5(File) of
    {ok, {_Module, _Chunks}=Ret} ->
      Ret;
    {error, beam_lib, Reason} ->
      throw({dialyzer_error, 
	     io_lib:format("Could not compute md5 for file: ~s\nReason: ~p\n", 
			   [File, Reason])})
  end.

find_diffs_in_md5(NewMd5, OldMd5) ->
  find_diffs_in_md5(NewMd5, OldMd5, []).

find_diffs_in_md5([{Mod, Md5}|Left1], [{Mod, Md5}|Left2], Acc) ->
  find_diffs_in_md5(Left1, Left2, Acc);
find_diffs_in_md5([{Mod, _}|Left1], [{Mod, _}|Left2], Acc) ->
  find_diffs_in_md5(Left1, Left2, [{diff, Mod}|Acc]);
find_diffs_in_md5([{Mod1, _}|Left1], L2 =[{Mod2, _}|_], Acc) when Mod1 < Mod2 ->
  find_diffs_in_md5(Left1, L2, [{new, Mod1}|Acc]);
find_diffs_in_md5(L1 =[{Mod1, _}|_], [{Mod2, _}|Left2], Acc) when Mod1 > Mod2 ->
  find_diffs_in_md5(L1, Left2, [{removed, Mod2}|Acc]);
find_diffs_in_md5([], [], Acc) ->
  Acc;
find_diffs_in_md5(L1, [], Acc) ->
  [{new, Mod} || {Mod, _} <- L1] ++ Acc;
find_diffs_in_md5([], L2, Acc) ->
  [{removed, Mod} || {Mod, _} <- L2] ++ Acc.

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
	   ({Label, _, _}, Acc) when is_integer(Label) -> [Label|Acc];
	   (_, Acc) -> Acc
	end,
  ets:safe_fixtable(Plt, true),
  Delete = ets:foldl(Fun, [], Plt),
  ets:safe_fixtable(Plt, false),
  delete_list(Plt, Delete).

error(Msg) ->
  throw({dialyzer_error, lists:flatten(Msg)}).

%%---------------------------------------------------------------------------
%% Debug utilities.

pp_non_returning() ->
  PltFile = filename:join([code:lib_dir(dialyzer), "plt", "dialyzer_init_plt"]),
  Plt = from_file(foo, PltFile),
  List = ets:tab2list(Plt),
  NonRet = [{MFA, erl_types:t_fun(Dom, Range)} || {MFA, Range, Dom} <- List,
						  erl_types:t_is_none(Range)],
  [io:format("~w:~w/~p :: ~s\n", [M, F, A, erl_types:t_to_string(Type)])
   || {{M,F,A}, Type} <- lists:sort(NonRet)],
  ets:delete(Plt),
  ok.

pp_mod(Mod) when is_atom(Mod) ->
  PltFile = filename:join([code:lib_dir(dialyzer), "plt", "dialyzer_init_plt"]),
  Plt = from_file(foo, PltFile),
  List = ets:match_object(Plt, {{Mod, '_', '_'}, '_', '_'}),
  [io:format("~w:~w/~p :: ~s\n", 
	     [M, F, A, erl_types:t_to_string(erl_types:t_fun(Args, Ret))])
   || {{M,F,A}, Ret, Args} <- lists:sort(List)],
  ets:delete(Plt),
  ok.
  
