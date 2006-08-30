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
	 lookup/4,
	 merge_and_write_file/2,
	 new/1,
	 strip_non_member_mfas/2,
	 to_edoc/1,
	 to_edoc/4
	]).

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

lookup(Plt, {M, F, A}) ->
  lookup(Plt, M, F, A).

lookup(Plt, M, F, A) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
  case ets:lookup(Plt, {M, F, A}) of
    [] -> none;
    [{_MFA, Ret, Arg}] -> {value, {Ret, Arg}}
  end.

from_file(Name, Dets) ->
  Plt = new(Name),
  {ok, D} = dets:open_file(Dets, [{access, read}]),
  true = ets:from_dets(Plt, D),
  ok = dets:close(D),
  Plt.

merge_and_write_file(PltList, File) ->
  NewPlt = merge_plts(PltList--[none]),
  to_file(NewPlt, File).

merge_plts([Plt]) ->
  Plt;
merge_plts([Plt1, Plt2|Left]) ->
  ets:foldl(fun(Obj, _) -> insert(Plt1, Obj) end, [], Plt2),
  ets:delete(Plt2),
  merge_plts([Plt1|Left]).
		

to_file(Plt, Dets) ->
  file:delete(Dets),
  MinSize = ets:info(Plt, size),
  {ok, Dets} = dets:open_file(Dets, [{min_no_slots, MinSize}]),
  ok = dets:from_ets(Dets, Plt),
  ok = dets:sync(Dets),
  ok = dets:close(Dets).

check_init_plt(Libs0, InitPlt) ->  
  case Libs0 =:= none of
    true ->
      case filelib:is_file(InitPlt) andalso dets:is_dets_file(InitPlt) of
	true ->
	  case dets:open_file(InitPlt, [{access, read}]) of
	    {ok, Dets1} ->
	      case dets:lookup(Dets1, libs) of
		[{libs, Libs}] -> ok;
		_ -> Libs = ?DEFAULT_LIBS
	      end,
	      ok = dets:close(Dets1);
	    {error, _} ->
	      Libs = ?DEFAULT_LIBS
	  end;
	false ->
	  Libs = ?DEFAULT_LIBS
      end;
    false ->
      Libs = Libs0
  end,
  MD5 = compute_md5(Libs),
  case filelib:is_file(InitPlt) andalso dets:is_dets_file(InitPlt) of
    true ->
      case dets:open_file(InitPlt, [{access, read}]) of
	{ok, Dets2} ->
	  Res =
	    case dets:lookup(Dets2, md5) of
	      [{md5, MD5}] -> {ok, InitPlt};
	      [{md5, _Other}] -> {fail, MD5, Libs, InitPlt};
	      [] ->
		%% This is a user-defined plt => No check
		{ok, InitPlt}
	    end,
	  ok = dets:close(Dets2),
	  Res;
	{error, _} ->
	  {fail, MD5, Libs, InitPlt}
      end;
    false -> {fail, MD5, Libs, InitPlt}
  end.      

compute_md5(Libs) ->
  LibDirs = [code:lib_dir(L) || L <- Libs],
  Dirs = [filename:join(L, "ebin") || L <- LibDirs],
  case list_dirs(Dirs) of
    {error, List} ->
      erlang:fault("Not valid libraries: ~w\n", [List]);
    {ok, List} ->
      BeamFiles = [filename:join(Dir, X) 
		   || {Dir, X} <- List, filename:extension(X)==".beam"],
      Context = erlang:md5_init(),
      Fun = fun(X, Acc) ->
		compute_md5_from_file(X, Acc)
	    end,
      FinalContext = lists:foldl(Fun, Context, BeamFiles),
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
