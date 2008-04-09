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
%% Copyright 2006, 2007 Tobias Lindahl and Kostis Sagonas
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
	 contains_mfa/2,
	 contains_module/2,
	 delete_contract_list/2,
	 delete_list/2,
	 delete_module/2,
	 from_file/1,
	 get_mod_deps/1,
	 %% insert/3,
	 insert_list/2,
	 insert_contract_list/2,
	 lookup/2,
	 lookup_contract/2,
	 lookup_module/2,
	 merge_plts/1,
	 new/0,
	 plt_and_info_from_file/1,
	 to_edoc/1,
	 to_edoc/4,
	 to_file/4
	]).

%% Debug utilities
-export([pp_non_returning/0, pp_mod/1]).

-include("dialyzer.hrl").

-record(dialyzer_file_plt, {version=[]            :: string(), 
			    libs=[]               :: [atom()], 
			    md5=[]                :: md5(),
			    info=dict:new()       :: dict(),
			    contracts=dict:new()  :: dict(),
			    mod_deps              :: dict(),
			    implementation_md5=[] :: [{atom(), _}]
			   }).

-spec(new/0 :: () -> #dialyzer_plt{}).

new() ->
  #dialyzer_plt{info=table_new(), contracts=table_new()}.

-spec(delete_module/2 :: (#dialyzer_plt{}, atom()) -> #dialyzer_plt{}).

delete_module(#dialyzer_plt{info=Info, contracts=Contracts}, Mod) ->
  #dialyzer_plt{info=table_delete_module(Info, Mod),
		contracts=table_delete_module(Contracts, Mod)}.

-spec(delete_list/2 :: (#dialyzer_plt{}, [_]) -> #dialyzer_plt{}).

delete_list(#dialyzer_plt{info=Info, contracts=Contracts}, List) ->
  #dialyzer_plt{info=table_delete_list(Info, List),
		contracts=table_delete_list(Contracts, List)}.

-spec(insert_contract_list/2 :: 
      (#dialyzer_plt{}, [{mfa(), #contract{}}]) -> #dialyzer_plt{}).

insert_contract_list(Plt = #dialyzer_plt{contracts=Contracts}, List) ->
  Plt#dialyzer_plt{contracts=table_insert_list(Contracts, List)}.

-spec(lookup_contract/2 :: 
      (#dialyzer_plt{}, mfa()) -> 'none' | {'value', #contract{}}).

lookup_contract(#dialyzer_plt{contracts=Contracts}, 
		MFA={M, F, A}) when is_atom(M), is_atom(F), is_integer(A), 
				    0 =< A, A =< 255 ->
  table_lookup(Contracts, MFA).

-spec(delete_contract_list/2 :: (#dialyzer_plt{}, [mfa()]) -> #dialyzer_plt{}).

delete_contract_list(Plt = #dialyzer_plt{contracts=Contracts}, List) ->
  Plt#dialyzer_plt{contracts=table_delete_list(Contracts, List)}.


%% -spec(insert/3 :: (#dialyzer_plt{}, mfa() | integer(), {_, _}) ->
%% 	 #dialyzer_plt{}).
%% 
%% insert(Plt = #dialyzer_plt{info=Info}, Id, Types) ->
%%   Plt#dialyzer_plt{info=table_insert(Info, Id, Types)}.

-spec(insert_list/2 :: (#dialyzer_plt{}, [{mfa() | integer(), {_, _}}]) ->
	 #dialyzer_plt{}).

insert_list(Plt = #dialyzer_plt{info=Info}, List) ->
  Plt#dialyzer_plt{info=table_insert_list(Info, List)}.

-spec(lookup/2 :: (#dialyzer_plt{}, integer() | mfa()) -> 
	 'none' | {'value', {_, _}}).

lookup(#dialyzer_plt{info=Info}, MFA={M, F, A}) when is_atom(M), 
						     is_atom(F),
						     is_integer(A), 
						     0 =< A, A =< 255 ->
  table_lookup(Info, MFA);
lookup(#dialyzer_plt{info=Info}, Label) when is_integer(Label) ->
  table_lookup(Info, Label).

-spec(lookup_module/2 :: 
      (#dialyzer_plt{}, atom()) -> 'none' | {'value', [{_, _}]}).

lookup_module(#dialyzer_plt{info=Info}, M) when is_atom(M) ->
  table_lookup_module(Info, M).

-spec(contains_module/2 :: (#dialyzer_plt{}, atom()) -> bool()).

contains_module(#dialyzer_plt{info=Info, contracts=Cs}, M) when is_atom(M) ->
  table_contains_module(Info, M) orelse table_contains_module(Cs, M).

-spec(contains_mfa/2 :: (#dialyzer_plt{}, mfa()) -> bool()).

contains_mfa(#dialyzer_plt{info=Info, contracts=Contracts}, MFA) ->
  (table_lookup(Info, MFA) =/= none) 
    orelse (table_lookup(Contracts, MFA) =/= none).

-spec(plt_and_info_from_file/1 :: (string()) -> {#dialyzer_plt{}, {_, _, _}}).
  
plt_and_info_from_file(FileName) ->
  from_file(FileName, true).

-spec(from_file/1 :: (string()) -> #dialyzer_plt{}).
	 
from_file(FileName) ->
  from_file(FileName, false).

from_file(FileName, ReturnInfo) ->
  case get_record_from_file(FileName) of
    {ok, Rec} ->
      case check_version(Rec) of
	error -> 
	  Msg = io_lib:format("Old PLT file ~s\n", [FileName]),
	  error(Msg);
	ok -> 
	  Plt = #dialyzer_plt{info = Rec#dialyzer_file_plt.info,
			      contracts = Rec#dialyzer_file_plt.contracts},
	  case ReturnInfo of
	    false -> Plt;
	    true ->
	      PltInfo = {Rec#dialyzer_file_plt.md5,
			 Rec#dialyzer_file_plt.libs,
			 {non_mergable, Rec#dialyzer_file_plt.mod_deps}},
	      {Plt, PltInfo}
	  end
      end;
    {error, Reason} ->
      error(io_lib:format("Could not read PLT file ~s: ~p\n", 
			  [FileName, Reason]))
  end.

check_version(#dialyzer_file_plt{version=?VSN, implementation_md5=ImplMd5}) ->
  case compute_implementation_md5() =:= ImplMd5 of
    true -> ok;
    false -> error
  end;
check_version(#dialyzer_file_plt{}) -> error.

-spec(get_mod_deps/1 :: (string()) -> tuple()).

get_mod_deps(FileName) ->
  {ok, Rec} = get_record_from_file(FileName),
  Rec#dialyzer_file_plt.mod_deps.

get_record_from_file(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      case catch binary_to_term(Bin) of
	Rec = #dialyzer_file_plt{} -> {ok, Rec};
	_ -> {error, not_valid}
      end;
    {error, enoent} ->
      {error, no_such_file};
    {error, _} -> 
      {error, read_error}
  end.

-spec(merge_plts/1 :: ([#dialyzer_plt{}]) -> #dialyzer_plt{}).

merge_plts(List) ->
  InfoList = lists:map(fun(#dialyzer_plt{info=Info}) -> Info end, List),
  ContractsList = lists:map(fun(#dialyzer_plt{contracts=Contracts}) -> 
				Contracts 
			      end, List),
  #dialyzer_plt{info=table_merge(InfoList),
		contracts=table_merge(ContractsList)}.

-spec(to_file/4 :: 
      (string(), #dialyzer_plt{},dict(),
       {md5(), [atom()], {'mergable' | 'non_mergable', dict()}}) -> 'ok').

to_file(FileName, #dialyzer_plt{info=Info, contracts=Contracts}, 
	ModDeps, {MD5, Libs, OldModDeps}) ->
  NewModDeps =
    case OldModDeps of
      {mergable, OMDs} -> 
	dict:merge(fun(_Key, _OldVal, NewVal) -> NewVal end, 
		   OMDs, ModDeps);
      {non_mergable, OMDs} ->
	OMDs
    end,
  ImplMd5 = compute_implementation_md5(),
  Record = #dialyzer_file_plt{version=?VSN, 
			      md5=MD5, 
			      libs=Libs, 
			      info=Info,
			      contracts=Contracts,
			      mod_deps=NewModDeps,
			      implementation_md5=ImplMd5},
  Bin = term_to_binary(Record, [compressed]),
  case file:write_file(FileName, Bin) of
    ok -> ok;
    {error, Reason} ->
      Msg = io_lib:format("Could not write PLT file ~s: ~w\n", 
			  [FileName, Reason]),
      throw({dialyzer_error, Msg})
  end.

-type(md5_diff() :: [{'diff',atom()} | {'new',atom()} | {'removed',atom()}]).

-spec(check_init_plt/1 :: 
      (string()) -> {'error' | 'ok', string()} 
		  | {'fail', md5(), 'none' | md5_diff(), [atom()], string()}).

check_init_plt(FileName) ->
  case get_record_from_file(FileName) of
    {ok, Rec = #dialyzer_file_plt{libs=FileLibs, md5=Md5}} ->
      case FileLibs =:= ?DEFAULT_LIBS of
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
	    error ->
	      NewMd5 = compute_md5(Libs),
	      {fail, NewMd5, none, Libs, FileName}
	  end
      end;
    {error, not_valid} ->
      Msg0 = io_lib:format("    The file ~s is not a PLT file\n", [FileName]),
      Msg = lists:flatten(Msg0),
      {error, Msg};
    {error, no_such_file} ->
      {fail, compute_md5(?DEFAULT_LIBS), none, ?DEFAULT_LIBS, FileName};
    {error, read_error} ->
      Msg0 = io_lib:format("    Could not read the file ~s\n", [FileName]),
      Msg = lists:flatten(Msg0),
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
  case dialyzer_utils:get_abstract_code_from_beam(File) of
    error ->
      throw({dialyzer_error, 
	     io_lib:format("Could not compute md5 for file: ~s\n", 
			   [File])});
    {ok, Abs} ->
      ModName = list_to_atom(filename:basename(File, ".beam")),
      Md5 = erlang:md5(term_to_binary(Abs)),
      {ModName, Md5}
  end.

compute_implementation_md5() ->
  Dir = code:lib_dir(hipe),
  Files1 = ["erl_types.beam", "erl_bif_types.beam"],
  Files2 = [filename:join([Dir, "ebin", F]) || F <- Files1],
  [compute_md5_from_file(F) || F <- Files2].

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

-spec(to_edoc/1 :: (#dialyzer_plt{}) -> string()).

to_edoc(#dialyzer_plt{info=Info}) ->
  %% TODO: Should print contracts as well.
  List = 
    lists:sort([{MFA, Val} || {MFA = {_,_,_}, Val} <- table_to_list(Info)]),
  lists:flatten(expand_edoc(List, [])).

-spec(to_edoc/4 :: (#dialyzer_plt{}, atom(), atom(), byte()) -> string()).

to_edoc(PLT, M, F, A) when is_atom(M), is_atom(F) ->
  {value, Val} = lookup(PLT, {M, F, A}),
  lists:flatten(expand_edoc([{{M, F, A}, Val}], [])).

expand_edoc([{{M, F, A}, {ReturnType, ArgTypes}}|Left], M) ->
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
expand_edoc(List = [{{M1, _F, _A}, {_ReturnType, _ArgTypes}}| _], _M) ->
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

error(Msg) ->
  throw({dialyzer_error, lists:flatten(Msg)}).

%%---------------------------------------------------------------------------
%% Ets table

table_new() ->
  dict:new().

table_to_list(Plt) ->
  dict:to_list(Plt).

table_delete_module(Plt, Mod) ->
  dict:filter(fun({M, _F, _A}, _Val) -> M =/= Mod;
		 (_, _) -> true
	      end, Plt).

table_delete_list(Plt, [H|T]) ->
  table_delete_list(dict:erase(H, Plt), T);
table_delete_list(Plt, []) ->
  Plt.

table_insert_list(Plt, [{Key, Val}|Left]) ->
  table_insert_list(table_insert(Plt, Key, Val), Left);
table_insert_list(Plt, []) ->
  Plt.

table_insert(Plt, Key, {Ret, Arg}) -> 
  dict:store(Key, {Ret, Arg}, Plt);
table_insert(Plt, Key, C = #contract{}) ->
  dict:store(Key, C, Plt).

table_lookup(Plt, Obj) ->
  case dict:find(Obj, Plt) of
    error -> none;
    {ok, Val} -> {value, Val}
  end.

table_lookup_module(Plt, Mod) ->
  List = dict:fold(fun(Key, Val, Acc) ->
		       case Key of
			 {Mod, _F, _A} -> [{Key, element(1, Val),
					    element(2, Val)}|Acc];
			 _ -> Acc
		       end
		   end, [], Plt),
  case List =:= [] of
    true -> none;
    false -> {value, List}
  end.

table_contains_module(Plt, Mod) ->
  dict:fold(fun({M, _F, _A}, _Val, _Acc) when M =:= Mod -> true;
	       (_, _, Acc) -> Acc
	    end, false, Plt).

table_merge([H|T]) ->
  table_merge(T, H).

table_merge([], Acc) ->
  Acc;
table_merge([Plt|Left], Acc) ->
  NewAcc = dict:merge(fun(_Key, Val, Val) -> Val end, Plt, Acc),
  table_merge(Left, NewAcc).

%%---------------------------------------------------------------------------
%% Debug utilities.

-spec(pp_non_returning/0 :: () -> 'ok').

pp_non_returning() ->
  PltFile = filename:join([code:lib_dir(dialyzer), "plt", "dialyzer_init_plt"]),
  Plt = from_file(PltFile),
  List = table_to_list(Plt#dialyzer_plt.info),
  Unit = [{MFA, erl_types:t_fun(Dom, Range)} || {MFA, Range, Dom} <- List,
						erl_types:t_is_unit(Range)],
  None = [{MFA, erl_types:t_fun(Dom, Range)} || {MFA, Range, Dom} <- List,
						erl_types:t_is_none(Range)],
  io:format("=========================================\n"),
  io:format("=                Loops                  =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M,F,A}, Type}) ->
		    io:format("~w:~w/~p :: ~s\n",
			      [M, F, A, erl_types:t_to_string(Type)])
		end, lists:sort(Unit)),
  io:format("\n\n"),
  io:format("=========================================\n"),
  io:format("=                Errors                 =\n"),
  io:format("=========================================\n\n"),
  lists:foreach(fun({{M,F,A}, Type}) ->
		    io:format("~w:~w/~p :: ~s\n",
			      [M, F, A, erl_types:t_to_string(Type)])
		end, lists:sort(None)),
  ok.

-spec(pp_mod/1 :: (atom()) -> 'ok').

pp_mod(Mod) when is_atom(Mod) ->
  PltFile = filename:join([code:lib_dir(dialyzer), "plt", "dialyzer_init_plt"]),
  Plt = from_file(PltFile),
  {value, List} = lookup_module(Plt, Mod),
  lists:foreach(fun({{M,F,A}, {Ret, Args}}) ->
		    T = erl_types:t_to_string(erl_types:t_fun(Args, Ret)),
		    io:format("~w:~w/~p :: ~s\n", [M, F, A, T])
		end, lists:sort(List)),
  ok.
