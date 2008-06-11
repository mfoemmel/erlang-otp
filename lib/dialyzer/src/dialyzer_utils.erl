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
%% Copyright 2006-2008, Tobias Lindahl and Kostis Sagonas
%%
%% $Id$
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_utils.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created :  5 Dec 2006 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_utils).

-export([
	 format_sig/1,
	 format_sig/2,
	 get_abstract_code_from_beam/1,
	 get_abstract_code_from_src/1,
	 get_abstract_code_from_src/2,
	 get_core_from_abstract_code/1,
	 get_core_from_abstract_code/2,
	 get_core_from_src/1,
	 get_core_from_src/2,
	 get_record_and_type_info/1,
	 get_spec_info/2,
	 pp_hook/0
	]).

-include("dialyzer.hrl").

%% ============================================================================
%%
%%  Compilation utils
%%
%% ============================================================================

-type(abstract_code() :: [_]).

-spec(get_abstract_code_from_src/1 :: 
      (atom() | string()) -> {'ok', abstract_code()} | {'error', [string()]}).

get_abstract_code_from_src(File) ->
  get_abstract_code_from_src(File, ?SRC_COMPILE_OPTS).

-spec(get_abstract_code_from_src/2 ::
      (atom() | string(), [_]) ->
	 {'ok', abstract_code()} | {'error', [string()]}).

get_abstract_code_from_src(File, Opts) ->
  case compile:file(File, [to_pp, binary|Opts]) of
    error -> {error, []};
    {error, Errors, _} -> {error, format_errors(Errors)};
    {ok, _, AbstrCode} -> {ok, AbstrCode}
  end.

-spec(get_core_from_src/1 ::
      (string()) -> {'ok', core_records()} | {'error', string()}).

get_core_from_src(File) ->
  get_core_from_src(File, []).

-spec(get_core_from_src/2 ::
      (string(), [_]) -> {'ok', core_records()} | {'error', string()}).

get_core_from_src(File, Opts) ->
  case get_abstract_code_from_src(File, Opts) of
    {error, What} -> {error, What};
    {ok, AbstrCode} ->
      case get_core_from_abstract_code(AbstrCode, Opts) of
	error -> {error, "  Could not get abstract core"};
	{ok, Core} -> {ok, Core}
      end
  end.

-spec(get_abstract_code_from_beam/1 :: 
      (string()) -> 'error' | {'ok', abstract_code()}).

get_abstract_code_from_beam(File) ->
  case beam_lib:chunks(File, [abstract_code]) of
    {ok, {_, List}} ->
      case lists:keysearch(abstract_code, 1, List) of
	{value, {abstract_code, {raw_abstract_v1, Abstr}}} -> {ok, Abstr};
	_ -> error
      end;
    _ ->
      %% No or unsuitable abstract code.
      error
  end.

-spec(get_core_from_abstract_code/1 ::
      (abstract_code()) -> 'error' | {ok, core_records()}).

get_core_from_abstract_code(AbstrCode) ->
  get_core_from_abstract_code(AbstrCode, []).

-spec(get_core_from_abstract_code/2 ::
      (abstract_code(), [_]) -> 'error' | {ok, core_records()}).

get_core_from_abstract_code(AbstrCode, Opts) ->
  %% We do not want the parse_transorms left since we have already
  %% performed them. In some cases we end up in trouble when
  %% performing them again.
  AbstrCode1 = cleanup_parse_transforms(AbstrCode),
  try compile:forms(AbstrCode1, Opts ++ ?SRC_COMPILE_OPTS) of
      {ok, _, Core} -> {ok, Core};
      _What -> error
  catch
    error:_ -> error
  end.

%% ============================================================================
%%
%%  Typed Records
%%
%% ============================================================================


-spec(get_record_and_type_info/1 :: 
      (abstract_code()) -> {'ok', dict()} | {'error', string()}).
	 
get_record_and_type_info(AbstractCode) ->
  get_record_and_type_info(AbstractCode, dict:new()).

-spec(get_record_and_type_info/2 :: 
      (abstract_code(), dict()) -> {'ok', dict()} | {'error', string()}).

get_record_and_type_info([{attribute, _, record, {Name, Fields0}}|Left], 
			 RecDict) ->
  case get_record_fields(Fields0, RecDict) of
    {ok, Fields} ->
      Arity = length(Fields),
      Fun = fun(OldOrdDict) -> orddict:store(Arity, Fields, OldOrdDict) end,
      NewRecDict = dict:update(Name, Fun, [{Arity, Fields}], RecDict),
      get_record_and_type_info(Left, NewRecDict);
    {error, Error} ->
      {error, lists:flatten(io_lib:format("  Error while parsing #~w{}: ~s\n",
					  [Name, Error]))}
  end;
get_record_and_type_info([{attribute, _, type, {{record, Name}, Fields0, []}}
			  |Left], RecDict) ->
  %% This overrides the original record declaration.
  case get_record_fields(Fields0, RecDict) of
    {ok, Fields} ->
      Arity = length(Fields),
      Fun = fun(OldOrdDict) -> orddict:store(Arity, Fields, OldOrdDict) end,
      NewRecDict = dict:update(Name, Fun, [{Arity, Fields}], RecDict),
      get_record_and_type_info(Left, NewRecDict);
    {error, Error} ->
      {error, lists:flatten(io_lib:format("  Error while parsing #~w{}: ~s\n",
					  [Name, Error]))}
  end;
get_record_and_type_info([{attribute, _, type, {Name, TypeForm}}|Left], 
			 RecDict) ->
  try
    NewRecDict = add_new_type(Name, TypeForm, [], RecDict),
    get_record_and_type_info(Left, NewRecDict)
  catch
    throw:{error, What} -> {error, What}
  end;
get_record_and_type_info([{attribute, _, type, {Name, TypeForm, Args}}|Left], 
			 RecDict) ->
  try
    NewRecDict = add_new_type(Name, TypeForm, Args, RecDict),
    get_record_and_type_info(Left, NewRecDict)
  catch
    throw:{error, What} -> {error, What}
  end;
get_record_and_type_info([_Other|Left], RecDict) ->
  get_record_and_type_info(Left, RecDict);
get_record_and_type_info([], RecDict) ->
  {ok, RecDict}.

add_new_type(Name, TypeForm, ArgForms, RecDict) ->
  case erl_types:type_is_defined(Name, RecDict) of 
    true ->
      throw({error, io_lib:format("Type already defined: ~w\n", [Name])});
    false ->
      ArgTypes = [erl_types:t_from_form(X) || X <- ArgForms],
      _Type = erl_types:t_from_form(TypeForm, RecDict),
      case lists:all(fun erl_types:t_is_var/1, ArgTypes) of
	true ->
	  ArgNames = [erl_types:t_var_name(X) || X <- ArgTypes],
	  dict:store({type, Name}, {TypeForm, ArgNames}, RecDict);
	false ->
	  throw({error, io_lib:format("Type specification for ~w does not"
				      " have variables as parameters", [Name])})
      end
  end.

get_record_fields(Fields, RecDict) ->
  get_record_fields(Fields, RecDict, []).

get_record_fields([{typed_record_field, OrdRecField, TypeForm}|Left], 
		  RecDict, Acc) ->
  Name =
    case OrdRecField of
      {record_field, _Line, Name0} -> erl_parse:normalise(Name0);
      {record_field, _Line, Name0, _Init} -> erl_parse:normalise(Name0)
    end,
  try 
    Type = erl_types:t_from_form(TypeForm, RecDict),
    get_record_fields(Left, RecDict, [{Name, Type}|Acc])
  catch
    throw:{error, What} -> {error, What}
  end;
get_record_fields([{record_field, _Line, Name}|Left], RecDict, Acc) ->
  NewAcc = [{erl_parse:normalise(Name), erl_types:t_any()}|Acc],
  get_record_fields(Left, RecDict, NewAcc);
get_record_fields([{record_field, _Line, Name, _Init}|Left], RecDict, Acc) ->
  NewAcc = [{erl_parse:normalise(Name), erl_types:t_any()}|Acc],
  get_record_fields(Left, RecDict, NewAcc);
get_record_fields([], _RecDict, Acc) ->
  {ok, lists:reverse(Acc)}.

%% ============================================================================
%%
%%  Spec info
%%
%% ============================================================================

-spec(get_spec_info/2 :: 
      (abstract_code(), dict()) -> {'ok', dict()} | {'error', string()}).

get_spec_info(AbstractCode, RecordsDict) ->
  {value, {attribute, _, module, ModName}} =
    lists:keysearch(module, 3, AbstractCode),
  get_spec_info(AbstractCode, dict:new(), RecordsDict, ModName, "nofile").

%% TypeSpec is a list of conditional contracts for a function. 
%% Each contract will be {[Argument], Range, [Constraint]} where 
%% Argument and Range -> erl_types format.
%% Constraint -> {subtype, X, Y}

get_spec_info([{attribute, Ln, spec, {Id, TypeSpec}}|Left], 
	      SpecDict, RecordsDict, ModName, 
	      CurrentFile) when is_list(TypeSpec) ->
  {Mod, Fun, Arity} =
    case Id of
      {_, _, _} = MFA -> MFA;
      {F, A} -> {ModName, F, A}
    end,
  try 
    Contract = dialyzer_contracts:contract_from_form(TypeSpec, RecordsDict),
    Index = {Mod, Fun, Arity},
    case dict:find(Index, SpecDict) of
      error -> 
	NewSpecDict = 
	  dict:store(Index, {{CurrentFile, Ln}, Contract}, SpecDict),
	get_spec_info(Left, NewSpecDict, RecordsDict, ModName, CurrentFile);
      {ok, {{File, L},_C}} -> 
	Msg = io_lib:format("  Contract for function ~w:~w/~w " 
			    "already defined in ~s:~w.\n", 
			    [ModName, Fun, Arity, File, L]),
	throw({error, Msg})
    end
  catch
    throw:{error, Error} -> 
      {error, lists:flatten(io_lib:format("  Error while parsing contract "
					  "in line ~w: ~s\n", [Ln,Error]))}
  end;
get_spec_info([{attribute, _, file, {File, _}}|Left],
	      SpecDict, RecordsDict, ModName, _CurrentFile) ->
  get_spec_info(Left, SpecDict, RecordsDict, ModName, File);
get_spec_info([_Other|Left], SpecDict, RecordsDict, ModName, CurrentFile) ->
  get_spec_info(Left, SpecDict, RecordsDict, ModName, CurrentFile);
get_spec_info([], SpecDict, _RecordsDict, _ModName, _CurrentFile) ->
  {ok, SpecDict}.

%% ============================================================================
%%
%%  Util utils
%%
%% ============================================================================

cleanup_parse_transforms([{attribute, _, compile, {parse_transform,_}}|Left]) ->
  cleanup_parse_transforms(Left);
cleanup_parse_transforms([Other|Left]) ->
  [Other|cleanup_parse_transforms(Left)];
cleanup_parse_transforms([]) ->
  [].

-spec(format_errors/1 :: ([{atom(), string()}]) -> [string()]).

format_errors([{Mod, Errors}|Left]) ->
  FormatedError = 
    [io_lib:format("~s:~w: ~s\n", [Mod, Line, M:format_error(Desc)])
     || {Line, M, Desc} <- Errors],
  [lists:flatten(FormatedError) | format_errors(Left)];
format_errors([]) ->
  [].

-spec(format_sig/1 :: (erl_type()) -> string()).

format_sig(Type) ->
  format_sig(Type, dict:new()).

-spec(format_sig/2 :: (erl_type(), dict()) -> string()).

format_sig(Type, RecDict) ->
  "fun(" ++ Sig = lists:flatten(erl_types:t_to_string(Type, RecDict)),
  ")" ++ RevSig = lists:reverse(Sig),
  lists:reverse(RevSig).

%%-------------------------------------------------------------------
%% Author      : Per Gustafsson <pergu@it.uu.se>
%% Description : Provides better printing of binaries.
%% Created     : 5 March 2007
%%-------------------------------------------------------------------

-spec(pp_hook/0 :: () -> fun((core_tree(), _, _) -> any())).

pp_hook() ->
  fun pp_hook/3.

pp_hook(Node, Ctxt, Cont) ->
  case cerl:type(Node) of
    binary ->
      pp_binary(Node,Ctxt,Cont);
    bitstr ->
      pp_segment(Node,Ctxt,Cont);
    _ ->
      Cont(Node, Ctxt)
  end.

pp_binary(Node, Ctxt, Cont) ->
  prettypr:beside(prettypr:text("<<"),
		  prettypr:beside(pp_segments(cerl:binary_segments(Node),
					      Ctxt, Cont),
				  prettypr:text(">>"))).

pp_segments([Seg], Ctxt, Cont) ->
  pp_segment(Seg, Ctxt, Cont);
pp_segments([], _Ctxt, _Cont) ->
  prettypr:text("");
pp_segments([Seg|Rest], Ctxt, Cont) ->
  prettypr:beside(pp_segment(Seg, Ctxt, Cont),
		  prettypr:beside(prettypr:text(","),
				  pp_segments(Rest, Ctxt, Cont))).

pp_segment(Node, Ctxt, Cont) ->
  Val = cerl:bitstr_val(Node),
  Size = cerl:bitstr_size(Node),
  Unit = cerl:bitstr_unit(Node),
  Type = cerl:bitstr_type(Node),
  Flags = cerl:bitstr_flags(Node),
  prettypr:beside(Cont(Val,Ctxt),
		  prettypr:beside(pp_size(Size, Ctxt, Cont),
				  prettypr:beside(pp_opts(Type, Flags),
						  pp_unit(Unit, Ctxt, Cont)))).
  
pp_size(Size, Ctxt, Cont) ->
  case cerl:is_c_atom(Size) of
    true ->
      prettypr:text("");
    false ->
      prettypr:beside(prettypr:text(":"), Cont(Size, Ctxt))
  end.

pp_opts(Type,Flags) ->
  FinalFlags = 
    case cerl:atom_val(Type) of
      integer -> keep_all(cerl:concrete(Flags));
      float -> keep_endian(cerl:concrete(Flags));
      binary -> []
    end,
  prettypr:beside(prettypr:text("/"),
		  prettypr:beside(pp_atom(Type),
				  pp_flags(FinalFlags))).

pp_flags([]) ->
  prettypr:text("");
pp_flags([Flag|Flags]) ->
  prettypr:beside(prettypr:text("-"),
		  prettypr:beside(pp_atom(Flag),
				  pp_flags(Flags))).

keep_endian(Flags) ->
  [cerl:c_atom(X) || X <- Flags, (X =:= little) or (X =:= native)].

keep_all(Flags) ->
  [cerl:c_atom(X) || X <- Flags,
		     (X =:= little) or (X =:= native) or (X =:= signed)].

pp_unit(Unit, Ctxt, Cont) ->
  prettypr:beside(prettypr:text("-"),
		  prettypr:beside(prettypr:text("unit:"),
				  Cont(Unit, Ctxt))).

pp_atom(Atom) ->
  String = atom_to_list(cerl:atom_val(Atom)),
  prettypr:text(String).
