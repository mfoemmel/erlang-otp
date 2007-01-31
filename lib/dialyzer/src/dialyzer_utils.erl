%%%-------------------------------------------------------------------
%%% File    : dialyzer_utils.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : 
%%%
%%% Created :  5 Dec 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_utils).

-export([
	 get_abstract_code_from_beam/1,
	 get_abstract_code_from_src/1,
	 get_abstract_code_from_src/2,
	 get_core_from_abstract_code/1,
	 get_core_from_src/1,
	 get_core_from_src/2,
	 get_core_from_beam/1,
	 get_record_info/1,
	 merge_record_dicts/1
	]).

-include("dialyzer.hrl").

%%% ============================================================================
%%%
%%%  Compilation utils
%%%
%%% ============================================================================

get_abstract_code_from_src(File) ->
  get_abstract_code_from_src(File, []).

get_abstract_code_from_src(File, Opts) ->
  case compile:file(File, [to_pp, binary, typed_record|Opts]) of
    error -> {error, []};
    {error, Errors, _} -> {error, format_errors(Errors)};
    {ok, _, AbstrCode} -> AbstrCode
  end.

get_core_from_src(File) ->
  get_core_from_src(File, []).

get_core_from_src(File, Opts) ->
  case get_abstract_code_from_src(File, Opts) of
    {error, What} -> {error, What};
    AbstrCode ->
      case get_core_from_abstract_code(AbstrCode) of
	error -> {error, "Could not get abstract core"};
	Core -> Core
      end
  end.

get_core_from_beam(File) ->
  case get_abstract_code_from_beam(File) of
    error -> error;
    Abs -> get_core_from_abstract_code(Abs)
  end.

get_abstract_code_from_beam(File) ->
  case beam_lib:chunks(File, [abstract_code]) of
    {ok,{_,List}} ->
      case lists:keysearch(abstract_code, 1, List) of
	{value, {abstract_code,{raw_abstract_v1,Abstr}}} -> Abstr;
	_ -> error
      end;
    _ ->
      %% No or unsuitable abstract code.
      error
  end.

get_core_from_abstract_code(AbstrCode) ->
  %% We do not want the parse_transorms left since we have already
  %% performed them. In some cases we end up in trouble when
  %% performing them again.
  AbstrCode1 = cleanup_parse_transforms(AbstrCode),
  try compile:forms(AbstrCode1, ?SRC_COMPILE_OPTS) of
      {ok,_,Core} -> Core;
      _What -> error
  catch
    error:_ -> error
  end.

%%% ============================================================================
%%%
%%%  Typed Records
%%%
%%% ============================================================================

get_record_info(AbstractCode) ->
  get_record_info(AbstractCode, dict:new()).

get_record_info([{attribute, _, record, {Name, Fields0}}|Left], RecDict) ->
  case get_record_fields(Fields0, RecDict) of
    {ok, Fields} ->
      Arity = length(Fields),
      Fun = fun(OldOrdDict)->orddict:store(Arity, Fields, OldOrdDict)end,
      NewRecDict = dict:update(Name, Fun, [{Arity, Fields}], RecDict),
      get_record_info(Left, NewRecDict);
    {error, Error} ->
      {error, lists:flatten(io_lib:format("Error while parsing ~w#{}: ~s\n",
					  [Name, Error]))}
  end;
get_record_info([_Other|Left], RecDict) ->
  get_record_info(Left, RecDict);
get_record_info([], RecDict) ->
  {ok, RecDict}.

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

merge_record_dicts([RecDict]) ->
  {ok, RecDict};
merge_record_dicts([RecDict1, RecDict2|Left]) ->
  case merge_record_dicts(RecDict1, RecDict2) of
    {ok, NewRecDict} -> merge_record_dicts([NewRecDict|Left]);
    Error -> Error
  end.

merge_record_dicts(RecDict1, RecDict2) ->
  try {ok, dict:merge(fun handle_merge_clash/3, RecDict1, RecDict2)}
  catch
    throw:{record_clash, Tag, Arity} ->
      {record_clash, Tag, Arity}
  end.

handle_merge_clash(Tag, Orddict1, Orddict2) ->
  orddict:merge(fun(_Arity, Fields, Fields) -> Fields;
		   (Arity, _, _) -> throw({record_clash, Tag, Arity})
		end, Orddict1, Orddict2).

%%% ============================================================================
%%%
%%%  Util utils
%%%
%%% ============================================================================

cleanup_parse_transforms([{attribute,_, compile, {parse_transform, _}}|Left]) ->
  cleanup_parse_transforms(Left);
cleanup_parse_transforms([Other|Left]) ->
  [Other|cleanup_parse_transforms(Left)];
cleanup_parse_transforms([]) ->
  [].

format_errors([{Mod, Errors}|Left])->
  FormatedError = 
    [io_lib:format("~s:~w: ~s\n", [Mod, Line,apply(M,format_error, [Desc])])
     || {Line, M, Desc} <- Errors],
  [lists:flatten(FormatedError) | format_errors(Left)];
format_errors([]) ->
  [].
