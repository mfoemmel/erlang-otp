%% -*- erlang-indent-level: 2 -*-
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
	 get_core_from_abstract_code/2,
	 get_core_from_src/1,
	 get_core_from_src/2,
	 get_core_from_beam/2,
	 get_record_info/1,
	 get_spec_info/1,
	 merge_record_dicts/1,
	 pp_hook/0
	]).

-include("dialyzer.hrl").

%% ============================================================================
%%
%%  Compilation utils
%%
%% ============================================================================

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
      case get_core_from_abstract_code(AbstrCode, Opts) of
	error -> {error, "Could not get abstract core"};
	Core -> Core
      end
  end.

get_core_from_beam(File, Opts) ->
  case get_abstract_code_from_beam(File) of
    error -> error;
    Abs -> get_core_from_abstract_code(Abs, Opts)
  end.

get_abstract_code_from_beam(File) ->
  case beam_lib:chunks(File, [abstract_code]) of
    {ok, {_, List}} ->
      case lists:keysearch(abstract_code, 1, List) of
	{value, {abstract_code, {raw_abstract_v1, Abstr}}} -> Abstr;
	_ -> error
      end;
    _ ->
      %% No or unsuitable abstract code.
      error
  end.

get_core_from_abstract_code(AbstrCode) ->
  get_core_from_abstract_code(AbstrCode, []).

get_core_from_abstract_code(AbstrCode, Opts) ->
  %% We do not want the parse_transorms left since we have already
  %% performed them. In some cases we end up in trouble when
  %% performing them again.
  AbstrCode1 = cleanup_parse_transforms(AbstrCode),
  try compile:forms(AbstrCode1, Opts ++ ?SRC_COMPILE_OPTS) of
      {ok, _, Core} -> Core;
      _What -> error
  catch
    error:_ -> error
  end.

%% ============================================================================
%%
%%  Typed Records
%%
%% ============================================================================

get_record_info(AbstractCode) ->
  get_record_info(AbstractCode, dict:new()).

get_record_info([{attribute, _, record, {Name, Fields0}}|Left], RecDict) ->
  case get_record_fields(Fields0, RecDict) of
    {ok, Fields} ->
      Arity = length(Fields),
      Fun = fun(OldOrdDict) -> orddict:store(Arity, Fields, OldOrdDict) end,
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


%% ============================================================================
%%
%%  Spec info
%%
%% ============================================================================


get_spec_info(AbstractCode) ->
  get_spec_info(AbstractCode, dict:new()).

%% TypeSpec is a list of contracts for a function. 
%% Each contract is [[Arguments], Range] with parse forms.
get_spec_info([{attribute, _, type_spec, {{Name, Arity}, TypeSpec}}|Left], 
	      SpecDict) when is_list(TypeSpec) ->
  case form_to_contract(TypeSpec) of
    error -> {error, SpecDict};
    Contract ->
      NewSpecDict = dict:store({Name, Arity}, Contract, SpecDict),
      get_spec_info(Left, NewSpecDict)
  end;

get_spec_info([_Other|Left], SpecDict) ->
  get_spec_info(Left, SpecDict);

get_spec_info([], SpecDict) ->
  {ok, SpecDict}.


form_to_contract([{type, 'fun', Info} | Left]) -> 
  [Info|form_to_contract(Left)];
form_to_contract([_|_]) -> io:format("The contract does not follow the function format: \"(Arguments) -> Return\"\n"), error;
form_to_contract([]) -> [].
  

% Warnings
% We admit 2 contracts for the same function rigth now. 
% Previous contracts are overwritten. 

%% ============================================================================
%%
%%  Util utils
%%
%% ============================================================================

cleanup_parse_transforms([{attribute,_, compile, {parse_transform, _}}|Left]) ->
  cleanup_parse_transforms(Left);
cleanup_parse_transforms([Other|Left]) ->
  [Other|cleanup_parse_transforms(Left)];
cleanup_parse_transforms([]) ->
  [].

format_errors([{Mod, Errors}|Left]) ->
  FormatedError = 
    [io_lib:format("~s:~w: ~s\n", [Mod, Line, M:format_error(Desc)])
     || {Line, M, Desc} <- Errors],
  [lists:flatten(FormatedError) | format_errors(Left)];
format_errors([]) ->
  [].

%%%-------------------------------------------------------------------
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : pp_hook for better printing of binaries. 
%%%
%%% Created :  5 Mar 2007 by Per Gustafsson <pergu@dhcp-12-245.it.uu.se>
%%%-------------------------------------------------------------------

pp_hook() ->
    fun pp_hook/3.

pp_hook(Node, Ctxt, Cont) ->
  case cerl:is_c_binary(Node) of
    true ->
      pp_binary(Node,Ctxt,Cont);
    false ->
      Cont(Node, Ctxt)
  end.


pp_binary(Node,Ctxt,Cont) ->
  prettypr:beside(prettypr:text("<<"),
		  prettypr:beside(pp_segments(cerl:binary_segments(Node),Ctxt,Cont),
				  prettypr:text(">>"))).

pp_segments([Seg],Ctxt,Cont) ->
  pp_segment(Seg,Ctxt,Cont);
pp_segments([],_Ctxt,_Cont) ->
  prettypr:text("");
pp_segments([Seg|Rest],Ctxt,Cont) ->
  prettypr:beside(pp_segment(Seg,Ctxt,Cont),
		  prettypr:beside(prettypr:text(","),
				  pp_segments(Rest,Ctxt,Cont))).

pp_segment(Node,Ctxt,Cont) ->
  Val = cerl:bitstr_val(Node),
  Size = cerl:bitstr_size(Node),
  Unit = cerl:bitstr_unit(Node),
  Type = cerl:bitstr_type(Node),
  Flags = cerl:bitstr_flags(Node),
  prettypr:beside(Cont(Val,Ctxt),
		  prettypr:beside(pp_size(Size,Ctxt,Cont),
				  prettypr:beside(pp_opts(Type,Flags),
						  pp_unit(Unit,Ctxt,Cont)))).
  
pp_size(Size,Ctxt,Cont) ->
  case cerl:is_c_atom(Size) of
    true ->
      prettypr:text("");
    false ->
      prettypr:beside(prettypr:text(":"),
		     Cont(Size,Ctxt))
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

%keep_endian(Flags) ->
%  [cerl:c_atom(X) || X <- Flags, (X =:= big) or (X =:= little)
%  or (X =:= native)].

%keep_all(Flags) ->
%  [cerl:c_atom(X) || X <- Flags].

keep_endian(Flags) ->
  [cerl:c_atom(X) || X <- Flags, (X =:= little) or (X =:= native)].

keep_all(Flags) ->
  [cerl:c_atom(X) || X <- Flags,(X =:= little) or (X =:= native)
		      or (X =:= signed)].

pp_unit(Unit,Ctxt,Cont) ->
  prettypr:beside(prettypr:text("-"),
		  prettypr:beside(prettypr:text("unit:"),
				  Cont(Unit,Ctxt))).
pp_atom(Atom) ->
  String = atom_to_list(cerl:atom_val(Atom)),
  prettypr:text(String).
