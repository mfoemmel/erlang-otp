%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc	This module performs integer range analysis on ICode.
%%
%% <h3>Purpose</h3>
%%
%% <p>iterating, fixing and adding in a happily manner.</p>
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_range_an).

-export([init/4]).
-export([to_string/1]).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").

-define(LABELITERATIONS, 10).
-define(PHIWIDENING, 4).
-define(FUNCTION_FIXPOINT_DEPTH, 5).

%%-define(not_done_debug, fun(X, Y) -> io:format(X, Y) end).
-define(not_done_debug, fun(_, _) -> ok end).

%% -define(MFA_debug, fun(MFA, X, Y) -> 
%%   		       if MFA =:= {epp,scan_toks,3} ->
%%   			   io:format("~s ~p~n", [X, Y]);
%%   			  true  ->
%%   			   ok
%%   		       end
%%   		   end).
-define(MFA_debug, fun(_, _, _) -> ok end).

%%-define(call_or_enter_debug, fun(X, Y) -> io:format(X, Y) end).
-define(call_or_enter_debug, fun(_, _) -> ok end).

%%todo snodd
-define(TAG_IMMED1_SIZE, 4).

-define(BITS, (hipe_rtl_arch:word_size() bsl 3) - ?TAG_IMMED1_SIZE).
-define(FIXNUM_UPPER, ((1 bsl (?BITS - 1)) - 1)).
-define(FIXNUM_LOWER, -(1 bsl (?BITS - 1))).
-define(MAX_BYTE, 255).
-define(MAX_CHAR, 16#10ffff).

%%-define(DEBUG, false).

-define(DO_RANGE_TEST, true).

-ifdef(DO_RANGE_TEST).
-export([test/0]).
-else.
-define(NO_UNUSED, true).
-endif.

%% This record is used when returning information
-record(range_info, {live_labels, 
		     in_label_range_trees,
		     out_label_range_trees,
		     return_points,
		     warn
		    }).	

%% This record is used by the range analyser
-record(info_struct, {worklist, 
		      phi_values=gb_trees:empty(), 
		      range_trees=gb_trees:empty(),
		      pred_trees=gb_trees:empty(),
		      current_range_tree=gb_trees:empty(),
		      current_label,
		      is_recursive=false,
		      current_mfa,
		      current_mfa_not_done=false,
		      label_counters=gb_trees:empty(),
		      return_vars=[],
		      predmap,
		      liveness,
		      live_labels=gb_sets:empty(),
		      startlabel,
		      wideningvalues=[posinf, neginf], 
		      server,
		      warn
		     }).

%% This is the representation of a range.
-record(var_range, {var_name,
		    range=empty,
		    other
		   }).

%%
%% Init
%%
%% Initializes the analysis
%%

init(IC, Options, Server, MFA) ->  
  %%io:format("analysing ~p ~n", [MFA]),
  %%hipe_icode_pp:pp(hipe_icode_cfg:cfg_to_linear(IC)),
  case info_struct__init(IC, Server, Options, MFA) of
    break ->
      none;
    Info ->
      Info2 = analyse_icode(IC, Info),
      case info_struct__current_mfa_not_done(Info2) of 
	true ->
	  server__update_return_value(Info2),
	  not_fixpoint;
	false ->
	  server__update_return_value(Info2), % I Know :)
	  Range_info = range_info_from_info_struct(Info2),	
	  %% Specialization
	  SpecIC = specialize_IC(IC, Range_info),
	  %% Optional annotation phase controlled by a compiler option
	  case proplists:get_bool(icode_range_analysis_annotate, Options) of
	    true ->
	      NewIC = annotate_IC(SpecIC, Range_info),
	      print_icode_to_file(NewIC, Info2);
	    false ->
	      ok
	  end,
	  case proplists:get_bool(icode_range_analysis_insn_count, Options) of
	    true ->
	      Old = hipe_icode_instruction_counter:cfg(IC, MFA, Options),
	      New = hipe_icode_instruction_counter:cfg(SpecIC, MFA, Options),
	      %%io:format("Old insn count ~p ~n", [gb_trees:to_list(Old)]),
	      hipe_icode_instruction_counter:compare(MFA, Old, New);
	    false ->
	      ok
	  end,	  
	  SpecIC 
      end
  end.

%%
%% Server
%%
%% Handles communication with the information server
%%

server__update_return_value(Info) ->
  Return_range = return_range(Info),
  Return_range_is_not_set = var_range__is_empty(Return_range) andalso var_range__is_not_other(Return_range),
  MFA = info_struct__current_mfa(Info),
  Not_done = info_struct__current_mfa_not_done(Info),
  Server = info_struct__server(Info),
  ?MFA_debug(MFA, "return range", Return_range),
  %%io:format("MFA ~p ~p ~n", [MFA, Return_range]),
  if not Return_range_is_not_set ->
      Wideningvalues = info_struct__wideningvalues(Info),
      Fun = fun (MessageTree) ->
		Key = {MFA, return_range},
		{State, Prev_return_range} = 
		  case gb_trees:lookup(Key, MessageTree) of
		    none ->
		      {0, range_init(return_range, empty, false)};
		    {value, {Lookup_return_range, Prev_state}} ->
		      {Prev_state, Lookup_return_range}
		  end,
		Fixed_return_range = 
		  if (State > ?FUNCTION_FIXPOINT_DEPTH) ->
		      range_widening(Prev_return_range, Return_range, 
				     Wideningvalues);
		     true ->
		      Return_range
		  end, 
		Is_not_updated = var_range__is_equal(Fixed_return_range, 
						     Prev_return_range),
		if Is_not_updated -> 
		    MessageTree;
		   true -> 
		    gb_trees:enter(Key, {Fixed_return_range, State + 1}, 
				   MessageTree)
		end
	    end,
      Server ! {self(), {transaction, Fun}};
     not Not_done ->
      Fun = fun(MessageTree) ->
		Key = {MFA, return_range},
		Any = range_init(return_range, {neginf, posinf}, true),
		case gb_trees:lookup(Key, MessageTree) of
		  none ->
		    %%io:format("no returnpoints ~p ~n", [MFA]),
		    gb_trees:enter(Key, {Any, 1}, 
				   MessageTree);
		  {value, _Val} ->
		    MessageTree
		end
	    end,
      Server ! {self(), {transaction, Fun}};
     true ->
      ok
  end.

server__update_call_args(MFA, Args, Info) -> %% TODO Needs comments
  Server = info_struct__server(Info),
  Server ! {self(), {load, message, {MFA, args}}},
  Rename_fun = fun(Arg) -> 
		   var_range__copy(get_range_from_arg(Arg, Info), param) 
	       end,
  receive
    none ->
      Lookup_state = 0,
      Args_updated = true,
      Insert_args = lists:map(Rename_fun, Args);
    {value, {Lookup_args, Lookup_state}} ->
      %% io:format("Lookup_args ~p ~n MFA ~p ~n", [Lookup_args, MFA]),
      Arg_ranges = lists:map(Rename_fun, Args), %% this isn't needed
      Tuple_args_list = lists:zipwith(fun(X, Y) -> [X,Y] end,
				      Arg_ranges, Lookup_args),
      %% io:format("Tuple ~p ~n", [Tuple_args_list]),
      Union_args = lists:map(fun(Ranges) -> range_union(param, Ranges) end,
			     Tuple_args_list),
      %% io:format("Lookup state ~p ~n", [Lookup_state]),
      Insert_args = 
	if Lookup_state > ?FUNCTION_FIXPOINT_DEPTH ->
	    Widening_range_list = lists:zip(Lookup_args, Union_args),
	    Wideningvalues = info_struct__wideningvalues(Info),
	    Widened_ranges = 
	      lists:map(
		fun({Old_range, New_range}) ->
		    range_widening(Old_range, New_range, Wideningvalues)
		end,
		Widening_range_list),
	    %% io:format("New_wided ~p ~n", [R]),
	    Widened_ranges;
	   true ->
	    Union_args
	end,
      Range_tuple_list = lists:zip(Insert_args, Lookup_args),
      Args_updated = lists:foldl(
		       fun({Range1, Range2}, Bool) ->
			   (Range1 =/= Range2) or Bool
		       end,
		       false,
		       Range_tuple_list)
  end,
  New_info=
    if Args_updated ->
	%%?not_done_debug("server_update_args break ~n", []),
	Server ! {self(), {message, {MFA, args}, {Insert_args, Lookup_state + 1}}},

	info_struct__set_current_mfa_not_done(Info, true);
       true ->
	Info
    end,
  New_info.


%%--------------------------------------------------------------------------
%% Icode helper functions
%%--------------------------------------------------------------------------

unannotate_var(An_var) ->
  case hipe_icode:is_annotated_var(An_var) of
    true ->
      hipe_icode:unannotate_var(An_var);
    false ->
      An_var
  end.

name_from_icode_var(An_var) ->
  Var = unannotate_var(An_var),
  case hipe_icode:is_var(Var) of
    true ->
      hipe_icode:var_name(Var);
    false ->
      case Var of 
	{reg, N} ->
	  {reg, N};
	{const, _} ->
	  const;
	_ ->
	  {f, hipe_icode:fvar_name(Var)}
      end
      %% constants??
  end.

get_range_from_args(Arglist, Info) ->
  lists:map(fun (Arg) -> get_range_from_arg(Arg, Info) end, Arglist).

get_range_from_arg(Arg, Info) ->
  UnannoArg = unannotate_var(Arg),
  case hipe_icode:is_const(UnannoArg) of
    true ->
      Value = hipe_icode:const_value(UnannoArg),
      case is_integer(Value) of
	true ->
	  range_init(const, {Value, Value}, false);
	false ->
	  range_init(const, empty, true)
      end;
    false -> % It's a variable
      case hipe_icode:is_fvar(Arg) of
	true ->
	  range_init(Arg, empty, true);
	false ->	
	  Var_name = name_from_icode_var(UnannoArg),
	  info_struct__get_range(Var_name, Info)
      end
  end.

int_range_from_number_val(Number) ->
  case Number of 
    any ->
      {neginf, posinf};
    N when is_integer(N) ->
      {N, N};
    none ->
      empty
  end.

int_range_from_number_vals([]) -> empty;
int_range_from_number_vals([First_number|Numbers]) ->
  The_union =
    fun(Number_val, Acc) ->
	case Acc of
	  {Min2, Max2} ->
	    case int_range_from_number_val(Number_val) of
	      {Min1, Max1} ->
		New_min = inf_min([Min1, Min2]),
		New_max = inf_max([Max1, Max2]),
		{New_min, New_max};
	      empty ->
		{Min2, Max2}
	    end;
	  empty ->
	    case int_range_from_number_val(Number_val) of
	      {Min1, Max1} ->
		{Min1, Max1};
	      empty ->
		empty
	    end
	end
    end,
  lists:foldl(The_union, int_range_from_number_val(First_number), Numbers);
int_range_from_number_vals(Number) ->
  int_range_from_number_val(Number).

get_range_from_annotation(Arg_info, Key) ->
  Is_byte = erl_types:t_is_byte(Arg_info),
  Is_char = erl_types:t_is_char(Arg_info),
  Is_integer = erl_types:t_is_integer(Arg_info),
  {Int_range, Other} = 
    if Is_byte ->
	{{0, ?MAX_BYTE}, false};
       Is_char ->
	{{0, ?MAX_CHAR}, false};
       Is_integer ->
	{{neginf, posinf}, false};
       true ->
	Number_vals = erl_types:t_number_vals(Arg_info),
	Arg = 
	  case Arg_info of 
	    [_|_] ->
	      [erl_types:t_integer()|Arg_info];
	    _ ->
	      [erl_types:t_integer(),Arg_info]
	  end,
	Is_only_int = erl_types:t_is_integer(erl_types:t_sup(Arg)),
	{int_range_from_number_vals(Number_vals), not Is_only_int} 
    end,
  range_init(Key, Int_range, Other).

keep_vars(Vars)->
  [V || V <- Vars, hipe_icode:is_var(unannotate_var(V))].

dont_keep_vars(Vars)->
  [V || V <- Vars, not hipe_icode:is_var(unannotate_var(V))].

defines(I)->
  keep_vars(hipe_icode:defines(I)).

uses(I)->
  keep_vars(hipe_icode:uses(I)).

consts(I) ->
  dont_keep_vars(hipe_icode:args(I)).

%%
%% Icode analysis
%%
%% Propagates range information using Icode.
%%

analyse_icode(IC, Info) ->
  {Work, Info2} = info_struct__get_work(Info),
  case Work of
    {value, Label} ->
      case info_struct__set_new_current_tree(Label, Info2) of
	break ->
	  analyse_icode(IC, Info2);
	Info3 ->
	  %% io:format("Analysing ~p ~n", [Label]),
	  BB = hipe_icode_cfg:bb(IC, Label),
	  Code = hipe_bb:code(BB),
	  %% hipe_icode_pp:pp_block(Code),
	  Info4 = analyse_BB(Code, Info3),
	  analyse_icode(IC, Info4)
      end;
    none ->
      Info2
  end.	

analyse_BB([Last], Info) ->
  analyse_last_insn(Last, Info);
analyse_BB([Insn|InsnList], Info) ->
  case analyse_insn(Insn, Info) of
    {break, NewInfo} ->
      %%?not_done_debug("analyse_BB break ~n", []),
      info_struct__set_current_mfa_not_done(NewInfo, true); 
    NewInfo = #info_struct{} ->
      analyse_BB(InsnList, NewInfo)
  end.

analyse_insn(Insn, Info) ->
  %% hipe_icode_pp:pp_block([Insn]),
  case Insn of 
    #call{} -> analyse_call(Insn, Info);
    #move{} -> analyse_move(Insn, Info);
    #phi{} -> analyse_phi(Insn, Info);
    #fmove{} -> analyse_fmove(Insn, Info);
    #begin_handler{} -> analyse_begin_handler(Insn, Info)
  end.

analyse_last_insn(Insn, Info) ->
  %% hipe_icode_pp:pp_block([Insn]),
  Case_return = 
    case Insn of 
      #return{} -> analyse_return(Insn, Info);
      #enter{} -> analyse_enter(Insn, Info);
      #switch_val{} -> analyse_switch_val(Insn, Info);
      #'if'{} -> analyse_if(Insn, Info);
      #goto{} -> analyse_goto(Insn, Info);	
      #type{} -> analyse_type(Insn, Info);
      #fail{} -> analyse_fail(Insn, Info);
      #call{} -> analyse_last_call(Insn, Info);
      #switch_tuple_arity{} -> 
	analyse_switch_tuple_arity(Insn, Info);
      #begin_try{} -> analyse_begin_try(Insn, Info)
    end,
  case Case_return of
    {break, Updated_info} ->
      %%?not_done_debug("analyse_last_insn break ~p ~n", [Insn]),
      info_struct__set_current_mfa_not_done(Updated_info, true);
    Updated_info = #info_struct{} -> %TODO ordning på labelar ... vad??
      {_, Return_info} = info_struct__save_current_range_tree({out_tree, info_struct__current_label(Updated_info)}, Updated_info),
      Return_info
  end.

%%---------------------------------------------------------------------------
%% Analysis for all icode instructions
%%---------------------------------------------------------------------------

analyse_call(Call, Info) ->
  Args = hipe_icode:args(Call),
  Dsts = hipe_icode:call_dstlist(Call),
  %% io:format("Calltype: ~p ~n", [Call]),
  Key = 
    case Dsts of
      [] ->
	undef; %%todo asumption that no real var name can be undef
      [Dst|_] ->
	name_from_icode_var(Dst)
    end,
  Fun = hipe_icode:call_fun(Call),
  Type = hipe_icode:call_type(Call),
  Analysis_result = analyse_call_or_enter_fun(Fun, Args, Key, Dsts, Info, Type),
  case Analysis_result of
    {break, Analyse_info} ->
      {break, Analyse_info};
    Dst_range = #var_range{} ->
      %%info_struct__insert_range(Dst_range, Info);
      if Key =/= undef ->
	  Annotation_range = get_range_from_dst_annotation(Key, Dsts),
	  Cut_range = range_cut(Key, [Dst_range, Annotation_range]),
	  Is_different = not var_range__is_equal(Dst_range, Cut_range),
	  if Is_different ->
	      Warn = info_struct__warn(Info),
	      warning(Cut_range, Dst_range, Warn);
	     true ->
	      ok
	  end,
	  info_struct__insert_range(Cut_range, Info);
	 true ->
	  Info
      end;
    New_Info = #info_struct{} -> New_Info
  end.

%%Analyses a call that is last in an BB.
analyse_last_call(Call, Info) ->
  %% hipe_icode_pp:pp_block([Insn]),
  case analyse_call(Call, Info) of
    {break, Info2} ->
      {break, Info2};
    Info2 = #info_struct{} ->
      Continuation = hipe_icode:call_continuation(Call),
      Current_label = info_struct__current_label(Info2),
      {Updated_edges, Info3} = info_struct__save_current_range_tree({Current_label, Continuation}, Info2),
      {_, Labels} = lists:unzip(Updated_edges),
      {Upd_fail_edge, Info4} = 
	case hipe_icode:call_fail_label(Call) of
	  [] -> {[], Info3};
	  Label when is_integer(Label) -> 
	    info_struct__save_current_range_tree({Current_label, Label}, Info3)
	end,
      Info5 = info_struct__add_work(Info4, Labels),
      {_, Upd_fail_label} = lists:unzip(Upd_fail_edge),
      info_struct__add_work(Info5, Upd_fail_label)
  end.

analyse_fmove(Insn, Info) ->
  Dst_name = hipe_icode:fmove_dst(Insn),
  Dst_range = range_init(Dst_name, empty, true),
  info_struct__insert_range(Dst_range, Info).

analyse_move(Insn, Info) ->
  Src = hipe_icode:move_src(Insn),
  Dst = hipe_icode:move_dst(Insn),
  Dst_name = name_from_icode_var(Dst),
  Dst_range = var_range__copy(get_range_from_arg(Src, Info), Dst_name),
  info_struct__insert_range(Dst_range, Info).


phi_widening(Union_range, Info) ->
  Name = var_range__name(Union_range),
  Old_range = info_struct__get_phi_range(Name, Info),
  Dont_wid_min=inf_geq(var_range__min(Union_range), var_range__min(Old_range)),
  Dont_wid_max=inf_geq(var_range__max(Old_range), var_range__max(Union_range)),
  Min = 
    case Dont_wid_min of
      true ->
	inf_min([var_range__min(Union_range),var_range__min(Old_range)]);
      false ->
	neginf
    end,
  Max =   
    case Dont_wid_max of
      true ->
	inf_max([var_range__max(Union_range), var_range__max(Old_range)]);
      false ->
	posinf
    end,
  Range_range = 
    case (Min =:= empty) or (Max =:= empty) of
      true ->
	empty;
      false ->
	{Min, Max}
    end,
  range_init(Name, Range_range, var_range__other(Union_range)).


analyse_phi(Phi, Info) ->
  {_, Args} = lists:unzip(hipe_icode:phi_arglist(Phi)),
  Dst = hipe_icode:phi_dst(Phi),
  Dst_name = name_from_icode_var(Dst),
  Arg_ranges = get_range_from_args(Args, Info),
  %%io:format("Phi-Arg_ranges: ~p ~n", [Arg_ranges]),
  Temp_dst_range = range_union(Dst_name, Arg_ranges),
  Label = info_struct__current_label(Info),
  Counter = info_struct__counter_from_label(Info, Label),
  Dst_range = 
    if Counter > ?PHIWIDENING ->
	phi_widening(Temp_dst_range, Info);
       true ->
	Temp_dst_range
    end,
  %%io:format("Phi_Dst_range: ~p ~n", [Dst_range]),
  Info2 = info_struct__set_phi_range(Dst_range, Info),	
  info_struct__insert_range(Dst_range, Info2).

analyse_return(Insn, Info) ->
  [Var] = hipe_icode:return_vars(Insn),
  Variable = name_from_icode_var(Var),
  Info2 = info_struct__add_return_var(Variable, Info),
  Current_label = info_struct__current_label(Info),
  {_, Info3} = info_struct__save_current_range_tree({Current_label, return}, Info2),
  Info3.

analyse_enter(Insn, Info) -> 
  Current_label = info_struct__current_label(Info),
  Key = enter_return,
  %% io:format("Enter ~p ~n", [Insn]),
  Args = hipe_icode:enter_args(Insn),
  Fun = hipe_icode:enter_fun(Insn),
  Analysis_result = analyse_call_or_enter_fun(Fun, Args, Key, Key, Info, local), 
  case Analysis_result of
    {break, Analyse_info} ->
      {break, Analyse_info};
    Dst_range = #var_range{} ->
      Dst_range#var_range{var_name = Key}, %% Assert
      Info3 = info_struct__insert_range(Dst_range, Info),
      Info4 = info_struct__add_return_var(Key, Info3),
      {_, Info5} = info_struct__save_current_range_tree({Current_label, return}, Info4),
      Info5;
    Info3 -> %fcall
      Info4 = info_struct__add_return_var(Key, Info3),
      {_, Info5} = info_struct__save_current_range_tree({Current_label, return}, Info4),
      Info5
  end.

analyse_begin_try(Insn, Info) ->
  Label = hipe_icode:begin_try_label(Insn),
  Successor = hipe_icode:begin_try_successor(Insn),
  Labels = [Label|[Successor]],
  %% io:format("begin try labels ~p ~n", [Labels]),
  {Updated_edges, Info2} = info_struct__save_trees_on_edges(Labels, Info, []),
  {_, Updated_Labels} = lists:unzip(Updated_edges),
  info_struct__add_work(Info2, Updated_Labels).	

analyse_begin_handler(Handler, Info) ->
  lists:foldl(
    fun (Var, InfoAcc) ->
	Var_range = range_init(
		      name_from_icode_var(Var), {neginf, posinf}, true),
	info_struct__insert_range(Var_range, InfoAcc)
    end,
    Info,
    hipe_icode:begin_handler_dstlist(Handler)).

analyse_switch_tuple_arity(Switch, Info) -> 
  Cases = hipe_icode:switch_tuple_arity_cases(Switch),
  Fail = hipe_icode:switch_tuple_arity_fail_label(Switch),
  {_, Case_labels} = lists:unzip(Cases),
  Labels = [Fail|Case_labels],
  %% io:format("switch labels ~p ~n", [Labels]),
  {Updated_edges, Info2} = info_struct__save_trees_on_edges(Labels, Info, []),
  {_, Updated_labels} = lists:unzip(Updated_edges),
  info_struct__add_work(Info2, Updated_labels).	

analyse_switch_val(Switch, Info) -> 
  Switch_range = get_range_from_arg(hipe_icode:switch_val_arg(Switch), Info),
  Cases = hipe_icode:switch_val_cases(Switch),
  %%io:format("switch ~p ~p ~n", [Switch, Cases]), 
  {_, Labels} = lists:unzip(Cases),
  {Fail_range, Range_label_list} = get_range_label_list(Switch_range, Cases, Info, [], []),
  Fail_label = hipe_icode:switch_val_fail_label(Switch), 
  {Updated_edges, Info2} = info_struct__save_spec_range_trees(Info, [{Fail_range, Fail_label}|Range_label_list], [Fail_label|Labels]),
  {_, To_labels} = lists:unzip(Updated_edges),
  Info3 = Info2,
  info_struct__add_work(Info3, To_labels).

analyse_sane_if(If, Info, [Range_1, Range_2]) ->
  case hipe_icode:if_op(If) of
    '>' ->
      {True_range_2, True_range_1, False_range_2, False_range_1} = 
	range_inequality_propagation(Range_2, Range_1);
    '==' -> 
      {Temp_true_range_1, Temp_true_range_2, False_range_1, False_range_2}=
	range_equality_propagation(Range_1, Range_2),
      True_range_1 = var_range__set_other(Temp_true_range_1, 
					  var_range__other(Range_1)),
      True_range_2 = var_range__set_other(Temp_true_range_2, 
					  var_range__other(Range_2));
    '<' ->
      {True_range_1, True_range_2, False_range_1, False_range_2} = 
	range_inequality_propagation(Range_1, Range_2);
    '>=' ->
      {False_range_1, False_range_2, True_range_1, True_range_2} =
	range_inequality_propagation(Range_1, Range_2);
    '=<' ->
      {False_range_2, False_range_1, True_range_2, True_range_1} = 
	range_inequality_propagation(Range_2, Range_1);
    '=:=' ->
      {True_range_1, True_range_2, False_range_1, False_range_2}=
	range_equality_propagation(Range_1, Range_2);
    '=/=' ->
      {False_range_1, False_range_2, True_range_1, True_range_2} =
	range_equality_propagation(Range_1, Range_2);
    '/=' -> 
      {Temp_false_range_1, Temp_false_range_2, True_range_1, True_range_2}=
	range_equality_propagation(Range_1, Range_2),
      False_range_1 = var_range__set_other(Temp_false_range_1, 
					   var_range__other(Range_1)),
      False_range_2 = var_range__set_other(Temp_false_range_2, 
					   var_range__other(Range_2))
  end,

  %% io:format("~nrange_1: ~p, ~nrange_2: ~p ~n if op: ~p ~ntrue_1: ~p, ~ntrue_2: ~p, ~nfalse_1: ~p, ~nfalse_2: ~p ~n", [Range_1, Range_2, hipe_icode:if_op(If), True_range_1, True_range_2, False_range_1, False_range_2]),

  True_label = hipe_icode:if_true_label(If),
  False_label = hipe_icode:if_false_label(If),

  Substitute_list = [{True_range_1, True_label}, {True_range_2, True_label}, 
		     {False_range_1, False_label}, {False_range_2, False_label}],
  {Updated_edges, Info2} = info_struct__save_spec_range_trees(Info, Substitute_list, [True_label, False_label]),
  {_, To_labels} = lists:unzip(Updated_edges),
  info_struct__add_work(Info2, To_labels).

analyse_if(If, Info) ->
  %% hipe_icode_pp:pp_block([If]),
  Args = hipe_icode:if_args(If),	
  %% io:format("if args ~p ~n~p ~n", [get_range_from_args(Args, Info), Args]),
  %% io:format("ifop ~p ~n", [hipe_icode:if_op(If)]),
  case get_range_from_args(Args, Info) of
    [] ->	
      Current_label = info_struct__current_label(Info),
      New_work = [hipe_icode:if_true_label(If), hipe_icode:if_false_label(If)],
      lists:foldl(
	fun (Label, InfoAcc) ->
	    {Updated_edges, InfoAcc2} = info_struct__save_current_range_tree({Current_label, Label}, InfoAcc),
	    {_, To_labels} = lists:unzip(Updated_edges),
	    info_struct__add_work(InfoAcc2, To_labels)
	end,
	Info,
	New_work);
    [_Range_1, _Range_2] = Ranges ->
      analyse_sane_if(If, Info, Ranges)
  end.

analyse_goto(Insn, Info) ->
  Goto_label = hipe_icode:goto_label(Insn),
  Current_label = info_struct__current_label(Info),
  {Updated_edges, Info2} = info_struct__save_current_range_tree({Current_label, Goto_label}, Info),
  {_, To_labels} = lists:unzip(Updated_edges),
  info_struct__add_work(Info2, To_labels).

analyse_type(Type, Info) ->
  Type_type = hipe_icode:type_type(Type),
  [First_arg|_] = hipe_icode:type_args(Type),
  Var_name = name_from_icode_var(First_arg),
  Old_var_range = info_struct__get_range(Var_name, Info),
  case Type_type of
    {integer, N} -> 
      True_range = range_cut(Var_name, 
			     [range_init(Var_name, {N, N}, false),
			      Old_var_range]),
      %% False range
      False_int_range = 
	case var_range__range(Old_var_range) of
	  {Min, Max} ->
	    New_small = inf_geq(Min, N),
	    New_large = inf_geq(N, Max),
	    if New_small ->
		{N + 1, Max};
	       New_large ->
		{Min, N - 1};
	       true -> 
		{Min, Max}
	    end;
	  Not_tuple ->
	    Not_tuple
	end,

      False_range = 
	range_init(
	  Var_name,
	  False_int_range, 
	  var_range__other(Old_var_range)
	 );

    integer -> 
      True_range = var_range__set_other(Old_var_range, false),
      False_range = range_init(Var_name,empty,var_range__other(Old_var_range));
    _ -> 
      True_range = var_range__set_other(Old_var_range, true), 
      False_range = Old_var_range
  end,

  True_label = hipe_icode:type_true_label(Type),
  False_label = hipe_icode:type_false_label(Type),
  Updates = [{True_range, True_label}, {False_range, False_label}], 
  Labels = [True_label, False_label],
  {Updated_edges, Info2} = info_struct__save_spec_range_trees(Info, Updates, Labels),
  {_, To_labels} = lists:unzip(Updated_edges),
  info_struct__add_work(Info2, To_labels).


analyse_fail(_Fail, Info) ->
  Current_label = info_struct__current_label(Info),
  {_, Info2} = info_struct__save_current_range_tree({Current_label, return}, Info),
  Info2.


%%---------------------------------------------------------------------------
%% Helper functions for icode instructions
%%---------------------------------------------------------------------------

analyse_other_module_fcall(Key, {M,F,A}, Info, Args) -> 
  Mfa_type = erl_bif_types:type(M,F,A),
  Info2 = 
    case erl_bif_types:arg_types(M,F,A) of
      any -> Info;
      List -> 
	New_arg_ranges = 
	  lists:zipwith(
	    fun (Arg, Arg_type) ->
		Prop_range = get_range_from_arg(Arg, Info),
		Name = name_from_icode_var(Arg),
		Anno_range = get_range_from_annotation(Arg_type, Name),
		range_cut(Name, [Prop_range, Anno_range])
	    end,
	    Args, List),
	lists:foldl(
	  fun (Range, InfoAcc) ->
	      info_struct__insert_range(Range, InfoAcc)
	  end,
	  Info,
	  New_arg_ranges)
    end,
  Return_range = get_range_from_annotation(Mfa_type, Key), 
  info_struct__insert_range(Return_range, Info2).

analyse_fcall(Key, {M,F,A}, Info, Args) ->
  %%io:format("~p fcall ~p ~n", [self(), {M,F,A}]),
  Current_mfa = info_struct__current_mfa(Info), 
  Server = info_struct__server(Info),
  Info2 = 
    if (Current_mfa =:= {M,F,A}) ->
	info_struct__set_is_recursive(true, Info);
       true ->
	Info
    end,
  Server ! {self(), {load, message, {{M,F,A}, return_range}}},
  receive
    none ->
      case Current_mfa of
	{M,F,A} ->
	  Info3 = server__update_call_args({M,F,A}, Args, Info2),
	  {break, Info3};	  
	{M,_,_} ->
	  %%?not_done_debug("need info ~p in ~p ~n", [{M,F,A}, Current_mfa]),
	  Info3 = server__update_call_args({M,F,A}, Args, Info2),
	  Range = range_init(Key, {neginf, posinf}, true),
	  info_struct__insert_range(Range, Info3);
	_ -> 
	  %% doesn't save args info about function in other module
	  analyse_other_module_fcall(Key, {M,F,A}, Info2, Args)
      end;
    {value, {Lookup_range, _Final}} ->
      Info3 = server__update_call_args({M,F,A}, Args, Info2),
      Range = var_range__copy(Lookup_range, Key),
      info_struct__insert_range(Range, Info3)
  end.

analyse_bs_get_integer_funs(Size, Flags, true) ->
  Signed = Flags band 4,
  if Signed =:= 0 ->
      Max = round(math:pow(2, Size)) - 1,
      Min = 0;
     true ->
      Max = posinf,
      Min = neginf
  end,
  {Min, Max};

analyse_bs_get_integer_funs(_Size, _Flags, false) -> {neginf, posinf}.


get_range_from_dst_annotation(Key, Dsts) ->
  GetAnno = fun(Dst) ->
		case hipe_icode:is_annotated_var(Dst) of
		  true ->
		    get_range_from_annotation(
		      hipe_icode:var_annotation(Dst), Key);		  
		  false ->
		    range_init(Key, {neginf, posinf}, true)
		end
	    end,
  case Dsts of
    [undef] ->
      range_init(Key, {neginf, posinf}, true);
    [] -> 
      range_init(Key, {neginf, posinf}, true);
    [Dst|_] -> %todo the others are what??
      GetAnno(Dst);
    Dst ->
      GetAnno(Dst)
  end.

lasy_type(Fun) ->
  ?call_or_enter_debug("fun type ~p ~n", [Fun]),
  basic_type(Fun).

analyse_call_or_enter_fun(Fun, Args, Key, Dsts, Info, CallType) ->
  %% io:format("Fun ~p ~n", [Fun]),
  case lasy_type(Fun) of
    {bin, Operation} ->
      ?call_or_enter_debug("bin", []),
      [Arg_range1|[Arg_range2|[]]] = get_range_from_args(Args, Info),
      A1_is_empty = var_range__is_empty(Arg_range1),
      A2_is_empty = var_range__is_empty(Arg_range2),
      if A1_is_empty or A2_is_empty ->
	  Other = var_range__other(Arg_range1) or var_range__other(Arg_range1),
	  range_init(Key, empty, Other);
	 true ->
	  Operation(Key, Arg_range1, Arg_range2)
      end;
    {unary, Operation} ->
      ?call_or_enter_debug("unary", []),
      [Arg_range] = get_range_from_args(Args, Info),
      case var_range__is_empty(Arg_range) of
	true ->
	  range_init(Key, empty, true);
	false ->
	  Operation(Key, Arg_range)
      end;
    {fcall, {M,F,A}} ->
      ?call_or_enter_debug("fcall", []),
      case CallType of
	local ->
	  case analyse_fcall(Key, {M,F,A}, Info, Args) of
	    Fcallinfo = #info_struct{} ->
	      Fcall_range = info_struct__get_range(Key, Fcallinfo),
	      Annotation_range = get_range_from_dst_annotation(Key, Dsts),
	      New_range = range_cut(Key, [Fcall_range, Annotation_range]),
	      info_struct__insert_range(New_range, Fcallinfo);
	    Other ->
	      Other
	  end;
	remote ->
	  analyse_other_module_fcall(Key, {M,F,A}, Info, Args)
      end;
    not_int ->
      ?call_or_enter_debug("not int", []),
      range_init(Key, empty, true);
    not_analysed -> 
      ?call_or_enter_debug("not analysed", []),
      get_range_from_dst_annotation(Key, Dsts);
    {hipe_bs_primop, {bs_get_integer, Size, Flags}} ->
      ?call_or_enter_debug("bs1", []),
      {Min, Max} = analyse_bs_get_integer_funs(Size, Flags, length(Args) =:= 4),
      range_init(Key, {Min, Max}, false);
    {hipe_bs_primop, {bs_get_integer_2, Size, Flags}} ->
      ?call_or_enter_debug("bs2", []),
      {Min, Max} = analyse_bs_get_integer_funs(Size, Flags, length(Args) =:= 1),					
      range_init(Key, {Min, Max}, false);
    {hipe_bs_primop, _} = Primop ->
      ?call_or_enter_debug("bs3 ~p ~n", [Primop]),
      Type = hipe_icode_primops:type(Primop),
      get_range_from_annotation(Type, Key);
    {hipe_bsi_primop, {bs_get_integer, Size, Flags}} ->
      ?call_or_enter_debug("bs4", []),
      {Min, Max} = analyse_bs_get_integer_funs(Size, Flags, true),
      range_init(Key, {Min, Max}, false)
  end.

%% @doc
%%   basic_type/1 specifies how to analyze a call or enter fun
%% @end

%% Arithmetic operations
basic_type('+') -> {bin, fun(Name, R1, R2) -> range_add(Name, R1, R2) end};
basic_type('-') -> {bin, fun(Name, R1, R2) -> range_sub(Name, R1, R2) end};
basic_type('*') -> {bin, fun(Name, R1, R2) -> range_mult(Name, R1, R2) end};
basic_type('/') -> not_int;
basic_type('div') -> {bin, fun(Name, R1, R2) -> range_div(Name, R1, R2) end};
basic_type('rem') -> {bin, fun(Name, R1, R2) -> range_rem(Name, R1, R2) end};
basic_type('bor') -> {bin, fun(Name, R1, R2) -> range_bor(Name, R1, R2) end};
basic_type('band') -> {bin, fun(Name, R1, R2) -> range_band(Name, R1, R2) end};
basic_type('bxor') -> {bin, fun(Name, R1, R2) -> range_bxor(Name, R1, R2) end};
basic_type('bnot') -> {unary, fun(Name, R1) -> range_bnot(Name, R1) end};
basic_type('bsl') -> {bin, fun(Name, R1, R2) -> range_bsl(Name, R1, R2) end};
basic_type('bsr') -> {bin, fun(Name, R1, R2) -> range_bsr(Name, R1, R2) end};

basic_type('unsafe_bor') ->  
  {bin, fun(Name, R1, R2) -> range_bor(Name, R1, R2) end};
basic_type('unsafe_band') ->
  {bin, fun(Name, R1, R2) -> range_band(Name, R1, R2) end};
basic_type('unsafe_bxor') ->
  {bin, fun(Name, R1, R2) -> range_bxor(Name, R1, R2) end};
basic_type('unsafe_bnot') ->
  {unary, fun(Name, R1) -> range_bnot(Name, R1) end};

basic_type('unsafe_bsl') ->
  {bin, fun(Name, R1, R2) -> range_bsl(Name, R1, R2) end};
basic_type('unsafe_bsr') ->
  {bin, fun(Name, R1, R2) -> range_bsr(Name, R1, R2) end};

basic_type('unsafe_add') ->
  {bin, fun(Name, R1, R2) -> range_add(Name, R1, R2) end};
basic_type('unsafe_sub') ->
  {bin, fun(Name, R1, R2) -> range_sub(Name, R1, R2) end};
basic_type('extra_unsafe_add') ->
  {bin, fun(Name, R1, R2) -> range_add(Name, R1, R2) end};
basic_type('extra_unsafe_sub') ->
  {bin, fun(Name, R1, R2) -> range_sub(Name, R1, R2) end};

%% Binaries
basic_type({hipe_bs_primop, Todo}) -> {hipe_bs_primop, Todo};
basic_type({hipe_bs_primop2, Todo}) -> {hipe_bs_primop, Todo};
basic_type({hipe_bsi_primop,{bs_get_integer, Size, _B, Flags}}) -> 
  {hipe_bsi_primop,{bs_get_integer, Size, Flags}}; 
basic_type({hipe_bsi_primop,bs_get_orig}) -> not_analysed;
basic_type({hipe_bsi_primop,bs_get_orig_offset}) -> not_analysed;
basic_type({hipe_bsi_primop,bs_get_size}) -> not_analysed;
basic_type({hipe_bsi_primop,bs_add}) -> not_analysed;
basic_type({hipe_bsi_primop,bs_div_test}) -> not_analysed;
basic_type({hipe_bsi_primop,bs_size_test_all}) -> not_analysed;
basic_type({hipe_bsi_primop,{bs_get_binary_all,_A,_B}}) -> not_analysed;
basic_type({hipe_bsi_primop,{bs_make_size,_A}}) -> not_analysed;
basic_type({hipe_bsi_primop,bs_size_test}) -> not_analysed;
basic_type({hipe_bsi_primop,{bs_get_binary,_A,_B,_C}}) -> not_analysed;
basic_type({hipe_bsi_primop,{bs_get_binary,_A,_B}}) -> not_analysed;

%% Unknown, other
basic_type(call_fun) -> not_analysed;
basic_type(clear_timeout) -> not_analysed;
basic_type(redtest) -> not_analysed;
basic_type(set_timeout) -> not_analysed;
basic_type(#apply_N{}) -> not_analysed;
basic_type(#closure_element{}) -> not_analysed; 
basic_type(#gc_test{}) -> not_analysed;

%% Message handling
basic_type(check_get_msg) -> not_analysed; 
basic_type(next_msg) -> not_analysed; 
basic_type(select_msg) -> not_analysed; 
basic_type(suspend_msg) -> not_analysed;

%% Functions
basic_type(enter_fun) -> not_analysed;
basic_type(#mkfun{}) -> not_int;
basic_type({M,F,A}) -> {fcall, {M,F,A}}; 

%% Floats
basic_type(conv_to_float) -> not_int;
basic_type(fclearerror) -> not_analysed;
basic_type(fcheckerror) -> not_analysed;
basic_type(fnegate) -> not_int;
basic_type(fp_add) -> not_int;
basic_type(fp_div) -> not_int;
basic_type(fp_mul) -> not_int;
basic_type(fp_sub) -> not_int;
basic_type(unsafe_tag_float) -> not_int;
basic_type(unsafe_untag_float) -> not_int;

%% Lists, tuples, records
basic_type(cons) -> not_int;
basic_type(mktuple) -> not_int;
basic_type(unsafe_hd) -> not_analysed;
basic_type(unsafe_tl) -> not_int;
basic_type(#element{}) -> not_analysed;
basic_type(#unsafe_element{}) -> not_analysed;
basic_type(#unsafe_update_element{}) -> not_analysed.


%% TODO: rename switch_val
get_range_label_list(Range, [], _Info, Range_label_list, []) ->
  {Range, Range_label_list};
get_range_label_list(Range, [], _Info, Range_label_list, Constants_to_cut) ->
  Sorted_constants = lists:sort(Constants_to_cut),
  [Smallest|_] = Sorted_constants,
  Largest = lists:last(Sorted_constants),
  Min_value = var_range__min(Range),
  if Min_value >= Smallest ->
      Sorted_constants_to_cut = Sorted_constants;
     true ->
      Sorted_constants_to_cut = lists:reverse(Sorted_constants)
  end,	
  New_temp_range = 
    lists:foldl(
      fun(Const, Acc_range) ->
	  Const_range = range_init(const, {Const, Const}, false),
	  range_remove_constant(Acc_range, Const_range)
      end,
      Range,
      Sorted_constants_to_cut),
  New_range_range = 
    case var_range__range(New_temp_range) of 
      {Min, Max} ->
	New_small = inf_geq(Smallest, Max),
	New_large = inf_geq(Min, Largest),
	New_max = 
	  if New_small -> Smallest - 1;
	     true-> Max
	  end,
	New_min =
	  if New_large -> Largest + 1;
	     true -> Min
	  end,
	{New_min, New_max};
      Not_tuple ->
	Not_tuple
    end,
  %%io:format("Range ~p ~n", [New_range_range]),
  New_range = range_init(var_range__name(Range), New_range_range, var_range__other(Range)), 
  {New_range, Range_label_list}; 
get_range_label_list(Range, [Case|Cases], Info, Range_label_list, Constants_to_cut) ->
  {Arg, Label} = Case,
  Arg_range = get_range_from_arg(Arg, Info),
  Cut_range = range_cut(var_range__name(Range), [Arg_range, Range]),
  Arg_name = var_range__name(Arg_range),
  Is_const = var_range__is_constant(Arg_range),
  Is_empty_arg = var_range__is_empty(Cut_range),
  New_constants_to_cut =
    if Is_const ->
	{Constant_value, Constant_value} = var_range__range(Arg_range),
	[Constant_value|Constants_to_cut];
       true ->
	Constants_to_cut
    end,
  if not Is_empty_arg ->
      Arg_cut_range = var_range__copy(Cut_range, Arg_name),
      get_range_label_list(Range, Cases, Info, [{Arg_cut_range, Label},{Cut_range, Label}|Range_label_list], New_constants_to_cut);
     true ->
      get_range_label_list(Range, Cases, Info, Range_label_list, New_constants_to_cut)
  end.

%%
%% Icode annotation and pritty print
%%
%% Annotates icode with range information
%%

annotate_IC(IC, Range_info) ->
  Label_list = hipe_icode_cfg:labels(IC),
  lists:foldl(
    fun (Label, ICAcc) ->
	case range_info__is_live(Range_info, Label) of
	  true ->
	    BB = hipe_icode_cfg:bb(ICAcc, Label),
	    Code = hipe_bb:code(BB),
	    NewCode = annotate_BB(Code, Range_info, Label),
	    NewBB = hipe_bb:code_update(BB, NewCode),
	    hipe_icode_cfg:bb_add(ICAcc, Label, NewBB);
	  false ->
	    ICAcc
	end
    end,
    IC,
    Label_list).

annotate_BB(Insts, Range_info, Label) ->
  lists:map(fun (Inst) -> annotate_insn(Inst, Range_info, Label) end, Insts).

annotate_insn(Insn, Range_info, Current_label) ->
  Def = defines(Insn),
  Use = uses(Insn),
  Fun = fun (X, Y) -> hipe_icode:annotate_var(X, Y) end,
  Lookup_def = fun (Info, Label, Var) ->
		   range_info__def_range(Info, Label, name_from_icode_var(Var))
	       end,
  Lookup_use = fun (Info, Label, Var) ->
		   range_info__use_range(Info, Label, name_from_icode_var(Var))
	       end,
  Old_anno = fun (X) -> case hipe_icode:is_annotated_var(X) of
			  true ->
			    hipe_icode:var_annotation(X);
			  false ->
			    any
			end
	     end,
  DefSubst = [{X, Fun(X, {Old_anno(X), Lookup_def(Range_info, Current_label, X)})} || X <- Def],
  UseSubst = [{X, Fun(X, {Old_anno(X), Lookup_use(Range_info, Current_label, X)})}|| X <- Use],
  case DefSubst ++ UseSubst of
    [] ->
      Insn;
    Subst ->
      hipe_icode:subst(Subst, Insn)
  end.

pp_args([]) -> []; %%lists_map ??
pp_args([Param]) -> to_string(Param);
pp_args([Param|Params]) ->
  to_string(Param) ++ ", " ++ pp_args(Params).	

pp_function_info(Info, File) ->
  Return_range = return_range(Info),
  Range = var_range__range(Return_range),
  Params = input_range(Info),
  R_other = 
    case var_range__other(Return_range) of
      true -> "may return other";
      false -> "only integer return values"
    end,	
  {M, F, _A} = info_struct__current_mfa(Info),
  ParamsString = lists:flatten(pp_args(Params)),
  io:format(File, "~p:~p/(~s) -> ~p, ~s ~n~n",
	    [M, F, ParamsString, Range, R_other]).

-ifdef(DEBUG).
pp_range_tree(Info) ->
  List = gb_trees:to_list(info_struct__range_tree(Info)),
  io:format("Range List, Label ~p: ~n", [info_struct__current_label(Info)]),
  pp_range_list(List).
-endif.

-ifdef(DEBUG).
pp_range_list(List) ->
  lists:foreach(fun(R) -> pp_range(R) end, List).
-endif.

-ifdef(DEBUG).
pp_range(Range) -> 
  Int_range = var_range__range(Range),
  Other = var_range__other(Range),
  Name = var_range__name(Range),
  io:format("~w, ~w ~w ~n", [Name, Int_range, Other]).
-endif.

print_icode_to_file(IC, Info) ->
  {M,F,A} = info_struct__current_mfa(Info),
  FileName = atom_to_list(M) ++ "_" ++ atom_to_list(F) ++ "_" ++ integer_to_list(A) ++ ".anno",
  %% io:format("filename ~p ~n", [FileName]),
  case file:open(FileName, write) of
    {ok, File} ->
      pp_function_info(Info, File),
      hipe_icode_pp:pp(File, hipe_icode_cfg:cfg_to_linear(IC)),
      file:close(File);
    {error, _Reason} ->
      %% probably a fun
      %% todo
      ok
  end.

%%
%% Icode specialization
%%
%% Modifies calls to arithmetic operations
%%

specialize_IC(IC, Range_info) ->
  Label_list = hipe_icode_cfg:labels(IC),
  lists:foldl(
    fun (Label, ICAcc) ->
	%% hipe_icode_pp:pp_block(Code),
	case range_info__is_live(Range_info, Label) of
	  true ->
	    BB = hipe_icode_cfg:bb(ICAcc, Label),
	    Code = hipe_bb:code(BB),
	    NewCode = specialize_BB(Code, Range_info, Label),
	    NewBB = hipe_bb:code_update(BB, NewCode),
	    hipe_icode_cfg:bb_add(ICAcc, Label, NewBB);
	  false ->
	    Warn = range_info__warn(Range_info),
	    warning("Dead code", Label, Warn),
	    ICAcc
	end
    end,
    IC,
    Label_list).

specialize_BB(Insts, Range_info, Label) ->
  lists:map(fun (Insn) -> specialize_insn(Insn, Range_info, Label) end, Insts).

specialize_insn(Insn, Range_info, Label) ->
  %% hipe_icode_pp:pp_block([Insn]),
  New_insn = 
    case Insn of
      #call{} ->
	specialize_call(Insn, Range_info, Label);
      #enter{} ->
	specialize_enter(Insn, Range_info, Label);
      #'if'{} ->
	specialize_if(Insn, Range_info, Label);
      Other ->
	Other
    end,
  New_insn.

%% specialized_op(op, specialization)
specialized_op('+', safe, _) -> '+';
specialized_op('+', unsafe, _) -> 'unsafe_add';
specialized_op('+', extra_unsafe, _) -> 'extra_unsafe_add';

specialized_op('-', safe, _) -> '-';
specialized_op('-', unsafe, _) -> 'unsafe_sub'; 
specialized_op('-', extra_unsafe, _) -> 'extra_unsafe_sub'; 

specialized_op('*', safe, _) -> '*';
specialized_op('*', unsafe, _) -> '*';
specialized_op('*', extra_unsafe, Warn) -> 
  warning('could be extra_unsafe_mult', '*', Warn),
  '*';

specialized_op('band', safe, _) -> 'band';
specialized_op('band', unsafe, _) ->
  io:format("band(Fix, Fix) -> Big ~n"), exit(1);
specialized_op('band', extra_unsafe, _) -> 'unsafe_band';

specialized_op('bor', safe, _) -> 'bor';
specialized_op('bor', unsafe, _) ->
  io:format("bor(Fix, Fix) -> Big ~n"), exit(1);
specialized_op('bor', extra_unsafe, _) -> 'unsafe_bor';

specialized_op('bxor', safe, _) -> 'bxor';
specialized_op('bxor', unsafe, _) ->
  io:format("bxor(Fix, Fix) -> Big ~n"), exit(1);
specialized_op('bxor', extra_unsafe, _) -> 'unsafe_bxor';

specialized_op('bnot', safe, _) -> 'bnot';
specialized_op('bnot', unsafe, _) -> 'bnot'; %% TODO: correct?
specialized_op('bnot', extra_unsafe, _) -> 'unsafe_bnot';

specialized_op('bsl', safe, _) -> 'bsl';
specialized_op('bsl', unsafe, _) -> 'bsl';
specialized_op('bsl', extra_unsafe, _) -> 'unsafe_bsl';

specialized_op('bsr', safe, _) -> 'bsr';
specialized_op('bsr', unsafe, _) -> 'bsr';
specialized_op('bsr', extra_unsafe, _) -> 'unsafe_bsr';

specialized_op('unsafe_add', safe, Warn) -> 
  warning('unsafe_add', safe, Warn), 'unsafe_add';
specialized_op('unsafe_add', unsafe, _) -> 'unsafe_add';
specialized_op('unsafe_add', extra_unsafe, _) -> 'extra_unsafe_add';

specialized_op('unsafe_sub', safe, Warn) ->
  warning('unsafe_sub', safe, Warn), 'unsafe_sub';
specialized_op('unsafe_sub', unsafe, _) -> 'unsafe_sub';
specialized_op('unsafe_sub', extra_unsafe, _) -> 'extra_unsafe_sub';

%% TODO: remove when not needed
specialized_op('extra_unsafe_add', safe, Warn) ->
  warning('extra_unsafe_add', safe, Warn), 'extra_unsafe_add';
specialized_op('extra_unsafe_add', unsafe, Warn) ->
  warning('extra_unsafe_add', unsafe, Warn), 'extra_unsafe_add';
specialized_op('extra_unsafe_add', extra_unsafe, _) -> 'extra_unsafe_add';

specialized_op(Op, _, _) -> Op.


specialized_if_op('<', true) -> 'fixnum_lt';
specialized_if_op('=<', true) -> 'fixnum_le';
specialized_if_op('>', true) -> 'fixnum_gt';
specialized_if_op('>=', true) -> 'fixnum_ge';
specialized_if_op('=:=', true) -> 'fixnum_eq';
specialized_if_op('=/=', true) -> 'fixnum_neq';
specialized_if_op(Op, _) -> Op.

%% this is not sane
make_const({reg, N}, Range_info, Label) ->
  range_info__def_range(Range_info, Label, {reg, N}); 
make_const({const, {flat, N}}, _, _) when is_integer(N) -> 
  range_init(const, {N, N}, false);
make_const({const, _}, _, _) -> range_init(const, empty, true);
make_const({fvar, _}, _, _) -> range_init(const, empty, true).

specialize_if(If, Range_info, Label) ->
  IfOp = hipe_icode:if_op(If),
  Use_ranges = lists:map(fun (Var) ->
			     range_info__use_range(Range_info, Label,
						   name_from_icode_var(Var))
			 end, uses(If)),
  Use_constants = lists:map(fun (Const) ->
				make_const(Const, Range_info, Label)
			    end, consts(If)), 
  RangeFun = fun(Range) -> specialize_is_fixnum(Range) end,
  New_if_op = specialized_if_op(IfOp, 
				lists:all(RangeFun, Use_ranges ++ Use_constants)),
  hipe_icode:if_op_update(If, New_if_op).



specialize_call(Call, Range_info, Label) ->
  Fun = hipe_icode:call_fun(Call),
  New_fun = specialize_call_or_enter(Fun, Call, Range_info, Label),
  hipe_icode:call_fun_update(Call, New_fun).

specialize_enter(Enter, Range_info, Label) ->
  Fun = hipe_icode:enter_fun(Enter),
  New_fun = specialize_call_or_enter(Fun, Enter, Range_info, Label),
  hipe_icode:enter_fun_update(Enter, New_fun).

%%-define(ALL_FIXNUMS, true).
-ifdef(ALL_FIXNUMS).
specialize_is_fixnum(Range) -> not var_range__other(Range).
-else.
specialize_is_fixnum(Range) -> var_range__is_fixnum(Range).
-endif.

specialize_is_bitwidth(Range) -> var_range__is_bitwidth(Range).

use_range([], _, _) -> [];
use_range([Use|UseTail], Info, Label) ->
  NewRange = 
    case hipe_icode:is_const(Use) of
      true  -> make_const(Use, Info, Label);
      false -> range_info__use_range(Info, Label, name_from_icode_var(Use))
    end,
  [NewRange|use_range(UseTail, Info, Label)].

all_safe(Op, Args, [Res]) ->
  specialize_is_fixnum(Res) and args_safe(Op, Args).

args_safe('bsl', [Left,Right]) -> 
  specialize_is_fixnum(Left) and specialize_is_bitwidth(Right);
args_safe('bsr', [Left,Right]) -> 
  specialize_is_fixnum(Left) and specialize_is_bitwidth(Right);
args_safe('bnot', [Arg]) -> 
  specialize_is_fixnum(Arg);
args_safe(_, [Left,Right]) -> 
  specialize_is_fixnum(Left) and specialize_is_fixnum(Right).


get_safety_class(Operation, Use_ranges, []) ->
  case args_safe(Operation, Use_ranges) of
    true  -> unsafe;
    false -> safe
  end;
get_safety_class(Operation, Use_ranges, Def_ranges) ->
  case all_safe(Operation, Use_ranges, Def_ranges) of
    true  -> extra_unsafe;
    false -> case args_safe(Operation, Use_ranges) of
	       true  -> unsafe;
	       false -> safe
	     end
  end.


is_arith_op('+') -> true;
is_arith_op('-') -> true;
is_arith_op('*') -> true;
is_arith_op('band') -> true;
is_arith_op('bor') -> true;
is_arith_op('bxor') -> true;
is_arith_op('bnot') -> true;
is_arith_op('bsl') -> true;
is_arith_op('bsr') -> true;
is_arith_op('unsafe_add') -> true;
is_arith_op('unsafe_sub') -> true;
is_arith_op('extra_unsafe_add') -> true;
is_arith_op(_) -> false.


%% TODO: don't do unnecessary work
specialize_call_or_enter(Fun, Operation, Range_info, Label) ->
  Warn = range_info__warn(Range_info),
  Use = hipe_icode:args(Operation),
  %%  io:format("Op ~p ~nUse ~p ~n", [Operation, Use]),
  Use_ranges = use_range(Use, Range_info, Label),
  Def_ranges = 
    case hipe_icode:is_enter(Operation) of
      true ->
	[range_info__def_range(Range_info, Label, enter_return)];
      false ->
	lists:map(fun (Var) ->
		      range_info__def_range(Range_info, Label,
					    name_from_icode_var(Var))
		  end, defines(Operation))
    end,
  case is_arith_op(Fun) of
    true  -> Safety_class = get_safety_class(Fun, Use_ranges, Def_ranges),
	     New_fun = specialized_op(Fun, Safety_class, Warn),
	     if New_fun =/= Fun -> info(New_fun, Fun, Warn);
		true -> ok
	     end,
	     New_fun;
    false -> Fun
  end.


%%
%% Info struct
%%
%% Functionallity for the info struct
%%

%%---------------------------------------------------------------------------
%% Init
%%---------------------------------------------------------------------------

info_struct__init(IC, Server, Options, Current_mfa = {_M, F, A}) ->
  Exports = proplists:get_value(exports, Options),
  Current_fa = {F,A},
  Is_exported = lists:member(Current_fa, Exports),
  StartLabel = hipe_icode_cfg:start_label(IC),
  Params = hipe_icode_cfg:params(IC),
  Liveness = hipe_icode_liveness:analyze(hipe_icode_type:unannotate_cfg(IC)), %todo
  Pred_map = hipe_icode_cfg:pred_map(IC),
  Is_closure = hipe_icode_cfg:is_closure(IC),
  Warn = proplists:get_bool(icode_range_analysis_warn, Options),
  %% io:format("~p analysing ~p ~n", [self(), Current_mfa]),
  Info = #info_struct{current_mfa=Current_mfa,
		      startlabel = StartLabel,
		      predmap = Pred_map,
		      liveness = Liveness,
		      worklist = init_work(),
		      server = Server,
		      warn = Warn
		     },
  Server ! {self(), {load, message, {Current_mfa, args}}},
  Arg_ranges = 
    receive
      none ->
	[];
      %% only if exported
      {value, {Lookup_args, _Lookup_state}} ->
	Lookup_args
    end,
  case info_struct__add_params(Params, Info, Arg_ranges, Is_exported, Is_closure) of
    break ->
      break;
    Info2 ->
      info_struct__add_work(Info2, [info_struct__startlabel(Info2)])
  end.

%%---------------------------------------------------------------------------
%% Set/Get functions
%%---------------------------------------------------------------------------

info_struct__get_range_tree(Tree_name, Info) ->
  case gb_trees:lookup(Tree_name, info_struct__range_trees(Info)) of
    {value, Tree} ->
      Tree;
    none ->
      gb_trees:empty()
  end.

%%info_struct__add_wideningvalue(_Value, Info) ->
%%  Wideningvalues = info_struct__wideningvalues(Info),
%%  New_wideningvalues = insert_ordered(Wideningvalues, Value),
%%  Info#info_struct{wideningvalues=New_wideningvalues}.
%%  Info.

info_struct__wideningvalues(#info_struct{wideningvalues = Values}) -> Values.

info_struct__insert_range(Var_range,Info) ->
  Key = var_range__name(Var_range),		
  Tree = info_struct__range_tree(Info),
  Info#info_struct{current_range_tree=gb_trees:enter(Key, Var_range, Tree)}.

info_struct__live_labels(#info_struct{live_labels=Labels}) -> Labels.

info_struct__server(#info_struct{server=Server}) -> Server.

info_struct__live_vars(#info_struct{liveness=Liveness}, Label) -> 
  hipe_icode_liveness:livein(Liveness, Label).

info_struct__increment_label_counter(Info = #info_struct{label_counters = Counters}, Label) ->
  case gb_trees:lookup(Label, Counters) of
    none ->
      NewCounter = 1;
    {value, Counter} ->
      NewCounter = Counter + 1
  end,
  New_label_counter_tree = gb_trees:enter(Label, NewCounter, Counters),
  Info#info_struct{label_counters = New_label_counter_tree}.

info_struct__counter_from_label(#info_struct{label_counters = Counters}, Label) ->
  case gb_trees:lookup(Label, Counters) of
    none ->
      0;
    {value, Counter} ->
      Counter
  end.

info_struct__get_phi_range(Dst_name, #info_struct{phi_values=Tree}) ->
  case gb_trees:lookup(Dst_name, Tree) of
    none ->
      range_init(Dst_name, empty, false);
    {value, Range} ->
      Range
  end.

info_struct__set_phi_range(Range, #info_struct{phi_values=Tree} = Info) ->
  New_tree = gb_trees:enter(var_range__name(Range), Range, Tree),
  Info#info_struct{phi_values=New_tree}.

info_struct__current_mfa(#info_struct{current_mfa=Mfa}) -> Mfa.

info_struct__set_current_mfa_not_done(Info, Bool) ->
  Info#info_struct{current_mfa_not_done=Bool}.

info_struct__current_mfa_not_done(#info_struct{current_mfa_not_done=NotDone, 
					   current_mfa = MFA,
					   server = Server,
					   is_recursive = Recursive} = Info) ->
  NotDone and 
    if Recursive ->
	Server ! {self(), {load, message, {MFA, return_range}}},
	Old_return = 
	  receive
	    none ->
	      range_init(return_range, empty, false);
	    {value, {Lookup_range, _Final}} ->
	      Lookup_range
	  end,
	not var_range__is_equal(return_range(Info), Old_return);
       true ->
	true
    end.

info_struct__predmap(#info_struct{predmap=Pred}) -> Pred.

info_struct__range_tree(#info_struct{current_range_tree=Tree}) -> Tree.

info_struct__range_trees(#info_struct{range_trees=Tree}) -> Tree.

info_struct__return_vars(#info_struct{return_vars=Vars}) -> Vars.

info_struct__startlabel(#info_struct{startlabel=StartLabel}) -> StartLabel.

info_struct__worklist(#info_struct{worklist=List}) -> List. 

info_struct__add_live_labels(Info, Label_list) ->
  lists:foldl(fun(Label, InfoAcc) ->
		  Labels = info_struct__live_labels(InfoAcc),
		  Info#info_struct{live_labels = gb_sets:add(Label, Labels)}
	      end,
	      Info,
	      Label_list).

info_struct__add_work(Info, NewWork) ->
  %% io:format("NewWork ~p ~n", [NewWork]),
  Worklist = info_struct__worklist(Info),
  New_worklist = add_work(Worklist, NewWork),
  Info2 = info_struct__add_live_labels(Info, NewWork),
  Info2#info_struct{worklist = New_worklist}.

info_struct__current_label(#info_struct{current_label=Label}) -> Label.

info_struct__add_params([], Info, _, _, _) -> 
  StartLabel = info_struct__startlabel(Info),
  {_, Info2} = info_struct__save_current_range_tree({params, StartLabel}, Info),
  Info2;
info_struct__add_params([Param|ParamList], Info, Arg_range_list,
			Is_exported, Is_closure) ->
  Key = name_from_icode_var(Param),
  {List, Range_info} =
    if Is_exported or Is_closure ->
	{[], range_init(Key, {neginf, posinf}, true)};
       true ->
	case Arg_range_list of
	  [Arg_range|Arg_range_tail] -> %% var_range__copy ??
	    Int_range = var_range__range(Arg_range),
	    Other = var_range__other(Arg_range),
	    New_arg_range = range_init(Key, Int_range, Other),
	    {Arg_range_tail, New_arg_range};
	  [] ->
	    %%?not_done_debug("In add params ~n", []),
	    {[], break}
	end
    end,
  case Range_info of
    break ->
      break;
    Range ->
      %% io:format("MFA: ~p arg: ~p ~n", [info_struct__current_mfa(Info), New_range]), 
      %% io:format("Param ~p range: ~p ~n", [Param, New_range]),
      NewInfo = info_struct__insert_range(Range, Info),
      info_struct__add_params(ParamList, NewInfo, List, Is_exported, Is_closure)
  end.

info_struct__add_return_var(Variable, Info) -> 
  Return_list = info_struct__return_vars(Info),
  Label = info_struct__current_label(Info),
  Key = {Variable, Label},
  case lists:member(Key, Return_list) of
    true ->	
      Info;
    false ->
      Info#info_struct{return_vars = [Key|Return_list]}
  end.

info_struct__get_range(Var, Info) ->
  case gb_trees:lookup(Var, info_struct__range_tree(Info)) of
    none ->
      range_init(Var, empty, false);
    {_, Range} ->
      Range
  end.

%%info_struct__is_recursive(#info_struct{is_recursive = Bool}) -> Bool.

info_struct__set_is_recursive(Bool, Info) -> 
  Info#info_struct{is_recursive = Bool}.

info_struct__warn(#info_struct{warn = Warn}) ->
  Warn.

%%---------------------------------------------------------------------------
%% Loading / Savinging range trees
%%---------------------------------------------------------------------------

info_struct__set_new_current_tree(Label, Info) ->
  New_range_tree = 
    case info_struct__pred_tree(Label, Info) of
      none ->
	[];
      {value, Tree} ->
	Tree
    end,
  Range_list = gb_trees:values(New_range_tree),
  No_not_set = lists:all(
		 fun (Range) ->
		     not (var_range__is_empty(Range) andalso
			  var_range__is_not_other(Range))
		 end,
		 Range_list),
  case No_not_set of
    true ->
      Counter = info_struct__counter_from_label(Info, Label),
      %% io:format("Counter ~p, Label ~p~n", [Counter, Label]),
      if Counter > ?LABELITERATIONS ->
	  MFA = info_struct__current_mfa(Info),
	  Label = info_struct__current_label(Info),
	  Warn = info_struct__warn(Info),
	  warning('taking forever', {MFA, Label}, Warn);
	 true ->
	  ok
      end,
      Info#info_struct{current_range_tree = New_range_tree};
    false ->
      %%?not_done_debug("set new current tree ~n", []),
      break
  end.

%% TODO: byt updatelist till updateset ??
info_struct__save_spec_range_tree(Info, {Range, To_label}, Update_list) ->
  %% io:format("Spec_tree Update_list ~p ~n", [Update_list]),
  Current_label = info_struct__current_label(Info),
  Edge = {Current_label, To_label},
  Range_name = var_range__name(Range),
  %% io:format("Range_name, Edge: ~p, ~p ~n", [Range_name, Edge]),
  case lists:keysearch({Range_name, Edge}, 1, Update_list) of
    {value, {_, Old_range}} -> 
      Insert_range = range_union(Range_name, [Old_range, Range]),
      lists:keyreplace({Range_name, Edge}, 1, Update_list, {{Range_name, Edge}, Insert_range});
    false ->
      [{{Range_name, Edge}, Range}|Update_list]
  end.


%%
%% Update_list is a list of ranges that has been updated on 
%%
%%
save_other_ranges(Info, _Update_list, [], Updated_edges) -> 
  {Updated_edges, Info};
save_other_ranges(Info, Update_list, [Label|Labels], Updated_edges_list) ->
  Current_range_tree = info_struct__range_tree(Info),
  %% io:format("Update_list ~p ~n", [Update_list]),
  New_range_tree = 
    lists:foldl(
      fun({{Range_name, {_From_label, To_label}}, Range}, Tree) ->
	  if (To_label =:= Label)  ->
	      gb_trees:enter(Range_name, Range, Tree); %%Todo union?
	     true ->
	      Tree
	  end
      end,
      Current_range_tree,
      Update_list),
  {Updated_edges, NewInfo} = info_struct__save_range_tree({info_struct__current_label(Info), Label}, New_range_tree, Info),
  save_other_ranges(NewInfo, Update_list, Labels, 
		    Updated_edges_list ++ Updated_edges).

info_struct__save_spec_range_trees(Info, List, Labels) ->
  info_struct__save_spec_range_trees(Info, List, [], Labels).

info_struct__save_spec_range_trees(Info, [], Update_list, Labels) -> 
  save_other_ranges(Info, Update_list, Labels, []);
info_struct__save_spec_range_trees(Info, [{Range, Label}|List], Update_list, Labels) ->
  New_update_list = info_struct__save_spec_range_tree(Info, {Range, Label}, Update_list),
  info_struct__save_spec_range_trees(Info, List, New_update_list, Labels).

info_struct__save_trees_on_edges([], Info, Updated_edges_list) -> {Updated_edges_list, Info};
info_struct__save_trees_on_edges([To_label|To_label_list], Info, Updated_edges_list) ->
  Current_label = info_struct__current_label(Info), 
  {Updated_edges, NewInfo} = info_struct__save_current_range_tree({Current_label, To_label}, Info),
  %% io:format("Updated_edges ~p ~n", [Updated_edges]),
  info_struct__save_trees_on_edges(To_label_list, NewInfo, Updated_edges ++ Updated_edges_list).

live_ranges({out_tree, _}, Range_tree, _) -> Range_tree;
live_ranges({_, return}, Range_tree, _) -> Range_tree;
live_ranges({_From_label, To_label}, Range_tree, Info) ->
  Livelist = info_struct__live_vars(Info, To_label),
  Liveset = gb_sets:from_list(Livelist), %%not linear search todo performance?
  Range_list = gb_trees:to_list(Range_tree),
  Live_range_list  =
    lists:filter(
      fun({Name, _Range}) -> 
	  gb_sets:is_element({var, Name}, Liveset)
      end,
      Range_list),
  gb_trees:from_orddict(Live_range_list).

info_struct__pred_tree(Label, #info_struct{pred_trees = Trees}) ->
  gb_trees:lookup(Label, Trees).

info_struct__set_pred_tree(Label, Pred_tree, #info_struct{pred_trees = Trees} = Info) ->
  Info#info_struct{pred_trees = gb_trees:enter(Label, Pred_tree, Trees)}.

info_struct__save_current_range_tree(Edge, Info) ->
  Current_range_tree = info_struct__range_tree(Info),	
  info_struct__save_range_tree(Edge, Current_range_tree, Info).

info_struct__save_range_tree(Edge, Insert_tree, Info) ->
  {_, To_label} = Edge,	
  Live_tree = live_ranges(Edge, Insert_tree, Info),
  {Update_value, New_range_tree, New_pred_tree} = 
    case info_struct__pred_tree(To_label, Info) of
      none ->
	{true, Live_tree, Live_tree};
      {value, Old_pred_tree} ->
	Union_pred_tree = range_tree_union([Old_pred_tree, 
					    Live_tree]),
	{range_tree_is_different(Old_pred_tree, Union_pred_tree), Insert_tree, Union_pred_tree}
    end,

  Final_range_trees = gb_trees:enter(Edge, New_range_tree, info_struct__range_trees(Info)),
  Info2 = Info#info_struct{range_trees = Final_range_trees},
  if Update_value =:= true ->
      {[Edge], info_struct__set_pred_tree(To_label, New_pred_tree, Info2)};
     true ->
      {[], Info2}
  end.

%%
%% Helper functions
%%

%% Range tree

range_tree_is_different(Range_tree1, Range_tree2) ->
  case gb_trees:size(Range_tree1) =:= gb_trees:size(Range_tree2) of
    true ->
      Range_list1 = gb_trees:values(Range_tree1),
      Range_list2 = gb_trees:values(Range_tree2),
      Range_tuple_list = lists:zip(Range_list1, Range_list2),
      lists:any(
	fun({Range1, Range2}) -> not var_range__is_equal(Range1, Range2) end, 
	Range_tuple_list
       );
    false ->
      true
  end.

get_edge_tree_list(PredList, Label, Version, Info) ->
  lists:map(
    fun(Pred) ->
	Search_pattern = 
	  if Version =:= old ->
	      {old, {Pred, Label}};
	     true ->
	      {Pred, Label}
	  end,
	info_struct__get_range_tree(Search_pattern, Info)
    end,
    PredList).

info_struct__merge_pred_range_trees(Label, Version, Info) ->
  StartLabel = info_struct__startlabel(Info),
  Predmap = info_struct__predmap(Info),
  %% io:format("PredMap: ~p, Label: ~p,", [Predmap, Label]),
  Pred = hipe_icode_cfg:pred(Predmap, Label),
  Pred_range_trees = get_edge_tree_list(Pred, Label, Version, Info),
  if Label =:= StartLabel ->
      [Params] = get_edge_tree_list([params], Label, Version, Info),
      New_pred_range_trees = [Params|Pred_range_trees];
     true ->
      New_pred_range_trees = Pred_range_trees
  end,
  range_tree_union(New_pred_range_trees).

range_tree_union([]) -> gb_trees:empty();
range_tree_union([Range_tree]) -> 
  Range_tree;
range_tree_union([Range_tree|Range_tree_list]) ->
  Range_List = gb_trees:to_list(Range_tree),
  range_tree_range_list_union(Range_List, range_tree_union(Range_tree_list)).

range_tree_range_list_union([], Tree) -> Tree;
range_tree_range_list_union([{Name, Range}|Range_list], Tree) ->
  case gb_trees:lookup(Name, Tree) of
    none ->
      NewTree = gb_trees:insert(Name, Range, Tree);
    {_Name2, Range2} ->
      NewRange = range_union(Name, [Range,Range2]),
      NewTree = gb_trees:update(Name, NewRange, Tree)
  end,
  range_tree_range_list_union(Range_list, NewTree).

in_range_trees_from_info_struct_1(_Info, [], Label_range_tree) ->
  Label_range_tree;
in_range_trees_from_info_struct_1(Info, [Label|Labels], Label_range_tree) ->
  Range_tree = info_struct__merge_pred_range_trees(Label, new, Info),
  %% io:format("Label ~p, ~nIn_Range_tree ~p ~n", [Label, gb_trees:values(Range_tree)]),
  New_label_range_tree = gb_trees:insert(Label, Range_tree, Label_range_tree),
  in_range_trees_from_info_struct_1(Info, Labels, New_label_range_tree).

in_range_trees_from_info_struct(Info, Live_labels) ->
  Live_labels_list = gb_sets:to_list(Live_labels),
  in_range_trees_from_info_struct_1(Info, Live_labels_list, gb_trees:empty()).

out_range_trees_from_info_struct_1(_Info, [], Label_range_tree) ->
  Label_range_tree;
out_range_trees_from_info_struct_1(Info, [Label|Labels], Label_range_tree) ->
  Range_tree = info_struct__get_range_tree({out_tree, Label}, Info),
  New_label_range_tree = gb_trees:insert(Label, Range_tree, Label_range_tree),
  out_range_trees_from_info_struct_1(Info, Labels, New_label_range_tree).

out_range_trees_from_info_struct(Info, Live_labels) ->
  Live_labels_list = gb_sets:to_list(Live_labels),
  out_range_trees_from_info_struct_1(Info, Live_labels_list, gb_trees:empty()).

%%-----------------------------------------------------------------------------
%% Worklist
%%-----------------------------------------------------------------------------

init_work() ->
  {[], [], gb_sets:empty()}.

get_work({[Label|Left], List, Set}) ->
  NewWork = {Left, List, gb_sets:delete(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set},[Label|Left])->
  case gb_sets:is_member(Label, Set) of
    true ->
      add_work(Work, Left);
    false ->
      %% io:format("Adding work: ~w\n", [Label]),
      add_work({List1, [Label|List2], gb_sets:insert(Label, Set)}, Left)
  end;
add_work(Work, []) ->
  Work.

info_struct__get_work(Info) ->
  Worklist = info_struct__worklist(Info),
  %% io:format("Worklist ~p ~n", [Worklist]),
  case get_work(Worklist) of 
    fixpoint ->
      {none, Info};
    {Label, New_worklist} ->
      NewWork = {value, Label},
      Info2 = Info#info_struct{current_label=Label},
      Work = New_worklist,
      Info3 = info_struct__increment_label_counter(Info2, Label),
      {NewWork, Info3#info_struct{worklist=Work}}
  end.


%% Other

input_range(Info) ->
  StartLabel = info_struct__startlabel(Info),
  Params = info_struct__get_range_tree({params, StartLabel}, Info),
  gb_trees:values(Params).

return_range(Info) ->
  Return_info = info_struct__return_vars(Info),
  {Return_variables, Return_labels} = lists:unzip(Return_info),
  Trees = get_edge_tree_list(Return_labels, return, new, Info), 
  Return_ranges = return_ranges(Return_variables, Trees, Info),
  range_union(return_range, Return_ranges).	

return_ranges([], [], _) -> []; %% lists:map ??
return_ranges([Variable|VariableList], [Tree|TreeList], Info) ->
  Range = gb_trees:get(Variable, Tree),
  [Range | return_ranges(VariableList, TreeList, Info)].


%%
%% Range struct
%%
%% Functionallity for the range struct
%%

-ifdef(DEBUG).
var_range__is_correct(#var_range{range=empty}) -> true;
var_range__is_correct(#var_range{range={Min, Max}}) when (Min =/= empty),
							 (Max =/= empty) ->
  true.
-endif.

%% is_max(posinf) -> true;
%% is_max(N) when is_integer(N) -> true.

%% is_min(neginf) -> true;
%% is_min(N) when is_integer(N) -> true.


range_init_1(Name, empty) ->
  #var_range{var_name = Name, range=empty};
range_init_1(Name, {Min, Max}) -> 
  IsNotEmpty = inf_geq(Max, Min),
  if not IsNotEmpty ->
      #var_range{var_name=Name, range=empty};
     (Min =/= empty) and (Max =/= empty) ->
      #var_range{var_name=Name, range={Min, Max}}
  end.

range_init(Name, empty, Other) -> 
  Var_range = range_init_1(Name, empty),
  Var_range#var_range{other = Other};
range_init(Name, {Min, Max} = Range, Other) when (Min =/= empty),
						 (Max =/= empty) ->
  Var_range = range_init_1(Name, Range),
  Var_range#var_range{other = Other}.

var_range__other(#var_range{other = Other}) -> Other.
var_range__name(#var_range{var_name=Name}) -> Name.

var_range__is_empty(#var_range{range=empty}) -> true;
var_range__is_empty(#var_range{range={Min, Max}})  
  when (Min =/= empty), (Max =/= empty) -> false. %% force tuple

var_range__is_not_other(#var_range{other=Bool}) -> not Bool.

var_range__range(#var_range{range=Range}) -> Range.

var_range__set_other(Range, Other) ->
  Range#var_range{other = Other}.

var_range__max(#var_range{range=empty}) -> empty;
var_range__max(#var_range{range={_, Max}}) when Max =/= empty -> Max.

var_range__min(#var_range{range=empty}) -> empty;
var_range__min(#var_range{range={Min, _}}) when Min =/= empty -> Min.

var_range__is_equal(#var_range{range=empty, other=Bool},
		    #var_range{range=empty, other=Bool}) -> true;
var_range__is_equal(#var_range{range={Min, Max}, other=Bool},
		    #var_range{range={Min, Max}, other=Bool}) -> true;
var_range__is_equal(#var_range{}, #var_range{}) -> false.

var_range__copy(Range, Name) ->
  Range#var_range{var_name = Name}.

var_range__is_constant(#var_range{range={Min,Max}}) 
  when (Min =/= empty), (Max =/= empty) -> Min =:= Max;
var_range__is_constant(#var_range{range=empty}) -> false.

to_string(#var_range{range = empty, other=false}) ->
  "no posible value";
to_string(#var_range{range = empty, other=_Other}) ->
  "[empty]"; %, other: " ++ atom_to_list(Other);
to_string(Range) ->
  Min = var_range__min(Range),
  Max = var_range__max(Range),
  if is_atom(Min) ->
      P_Min = atom_to_list(Min);
     true ->
      P_Min = integer_to_list(Min)
  end,
  if is_atom(Max) ->
      P_Max = atom_to_list(Max);
     true ->
      P_Max = integer_to_list(Max)
  end,
  "[" ++ P_Min ++ ", " ++ P_Max ++ "]". 
%% "[" ++ P_Min ++ ", " ++ P_Max ++ "], other: " ++ atom_to_list(Other). 

var_range__is_fixnum(Range) ->
  Min = var_range__min(Range),
  Max = var_range__max(Range),
  if is_integer(Min), is_integer(Max) ->
      hipe_tagscheme:is_fixnum(Min) and 
	hipe_tagscheme:is_fixnum(Max) and (var_range__other(Range) =:= false);
     true ->
      false
  end.

var_range__is_bitwidth(Range) ->
  Min = var_range__min(Range),
  Max = var_range__max(Range),
  Bits = ?BITS,
  inf_geq(Min, 0) and inf_geq(Bits, Max) and (var_range__other(Range) =:= false).


%%---------------------------------------------------------------------------
%% Range operations
%%---------------------------------------------------------------------------

%% Arithmetic

range_add(Name, Range1, Range2) ->
  NewMin = inf_add(var_range__min(Range1), var_range__min(Range2)),
  NewMax = inf_add(var_range__max(Range1), var_range__max(Range2)),
  Other = var_range__other(Range1) orelse var_range__other(Range2),
  range_init(Name, {NewMin, NewMax}, Other).


range_sub(Name, Range1, Range2) ->
  Min_sub = inf_min([inf_inv(var_range__max(Range2)), 
		     inf_inv(var_range__min(Range2))]),
  Max_sub = inf_max([inf_inv(var_range__max(Range2)), 
		     inf_inv(var_range__min(Range2))]),
  NewMin = inf_add(var_range__min(Range1), Min_sub),
  NewMax = inf_add(var_range__max(Range1), Max_sub),
  Other = var_range__other(Range1) orelse var_range__other(Range2),
  range_init(Name, {NewMin, NewMax}, Other).


range_mult(Name, #var_range{range = empty, other = true}, _Range2) ->
  range_init(Name, empty, true); 
range_mult(Name, _Range1, #var_range{range = empty, other = true}) ->
  range_init(Name, empty, true); 
range_mult(Name, Range1, Range2) ->
  Min1 = var_range__min(Range1),
  Min2 = var_range__min(Range2),
  Max1 = var_range__max(Range1),
  Max2 = var_range__max(Range2),
  GreaterMin1 = inf_greater_zero(Min1),
  GreaterMin2 = inf_greater_zero(Min2),
  GreaterMax1 = inf_greater_zero(Max1),
  GreaterMax2 = inf_greater_zero(Max2),
  Range = 
    if GreaterMin1 -> 
	if GreaterMin2 -> {inf_mult(Min1, Min2), inf_mult(Max1, Max2)};
	   GreaterMax2 -> {inf_mult(Min2, Max1), inf_mult(Max2, Max1)};
	   true        -> {inf_mult(Min2, Max1), inf_mult(Max2, Min1)}
	end;
       %% Kolumn 1 eller 2
       GreaterMin2 -> % Kolumn 1 eller 2 rad 3
	var_range__range(range_mult(Name, Range2, Range1));
       GreaterMax1 -> %Kolumn 2 Rad 1 eller 2
	if GreaterMax2 -> % Kolumn 2 Rad 2
	    NewMin = inf_min([inf_mult(Min2, Max1), inf_mult(Max2, Min1)]),
	    NewMax = inf_max([inf_mult(Min2, Min1), inf_mult(Max2, Max1)]),
	    {NewMin, NewMax};
	   true -> % Kolumn 2 Rad 1
	    {inf_mult(Min2, Max1), inf_mult(Min2, Min1)}
	end;
       GreaterMax2 -> % Kolumn 1 Rad 2	
	var_range__range(range_mult(Name, Range2, Range1));
       true -> % Kolumn 1 Rad 1 
	{inf_mult(Max1, Max2), inf_mult(Min2, Min1)}
    end,
  Other = var_range__other(Range1) orelse var_range__other(Range2),
  range_init(Name, Range, Other).


extreme_divisors(#var_range{range={0,0}}) -> {0,0};
extreme_divisors(#var_range{range={0,Max}}) -> {1,Max};
extreme_divisors(#var_range{range={Min,0}}) -> {Min,-1};
extreme_divisors(#var_range{range={Min,Max}}) ->
  case inf_geq(Min, 0) of 
    true -> {Min, Max};
    false -> %Min < 0
      case inf_geq(0, Max) of
	true -> {Min,Max}; %Max < 0
	false -> {-1,1} %Max > 0
      end
  end.

%% this is div, not /.
range_div(Name, _, #var_range{range={0,0}}) ->
  range_init(Name, empty, false);
range_div(Name, #var_range{range=empty}, _) ->
  range_init(Name, empty, false);
range_div(Name, _, #var_range{range=empty}) ->
  range_init(Name, empty, false);
range_div(Name, Range1, Den) ->
  Min1 = var_range__min(Range1),
  Max1 = var_range__max(Range1),
  {Min2, Max2} = extreme_divisors(Den),
  Min_max_list = [inf_div(Min1, Min2), inf_div(Min1, Max2), inf_div(Max1, Min2), inf_div(Max1, Max2)],
  range_init(Name, {inf_min(Min_max_list), inf_max(Min_max_list)}, false).

range_rem(Name, Range1, Range2) ->
  %% Range1 desides the sign of the answer.
  Min1 = var_range__min(Range1),
  Max1 = var_range__max(Range1),
  Min2 = var_range__min(Range2),
  Max2 = var_range__max(Range2),
  Min1_geq_zero = inf_geq(Min1, 0),
  Max1_leq_zero = inf_geq(0, Max1),
  Max_range2 = inf_max([inf_abs(Min2), inf_abs(Max2)]),
  Max_range2_leq_zero = inf_geq(0, Max_range2),
  New_min = 
    if Min1_geq_zero ->	0;
       Max_range2_leq_zero -> Max_range2;
       true -> inf_inv(Max_range2)
    end,
  New_max = 
    if Max1_leq_zero -> 0;
       Max_range2_leq_zero -> inf_inv(Max_range2);
       true -> Max_range2
    end,
  range_init(Name, {New_min, New_max}, false).

%% Bit operations	

range_bsr(Name, Range1, Range2=#var_range{range={Min, Max}}) -> 
  New_Range2 = range_init(var_range__name(Range2), {inf_inv(Max), inf_inv(Min)}, var_range__other(Range2)), 
  range_bsl(Name, Range1, New_Range2).

range_bsl(Name, Range1, Range2) ->
  Min1 = var_range__min(Range1),
  Min2 = var_range__min(Range2),
  Max1 = var_range__max(Range1),
  Max2 = var_range__max(Range2),
  Min1Geq0 = inf_geq(Min1, 0),
  Max1Less0 = not inf_geq(Max1, 0),
  {Min, Max} = 
    if Min1Geq0 ->
	{inf_bsl(Min1, Min2), inf_bsl(Max1, Max2)};
       true ->
	if Max1Less0 -> {inf_bsl(Min1, Max2), inf_bsl(Max1, Min2)};
	   true -> {inf_bsl(Min1, Max2), inf_bsl(Max1, Max2)}
	end
    end,
  range_init(Name, {Min, Max}, false).

range_bnot(Name, Range) ->
  Minus_one = range_init(const, {-1,-1}, false),
  range_add(Name, range_mult(Name, Range, Minus_one), Minus_one).

width({Min, Max}) -> inf_max([width(Min), width(Max)]);
width(posinf) -> posinf;
width(neginf) -> posinf;
width(X) when X >= 0 -> poswidth(X, 0);
width(X) when X < 0 -> negwidth(X, 0).

poswidth(X, N) ->
  case X < (1 bsl N) of
    true  -> N;
    false -> poswidth(X, N+1)
  end.

negwidth(X, N) ->
  case X > (-1 bsl N) of
    true  -> N;
    false -> negwidth(X, N+1)
  end.

band_if_constant({Min, Max}, Range) ->
  case var_range__range(Range) of 
    {Const, Const} when is_integer(Const) ->
      {Min band Const, Max band Const};
    _ ->
      {Min, Max}
  end.


range_band(Name, R1, R2) ->
  {Min1, Max1} = var_range__range(R1),
  {Min2, Max2} = var_range__range(R2),
  Width = inf_min([width({Min1, Max1}), width({Min2, Max2})]),
  Min =
    case inf_geq(Min1, 0) or inf_geq(Min2, 0) of
      true  -> 0;
      false -> inf_bsl(-1, Width)
    end,
  Max =
    case inf_geq(Max1, 0) or inf_geq(Max2, 0) of
      true  -> inf_add(inf_bsl(1, Width),-1);
      false -> 0
    end,
  DstRange = band_if_constant(band_if_constant({Min, Max}, R1), R2),
  Ans = range_init(Name, DstRange, false),
  case (var_range__is_fixnum(R1) or var_range__is_fixnum(R2)) and not var_range__is_fixnum(Ans) of
    true ->
      warning("band", {R1, R2, Ans}, true);
    false ->
      ok
  end,
  Ans.

range_bor(Name, R1, R2) ->
  {Min1, Max1} = var_range__range(R1),
  {Min2, Max2} = var_range__range(R2),
  Width = inf_max([width({Min1, Max1}), width({Min2, Max2})]),
  Min =
    case inf_geq(Min1, 0) and inf_geq(Min2, 0) of
      true  -> 0;
      false -> inf_bsl(-1, Width)
    end,	  
  Max =
    case inf_geq(Max1, 0) and inf_geq(Max2, 0) of
      true  -> inf_add(inf_bsl(1, Width), -1);
      false -> -1
    end,
  Ans = range_init(Name, {Min, Max}, false),
  case (var_range__is_fixnum(R1) and var_range__is_fixnum(R2)) and not var_range__is_fixnum(Ans) of
    true ->
      warning("bor", {R1, R2, Ans}, true);
    false ->
      ok
  end,
  Ans.


range_bxor(Name, R1, R2) ->
  range_bor(Name, R1, R2). %% overapproximation 

%% Propagation

range_remove_constant(Range1, #var_range{range=empty}) ->
  Range1;
range_remove_constant(Range1, #var_range{range={Const, Const}}) when is_integer(Const) ->
  Old_min = var_range__min(Range1),
  Old_max = var_range__max(Range1),
  if Old_min =:= Const ->
      New_min = Old_min + 1;
     true ->
      New_min = Old_min
  end,
  if Old_max =:= Const ->
      New_max = Old_max - 1;
     true ->
      New_max = Old_max
  end,
  Range_range = 
    if (New_min =:= empty) or (New_max =:= empty) ->
	empty;
       true ->
	{New_min, New_max}
    end,
  range_init(var_range__name(Range1), Range_range, var_range__other(Range1)).

%% range_cut(Name, []) -> #var_range{var_name=Name};
range_cut(Name, [#var_range{range=empty, other=false}|_]) ->
  range_init(Name, empty, false);
range_cut(Name, [Range|Rangelist]) ->
  range_cut(Name, Rangelist, Range).

range_cut(Name, [], Dst_range) -> 
  var_range__copy(Dst_range, Name);
range_cut(Name, [#var_range{range = empty, other = Other1}|_],
	  #var_range{other = Other2}) ->
  range_init(Name, empty, Other1 and Other2);
range_cut(Name, [Range|Rangelist], Dst_range) ->
  Min1 = var_range__min(Range),
  Max1 = var_range__max(Range),
  Min2 = var_range__min(Dst_range),
  Max2 = var_range__max(Dst_range),
  if (Min1 =:= empty) orelse (Min2 =:= empty) ->
      Int_range = empty;
     true ->
      Min = inf_max([Min1, Min2]),
      Max = inf_min([Max1, Max2]),
      Int_range = {Min, Max}
  end,	
  Other = var_range__other(Range) andalso var_range__other(Dst_range),
  range_cut(Name, Rangelist, range_init(Name, Int_range, Other)).

range_union(Name, []) ->
  range_init(Name, empty, false); %#var_range{var_name=Name};
range_union(Name, [Range|Range_list]) ->
  %% io:format("range: ~p ~n rangelist: ~p ~n", [Range, Range_list]),
  range_union(Name, Range_list, Range).

range_union(Name, [], Dst_range) -> 
  %% io:format("dstRange: ~p ~n", [DstRange]),
  var_range__copy(Dst_range, Name);
range_union(Name, [Range|Rangelist], Dst_range) ->
  %% io:format("Range: ~p, DstRange ~p ->", [Range, DstRange]),
  Min1 = var_range__min(Range),
  Max1 = var_range__max(Range),
  Min2 = var_range__min(Dst_range),
  Max2 = var_range__max(Dst_range),
  Min = inf_min([Min1, Min2]),
  Max = inf_max([Max1, Max2]),
  Int_range = 
    if (Min =:= empty) or (Max =:= empty) ->
	empty;
       true ->
	{Min, Max}
    end,
  Other = var_range__other(Range) orelse var_range__other(Dst_range),
  %% io:format("{~p, ~p} ~n", [Min, Max]),
  range_union(Name, Rangelist, 
	      range_init(Name, Int_range, Other)).

range_equality_propagation(Range_1, Range_2) ->  
  True_range_1 = range_cut(var_range__name(Range_1), [Range_1, Range_2]),
  True_range_2 = range_cut(var_range__name(Range_2), [Range_1, Range_2]),
  False_range_2 =
    case var_range__range(Range_1) of
      {Min1, Max1} ->
	if Min1 =:= Max1 ->
	    range_remove_constant(Range_2, Range_1);
	   true ->
	    Range_2
	end;
      empty ->
	Range_2
    end,
  False_range_1 = 
    case var_range__range(Range_2) of
      {Min2, Max2} ->
	if Min2 =:= Max2 ->
	    range_remove_constant(Range_1, Range_2);
	   true ->
	    Range_1
	end;
      empty ->
	Range_1
    end,
  {True_range_1, True_range_2, False_range_1, False_range_2}.

%% Range1 < Range2
range_inequality_propagation(Range1, Range2) ->
  R1_name = var_range__name(Range1),
  R2_name = var_range__name(Range2),
  R1_min = var_range__min(Range1),
  R2_min = var_range__min(Range2),
  R1_max = var_range__max(Range1),
  R2_max = var_range__max(Range2),
  R1_other = var_range__other(Range1),
  R2_other = var_range__other(Range2),

  R1_is_empty = var_range__is_empty(Range1),
  R2_is_empty = var_range__is_empty(Range2),

  {R1_true_range, R1_false_range} = 
    if R1_is_empty -> {empty, empty};
       R2_is_empty -> Range1_range = var_range__range(Range1),
		      {Range1_range, Range1_range};
       true        -> R1_true_max = inf_min([R1_max, inf_add(R2_max, -1)]),
		      R1_false_min = inf_max([R1_min, R2_min]),
		      {{R1_min, R1_true_max}, {R1_false_min, R1_max}}
    end,	

  {R2_true_range, R2_false_range} = 
    if R2_is_empty -> {empty, empty};
       R1_is_empty -> Range2_range = var_range__range(Range2),
		      {Range2_range, Range2_range};
       true        -> R2_true_min = inf_max([inf_add(R1_min,1),R2_min]),
		      R2_false_max = inf_min([R1_max, R2_max]),
		      {{R2_true_min, R2_max}, {R2_min, R2_false_max}}
    end,	

  R1_true = range_init(R1_name, R1_true_range, R1_other),
  R2_true = range_init(R2_name, R2_true_range, R2_other),
  R1_false = range_init(R1_name, R1_false_range, R1_other),
  R2_false = range_init(R2_name, R2_false_range, R2_other),
  {R1_true, R2_true, R1_false, R2_false}.

%% Range widening


get_larger_value(Value, List) ->
  get_larger_value_1(Value, lists:reverse(List)).

get_larger_value_1(Value, []) -> Value;
get_larger_value_1(Value, [Head|Tail]) ->
  case inf_geq(Head, Value) of
    true  -> Head;
    false -> get_larger_value_1(Value, Tail)
  end.

get_smaller_value(Value, List) ->
  get_smaller_value_1(Value, List).

get_smaller_value_1(Value, []) -> Value;
get_smaller_value_1(Value, [Head|Tail]) ->
  case inf_geq(Head, Value) and (Head =/= Value) of
    true  -> get_smaller_value_1(Value, Tail);
    false -> Head
  end.

range_widening(Old_range, New_range, Wideningvalues) ->
  New_min_fixed = inf_geq(var_range__min(New_range), var_range__min(Old_range)),
  New_max_fixed = inf_geq(var_range__max(Old_range), var_range__max(New_range)), 

  Old_empty = var_range__is_empty(Old_range),

  New_min = 
    if New_min_fixed or Old_empty ->
	var_range__min(New_range);
       true ->
	Min = var_range__min(New_range),
	get_smaller_value(Min, Wideningvalues)
    end,
  New_max = 
    if New_max_fixed or Old_empty ->
	var_range__max(New_range);
       true ->
	Max = var_range__max(New_range),
	get_larger_value(Max, Wideningvalues)
    end,
  Range_range = 
    if (New_min =:= empty) or (New_max =:= empty) ->
	empty;
       true ->
	{New_min, New_max}
    end,
  New_other = var_range__other(New_range),
  R = range_init(var_range__name(New_range), Range_range, New_other),
						%  io:format("Range widening ~p~n", [R]),
  R.

%%---------------------------------------------------------------------------
%% Inf operations
%%---------------------------------------------------------------------------

inf_max([]) -> empty;
inf_max([H|T])->
  if H =:= empty ->
      inf_max(T);
     true ->
      lists:foldl(
	fun(Elem, Max) ->
	    Geq = inf_geq(Elem, Max),
	    if not Geq or (Elem =:= empty) ->
		Max;
	       true ->
		Elem
	    end
	end,
	H,
	T)
  end. 

inf_min([]) -> empty;
inf_min([H|T])->
  if H =:= empty ->
      inf_min(T);
     true ->
      lists:foldl(fun(Elem, Min) ->
		      Geq = inf_geq(Elem, Min),
		      if Geq or (Elem =:= empty) ->
			  Min;
			 true ->
			  Elem
		      end
		  end,
		  H,
		  T)
  end. 

inf_abs(posinf) -> posinf;
inf_abs(neginf) -> posinf;
inf_abs(Number) when is_integer(Number), (Number < 0) -> - Number;
inf_abs(Number) when is_integer(Number) -> Number.

inf_add(posinf, _Number) -> posinf;
inf_add(neginf, _Number) -> neginf;
inf_add(_Number, posinf) -> posinf;
inf_add(_Number, neginf) -> neginf;
inf_add(Number1, Number2) when is_integer(Number1), is_integer(Number2) ->
  Number1 + Number2.

inf_inv(posinf) -> neginf;
inf_inv(neginf) -> posinf;
inf_inv(Number) -> -Number.

inf_geq(posinf, _) -> true;
inf_geq(_, posinf) -> false;
inf_geq(_, neginf) -> true;
inf_geq(neginf, _) -> false;
inf_geq(A, B) -> A >= B.

inf_greater_zero(posinf) -> true;
inf_greater_zero(neginf) -> false;
inf_greater_zero(Number) when Number >= 0 -> true;
inf_greater_zero(Number) when Number < 0 -> false.

inf_div(Number, 0) ->
  Greater = inf_greater_zero(Number),
  if Greater -> posinf;
     true -> neginf
  end;
inf_div(posinf, Number) ->
  Greater = inf_greater_zero(Number),
  if Greater -> posinf;
     true -> neginf
  end;
inf_div(neginf, Number) ->
  Greater = inf_greater_zero(Number),
  if Greater -> neginf;
     true -> posinf
  end;
inf_div(Number, posinf) -> 
  Greater = inf_greater_zero(Number),
  if Greater -> posinf;
     true -> neginf
  end;
inf_div(Number, neginf) ->
  Greater = inf_greater_zero(Number),
  if Greater -> neginf;
     true -> posinf
  end;
inf_div(Number1, Number2) -> Number1 div Number2.

inf_mult(neginf, Number) -> 
  Greater = inf_greater_zero(Number), 
  if Greater -> neginf;
     true -> posinf
  end;
inf_mult(posinf, Number) -> 
  Greater = inf_greater_zero(Number),
  if Greater -> posinf;
     true -> neginf
  end;
inf_mult(Number, posinf) -> inf_mult(posinf, Number);
inf_mult(Number, neginf) -> inf_mult(neginf, Number);
inf_mult(Number1, Number2) -> Number1 * Number2.

inf_bsl(posinf, _) -> posinf;
inf_bsl(neginf, _) -> neginf;
inf_bsl(Number, posinf) when Number >= 0 -> posinf;
inf_bsl(_, posinf) -> neginf;
inf_bsl(Number, neginf) when Number >= 0 -> 0;
inf_bsl(_Number, neginf) -> -1;
inf_bsl(Number1, Number2) -> 
  Bits = ?BITS,
  if Number2 > (Bits bsl 1) -> inf_bsl(Number1, posinf);
     Number2 < (-Bits bsl 1) ->	inf_bsl(Number1, neginf);
     true -> Number1 bsl Number2
  end.

%%---------------------------------------------------------------------------
%% Range info
%%---------------------------------------------------------------------------

range_info_from_info_struct(Info) ->
  Live_labels = info_struct__live_labels(Info),
  #range_info{live_labels = Live_labels,
	      in_label_range_trees = in_range_trees_from_info_struct(Info, Live_labels), 
	      out_label_range_trees = out_range_trees_from_info_struct(Info, Live_labels),		
	      return_points = info_struct__return_vars(Info),
	      warn = info_struct__warn(Info)
	     }.

range_info__def_range(#range_info{out_label_range_trees = Label_tree}, Label, Variable) ->
  %% io:format("Def: Label ~p, Var ~p ~n", [Label, Variable]),
  Range_tree = gb_trees:get(Label, Label_tree),
  case gb_trees:lookup(Variable, Range_tree) of
    {value, Range} -> Range;
    none -> range_init(Variable, empty, false)
  end.

range_info__use_range(Range_info = #range_info{in_label_range_trees = Label_tree}, Label, Variable) ->
  %% io:format("Use: Label ~p, Var ~p ~n", [Label, Variable]),
  Range_tree = gb_trees:get(Label, Label_tree),
  case gb_trees:lookup(Variable, Range_tree) of
    {value, Range} -> Range;
    none -> range_info__def_range(Range_info, Label, Variable)
  end. 

range_info__is_live(#range_info{live_labels = Labels}, Label) ->
  gb_sets:is_member(Label, Labels).

range_info__warn(#range_info{warn = Warn}) ->
  Warn.

%%---------------------------------------------------------------------------
%% Print out messages
%%---------------------------------------------------------------------------

warning(Op, Spec, true) ->
  Text = "Icode range analysis not good (~w, ~w)\n", 
  Args = [Op,Spec],
  ?msg(Text,Args);
warning(_, _, false) ->
  ok.

info(New, Old, true) ->
  Text = "Icode range analysis found info (~w, ~w)\n", 
  Args = [New, Old],
  ?msg(Text,Args);
info(_, _, false) ->
  ok. 

test() ->
  put(hipe_target_arch, amd64),
  %% Bnot
  A = range_init(a, {11, 17}, false),
  Ainv = range_init(a, {-18, -12}, false),
  A = range_bnot(a, Ainv),
  Ainv = range_bnot(a, A),

  B = range_init(b, {-3, 4}, true),
  Binv = range_init(b, {-5, 2}, true),
  Binv = range_bnot(b, B),
  B = range_bnot(b, Binv),

  %% Band
  C = range_init(a, {0, 31}, false),
  C = range_band(a, A, Ainv),
  D = range_init(a, {-8, 7}, false),
  D = range_band(a, Ainv, Binv),

  %% Bor
  E = range_bnot(a, C),
  E = range_bor(a, A, Ainv),
  F = range_init(a, {-32, 31}, false),
  F = range_bor(a, A, Binv),

  %% Add
  G = range_init(a, {-7, 5}, false),
  G = range_add(a, A, Ainv),
  H = range_init(a, {-8, 6}, true),
  H = range_add(a, B, Binv),

  %% Bsl
  I = range_init(a, {1, 272}, false),
  I = range_bsl(a, A, B),
  J = range_init(a, {-12,16}, false),
  J = range_bsl(a, B, Binv),

  %% Bsr
  K = range_init(a, {-96,128}, false),
  K = range_bsr(a, B, Binv).

%% vim: set tabstop=2 ft=erlang
