%%%-------------------------------------------------------------------
%%% File    : hipe_icode_range.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created : 12 Mar 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_icode_range).

-export([cfg/4,pp_ann/1]).

%%=====================================================================
%% Icode Coordinator Callbacks
%%=====================================================================

-export([replace_nones/1,
	 update__info/2, new__info/1, return__info/1,
	 return_none/0, return_none_args/2, return_any_args/2]).

%%=====================================================================

-import(erl_types, [t_any/0,
		    t_components/1,
		    t_inf/2, t_integer/0,
		    t_from_range_unsafe/2,
		    t_to_string/1,
		    t_none/0,
		    number_min/1, number_max/1
		   ]).

-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").
-include("../main/hipe.hrl").
-include("../flow/cfg.hrl").
-include("../flow/hipe_bb.hrl").

-type(range_rep() :: {'neg_inf' | integer(), 'pos_inf' | integer()} | 'empty').
-type(fun_name() :: atom() | tuple()).

-record(range, {range :: range_rep(),
		other :: bool()}).

-record(ann,   {range :: #range{}, 
                type  :: any(), 
		count :: integer()}).

-type(args_fun() :: fun((mfa(),cfg()) -> [#range{}])). 
-type(call_fun() :: fun((mfa(),[#range{}]) -> #range{})). 		      
-type(final_fun() :: fun((mfa(),[#range{}]) -> ok)). 	 
-type(data() :: {mfa(), args_fun(), call_fun(), final_fun()}).
-type(label() :: integer()).
-type(set() :: tuple()).
-type(gb_tree() :: tuple()).
-type(dict() :: tuple()).
-type(info() :: gb_tree()).
-type(work_list() :: {[label()],[label()],set()}).
-type(variable() :: {'var', integer(), any()} | 
                    {'reg', integer(), any()} | 
                    {'fvar', integer()}).
-type(annotated_variable() :: {'var', integer(), #ann{}} | 
                              {'reg', integer(), #ann{}} | 
                              {'fvar', integer()}).
-type(argument() :: {const,_} | variable()).
-type(three_range_fun() :: fun((#range{},#range{},#range{}) -> #range{})).
-type(instr_split_info() ::  {icode_instr(),[{label(),info()}]}).
-type(last_instr_return() ::  {instr_split_info(), #range{}}).  

-record(state, {info_map	   :: gb_tree(), 
		counter=dict:new() :: dict(), 
		cfg		   :: cfg(), 
		liveness	   :: gb_tree(), 
		ret_type	   :: #range{}, 
		lookupfun	   :: call_fun(),
		resultaction	   :: final_fun()}).

-define(WIDEN, 1).

-define(TAG_IMMED1_SIZE, 4).

-define(BITS, 64).


-spec(cfg/4 :: (cfg(), mfa(), comp_options(), #comp_servers{}) -> cfg()).

cfg(Cfg, MFA, Options, Servers) ->
  case proplists:get_bool(concurrent_comp, Options) of
    true ->
      concurrent_cfg(Cfg, MFA, Servers#comp_servers.range);
    false ->
      ordinary_cfg(Cfg, MFA)
  end.

-spec(concurrent_cfg/3 :: (cfg(), mfa(), pid()) -> cfg()).

concurrent_cfg(Cfg, MFA, CompServer) ->
  CompServer ! {ready, {MFA,self()}},
  {ArgsFun,CallFun,FinalFun} = do_analysis(Cfg, MFA),
  do_rewrite(Cfg, MFA, ArgsFun, CallFun, FinalFun).

-spec(do_analysis/2 :: 
      (cfg(), mfa()) -> {args_fun(), call_fun(), final_fun()}).

do_analysis(Cfg, MFA) ->
  receive
    {analyse, {ArgsFun, CallFun, FinalFun}} ->
      analyse(Cfg, {MFA, ArgsFun, CallFun, FinalFun}),
      do_analysis(Cfg, MFA);
    {done, {NewArgsFun, NewCallFun, NewFinalFun}} ->
      {NewArgsFun, NewCallFun, NewFinalFun}
  end.

-spec(do_rewrite/5 :: 
      (cfg(), mfa(), args_fun(), call_fun(), final_fun()) -> cfg()).

do_rewrite(Cfg, MFA, ArgsFun, CallFun, FinalFun) ->
  common_rewrite(Cfg, {MFA, ArgsFun, CallFun, FinalFun}).
 
-spec(ordinary_cfg/2 :: (cfg(), mfa()) -> cfg()).

ordinary_cfg(Cfg, MFA) ->
  Data = make_data(Cfg,MFA),
  common_rewrite(Cfg, Data).
  
-spec(common_rewrite/2 :: (cfg(), data()) -> cfg()).

common_rewrite(Cfg, Data) ->
  State = analyse(Cfg, Data),
  State2 = rewrite_blocks(State),
  Cfg1 = state__cfg(State2),
  Cfg2 = hipe_icode_cfg:remove_unreachable_code(Cfg1),
  Cfg3 = convert_cfg_to_types(Cfg2),
  hipe_icode_type:specialize(Cfg3).

-spec(make_data/2 :: (cfg(), mfa()) -> data()).

make_data(Cfg, {_M,_F,A}=MFA) ->
  NoArgs = 
   case hipe_icode_cfg:is_closure(Cfg) of
      true -> hipe_icode_cfg:closure_arity(Cfg)+1;
      false -> A
    end,
  Args = lists:duplicate(NoArgs, any_type()), 
  ArgsFun = fun(_,_) -> Args end,
  CallFun = fun(_,_) -> any_type() end,
  FinalFun = fun(_,_) -> ok end,
  {MFA,ArgsFun,CallFun,FinalFun}.

-spec(analyse/2 :: (cfg(), data()) -> #state{} | 'ok').

analyse(Cfg, Data={MFA,_,_,_}) ->
  try 
    State = state__init(Cfg,Data),
    Work = init_work(State),
    NewState = analyse_blocks(State,Work),
    (state__resultaction(NewState))(MFA,[state__ret_type(NewState)]),
    NewState
  catch throw:no_input -> ok
  end.

-spec(rewrite_blocks/1 :: (#state{}) -> #state{}).

rewrite_blocks(State) ->
  Cfg = state__cfg(State),
  Start = hipe_icode_cfg:start_label(Cfg),
  rewrite_blocks([Start], State, [Start]).

-spec(rewrite_blocks/3 :: ([label()], #state{}, [label()]) -> #state{}).

rewrite_blocks([Next|Rest], State, Visited) ->
  Info = state__info_in(State, Next),
  {NewState, NewLabels} = analyse_block(Next, Info, State, true),
  NewLabelsSet = ordsets:from_list(NewLabels),
  RealNew = ordsets:subtract(NewLabelsSet, Visited),
  NewVisited = ordsets:union([RealNew,Visited,[Next]]),
  NewWork = ordsets:union([RealNew,Rest]),
  rewrite_blocks(NewWork, NewState, NewVisited);
rewrite_blocks([], State, _) ->
  State.

-spec(analyse_blocks/2 :: (#state{}, work_list()) -> #state{}).

analyse_blocks(State, Work) ->
  case get_work(Work) of
    fixpoint ->
      State;
    {Label, NewWork} ->
      Info = state__info_in(State, Label),
      {NewState, NewLabels}  = 
	try analyse_block(Label, Info, State, false)
	catch throw:none_range ->
	    {State,[]}
	end,
      NewWork2 = add_work(NewWork, NewLabels),
      analyse_blocks(NewState, NewWork2)
  end.

-spec(analyse_block/4 :: 
      (label(), info(), #state{}, bool()) -> {#state{}, [label()]}).

analyse_block(Label, Info, State, Rewrite) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  {NewCode, InfoList, RetType} = 
    analyse_BB(Code,Info,[],Rewrite,state__lookupfun(State)),
  State1 = state__bb_add(State, Label, hipe_bb:mk_bb(NewCode)),
  State2 = state__ret_type_update(State1, RetType),
  state__update_info(State2, InfoList, Rewrite).

-spec(analyse_BB/5 ::
      ([icode_instr()], info(), [icode_instr()], bool(), call_fun()) -> 
	 {[icode_instr()], [info()], #range{}}).

analyse_BB([Last], Info, Code, Rewrite, LookupFun) ->
  %% io:format("I: ~w~n",[Last]),
  {{NewI,InfoList},RetType} = analyse_last_insn(Last, Info, Rewrite, LookupFun),
  {lists:reverse([NewI|Code]), InfoList, RetType};
analyse_BB([Insn|InsnList], Info, Code, Rewrite, LookupFun) ->
  {NewInfo,NewI} = analyse_insn(Insn, Info, LookupFun), 
  analyse_BB(InsnList, NewInfo, [NewI|Code], Rewrite, LookupFun).

-spec(analyse_insn/3 ::
      (icode_instr(), info(), call_fun()) -> {info(), icode_instr()}).

analyse_insn(I, Info, LookupFun) ->
  %% io:format("~w Info: ~p~n",[I,Info]),
  NewI = handle_args(I,Info),
  FinalI = 
    case NewI of 
      #call{} -> analyse_call(NewI, LookupFun);
      #move{} -> analyse_move(NewI);
      #phi{} -> analyse_phi(NewI);
      #fmove{} -> analyse_fmove(NewI);
      #begin_handler{} -> analyse_begin_handler(NewI);
      #comment{} -> NewI
    end,
  {enter_vals(FinalI,Info), FinalI}.

-spec(handle_args/2 :: (icode_instr(), info()) -> icode_instr()).

handle_args(I, Info) ->
  WidenFun = fun update_three/3,
  handle_args(I, Info, WidenFun).

-spec(handle_args/3 :: (icode_instr(), info(), 
			three_range_fun()) -> 
	 icode_instr()).

handle_args(I, Info, WidenFun) ->
  Uses = hipe_icode:uses(I),
  PresentRanges = [lookup(V,Info) || V <- Uses],
  %%io:format("Uses: ~p~nRanges: ~p~n",[Uses,PresentRanges]),
  JoinFun = fun(Var, Range) -> update_info(Var, Range, WidenFun) end,
  NewUses = lists:zipwith(JoinFun, Uses, PresentRanges),
  hipe_icode:subst_uses(lists:zip(Uses, NewUses),I).

-spec(join_info/3 :: (#ann{}, #range{}, three_range_fun()) -> #ann{}).

join_info(Ann = #ann{range=R1,type=Type,count=?WIDEN}, R2, Fun)  ->
  Ann#ann{range = Fun(R1,R2,range_from_simple_type(Type))};
join_info(Ann = #ann{range=R1,type=Type,count=C}, R2, _Fun) when C < ?WIDEN -> 
  case join_three(R1, R2, range_from_simple_type(Type)) of
    R1 -> Ann;
    NewR -> Ann#ann{range = NewR, count=C+1}
  end.

-spec(join_three/3 :: (#range{}, #range{}, #range{}) -> #range{}).

join_three(R1,R2,R3) ->
  inf(sup(R1,R2),R3).

-spec(update_info/2 :: (variable(), #range{}) -> annotated_variable()).

update_info(Var, Range) ->
  update_info(Var, Range, fun update_three/3).

-spec(update_info/3 :: (variable(), #range{}, three_range_fun()) -> 
                          annotated_variable()).

update_info({var,Name,Ann}, R, Fun) ->
  {var,Name,update_info1(Ann,R,Fun)};
update_info({reg,Name,Ann}, R, Fun) ->
  {reg,Name,update_info1(Ann,R,Fun)};
update_info({fvar,Name}, _R, _) ->
  {fvar,Name}.

-spec(update_info1/3 :: (any(), #range{}, three_range_fun()) -> #ann{}).

update_info1(Ann = #ann{range=R1,type=Type,count=?WIDEN}, R2, Fun)  ->
  Ann#ann{range = Fun(R1,R2,range_from_simple_type(Type))};
update_info1(Ann = #ann{range=R1,type=Type,count=C}, R2, _Fun) -> 
  case update_three(R1, R2, range_from_simple_type(Type)) of
    R1 -> Ann;
    NewR -> Ann#ann{range = NewR, count=C+1}
  end;
update_info1(Type,R2, _Fun) ->
  #ann{range = inf(range_from_simple_type(Type),R2), type = Type, count=1}.

-spec(update_three/3 :: (#range{}, #range{}, #range{}) -> #range{}).

update_three(_R1,R2,R3) ->
  inf(R2,R3).

-spec(safe_widen/3 :: (#range{},#range{},#range{}) -> #range{}).

safe_widen(#range{range=Old},#range{range=New},T = #range{range=Wide}) ->
  ResRange = 
    case {Old,New,Wide} of
      {{Min,Max1},{Min,Max2},{_,Max}} ->
	case inf_geq(OMax = next_up_limit(inf_max([Max1,Max2])),Max) of
	  true -> {Min,Max};
	  false -> {Min,OMax}
	end;
      {{Min1,Max},{Min2,Max},{Min,_}} ->
	case inf_geq(Min, OMin = next_down_limit(inf_min([Min1,Min2]))) of
	  true -> {Min,Max};
	  false -> {OMin,Max}
	end;
      {{Min1,Max1},{Min2,Max2},{Min,Max}} -> 
	RealMax =
	  case inf_geq(OMax = next_up_limit(inf_max([Max1,Max2])),Max) of
	    true -> Max;
	    false -> OMax
	  end,
	RealMin = 
	  case inf_geq(Min, OMin = next_down_limit(inf_min([Min1,Min2]))) of
	    true -> Min;
	    false -> OMin
	  end,
	{RealMin,RealMax};
      _ ->
	Wide
    end,
  T#range{range=ResRange}.

-spec(widen/3 :: (#range{},#range{},#range{}) -> #range{}).

widen(#range{range=Old},#range{range=New},T = #range{range=Wide}) ->
  ResRange = 
    case {Old,New,Wide} of
      {{Min,_},{Min,Max2},{_,Max}} ->
	case inf_geq(OMax = next_up_limit(Max2),Max) of
	  true -> {Min,Max};
	  false -> {Min,OMax}
	end;
      {{_,Max},{Min2,Max},{Min,_}} ->
	case inf_geq(Min, OMin = next_down_limit(Min2)) of
	  true -> {Min,Max};
	  false -> {OMin,Max}
	end;
      {_,{Min2,Max2},{Min,Max}} -> 
	RealMax =
	  case inf_geq(OMax = next_up_limit(Max2),Max) of
	    true -> Max;
	    false -> OMax
	  end,
	RealMin = 
	  case inf_geq(Min, OMin = next_down_limit(Min2)) of
	    true -> Min;
	    false -> OMin
	  end,
	{RealMin,RealMax};
      _ ->
	Wide
    end,
  T#range{range=ResRange}.

-spec(analyse_call/2 :: (#call{},call_fun()) -> #call{}).
	
analyse_call(Call, LookupFun) ->
  case hipe_icode:call_dstlist(Call) of
    [] ->
      Call;
    Dsts ->
      Args = hipe_icode:args(Call),
      Fun = hipe_icode:call_fun(Call),
      Type = hipe_icode:call_type(Call),
      DstRanges = analyse_call_or_enter_fun(Fun, Args, Type, LookupFun),
      NewDefs = [update_info(Var,Type) || {Var,Type} <- lists:zip(Dsts,DstRanges)],
      hipe_icode:subst_defines(lists:zip(Dsts,NewDefs),Call)
  end.

-spec(analyse_fmove/1 :: (#fmove{}) -> #fmove{}).

analyse_fmove(FMove) ->
  Dst = hipe_icode:fmove_dst(FMove),
  DstRange = none_range(),
  NewDst = update_info(Dst,DstRange),
  hipe_icode:subst_defines([{Dst,NewDst}],FMove).

-spec(analyse_move/1 :: (#move{}) -> #move{}).

analyse_move(Move) ->
  Src = hipe_icode:move_src(Move),
  Dst = hipe_icode:move_dst(Move),
  Range = get_range_from_arg(Src),
  NewDst = update_info(Dst,Range),  
  hipe_icode:subst_defines([{Dst,NewDst}],Move).

-spec(analyse_begin_handler/1 :: (#begin_handler{}) -> #begin_handler{}).

analyse_begin_handler(Handler) ->
  SubstList =
    [{Dst,update_info(Dst,any_type())} || 
      Dst <- hipe_icode:begin_handler_dstlist(Handler)],
  hipe_icode:subst_defines(SubstList,Handler).

-spec(analyse_phi/1 :: (#phi{}) -> #phi{}).
    
analyse_phi(Phi) ->
  {_, Args} = lists:unzip(hipe_icode:phi_arglist(Phi)),
  Dst = hipe_icode:phi_dst(Phi),
  Arg_ranges = get_range_from_args(Args),
  %%%%io:format("Phi-Arg_ranges: ~p ~n", [Arg_ranges]),
  DstRange = sup(Arg_ranges),
  NewDst = update_info(Dst,DstRange,fun widen/3),  
  hipe_icode:subst_defines([{Dst,NewDst}],Phi).

-spec(analyse_last_insn/4 :: (icode_instr(),info(),bool(),call_fun()) -> 
	                       last_instr_return()).

analyse_last_insn(I, Info, Rewrite, LookupFun) ->
  %%io:format("~w Info: ~p~n",[I,Info]),
  NewI = handle_args(I,Info),
  %%io:format("~w -> ~w~n",[NewI,I]),
  case NewI of 
    #return{} -> analyse_return(NewI, Info);
    #enter{} -> analyse_enter(NewI, Info, LookupFun);
    #switch_val{} -> {analyse_switch_val(NewI, Info, Rewrite),none_type()};
    #'if'{} -> {analyse_if(NewI, Info, Rewrite),none_type()};
    #goto{} -> {analyse_goto(NewI, Info),none_type()};	
    #type{} -> {analyse_type(NewI, Info, Rewrite),none_type()};
    #fail{} -> {analyse_fail(NewI, Info),none_type()};
    #call{} -> {analyse_last_call(NewI, Info, LookupFun),none_type()};
    #switch_tuple_arity{} -> 
      {analyse_switch_tuple_arity(NewI, Info),none_type()};
    #begin_try{} -> {analyse_begin_try(NewI, Info),none_type()}
  end.

-spec(analyse_return/2 :: (icode_instr(),info()) -> 
	                       last_instr_return()).

analyse_return(Insn, _Info) ->
  [RetRange] = get_range_from_args(hipe_icode:return_vars(Insn)),
  {{Insn,[]},RetRange}.

-spec(analyse_enter/3 :: (icode_instr(),info(),call_fun()) -> 
	                       last_instr_return()).
  
analyse_enter(Insn, _Info, LookupFun) ->
  Args = hipe_icode:args(Insn),
  Fun = hipe_icode:enter_fun(Insn),
  CallType = hipe_icode:enter_type(Insn),
  [RetRange] = analyse_call_or_enter_fun(Fun,Args,CallType,LookupFun),
  {{Insn,[]},RetRange}.

-spec(analyse_switch_val/3 :: (#switch_val{},info(),bool()) -> instr_split_info()).

analyse_switch_val(Switch, Info, Rewrite) -> 
  Arg = hipe_icode:switch_val_arg(Switch),
  SwitchRange = get_range_from_arg(Arg),
  Cases = hipe_icode:switch_val_cases(Switch), 
  {FailRange, LabelRangeList} = get_range_label_list(Cases, SwitchRange, []),
  case range__is_none(FailRange) of
    true -> 
      InfoList = update_infos(Arg, Info, LabelRangeList),
      if Rewrite -> {update_switch(Switch,LabelRangeList,false),InfoList};
	 true -> {Switch,InfoList}
      end;
    false ->
      FailLabel = hipe_icode:switch_val_fail_label(Switch),
      InfoList = update_infos(Arg, Info, [{FailRange, FailLabel}|LabelRangeList]),
      if Rewrite -> {update_switch(Switch,LabelRangeList,true),
		     InfoList};
	 true -> {Switch,InfoList}
      end
  end.

-spec(update_infos/3 :: 
      (argument(), info(), [{#range{},label()}]) -> [{label(),info()}]).

update_infos(Arg, Info, [{Range, Label}|Rest]) ->
  [{Label,enter_define({Arg,Range},Info)} | update_infos(Arg,Info,Rest)];
update_infos(_, _, []) -> [].

-spec(get_range_label_list/3 :: 
      ([{argument(),label()}], #range{},[{#range{},label()}]) -> 
	 {#range{},[{#range{},label()}]}).

get_range_label_list([{Val,Label}|Cases],SRange,Acc) ->
  VRange = get_range_from_arg(Val),
  None = none_type(),
  case inf(SRange, VRange) of
    None ->
      get_range_label_list(Cases, SRange, Acc);
    ResRange ->
      get_range_label_list(Cases, SRange, [{ResRange,Label}|Acc])
  end;
get_range_label_list([], SRange, Acc) ->
  {PointTypes, _} = lists:unzip(Acc),
  {remove_point_types(SRange, PointTypes), Acc}.

-spec(update_switch/3 :: 
      (#switch_val{},[{#range{},label()}],bool()) -> #switch_val{}).

update_switch(Switch, LabelRangeList, KeepFail) ->
  S2 = 
    case label_range_list_to_cases(LabelRangeList,[]) of
      no_update ->
	Switch;
      Cases -> 
	hipe_icode:switch_val_cases_update(Switch, Cases)
    end,
  if KeepFail -> S2;
     true -> S2
  end.

-spec(label_range_list_to_cases/2 :: 
      ([{#range{},label()}],[{{const,_},label()}]) -> 'no_update' | [{{const,_},label()}]).

label_range_list_to_cases([{#range{range={C,C},other=false},Label}|Rest],
			  Acc) when is_integer(C) -> 
  label_range_list_to_cases(Rest,[{hipe_icode:mk_const(C),Label}|Acc]);
label_range_list_to_cases([{_NotAConstantRange,_Label}|_Rest],_Acc) ->
  no_update;
label_range_list_to_cases([],Acc) ->
  lists:reverse(Acc).

-spec(analyse_switch_tuple_arity/2 :: (#switch_tuple_arity{},info()) -> {#switch_tuple_arity{},[{label(),info()}]}).
  
analyse_switch_tuple_arity(Switch, Info) -> 
  Arg = hipe_icode:switch_tuple_arity_arg(Switch),
  NewInfo = enter_define({Arg,get_range_from_arg(Arg)},Info),
  Cases = hipe_icode:switch_tuple_arity_cases(Switch),
  Fail = hipe_icode:switch_tuple_arity_fail_label(Switch),
  {_, Case_labels} = lists:unzip(Cases),
  Labels = [Fail|Case_labels],
  {Switch,[{Label,NewInfo} || Label <- Labels]}.

-spec(analyse_goto/2 :: (#goto{},info()) -> {#goto{},[{label(),info()},...]}).

analyse_goto(Insn, Info) ->
  GotoLabel = hipe_icode:goto_label(Insn),
  {Insn,[{GotoLabel,Info}]}.

-spec(analyse_fail/2 :: (#fail{},info()) -> {#fail{},[{label(),info()}]}).

analyse_fail(Fail, Info) ->
  case hipe_icode:fail_label(Fail) of
    [] -> {Fail,[]};
    Label -> {Fail,[{Label,Info}]}
  end.

-spec(analyse_begin_try/2 :: 
      (#begin_try{},info()) -> {#begin_try{},[{label(),info()},...]}).

analyse_begin_try(Insn, Info) ->
  Label = hipe_icode:begin_try_label(Insn),
  Successor = hipe_icode:begin_try_successor(Insn),
  {Insn,[{Label,Info},{Successor,Info}]}.

-spec(analyse_last_call/3 :: 
      (#call{},info(),call_fun()) -> {#call{},[{label(),info()},...]}).

analyse_last_call(Call, Info, LookupFun) ->
  %% hipe_icode_pp:pp_block([Insn]),
  NewI = analyse_call(Call, LookupFun),
  Continuation = hipe_icode:call_continuation(Call),
  NewInfo = enter_vals(NewI,Info),
  case hipe_icode:call_fail_label(Call) of
    [] -> 
      {NewI,[{Continuation,NewInfo}]};
    Fail ->
      {NewI,[{Continuation,NewInfo},{Fail,Info}]}
  end.

-spec(analyse_if/3 :: (#'if'{},info(),bool()) -> {#goto{} | #'if'{},[{label(),info()}]}).

analyse_if(If, Info, Rewrite) ->
  case hipe_icode:if_args(If) of
    Args = [_,_] ->
      analyse_sane_if(If, Info, Args, get_range_from_args(Args), Rewrite);
    _ ->
      TrueLabel = hipe_icode:if_true_label(If),
      FalseLabel = hipe_icode:if_false_label(If),
      {If,[{TrueLabel,Info},{FalseLabel,Info}]}
  end.

-spec(analyse_sane_if/5 :: 
      (#'if'{},info(),[argument(),...],[#range{},...],bool()) -> 
	 {#goto{} | #'if'{},[{label(),info()}]}).

analyse_sane_if(If, Info, [Arg1, Arg2], [Range1, Range2], Rewrite) ->
  case normalize_name(hipe_icode:if_op(If)) of
    '>' ->
      {TrueRange2, TrueRange1, FalseRange2, FalseRange1} = 
	range_inequality_propagation(Range2, Range1);
    '==' -> 
      {TempTrueRange1, TempTrueRange2, FalseRange1, FalseRange2}=
	range_equality_propagation(Range1, Range2),
      TrueRange1 = set_other(TempTrueRange1,other(Range1)),
      TrueRange2 = set_other(TempTrueRange2,other(Range2));
    '<' ->
      {TrueRange1, TrueRange2, FalseRange1, FalseRange2} = 
	range_inequality_propagation(Range1, Range2);
    '>=' ->
      {FalseRange1, FalseRange2, TrueRange1, TrueRange2} =
	range_inequality_propagation(Range1, Range2);
    '=<' ->
      {FalseRange2, FalseRange1, TrueRange2, TrueRange1} = 
	range_inequality_propagation(Range2, Range1);
    '=:=' ->
      {TrueRange1, TrueRange2, FalseRange1, FalseRange2}=
	range_equality_propagation(Range1, Range2);
    '=/=' ->
      {FalseRange1, FalseRange2, TrueRange1, TrueRange2} =
	range_equality_propagation(Range1, Range2);
    '/=' -> 
      {TempFalseRange1, TempFalseRange2, TrueRange1, TrueRange2}=
	range_equality_propagation(Range1, Range2),
      FalseRange1 = set_other(TempFalseRange1,other(Range1)),
      FalseRange2 = set_other(TempFalseRange2,other(Range2))
  end,
  TrueLabel = hipe_icode:if_true_label(If),
  FalseLabel = hipe_icode:if_false_label(If),
  TrueInfo = 
    enter_defines([{Arg1,TrueRange1}, {Arg2,TrueRange2}],Info),
  FalseInfo = 
    enter_defines([{Arg1,FalseRange1}, {Arg2,FalseRange2}],Info),
  True = 
    case lists:any(fun range__is_none/1,[TrueRange1,TrueRange2]) of
      true -> [];
      false -> [{TrueLabel,TrueInfo}]
    end,
  False = 
    case lists:any(fun range__is_none/1, [FalseRange1,FalseRange2]) of
      true -> [];
      false -> [{FalseLabel,FalseInfo}]
    end,
  UpdateInfo = True++False,
  NewIF =
    if Rewrite ->
	%%io:format("~w~n~w~n",[{Arg1,FalseRange1},{Arg2,FalseRange2}]),
	%%io:format("Any none: ~w~n", [lists:any(fun range__is_none/1,[FalseRange1,FalseRange2])]),
	case UpdateInfo of
	  [] -> %%This is weird
	    If;
	  [{Label,_Info}] ->
	    hipe_icode:mk_goto(Label);
	  [_,_] ->
	    If
	end;
       true ->
	If
    end,
  {NewIF, UpdateInfo}.

-spec(normalize_name/1 :: (atom()) -> atom()).

normalize_name(Name) ->
  case Name of
    'fixnum_eq'  -> '=:='; 
    'fixnum_neq' ->   '=/=';
    'fixnum_gt'  ->  '>' ;
    'fixnum_lt' ->  '<' ;
    'fixnum_ge' ->  '>=';
    'fixnum_le' -> '=<';
    Name -> Name
  end.

-spec(range_equality_propagation/2 :: 
      (#range{},#range{}) -> {#range{},#range{},#range{},#range{}}).

range_equality_propagation(Range_1, Range_2) ->  
  True_range = inf(Range_1, Range_2),
  case {range(Range_1) ,range(Range_2)} of
    {{N,N},{N,N}} ->
      False_range_1 = none_range(),
      False_range_2 = none_range();
    {{N1,N1},{N2,N2}} ->
      False_range_1 = Range_1,
      False_range_2 = Range_2;
    {{N,N},_} ->
      False_range_1 = Range_1,
      {_,False_range_2} = compare_with_integer(N, Range_2);
    {_,{N,N}} ->
      False_range_2 = Range_2,
      {_,False_range_1} = compare_with_integer(N, Range_1);
    {_,_} ->
      False_range_1 = Range_1,
      False_range_2 = Range_2
  end,
  {True_range, True_range, False_range_1, False_range_2}.

-spec(range_inequality_propagation/2 :: 
      (#range{},#range{}) -> {#range{},#range{},#range{},#range{}}).

%% Range1 < Range2
range_inequality_propagation(Range1, Range2) ->
  R1_other = other(Range1),
  R2_other = other(Range2),
  {R1_true_range, R1_false_range, R2_true_range, R2_false_range} =
    case {range(Range1),range(Range2)} of
      {{N1,N1},{N2,N2}} ->
	case inf_geq(N2,inf_add(N1,1)) of
	  true ->
	    {{N1,N1},empty,{N2,N2},empty};
	  false ->
	    {empty,{N1,N1},empty,{N2,N2}}
	  end;
      {{N1,N1},{Min2,Max2}} ->
	case inf_geq(Min2,inf_add(N1,1)) of
	  true ->
	    {{N1,N1},empty,{inf_add(N1,1),Max2},empty};
	  false ->
	    case inf_geq(N1,Max2) of
	      true ->
		{empty,{N1,N1},empty,{Min2,N1}};
	      false ->
		{{N1,N1},{N1,N1},{inf_add(N1,1),Max2},{Min2,N1}}
	    end
	end;
      {{Min1,Max1},{N2,N2}} ->
	case inf_geq(N2,inf_add(Max1,1)) of
	  true ->
	  {{Min1,inf_add(N2,-1)},empty,{N2,N2},empty};
	  false ->
	    case inf_geq(Min1,N2) of
	      true ->
		{empty,{N2,Max1},empty,{N2,N2}};
	      false ->
		{{Min1,inf_add(N2,-1)},{N2,Max1},{N2,N2},{N2,N2}}
	    end
	end;
      {empty,{Min2,Max2}} ->
	{empty,empty,{Min2,Max2},{Min2,Max2}};
      {{Min1,Max1},empty} ->
	{{Min1,Max1},{Min1,Max1},empty,empty};
      {empty,empty} ->
	{empty,empty,empty,empty};
      {{Min1,Max1},{Min2,Max2}} ->
	{{Min1,inf_min([Max1,inf_add(Max2,-1)])},
	 {inf_max([Min1,Min2]),Max1},
	 {inf_max([inf_add(Min1,1),Min2]),Max2},
	 {Min2,inf_min([Max1,Max2])}}
    end,
  {range_init(R1_true_range, R1_other),
   range_init(R2_true_range, R2_other),
   range_init(R1_false_range, R1_other),
   range_init(R2_false_range, R2_other)}.

-spec(analyse_type/3 :: (#type{},info(),bool()) -> {#goto{} | #type{},[{label(),info()}]}).

analyse_type(Type, Info, Rewrite) ->
  Type_type = hipe_icode:type_type(Type),
  [Arg|_] = hipe_icode:type_args(Type),
  OldVarRange = get_range_from_arg(Arg),
  case Type_type of
    {integer, N} ->
      {TrueRange,FalseRange} = compare_with_integer(N,OldVarRange);
    integer ->
      TrueRange = inf(any_range(),OldVarRange),
      FalseRange = inf(none_range(),OldVarRange);
    _ ->
      TrueRange = inf(none_range(),OldVarRange),
      FalseRange = OldVarRange
  end,
  TrueLabel = hipe_icode:type_true_label(Type),
  FalseLabel = hipe_icode:type_false_label(Type),
  TrueInfo = 
    enter_define({Arg,TrueRange},Info),
  FalseInfo = 
    enter_define({Arg,FalseRange},Info),
  True = 
    case range__is_none(TrueRange) of
      true -> [];
      false -> [{TrueLabel,TrueInfo}]
    end,
  False = 
    case range__is_none(FalseRange) of
      true -> [];
      false -> [{FalseLabel,FalseInfo}]
    end,
  UpdateInfo = True++False,
  NewType =
    if Rewrite ->
	case UpdateInfo of
	  [] -> %%This is weird
	    Type;
	  [{Label,_Info}] ->
	    hipe_icode:mk_goto(Label);
	  [_,_] ->
	    Type
	end;
       true ->
	Type
    end,
  {NewType,True ++ False}.

-spec(compare_with_integer/2 :: (integer(),#range{}) -> {#range{},#range{}}).

compare_with_integer(N, OldVarRange) ->
  TestRange = range_init({N, N}, false),
  TrueRange = inf(TestRange,OldVarRange),
  %% False range
  TempFalseRange = 
    range__remove_constant(OldVarRange,TestRange),
  BetterRange = 
    case range(TempFalseRange) of
      {Min, Max} ->
	New_small = inf_geq(Min, N),
	New_large = inf_geq(N, Max),
	if New_small and not New_large ->
	    {N + 1, Max};
	   New_large and not New_small ->
	    {Min, N - 1};
	   true -> 
	    {Min, Max}
	end;
      Not_tuple ->
	Not_tuple
    end,
  FalseRange = range_init(BetterRange, other(TempFalseRange)),
  {TrueRange,FalseRange}.

%%== Ranges ==================================================================

-spec(pp_ann/1 :: (any()) -> [string()]).

pp_ann(#ann{range=#range{range=R,other=false}}) ->
  pp_range(R);
pp_ann(#ann{range=#range{range=empty,other=true},type=Type}) ->
  t_to_string(Type);
pp_ann(#ann{range=#range{range=R,other=true},type=Type}) ->
  pp_range(R) ++ " | " ++ t_to_string(Type);
pp_ann(Type) ->
  t_to_string(Type).

-spec(pp_range/1 :: ('empty' | {integer(),integer()}) -> string()).

pp_range(empty) ->
  "none";
pp_range({Min,Max}) ->
  val_to_string(Min) ++ ".." ++ val_to_string(Max).

-spec(val_to_string/1 :: (integer()) -> string()).

val_to_string(pos_inf) -> "inf";
val_to_string(neg_inf) -> "-inf";
val_to_string(X) when is_integer(X) -> integer_to_list(X).

-spec(range_from_type/1 :: (_) -> [#range{}]).

range_from_type(Type) ->
  case t_components(Type) of
    [_|_] = Types ->
      [range_from_simple_type(T) || T <- Types];
    _ ->
      [range_from_simple_type(Type)]
  end.
  
-spec(range_from_simple_type/1 :: (_) -> #range{}).

range_from_simple_type(Type) ->
  None = t_none(),
  case t_inf(t_integer(),Type) of
    None ->
      #range{range=empty,other=true};
    Type ->
      Range = {number_min(Type),number_max(Type)},
      #range{range=Range,other=false};
    NewType ->
      Range = {number_min(NewType),number_max(NewType)},
      #range{range=Range,other=true}
  end.

-spec(range_init/2 :: (range_rep(),bool()) -> #range{}).

range_init({Min,Max},Other) ->
  case inf_geq(Max,Min) of
    true ->
      #range{range={Min,Max},other=Other};
    false ->
      #range{range=empty,other=Other}
  end;
range_init(empty,Other) ->
  #range{range=empty,other=Other}.

-spec(range/1 :: (#range{}) -> range_rep()).

range(#range{range=R}) -> R.

-spec(other/1 :: (#range{}) -> bool()).

other(#range{other=O}) -> O.

-spec(set_other/2 :: (#range{},bool()) -> #range{}).

set_other(R,O) -> R#range{other=O}.

-spec(range__min/1 :: (#range{}) -> 'empty' | 'neg_inf' | integer()).

range__min(#range{range=empty}) -> empty;
range__min(#range{range={Min,_}}) -> Min.

-spec(range__max/1 :: (#range{}) -> 'empty' | 'pos_inf' | integer()).

range__max(#range{range=empty}) -> empty;
range__max(#range{range={_,Max}}) -> Max.

-spec(range__is_none/1 :: (#range{}) -> bool()).

range__is_none(#range{range=empty, other=false}) -> true;
range__is_none(#range{}) -> false.

-spec(range__is_empty/1 :: (#range{}) -> bool()).

range__is_empty(#range{range=empty}) -> true;
range__is_empty(#range{range={_,_}}) -> false.

-spec(remove_point_types/2 :: (#range{},[#range{}]) -> #range{}).

remove_point_types(Range, Ranges) ->
  Sorted = lists:sort(Ranges),
  FoldFun = fun(R,Acc) -> range__remove_constant(Acc,R) end,
  Range1 = lists:foldl(FoldFun,Range,Sorted),
  lists:foldl(FoldFun,Range1,lists:reverse(Sorted)).

-spec(range__remove_constant/2 :: (#range{},#range{}) -> #range{}).

range__remove_constant(R = #range{range={C,C}}, #range{range={C,C}}) ->
  R#range{range=empty};
range__remove_constant(R = #range{range={C,H}}, #range{range={C,C}}) ->
  R#range{range={C+1,H}};
range__remove_constant(R = #range{range={L,C}}, #range{range={C,C}}) ->
  R#range{range={L,C-1}};
range__remove_constant(R = #range{}, #range{range={C,C}}) ->
  R;
range__remove_constant(R = #range{}, _) ->
  R.

-spec(any_type/0 :: () -> #range{}).

any_type() ->
  #range{range=any_r(), other=true}.

-spec(any_range/0 :: () -> #range{}).

any_range() ->
  #range{range=any_r(), other=false}.

-spec(none_range/0 :: () -> #range{}).

none_range() ->
  #range{range=empty, other=true}.

-spec(none_type/0 :: () -> #range{}).

none_type() ->
  #range{range=empty, other=false}.

-spec(any_r/0 :: () -> {'neg_inf','pos_inf'}).

any_r() -> {neg_inf,pos_inf}.

-spec(get_range_from_args/1 :: ([argument()]) -> [#range{}]).
  
get_range_from_args(Args) ->
  [get_range_from_arg(Arg) || Arg <- Args].

-spec(get_range_from_arg/1 :: (argument()) -> #range{}).

get_range_from_arg(Arg) ->
  case hipe_icode:is_const(Arg) of
    true ->
      Value = hipe_icode:const_value(Arg),
      case is_integer(Value) of
	true ->
	  #range{range={Value,Value},other=false};
	false ->
	  #range{range=empty,other=true}
      end;
    false ->
      case Arg of
	{var,_,#ann{range=Range}} ->
	  Range;
	{var,_,Type} ->
	  range_from_simple_type(Type);
	_ ->
	  any_type()
      end
  end.

%% inf([R]) ->
%%   R;
%% inf([R1,R2|Rest]) ->
%%   inf([inf(R1,R2)|Rest]).

-spec(inf/2 :: (#range{},#range{}) -> #range{}).

inf(#range{range=R1,other=O1}, #range{range=R2,other=O2}) -> 
  #range{range=range_inf(R1,R2),other=other_inf(O1,O2)}.

-spec(range_inf/2 :: (range_rep(),range_rep()) -> range_rep()).

range_inf(empty, _) -> empty;
range_inf(_, empty) -> empty;
range_inf({Min1,Max1}, {Min2,Max2}) ->
  NewMin = inf_max([Min1,Min2]),
  NewMax = inf_min([Max1,Max2]),
  case inf_geq(NewMax,NewMin) of
    true ->
      {NewMin,NewMax};
    false ->
      empty
  end.

-spec(other_inf/2 :: (bool(),bool()) -> bool()).

other_inf(O1,O2) -> O1 and O2.

-spec(sup/1 :: ([#range{},...]) -> #range{}).

sup([R]) ->
  R;
sup([R1,R2|Rest]) ->
  sup([sup(R1,R2)|Rest]).

-spec(sup/2 :: (#range{},#range{}) -> #range{}).

sup(#range{range=R1,other=O1}, #range{range=R2,other=O2}) -> 
  #range{range=range_sup(R1,R2),other=other_sup(O1,O2)}.

-spec(range_sup/2 :: (range_rep(), range_rep()) -> range_rep()).
	 
range_sup(empty, R) -> R;
range_sup(R, empty) -> R;
range_sup({Min1,Max1}, {Min2,Max2}) ->
  NewMin = inf_min([Min1,Min2]),
  NewMax = inf_max([Max1,Max2]),
  {NewMin,NewMax}.

-spec(other_sup/2 :: (bool(),bool()) -> bool()).

other_sup(O1, O2) -> O1 or O2.

%%== Call Support =============================================================

-spec(analyse_call_or_enter_fun/4 :: 
      (fun_name(), [argument()], 'local' | 'remote' | 'primop', call_fun()) -> 
	 [#range{}]).

analyse_call_or_enter_fun(Fun, Args, CallType, LookupFun) ->
  case basic_type(Fun) of
    {bin, Operation} ->
      [Arg_range1,Arg_range2] = get_range_from_args(Args),
      A1_is_empty = range__is_empty(Arg_range1),
      A2_is_empty = range__is_empty(Arg_range2),
      if A1_is_empty or A2_is_empty ->
	  [none_type()];
	 true ->
	  [Operation(Arg_range1, Arg_range2)]
      end;
    {unary, Operation} ->
      [Arg_range] = get_range_from_args(Args),
      case range__is_empty(Arg_range) of
	true ->
	  [none_type()];
	false ->
	  [Operation(Arg_range)]
      end;
    {fcall, MFA} ->
      case CallType of
	local ->
	  Range = LookupFun(MFA, get_range_from_args(Args)),
	  case range__is_none(Range) of
	    true ->
	      throw(none_range);
	    false ->
	      [Range]
	  end;
	remote ->
	  [any_type()]
      end;
    not_int ->
      [any_type()];
    not_analysed -> 
      [any_type()];
    {hipe_bs_primop, {bs_get_integer, Size, Flags}} ->
      {Min, Max} = analyse_bs_get_integer_funs(Size, Flags, length(Args) =:= 1),
      [#range{range={Min, Max}, other=false},any_type()];
    {hipe_bs_primop, _} = Primop ->
      Type = hipe_icode_primops:type(Primop),
      range_from_type(Type)
  end.

-type(bin_operation() :: fun((#range{},#range{}) -> #range{})).
-type(unary_operation() :: fun((#range{}) -> #range{})).			    

-spec(basic_type/1 :: 
      (fun_name()) -> 'not_int' | 'not_analysed' | {bin, bin_operation()} |
			{unary, unary_operation()} | {fcall, mfa()} |
			{hipe_bs_primop, _}).
	 

%% Arithmetic operations
basic_type('+') -> {bin, fun(R1, R2) -> range_add(R1, R2) end};
basic_type('-') -> {bin, fun(R1, R2) -> range_sub(R1, R2) end};
basic_type('*') -> {bin, fun(R1, R2) -> range_mult(R1, R2) end};
basic_type('/') -> not_int;
basic_type('div') -> {bin, fun(R1, R2) -> range_div(R1, R2) end};
basic_type('rem') -> {bin, fun(R1, R2) -> range_rem(R1, R2) end};
basic_type('bor') -> {bin, fun(R1, R2) -> range_bor(R1, R2) end};
basic_type('band') -> {bin, fun(R1, R2) -> range_band(R1, R2) end};
basic_type('bxor') -> {bin, fun(R1, R2) -> range_bxor(R1, R2) end};
basic_type('bnot') -> {unary, fun(R1) -> range_bnot(R1) end};
basic_type('bsl') -> {bin, fun(R1, R2) -> range_bsl(R1, R2) end};
basic_type('bsr') -> {bin, fun(R1, R2) -> range_bsr(R1, R2) end};
%% unsafe_*
basic_type('unsafe_bor') ->  
  {bin, fun(R1, R2) -> range_bor(R1, R2) end};
basic_type('unsafe_band') ->
  {bin, fun(R1, R2) -> range_band(R1, R2) end};
basic_type('unsafe_bxor') ->
  {bin, fun(R1, R2) -> range_bxor(R1, R2) end};
basic_type('unsafe_bnot') ->
  {unary, fun(R1) -> range_bnot(R1) end};
basic_type('unsafe_bsl') ->
  {bin, fun(R1, R2) -> range_bsl(R1, R2) end};
basic_type('unsafe_bsr') ->
  {bin, fun(R1, R2) -> range_bsr(R1, R2) end};
basic_type('unsafe_add') ->
  {bin, fun(R1, R2) -> range_add(R1, R2) end};
basic_type('unsafe_sub') ->
  {bin, fun(R1, R2) -> range_sub(R1, R2) end};
basic_type('extra_unsafe_add') ->
  {bin, fun(R1, R2) -> range_add(R1, R2) end};
basic_type('extra_unsafe_sub') ->
  {bin, fun(R1, R2) -> range_sub(R1, R2) end};
%% Binaries
basic_type({hipe_bs_primop, Todo}) -> {hipe_bs_primop, Todo};
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

-spec(analyse_bs_get_integer_funs/3 :: (integer(),integer(),bool()) -> range_rep()).

analyse_bs_get_integer_funs(Size, Flags, true) ->
  Signed = Flags band 4,
  if Signed =:= 0 ->
      Max = 1 bsl Size - 1,
      Min = 0;
     true ->
      Max = 1 bsl (Size-1) - 1,
      Min = -(1 bsl (Size-1))
  end,
  {Min, Max};
analyse_bs_get_integer_funs(_Size, _Flags, false) ->
  any_r().

%%---------------------------------------------------------------------------
%% Range operations
%%---------------------------------------------------------------------------

%% Arithmetic

-spec(range_add/2 :: (#range{},#range{}) -> #range{}).

range_add(Range1, Range2) ->
  NewMin = inf_add(range__min(Range1), range__min(Range2)),
  NewMax = inf_add(range__max(Range1), range__max(Range2)),
  Other = other(Range1) orelse other(Range2),
  range_init({NewMin, NewMax}, Other).

-spec(range_sub/2 :: (#range{},#range{}) -> #range{}).

range_sub(Range1, Range2) ->
  Min_sub = inf_min([inf_inv(range__max(Range2)), 
		     inf_inv(range__min(Range2))]),
  Max_sub = inf_max([inf_inv(range__max(Range2)), 
		     inf_inv(range__min(Range2))]),
  NewMin = inf_add(range__min(Range1), Min_sub),
  NewMax = inf_add(range__max(Range1), Max_sub),
  Other = other(Range1) orelse other(Range2),
  range_init({NewMin, NewMax}, Other).

-spec(range_mult/2 :: (#range{},#range{}) -> #range{}).

range_mult(#range{range = empty, other = true}, _Range2) ->
  range_init(empty, true); 
range_mult(_Range1, #range{range = empty, other = true}) ->
  range_init(empty, true); 
range_mult(Range1, Range2) ->
  Min1 = range__min(Range1),
  Min2 = range__min(Range2),
  Max1 = range__max(Range1),
  Max2 = range__max(Range2),
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
	range(range_mult(Range2, Range1));
       GreaterMax1 -> %Kolumn 2 Rad 1 eller 2
	if GreaterMax2 -> % Kolumn 2 Rad 2
	    NewMin = inf_min([inf_mult(Min2, Max1), inf_mult(Max2, Min1)]),
	    NewMax = inf_max([inf_mult(Min2, Min1), inf_mult(Max2, Max1)]),
	    {NewMin, NewMax};
	   true -> % Kolumn 2 Rad 1
	    {inf_mult(Min2, Max1), inf_mult(Min2, Min1)}
	end;
       GreaterMax2 -> % Kolumn 1 Rad 2	
	range(range_mult(Range2, Range1));
       true -> % Kolumn 1 Rad 1 
	{inf_mult(Max1, Max2), inf_mult(Min2, Min1)}
    end,
  Other = other(Range1) orelse other(Range2),
  range_init(Range, Other).

-spec(extreme_divisors/1 :: (#range{}) -> range_rep()).

extreme_divisors(#range{range={0,0}}) -> {0,0};
extreme_divisors(#range{range={0,Max}}) -> {1,Max};
extreme_divisors(#range{range={Min,0}}) -> {Min,-1};
extreme_divisors(#range{range={Min,Max}}) ->
  case inf_geq(Min, 0) of 
    true -> {Min, Max};
    false -> %Min < 0
      case inf_geq(0, Max) of
	true -> {Min,Max}; %Max < 0
	false -> {-1,1} %Max > 0
      end
  end.

-spec(range_div/2 :: (#range{},#range{}) -> #range{}).

%% this is div, not /.
range_div(_, #range{range={0,0}}) ->
  range_init(empty, false);
range_div(#range{range=empty}, _) ->
  range_init(empty, false);
range_div(_, #range{range=empty}) ->
  range_init(empty, false);
range_div(Range1, Den) ->
  Min1 = range__min(Range1),
  Max1 = range__max(Range1),
  {Min2, Max2} = extreme_divisors(Den),
  Min_max_list = [inf_div(Min1, Min2), inf_div(Min1, Max2),
		  inf_div(Max1, Min2), inf_div(Max1, Max2)],
  range_init({inf_min(Min_max_list), inf_max(Min_max_list)}, false).

-spec(range_rem/2 :: (#range{},#range{}) -> #range{}).

range_rem(Range1, Range2) ->
  %% Range1 desides the sign of the answer.
  Min1 = range__min(Range1),
  Max1 = range__max(Range1),
  Min2 = range__min(Range2),
  Max2 = range__max(Range2),
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
  range_init({New_min, New_max}, false).

%%--- Bit operations ----------------------------

-spec(range_bsr/2 :: (#range{},#range{}) -> #range{}).

range_bsr(Range1, Range2=#range{range={Min, Max}}) -> 
  New_Range2 = range_init({inf_inv(Max), inf_inv(Min)}, other(Range2)), 
  Ans = range_bsl(Range1, New_Range2),
  %%io:format("bsr res:~w~nInput:= ~w~n",[Ans,{Range1,Range2}]),
  Ans.

-spec(range_bsl/2 :: (#range{},#range{}) -> #range{}).

range_bsl(Range1, Range2) ->
  Min1 = range__min(Range1),
  Min2 = range__min(Range2),
  Max1 = range__max(Range1),
  Max2 = range__max(Range2),
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
  range_init({Min, Max}, false).

-spec(range_bnot/1 :: (#range{}) -> #range{}).

range_bnot(Range) ->
  Minus_one = range_init({-1,-1}, false),
  range_add(range_mult(Range, Minus_one), Minus_one).

-spec(width/1 :: (range_rep() | integer()) -> 'pos_inf' | non_neg_integer()).

width({Min, Max}) -> inf_max([width(Min), width(Max)]);
width(pos_inf) -> pos_inf;
width(neg_inf) -> pos_inf;
width(X) when is_integer(X), X >= 0 -> poswidth(X, 0);
width(X) when is_integer(X), X < 0 -> negwidth(X, 0).

-spec(poswidth/2 :: (integer(),non_neg_integer()) -> non_neg_integer()).

poswidth(X, N) ->
  case X < (1 bsl N) of
    true  -> N;
    false -> poswidth(X, N+1)
  end.

-spec(negwidth/2 :: (integer(),non_neg_integer()) -> non_neg_integer()).

negwidth(X, N) ->
  case X > (-1 bsl N) of
    true  -> N;
    false -> negwidth(X, N+1)
  end.

-spec(range_band/2 :: (#range{},#range{}) -> #range{}).

range_band(R1, R2) ->
  {Min1, Max1} = range(R1),
  {Min2, Max2} = range(R2),
  Width1 = width({Min1, Max1}),
  Width2 = width({Min2, Max2}),
  Range = 
    case {classify_range(R1), classify_range(R2)} of
      {minus_minus, minus_minus} ->
	Width = inf_max([Width1, Width2]),
	{inf_bsl(-1, Width), -1};
      {minus_minus, minus_plus} ->
	Width = inf_max([Width1, Width2]),
	{inf_bsl(-1, Width), Max2};
      {minus_minus, plus_plus} ->
	{0, Max2};
      {minus_plus,  minus_minus} ->
	Width = inf_max([Width1, Width2]),
	{inf_bsl(-1, Width), Max1};
      {minus_plus,  minus_plus} ->
	Width = inf_max([Width1, Width2]),
	{inf_bsl(-1, Width), inf_max([Max1,Max2])};
      {minus_plus,  plus_plus} ->
	{0, Max2};
      {plus_plus,   minus_minus} ->
	{0, Max1};
      {plus_plus,   minus_plus} ->
	{0, Max1};
      {plus_plus,   plus_plus} ->
	{0, inf_min([Max1, Max2])}
    end,
  range_init(Range, false).  

-spec(range_bor/2 :: (#range{},#range{}) -> #range{}).

range_bor(R1, R2) ->
  {Min1, Max1} = range(R1),
  {Min2, Max2} = range(R2),
  Width1 = width({Min1, Max1}),
  Width2 = width({Min2, Max2}),
  Range = 
    case {classify_range(R1), classify_range(R2)} of
      {minus_minus, minus_minus} ->
	{inf_max([Min1, Min2]), -1};
      {minus_minus, minus_plus} ->
	{Min1, -1};
      {minus_minus, plus_plus} ->
	{Min1, -1};
      {minus_plus,  minus_minus} ->
	{Min2, -1};
      {minus_plus,  minus_plus} ->
	Width = inf_max([Width1, Width2]),
	{inf_min([Min1, Min2]), inf_add(-1,inf_bsl(1, Width))};
      {minus_plus,  plus_plus} ->
	Width = inf_max([Width1, Width2]),
	{Min1, inf_add(-1,inf_bsl(1, Width))};
      {plus_plus,   minus_minus} ->
	{Min2, -1};
      {plus_plus,   minus_plus} ->
	Width = inf_max([Width1, Width2]),
	{Min2, inf_add(-1,inf_bsl(1, Width))};
      {plus_plus,   plus_plus} ->
	Width = inf_max([Width1, Width2]),
	{0, inf_add(-1,inf_bsl(1, Width))}
    end,
  range_init(Range, false).  

-spec(classify_range/1 :: (#range{}) -> 'minus_minus' | 'minus_plus' | 'plus_plus').

classify_range(Range) ->
  case range(Range) of
    {neg_inf, Number} when is_integer(Number), Number < 0 -> minus_minus;
    {neg_inf, Number} when is_integer(Number), Number >= 0 -> minus_plus;
    {Number, pos_inf} when is_integer(Number), Number < 0 -> minus_plus;
    {Number, pos_inf} when is_integer(Number), Number >= 0 -> plus_plus;
    {neg_inf, pos_inf} -> minus_plus;
    {Number1,Number2} when is_integer(Number1), is_integer(Number2) ->
      classify_int_range(Number1, Number2)
  end.

-spec(classify_int_range/2 :: 
      (integer(),integer()) -> 'minus_minus' | 'minus_plus' | 'plus_plus').

classify_int_range(Number1,_Number2) when Number1 >= 0 ->
  plus_plus;
classify_int_range(_Number1,Number2) when Number2 < 0 ->
  minus_minus;
classify_int_range(_Number1,_Number2) ->
  minus_plus.
      
-spec(range_bxor/2 :: (#range{},#range{}) -> #range{}).
 
range_bxor(R1, R2) ->
  {Min1, Max1} = range(R1),
  {Min2, Max2} = range(R2),
  Width1 = width({Min1, Max1}),
  Width2 = width({Min2, Max2}),
  Range = 
    case {classify_range(R1), classify_range(R2)} of
      {minus_minus, minus_minus} ->
	Width = inf_max([Width1, Width2]),
	{0, inf_add(-1,inf_bsl(1, Width))};
      {minus_minus, minus_plus} ->
	MinWidth = inf_max([Width1,width({0,Max2})]),
	MaxWidth = inf_max([Width1,width({Min2,-1})]),
	{inf_bsl(-1, MinWidth), inf_add(-1,inf_bsl(1, MaxWidth))};
      {minus_minus, plus_plus} ->
	Width = inf_max([Width1, Width2]),
	{inf_bsl(-1, Width), -1};
      {minus_plus,  minus_minus} ->
	MinWidth = inf_max([Width2,width({0,Max1})]),
	MaxWidth = inf_max([Width2,width({Min1,-1})]),
	{inf_bsl(-1, MinWidth), inf_add(-1,inf_bsl(1, MaxWidth))};
      {minus_plus,  minus_plus} ->
	Width = inf_max([Width1, Width2]),
	{inf_bsl(-1, Width), inf_add(-1,inf_bsl(1, Width))};
      {minus_plus,  plus_plus} ->
	MinWidth = inf_max([Width2,width({Min1,-1})]),
	MaxWidth = inf_max([Width2,width({0,Max1})]),
	{inf_bsl(-1, MinWidth), inf_add(-1,inf_bsl(1, MaxWidth))};
      {plus_plus,   minus_minus} ->
	Width = inf_max([Width1, Width2]),
	{inf_bsl(-1, Width), -1};
      {plus_plus,   minus_plus} ->
	MinWidth = inf_max([Width1,width({Min2,-1})]),
	MaxWidth = inf_max([Width1,width({0,Max2})]),
	{inf_bsl(-1, MinWidth), inf_add(-1,inf_bsl(1, MaxWidth))};
      {plus_plus,   plus_plus} ->
	Width = inf_max([Width1, Width2]),
	{0, inf_add(-1,inf_bsl(1, Width))}
    end,
  range_init(Range, false).  

%%---------------------------------------------------------------------------
%% Inf operations
%%---------------------------------------------------------------------------

-spec(inf_max/1 :: 
      (['neg_inf' | 'pos_inf' | integer(),...]) -> 
	 'neg_inf' | 'pos_inf' | integer()).

%% This clause is not needed at the moment
inf_max([H|T])->
  lists:foldl(fun(Elem, Max) ->
		  Geq = inf_geq(Elem, Max),
		  if not Geq ->
		      Max;
		     true ->
		      Elem
		  end
	      end,
	      H,
	      T).


-spec(inf_min/1 :: 
      (['neg_inf' | 'pos_inf' | integer(),...]) -> 
	 'neg_inf' | 'pos_inf' | integer()).

%% This clause is not needed at the moment
%%inf_min([]) -> empty;
inf_min([H|T])->
  lists:foldl(fun(Elem, Min) ->
		  Geq = inf_geq(Elem, Min),
		  if Geq ->
		      Min;
		     true ->
		      Elem
		  end
	      end,
	      H,
	      T).

-spec(inf_abs/1 :: ('neg_inf' | 'pos_inf' | integer()) -> 'pos_inf' | integer()).

inf_abs(pos_inf) -> pos_inf;
inf_abs(neg_inf) -> pos_inf;
inf_abs(Number) when is_integer(Number), (Number < 0) -> - Number;
inf_abs(Number) when is_integer(Number) -> Number.

-spec(inf_add/2 :: 
      ('empty' | 'neg_inf' | 'pos_inf' | integer(),_) -> 
	 'neg_inf' | 'pos_inf' | integer()).

inf_add(pos_inf, _Number) -> pos_inf;
inf_add(neg_inf, _Number) -> neg_inf;
inf_add(_Number, pos_inf) -> pos_inf;
inf_add(_Number, neg_inf) -> neg_inf;
inf_add(Number1, Number2) when is_integer(Number1), is_integer(Number2) ->
  Number1 + Number2.

-spec(inf_inv/1 :: 
      ('neg_inf' | 'pos_inf' | integer()) -> 
	 'neg_inf' | 'pos_inf' | integer()).

inf_inv(pos_inf) -> neg_inf;
inf_inv(neg_inf) -> pos_inf;
inf_inv(Number) -> -Number.

-spec(inf_geq/2 :: 
      ('neg_inf' | 'pos_inf' | integer(), 'neg_inf' | 'pos_inf' | integer()) -> 
	 bool()).

inf_geq(pos_inf, _) -> true;
inf_geq(_, pos_inf) -> false;
inf_geq(_, neg_inf) -> true;
inf_geq(neg_inf, _) -> false;
inf_geq(A, B) -> A >= B.

-spec(inf_greater_zero/1 :: ('neg_inf' | 'pos_inf' | integer()) -> bool()).

inf_greater_zero(pos_inf) -> true;
inf_greater_zero(neg_inf) -> false;
inf_greater_zero(Number) when is_integer(Number), Number >= 0 -> true;
inf_greater_zero(Number) when is_integer(Number), Number < 0 -> false.

-spec(inf_div/2 :: 
      ('neg_inf' | 'pos_inf' | integer(),'neg_inf' | 'pos_inf' | integer()) -> 
	 'neg_inf' | 'pos_inf' | integer()).

inf_div(Number, 0) ->
  Greater = inf_greater_zero(Number),
  if Greater -> pos_inf;
     true -> neg_inf
  end;
inf_div(pos_inf, Number) ->
  Greater = inf_greater_zero(Number),
  if Greater -> pos_inf;
     true -> neg_inf
  end;
inf_div(neg_inf, Number) ->
  Greater = inf_greater_zero(Number),
  if Greater -> neg_inf;
     true -> pos_inf
  end;
inf_div(Number, pos_inf) -> 
  Greater = inf_greater_zero(Number),
  if Greater -> pos_inf;
     true -> neg_inf
  end;
inf_div(Number, neg_inf) ->
  Greater = inf_greater_zero(Number),
  if Greater -> neg_inf;
     true -> pos_inf
  end;
inf_div(Number1, Number2) -> Number1 div Number2.

-spec(inf_mult/2 :: 
      ('neg_inf' | 'pos_inf' | integer(), 'neg_inf' | 'pos_inf' | integer()) -> 
	 'neg_inf' | 'pos_inf' | integer()).

inf_mult(neg_inf, Number) -> 
  Greater = inf_greater_zero(Number), 
  if Greater -> neg_inf;
     true -> pos_inf
  end;
inf_mult(pos_inf, Number) -> 
  Greater = inf_greater_zero(Number),
  if Greater -> pos_inf;
     true -> neg_inf
  end;
inf_mult(Number, pos_inf) -> inf_mult(pos_inf, Number);
inf_mult(Number, neg_inf) -> inf_mult(neg_inf, Number);
inf_mult(Number1, Number2) -> Number1 * Number2.

-spec(inf_bsl/2 :: ('empty' | 'neg_inf' | 'pos_inf' | integer(),_) -> 
	 'neg_inf' | 'pos_inf' | integer()).

inf_bsl(pos_inf, _) -> pos_inf;
inf_bsl(neg_inf, _) -> neg_inf;
inf_bsl(Number, pos_inf) when is_integer(Number), Number >= 0 -> pos_inf;
inf_bsl(_, pos_inf) -> neg_inf;
inf_bsl(Number, neg_inf) when is_integer(Number), Number >= 0 -> 0;
inf_bsl(_Number, neg_inf) -> -1;
inf_bsl(Number1, Number2) when is_integer(Number1), is_integer(Number2) ->
  %% We can not shift left with a number which is not a fixnum. We
  %% don't have enough memory.
  Bits = ?BITS,
  if Number2 > (Bits bsl 1) -> inf_bsl(Number1, pos_inf);
     Number2 < (-Bits bsl 1) ->	inf_bsl(Number1, neg_inf);
     true -> Number1 bsl Number2
  end.

%% State

-spec(state__init/2 :: (cfg(),data()) -> #state{}).

state__init(Cfg,{MFA,ArgsFun,CallFun,FinalFun}) ->
  Start = hipe_icode_cfg:start_label(Cfg),  
  Params = hipe_icode_cfg:params(Cfg),
  Ranges = ArgsFun(MFA,Cfg),
  %%io:format("MFA: ~w~nRanges: ~w~n",[MFA,Ranges]),
  Liveness = hipe_icode_ssa:ssa_liveness__analyze(
	       hipe_icode_type:unannotate_cfg(Cfg)),
  case lists:any(fun range__is_none/1, Ranges) of
    true -> 
      FinalFun(MFA,[none_type()]),
      throw(no_input);
    false ->
      NewParams = lists:zipwith(fun update_info/2, Params, Ranges),
      NewCfg = hipe_icode_cfg:params_update(Cfg, NewParams),
      Info = enter_defines(NewParams,gb_trees:empty()),
      InfoMap = gb_trees:insert({Start, in}, Info, gb_trees:empty()),
      #state{info_map=InfoMap, cfg=NewCfg, liveness=Liveness,
	     ret_type=none_type(),
	     lookupfun=CallFun, resultaction=FinalFun}
  end.

-spec(state__cfg/1 :: (#state{}) -> cfg()).

state__cfg(#state{cfg=Cfg}) ->
  Cfg.

-spec(state__bb/2 :: (#state{},label()) -> bb()).

state__bb(#state{cfg=Cfg}, Label) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  true = hipe_bb:is_bb(BB), % Just an assert
  BB.
  
-spec(state__bb_add/3 :: (#state{},label(),bb()) -> #state{}).

state__bb_add(S=#state{cfg=Cfg}, Label, BB) ->
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__lookupfun(#state{lookupfun = LF}) -> LF.

state__resultaction(#state{resultaction = RA}) -> RA.

state__ret_type(#state{ret_type = RT}) -> RT.

state__ret_type_update(#state{ret_type=RT} = State, NewType) ->
  TotType = sup(RT, NewType),
  State#state{ret_type=TotType}.

state__info_in(S, Label) ->
  state__info(S, {Label, in}).

state__info(#state{info_map=IM}, Key) ->
  gb_trees:get(Key,IM).

state__update_info(State,LabelInfo,Rewrite) ->
  update_info(LabelInfo,State,[],Rewrite).

update_info([{Label,InfoIn}|Rest],State,LabelAcc,Rewrite) ->
  case state__info_in_update(State, Label, InfoIn) of
    fixpoint ->
      if Rewrite ->
	  update_info(Rest,State,[Label|LabelAcc],Rewrite);
	 true ->
	  update_info(Rest,State,LabelAcc,Rewrite)
      end;
    NewState ->
      update_info(Rest,NewState,[Label|LabelAcc],Rewrite)
  end;
update_info([], State, LabelAcc,_Rewrite) ->
  {State, LabelAcc}.

state__info_in_update(S=#state{info_map=IM,liveness=Liveness}, Label, Info) ->
  case gb_trees:lookup({Label, in}, IM) of
    none -> 
      LiveIn = hipe_icode_ssa:ssa_liveness__livein(Liveness, Label),
      NamesLiveIn = [Name || {var,Name} <- LiveIn],
      OldInfo = gb_trees:empty(),
      case join_info_in(NamesLiveIn, OldInfo, Info) of
	fixpoint -> 
	  S#state{info_map=gb_trees:insert({Label,in},OldInfo,IM)};
	NewInfo ->
	  S#state{info_map=gb_trees:enter({Label, in},NewInfo,IM)}
      end;
    {value, OldInfo} ->
      OldVars = gb_trees:keys(OldInfo),
      case join_info_in(OldVars, OldInfo, Info) of
	fixpoint -> 
	  fixpoint;
	NewInfo ->
	  S#state{info_map=gb_trees:update({Label,in},NewInfo,IM)}
      end
  end.
	  
join_info_in(Vars, OldInfo, NewInfo) ->
  case join_info_in(Vars, OldInfo, NewInfo, gb_trees:empty(), false) of
    {Res, true} -> Res;
    {_, false} -> fixpoint
  end.
  
join_info_in([Var|Left], Info1, Info2, Acc, Changed) ->
  Type1 = gb_trees:lookup(Var, Info1),
  Type2 = gb_trees:lookup(Var, Info2),
  case {Type1, Type2} of
    {none, none} ->
      NewTree = gb_trees:insert(Var, none_type(), Acc),
      join_info_in(Left, Info1, Info2, NewTree, true);
    {none, {value, Val}} ->
      NewTree = gb_trees:insert(Var, Val, Acc),
      join_info_in(Left, Info1, Info2, NewTree, true);
    {{value, Val}, none} ->
      NewTree = gb_trees:insert(Var, Val, Acc),
      join_info_in(Left, Info1, Info2, NewTree, Changed);
    {{value, Val}, {value, Val}} ->
      NewTree = gb_trees:insert(Var, Val, Acc),
      join_info_in(Left, Info1, Info2, NewTree, Changed);
    {{value, Val1}, {value, Val2}} ->
      NewVal = 
	case sup(Val1,Val2) of
	  Val1 -> 
	    NewChanged = Changed,
	    Val1;
	  Val ->
	    NewChanged = true,
	    Val
	end,
      NewTree = gb_trees:insert(Var, NewVal, Acc),
      join_info_in(Left, Info1, Info2, NewTree, NewChanged)
  end;
join_info_in([], _Info1, _Info2, Acc, NewChanged) ->
  {Acc, NewChanged}.

enter_defines([Def|Rest],Info) ->
  enter_defines(Rest,enter_define(Def,Info));
enter_defines([], Info) -> Info.

enter_define({{var,Name,_},Range}, Info) ->
  gb_trees:enter(Name,Range,Info);
enter_define({var,Name,#ann{range=Range}}, Info) ->
  gb_trees:enter(Name,Range,Info);
enter_define(_, Info) -> Info.

enter_vals(Ins,Info) ->
  NewInfo = enter_defines(hipe_icode:args(Ins),Info),
  enter_defines(hipe_icode:defines(Ins),NewInfo).
  
lookup({var,Name,_},Info) ->
  case gb_trees:lookup(Name,Info) of
    none ->
      none_type();
    {value,Val} ->
      Val
  end;
lookup({reg,_,_},_Info) ->
  any_type();
lookup({fvar,_},_Info) ->
  none_range().

%% _________________________________________________________________
%%
%% The worklist.
%%

init_work(State) ->
  %%Labels = hipe_icode_cfg:reverse_postorder(state__cfg(State)),
  Labels = [hipe_icode_cfg:start_label(state__cfg(State))],
  {Labels, [], sets:from_list(Labels)}.

get_work({[Label|Left], List, Set}) ->
  NewWork = {Left, List, sets:del_element(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set},[Label|Left]) ->
  case sets:is_element(Label, Set) of
    true ->
      add_work(Work, Left);
    false ->
      %%io:format("Adding work: ~w\n", [Label]),
      add_work({List1, [Label|List2], sets:add_element(Label, Set)}, Left)
  end;
add_work(Work, []) ->
  Work.

convert_cfg_to_types(Cfg) ->
  Lbls = hipe_icode_cfg:reverse_postorder(Cfg),
  lists:foldl(fun convert_lbl_to_type/2, Cfg, Lbls).

convert_lbl_to_type(Lbl, Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Lbl),
  Code = hipe_bb:code(BB),
  NewCode = [convert_instr_to_type(I) || I <- Code],
  hipe_icode_cfg:bb_add(Cfg, Lbl, hipe_bb:mk_bb(NewCode)).

convert_instr_to_type(I) ->
  Uses = hipe_icode:uses(I),
  UseSubstList = [{Use,{var,Name,convert_ann_to_types(Ann)}} ||
		   Use = {var,Name,Ann = #ann{}} <- Uses],
  NewI = hipe_icode:subst_uses(UseSubstList,I),
  Defs = hipe_icode:defines(NewI),
  DefSubstList = [{Def,{var,Name,convert_ann_to_types(Ann)}} ||
		   Def = {var,Name,Ann = #ann{}} <- Defs],
  hipe_icode:subst_defines(DefSubstList, NewI).

convert_ann_to_types(#ann{range=#range{range={Min,Max}, other=false}}) ->
  t_from_range_unsafe(Min, Max);
convert_ann_to_types(#ann{range=#range{range=empty, other=false}}) ->
  t_none();
convert_ann_to_types(#ann{range=#range{other=true}, type=Type}) ->
  Type.

%%=====================================================================
%% Icode Coordinator Callbacks
%%=====================================================================

replace_nones(Args) ->
  [replace_none(Arg) || Arg <- Args].

replace_none(Arg) ->
  case range__is_none(Arg) of
    true -> any_type();
    false -> Arg
  end.
      
update__info(NewRanges, OldRanges) ->
  SupFun = fun(Ann, Range) -> 
	       join_info(Ann, Range, fun safe_widen/3)
	   end,
  EqFun = fun(X,Y) -> X =:= Y end,
  ResRanges = lists:zipwith(SupFun, OldRanges, NewRanges),
  Change = lists:zipwith(EqFun, ResRanges, OldRanges),
  {lists:all(fun(X) -> X end, Change), ResRanges}.

new__info(NewRanges) ->
  [#ann{range=Range,count=1,type=t_any()} || Range <- NewRanges].

return__info(Ranges) ->
  [Range || #ann{range=Range} <- Ranges].

return_none() ->
  [none_type()].

return_none_args(Cfg, {_M,_F,A}) ->
  NoArgs =
    case hipe_icode_cfg:is_closure(Cfg) of
      true -> hipe_icode_cfg:closure_arity(Cfg) + 1;
      false -> A
    end,
  lists:duplicate(NoArgs, none_type()).

return_any_args(Cfg, {_M,_F,A}) ->
  NoArgs = 
    case hipe_icode_cfg:is_closure(Cfg) of
      true -> hipe_icode_cfg:closure_arity(Cfg) + 1;
      false -> A
    end,
  lists:duplicate(NoArgs, any_type()).

%%=====================================================================

next_up_limit(X) when is_integer(X), X < 0 -> 0;
next_up_limit(X) when is_integer(X), X < 255 -> 255;
next_up_limit(X) when is_integer(X), X < 16#10ffff -> 16#10ffff;
next_up_limit(X) when is_integer(X), X < 16#7ffffff -> 16#7ffffff;
next_up_limit(X) when is_integer(X), X < 16#7fffffff -> 16#7fffffff;
next_up_limit(X) when is_integer(X), X < 16#ffffffff -> 16#ffffffff;
next_up_limit(X) when is_integer(X), X < 16#fffffffffff -> 16#fffffffffff;
next_up_limit(X) when is_integer(X), X < 16#7fffffffffffffff -> 16#7fffffffffffffff;
next_up_limit(_X) -> pos_inf.

next_down_limit(X) when is_integer(X), X > 0 -> 0;
next_down_limit(X) when is_integer(X), X > -256 -> -256;
next_down_limit(X) when is_integer(X), X > -16#10ffff -> -16#10ffff;
next_down_limit(X) when is_integer(X), X > -16#8000000 -> -16#8000000;
next_down_limit(X) when is_integer(X), X > -16#80000000 -> -16#80000000;
next_down_limit(X) when is_integer(X), X > -16#800000000000000 -> -16#800000000000000;
next_down_limit(_X) -> neg_inf.
