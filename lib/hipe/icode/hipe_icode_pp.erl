%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2003 by Erik Stenman.  
%% ====================================================================
%%  Filename : 	hipe_icode_pp.erl
%%  Module   :	hipe_icode_pp
%%  Purpose  :  Pretty-printer for Icode.
%%  Notes    : 
%%  History  :	* 2003-04-16 (stenman@epfl.ch): Created.
%%  CVS      :
%%              $Author$
%%              $Date$
%%              $Revision$
%% ====================================================================
%% 
%% @doc
%%   Icode Pretty-Printer.
%% @end
%%
%% ====================================================================

-module(hipe_icode_pp).

-export([pp/1, pp/2, pp_block/1]).

-ifdef(DEBUG_ICODE).
-export([pp_instrs/2, pp_exit/1]).
-endif.

-include("hipe_icode.hrl").

%%---------------------------------------------------------------------

%% @spec pp(Icode::hipe_icode:icode()) -> ok
%%
%% @doc Prettyprints linear Icode on stdout.
%%  <p> Badly formed or unknown instructions are printed surrounded
%%      by three stars "***".</p>
pp(Icode) ->
  pp(standard_io, Icode).

%% @spec pp(IoDevice::iodevice(), Icode::hipe_icode:icode()) -> ok
%%
%% @doc Prettyprints linear Icode on IoDevice.
%%  <p> Badly formed or unknown instructions are printed surrounded by
%%      three stars "***".</p>
pp(Dev, Icode) ->
  {Mod, Fun, Arity} = hipe_icode:icode_fun(Icode),
  Args = hipe_icode:icode_params(Icode),
  io:format(Dev, "~w:~w/~w(", [Mod, Fun, Arity]),
  pp_args(Dev, Args),
  io:format(Dev, ") ->~n", []),
  Info = lists:map(fun(X)-> case X of
			      {arg_type, AT} -> 
				{arg_type, 
				 [erl_types:t_to_string(Y)
				  ||Y <- AT]};
			      _  -> 
				X
			    end
		   end,
		   hipe_icode:icode_info(Icode)),
  io:format(Dev, "%% Info:~p\n",
	    [[case hipe_icode:icode_is_closure(Icode) of
		true -> 'Closure'; 
		false -> 'Not a closure'
	      end,
	      case hipe_icode:icode_is_leaf(Icode) of
		true -> 'Leaf function'; 
		false -> 'Not a leaf function'
	      end |
	      Info]]),
  pp_instrs(Dev, hipe_icode:icode_code(Icode)),
  io:format(Dev, "%% Data:\n", []),
  hipe_data_pp:pp(Dev, hipe_icode:icode_data(Icode), icode, "").

pp_block(Code) ->
  pp_instrs(standard_io, Code).

%% @spec pp_instrs(iodevice(), [hipe_icode:icode_instruction()]) -> ok
%%
%% @doc Prettyprints a list of Icode instructions. Badly formed or
%%      unknown instructions are printed surrounded by three stars
%%      "***".
pp_instrs(_Dev, []) ->
  ok;
pp_instrs(Dev, [I|Is]) ->
  case catch pp_instr(Dev, I) of
    {'EXIT',_Rsn} ->
      io:format(Dev, '*** ~w ***~n',[I]);
    _ ->
      ok
  end,
  pp_instrs(Dev, Is).

%%---------------------------------------------------------------------

-ifdef(DEBUG_ICODE).

%% @spec (Icode::hipe_icode:icode()) -> ok
%%
%% @doc Prettyprints linear Icode on stdout.
%%      Badly formed or unknown instructions generate an exception.
pp_exit(Icode) ->
  pp_exit(standard_io, Icode).

%% @clear
%% @spec (IoDevice::iodevice(), Icode::hipe_icode:icode()) -> ok
%%
%% @doc Prettyprints linear Icode on IoDevice.
%%      Badly formed or unknown instructions generate an exception.
%% @end
pp_exit(Dev, Icode) ->
  {Mod, Fun, _Arity} = hipe_icode:icode_fun(Icode),
  Args = hipe_icode:icode_params(Icode),
  io:format(Dev, "~w:~w(", [Mod, Fun]),
  pp_args(Dev, Args),
  io:format(Dev, ") ->~n", []),
  pp_instrs_exit(Dev, hipe_icode:icode_code(Icode)).

pp_instrs_exit(_Dev, []) ->
  ok;
pp_instrs_exit(Dev, [I|Is]) ->
  case catch pp_instr(Dev, I) of
    {'EXIT',_Rsn} ->
      exit({pp,I});
    _ ->
      ok
  end,
  pp_instrs_exit(Dev, Is).

-endif.

%%---------------------------------------------------------------------

pp_instr(Dev, I) ->
  case I of 
    #label{} ->
      io:format(Dev, "~p:~n", [hipe_icode:label_name(I)]);
    #comment{} ->
      Txt = hipe_icode:comment_text(I),
      Str = case io_lib:deep_char_list(Txt) of
	      true -> Txt;
	      false -> io_lib:format("~p", [Txt])
	    end,
      io:format(Dev, "    % ~s~n", [Str]);
    #phi{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, hipe_icode:phi_dst(I)),
      io:format(Dev, " := phi(", []),
      pp_phi_args(Dev, hipe_icode:phi_arglist(I)),
      io:format(Dev, ")~n", []);
    #move{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, hipe_icode:move_dst(I)),
      io:format(Dev, " := ", []),
      pp_arg(Dev, hipe_icode:move_src(I)),
      io:format(Dev, "~n", []);
    #call{} ->
      io:format(Dev, "    ", []),
      case hipe_icode:call_dstlist(I) of
	[] -> %% result is unused -- e.g. taken out by dead code elimination
	  io:format(Dev, "_ := ", []);
	DstList ->
	  pp_args(Dev, DstList),
	  io:format(Dev, " := ", [])
      end,
      pp_fun(Dev, hipe_icode:call_fun(I),
	     hipe_icode:call_args(I),
	     hipe_icode:call_type(I),
	     hipe_icode:call_in_guard(I)),
      case hipe_icode:call_continuation(I) of
	[] ->
	  ok;
	CC ->
	  io:format(Dev, " -> ~w", [CC])
      end,
      case hipe_icode:call_fail_label(I) of
	[] ->  io:format(Dev, "~n", []);
	Fail ->  io:format(Dev, ", #fail ~w~n", [Fail])
      end;
    #enter{} ->
      io:format(Dev, "    ", []),
      pp_fun(Dev, hipe_icode:enter_fun(I),
	     hipe_icode:enter_args(I),
	     hipe_icode:enter_type(I)),
      io:format(Dev, "~n", []);
    #return{} ->
      io:format(Dev, "    return(", []),
      pp_args(Dev, hipe_icode:return_vars(I)),
      io:format(Dev, ")~n", []);
    #begin_try{} ->
      io:format(Dev, "    begin_try -> ~w cont ~w~n", 
		[hipe_icode:begin_try_label(I), 
		 hipe_icode:begin_try_successor(I)]);
    #begin_handler{} ->
      io:format(Dev, "    ", []),
      pp_args(Dev, hipe_icode:begin_handler_dstlist(I)),
      io:format(Dev, " := begin_handler()~n",[]);
    #end_try{} ->
      io:format(Dev, "    end_try~n", []);
    #fail{} ->
      Type = hipe_icode:fail_class(I),
      io:format(Dev, "    fail(~w, [", [Type]),
      pp_args(Dev, hipe_icode:fail_args(I)),
      case hipe_icode:fail_label(I) of
	[] ->  io:put_chars(Dev, "])\n");
	Fail ->  io:format(Dev, "]) -> ~w\n", [Fail])
      end;
    #'if'{} ->
      io:format(Dev, "    if ~w(", [hipe_icode:if_op(I)]),
      pp_args(Dev, hipe_icode:if_args(I)),
      io:format(Dev, ") then ~p (~.2f) else ~p~n", 
		[hipe_icode:if_true_label(I), hipe_icode:if_pred(I),
		 hipe_icode:if_false_label(I)]);
    #switch_val{} ->
      io:format(Dev, "    switch_val ",[]),
      pp_arg(Dev, hipe_icode:switch_val_arg(I)),
      pp_switch_cases(Dev, hipe_icode:switch_val_cases(I)),
      io:format(Dev, "    fail -> ~w\n", 
		[hipe_icode:switch_val_fail_label(I)]);
    #switch_tuple_arity{} ->
      io:format(Dev, "    switch_tuple_arity ",[]),
      pp_arg(Dev, hipe_icode:switch_tuple_arity_arg(I)),
      pp_switch_cases(Dev,hipe_icode:switch_tuple_arity_cases(I)),
      io:format(Dev, "    fail -> ~w\n", 
		[hipe_icode:switch_tuple_arity_fail_label(I)]);
    #type{} ->
      io:format(Dev, "    if is_", []),
      pp_type(Dev, hipe_icode:type_type(I)),
      io:format(Dev, "(", []),
      pp_args(Dev, hipe_icode:type_args(I)),
      io:format(Dev, ") then ~p (~.2f) else ~p~n", 
		[hipe_icode:type_true_label(I), hipe_icode:type_pred(I), 
		 hipe_icode:type_false_label(I)]);
    #goto{} ->
      io:format(Dev, "    goto ~p~n", [hipe_icode:goto_label(I)]);
    #fmove{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, hipe_icode:fmove_dst(I)),
      io:format(Dev, " f:= ", []),
      pp_arg(Dev, hipe_icode:fmove_src(I)),
      io:format(Dev, "~n", [])
  end.

pp_fun(Dev, Fun, Args, Type) ->
  pp_fun(Dev, Fun, Args, Type, false).

pp_fun(Dev, Fun, Args, Type, Guard) ->
  case Type of
    primop ->
      hipe_icode_primops:pp(Fun, Dev);
    local ->
      {_,F,A} = Fun,
      io:format(Dev, "~w/~w", [F, A]);
    remote ->
      {M,F,A} = Fun,
      io:format(Dev, "~w:~w/~w", [M, F, A])
  end,
  io:format(Dev, "(", []),
  pp_args(Dev, Args),
  case Guard of
    true ->
      case Type of
	primop ->
	  io:format(Dev, ") (primop,guard)", []);
	_ ->
	  io:format(Dev, ") (guard)", [])
      end;
    false ->
      case Type of
	primop ->
	  io:format(Dev, ") (primop)", []);
	_ ->
	  io:format(Dev, ")", [])
      end
  end.

pp_arg(Dev, {var, V, {T, R}}) ->
  io:format(Dev, "v~p (~s, ~s)", [V, erl_types:t_to_string(T), hipe_icode_range_an:to_string(R)]);
pp_arg(Dev, {var, V, T}) ->
  io:format(Dev, "v~p (~s)", [V, erl_types:t_to_string(T)]);
pp_arg(Dev, {var, V}) ->
  io:format(Dev, "v~p", [V]);
pp_arg(Dev, {fvar, V}) ->
  io:format(Dev, "fv~p", [V]);
pp_arg(Dev, {reg, V}) -> 
  io:format(Dev, "r~p", [V]);
pp_arg(Dev, C) ->
  Const = hipe_icode:const_value(C), 
  case is_string(Const) of
    true ->
      io:format(Dev, "~p", [Const]);
    false ->
      io:format(Dev, "~w", [Const])
  end.

pp_args(_Dev, []) -> ok;
pp_args(Dev, [A]) ->
  pp_arg(Dev, A);
pp_args(Dev, [A|Args]) ->
  pp_arg(Dev, A),
  io:format(Dev, ", ", []),
  pp_args(Dev, Args).

pp_phi_args(_Dev, []) -> ok;
pp_phi_args(Dev, [{Pred,A}]) ->
  io:format(Dev, "{~w, ", [Pred]),
  pp_arg(Dev, A),
  io:format(Dev, "}", []);
pp_phi_args(Dev, [{Pred,A}|Args]) ->
  io:format(Dev, "{~w, ", [Pred]),
  pp_arg(Dev, A),
  io:format(Dev, "}, ", []),
  pp_phi_args(Dev, Args);
pp_phi_args(Dev, Args) ->
  pp_args(Dev, Args).

pp_type(Dev, T) ->
  io:format(Dev, "~w", [T]).

pp_switch_cases(Dev, Cases) ->
  io:format(Dev, " of\n",[]),
  pp_switch_cases(Dev, Cases,1),
  io:format(Dev, "",[]).

pp_switch_cases(Dev, [{Val,L}], _Pos) -> 
  io:format(Dev, "        ",[]),
  pp_arg(Dev, Val),
  io:format(Dev, " -> ~w\n", [L]);
pp_switch_cases(Dev, [{Val, L}|Ls], Pos) -> 
  io:format(Dev, "        ",[]),
  pp_arg(Dev, Val),
  io:format(Dev, " -> ~w;\n", [L]),
  NewPos = Pos,
  %%    case Pos of
  %%      5 -> io:format(Dev, "\n              ",[]),
  %%	   0;
  %%      N -> N + 1
  %%    end,
  pp_switch_cases(Dev, Ls, NewPos);
pp_switch_cases(_Dev, [], _) -> ok.

is_string([X|Rest]) when X > 0, X < 256 ->
  is_string(Rest);
is_string([]) ->
  true;
is_string(_) ->
  false.
