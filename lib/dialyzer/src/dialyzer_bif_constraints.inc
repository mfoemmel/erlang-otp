%% This is an -*- erlang -*- file

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
%% $Id$
%%

get_bif_constr({erlang, Op, 2}, Dst, Args = [Arg1, Arg2], _State) 
  when Op =:= '+'; Op =:= '-'; Op =:= '*' ->
  ReturnType = mk_fun_var(fun(Map) ->
			      TmpArgTypes = lookup_type_list(Args, Map),
			      erl_bif_types:type(erlang, Op, 2, TmpArgTypes)
			  end, Args),
  ArgFun = 
    fun(A, Pos) ->
	F = 
	  fun(Map)->
	      DstType = lookup_type(Dst, Map),
	      AType = lookup_type(A, Map),
	      case t_is_integer(DstType) of
		true ->
		  case t_is_integer(AType) of
		    true -> 
		      eval_inv_arith(Op, Pos, DstType, AType);
		    false  ->
		      %% This must be temporary.
		      t_integer()
		  end;
		false ->
		  case t_is_float(DstType) of
		    true -> 
		      case t_is_integer(AType) of
			true -> t_float();
			false -> t_number()
		      end;
		    false ->
		      t_number()
		  end
	      end
	  end,
	mk_fun_var(F, [Dst, A])
    end,
  Arg1FunVar = ArgFun(Arg2, 2),
  Arg2FunVar = ArgFun(Arg1, 1),
  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType),
			   mk_constraint(Arg1, sub, Arg1FunVar),
			   mk_constraint(Arg2, sub, Arg2FunVar)]);
get_bif_constr({erlang, Op, 2}, Dst, Args = [Arg1, Arg2], _State) 
  when Op =:= '<'; Op =:= '=<'; Op =:= '>'; Op =:= '>=' ->
  ArgFun = 
    fun(LocalArg1, LocalArg2, LocalOp) ->
	fun(Map) ->
	    DstType = lookup_type(Dst, Map),
	    IsTrue = t_is_atom(true, DstType),
	    IsFalse = t_is_atom(false, DstType),
	    case IsTrue orelse IsFalse of
	      true ->
		Arg1Type = lookup_type(LocalArg1, Map),
		Arg2Type = lookup_type(LocalArg2, Map),
		case t_is_integer(Arg1Type) andalso t_is_integer(Arg2Type) of
		  true ->
		    Max1 = erl_types:number_max(Arg1Type),
		    Min1 = erl_types:number_min(Arg1Type),
		    Max2 = erl_types:number_max(Arg2Type),
		    Min2 = erl_types:number_min(Arg2Type),
		    case LocalOp of
		      '=<' -> 
			if IsTrue  -> t_from_range(Min1, Max2);
			   IsFalse -> t_from_range(range_inc(Min2), Max1)
			end;
		      '<'  -> 
			if IsTrue  -> t_from_range(Min1, range_dec(Max2));
			   IsFalse -> t_from_range(Min2, Max1)
			end;
		      '>=' -> 
			if IsTrue  -> t_from_range(Min2, Max1);
			   IsFalse -> t_from_range(Min1, range_dec(Max2))
			end;
		      '>'  -> 
			if IsTrue  -> t_from_range(range_inc(Min2), Max1);
			   IsFalse -> t_from_range(Min1, Max2)
			end
		    end;
		  false -> t_any()
		end;
	      false -> t_any()
	    end
	end
    end,
  {Arg1Fun, Arg2Fun} =
    case Op of
      '<'  -> {ArgFun(Arg1, Arg2, '<'),  ArgFun(Arg2, Arg1, '>=')};
      '=<' -> {ArgFun(Arg1, Arg2, '=<'), ArgFun(Arg2, Arg1, '>=')};
      '>'  -> {ArgFun(Arg1, Arg2, '>'),  ArgFun(Arg2, Arg1, '<')};
      '>=' -> {ArgFun(Arg1, Arg2, '>='), ArgFun(Arg2, Arg1, '=<')}
    end,
  Arg1Var = mk_fun_var(Arg1Fun, [Dst, Arg1, Arg2]),
  Arg2Var = mk_fun_var(Arg2Fun, [Dst, Arg1, Arg2]),
  DstVar = mk_fun_var(fun(Map)-> 
			  TmpArgTypes = lookup_type_list(Args, Map),
			  erl_bif_types:type(erlang, Op, 2, TmpArgTypes)
		      end, Args),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstVar),
			   mk_constraint(Arg1, sub, Arg1Var),
			   mk_constraint(Arg2, sub, Arg2Var)]);
get_bif_constr({erlang, '++', 2}, Dst, Args = [Hd, Tl], _State) ->
  HdFun = fun(Map) ->
	      DstType = lookup_type(Dst, Map),
	      case t_is_cons(DstType) of
		true -> t_list(t_cons_hd(DstType));
		false -> 
		  case t_is_list(DstType) of
		    true -> 
		      case t_is_nil(DstType) of
			true -> DstType;
			false -> t_list(t_list_elements(DstType))
		      end;
		    false -> t_list()
 		  end
	      end
	  end,
  TlFun = fun(Map) ->
	      DstType = lookup_type(Dst, Map),
	      case t_is_cons(DstType) of
		true -> t_sup(t_cons_tl(DstType), DstType);
		false ->
		  case t_is_list(DstType) of
		    true -> 
		      case t_is_nil(DstType) of
			true -> DstType;
			false -> t_list(t_list_elements(DstType))
		      end;
		    false -> t_any()
		  end
	      end
	  end,
  HdVar = mk_fun_var(HdFun, [Dst]),  
  TlVar = mk_fun_var(TlFun, [Dst]),
  ArgTypes = erl_bif_types:arg_types(erlang, '++', 2),
  ReturnType = mk_fun_var(fun(Map)-> 
			      TmpArgTypes = lookup_type_list(Args, Map),
			      erl_bif_types:type(erlang, '++', 2, TmpArgTypes)
			  end, Args),
  Cs = mk_constraints(Args, sub, ArgTypes),
  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType),
			   mk_constraint(Hd, sub, HdVar),
			   mk_constraint(Tl, sub, TlVar)
			   |Cs]);
get_bif_constr({erlang, is_atom, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_atom());
get_bif_constr({erlang, is_binary, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_binary());
get_bif_constr({erlang, is_boolean, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_bool());
get_bif_constr({erlang, is_float, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_float());
get_bif_constr({erlang, is_function, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_fun());
get_bif_constr({erlang, is_function, 2}, Dst, [Fun, Arity], _State) ->
  ArgFun = fun(Map) ->
	       DstType = lookup_type(Dst, Map),
	       case t_is_atom(true, DstType) of
		 true -> 
		   ArityType = lookup_type(Arity, Map),
		   case t_number_vals(ArityType) of
		     any -> t_fun();
		     Vals -> t_sup([t_fun(X, t_any()) || X <- Vals])
		   end;
		 false -> t_any()
	       end
	   end,
  ArgV = mk_fun_var(ArgFun, [Dst, Arity]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, t_bool()),
			   mk_constraint(Arity, sub, t_integer()),
			   mk_constraint(Fun, sub, ArgV)]);
get_bif_constr({erlang, is_integer, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_integer());
get_bif_constr({erlang, is_list, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_maybe_improper_list());
get_bif_constr({erlang, is_number, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_number());
get_bif_constr({erlang, is_pid, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_pid());
get_bif_constr({erlang, is_port, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_port());
get_bif_constr({erlang, is_reference, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_ref());
get_bif_constr({erlang, is_record, 3}, Dst, [Var, Tag, Arity] = Args, State) ->
  %% TODO: Revise this to make it precise for Tag and Arity.
  ArgFun = 
    fun(Map) ->
	case t_is_atom(true, lookup_type(Dst, Map)) of
	  true ->
	    ArityType = lookup_type(Arity, Map),
	    case t_is_integer(ArityType) of
	      true ->
		case t_number_vals(ArityType) of
		  [ArityVal] ->
		    TagType = lookup_type(Tag, Map),
		    case t_is_atom(TagType) of
		      true ->
			GenRecord = t_tuple([TagType|
					     duplicate(ArityVal-1, t_any())]),
			case t_atom_vals(TagType) of
			  [TagVal] ->
			    case state__lookup_record(State, TagVal, 
						      ArityVal - 1) of
			      {ok, Type} ->  Type;
			      error -> GenRecord
			    end;
			  _ -> GenRecord
			end;
		      false -> t_tuple(ArityVal)
		    end;
		  _ -> t_tuple()
		end;
	      false -> t_tuple()
	    end;
	  false -> t_any()
	end
    end,
  ArgV = mk_fun_var(ArgFun, [Tag, Arity, Dst]),
  DstFun = fun(Map) -> 
	       TmpArgTypes = lookup_type_list(Args, Map),
	       erl_bif_types:type(erlang, is_record, 3, TmpArgTypes)
	   end,
  DstV = mk_fun_var(DstFun, Args),  
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arity, sub, t_integer()),
			   mk_constraint(Tag, sub, t_atom()),
			   mk_constraint(Var, sub, ArgV)]);
get_bif_constr({erlang, is_record, 2}, Dst, Args = [Var, Tag], _State) ->
  ArgFun = fun(Map) ->
	       case t_is_atom(true, lookup_type(Dst, Map)) of
		 true -> t_tuple();
		 false -> t_any()
	       end
	   end,
  ArgV = mk_fun_var(ArgFun, [Dst]),
  DstFun = fun(Map) -> 
	       TmpArgTypes = lookup_type_list(Args, Map),
	       erl_bif_types:type(erlang, is_record, 2, TmpArgTypes)
	   end,
  DstV = mk_fun_var(DstFun, Args),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Tag, sub, t_atom()),
			   mk_constraint(Var, sub, ArgV)]);
get_bif_constr({erlang, is_tuple, 1}, Dst, [Arg], _State) ->
  get_bif_test_constr(Dst, Arg, t_tuple());
get_bif_constr({erlang, 'and', 2}, Dst, [Arg1, Arg2], _State) ->
  True = t_from_term(true),
  False = t_from_term(false),
  ArgFun = fun(Var) ->
	       fun(Map) ->
		   DstType = lookup_type(Dst, Map),
		   case t_is_atom(true, DstType) of
		     true -> True;
		     false ->
		       case t_is_atom(false, DstType) of
			 true ->
			   case t_is_atom(true, lookup_type(Var, Map)) of
			     true -> False;
			     false -> t_bool()
			   end;
			 false -> 
			   t_bool()
		       end
		   end
	       end
	   end,
  DstFun = fun(Map) ->
	       Arg1Type = lookup_type(Arg1, Map),
	       case t_is_atom(false, Arg1Type) of
		 true -> False;
		 false ->
		   Arg2Type = lookup_type(Arg2, Map),
		   case t_is_atom(false, Arg2Type) of
		     true -> False;
		     false ->
		       case (t_is_atom(true, Arg1Type) 
			     andalso t_is_atom(true, Arg2Type)) of
			 true -> True;
			 false -> t_bool()
		       end
		   end
	       end
	   end,
  ArgV1 = mk_fun_var(ArgFun(Arg2), [Arg2, Dst]),
  ArgV2 = mk_fun_var(ArgFun(Arg1), [Arg1, Dst]),
  DstV = mk_fun_var(DstFun, [Arg1, Arg2]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2)]);
get_bif_constr({erlang, 'or', 2}, Dst, [Arg1, Arg2], _State) ->
  True = t_from_term(true),
  False = t_from_term(false),
  ArgFun = fun(Var) ->
	       fun(Map) ->
		   DstType = lookup_type(Dst, Map),
		   case t_is_atom(false, DstType) of
		     true -> False;
		     false ->
		       case t_is_atom(true, DstType) of
			 true ->
			   case t_is_atom(false, lookup_type(Var, Map)) of
			     true -> True;
			     false -> t_bool()
			   end;
			 false -> 
			   t_bool()
		       end
		   end
	       end
	   end,
  DstFun = fun(Map) ->
	       Arg1Type = lookup_type(Arg1, Map),
	       case t_is_atom(true, Arg1Type) of
		 true -> True;
		 false ->
		   Arg2Type = lookup_type(Arg2, Map),
		   case t_is_atom(true, Arg2Type) of
		     true -> True;
		     false ->
		       case (t_is_atom(false, Arg1Type) 
			     andalso t_is_atom(false, Arg2Type)) of
			 true -> False;
			 false -> t_bool()
		       end
		   end
	       end
	   end,
  ArgV1 = mk_fun_var(ArgFun(Arg2), [Arg2, Dst]),
  ArgV2 = mk_fun_var(ArgFun(Arg1), [Arg1, Dst]),
  DstV = mk_fun_var(DstFun, [Arg1, Arg2]),
  Disj = mk_disj_constraint_list([mk_constraint(Arg1, sub, True),
				  mk_constraint(Arg2, sub, True),
				  mk_constraint(Dst, sub, False)]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2),
			   Disj]);
get_bif_constr({erlang, 'not', 1}, Dst, [Arg], _State) ->
  True = t_from_term(true),
  False = t_from_term(false),
  Fun = fun(Var) -> 
	    fun(Map) ->
		Type = lookup_type(Var, Map),
		case t_is_atom(true, Type) of
		  true -> False;
		  false ->
		    case t_is_atom(false, Type) of
		      true -> True;
		      false -> t_bool()
		    end
		end
	    end
	end,
  ArgV = mk_fun_var(Fun(Dst), [Dst]),
  DstV = mk_fun_var(Fun(Arg), [Arg]),
  mk_conj_constraint_list([mk_constraint(Arg, sub, ArgV),
			   mk_constraint(Dst, sub, DstV)]);
get_bif_constr({erlang, '=:=', 2}, Dst, [Arg1, Arg2], _State) ->
  ArgFun =
    fun(Self, OtherVar) ->
	fun(Map) ->
	    DstType = lookup_type(Dst, Map),
	    OtherVarType = lookup_type(OtherVar, Map),
	    case t_is_atom(true, DstType) of
	      true -> OtherVarType;
	      false -> 
		case t_is_atom(false, DstType) of
		  true ->
		    case is_singleton_type(OtherVarType) of
		      true -> t_subtract(lookup_type(Self, Map), OtherVarType);
		      false -> t_any()
		    end;
		  false ->
		    t_any()
		end
	    end
	end
    end,
  DstFun = fun(Map) ->
	       ArgType1 = lookup_type(Arg1, Map),
	       ArgType2 = lookup_type(Arg2, Map),
	       case t_is_none(t_inf(ArgType1, ArgType2)) of
		 true -> t_from_term(false);
		 false -> t_bool()
	       end
	   end,
  ArgV1 = mk_fun_var(ArgFun(Arg1, Arg2), [Dst, Arg1, Arg2]),
  ArgV2 = mk_fun_var(ArgFun(Arg2, Arg1), [Dst, Arg1, Arg2]),
  DstV = mk_fun_var(DstFun, [Arg1, Arg2]),

  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2)]);
get_bif_constr({erlang, '==', 2}, Dst, Args = [Arg1, Arg2], _State) ->
  DstFun = fun(Map) ->
	       TmpArgTypes = lookup_type_list(Args, Map),
	       erl_bif_types:type(erlang, '==', 2, TmpArgTypes)
	   end,
  ArgFun =
    fun(Var, Self) ->
	fun(Map) ->
	    VarType = lookup_type(Var, Map),
	    DstType = lookup_type(Dst, Map),
	    case is_singleton_non_number_type(VarType) of
	      true ->
		case t_is_atom(true, DstType) of
		  true -> VarType;
		  false ->
		    case t_is_atom(false, DstType) of
		      true -> t_subtract(lookup_type(Self, Map), VarType);
		      false -> t_any()
		    end
		end;
	      false ->
		case t_is_atom(true, DstType) of
		  true ->
		    case t_is_number(VarType) of
		      true -> t_number();
		      false -> 
			case t_is_atom(VarType) of
			  true -> VarType;
			  false -> t_any()
			end
		    end;
		  false ->
		    t_any()
		end
	    end
	end
    end,
  DstV = mk_fun_var(DstFun, Args),
  ArgV1 = mk_fun_var(ArgFun(Arg2, Arg1), [Arg1, Arg2, Dst]),
  ArgV2 = mk_fun_var(ArgFun(Arg1, Arg2), [Arg1, Arg2, Dst]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, DstV),
			   mk_constraint(Arg1, sub, ArgV1),
			   mk_constraint(Arg2, sub, ArgV2)]);
get_bif_constr({M, F, A}, Dst, Args, _State) ->
  GenType = erl_bif_types:type(M, F, A),
  case t_is_none(GenType) of
    true -> ?debug("Bif: ~w failed\n", [{M, F, A}]), throw(error);
    false ->
      ArgTypes = erl_bif_types:arg_types(M, F, A),
      ReturnType = mk_fun_var(fun(Map) -> 
				  TmpArgTypes = lookup_type_list(Args, Map),
				  erl_bif_types:type(M, F, A, TmpArgTypes)
			      end, Args),
      case ArgTypes =:= any of
	true -> 
	  case t_is_any(GenType) of
	    true -> 
	      none;
	    false ->
	      mk_constraint(Dst, sub, ReturnType)
	  end;
	false -> 
	  Cs = mk_constraints(Args, sub, ArgTypes),
	  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType)|Cs])
      end
  end.

eval_inv_arith('+', _Pos, Dst, Arg) -> 
  erl_bif_types:type(erlang, '-', 2, [Dst, Arg]);
eval_inv_arith('*', _Pos, Dst, Arg) -> 
  case t_number_vals(Arg) of
    [0] -> t_integer();
    _ -> 
      TmpRet = erl_bif_types:type(erlang, 'div', 2, [Dst, Arg]),
      Zero = t_from_term(0),
      %% If 0 is not part of the result, it cannot be part of the argument.
      case t_is_subtype(Zero, Dst) of
	false -> t_subtract(TmpRet, Zero);
	true -> TmpRet
      end
  end;
eval_inv_arith('-', 1, Dst, Arg) -> 
  erl_bif_types:type(erlang, '-', 2, [Arg, Dst]);
eval_inv_arith('-', 2, Dst, Arg) -> 
  erl_bif_types:type(erlang, '+', 2, [Arg, Dst]).

range_inc(neg_inf) -> neg_inf;
range_inc(pos_inf) -> pos_inf;
range_inc(Int) when is_integer(Int) -> Int + 1.

range_dec(neg_inf) -> neg_inf;
range_dec(pos_inf) -> pos_inf;
range_dec(Int) when is_integer(Int) -> Int - 1.

get_bif_test_constr(Dst, Arg, Type) ->
  ArgFun = fun(Map) ->
	       DstType = lookup_type(Dst, Map),
	       case t_is_atom(true, DstType) of
		 true -> Type;
		 false -> t_any()
	       end
	   end,
  ArgV = mk_fun_var(ArgFun, [Dst]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, t_bool()),
			   mk_constraint(Arg, sub, ArgV)]).
