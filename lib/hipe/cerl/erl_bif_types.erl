%% -*- erlang-indent-level: 4 -*-
%% =====================================================================
%% Type information for Erlang Built-in functions
%%
%% Copyright (C) 2002 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Author contact: richardc@csd.uu.se
%%
%% NOTE: Significantly extended by Tobias Lindahl and Kostis Sagonas
%%
%% $Id$
%%
%% =====================================================================

-module(erl_bif_types).

-export([type/3, type/4, arg_types/1, arg_types/3]).

-import(erl_types, [t_any/0, t_atom/0, t_atom/1, t_atom_vals/1, 
		    t_binary/0, t_bool/0,
		    t_byte/0, t_char/0, t_constant/0,
		    t_cons/0, t_cons/2, t_cons_hd/1, t_cons_tl/1,
		    t_float/0, t_from_term/1, t_fun/0, t_fun/2, t_fun_range/1,
		    t_integer/0, t_integers/1, t_is_float/1, t_improper_list/0,
		    t_is_atom/1, t_is_binary/1, t_is_bool/1, t_is_constant/1,
		    t_is_float/1, t_is_fun/1, t_is_improper_list/1,
		    t_is_integer/1, t_is_number/1, t_is_pid/1, t_is_port/1,
		    t_is_ref/1, t_is_tuple/1,
		    t_is_any/1, t_is_byte/1, t_is_integer/1, t_is_nil/1,
		    t_is_none/1, t_list/0, t_list/1,
		    t_list_elements/1, t_number/0, t_number_vals/1,
		    t_nil/0, t_nonempty_list/0, t_nonempty_list/1, 
		    t_pid/0, t_port/0, 
		    t_ref/0, t_string/0, t_tuple/0,
		    t_tuple/1, t_tuple_args/1, t_tuple_arity/1, t_sup/1,
		    t_tuple_subtypes/1,
		    t_sup/2, t_inf/2, t_subtract/2, t_none/0,
		    t_identifier/0,
		    t_is_atom/1]).

type(M, F, A) ->
    type(M, F, A, any_list(A)).

%% Arguments should be checked for undefinedness, so we do not make
%% unnecessary overapproximations.

%%-- code ---------------------------------------------------------------------
%% type(code, get_chunk, 2, Xs) ->
%% type(code, module_md5, 1, Xs) ->
%% type(code, make_stub_module, 3, Xs) ->
type(code, is_module_native, 1, Xs) ->
    strict(arg_types(code, is_module_native, 1), Xs,
	   fun (_) -> t_sup(t_bool(), t_atom('undefined')) end);
%%-- erlang -------------------------------------------------------------------
type(erlang, halt, 0, _) -> t_none();
type(erlang, halt, 1, _) -> t_none();
type(erlang, exit, 1, _) -> t_none();
type(erlang, exit, 2, _) -> t_none();
type(erlang, fault, 1, _) -> t_none();
type(erlang, fault, 2, _) -> t_none();
type(erlang, error, 1, _) -> t_none();
type(erlang, error, 2, _) -> t_none();
type(erlang, throw, 1, _) -> t_none();
type(erlang, hibernate, 3, _) -> t_none();
type(erlang, '==', 2, Xs = [X1, X2]) ->
    True = t_from_term(true),
    case t_is_atom(X1) andalso t_is_atom(X2) of
	true ->
	    AVals = t_atom_vals(X1),
	    case t_atom_vals(X2) of
		AVals when length(AVals) == 1 -> True;
		_ -> t_bool()
	    end;
	false ->
	    case t_is_integer(X1) andalso t_is_integer(X2) of
		true ->
		    NumVals = t_number_vals(X1),
		    case t_number_vals(X2) of
			NumVals when length(NumVals) == 1 -> 
			    True;
			_ -> 
			    t_bool()
		    end;
		false ->
		    strict(Xs, t_bool())
	    end
    end;
type(erlang, '/=', 2, Xs) -> strict(Xs, t_bool());
type(erlang, '=:=', 2, Xs = [X1, X2]) -> 
    True = t_from_term(true),
    case t_is_atom(X1) andalso t_is_atom(X2) of
	true ->
	    AVals = t_atom_vals(X1),
	    case t_atom_vals(X2) of
		AVals when length(AVals) == 1 -> True;
		_ -> t_bool()
	    end;
	false ->
	    case t_is_integer(X1) andalso t_is_integer(X2) of
		true ->
		    NumVals = t_number_vals(X1),
		    case t_number_vals(X2) of
			NumVals when length(NumVals) == 1 -> 
			    True;
			_ -> 
			    t_bool()
		    end;
		false ->
		    case t_is_none(t_inf(X1, X2)) of
			true ->  
			    strict(Xs, t_from_term(false));
			false ->
			    t_bool()
		    end
	    end
    end;
type(erlang, '=/=', 2, Xs) -> strict(Xs, t_bool());
type(erlang, '>', 2, Xs) -> strict(Xs, t_bool());
type(erlang, '>=', 2, Xs) -> strict(Xs, t_bool());
type(erlang, '<', 2, Xs) -> strict(Xs, t_bool());
type(erlang, '=<', 2, Xs) -> strict(Xs, t_bool());
type(erlang, '+', 1, [X]) -> X;   % +X = X for all X!
type(erlang, '-', 1, Xs) ->
    strict(arg_types(erlang, '-', 1), Xs, 
	   fun ([X]) -> 
		   case t_number_vals(X) of
		       any -> X;
		       List when is_list(List) ->
			   NewList = [-N || N <- List],
			   t_integers(NewList)
		   end
	   end);
type(erlang, '!', 2, Xs) ->
    strict(arg_types(erlang, '!', 2), Xs, fun ([_, X2]) -> X2 end);
type(erlang, '+', 2, Xs) ->
    strict(arg_types(erlang, '+', 2), Xs,
	   fun ([X1, X2]) ->
		   case arith('+', X1, X2) of
		       {ok, T} -> T;
		       error ->
			   case t_is_byte(X1) andalso t_is_byte(X2) of
			       true ->
				   t_char();
			       false ->
				   case (t_is_integer(X1) andalso 
					 t_is_integer(X2)) of
				       true ->
					   t_integer();
				       false ->
					   case (t_is_float(X1) 
						 orelse t_is_float(X2)) of
					       true -> t_float();
					       false -> t_number()
					   end
				   end
			   end
		   end
	   end);
type(erlang, '-', 2, Xs) ->
    strict(arg_types(erlang, '-', 2), Xs,
	   fun ([X1, X2]) ->
		   case arith('-', X1, X2) of
		       {ok, T} -> T;
		       error ->
			   case t_is_integer(X1) andalso t_is_integer(X2) of
			       true -> t_integer();
			       false ->
				   case t_is_float(X1) orelse t_is_float(X2) of
				       true -> t_float();
				       false -> t_number()
				   end
			   end
		   end
	   end);
type(erlang, '*', 2, Xs) ->
    strict(arg_types(erlang, '*', 2), Xs,
	   fun ([X1, X2]) ->
		   case arith('*', X1, X2) of
		       {ok, T} -> T;
		       error ->
			   case t_is_integer(X1) andalso t_is_integer(X2) of
			       true -> t_integer();
			       false ->
				   case t_is_float(X1) orelse t_is_float(X2) of
				       true -> t_float();
				       false -> t_number()
				   end
			   end
		   end
	   end);
type(erlang, '/', 2, Xs) ->
    strict(arg_types(erlang, '/', 2), Xs,
	   fun (_) -> t_float() end);
type(erlang, 'div', 2, Xs) ->
    strict(arg_types(erlang, 'div', 2), Xs,
	   fun ([X1, X2]) ->
		   case arith('div', X1, X2) of
		       error -> t_integer();
		       {ok, T} -> T
		   end
	   end);
type(erlang, 'rem', 2, Xs) ->
    strict(arg_types(erlang, 'rem', 2), Xs,
	   fun ([X1, X2]) ->
		   case arith('rem', X1, X2) of
		       error -> t_integer();
		       {ok, T} -> T
		   end
	   end);
type(erlang, '++', 2, Xs) ->
    strict(arg_types(erlang, '++', 2), Xs,
	   fun ([X1, X2]) ->
		   case t_is_nil(X1) of
		       true ->
			   X2;    % even if X2 is not a list
		       false ->
			   case t_is_nil(X2) of
			       true ->
				   X1;
			       false ->
				   E1 = t_list_elements(X1),
				   t_sup(t_sup(X1, X2), t_cons(E1, X2))
			   end
		   end
	   end);
type(erlang, '--', 2, Xs) ->
    %% We don't know which elements (if any) in X2 will be found and
    %% removed from X1, even if they would have the same type. Thus, we
    %% must assume that X1 can remain unchanged. However, if we succeed,
    %% we know that X1 must be a proper list, but the result could
    %% possibly be empty even if X1 is nonempty.
    strict(arg_types(erlang, '--', 2), Xs,
	   fun ([X1, _]) -> t_list(t_list_elements(X1)) end);
type(erlang, 'and', 2, Xs) ->
    strict(arg_types(erlang, 'and', 2), Xs, fun (_) -> t_bool() end);
type(erlang, 'or', 2, Xs) ->
    strict(arg_types(erlang, 'or', 2), Xs, fun (_) -> t_bool() end);
type(erlang, 'xor', 2, Xs) ->
    strict(arg_types(erlang, 'xor', 2), Xs, fun (_) -> t_bool() end);
type(erlang, 'not', 1, Xs) ->
    strict(arg_types(erlang, 'not', 1), Xs, fun (_) -> t_bool() end);
type(erlang, 'band', 2, Xs) ->
    %% The result is not wider than the smallest argument. We need to
    %% kill any value-sets in the result.
    strict(arg_types(erlang, 'band', 2), Xs,
	   fun ([X1, X2]) -> t_sup(t_inf(X1, X2), t_byte()) end);
type(erlang, 'bor', 2, Xs) ->
    %% The result is not wider than the largest argument. We need to
    %% kill any value-sets in the result.
    strict(arg_types(erlang, 'bor', 2), Xs,
	   fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bxor', 2, Xs) ->
    %% The result is not wider than the largest argument. We need to
    %% kill any value-sets in the result.
    strict(arg_types(erlang, 'bxor', 2), Xs,
	   fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bsr', 2, Xs) ->
    %% If the first argument is unsigned (which is the case for
    %% characters and bytes), the result is never wider. We need to kill
    %% any value-sets in the result.
    strict(arg_types(erlang, 'bsr', 2), Xs,
	   fun ([X, _]) -> t_sup(X, t_byte()) end);
type(erlang, 'bsl', 2, Xs) ->
    %% Not worth doing anything special here.
    strict(arg_types(erlang, 'bsl', 2), Xs, fun (_) -> t_integer() end);
type(erlang, 'bnot', 1, Xs) ->
    %% This returns (-X)-1, so it often gives a negative result.
    strict(arg_types(erlang, 'bnot', 1), Xs, fun (_) -> t_integer() end);
type(erlang, abs, 1, Xs) ->
    strict(arg_types(erlang, abs, 1), Xs, fun ([X]) -> X end);
type(erlang, append_element, 2, Xs) ->
    strict(arg_types(erlang, append_element, 2), Xs, fun (_) -> t_tuple() end);
type(erlang, apply, 2, Xs) ->
    Fun = fun ([X, _Y]) -> 
		  case t_is_fun(X) of
		      true ->
			  t_fun_range(X);
		      false ->
			  t_any() 
		  end
	  end,
    strict(arg_types(erlang, apply, 2), Xs, Fun);
type(erlang, apply, 3, Xs) ->
    strict(arg_types(erlang, apply, 3), Xs, fun (_) -> t_any() end);
type(erlang, atom_to_list, 1, Xs) ->
    strict(arg_types(erlang, atom_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, binary_to_list, 1, Xs) ->
    strict(arg_types(erlang, binary_to_list, 1), Xs,
	   fun (_) -> t_list(t_byte()) end);
type(erlang, binary_to_list, 3, Xs) ->
    strict(arg_types(erlang, binary_to_list, 3), Xs,
	   fun (_) -> t_list(t_byte()) end);
type(erlang, binary_to_term, 1, Xs) ->
    strict(arg_types(erlang, binary_to_term, 1), Xs, fun (_) -> t_any() end);
type(erlang, cancel_timer, 1, Xs) ->
    strict(arg_types(erlang, cancel_timer, 1), Xs,
	   fun (_) -> t_sup(t_integer(), t_atom('false')) end);
type(erlang, concat_binary, 1, Xs) ->
    strict(arg_types(erlang, concat_binary, 1), Xs, fun (_) -> t_binary() end);
type(erlang, date, 0, _) ->
    t_tuple([t_integer(), t_integer(), t_integer()]);
type(erlang, demonitor, 1, Xs) ->
    strict(arg_types(erlang, demonitor, 1), Xs, fun (_) -> t_atom('true') end);
type(erlang, disconnect_node, 1, Xs) ->
    strict(arg_types(erlang, disconnect_node, 1), Xs, fun (_) -> t_bool() end);
type(erlang, display, 1, _) -> t_atom('true');
type(erlang, element, 2, Xs) ->
    strict(arg_types(erlang, element, 2), Xs,
	   fun ([X1, X2]) ->
		   case t_tuple_subtypes(X2) of
		       any -> t_any();
		       [_] ->
			   A = t_tuple_arity(X2),
			   As = t_tuple_args(X2),
			   case t_number_vals(X1) of
			       Ns when is_list(Ns) ->
				   lists:foldl(
				     fun (N, X) when N >= 1, N =< A ->
					     t_sup(X, lists:nth(N, As));
					 (_, X) ->
					     X
				     end,
				     t_none(), Ns);
			       _ ->
				   t_sup(t_tuple_args(X2))
			   end;
		       Ts when is_list(Ts) ->
			   t_sup([type(erlang, element, 2, [X1, Y]) || 
				     Y <- Ts])
		   end
	   end);
type(erlang, erase, 0, _) -> t_any();
type(erlang, erase, 1, _) -> t_any();
type(erlang, float, 1, Xs) ->
    strict(arg_types(erlang, float, 1), Xs, fun (_) -> t_float() end);
type(erlang, float_to_list, 1, Xs) ->
    strict(arg_types(erlang, float_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, fun_info, 1, Xs) ->
    strict(arg_types(erlang, fun_info, 1), Xs,
	   fun (_) -> t_list(t_tuple([t_atom(), t_any()])) end);
type(erlang, fun_info, 2, Xs) ->
    strict(arg_types(erlang, fun_info, 2), Xs,
	   fun (_) -> t_tuple([t_atom(), t_any()]) end);
type(erlang, fun_to_list, 1, Xs) ->
    strict(arg_types(erlang, fun_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, get, 0, _) -> t_list(t_tuple(2));
type(erlang, get, 1, _) -> t_any();          % | t_atom('undefined')
type(erlang, get_cookie, 0, _) -> t_atom();  % | t_atom('nocookie')
type(erlamg, get_keys, 1, _) -> t_list();
type(erlang, get_module_info, 1, Xs) ->
    strict(arg_types(erlang, get_module_info, 1), Xs,
	   fun (_) -> t_list(t_tuple([t_atom(),
				      t_list(t_tuple([t_atom(), t_any()]))]))
	   end);
type(erlang, get_module_info, 2, Xs) ->
    strict(arg_types(erlang, get_module_info, 2), Xs,
	   fun (_) -> t_list(t_tuple([t_atom(), t_any()])) end);
type(erlang, group_leader, 0, _) -> t_pid();
type(erlang, group_leader, 2, Xs) ->
    strict(arg_types(erlang, group_leader, 2), Xs,
	   fun (_) -> t_atom('true') end);
type(erlang, hash, 2, Xs) ->
    strict(arg_types(erlang, hash, 2), Xs, fun (_) -> t_integer() end);
type(erlang, hd, 1, Xs) ->
    strict(arg_types(erlang, hd, 1), Xs, fun ([X]) -> t_cons_hd(X) end);
type(erlang, integer_to_list, 1, Xs) ->
    strict(arg_types(erlang, integer_to_list, 1), Xs,
	   fun (_) -> t_string() end);
type(erlang, info, 1, Xs) -> type(erlang, system_info, 1, Xs); % alias
type(erlang, is_alive, 0, _) -> t_bool();
type(erlang, is_atom, 1, Xs) ->   
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_atom(Y) end, t_atom()) end,
    strict(arg_types(erlang, is_atom, 1), Xs, Fun);
type(erlang, is_binary, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_binary(Y) end,
				 t_binary())
	  end,
    strict(arg_types(erlang, is_binary, 1), Xs, Fun);
type(erlang, is_boolean, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_bool(Y) end,
				 t_bool())
	  end,
    strict(arg_types(erlang, is_boolean, 1), Xs, Fun);
type(erlang, is_builtin, 3, Xs) ->
    strict(arg_types(erlang, is_builtin, 3), Xs, fun(_) -> t_bool() end);
type(erlang, is_constant, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_constant(Y) end,
				 t_constant())
	  end,
    strict(arg_types(erlang, is_constant, 1), Xs, Fun);
type(erlang, is_float, 1, Xs) ->
    Fun = fun (X) ->
		  check_guard(X, fun(Y) -> t_is_float(Y) end, t_float())
	  end,
    strict(arg_types(erlang, is_float, 1), Xs, Fun);
type(erlang, is_function, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_fun(Y) end, t_fun()) end,
    strict(arg_types(erlang, is_function, 1), Xs, Fun);
type(erlang, is_integer, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_integer(Y) end,
				t_integer())
	  end,
    strict(arg_types(erlang, is_integer, 1), Xs, Fun);
type(erlang, is_list, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_improper_list(Y) end,
				 t_improper_list())
	  end,
    strict(arg_types(erlang, is_list, 1), Xs, Fun);
type(erlang, is_number, 1, Xs) ->
    Fun = fun(X) ->
		  check_guard(X, fun (Y) -> t_is_number(Y) end, t_number())
	  end,
    strict(arg_types(erlang, is_number, 1), Xs, Fun);
type(erlang, is_pid, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_pid(Y) end, t_pid()) end,
    strict(arg_types(erlang, is_pid, 1), Xs, Fun);
type(erlang, is_port, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_port(Y) end, t_port()) end,
    strict(arg_types(erlang, is_port, 1), Xs, Fun);
type(erlang, is_process_alive, 1, Xs) ->
    strict(arg_types(erlang, is_process_alive, 1), Xs,
	   fun (_) -> t_bool() end);
type(erlang, is_record, 2, Xs) ->
    Fun = fun([X, Y]) -> 
		  case t_is_tuple(X) of
		      false ->
			  case t_is_none(t_inf(t_tuple(), X)) of
			      true -> t_atom('false');
			      false -> t_bool()
			  end;
		      true ->
			  case t_tuple_args(X) of
			      any -> t_bool();
			      [Tag|_] ->
				  case t_is_atom(Tag) of
				      false ->
					  TagAtom = t_inf(Tag, t_atom()),
					  case t_is_none(TagAtom) of
					      true -> t_atom('false');
					      false -> t_bool()
					  end;
				      true ->
					  case t_atom_vals(Tag) of
					      [RealTag] -> 
						  case t_atom_vals(Y) of
						      [RealTag] ->
							  t_atom('true');
						      _ -> t_bool() 
						  end;
					      _ -> 
						  t_bool()
					  end
				  end
			  end
		  end
	  end,
    strict(arg_types(erlang, is_record, 2), Xs, Fun);
type(erlang, is_record, 3, Xs) ->
    Fun = fun([X, Y, Z]) ->
		  Arity = t_number_vals(Z),
		  case t_is_tuple(X) of
		      false when length(Arity) =:= 1 ->
			  [RealArity] = Arity,
			  case t_is_none(t_inf(t_tuple(RealArity), X)) of
			      true -> t_atom('false');
			      false -> t_bool()
			  end;
		      false ->
			  case t_is_none(t_inf(t_tuple(), X)) of
			      true -> t_atom('false');
			      false -> t_bool()
			  end;
		      true when length(Arity) =:= 1 ->
			  [RealArity] = Arity,
			  case t_tuple_args(X) of
			      any -> t_bool();
			      Args when length(Args) =:= RealArity ->
				  Tag = hd(Args),
				  case t_is_atom(Tag) of
				      false ->
					  TagAtom = t_inf(Tag, t_atom()),
					  case t_is_none(TagAtom) of
					      true -> t_atom('false');
					      false -> t_bool()
					  end;
				      true ->
					  case t_atom_vals(Tag) of
					      [RealTag] -> 
						  case t_atom_vals(Y) of
						      [RealTag] ->
							  t_atom('true');
						      _ -> t_bool()
						  end;
					      _ -> 
						  t_bool()
					  end
				  end
			  end;
		      true ->
			  t_bool()
		  end
	  end,
    strict(arg_types(erlang, is_record, 3), Xs, Fun);
type(erlang, is_reference, 1, Xs) ->
    Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_ref(Y) end, t_ref()) end,
    strict(arg_types(erlang, is_reference, 1), Xs, Fun);
type(erlang, is_tuple, 1, Xs) ->
    Fun = fun (X) ->
		  check_guard(X, fun (Y) -> t_is_tuple(Y) end, t_tuple())
	  end,
    strict(arg_types(erlang, is_tuple, 1), Xs, Fun);
type(erlang, length, 1, Xs) ->
    strict(arg_types(erlang, length, 1), Xs, fun (_) -> t_integer() end);
type(erlang, link, 1, Xs) ->
    strict(arg_types(erlang, link, 1), Xs, fun (_) -> t_atom('true') end);
type(erlang, list_to_atom, 1, Xs) ->
    strict(arg_types(erlang, list_to_atom, 1), Xs, fun (_) -> t_atom() end);
type(erlang, list_to_binary, 1, Xs) ->
    strict(arg_types(erlang, list_to_binary, 1), Xs,
	   fun (_) -> t_binary() end);
type(erlang, list_to_float, 1, Xs) ->
    strict(arg_types(erlang, list_to_float, 1), Xs, fun (_) -> t_float() end);
type(erlang, list_to_integer, 1, Xs) ->
    strict(arg_types(erlang, list_to_integer, 1), Xs,
	   fun (_) -> t_integer() end);
type(erlang, list_to_pid, 1, Xs) ->
    strict(arg_types(erlang, list_to_pid, 1), Xs, fun (_) -> t_pid() end);
type(erlang, list_to_tuple, 1, Xs) ->
    strict(arg_types(erlang, list_to_tuple, 1), Xs, fun (_) -> t_tuple() end);
type(erlang, loaded, 0, _) ->
    t_list(t_atom());
type(erlang, localtime, 0, Xs) ->
    type(erlang, universaltime, 0, Xs);    % same
type(erlang, localtime_to_universaltime, 1, Xs) ->
    type(erlang, universaltime_to_localtime, 1, Xs);    % same
type(erlang, make_ref, 0, _) -> t_ref();
type(erlang, make_tuple, 2, Xs) ->
    strict(arg_types(erlang, make_tuple, 2), Xs,
	   fun ([Int,_]) ->
		   case t_number_vals(Int) of
		       any -> t_tuple();
		       [N] when N >= 1 -> t_tuple(N);
		       _Other -> t_none()
		   end
	   end);
type(erlang, md5, 1, Xs) ->
    strict(arg_types(erlang, md5, 1), Xs, fun (_) -> t_binary() end);
type(erlang, md5_final, 1, Xs) ->
    strict(arg_types(erlang, md5_final, 1), Xs, fun (_) -> t_binary() end);
type(erlang, md5_init, 0, _) -> t_binary();
type(erlang, md5_update, 2, Xs) ->
    strict(arg_types(erlang, md5_update, 2), Xs, fun (_) -> t_binary() end);
type(erlang, monitor, 2, Xs) ->
    strict(arg_types(erlang, monitor, 2), Xs, fun (_) -> t_ref() end);
type(erlang, monitor_node, 2, Xs) ->
    strict(arg_types(erlang, monitor_node, 2), Xs,
	   fun (_) -> t_atom('true') end);
type(erlang, node, 0, _) -> t_atom();
type(erlang, node, 1, Xs) ->
    strict(arg_types(erlang, node, 1), Xs, fun (_) -> t_atom() end);
type(erlang, nodes, 0, _) -> t_list(t_atom());
type(erlang, nodes, 1, Xs) ->
    strict(arg_types(erlang, nodes, 1), Xs,
	   fun (_) -> t_list(t_atom()) end);
type(erlang, now, 0, _) ->
    t_tuple([t_integer(), t_integer(), t_integer()]);
type(erlang, open_port, 2, Xs) ->
    strict(arg_types(erlang, open_port, 2), Xs, fun (_) -> t_port() end);
type(erlang, phash, 2, Xs) ->
    strict(arg_types(erlang, phash, 2), Xs, fun (_) -> t_integer() end);
type(erlang, pid_to_list, 1, Xs) ->
    strict(arg_types(erlang, pid_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, port_call, 3, Xs) ->
    strict(arg_types(erlang, port_call, 3), Xs, fun (_) -> t_any() end);
type(erlang, port_close, 1, Xs) ->
    strict(arg_types(erlang, port_close, 1), Xs,
	   fun (_) -> t_atom('true') end);
type(erlang, port_command, 2, Xs) ->
    strict(arg_types(erlang, port_command, 2), Xs,
	   fun (_) -> t_atom('true') end);
type(erlang, port_connect, 2, Xs) ->
    strict(arg_types(erlang, port_connect, 2), Xs,
	   fun (_) -> t_atom('true') end);
type(erlang, port_control, 3, Xs) ->
    strict(arg_types(erlang, port_control, 3), Xs,
	   fun (_) -> t_sup(t_string(), t_binary()) end);
type(erlang, port_info, 2, Xs) ->
    strict(arg_types(erlang, port_info, 2), Xs,
	   fun ([_Port, Item]) ->
		   t_sup(t_atom('undefined'),
			 case t_atom_vals(Item) of
			     ['connected'] -> t_tuple([Item, t_pid()]);
			     ['id'] -> t_tuple([Item, t_integer()]);
			     ['input'] -> t_tuple([Item, t_integer()]);
			     ['links'] -> t_tuple([Item, t_list(t_pid())]);
			     ['name'] -> t_tuple([Item, t_string()]);
			     ['output'] -> t_tuple([Item, t_integer()]);
			     ['registered_name'] -> t_tuple([Item, t_atom()]);
			     List when is_list(List) ->
				 t_tuple([t_sup([t_atom(A) || A <- List]),
					  t_sup([t_atom(), t_integer(),
						 t_pid(), t_list(t_pid()),
						 t_string()])])
			 end)
	   end);
type(erlang, port_to_list, 1, Xs) ->
    strict(arg_types(erlang, port_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, ports, 0, _) -> t_list(t_port());
type(erlang, pre_loaded, 0, _) -> t_list(t_atom());
type(erlang, process_display, 2, _) -> t_atom('true');
type(erlang, process_flag, 2, Xs) ->
    strict(arg_types(erlang, process_flag, 2), Xs,
	   fun ([Flag, _Option]) ->
		   case t_atom_vals(Flag) of
		       ['error_handler'] -> t_atom();
		       ['min_heap_size'] -> t_integer();
		       ['priority'] -> t_sup([t_atom('low'),
					      t_atom('normal'),
					      t_atom('high')]);
		       ['save_calls'] -> t_integer();
		       ['trap_exit'] -> t_bool();
		       List when is_list(List) ->
			   t_sup([t_bool(), t_atom(), t_integer()])
		   end
	   end);
type(erlang, process_info, 1, Xs) ->
    strict(arg_types(erlang, process_info, 1), Xs,
	   fun (_) ->
		   t_sup(t_list(t_tuple([t_atom(), t_any()])),
			 t_atom('undefined'))
	   end);
type(erlang, process_info, 2, Xs) ->
    strict(arg_types(erlang, process_info, 2), Xs,
	   fun ([_Pid,_InfoType]) -> t_any() end); %% TODO: Very underspecified
type(erlang, processes, 0, _) -> t_list(t_pid());
type(erlang, put, 2, Xs) ->
    strict(arg_types(erlang, put, 2), Xs, fun (_) -> t_any() end);
type(erlang, read_timer, 1, Xs) ->
    strict(arg_types(erlang, read_timer, 1), Xs,
	   fun (_) -> t_sup(t_integer(), t_atom('false')) end);
type(erlang, ref_to_list, 1, Xs) ->
    strict(arg_types(erlang, ref_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, register, 2, Xs) ->
    strict(arg_types(erlang, register, 2), Xs, fun (_) -> t_atom('true') end);
type(erlang, registered, 0, _) -> t_list(t_atom());
type(erlang, resume_process, 1, Xs) ->
    strict(arg_types(erlang, resume_process, 1), Xs,
	   fun (_) -> t_any() end); %% TODO: overapproximation -- fix this
type(erlang, round, 1, Xs) ->
    strict(arg_types(erlang, round, 1), Xs, fun (_) -> t_integer() end);
type(erlang, self, 0, _) -> t_pid();
type(erlang, send, 2, Xs) -> type(erlang, '!', 2, Xs);  % alias
type(erlang, send_after, 3, Xs) ->
    strict(arg_types(erlang, send_after, 3), Xs, fun (_) -> t_ref() end);
type(erlang, set_cookie, 2, Xs) ->
    strict(arg_types(erlang, set_cookie, 2), Xs,
	   fun (_) -> t_atom('true') end);
type(erlang, setelement, 3, Xs) ->
    strict(arg_types(erlang, setelement, 3), Xs,
	   fun ([X1, X2, X3]) ->
		   case t_tuple_subtypes(X2) of
		       any -> t_tuple();
		       [_] ->
			   A = t_tuple_arity(X2),
			   As = t_tuple_args(X2),
			   case t_number_vals(X1) of
			       any ->
				   t_tuple([t_sup(X, X3) || X <- As]);
			       [N] when N >= 1, N =< A ->
				   t_tuple(list_replace(N, X3, As));
			       [N] when N < 1; N > A ->
				   t_none();
			       Ns ->
				   t_tuple(
				     lists:foldl(
				       fun (N, Xs) when N >= 1, N =< A ->
					       X = lists:nth(N, Xs),
					       Y = t_sup(X, X3),
					       list_replace(N, Y, Xs);
					   (_, Xs) ->
					       Xs
				       end,
				       As, Ns))
			   end;
		       Ts when is_list(Ts) ->
			   t_sup([type(erlang, setelement, 3, [X1, Y, X3]) ||
				     Y <- Ts])
		   end
	   end);
type(erlang, size, 1, Xs) ->
    strict(arg_types(erlang, size, 1), Xs, fun (_) -> t_integer() end);
type(erlang, spawn, 1, Xs) ->
    strict(arg_types(erlang, spawn, 1), Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 2, Xs) ->
    strict(arg_types(erlang, spawn, 2), Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 3, Xs) ->
    strict(arg_types(erlang, spawn, 3), Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 4, Xs) ->
    strict(arg_types(erlang, spawn, 4), Xs, fun (_) -> t_pid() end);
type(erlang, spawn_link, 1, Xs) -> type(erlang, spawn, 1, Xs);  % same
type(erlang, spawn_link, 2, Xs) -> type(erlang, spawn, 2, Xs);  % same
type(erlang, spawn_link, 3, Xs) -> type(erlang, spawn, 3, Xs);  % same
type(erlang, spawn_link, 4, Xs) -> type(erlang, spawn, 4, Xs);  % same
type(erlang, spawn_opt, 2, Xs) -> 
    strict(arg_types(erlang, spawn_opt, 2), Xs, fun (_) -> t_pid() end);
type(erlang, spawn_opt, 3, Xs) -> 
    strict(arg_types(erlang, spawn_opt, 3), Xs, fun (_) -> t_pid() end);
type(erlang, spawn_opt, 4, Xs) -> 
    strict(arg_types(erlang, spawn_opt, 4), Xs, fun (_) -> t_pid() end);
type(erlang, split_binary, 2, Xs) ->
    strict(arg_types(erlang, split_binary, 2), Xs,
	   fun (_) -> t_tuple([t_binary(), t_binary()]) end);
type(erlang, start_timer, 3, Xs) ->
    strict(arg_types(erlang, start_timer, 3), Xs, fun (_) -> t_ref() end);
type(erlang, statistics, 1, Xs) ->
    strict(arg_types(erlang, statistics, 1), Xs,
	   fun(_) -> t_sup([t_integer(),
			    t_tuple([t_integer(), t_integer()]),
			    %% When called with the argument 'io'.
			    t_tuple([t_tuple([t_atom('input'), t_integer()]),
				     t_tuple([t_atom('output'), t_integer()])]),
			    t_tuple([t_integer(), t_integer(), t_integer()])])
	   end);
type(erlang, suspend_process, 1, Xs) ->
    strict(arg_types(erlang, suspend_process, 1), Xs,
	   fun (_) -> t_any() end); %% TODO: overapproximation -- fix this
type(erlang, system_flag, 2, Xs) ->
    strict(arg_types(erlang, system_flag, 2), Xs, fun (_) -> t_integer() end);
type(erlang, system_info, 1, Xs) ->
    strict(arg_types(erlang, system_info, 1), Xs, fun (_) -> t_any() end);
type(erlang, term_to_binary, 1, Xs) ->
    strict(arg_types(erlang, term_to_binary, 1), Xs,
	   fun (_) -> t_binary() end);
type(erlang, term_to_binary, 2, Xs) ->
    strict(arg_types(erlang, term_to_binary, 2), Xs,
	   fun (_) -> t_binary() end);
type(erlang, time, 0, _) ->
    t_tuple([t_integer(), t_integer(), t_integer()]);
type(erlang, tl, 1, Xs) ->
    strict(arg_types(erlang, tl, 1), Xs, fun ([X]) -> t_cons_tl(X) end);
type(erlang, trace, 3, Xs) ->
    strict(arg_types(erlang, trace, 3), Xs, fun (_) -> t_integer() end);
type(erlang, trace_info, 2, Xs) ->
    strict(arg_types(erlang, trace_info, 2), Xs,
	   fun (_) ->
		   t_tuple([t_atom(),
			    t_sup([%% the following is info about a PID
				   t_list(t_atom()), t_pid(), t_port(),
				   %% the following is info about a func
				   t_atom('global'), t_atom('local'),
				   t_atom('false'), t_atom('true'),
				   t_list(), t_pid(), t_port(),
				   t_integer(),
				   t_list(t_tuple([t_atom(), t_any()])),
				   %% and this is the 'not found' value
				   t_atom('undefined')])])
	   end);
type(erlang, trace_pattern, 2, Xs) ->
    strict(arg_types(erlang, trace_pattern, 2), Xs,
	   fun (_) -> t_integer() end);
type(erlang, trace_pattern, 3, Xs) ->
    strict(arg_types(erlang, trace_pattern, 3), Xs,
	   fun (_) -> t_integer() end);
type(erlang, trunc, 1, Xs) ->
    strict(arg_types(erlang, trunc, 1), Xs, fun (_) -> t_integer() end);
type(erlang, tuple_to_list, 1, Xs) ->
    strict(arg_types(erlang, tuple_to_list, 1), Xs,
	   fun ([X]) ->
		   case t_tuple_subtypes(X) of
		       any -> t_list();
		       SubTypes -> 
			   Args = lists:flatten([t_tuple_args(ST)
						 || ST <-SubTypes]),
			   t_nonempty_list(t_sup(Args))
		   end
	   end);
type(erlang, universaltime, 0, _) ->
    t_tuple([t_tuple([t_integer(), t_integer(), t_integer()]),
	     t_tuple([t_integer(), t_integer(), t_integer()])]);
type(erlang, universaltime_to_localtime, 1, Xs) ->
    strict(arg_types(erlang, universaltime_to_localtime, 1), Xs,
	   fun ([T]) -> T end);
type(erlang, unlink, 1, Xs) ->
    strict(arg_types(erlang, unlink, 1), Xs, fun (_) -> t_atom('true') end);
type(erlang, unregister, 1, Xs) ->
    strict(arg_types(erlang, unregister, 1), Xs,
	   fun (_) -> t_atom('true') end);
type(erlang, whereis, 1, Xs) ->
    strict(arg_types(erlang, whereis, 1), Xs,
	   fun (_) -> t_sup([t_pid(), t_port(), t_atom('undefined')]) end);
type(erlang, yield, 0, _) -> t_atom('true');
%%-- ets ----------------------------------------------------------------------
type(ets, all, 0, _) ->
    t_list(t_tid());
type(ets, db_delete, 1, Xs) ->
    strict(arg_types(ets, db_delete, 1), Xs, fun (_) -> t_atom('true') end);
type(ets, db_info, 2, Xs) ->
    strict(arg_types(ets, db_info, 2), Xs, fun (_) -> t_any() end);
type(ets, delete, 2, Xs) ->
    strict(arg_types(ets, delete, 2), Xs, fun (_) -> t_atom('true') end);
type(ets, delete_all_objects, 1, Xs) ->
    strict(arg_types(ets, delete_all_objects, 1), Xs,
	   fun (_) -> t_atom('true') end);
type(ets, delete_object, 2, Xs) ->
    strict(arg_types(ets, delete_object, 2), Xs,
	   fun (_) -> t_atom('true') end);
type(ets, first, 1, Xs) ->
    strict(arg_types(ets, first, 1), Xs, fun (_) -> t_any() end);
type(ets, insert, 2, Xs) ->
    strict(arg_types(ets, insert, 2), Xs, fun (_) -> t_atom('true') end);
type(ets, insert_new, 2, Xs) ->
    strict(arg_types(ets, insert_new, 2), Xs, fun(_) -> t_bool() end);
type(ets, is_compiled_ms, 1, Xs) ->
    strict(arg_types(ets, is_compiled_ms, 1), Xs, fun (_) -> t_bool() end);
type(ets, last, 1, Xs) ->
    type(ets, first, 1, Xs);
type(ets, lookup, 2, Xs) ->
    strict(arg_types(ets, lookup, 2), Xs, fun (_) -> t_list(t_tuple()) end);
type(ets, lookup_element, 3, Xs) ->
    strict(arg_types(ets, lookup_element, 3), Xs, fun (_) -> t_any() end);
type(ets, match, 1, Xs) ->
    strict(arg_types(ets, match, 1), Xs, fun (_) -> t_matchres() end);
type(ets, match, 2, Xs) ->
    strict(arg_types(ets, match, 2), Xs, fun (_) -> t_list() end);
type(ets, match, 3, Xs) ->
    strict(arg_types(ets, match, 3), Xs, fun (_) -> t_matchres() end);
type(ets, match_object, 1, Xs) -> type(ets, match, 1, Xs);
type(ets, match_object, 2, Xs) -> type(ets, match, 2, Xs);
type(ets, match_object, 3, Xs) -> type(ets, match, 3, Xs);
type(ets, match_spec_compile, 1, Xs) ->
    strict(arg_types(ets, match_spec_compile, 1), Xs, fun (_) -> t_any() end);
type(ets, match_spec_run_r, 3, Xs) ->
    strict(arg_types(ets, match_spec_run_r, 3), Xs, fun (_) -> t_list() end);
type(ets, member, 2, Xs) ->
    strict(arg_types(ets, member, 2), Xs, fun (_) -> t_bool() end);
type(ets, new, 2, Xs) ->
    strict(arg_types(ets, new, 2), Xs, fun (_) -> t_tid() end);
type(ets, next, 2, Xs) ->
    strict(arg_types(ets, next, 2), Xs,
	   %% t_any below stands for:  term() | '$end_of_table'
	   fun (_) -> t_any() end);
type(ets, prev, 2, Xs) -> type(ets, next, 2, Xs);
type(ets, rename, 2, Xs) ->
    strict(arg_types(ets, rename, 2), Xs, fun ([_, Name]) -> Name end);
type(ets, safe_fixtable, 2, Xs) ->
    strict(arg_types(ets, safe_fixtable, 2), Xs,
	   fun (_) -> t_atom('true') end);
type(ets, select, 1, Xs) ->
    strict(arg_types(ets, select, 1), Xs, fun (_) -> t_matchres() end);
type(ets, select, 2, Xs) ->
    strict(arg_types(ets, select, 2), Xs, fun (_) -> t_list(t_tuple()) end);
type(ets, select, 3, Xs) ->
    strict(arg_types(ets, select, 3), Xs, fun (_) -> t_matchres() end);
type(ets, select_delete, 2, Xs) ->
    strict(arg_types(ets, select_delete, 2), Xs, fun (_) -> t_integer() end);
type(ets, select_reverse, 1, Xs) -> type(ets, select, 1, Xs);
type(ets, select_reverse, 2, Xs) -> type(ets, select, 2, Xs);
type(ets, select_reverse, 3, Xs) -> type(ets, select, 3, Xs);
type(ets, slot, 2, Xs) ->
    strict(arg_types(ets, slot, 2), Xs,
	   fun (_) -> t_sup(t_list(t_tuple()), t_atom('$end_of_table')) end);
type(ets, update_counter, 3, Xs) ->
    strict(arg_types(ets, update_counter, 3), Xs, fun (_) -> t_integer() end);
%%-- lists --------------------------------------------------------------------
type(lists, all, 2, Xs) ->
    strict(arg_types(lists, all, 2), Xs, fun (_) -> t_bool() end);
type(lists, any, 2, Xs) ->
    strict(arg_types(lists, any, 2), Xs, fun (_) -> t_bool() end);
type(lists, append, 2, Xs) -> type(erlang, '++', 2, Xs);  % alias
type(lists, filter, 2, Xs) ->
    strict(arg_types(lists, filter, 2), Xs,
	   fun ([_, X]) -> 
		   case t_is_nil(X) of
		       true -> X;
		       false ->
			   X1 = t_list_elements(X),
			   case t_is_any(X1) of
			       true -> t_list();
			       false -> t_list(X1)
			   end
		   end
	   end);
type(lists, flatten, 1, Xs) ->
    strict(arg_types(lists, flatten, 1), Xs,
	   fun ([X]) ->
		   case t_is_nil(X) of
		       true -> X;    % (nil has undefined elements)
		       false ->
			   %% Avoiding infinite recursion is tricky
			   X1 = t_list_elements(X),
			   case t_is_any(X1) of
			       true -> t_list();
			       false ->
				   X2 = type(lists, flatten, 1,
					     [t_inf(X1, t_list())]),
				   t_sup(t_list(t_subtract(X1, t_list())),
					 X2)
			   end
		   end
	   end);
type(lists, foreach, 2, Xs) ->
    strict(arg_types(lists, foreach, 2), Xs,
	   fun (_) -> t_atom('ok') end);
type(lists, foldl, 3, Xs) ->
    strict(arg_types(lists, foldl, 3), Xs,
	   fun ([X, Y, _]) -> t_sup(t_fun_range(X), Y) end);
type(lists, foldr, 3, Xs) -> type(lists, foldl, 3, Xs);    % same
type(lists, keymember, 3, Xs) ->
    strict(arg_types(lists, keymember, 3), Xs,
	   fun([X, Y, Z]) ->
		   ListEs = t_list_elements(Z),
		   Tuple = t_inf(t_tuple(), ListEs),
		   case t_is_none(Tuple) of
		       true -> t_atom('false');
		       false ->
			   case t_is_any(X) of
			       true -> t_bool();
			       false ->
				   case t_tuple_subtypes(Tuple) of
				       any -> t_bool();
				       List ->
					   Keys = [type(erlang, element, 2, 
							[Y, S])
						   || S <- List],
					   Infs = [t_inf(Key, X)||Key <- Keys],
					   case all_is_none(Infs) of
					       true -> t_atom('false');
					       false -> t_bool()
					   end
				   end
			   end
		   end
	   end);
type(lists, keysearch, 3, Xs) ->
    strict(arg_types(lists, keysearch, 3), Xs,
	   fun([X, Y, Z]) ->
		   ListEs = t_list_elements(Z),
		   Tuple = t_inf(t_tuple(), ListEs),
		   case t_is_none(Tuple) of
		       true -> t_atom('false');
		       false ->
			   Ret = t_sup(t_tuple([t_atom('value'), Tuple]), 
				       t_atom('false')),
			   case t_is_any(X) of
			       true -> Ret;
			       false ->
				   case t_tuple_subtypes(Tuple) of
				       any -> Ret;
				       List ->
					   Keys = [type(erlang, element, 2, 
							[Y, S])
						   || S <- List],
					   Infs = [t_inf(Key, X)||Key <- Keys],
					   case all_is_none(Infs) of
					       true -> t_atom('false');
					       false -> Ret
					   end
				   end
			   end
		   end
	   end);
type(lists, last, 1, Xs) ->
    strict(arg_types(lists, last, 1), Xs, fun([X]) -> t_list_elements(X)end);
type(lists, map, 2, Xs) ->
    strict(arg_types(lists, map, 2), Xs,
	   fun ([X, _]) -> t_sup(t_list(t_fun_range(X)), t_nil()) end);
type(lists, mapfoldl, 3, Xs) ->
    strict(arg_types(lists, mapfoldl, 3), Xs,
	   fun ([X, Acc, _]) ->
		   R = t_fun_range(X),
		   case t_is_none(R) of
		       true -> t_tuple([t_nil(), Acc]);
		       false ->
			   case t_tuple_args(R) of
			       [T1, T2] ->
				   t_tuple([t_list(T1), t_sup(Acc, T2)]);
			       _ ->
				   t_tuple([t_list(), t_any()])
			   end
		   end
	   end);
type(lists, mapfoldr, 3, Xs) -> type(lists, mapfoldl, 3, Xs);    % same
type(lists, member, 2, Xs) ->
    strict(arg_types(lists, member, 2), Xs,
	   fun([X, Y]) ->
		   Y1 = t_list_elements(Y),
		   case t_is_none(t_inf(Y1, X)) of
		       true -> t_from_term(false);
		       false -> t_bool()
		   end
	   end);
type(lists, nth, 2, Xs) ->
    strict(arg_types(lists, nth, 2), Xs,
	   fun([_, Y]) -> t_list_elements(Y) end);
type(lists, nthtail, 2, Xs) ->
    strict(arg_types(lists, nthtail, 2), Xs, 
	   fun([_, Y]) -> t_sup(Y, t_list()) end);
type(lists, reverse, 1, Xs) ->
    strict(arg_types(lists, reverse, 1), Xs, fun ([X]) -> X end);
type(lists, reverse, 2, Xs) ->
    type(erlang, '++', 2, Xs);    % reverse-onto is just like append
type(lists, seq, 2, Xs) ->
    strict(arg_types(lists, seq, 2), Xs, fun(_) -> t_list(t_integer()) end);
type(lists, seq, 3, Xs) ->
    strict(arg_types(lists, seq, 3), Xs, fun(_) -> t_list(t_integer()) end);
type(lists, subtract, 2, Xs) -> type(erlang, '--', 2, Xs);  % alias
%%-- math ---------------------------------------------------------------------
type(math, acos, 1, Xs) ->
    strict(arg_types(math, acos, 1), Xs, fun (_) -> t_float() end);
type(math, acosh, 1, Xs) ->
    strict(arg_types(math, acosh, 1), Xs, fun (_) -> t_float() end);
type(math, asin, 1, Xs) ->
    strict(arg_types(math, asin, 1), Xs, fun (_) -> t_float() end);
type(math, asinh, 1, Xs) ->
    strict(arg_types(math, asinh, 1), Xs, fun (_) -> t_float() end);
type(math, atan, 1, Xs) ->
    strict(arg_types(math, atan, 1), Xs, fun (_) -> t_float() end);
type(math, atan2, 2, Xs) ->
    strict(arg_types(math, atan2, 2), Xs, fun (_) -> t_float() end);
type(math, atanh, 1, Xs) ->
    strict(arg_types(math, atanh, 1), Xs, fun (_) -> t_float() end);
type(math, cos, 1, Xs) ->
    strict(arg_types(math, cos, 1), Xs, fun (_) -> t_float() end);
type(math, cosh, 1, Xs) ->
    strict(arg_types(math, cosh, 1), Xs, fun (_) -> t_float() end);
type(math, erf, 1, Xs) ->
    strict(arg_types(math, erf, 1), Xs, fun (_) -> t_float() end);
type(math, erfc, 1, Xs) ->
    strict(arg_types(math, erfc, 1), Xs, fun (_) -> t_float() end);
type(math, exp, 1, Xs) ->
    strict(arg_types(math, exp, 1), Xs, fun (_) -> t_float() end);
type(math, log, 1, Xs) ->
    strict(arg_types(math, log, 1), Xs, fun (_) -> t_float() end);
type(math, log10, 1, Xs) ->
    strict(arg_types(math, log10, 1), Xs, fun (_) -> t_float() end);
type(math, pi, 0, _) -> t_float();
type(math, pow, 2, Xs) ->
    strict(arg_types(math, pow, 2), Xs, fun (_) -> t_float() end);
type(math, sin, 1, Xs) ->
    strict(arg_types(math, sin, 1), Xs, fun (_) -> t_float() end);
type(math, sinh, 1, Xs) ->
    strict(arg_types(math, sinh, 1), Xs, fun (_) -> t_float() end);
type(math, sqrt, 1, Xs) ->
    strict(arg_types(math, sqrt, 1), Xs, fun (_) -> t_float() end);
type(math, tan, 1, Xs) ->
    strict(arg_types(math, tan, 1), Xs, fun (_) -> t_float() end);
type(math, tanh, 1, Xs) ->
    strict(arg_types(math, tanh, 1), Xs, fun (_) -> t_float() end);
%%-- os -----------------------------------------------------------------------
type(os, getenv, 0, _) -> t_list(t_string());
type(os, getenv, 1, Xs) ->
    strict(arg_types(os, getenv, 1), Xs,
	   fun (_) -> t_sup(t_string(), t_atom('false')) end);
type(os, getpid, 0, _) -> t_string();
type(os, putenv, 2, Xs) ->
    strict(arg_types(os, putenv, 2), Xs, fun (_) -> t_atom('true') end);
%%-- string -------------------------------------------------------------------
type(string, to_float, 1, Xs) ->
    strict(arg_types(string, to_float, 1), Xs,
	   fun (_) -> t_sup(t_tuple([t_float(), t_string()]),
			    t_tuple([t_atom('error'),
				     t_sup(t_atom('no_float'),
					   t_atom('not_a_list'))]))
	   end);
type(string, to_integer, 1, Xs) ->
    strict(arg_types(string, to_integer, 1), Xs,
	   fun (_) -> t_sup(t_tuple([t_integer(), t_string()]),
			    t_tuple([t_atom('error'),
				     t_sup(t_atom('no_integer'),
					   t_atom('not_a_list'))]))
	   end);
type(_, _, _, Xs) ->
    strict(Xs, t_any()).  % safe approximation for all functions.


strict(Xs, Ts, F) ->
    Xs1 = inf_lists(Xs, Ts),
    case any_is_none(Xs1) of
	true -> t_none();
	false -> F(Xs1)
    end.

strict(Xs, X) ->
    case any_is_none(Xs) of
 	true -> t_none();
 	false -> X
    end.

inf_lists([X | Xs], [T | Ts]) ->
    [t_inf(X, T) | inf_lists(Xs, Ts)];
inf_lists([], []) ->
    [].

any_list(N) -> any_list(N, t_any()).

any_list(N, A) when N > 0 ->
    [A | any_list(N - 1, A)];
any_list(0, _) ->
    [].

list_replace(N, E, [X | Xs]) when N > 1 ->
    [X | list_replace(N - 1, E, Xs)];
list_replace(1, E, [_X | Xs]) ->
    [E | Xs].

any_is_none([X | Xs]) ->
    case t_is_none(X) of
	true ->
	    true;
	false ->
	    any_is_none(Xs)
    end;
any_is_none([]) -> false.

all_is_none([X | Xs]) ->
    case t_is_none(X) of
	true ->
	    all_is_none(Xs);
	false ->
	    false
    end;
all_is_none([]) -> true.

check_guard([X], Test, Type) ->
    case Test(X) of
	true -> 
	    t_from_term(true);
	false ->
	    case t_is_none(t_inf(Type, X)) of
		true -> t_from_term(false);
		false -> t_bool()
	    end
    end.

arith(Op, X1, X2) ->
    case t_is_integer(X1) andalso t_is_integer(X2) of
	false -> error;
	true ->
	    case {t_number_vals(X1), t_number_vals(X2)} of
		{any, _} -> error;
		{_, any} -> error;
		{L1, L2} ->
		    case Op of
			'+' -> AllVals = [X + Y ||X <- L1, Y <- L2];
			'-' -> AllVals = [X - Y ||X <- L1, Y <- L2];
			'*' -> AllVals = [X * Y ||X <- L1, Y <- L2];
			'div' -> AllVals = [X div Y ||X <- L1, Y <- L2];
			'rem' -> AllVals = [X rem Y ||X <- L1, Y <- L2]
		    end,
		    {ok, t_integers(ordsets:from_list(AllVals))}
	    end
    end.

%% =====================================================================
%% arg_types returns a list of the demanded argument types for a bif
%% to succeed.

arg_types({M, F, A}) ->
  arg_types(M, F, A);
arg_types('+') ->
  [t_number(), t_number()];
arg_types('-') ->
  [t_number(), t_number()];
arg_types('*') ->
  [t_number(), t_number()];
arg_types('/') ->
  [t_number(), t_number()];
arg_types('div') ->
  [t_integer(), t_integer()];
arg_types('band') ->
  [t_integer(), t_integer()];
arg_types('bor') ->
  [t_integer(), t_integer()];
arg_types('bxor') ->
  [t_integer(), t_integer()];
arg_types('bsr') ->
  [t_integer(), t_integer()];
arg_types('bsl') ->
  [t_integer(), t_integer()];
arg_types('bnot') ->
  [t_integer()];
arg_types('rem') ->
  [t_integer(), t_integer()];
arg_types({element, _}) ->
  [t_integer(), t_tuple()];
arg_types(_) ->
  any.                     % safe approximation for all functions.


%%------- code ----------------------------------------------------------------
%% arg_types(code, get_chunk, 2) ->
%% arg_types(code, module_md5, 1) ->
%% arg_types(code, make_stud_module, 3) ->
arg_types(code, is_module_native, 1) ->
  [t_atom()];
%%------- erlang --------------------------------------------------------------
arg_types(erlang, '!', 2) ->
  Pid = t_sup([t_pid(), t_port(), t_atom(),
	       t_tuple([t_atom(), t_atom()])]),
  [Pid, t_any()];
arg_types(erlang, '+', 1) ->
  [t_any()];
arg_types(erlang, '+', 2) ->
  arg_types('+');
arg_types(erlang, '++', 2) ->
  [t_list(), t_any()];
arg_types(erlang, '-', 1) ->
  [t_number()];
arg_types(erlang, '-', 2) ->
  arg_types('-');
arg_types(erlang, '--', 2) ->
  [t_list(), t_list()];
arg_types(erlang, '*', 2) ->
  arg_types('*');
arg_types(erlang, '/', 2) ->
  arg_types('/');
arg_types(erlang, 'div', 2) ->
  arg_types('div');
arg_types(erlang, 'rem', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'and', 2) ->
  [t_bool(), t_bool()];
arg_types(erlang, 'or', 2) ->
  [t_bool(), t_bool()];
arg_types(erlang, 'xor', 2) ->
  [t_bool(), t_bool()];
arg_types(erlang, 'not', 1) ->
  [t_bool()];
arg_types(erlang, 'band', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bor', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bxor', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bsr', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bsl', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bnot', 1) ->
  [t_integer()];
arg_types(erlang, abs, 1) ->
  [t_number()];
arg_types(erlang, append_element, 2) ->
  [t_tuple(), t_any()];
arg_types(erlang, apply, 2) ->
  [t_sup(t_tuple([t_sup(t_atom(),   % module name
			t_tuple()), % parameterized module	    
		  t_atom()]),
	 t_fun()),
   t_list()];
arg_types(erlang, apply, 3) ->
  [t_sup(t_atom(), t_tuple()), t_atom(), t_list()];
arg_types(erlang, atom_to_list, 1) ->
  [t_atom()];
arg_types(erlang, binary_to_list, 1) ->
  [t_binary()];
arg_types(erlang, binary_to_list, 3) ->
  [t_binary(), t_integer(), t_integer()];
arg_types(erlang, binary_to_term, 1) ->
  [t_binary()];
arg_types(erlang, cancel_timer, 1) ->
  [t_ref()];
arg_types(erlang, concat_binary, 1) ->
  [t_list(t_binary())];
arg_types(erlang, date, 0) ->
  [];
arg_types(erlang, demonitor, 1) ->
  [t_ref()];
arg_types(erlang, disconnect_node, 1) ->
  [t_atom()];
arg_types(erlang, display, 1) ->
  [t_any()];
arg_types(erlang, element, 2) ->
  [t_integer(), t_tuple()];
arg_types(erlang, erase, 0) ->
  [];
arg_types(erlang, erase, 1) ->
  [t_any()];
arg_types(erlang, error, 1) ->
  [t_any()];
arg_types(erlang, error, 2) ->
  [t_any(), t_list()];
arg_types(erlang, exit, 1) ->
  [t_any()];
arg_types(erlang, exit, 2) ->
  [t_pid(), t_any()];
arg_types(erlang, fault, 1) ->
  arg_types(erlang, error, 1);  % alias for compatibility
arg_types(erlang, fault, 2) ->
  arg_types(erlang, error, 2);  % alias for compatibility
arg_types(erlang, float, 1) ->
  [t_number()];
arg_types(erlang, float_to_list, 1) ->
  [t_float()];
arg_types(erlang, fun_info, 1) ->
  [t_fun()];
arg_types(erlang, fun_info, 2) ->
  [t_fun(), t_atom()];
arg_types(erlang, fun_to_list, 1) ->
  [t_fun()];
arg_types(erlang, get, 0) ->
  [];
arg_types(erlang, get, 1) ->
  [t_any()];
arg_types(erlang, get_cookie, 0) ->
  [];
arg_types(erlang, get_keys, 1) ->
  [t_any()];
arg_types(erlang, get_module_info, 1) ->
  [t_atom()];
arg_types(erlang, get_module_info, 2) ->
  [t_atom(), t_atom()];
arg_types(erlang, group_leader, 0) ->
  [];
arg_types(erlang, group_leader, 2) ->
  [t_pid(), t_pid()];
arg_types(erlang, halt, 0) ->
  [];
arg_types(erlang, halt, 1) ->
  [t_sup(t_integer(), t_string())];
arg_types(erlang, hash, 2) ->
  [t_any(), t_integer()];
arg_types(erlang, hd, 1) ->
  [t_cons()];
arg_types(erlang, hibernate, 3) ->
  [t_atom(), t_atom(), t_list()];
arg_types(erlang, info, 1) ->
  arg_types(erlang, system_info, 1); % alias
arg_types(erlang, integer_to_list, 1) ->
  [t_integer()];
arg_types(erlang, is_alive, 0) ->
  [];
arg_types(erlang, is_atom, 1) ->
  [t_any()];
arg_types(erlang, is_binary, 1) ->
  [t_any()];
arg_types(erlang, is_boolean, 1) ->
  [t_any()];
arg_types(erlang, is_builtin, 3) ->
  [t_atom(), t_atom(), t_integer()];
arg_types(erlang, is_constant, 1) ->
  [t_any()];
arg_types(erlang, is_float, 1) ->
  [t_any()];
arg_types(erlang, is_function, 1) ->
  [t_any()];
arg_types(erlang, is_integer, 1) ->
  [t_any()];
arg_types(erlang, is_list, 1) ->
  [t_any()];
arg_types(erlang, is_number, 1) ->
  [t_any()];
arg_types(erlang, is_pid, 1) ->
  [t_any()];
arg_types(erlang, is_port, 1) ->
  [t_any()];
arg_types(erlang, is_process_alive, 1) ->
  [t_pid()];
arg_types(erlang, is_record, 2) ->
  [t_any(), t_atom()];
arg_types(erlang, is_record, 3) ->
  [t_any(), t_atom(), t_integer()];
arg_types(erlang, is_reference, 1) ->
  [t_any()];
arg_types(erlang, is_tuple, 1) ->
  [t_any()];
arg_types(erlang, length, 1) ->
  [t_list()];
arg_types(erlang, link, 1) ->
  [t_sup(t_pid(), t_port())];
arg_types(erlang, list_to_atom, 1) ->
  [t_string()];
arg_types(erlang, list_to_binary, 1) ->
  [t_list()];
arg_types(erlang, list_to_float, 1) ->
  [t_list(t_byte())];
arg_types(erlang, list_to_integer, 1) ->
  [t_list(t_byte())];
arg_types(erlang, list_to_pid, 1) ->
  [t_string()];
arg_types(erlang, list_to_tuple, 1) ->
  [t_list()];
arg_types(erlang, loaded, 0) ->
  [];
arg_types(erlang, localtime, 0) ->
  [];
arg_types(erlang, localtime_to_universaltime, 1) ->
  T = t_tuple([t_tuple([t_integer(), t_integer(), t_integer()]),
	       t_tuple([t_integer(), t_integer(), t_integer()])]),
  [T];
arg_types(erlang, make_ref, 0) ->
  [];
arg_types(erlang, make_tuple, 2) ->
  [t_integer(), t_any()];
arg_types(erlang, md5, 1) ->
  [t_sup(t_list(), t_binary())];
arg_types(erlang, md5_final, 1) ->
  [t_binary()];
arg_types(erlang, md5_init, 0) ->
  [];
arg_types(erlang, md5_update, 2) ->
  [t_binary(), t_sup(t_list(), t_binary())];
arg_types(erlang, monitor, 2) ->
  [t_atom(), t_sup([t_pid(), t_atom(), t_tuple([t_atom(), t_atom()])])];
arg_types(erlang, monitor_node, 2) ->
  [t_atom(), t_bool()];
arg_types(erlang, node, 0) ->
  [];
arg_types(erlang, node, 1) ->
  [t_identifier()];
arg_types(erlang, nodes, 0) ->
  [];
arg_types(erlang, nodes, 1) ->
  [t_sup(t_atom(), t_list(t_atom()))];
arg_types(erlang, now, 0) ->
  [];
arg_types(erlang, open_port, 2) ->
  [t_sup(t_atom(), t_tuple()), t_list()];
arg_types(erlang, phash, 2) ->
  [t_any(), t_integer()];
arg_types(erlang, pid_to_list, 1) ->
  [t_pid()];
arg_types(erlang, port_call, 3) ->
  [t_sup(t_port(), t_atom()), t_integer(), t_any()];
arg_types(erlang, port_close, 1) ->
  [t_sup(t_port(), t_atom())];
arg_types(erlang, port_command, 2) ->
  [t_sup(t_port(), t_atom()), t_sup(t_list(), t_binary())];
arg_types(erlang, port_connect, 2) ->
  [t_sup(t_port(), t_atom()), t_pid()];
arg_types(erlang, port_control, 3) ->
  [t_sup(t_port(), t_atom()), t_integer(), t_sup(t_list(), t_binary())];
arg_types(erlang, port_info, 2) ->
  [t_sup(t_port(), t_atom()),
   t_sup([t_atom('registered_name'), t_atom('id'), t_atom('connected'),
	  t_atom('links'), t_atom('name'), t_atom('input'),t_atom('output')])];
arg_types(erlang, port_to_list, 1) ->
  [t_port()];
arg_types(erlang, ports, 0) ->
  [];
arg_types(erlang, pre_loaded, 0) ->
  [];
arg_types(erlang, process_display, 2) ->
  [t_pid(), t_atom('backtrace')];
arg_types(erlang, process_flag, 2) ->
  [t_sup([t_atom('trap_exit'), t_atom('error_handler'),
	  t_atom('min_heap_size'), t_atom('priority'), t_atom('save_calls')]),
   t_sup([t_bool(), t_atom(), t_integer()])];
arg_types(erlang, process_flag, 3) ->
  [t_pid(), t_atom('save_calls'), t_integer()];
arg_types(erlang, process_info, 1) ->
  [t_pid()];
arg_types(erlang, process_info, 2) ->
  [t_pid(), t_pinfo()];
arg_types(erlang, processes, 0) ->
  [];
arg_types(erlang, put, 2) ->
  [t_any(), t_any()];
arg_types(erlang, read_timer, 1) ->
  [t_ref()];
arg_types(erlang, ref_to_list, 1) ->
  [t_ref()];
arg_types(erlang, register, 2) ->
  [t_atom(), t_sup(t_port(), t_pid())];
arg_types(erlang, registered, 0) ->
  [];
arg_types(erlang, resume_process, 1) ->
  [t_pid()]; % intended for debugging only
arg_types(erlang, round, 1) ->
  [t_number()];
arg_types(erlang, self, 0) ->
  [];
arg_types(erlang, send, 2) -> 
  arg_types(erlang, '!', 2);  % alias
arg_types(erlang, send_after, 3) ->
  [t_integer(), t_sup(t_pid(), t_atom()), t_any()];
arg_types(erlang, set_cookie, 2) ->
 [t_identifier(), t_atom()];
arg_types(erlang, setelement, 3) ->
  [t_integer(), t_tuple(), t_any()];
arg_types(erlang, size, 1) ->
  [t_sup(t_tuple(), t_binary())];
arg_types(erlang, spawn, 1) -> %% TODO: Tuple?
  [t_fun()];
arg_types(erlang, spawn, 2) -> %% TODO: Tuple?
  [t_atom(), t_fun()];
arg_types(erlang, spawn, 3) -> %% TODO: Tuple?
  [t_atom(), t_atom(), t_list()];
arg_types(erlang, spawn, 4) -> %% TODO: Tuple?
  [t_atom(), t_atom(), t_atom(), t_list()];
arg_types(erlang, spawn_link, 1) -> 
  arg_types(erlang, spawn, 1);  % same
arg_types(erlang, spawn_link, 2) -> 
  arg_types(erlang, spawn, 2);  % same
arg_types(erlang, spawn_link, 3) -> 
  arg_types(erlang, spawn, 3);  % same
arg_types(erlang, spawn_link, 4) -> 
  arg_types(erlang, spawn, 4);  % same
arg_types(erlang, spawn_opt, 2) -> 
  [t_fun(), t_list()];
arg_types(erlang, spawn_opt, 3) -> 
  [t_atom(), t_fun(), t_list()];
arg_types(erlang, spawn_opt, 4) -> 
  [t_atom(), t_atom(), t_list(), t_list()];
arg_types(erlang, split_binary, 2) ->
  [t_binary(), t_integer()];
arg_types(erlang, start_timer, 3) ->
  [t_integer(), t_sup(t_pid(), t_atom()), t_any()];
arg_types(erlang, statistics, 1) ->
  [t_atom()];
arg_types(erlang, suspend_process, 1) ->
  [t_pid()]; % intended for debugging only
arg_types(erlang, system_flag, 2) ->
  [t_sup([t_atom('backtrace_depth'), t_atom('fullsweep_after'),
	  t_atom('min_heap_size'), t_atom('trace_control_word'),
	  %% Undocumented; used to implement (the documented) seq_trace module.
	  t_atom('sequential_tracer'),
	  t_integer()]),
   t_integer()];
arg_types(erlang, system_info, 1) ->
  [t_sup([t_atom(),                     % documented
	  t_tuple([t_atom(), t_any()]), % documented
	  t_tuple([t_atom(), t_atom(), t_any()])])];
arg_types(erlang, term_to_binary, 1) ->
  [t_any()];
arg_types(erlang, term_to_binary, 2) ->
  [t_any(), t_list()];
arg_types(erlang, throw, 1) ->
  [t_any()];
arg_types(erlang, time, 0) ->
  [];
arg_types(erlang, tl, 1) ->
  [t_cons()];
arg_types(erlang, trace, 3) ->
  [t_sup(t_pid(), t_sup([t_atom('existing'), t_atom('new'), t_atom('all')])),
   t_bool(),
   t_list(t_sup(t_atom(), t_tuple(2)))];
arg_types(erlang, trace_info, 2) ->
  [t_sup([%% the following two get info about a PID
	  t_pid(), t_atom('new'),
	  %% while the following two get info about a func
	  t_tuple([t_atom(), t_atom(), t_integer()]), t_atom('on_load')]),
   t_sup([%% the following are items about a PID
	  t_atom('flags'), t_atom('tracer'),
	  %% while the following are items about a func
	  t_atom('traced'), t_atom('match_spec'), t_atom('meta'),
	  t_atom('meta_match_spec'), t_atom('call_count'), t_atom('all')])];
arg_types(erlang, trace_pattern, 2) ->
  [t_sup(t_tuple([t_atom(), t_atom(), t_sup(t_integer(), t_atom('_'))]),
	 t_atom('on_load')),
   t_sup([t_bool(), t_list(), t_atom('restart'), t_atom('pause')])];
arg_types(erlang, trace_pattern, 3) ->
  arg_types(erlang, trace_pattern, 2) ++
    [t_list(t_sup([t_atom('global'), t_atom('local'),
		   t_atom('meta'), t_tuple([t_atom('meta'), t_pid()]),
		   t_atom('call_count')]))];
arg_types(erlang, trunc, 1) ->
  [t_number()];
arg_types(erlang, tuple_to_list, 1) ->
  [t_tuple()];
arg_types(erlang, universaltime, 0) ->
  [];
arg_types(erlang, universaltime_to_localtime, 1) ->
  T = t_tuple([t_tuple([t_integer(), t_integer(), t_integer()]),
	       t_tuple([t_integer(), t_integer(), t_integer()])]),
  [T];
arg_types(erlang, unlink, 1) ->
  [t_sup(t_pid(), t_port())];
arg_types(erlang, unregister, 1) ->
  [t_atom()];
arg_types(erlang, whereis, 1) ->
  [t_atom()];
arg_types(erlang, yield, 0) ->
  [];
%%------- ets -----------------------------------------------------------------
arg_types(ets, all, 0) ->
  [];
arg_types(ets, db_delete, 1) ->
  [t_tid()];
arg_types(ets, db_info, 2) ->
  [t_tid(), t_atom()];
arg_types(ets, delete, 2) ->
  [t_tid(), t_any()];
arg_types(ets, delete_all_objects, 1) ->
  [t_tid()];
arg_types(ets, delete_object, 2) ->
  [t_tid(), t_tuple()];
arg_types(ets, first, 1) ->
  [t_tid()];
arg_types(ets, insert, 2) ->
  [t_tid(), t_sup(t_tuple, t_list(t_tuple()))];
arg_types(ets, insert_new, 2) ->
  [t_tid(), t_sup(t_tuple, t_list(t_tuple()))];
arg_types(ets, is_compiled_ms, 1) ->
  [t_any()];
arg_types(ets, last, 1) ->
  arg_types(ets, first, 1);
arg_types(ets, lookup, 2) ->
  [t_tid(), t_any()];
arg_types(ets, lookup_element, 3) ->
  [t_tid(), t_any(), t_integer()];
arg_types(ets, match, 1) ->
  [t_any()];
arg_types(ets, match, 2) ->
  [t_tid(), t_matchspec()];
arg_types(ets, match, 3) ->
  [t_tid(), t_matchspec(), t_integer()];
arg_types(ets, match_object, 1) ->
  arg_types(ets, match, 1);
arg_types(ets, match_object, 2) ->
  arg_types(ets, match, 2);
arg_types(ets, match_object, 3) ->
  arg_types(ets, match, 3);
arg_types(ets, match_spec_compile, 1) ->
  [t_any()];
arg_types(ets, match_spec_run_r, 3) ->
  [t_list(t_tuple()), t_any(), t_list()];
arg_types(ets, member, 2) ->
  [t_tid(), t_any()];
arg_types(ets, new, 2) ->
  [t_atom(), t_options()];
arg_types(ets, next, 2) ->
  [t_tid(), t_any()];
arg_types(ets, prev, 2) ->
  [t_tid(), t_any()];
arg_types(ets, rename, 2) ->
  [t_atom(), t_atom()];
arg_types(ets, safe_fixtable, 2) ->
  [t_tid(), t_bool()];
arg_types(ets, select, 1) ->
  arg_types(ets, match, 1);
arg_types(ets, select, 2) ->
  [t_tid(), t_list(t_matchspec())];
arg_types(ets, select, 3) ->
  [t_tid(), t_list(t_matchspec()), t_integer()];
arg_types(ets, select_delete, 2) ->
  [t_tid(), t_list(t_matchspec())];
arg_types(ets, select_reverse, 1) ->
  arg_types(ets, select, 1);
arg_types(ets, select_reverse, 2) ->
  arg_types(ets, select, 2);
arg_types(ets, select_reverse, 3) ->
  arg_types(ets, select, 3);
arg_types(ets, slot, 2) ->
  [t_tid(), t_integer()];
arg_types(ets, update_counter, 3) ->
  [t_tid(), t_any(), t_sup(t_integer(),
			   t_sup(t_tuple([t_integer(), t_integer()]),
				 t_tuple([t_integer(), t_integer(),
					  t_integer(), t_integer()])))];
%%------- lists ---------------------------------------------------------------
arg_types(lists, all, 2) ->
  [t_fun([t_any()], t_bool()), t_list()];
arg_types(lists, any, 2) ->
  [t_fun([t_any()], t_bool()), t_list()];
arg_types(lists, append, 2) -> 
  arg_types(erlang, '++', 2);  % alias
arg_types(lists, filter, 2) ->
  [t_fun([t_any()], t_bool()), t_list()];
arg_types(lists, flatten, 1) ->
  [t_list()];
arg_types(lists, foreach, 2) ->
  [t_fun([t_any()], t_any()), t_list()];
arg_types(lists, foldl, 3) ->
  [t_fun([t_any(), t_any()], t_any()), t_any(), t_list()];
arg_types(lists, foldr, 3) -> 
  arg_types(lists, foldl, 3);    % same
arg_types(lists, keymember, 3) ->
  [t_any(), t_integer(), t_list()];
arg_types(lists, keysearch, 3) ->
  [t_any(), t_integer(), t_list()];
arg_types(lists, last, 1) ->
  [t_nonempty_list()];
arg_types(lists, map, 2) ->
  [t_fun([t_any()], t_any()), t_list()];
arg_types(lists, mapfoldl, 3) ->
  [t_fun([t_any(), t_any()], t_tuple([t_any(), t_any()])), t_any(), t_list()];
arg_types(lists, mapfoldr, 3) -> 
  arg_types(lists, mapfoldl, 3); % same
arg_types(lists, member, 2) ->
  [t_any(), t_list()];
arg_types(lists, nth, 2) ->
  [t_integer(), t_nonempty_list()];
arg_types(lists, nthtail, 2) ->
  [t_integer(), t_nonempty_list()];
arg_types(lists, reverse, 1) ->
  [t_list()];
arg_types(lists, reverse, 2) ->
  [t_list(), t_any()];
arg_types(lists, seq, 2) ->
  [t_integer(), t_integer()];
arg_types(lists, seq, 3) ->
  [t_integer(), t_integer(), t_integer()];
arg_types(lists, subtract, 2) ->
  arg_types(erlang, '--', 2);  % alias
%%------- math ----------------------------------------------------------------
arg_types(math, acos, 1) ->
  [t_number()];
arg_types(math, acosh, 1) ->
  [t_number()];
arg_types(math, asin, 1) ->
  [t_number()];
arg_types(math, asinh, 1) ->
  [t_number()];
arg_types(math, atan, 1) ->
  [t_number()];
arg_types(math, atan2, 2) ->
  [t_number(), t_number()];
arg_types(math, atanh, 1) ->
  [t_number()];
arg_types(math, cos, 1) ->
  [t_number()];
arg_types(math, cosh, 1) ->
  [t_number()];
arg_types(math, erf, 1) ->
  [t_number()];
arg_types(math, erfc, 1) ->
  [t_number()];
arg_types(math, exp, 1) ->
  [t_number()];
arg_types(math, log, 1) ->
  [t_number()];
arg_types(math, log10, 1) ->
  [t_number()];
arg_types(math, pi, 0) ->
  [];
arg_types(math, pow, 2) ->
  [t_number(), t_number()];
arg_types(math, sin, 1) ->
  [t_number()];
arg_types(math, sinh, 1) ->
  [t_number()];
arg_types(math, sqrt, 1) ->
  [t_number()];
arg_types(math, tan, 1) ->
  [t_number()];
arg_types(math, tanh, 1) ->
  [t_number()];
%%------- os ------------------------------------------------------------------
arg_types(os, getenv, 0) ->
  [];
arg_types(os, getenv, 1) ->
  [t_string()];
arg_types(os, getpid, 0) ->
  [];
arg_types(os, putenv, 2) ->
  [t_string(), t_string()];
%%------- string --------------------------------------------------------------
arg_types(string, to_float, 1) ->
  [t_string()];
arg_types(string, to_integer, 1) ->
  [t_string()];
arg_types(M, F, A) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
  any.                     % safe approximation for all functions.


%% =====================================================================
%% These are used for the built-in functions of 'erlang'
%% =====================================================================

t_pinfo() ->
  t_sup([t_atom('current_function'),
	 t_atom('initial_call'),
	 t_atom('status'),
	 t_atom('message_queue_len'),
	 t_atom('messages'),
	 t_atom('links'),
	 t_atom('dictionary'),
	 t_atom('trap_exit'),
	 t_atom('error_handler'),
	 t_atom('priority'),
	 t_atom('group_leader'),
	 t_atom('heap_size'),
	 t_atom('stack_size'),
	 t_atom('reductions'),
	 t_atom('garbage_collection')]).

%% =====================================================================
%% These are used for the built-in functions of 'ets'
%% =====================================================================

t_tid() ->
  t_sup(t_integer(), t_atom()).

t_matchspec() ->
  t_sup(t_atom(), t_tuple()).

t_matchres() ->
  t_sup(t_tuple([t_list(), t_any()]), t_atom('$end_of_table')).

%% From the 'ets' documentation
%%-----------------------------
%% Option = Type | Access | named_table | {keypos,Pos}
%%   Type = set | ordered_set | bag | duplicate_bag
%% Access = public | protected | private
%%    Pos = int()
t_options() ->
  t_list(t_sup(t_option_atom(), t_tuple([t_atom('keypos'), t_integer()]))).

t_option_atom() ->
  t_sup([t_atom('set'),
	 t_atom('ordered_set'),
	 t_atom('bag'),
	 t_atom('duplicate_bag'),
	 t_atom('public'),
	 t_atom('protected'),
	 t_atom('private'),
	 t_atom('named_table')]).
