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
%% $Id$
%%
%% =====================================================================

-module(erl_bif_types).

-export([type/3, type/4, arg_types/1, arg_types/3]).

-import(erl_types, [t_any/0, t_atom/0, t_atom/1, t_atom_vals/1, 
		    t_binary/0, t_bool/0,
		    t_byte/0, t_cons/0, t_char/0, t_cons/2, t_cons_hd/1,
		    t_cons_tl/1, t_float/0, t_from_term/1, t_fun/0, t_fun/2,
		    t_fun_range/1, t_integer/0, t_is_float/1,
		    t_improper_list/0, t_is_atom/1, t_is_binary/1, t_is_float/1,
		    t_is_fun/1, t_is_improper_list/1, t_is_integer/1, 
		    t_is_number/1, t_is_pid/1, t_is_port/1, t_is_ref/1,
		    t_is_tuple/1,
		    t_is_any/1, t_is_byte/1, t_is_integer/1, t_is_nil/1,
		    t_is_none/1, t_list/0, t_list/1,
		    t_list_elements/1, t_number/0, t_number_vals/1,
		    t_nil/0, t_pid/0, t_port/0, t_ref/0, t_string/0, t_tuple/0,
		    t_tuple/1, t_tuple_args/1, t_tuple_arity/1, t_sup/1,
		    t_tuple_subtypes/1,
		    t_sup/2, t_inf/2, t_subtract/2, t_none/0,
		    t_identifier/0,
		    t_is_atom/1]).

type(M, F, A) ->
    type(M, F, A, any_list(A)).

%% Arguments should be checked for undefinedness, so we do not make
%% unnecessary overapproximations.

type(erlang, halt, 0, _) -> t_none();
type(erlang, exit, 1, _) -> t_none();
type(erlang, fault, 1, _) -> t_none();
type(erlang, fault, 2, _) -> t_none();
type(erlang, error, 1, _) -> t_none();
type(erlang, error, 2, _) -> t_none();
type(erlang, throw, 1, _) -> t_none();
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
    strict(Xs, [t_number()], fun ([X]) -> X end);
type(erlang, '!', 2, Xs) ->
    Pid = t_sup([t_pid(), t_port(), t_atom(),
		 t_tuple([t_atom(), t_atom()])]),
    strict([Pid, t_any()], Xs, fun ([_, X2]) -> X2 end);
type(erlang, '+', 2, Xs) ->
    strict([t_number(), t_number()], Xs,
	   fun ([X1, X2]) ->
		   case t_is_byte(X1) andalso t_is_byte(X2) of
		       true ->
			   t_char();
		       false ->
			   case t_is_integer(X1) andalso t_is_integer(X2) of
			       true ->
		       t_integer();
			       false ->
				   case t_is_float(X1) orelse t_is_float(X2) of
				       true -> t_float();
				       false -> t_number()
				   end
			   end
		   end
	   end);
type(erlang, '-', 2, Xs) ->
    strict([t_number(), t_number()], Xs,
	   fun ([X1, X2]) ->
		   case t_is_integer(X1) andalso t_is_integer(X2) of
		       true -> t_integer();
		       false ->
			   case t_is_float(X1) orelse t_is_float(X2) of
			       true -> t_float();
			       false -> t_number()
			   end
		   end
	   end);
type(erlang, '*', 2, Xs) ->
    strict([t_number(), t_number()], Xs,
	   fun ([X1, X2]) ->
		   case t_is_integer(X1) andalso t_is_integer(X2) of
		       true -> t_integer();
		       false ->
			   case t_is_float(X1) orelse t_is_float(X2) of
			       true -> t_float();
			       false -> t_number()
			   end
		   end
	   end);
type(erlang, '/', 2, Xs) ->
    strict([t_number(), t_number()], Xs,
	   fun (_) -> t_float() end);
type(erlang, 'div', 2, Xs) ->
    strict([t_integer(), t_integer()], Xs,
	   fun (_) -> t_integer() end);
type(erlang, 'rem', 2, Xs) ->
    strict([t_integer(), t_integer()], Xs,
	   fun (_) -> t_integer() end);
type(erlang, '++', 2, Xs) ->
    strict([t_list(), t_any()], Xs,
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
    strict([t_list(), t_list()], Xs,
	   fun ([X1, _]) -> t_list(t_list_elements(X1)) end);
type(erlang, 'and', 2, Xs) ->
    strict([t_bool(), t_bool()], Xs, fun (_) -> t_bool() end);
type(erlang, 'or', 2, Xs) ->
    strict([t_bool(), t_bool()], Xs, fun (_) -> t_bool() end);
type(erlang, 'xor', 2, Xs) ->
    strict([t_bool(), t_bool()], Xs, fun (_) -> t_bool() end);
type(erlang, 'not', 1, Xs) ->
    strict([t_bool()], Xs, fun (_) -> t_bool() end);
type(erlang, 'band', 2, Xs) ->
    %% The result is not wider than the smallest argument. We need to
    %% kill any value-sets in the result.
    strict([t_integer(), t_integer()], Xs,
	   fun ([X1, X2]) -> t_sup(t_inf(X1, X2), t_byte()) end);
type(erlang, 'bor', 2, Xs) ->
    %% The result is not wider than the largest argument. We need to
    %% kill any value-sets in the result.
    strict([t_integer(), t_integer()], Xs,
	   fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bxor', 2, Xs) ->
    %% The result is not wider than the largest argument. We need to
    %% kill any value-sets in the result.
    strict([t_integer(), t_integer()], Xs,
	   fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bsr', 2, Xs) ->
    %% If the first argument is unsigned (which is the case for
    %% characters and bytes), the result is never wider. We need to kill
    %% any value-sets in the result.
    strict([t_integer(), t_integer()], Xs,
	   fun ([X, _]) -> t_sup(X, t_byte()) end);
type(erlang, 'bsl', 2, Xs) ->
    %% Not worth doing anything special here.
    strict([t_integer(), t_integer()], Xs, fun (_) -> t_integer() end);
type(erlang, 'bnot', 1, Xs) ->
    %% This returns (-X)-1, so it often gives a negative result.
    strict([t_integer()], Xs, fun (_) -> t_integer() end);
type(erlang, is_atom, 1, Xs) ->   
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_atom(Y)end, t_atom())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_binary, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_binary(Y)end, t_binary())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_float, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_float(Y)end, t_float())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_function, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_fun(Y)end, t_fun())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_integer, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_integer(Y)end, t_integer())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_list, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_improper_list(Y)end,
			      t_improper_list())
	  end,
    strict([t_any()], Xs, Fun);
type(erlang, is_number, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_number(Y)end, t_number())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_pid, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_pid(Y)end, t_pid())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_port, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_port(Y)end, t_port())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_reference, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_ref(Y)end, t_ref())end,
    strict([t_any()], Xs, Fun);
type(erlang, is_tuple, 1, Xs) ->
    Fun = fun(X)->check_guard(X, fun(Y)-> t_is_tuple(Y)end, t_tuple())end,
    strict([t_any()], Xs, Fun);
type(erlang, abs, 1, Xs) ->
    strict([t_number()], Xs, fun ([X]) -> X end);
type(erlang, append_element, 2, Xs) ->
    strict([t_tuple(), t_any()], Xs, fun (_) -> t_tuple() end);
type(erlang, atom_to_list, 1, Xs) ->
    strict([t_atom()], Xs, fun (_) -> t_string() end);
type(erlang, binary_to_list, 1, Xs) ->
    strict([t_binary()], Xs, fun (_) -> t_list(t_byte()) end);
type(erlang, binary_to_list, 3, Xs) ->
    strict([t_binary(), t_integer(), t_integer()], Xs,
	   fun (_) -> t_list(t_byte()) end);
type(erlang, concat_binary, 1, Xs) ->
    strict([t_list(t_binary())], Xs, fun (_) -> t_binary() end);
type(erlang, date, 0, _) ->
    t_tuple([t_integer(), t_integer(), t_integer()]);
type(erlang, element, 2, Xs) ->
    strict([t_integer(), t_tuple()], Xs,
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
type(erlang, float, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(erlang, float_to_list, 1, Xs) ->
    strict([t_float()], Xs, fun (_) -> t_string() end);
type(erlang, group_leader, 0, _) -> t_pid();
type(erlang, hash, 2, Xs) ->
    strict([t_any(), t_integer()], Xs, fun (_) -> t_integer() end);
type(erlang, hd, 1, Xs) ->
    strict([t_cons()], Xs, fun ([X]) -> t_cons_hd(X) end);
type(erlang, tl, 1, Xs) ->
    strict([t_cons()], Xs, fun ([X]) -> t_cons_tl(X) end);
type(erlang, integer_to_list, 1, Xs) ->
    strict([t_integer()], Xs, fun (_) -> t_string() end);
type(erlang, length, 1, Xs) ->
    strict([t_list()], Xs, fun (_) -> t_integer() end);
type(erlang, list_to_atom, 1, Xs) ->
    strict([t_list()], Xs, fun (_) -> t_atom() end);
type(erlang, list_to_binary, 1, Xs) ->
    strict([t_list()], Xs, fun (_) -> t_binary() end);
type(erlang, list_to_float, 1, Xs) ->
    strict([t_string()], Xs, fun (_) -> t_float() end);
type(erlang, list_to_integer, 1, Xs) ->
    strict([t_string()], Xs, fun (_) -> t_integer() end);
type(erlang, list_to_tuple, 1, Xs) ->
    strict([t_list()], Xs, fun (_) -> t_tuple() end);
type(erlang, localtime, 0, Xs) ->
    type(erlang, universaltime, 0, Xs);    %same
type(erlang, localtime_to_universaltime, 1, Xs) ->
    type(erlang, universaltime_to_localtime, 1, Xs);    % same
type(erlang, make_ref, 0, _) -> t_ref();
type(erlang, make_tuple, 2, Xs) ->
    strict([t_integer(), t_any()], Xs, fun (_) -> t_tuple() end);
type(erlang, node, 0, _) -> t_atom();
type(erlang, node, 1, Xs) ->
    strict([t_identifier()], Xs, fun (_) -> t_atom() end);
type(erlang, nodes, 0, _) -> t_list(t_atom());
type(erlang, nodes, 1, Xs) ->
    strict([t_sup(t_atom(), t_list(t_atom()))], Xs,
	   fun (_) -> t_list(t_atom()) end);
type(erlang, now, 0, _) ->
    t_tuple([t_integer(), t_integer(), t_integer()]);
type(erlang, open_port, 2, Xs) ->
    strict([t_sup(t_atom(), t_tuple()), t_list()], Xs,
	   fun (_) -> t_port() end);
type(erlang, phash, 2, Xs) ->
    strict([t_any(), t_integer()], Xs, fun (_) -> t_integer() end);
type(erlang, ports, 0, _) -> t_list(t_port());
type(erlang, processes, 0, _) -> t_list(t_pid());
type(erlang, round, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_integer() end);
type(erlang, self, 0, _) -> t_pid();
type(erlang, send, 2, Xs) -> type(erlang, '!', 2, Xs);  % alias
type(erlang, send_after, 3, Xs) ->
    strict([t_integer(), t_sup(t_pid(), t_atom()), t_any()], Xs,
	   fun (_) -> t_ref() end);
type(erlang, setelement, 3, Xs) ->
    strict([t_integer(), t_tuple(), t_any()], Xs,
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
    strict([t_sup(t_tuple(), t_binary())], Xs,
	   fun (_) -> t_integer() end);
type(erlang, spawn, 1, Xs) ->
    strict([t_fun()], Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 2, Xs) ->
    strict([t_atom(), t_fun()], Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 3, Xs) ->
    strict([t_atom(), t_atom(), t_list()], Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 4, Xs) ->
    strict([t_atom(), t_atom(), t_atom(), t_list()], Xs,
	   fun (_) -> t_pid() end);
type(erlang, spawn_link, 1, Xs) -> type(erlang, spawn, 1, Xs);  % same
type(erlang, spawn_link, 2, Xs) -> type(erlang, spawn, 2, Xs);  % same
type(erlang, spawn_link, 3, Xs) -> type(erlang, spawn, 3, Xs);  % same
type(erlang, spawn_link, 4, Xs) -> type(erlang, spawn, 4, Xs);  % same
type(erlang, spawn_opt, 2, Xs) -> 
    strict([t_fun(), t_list()], Xs, fun (_) -> t_pid() end);
type(erlang, spawn_opt, 3, Xs) -> 
    strict([t_atom(), t_fun(), t_list()], Xs, fun (_) -> t_pid() end);
type(erlang, spawn_opt, 4, Xs) -> 
    strict([t_atom(), t_atom(), t_list(), t_list()], Xs,
	   fun (_) -> t_pid() end);
type(erlang, split_binary, 2, Xs) ->
    strict([t_binary(), t_integer()], Xs,
	   fun (_) -> t_tuple([t_binary(), t_binary()]) end);
type(erlang, start_timer, 3, Xs) ->
    strict([t_integer(), t_sup(t_pid(), t_atom()), t_any()], Xs,
	   fun (_) -> t_ref() end);
type(erlang, term_to_binary, 1, Xs) ->
    strict([t_any()], Xs, fun (_) -> t_binary() end);
type(erlang, term_to_binary, 2, Xs) ->
    strict([t_any(), t_list()], Xs, fun (_) -> t_binary() end);
type(erlang, time, 0, _) ->
    t_tuple([t_integer(), t_integer(), t_integer()]);
type(erlang, trunc, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_integer() end);
type(erlang, tuple_to_list, 1, Xs) ->
    strict([t_tuple()], Xs,
	   fun ([X]) ->
		   case t_tuple_args(X) of
		       any -> t_list();
		       Ts -> t_list(t_sup(Ts))
		   end
	   end);
type(erlang, universaltime, 0, _) ->
    t_tuple([t_tuple([t_integer(), t_integer(), t_integer()]),
	     t_tuple([t_integer(), t_integer(), t_integer()])]);
type(erlang, universaltime_to_localtime, 1, Xs) ->
    T = t_tuple([t_tuple([t_integer(), t_integer(), t_integer()]),
		 t_tuple([t_integer(), t_integer(), t_integer()])]),
    strict([T], Xs, fun (_) -> T end);
type(lists, all, 2, Xs) ->
    strict([t_fun([t_any()], t_bool()), t_list()], Xs,
	   fun (_) -> t_bool() end);
type(lists, any, 2, Xs) ->
    strict([t_fun([t_any()], t_bool()), t_list()], Xs,
	   fun (_) -> t_bool() end);
type(lists, append, 2, Xs) -> type(erlang, '++', 2, Xs);  % alias
type(lists, filter, 2, Xs) ->
    strict([t_fun([t_any()], t_bool()), t_list()], Xs,
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
    strict([t_list()], Xs,
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
				   t_sup(t_list(t_subtract(X1,
							   t_list())),
					 X2)
			   end
		   end
	   end);
type(lists, foreach, 2, Xs) ->
    strict([t_fun([t_any()], t_any()), t_list()], Xs,
	   fun (_) -> t_atom(ok) end);
type(lists, foldl, 3, Xs) ->
    strict([t_fun([t_any(), t_any()], t_any()), t_any(), t_list()], Xs,
	   fun ([X, Y, _]) -> 
		   t_sup(t_fun_range(X), Y) end);
type(lists, foldr, 3, Xs) -> type(lists, foldl, 3, Xs);    % same
type(lists, map, 2, Xs) ->
    strict([t_fun([t_any()], t_any()), t_list()], Xs,
	   fun ([X, _]) -> t_sup(t_list(t_fun_range(X)), t_nil()) end);
type(lists, mapfoldl, 3, Xs) ->
    strict([t_fun([t_any(), t_any()], t_tuple([t_any(), t_any()])),
	    t_any(), t_list()], Xs,
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
type(lists, reverse, 1, Xs) ->
    strict([t_list()], Xs, fun ([X]) -> X end);
type(lists, reverse, 2, Xs) ->
    type(erlang, '++', 2, Xs);    % reverse-onto is just like append
type(lists, subtract, 2, Xs) -> type(erlang, '--', 2, Xs);  % alias
type(math, pi, 0, _) -> t_float();
type(math, acos, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, acosh, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, asin, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, asinh, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, atan, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, atan2, 2, Xs) ->
    strict([t_number(), t_number()], Xs, fun (_) -> t_float() end);
type(math, atanh, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, cos, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, cosh, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, erf, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, erfc, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, exp, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, log, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, log10, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, pow, 2, Xs) ->
    strict([t_number(), t_number()], Xs, fun (_) -> t_float() end);
type(math, sin, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, sinh, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, sqrt, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, tan, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
type(math, tanh, 1, Xs) ->
    strict([t_number()], Xs, fun (_) -> t_float() end);
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


arg_types(erlang, '-', 1) ->
  [t_number()];
arg_types(erlang, '+', 2) ->
  arg_types('+');
arg_types(erlang, '-', 2) ->
  arg_types('-');
arg_types(erlang, '*', 2) ->
  arg_types('*');
arg_types(erlang, 'div', 2) ->
  arg_types('div');
arg_types(erlang, 'rem', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, '++', 2) ->
  [t_list(), t_any()];
arg_types(erlang, '--', 2) ->
  [t_list(), t_list()];
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
arg_types(erlang, atom_to_list, 1) ->
  [t_atom()];
arg_types(erlang, binary_to_list, 1) ->
  [t_binary()];
arg_types(erlang, binary_to_list, 3) ->
  [t_binary(), t_integer(), t_integer()];
arg_types(erlang, concat_binary, 1) ->
  [t_list(t_binary())];
arg_types(erlang, element, 2) ->
  [t_integer(), t_tuple()];
arg_types(erlang, float, 1) ->
  [t_number()];
arg_types(erlang, float_to_list, 1) ->
  [t_float()];
arg_types(erlang, hash, 2) ->
  [t_any(), t_integer()];
arg_types(erlang, hd, 1) ->
  [t_cons()];
arg_types(erlang, tl, 1) ->
  [t_cons()];
arg_types(erlang, integer_to_list, 1) ->
  [t_integer()];
arg_types(erlang, length, 1) ->
  [t_list()];
arg_types(erlang, list_to_atom, 1) ->
  [t_list()];
arg_types(erlang, list_to_binary, 1) ->
  [t_list()];
arg_types(erlang, list_to_float, 1) ->
  [t_string()];
arg_types(erlang, list_to_integer, 1) ->
  [t_string()];
arg_types(erlang, list_to_tuple, 1) ->
  [t_list()];
arg_types(erlang, make_tuple, 2) ->
  [t_integer(), t_any()];
arg_types(erlang, node, 1) ->
  [t_identifier()];
arg_types(erlang, nodes, 1) ->
  [t_sup(t_atom(), t_list(t_atom()))];
arg_types(erlang, open_port, 2) ->
  [t_sup(t_atom(), t_tuple()), t_list()];
arg_types(erlang, phash, 2) ->
  [t_any(), t_integer()];
arg_types(erlang, round, 1) ->
  [t_number()];
arg_types(erlang, send, 2) -> 
  arg_types(erlang, '!', 2);  % alias
arg_types(erlang, '!', 2) ->
  Pid = t_sup([t_pid(), t_port(), t_atom(),
	       t_tuple([t_atom(), t_atom()])]),
  [Pid, t_any()];
arg_types(erlang, send_after, 3) ->
  [t_integer(), t_sup(t_pid(), t_atom()), t_any()];
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
arg_types(erlang, term_to_binary, 1) ->
  [t_any()];
arg_types(erlang, term_to_binary, 2) ->
  [t_any(), t_list()];
arg_types(erlang, trunc, 1) ->
  [t_number()];
arg_types(erlang, tuple_to_list, 1) ->
  [t_tuple()];
arg_types(erlang, universaltime_to_localtime, 1) ->
  T = t_tuple([t_tuple([t_integer(), t_integer(), t_integer()]),
	       t_tuple([t_integer(), t_integer(), t_integer()])]),
  [T];
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
arg_types(lists, last, 1) ->
  [t_list()];
arg_types(lists, map, 2) ->
  [t_fun([t_any()], t_any()), t_list()];
arg_types(lists, mapfoldl, 3) ->
  [t_fun([t_any(), t_any()], t_tuple([t_any(), t_any()])),t_any(), t_list()];
arg_types(lists, mapfoldr, 3) -> 
  arg_types(lists, mapfoldl, 3); % same
arg_types(lists, nth, 2) ->
  [t_integer(), t_list()];
arg_types(lists, nthtail, 2) ->
  [t_integer(), t_list()];
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
arg_types(_, _, _) ->  
  any.                     % safe approximation for all functions.
