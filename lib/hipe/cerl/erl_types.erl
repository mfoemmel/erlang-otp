%% -*- erlang-indent-level: 4 -*-
%% =====================================================================
%% Basic representation of Erlang types.
%%
%% Copyright (C) 2000-2003 Richard Carlsson
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
%% @doc Basic representation of Erlang types.
%%
%% <p>See the <a href="#type-type"><code>type()</code></a> datatype
%% for details.</p>
%%
%% @type type(). An abstract representation of Erlang data types.
%%
%% <p>The following types can be represented:
%% <dl>
%%   <dt><a href="#t_none-0"><code>none()</code></a></dt>
%%     <dd>The empty type. If some type <code>T</code> is a subtype
%%     of <code>none()</code>, then <code>T =
%%     none()</code>. In general, type constructors are <em>strict</em>,
%%     so that if some component is <code>none()</code>, the result is
%%     also <code>none()</code>, unless otherwise noted.</dd>
%%   <dt><a href="#t_any-0"><code>any()</code></a></dt>
%%     <dd>The "any value" type.  All types are subtypes of
%%     <code>any()</code>.</dd>
%%   <dt><a href="#t_product-1"><code>&lt;T1, ... Tn&gt;</code></a></dt>
%%     <dd>Cartesian product. There is one product type for each <code>n
%%     >= 0</code>, where <code>n</code> is called the <em>degree</em>
%%     of the type. The degree of each <code>Ti</code> must be 1. Note
%%     that <code>&lt;T&gt; = T</code>, and that <code>&lt;&gt;</code>
%%     is unique. The product constructor is strict.</dd>
%%   <dt><a href="#t_number-0"><code>number()</code></a></dt>
%%     <dd>All Erlang numbers. This is the union of the
%%     <code>integer()</code> and <code>float()</code> types:
%%     <dl>
%%       <dt><a href="#t_integer-0"><code>integer()</code></a></dt>
%%         <dd>All integer values. In Erlang, characters are a subtype
%%         of the integers:
%%         <dl> 
%%           <dt><a href="#t_char-0"><code>char()</code></a></dt>
%%             <dd>The nonnegative integers representing character
%%             codes. The upper limit is not yet defined, but is at
%%             least 255 (<code>16#ff</code>) and at most
%%             <code>16#10ffff</code> (full Unicode).</dd>
%%           <dt><a href="#t_byte-0"><code>byte()</code></a></dt>
%%             <dd>The integers in the range <code>[0,255]</code>. This
%%             is a subset of <code>char()</code>.</dd>
%%         </dl></dd>
%%   <dt><a href="#t_float-0"><code>float()</code></a></dt>
%%     <dd>All floating-point values.</dd>
%% </dl></dd>
%%   <dt><a href="#t_atom-0"><code>atom()</code></a></dt>
%%     <dd>All atoms.
%%     <dl>
%%       <dt><a href="#t_bool-0"><code>bool()</code></a></dt>
%%         <dd>The atoms <code>true</code> and <code>false</code>. This
%%         is a subset of <code>atom().</code></dd>
%%     </dl></dd>
%%   <dt><a href="#t_tuple-0"><code>tuple()</code></a></dt>
%%     <dd>All tuples <code>{T1, ..., Tn}</code>, for all <code>n >=
%%     0</code>, where the degree of each <code>Ti</code> is 1
%%     (see <code>product()</code>). Note that <code>{}</code> is
%%     unique. The tuple constructor is strict.</dd>
%%   <dt><a href="#t_improper_list-0"><code>improper_list()</code></a></dt>
%%     <dd>All proper and non-proper lists, i.e., all <code>[T, ...,
%%     T]</code> and <code>[T, ..., T | T]</code> (where the degree of
%%     <code>T</code> is 1), including the empty list
%%     <code>[]</code>. Note that for non-proper lists, the "tail" is
%%     regarded as one of the elements.  The list type constructors
%%     are strict.
%%     <dl>
%%       <dt><a href="#t_nonempty_improper_list-0"><code>nonempty_improper_list()</code></a></dt>
%%         <dd>All nonempty proper and non-proper lists <code>[... |
%%         ...]</code>.</dd>
%%       <dt><a href="#t_nil-0"><code>nil()</code></a></dt>
%%         <dd>The empty list <code>[]</code>.</dd>
%%       <dt><a href="#t_list-0"><code>list()</code></a></dt>
%%         <dd>All proper (nil-terminated) lists, i.e., all <code>[T,
%%         ..., T]</code>, including <code>[]</code>.</dd>
%%       <dt><a href="#t_nonempty_list-0"><code>nonempty_list()</code></a></dt>
%%         <dd>All proper lists except <code>[]</code>.</dd>
%%     </dl></dd>
%%   <dt><a href="#t_fun-0"><code>function()</code></a></dt>
%%      <dd>All functions <code>(T1, ... Tn) -> T0</code>, for all
%%      <code>n >= 0</code>, where the degree of each <code>Ti</code>
%%      is 1, except for <code>T0</code> which can be of any
%%      degree. The function type constructor is <em>not</em> strict:
%%      it is possible to represent functions with (partially)
%%      undefined domain and/or undefined range.</dd>
%%   <dt><a href="#t_binary-0"><code>binary()</code></a></dt>
%%      <dd>Binary-objects.</dd>
%%   <dt><a href="#t_pid-0"><code>pid()</code></a></dt>
%%      <dd>Process identifiers.</dd>
%%   <dt><a href="#t_port-0"><code>port()</code></a></dt>
%%      <dd>Port identifiers.</dd>
%%   <dt><a href="#t_ref-0"><code>ref()</code></a></dt>
%%      <dd>Unique references.</dd>
%% </dl>
%%
%% In addition, types can contain <a href="#t_var-1">type variables</a>,
%% and <a href="#t_subst-2">substitution</a> and <a href="#t_var-1">type
%% unification</a> can be performed. The function <a
%% href="#t_from_term-1"><code>t_from_term/1</code></a> gives the type
%% of a given Erlang term. To make some tasks easier, there are <a
%% href="#t_is_data-1">functions for handling data constructor types in
%% a unified way</a>.</p>
%%
%% <p>Furthermore, there are functions for computing the <a
%% href="#t_sup-2">supremum (least upper bound)</a> and <a
%% href="#t_inf-2">infimum (greatest lower bound)</a> of two types, and
%% testing if one type is a <a href="#t_is_subtype-2">subtype</a> of
%% another. Type representations can also be <a
%% href="#t_limit-2">limited (pruned) to a specified depth</a>.</p>
%%
%% <p>Types can be compared for exact equality by using the exact term
%% equality operator <code>=:=</code> in Erlang.</p>

-module(erl_types).

-define(NO_UNUSED, true).
%-define(DEBUG, true).

-export([t_any/0, t_atom/0, t_atom/1, t_atom_vals/1, t_binary/0,
	 t_bool/0, t_byte/0, t_char/0,
	 t_components/1,
	 t_cons/0, t_cons/2, t_cons_hd/1, t_cons_tl/1,
	 t_float/0, t_from_term/1, t_from_range/2,
	 t_fun/0, t_fun/1, t_fun/2,
	 t_fun_args/1,
	 t_fun_range/1, t_identifier/0,
	 t_inf/2, t_inf_lists/2,
	 t_integer/0, t_integer/1,
	 t_improper_list/0,
	 t_is_any/1, t_is_atom/1,
	 t_is_binary/1, t_is_bool/1, t_is_byte/1,
	 t_is_char/1,
	 t_is_cons/1, t_is_improper_list/1,
	 t_is_equal/2,
	 t_is_none/1,
	 t_is_float/1, t_is_fun/1,
	 t_is_integer/1, t_is_list/1,
	 t_is_nil/1,
	 t_is_number/1,
	 t_is_pid/1, t_is_port/1, t_is_ref/1,
	 t_is_subtype/2,
	 t_is_tuple/1,
	 t_is_var/1,
	 t_limit/2,
	 t_list/0, t_list/1, t_list_elements/1, t_nil/0,
	 t_number/0, t_number/1, t_number_vals/1,
	 t_pid/0, t_port/0, t_product/1, t_ref/0, t_string/0,
	 t_subst/2, t_subtract/2, t_sup/1, t_sup/2,
	 t_to_string/1,
	 t_tuple/0, t_tuple/1, t_tuple_args/1,
	 t_tuple_arity/1, t_tuple_arities/1, t_tuple_subtypes/1,
	 t_unify/2,
	 t_var/1, t_var_name/1,
	 t_none/0
	]).
-ifndef(NO_UNUSED).
-export([t_bool/1, t_byte/1, t_char/1, t_cons/1, t_data_arity/1,
	 t_degree/1, t_data_args/1, t_from_term/2,
	 t_fun_arity/1, t_improper_list/1, t_inf/1, t_is_atom/2, t_is_data/1,
	 t_is_identifier/1, t_is_number/2, t_is_string/1,
	 t_is_n_tuple/2, t_is_nonempty_list/1,
	 t_nonempty_improper_list/0, t_nonempty_improper_list/1,
	 t_nonempty_list/0, t_nonempty_list/1, t_nonempty_string/0,
	 t_tuple_max_arity/1, t_tuple_min_arity/1]).
-endif.


%% ---------------------------------------------------------------------

%% Equality on type descriptors is the normal Erlang term equality.

%% Internal generic type representation definitions

-ifndef(DEBUG).
-define(none, none).		%% no type
-define(any, any).		%% any-value
-else.
-define(none, none_).
-define(any, any_).
-endif.

-record(var, {n}).		%% type variable
-record(c, {c, n, as}).		%% generic constructor type
-record(set, {n, s}).		%% value-set type (always wrapped)
-record(cs, {as}).		%% constructor list (always wrapped)
-record(m, {m}).		%% bit mask (always wrapped)

%% Value set macros. The upper set size limit should at least be
%% strictly smaller than 'byte' (i.e., 255), and large enough for most
%% sets of constants used for switching.
-define(value_set(S), #c{c = S, n = 0, as = []}).
-define(singleton(V), ?value_set(#set{n = 1, s = [V]})).
-define(booleans, ?value_set(#set{n = 2, s = [false,true]})).
-define(SET_LIMIT, 32).

%% Generic disjoint union macros. The list of subtypes is kept as an
%% ordered list, sorted on constructor name and arity.

-define(dunion(Ts), #c{c = #cs{as = Ts}, n = 0, as = []}).
-define(DUNION_LIMIT, 4).

%% Mask wrapper for same-arity tuples separated by tagging.

-define(tuple_mask(M, Ts), #c{c = #m{m = M}, n = 1,
			      as = [?dunion(Ts)]}).

%% Union type wrapper representation
%%
%%    Union: Atoms
%%           |
%%           Numbers
%%           |
%%           Lists
%%           |
%%           Tuples
%%           |
%%           Functions
%%           |
%%           Binaries
%%           |
%%           Identifiers

-define(union(Ts), #c{c = union, n = 7, as = Ts}).

-define(u_atom(X, A),		?union([A, X, X, X, X, X, X])).
-define(u_number(X, N),		?union([X, N, X, X, X, X, X])).
-define(u_list(X, L),		?union([X, X, L, X, X, X, X])).
-define(u_tuple(X, T),		?union([X, X, X, T, X, X, X])).
-define(u_fun(X, F),		?union([X, X, X, X, F, X, X])).
-define(u_binary(X, B),		?union([X, X, X, X, X, B, X])).
-define(u_identifier(X, I),	?union([X, X, X, X, X, X, I])).


%% ---------------------------------------------------------------------

%% @spec t_none() -> type()
%%
%% @doc Returns the <code>none()</code>, i.e., "empty" type. No
%% other type is a subtype of <code>none()</code>
%%
%% @see t_is_none/1
%% @see t_any/0

t_none() -> ?none.


%% @spec t_is_none(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is the
%% <code>none()</code> type, otherwise <code>false</code>.
%%
%% @see t_none/0
 
t_is_none(?none) -> true;
t_is_none(_) -> false.


%% @spec t_any() -> type()
%%
%% @doc Returns the <code>any()</code>, i.e., "any value" type. All
%% types are subtypes of <code>any()</code>.
%%
%% @see t_is_any/1
%% @see t_none/0

t_any() -> ?any.


%% @spec t_is_any(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is exactly the
%% <code>any()</code> type, otherwise <code>false</code>. Note that
%% <code>true</code> here means that <code>any()</code> is a subtype of
%% <code>T</code>, not the other way around.
%%
%% @see t_any/0

t_is_any(?any) -> true;
t_is_any(_) -> false.


%% ---------------------------------------------------------------------

%% Representation of products:
%%
%%    Types: Type list

-define(product(N, Ts), #c{c = product, n = N, as = Ts}).


%% @spec t_product(Types) -> type()
%%
%%    Types = [type()] | integer()
%%
%% @doc Returns a cartesian product type. If <code>Types</code> is
%% <code>[T1, ..., Tn]</code>, the result is the type <code>&lt;T1, ...,
%% Tn&gt;</code>. If all of <code>[T1, ..., Tn]</code> are
%% <code>none()</code>, the result is also
%% <code>none()</code>. Note that if the degree <code>n</code> is
%% 1, this function is the identity function, i.e., <code>t_product([T])
%% = T</code>.
%%
%% <p>Passing an integer <code>n</code> as argument is equivalent to
%% passing a list of length <code>n</code>, where each component is the
%% <code>any()</code> type.</p>
%%
%% <p>Use <code>t_degree/1</code> to check the degree of any type.</p>
%%
%% <p>Note: product types are not part of the Erlang language, but are
%% used in Core Erlang, and are supported here in order to facilitate
%% type representation at the Core Erlang level. The components of a
%% product may not themselves be products (of degree other than 1).</p>
%%
%% @see t_any/0
%% @see t_none/0
%% @see t_degree/1
%% @see t_components/1

t_product([]) -> ?product(0, []);
t_product([?none]) -> ?none;
t_product([A]) -> A;
t_product(As = [_ | _]) ->
    case all_t_none(As) of
	true ->
	    ?none;
	false ->
	    ?product(length(As), As)
    end;
t_product(N) when is_integer(N), N >= 0 ->
    ?product(N, mk_any_list(N)).


%% @spec t_degree(T::type()) -> integer() | any | none
%%
%% @doc Returns the degree of a (product) type. If <code>T</code> is
%% <code>&lt;T1, ..., Tn&gt;</code>, the result is the integer
%% <code>n</code>. If <code>T</code> is <code>any()</code>, the result
%% is <code>any</code>. If <code>T</code> is <code>none()</code>,
%% the result is <code>none</code>.
%%
%% <p>Note that this is defined on all types: e.g.,
%% <code>t_degree(t_integer()) = 1</code>.</p>
%%
%% @see t_product/1
%% @see t_components/1
 
-ifndef(NO_UNUSED).
t_degree(?product(N, _)) -> N;
t_degree(?any) -> any;
t_degree(?none) -> none;
t_degree(_) -> 1.
-endif.
%% @clear

%% @spec t_components(T::type()) -> [type()] | any | none
%%
%% @doc Returns the component types of a (product) type. If
%% <code>T</code> is <code>&lt;T1, ..., Tn&gt;</code>, the result is
%% <code>[T1, ..., Tn]</code>. If <code>T</code> is
%% <code>any()</code>, the result is <code>any</code>.  If
%% <code>T</code> is <code>none()</code>, the result is
%% <code>none</code>.
%%
%% <p>Note that this is defined on all types: e.g.,
%% <code>t_components(t_integer()) = [t_integer()]</code>.</p>
%%
%% @see t_product/1
%% @see t_degree/1

t_components(?product(_, As)) -> As;
t_components(?any) -> any;
t_components(?none) -> none;
t_components(T) -> [T].


%% ---------------------------------------------------------------------

%% @spec t_var(term()) -> type()
%%
%% @doc Returns a type variable whose name is the given term. Typically,
%% the name is an atom or an integer.
%%
%% @see t_var_name/1
%% @see t_is_var/1
%% @see t_subst/2
%% @see t_unify/2

t_var(N) -> #var{n = N}.

%% @spec t_is_var(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is a type variable,
%% otherwise <code>false</code>.
%%
%% @see t_var/1

t_is_var(#var{}) -> true;
t_is_var(_) -> false.


%% @spec t_var_name(t_var()) -> term()
%%
%% @doc Returns the name of a type variable. This can be any term,
%% typically an atom or an integer.
%%
%% @see t_var/1

t_var_name(#var{n = N}) -> N.


%% ---------------------------------------------------------------------

%% Representation of numbers:
%%
%%    Values: Any
%%            |
%%            Value Set
%%
%%    Class: Any
%%           |
%%           Floating-point
%%           |
%%           Integer -- Subclass: Any
%%                                |
%%                                Character -- Subclass: Any
%%                                                       |
%%                                                       Byte
%%
%% Note that Values is not independent - it is a restriction on the
%% Class. This must be handled in functions like t_sup and
%% t_subtract. Values is not used (i.e., is always any()), if Class is
%% Floating-point.

-define(c_number(V, C), #c{c = number, n = 2, as = [V, C]}).
-define(number(V, C), ?u_number(?none, ?c_number(V, C))).
-define(is_number(V, C), ?u_number(?none, ?c_number(V, C))).
-define(has_number(V, C), ?u_number(_, ?c_number(V, C))).
-define(number_class_float, #c{c = float, n = 0, as = []}).
-define(number_class_is_float, #c{c = float}).
-define(number_class_integer(C), #c{c = integer, n = 1, as = [C]}).
-define(number_class_is_integer(C), #c{c = integer, n = 1, as = [C]}).

-define(float, ?number(?any, ?number_class_float)).
-define(is_float, ?is_number(_, ?number_class_is_float)).

-define(integer(V, C), ?number(V, ?number_class_integer(C))).
-define(is_integer(V, C), ?is_number(V, ?number_class_is_integer(C))).
-define(integer_class_char(C), #c{c = char, n = 1, as = [C]}).
-define(integer_class_is_char(C), #c{c = char, n = 1, as = [C]}).

-define(char(V, C), ?integer(V, ?integer_class_char(C))).
-define(is_char(V, C), ?is_integer(V, ?integer_class_is_char(C))).
-define(char_class_byte, #c{c = byte, n = 0, as = []}).
-define(char_class_is_byte, #c{c = byte}).

-define(byte(V), ?char(V, ?char_class_byte)).
-define(is_byte(V), ?char(V, ?char_class_is_byte)).

-define(MAX_BYTE, 255).
-define(MAX_CHAR, 16#10ffff).

%% Note that we need not check for floats - they don't have value sets.
filter_numbers(S, ?number_class_is_integer(C)) ->
    case C of
	?integer_class_is_char(C1) ->
	    case C1 of
		?char_class_is_byte ->
		    F = fun (X) when X >= 0, X =< ?MAX_BYTE -> true;
			    (_) -> false
			end,
		    set_filter(F, S);
		?any ->
		    F = fun (X) when X >= 0, X =< ?MAX_CHAR -> true;
			    (_) -> false
			end,
		    set_filter(F, S)
	    end;
	?any ->
	    S
    end;
filter_numbers(_, ?none) -> #set{n = 0};
filter_numbers(S, _) -> S.

%% Note that we need not check for floats - they don't have value sets.
classify_numbers(?any) -> ?any;
classify_numbers(#set{n = 0}) -> ?none;
classify_numbers(#set{s = S}) ->
    A = ?number_class_integer(?integer_class_char(?char_class_byte)),
    classify_numbers_1(S, A).

classify_numbers_1([N | Ns], A) when N >= 0, N =< ?MAX_BYTE ->
    classify_numbers_1(Ns, A);
classify_numbers_1([_ | _] = Ns, _) ->
    A = ?number_class_integer(?integer_class_char(?any)),
    classify_numbers_2(Ns, A);
classify_numbers_1([], A) -> A.

classify_numbers_2([N | Ns], A) when N >= 0, N =< ?MAX_CHAR ->
    classify_numbers_2(Ns, A);
classify_numbers_2([_ | _], _) ->
    ?number_class_integer(?any);
classify_numbers_2([], A) -> A.


%% @spec t_number() -> type()
%%
%% @doc Returns the <code>number()</code> type. This is the union of
%% the <code>integer()</code> and <code>float()</code> types.
%%
%% @see t_is_number/1
%% @see t_number/1
%% @see t_integer/0
%% @see t_float/0

t_number() -> ?number(?any, ?any).


%% @spec t_number(N::number()) -> type()
%%
%% @doc Returns an <code>integer(N)</code> or <code>float(N)</code> type
%% corresponding to the given value <code>N</code>.
%%
%% @see t_number/0
%% @see t_integer/1
%% @see t_float/0
%% @see t_number_vals/1

t_number(X) when is_integer(X) -> t_integer(X);
t_number(X) when is_float(X) -> t_float().


%% @spec t_is_number(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>number()</code>, otherwise <code>false</code>.
%%
%% @see t_is_number/2
%% @see t_number/0

t_is_number(?is_number(_, _)) -> true;
t_is_number(_) -> false.


%% @spec t_is_number(N::number(), T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is exactly the
%% number <code>N</code>, otherwise <code>false</code>.
%%
%% @see t_is_number/1
%% @see t_number/0

-ifndef(NO_UNUSED).
t_is_number(N, ?is_number(?singleton(N), _)) -> true;
t_is_number(_, _) -> false.
-endif.
%% @clear


%% @spec t_number_vals(T::type()) -> [number()]
%%
%% @doc Returns the ordered list of numbers in <code>T</code>, or
%% <code>any</code> if <code>T</code> can contain any number. Note that
%% the returned list can be empty. (This function is defined on all
%% types.)
%%
%% @see t_number/1

t_number_vals(?any) -> any;
t_number_vals(?has_number(?any, _)) -> any;
t_number_vals(?has_number(?value_set(S), _)) -> set_to_list(S);
t_number_vals(_) -> [].


%% @spec t_float() -> type()
%% @doc Returns the <code>float()</code> type.
%% @see t_is_float/1
%% @see t_number/0

t_float() -> ?float.


%% @spec t_is_float(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>float()</code>, otherwise <code>false</code>.
%%
%% @see t_float/0

t_is_float(?is_float) -> true;
t_is_float(_) -> false.


%% @spec t_integer() -> type()
%% @doc Returns the <code>integer()</code> type.
%% @see t_integer/1
%% @see t_is_integer/1
%% @see t_number/0
%% @see t_char/0
%% @see t_byte/0

t_integer() -> ?integer(?any, ?any).


%% @spec t_integer(N::integer()) -> type()
%%
%% @doc Returns the <code>integer(N)</code> type for the given
%% <code>N</code>.
%%
%% @see t_integer/0
%% @see t_char/1

t_integer(V) when is_integer(V), V >= 0, V =< ?MAX_BYTE ->
    t_byte(V);
t_integer(V) when is_integer(V), V >= 0, V =< ?MAX_CHAR ->
    t_char(V);
t_integer(V) when is_integer(V) ->
    ?integer(?singleton(V), ?any).


%% @spec t_from_range(X::integer(), Y::integer()) -> type()
%%
%% @doc Returns a type that is in <code>integer()</code> and contains
%% all the integers between <code>X</code> and <code>Y</code>,
%% inclusive.
%%
%% @see t_integer/0

t_from_range(X, Y) when integer(X), integer(Y) ->
    if	X =< Y ->
	    t_from_range_1(X, Y);
	true ->
	    t_from_range_1(Y, X)
    end.

t_from_range_1(X, Y) ->
    if Y - X =< ?SET_LIMIT ->
	    t_from_range_1(X, Y, t_none());
       true ->
	    t_from_range_2(X, Y)
    end.

t_from_range_1(X, Y, T) when X =< Y ->
    t_from_range_1(X + 1, Y, t_sup(t_integer(X), T));
t_from_range_1(_, _, T) ->
    T.

t_from_range_2(X, Y) when X >=0 ->
    if Y =< ?MAX_BYTE ->
	    t_byte();
       Y =< ?MAX_CHAR ->
	    t_char();
       true ->
	    t_integer()
    end;
t_from_range_2(_, _) ->
    t_integer().


%% @spec t_is_integer(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>integer()</code>, otherwise <code>false</code>.
%%
%% @see t_integer/0

t_is_integer(?is_integer(_, _)) -> true;
t_is_integer(_) -> false.


%% @spec t_char() -> type()
%%
%% @doc Returns the <code>char()</code> type. This is a subset of the
%% <code>integer()</code> type, corresponding to values in the range
%% <code>[0, 16#10ffff]</code> (i.e., the set of Unicode character
%% codes).
%%
%% @see t_integer/0
%% @see t_char/1
%% @see t_is_char/1
%% @see t_string/0

t_char() -> ?char(?any, ?any).


%% @spec t_char(C::char()) -> type()
%%
%% @doc Returns the <code>char(C)</code> type for the given
%% <code>C</code>. This is also an integer type. Values must be in the
%% range <code>[0, 16#10ffff]</code> (i.e., the set of Unicode character
%% codes).
%%
%% @see t_char/0
%% @see t_is_char/1
%% @see t_string/0

t_char(V) when is_integer(V), V >= 0, V =< ?MAX_BYTE ->
    t_byte(V);
t_char(V) when is_integer(V), V >= 0, V =< ?MAX_CHAR ->
    ?char(?singleton(V), ?any).


%% @spec t_is_char(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>char()</code>, otherwise <code>false</code>.
%%
%% @see t_char/0

t_is_char(?is_char(_, _)) -> true;
t_is_char(_) -> false.


%% @spec t_byte() -> type()
%%
%% @doc Returns the <code>byte()</code> type. This is a subset of the
%% <code>integer()</code> type, corresponding to values in the range
%% <code>[0, 255]</code>. It is also a subset of the <code>char()</code>
%% type.
%%
%% @see t_integer/0
%% @see t_char/0
%% @see t_byte/1
%% @see t_is_byte/1

t_byte() -> ?byte(?any).


%% @spec t_byte(integer()) -> type()
%%
%% @doc Returns the <code>byte(N)</code> type for the given
%% <code>N</code>. This is also an integer type. Values must be in the
%% range <code>[0, 255]</code>.
%%
%% @see t_byte/0
%% @see t_is_byte/1

t_byte(V) when is_integer(V), V >= 0, V =< ?MAX_BYTE ->
    ?byte(?singleton(V)).


%% @spec t_is_byte(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>byte()</code>, otherwise <code>false</code>.
%%
%% @see t_byte/0

t_is_byte(?is_byte(_)) -> true;
t_is_byte(_) -> false.


%% ---------------------------------------------------------------------

%% Representation of atoms:
%%
%%    Values: Any
%%            |
%%            Value Set
%%
%%    Class: Any
%%           |
%%           Boolean
%%
%% Note that Values is not independent - it is a restriction on the
%% Class. This must be handled in functions like t_sup and
%% t_subtract. Also note that the subset of boolean atoms is a small
%% finite set, and that Values is never Any if the class is Boolean.

-define(c_atom(V, C), #c{c = atom, n = 2, as = [V, C]}).
-define(atom(V, C), ?u_atom(?none, ?c_atom(V, C))).
-define(is_atom(V, C), ?u_atom(?none, ?c_atom(V, C))).
-define(has_atom(V, C), ?u_atom(_, ?c_atom(V, C))).
-define(atom_class_bool, #c{c = bool, n = 0, as = []}).
-define(atom_class_is_bool, #c{c = bool}).

-define(bool, ?atom(?booleans, ?atom_class_bool)).
-define(is_bool, ?is_atom(_, ?atom_class_is_bool)).


%% @spec t_atom() -> type()
%% @doc Returns the <code>atom()</code> type.
%% @see t_atom/1
%% @see t_is_atom/1
%% @see t_bool/0

t_atom() -> ?atom(?any, ?any).


%% @spec t_atom(A::atom()) -> type()
%%
%% @doc Returns the <code>atom(A)</code> type for the given
%% <code>A</code>.
%%
%% @see t_atom/0
%% @see t_is_atom/1
%% @see t_bool/0
%% @see t_atom_vals/1

t_atom(true) -> ?atom(?singleton(true), ?atom_class_bool);
t_atom(false) -> ?atom(?singleton(false), ?atom_class_bool);
t_atom(V) when is_atom(V) -> ?atom(?singleton(V), ?any).


%% @spec t_is_atom(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>atom()</code>, otherwise <code>false</code>.
%%
%% @see t_is_atom/2
%% @see t_atom/0

t_is_atom(?is_atom(_, _)) -> true;
t_is_atom(_) -> false.


%% @spec t_is_atom(A::atom(), T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is exactly the atom
%% <code>A</code>, otherwise <code>false</code>.
%%
%% @see t_is_atom/1
%% @see t_atom/0

-ifndef(NO_UNUSED).
t_is_atom(A, ?is_atom(?singleton(A), _)) -> true;
t_is_atom(_, _) -> false.
-endif.
%% @clear


%% @spec t_atom_vals(T::type()) -> [atom()] | any
%%
%% @doc Returns the ordered list of atoms in <code>T</code>, or
%% <code>any</code> if <code>T</code> can contain any atom. Note that
%% the returned list can be empty. (This function is defined on all
%% types.)
%%
%% @see t_atom/1

t_atom_vals(?any) -> any;
t_atom_vals(?has_atom(?any, _)) -> any;
t_atom_vals(?has_atom(?value_set(S), _)) -> set_to_list(S);
t_atom_vals(_) -> [].


%% @spec t_bool() -> type()
%%
%% @doc Returns the <code>bool()</code> type. This is the subset of
%% <code>atom()</code> containing only the atoms <code>true</code> and
%% <code>false</code>.
%%
%% @see t_atom/0
%% @see t_is_bool/1

t_bool() -> ?bool.


%% @spec t_bool(B::true | false) -> type()
%%
%% @doc Returns the <code>bool(B)</code> type for the given
%% <code>B</code>.
%%
%% @see t_bool/0
%% @see t_atom/1

-ifndef(NO_UNUSED).
t_bool(true) -> t_atom(true);
t_bool(false) -> t_atom(false).
-endif.
%% @clear


%% @spec t_is_bool(type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>bool()</code>, otherwise <code>false</code>.
%%
%% @see t_bool/0

t_is_bool(?is_bool) -> true;
t_is_bool(_) -> false.


%% ---------------------------------------------------------------------

%% Representation of tuples:
%%
%%    Tuple: Any
%%	     |
%%           Arities -- Arity: Mask -- Tuples -- Tagged Tuple: Elements
%%                        +              +
%%                        +              +

-define(c_tuple(T), #c{c = tuple, n = 1, as = [T]}).
-define(tuple(T), ?u_tuple(?none, ?c_tuple(T))).
-define(is_tuple(T), ?u_tuple(?none, ?c_tuple(T))).
-define(tuple_arities(Ts), ?dunion(Ts)).
-define(tuple_arity(N, T), #c{c = N, n = 1, as = [T]}).
-define(tuple_types(M, N, Ts), ?tuple_arity(N, ?tuple_mask(M, Ts))).
-define(tuple_type(K, N, Ts), #c{c = K, n = N, as = Ts}).
-define(n_tuple(M, K, N, Ts),
	?tuple_types(M, N, [?tuple_type(K, N, Ts)])).
-define(n_tuple_notag(N, Ts), ?n_tuple(0, {}, N, Ts)).


%% @spec t_tuple() -> type()
%%
%% @doc Returns the <code>tuple()</code> type. This represents all
%% tuples <code>{...}</code>, including the unit tuple <code>{}</code>.
%%
%% @see t_tuple/1
%% @see t_is_tuple/1

t_tuple() -> ?tuple(?any).


%% @spec t_tuple(Args) -> type()
%%         Args = [type()] | integer()
%%
%% @doc Returns a tuple type <code>{T1, ..., Tn}</code>. If
%% <code>Args</code> is <code>[E1, ..., En]</code>, then <code>Ti =
%% Ei</code> for all <code>i</code>. If <code>Args</code> is an integer
%% <code>n</code>, then <code>Ti = any()</code> for all
%% <code>i</code>. If any of <code>[E1, ..., En]</code> are
%% <code>none()</code>, the result is also
%% <code>none()</code>.
%%
%% @see t_tuple/0
%% @see t_is_tuple/1
%% @see t_is_n_tuple/1
%% @see t_tuple_args/1
%% @see t_tuple_arity/1
%% @see t_tuple_subtypes/1
%% @see t_tuple_arities/1

t_tuple(As) when is_list(As) ->
    case any_t_none(As) of
 	true ->
 	    ?none;
 	false ->
	    {M, K} = tuple_signature(As),
	    N = length(As),
	    T = ?tuple_type(K, N, As),
  	    ?tuple(?tuple_arities([?tuple_types(M, N, [T])]))
    end;
t_tuple(N) when is_integer(N), N >= 0 ->
    ?tuple(?tuple_arities([?n_tuple_notag(N, mk_any_list(N))])).

%% The least significant nonzero bit in M corresponds to the last
%% element in the key, and also to the last constant atom/integer in the
%% tuple, i.e., the mask is reversed w.r.t. the key. (The key is in the
%% same order as the tuple, mostly for reasons of printing.)

tuple_signature(As) ->
    tuple_signature(As, 0, []).

tuple_signature([A | As], M, Ts) ->
    M1 = M bsl 1,
    case A of
 	?is_atom(?singleton(T), _) ->
  	    tuple_signature(As, M1 + 1, [T | Ts]);
  	?is_integer(?singleton(T), _) ->
  	    tuple_signature(As, M1 + 1, [T | Ts]);
  	_ ->
	    tuple_signature(As, M1, Ts)
    end;
tuple_signature([], M, Ts) ->
    {M, list_to_tuple(lists:reverse(Ts))}.


%% @spec t_is_tuple(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>tuple()</code>, or <code>false</code> otherwise.
%%
%% @see t_tuple/0
%% @see t_is_n_tuple/2

t_is_tuple(?is_tuple(_)) -> true;
t_is_tuple(_) -> false.


%% @spec t_is_n_tuple(N::integer(), T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is <code>{T1, ...,
%% Tn}</code>, where <code>n = N</code>, or <code>false</code>
%% otherwise.
%%
%% @see t_tuple/0
%% @see t_is_tuple/1
%% @see t_tuple_arity/1

-ifndef(NO_UNUSED).
t_is_n_tuple(_, ?is_tuple(?any)) -> false;
t_is_n_tuple(N, ?is_tuple(?tuple_arities([?n_tuple(_, _, N, _)]))) -> true;
t_is_n_tuple(_, ?is_tuple(_)) -> false.
-endif.
%% @clear


%% @spec t_tuple_arity(T::type()) -> integer() | any
%%
%% @doc Returns the arity of a tuple type. If <code>T</code> is
%% <code>{T1, ..., Tn}</code>, the result is the integer <code>n</code>,
%% otherwise the result is <code>any</code>.
%%
%% @see t_tuple/1
%% @see t_tuple_args/1
%% @see t_tuple_max_arity/1
%% @see t_tuple_min_arity/1

t_tuple_arity(?is_tuple(?any)) -> any;
t_tuple_arity(?is_tuple(?tuple_arities([?n_tuple(_, _, N, _)]))) -> N;
t_tuple_arity(?is_tuple(_)) -> any.


%% @spec t_tuple_max_arity(T::type()) -> integer() | any
%%
%% @doc Returns the maximum arity of a tuple type. Cf.
%% <code>t_tuple_arity/1</code>. If no specific maximum arity is known,
%% the result is <code>any</code>.
%%
%% @see t_tuple/1
%% @see t_tuple_arity/1

-ifndef(NO_UNUSED).
t_tuple_max_arity(?is_tuple(?any)) -> any;
t_tuple_max_arity(?is_tuple(?tuple_arities(Ts))) ->
    ?n_tuple(_, _, N, _) = lists:last(Ts),
    N.
-endif.
%% @clear


%% @spec t_tuple_min_arity(T::type()) -> integer() | any
%%
%% @doc Returns the minimum arity of a tuple type. Cf.
%% <code>t_tuple_arity/1</code>. If no specific minimum arity is known,
%% the result is <code>any</code>.
%%
%% @see t_tuple/1
%% @see t_tuple_arity/1

-ifndef(NO_UNUSED).
t_tuple_min_arity(?is_tuple(?any)) -> any;

t_tuple_min_arity(?is_tuple(?tuple_arities([?n_tuple(_, _, N, _) | _]))) ->
    N.
-endif.
%% @clear


%% @spec t_tuple_args(T::type()) -> [type()] | any
%%
%% @doc Returns the list of arguments of a tuple type. If <code>T</code>
%% is <code>{T1, ..., Tn}</code>, the result is <code>[T1, ...,
%% Tn]</code>, otherwise the result is <code>any</code>.
%%
%% @see t_tuple/1
%% @see t_tuple_arity/1

t_tuple_args(?is_tuple(?any)) -> any;
t_tuple_args(?is_tuple(?tuple_arities([?n_tuple(_, _, _, As)]))) -> As;
t_tuple_args(?is_tuple(_)) -> any.


%% @spec t_tuple_subtypes(T::type()) -> [type()] | any
%%
%% @doc Returns the list of disjoint tuple subtypes in <code>T</code>.
%% If <code>T</code> does not contain any tuple subtypes, the result is
%% the empty list. If <code>T</code> contains all of
%% <code>tuple()</code>, the result is <code>any</code>.
%%
%% @see t_tuple/1

t_tuple_subtypes(?any) -> any;
t_tuple_subtypes(?is_tuple(?any)) -> any;
t_tuple_subtypes(?is_tuple(?tuple_arities(Ts))) ->
    lists:append([[?tuple(?tuple_arities([?n_tuple(M, K, N, Es)]))
		   || ?tuple_type(K, _, Es) <- As]
		  || ?tuple_types(M, N, As) <- Ts]);
t_tuple_subtypes(_) -> [].


%% @spec t_tuple_arities(T::type()) -> [integer()] | any
%%
%% @doc Returns the list of arities of tuple subtypes in <code>T</code>.
%% If <code>T</code> does not contain any tuple subtypes, the result is
%% the empty list. If <code>T</code> contains all of
%% <code>tuple()</code>, the result is <code>any</code>.
%%
%% @see t_tuple/1
%% @see t_tuple_arity/1

t_tuple_arities(?any) -> any;
t_tuple_arities(?is_tuple(?any)) -> any;
t_tuple_arities(?is_tuple(?tuple_arities(Ts))) ->
    [N || ?tuple_arity(N, _) <- Ts];
t_tuple_arities(_) -> [].


%% ---------------------------------------------------------------------

%% Note: For non-nil-terminated lists, the element type includes the
%% type of the tail. E.g., if T = `t_cons(t_atom(foo), t_atom(bar))',
%% then both `t_cons_hd(T)' and `t_cons_tl(T)' return the t_atom() type,
%% and so does `t_list_elements(T)'. This discards some information from
%% the head element, but avoids a complete loss of information about the
%% tail element.
%%
%% Representation of lists:
%%
%%    Elements: Type
%%
%%    Constructor: Any
%%                 |
%%                 Cons
%%                 |
%%                 Nil
%%
%%    Termination: Any
%%                 |
%%                 Nil
%%
%% The Type component is always none() for the empty list (nil),
%% but never for any other list type.
%%
%% The following summarizes the subtype ordering for list types:
%%
%%     any() > improper_list(T)
%%     improper_list(T) > nonempty_improper_list(T)
%%     improper_list(T) > list(T)
%%     nonempty_improper_list(T) > nonempty_list(T)
%%     list(T) > nonempty_list(T)
%%     list(T) > nil()
%%     nonempty_list(T) > none()
%%     nil() > none()

-define(c_list(E, C, T), #c{c = list, n = 3, as = [E, C, T]}).
-define(c_nil, ?c_list(?none, ?list_start_nil, ?list_end_nil)).
-define(list(E, C, T), ?u_list(?none, ?c_list(E, C, T))).
-define(is_list(E, C, T), ?u_list(?none, ?c_list(E, C, T))).
-define(list_start_cons, #c{c = cons, n = 0, as = []}).
-define(list_start_is_cons, #c{c = cons}).
-define(list_start_nil, #c{c = nil, n = 0, as = []}).
-define(list_start_is_nil, #c{c = nil}).
-define(list_end_nil, #c{c = nil, n = 0, as = []}).
-define(list_end_is_nil, #c{c = nil}).

-define(proper_list(E, C), ?list(E, C, ?list_end_nil)).
-define(improper_list(E, C), ?list(E, C, ?any)).
-define(nil, ?proper_list(?none, ?list_start_nil)).
-define(cons(E, T), ?list(E, ?list_start_cons, T)).
-define(nonempty_proper_list(E), ?proper_list(E, ?list_start_cons)).
-define(is_proper_list(E), ?is_list(E, _, ?list_end_is_nil)).
-define(is_improper_list(E), ?is_list(E, _, _)).
-define(is_nil, ?is_list(_, ?list_start_is_nil, _)).
-define(is_cons(E, T), ?is_list(E, ?list_start_is_cons, T)).
-define(is_nonempty_proper_list(E), ?is_cons(E, ?list_end_is_nil)).


%% @spec t_is_list(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>list()</code>, otherwise <code>false</code>.
%%
%% @see t_list/0

t_is_list(?is_proper_list(_)) -> true;
t_is_list(_) -> false.


%% @spec t_is_nonempty_list(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>nonempty_list()</code>, otherwise <code>false</code>.
%%
%% @see t_nonempty_list/0

-ifndef(NO_UNUSED).
t_is_nonempty_list(?is_nonempty_proper_list(_)) -> true;
t_is_nonempty_list(_) -> false.
-endif.


%% @spec t_is_nil(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>nil()</code>, otherwise <code>false</code>.
%%
%% @see t_nil/0

t_is_nil(?is_nil) -> true;
t_is_nil(_) -> false.


%% @spec t_is_cons(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>nonempty_improper_list()</code>, otherwise <code>false</code>.
%%
%% @see t_nonempty_improper_list/0

t_is_cons(?is_cons(_, _)) -> true;
t_is_cons(_) -> false.


%% @spec t_is_improper_list(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>improper_list()</code>, otherwise <code>false</code>.
%%
%% @see t_improper_list/0

t_is_improper_list(?is_improper_list(_)) -> true;
t_is_improper_list(_) -> false.


%% @spec t_is_string(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>string()</code>, otherwise <code>false</code>.
%%
%% @see t_string/0

-ifndef(NO_UNUSED).
t_is_string(?is_nil) -> true;
t_is_string(?is_proper_list(T)) -> t_is_char(T);
t_is_string(_) -> false.
-endif.
%% @clear


%% @spec t_list() -> type()
%%
%% @doc Returns the <code>list()</code> type. A (proper) list is either
%% a nil-terminated chain of one or more cons cells
%% <code>[...|...]</code>, nested in the second (right) argument, or nil
%% itself (the empty list, <code>[]</code>).
%%
%% @see t_list/1
%% @see t_is_list/1
%% @see t_list_elements/1
%% @see t_nonempty_improper_list/0
%% @see t_nil/0
%% @see t_nonempty_list/0
%% @see t_improper_list/0
%% @see t_string/0

t_list() -> ?proper_list(?any, ?any).


%% @spec t_list(T::type()) -> type()
%%
%% @doc Returns the <code>list(T)</code> type. This represents all
%% proper lists <code>[T, ..., T]</code>, i.e., whose elements have type
%% T. If <code>T</code> is <code>none()</code>, the result is also
%% <code>none()</code>.
%%
%% @see t_list/0
%% @see t_is_list/1
%% @see t_list_elements/1
%% @see t_cons/2
%% @see t_nil/0
%% @see t_nonempty_list/1
%% @see t_improper_list/1
%% @see t_string/0

t_list(?none) -> ?none;
t_list(T) -> ?proper_list(T, ?any).


%% @spec t_list_elements(type()) -> type()
%%
%% @doc Returns the type of the elements of any list type. This includes
%% all proper and non-proper lists. Note that the element type of the
%% empty list is <code>none()</code>.
%%
%% @see t_list/1
%% @see t_improper_list/1
%% @see t_cons_hd/1
%% @see t_cons_tl/1

t_list_elements(?is_list(T, _, _)) -> T.


%% @spec t_nil() -> type()
%%
%% @doc Returns the <code>nil()</code> type. ("Nil" is the common name
%% for the empty list, also written <code>[]</code>.) Note that there is
%% no distinction between the empty list and the empty string.
%%
%% @see t_list/0
%% @see t_is_nil/1
%% @see t_list_elements/1

t_nil() -> ?nil.


%% @spec t_nonempty_improper_list() -> type()
%%
%% @doc Returns the <code>nonempty_improper_list()</code> type. This
%% represents all nonempty proper and non-proper lists <code>[... |
%% ...]</code>.
%%
%% @see t_cons/2
%% @see t_list/0

-ifndef(NO_UNUSED).
t_nonempty_improper_list() -> ?cons(?any, ?any).
-endif.
%% @clear


%% @spec t_nonempty_improper_list(T::type()) -> type()
%% 
%% @doc Returns the <code>nonempty_improper_list(T)</code> type. This
%% represents all proper and non-proper nonempty lists <code>[T, ..., T
%% | T]</code>. If <code>T</code> is <code>none()</code>, the result is
%% also <code>none()</code>.
%% 
%% @see t_cons/2

-ifndef(NO_UNUSED).
t_nonempty_improper_list(T) -> ?cons(T, ?any).
-endif.
%% @clear


%% @spec t_cons() -> type()
%% @equiv t_cons(t_any())
%% @see t_cons/2

t_cons() ->
    ?cons(?any, ?any).


%% @spec t_cons(T::type()) -> type()
%% @equiv t_cons(T, T)
%% @see t_cons/2

-ifndef(NO_UNUSED).
t_cons(T) ->
    t_cons(T, T).
-endif.
%% @clear


%% @spec t_cons(Head::type(), Tail::type()) -> type()
%%
%% @doc Returns a <code>nonempty_list()</code> or
%% <code>nonempty_improper_list()</code> type, depending on the type of
%% <code>Tail</code>. If <code>Tail</code> is in <code>list(T)</code>,
%% the result is in <code>nonempty_list(U)</code>, where <code>U =
%% t_sup(T, Head)</code>. Otherwise, the result is in
%% <code>nonempty_improper_list(V)</code>, where <code>V = t_sup(Head,
%% Tail)</code>. If <code>Head</code> or <code>Tail</code> is
%% <code>none()</code>, the result is also <code>none()</code>.
%%
%% @see t_cons/0
%% @see t_cons/1
%% @see t_cons_hd/1
%% @see t_cons_tl/1
%% @see t_list/1
%% @see t_nonempty_list/1
%% @see t_nonempty_improper_list/1
%% @see t_sup/2

t_cons(?none, _) -> ?none;
t_cons(_, ?none) -> ?none;
t_cons(T1, ?is_proper_list(T2)) -> ?nonempty_proper_list(t_sup(T1, T2));
t_cons(T1, ?is_improper_list(T2)) -> ?cons(t_sup(T1, T2), ?any);
t_cons(T1, T2) -> ?cons(t_sup(T1, T2), ?any).


%% @spec t_cons_hd(type()) -> type()
%%
%% @doc Returns the type of the head of any nonempty-list type. This
%% includes all proper and non-proper nonempty lists.
%%
%% @see t_nonempty_list/1
%% @see t_cons/2
%% @see t_cons_tl/1
%% @see t_list_elements/1
%% @see t_improper_list/1

t_cons_hd(?is_cons(T, _)) -> T.


%% @spec t_cons_tl(type()) -> type()
%%
%% @doc Returns the type of the tail of any nonempty-list type. This
%% includes all proper and non-proper nonempty lists.
%%
%% @see t_nonempty_list/1
%% @see t_cons/2
%% @see t_cons_hd/1
%% @see t_list_elements/1
%% @see t_improper_list/1

t_cons_tl(?is_nonempty_proper_list(T)) -> ?proper_list(T, ?any);
t_cons_tl(?is_cons(T, ?any)) -> t_sup(?improper_list(T, ?any), T).


%% @spec t_improper_list() -> type()
%%
%% @doc Returns the <code>improper_list()</code> type. This represents all
%% proper and non-proper lists <code>[... | ...]</code>, including the
%% empty list <code>[]</code>.
%%
%% @see t_improper_list/1
%% @see t_is_improper_list/1
%% @see t_list_elements/1
%% @see t_cons/2
%% @see t_nil/0
%% @see t_list/0

t_improper_list() -> ?improper_list(?any, ?any).


%% @spec t_improper_list(T::type()) -> type()
%%
%% @doc Returns the <code>improper_list(T)</code> type. This represents
%% all proper and non-proper lists <code>[T, ..., T | T]</code>,
%% including the empty list <code>[]</code>. If <code>T</code> is
%% <code>none()</code>, the result is also
%% <code>none()</code>.
%%
%% @see t_improper_list/0
%% @see t_is_improper_list/1
%% @see t_list_elements/1
%% @see t_cons/2
%% @see t_nil/0
%% @see t_list/1

-ifndef(NO_UNUSED).
t_improper_list(T) -> ?improper_list(T, ?any).
-endif.
%% @clear


%% @spec t_nonempty_list() -> type()
%%
%% @doc Returns the <code>nonempty_list()</code> type. This represents
%% all nonempty proper lists.
%%
%% @see t_nonempty_list/1
%% @see t_nonempty_string/0
%% @see t_is_nonempty_list/1
%% @see t_list/0

-ifndef(NO_UNUSED).
t_nonempty_list() -> ?nonempty_proper_list(?any).
-endif.


%% @spec t_nonempty_list(T::type()) -> type()
%%
%% @doc Returns the <code>nonempty_list(T)</code> type. This represents
%% all nonempty proper lists <code>[T, ..., T]</code>. If <code>T</code>
%% is <code>none()</code>, the result is also
%% <code>none()</code>.
%%
%% @see t_nonempty_list/0
%% @see t_is_nonempty_list/1
%% @see t_list/0

-ifndef(NO_UNUSED).
t_nonempty_list(?none) -> ?none;
t_nonempty_list(T) -> ?nonempty_proper_list(T).
-endif.
%% @clear


%% @spec t_string() -> type()
%% @equiv t_list(t_char())
%% @see t_list/1
%% @see t_char/0
%% @see t_nonempty_string/0

t_string() -> ?proper_list(t_char(), ?any).


%% @spec t_nonempty_string() -> type()
%% @equiv t_nonempty_list(t_char())
%% @see t_nonempty_list/1
%% @see t_char/0
%% @see t_string/0

-ifndef(NO_UNUSED).
t_nonempty_string() -> ?nonempty_proper_list(t_char()).
-endif.
%% @clear


%% ---------------------------------------------------------------------

%% Representation of functions:
%%
%%    Domain: Any
%%	      |
%%            Arguments: Type list
%%
%%    Range: Type

-define(c_fun(D, R), #c{c = 'fun', n = 2, as = [D, R]}).
-define('fun'(D, R), ?u_fun(?none, ?c_fun(D, R))).
-define(is_fun(D, R), ?u_fun(?none, ?c_fun(D, R))).
-define(fun_domain_args(N, Ts), #c{c = args, n = N, as = Ts}).


%% @spec t_fun() -> type()
%%
%% @doc Returns a function type <code>(...) -> any()</code>. This
%% represents all partial functions on all domains, i.e., with any
%% number of parameters (including zero) of any types.
%%
%% @see t_fun/1

t_fun() -> ?'fun'(?any, ?any).


%% @spec t_fun(T::type()) -> type()
%%
%% @doc Returns a function type <code>(...) -> T</code>. This represents
%% all partial functions with range <code>T</code> and with any number
%% of parameters (including zero) of any types.
%%
%% @see t_fun/0
%% @see t_fun/2

t_fun(T) -> ?'fun'(?any, T).


%% @spec t_fun(Args, Range) -> type()
%%          Args = [type()] | integer()
%%          Range = type()
%%
%% @doc Returns a function type <code>(T1, ..., Tn) -> Range</code>. If
%% <code>Args</code> is <code>[A1, ..., An]</code>, then <code>Ti =
%% Ai</code> for all <code>i</code>. If <code>Args</code> is an integer
%% <code>n</code>, then <code>Ti = any()</code> for all <code>i</code>.
%%
%% <p>Note that the function type constructors do not preserve the
%% <code>none()</code> type; a function type can represent a
%% mapping from (partially) undefined arguments to an undefined
%% result.</p>
%%
%% @see t_fun/0
%% @see t_fun/1
%% @see t_is_fun/1
%% @see t_fun_args/1
%% @see t_fun_range/1

t_fun(As, T) when is_list(As) ->
    ?'fun'(?fun_domain_args(length(As), As), T);
t_fun(N, T) when is_integer(N), N >= 0 ->
    ?'fun'(?fun_domain_args(N, mk_any_list(N)), T).


%% @spec t_is_fun(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>fun()</code>, or <code>false</code> otherwise.
%%
%% @see t_fun/0

t_is_fun(?is_fun(_, _)) -> true;
t_is_fun(_) -> false.


%% @spec t_fun_arity(T::type()) -> integer() | any
%%
%% @doc Returns the arity of a function type. If <code>T</code> is
%% <code>(T1, ..., Tn) -> ...</code>, the result is the integer
%% <code>n</code>, otherwise the result is <code>any</code>.
%%
%% @see t_fun/2
%% @see t_fun_args/1

-ifndef(NO_UNUSED).
t_fun_arity(?is_fun(?any, _)) -> any;
t_fun_arity(?is_fun(?fun_domain_args(N, _), _)) -> N.
-endif.
%% @clear


%% @spec t_fun_args(T::type()) -> [type()] | any
%%
%% @doc Returns the list of arguments of a function type. If
%% <code>T</code> is <code>(T1, ..., Tn) -> ...</code>, the result is
%% <code>[T1, ..., Tn]</code>, otherwise the result is <code>any</code>.
%%
%% @see t_fun/2
%% @see t_fun_arity/1

t_fun_args(?is_fun(?any, _)) -> any;
t_fun_args(?is_fun(?fun_domain_args(_, As), _)) -> As.


%% @spec t_fun_range(T::type()) -> type()
%%
%% @doc Returns the range of a function type. If <code>T</code> is
%% <code>(...) -> R</code>, the result is <code>R</code>.
%%
%% @see t_fun/2
%% @see t_fun_arity/1

t_fun_range(?'fun'(_, R)) -> R.


%% ---------------------------------------------------------------------

-define(c_binary, #c{c = binary, n = 0, as = []}).
-define(binary, ?u_binary(?none, ?c_binary)).
-define(is_binary, ?u_binary(?none, ?c_binary)).

%% @spec t_binary() -> type()
%% @doc Returns the <code>binary()</code> type.
%% @see t_is_binary/1

t_binary() -> ?binary.

%% @spec t_is_binary(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>binary()</code>, or <code>false</code> otherwise.
%%
%% @see t_binary/0

t_is_binary(?is_binary) -> true;
t_is_binary(_) -> false.


%% ---------------------------------------------------------------------

%% Representation of the "identifier" type:
%%
%%    Class: Any
%%           |
%%           Pid
%%           |
%%           Port
%%           |
%%           Reference

-define(c_identifier(C), #c{c = identifier, n = 1, as = [C]}).
-define(identifier(C), ?u_identifier(?none, ?c_identifier(C))).
-define(is_identifier(C), ?u_identifier(?none, ?c_identifier(C))).
-define(identifier_class_pid, #c{c = pid, n = 0, as = []}).
-define(identifier_class_is_pid, #c{c = pid}).
-define(identifier_class_port, #c{c = port, n = 0, as = []}).
-define(identifier_class_is_port, #c{c = port}).
-define(identifier_class_ref, #c{c = ref, n = 0, as = []}).
-define(identifier_class_is_ref, #c{c = ref}).


%% @spec t_identifier() -> type()
%%
%% @doc Returns the <code>identifier()</code> type. This is the union of
%% the <code>pid()</code> and <code>port()</code> types.
%%
%% @see t_is_identifier/1
%% @see t_pid/0
%% @see t_port/0

t_identifier() -> ?identifier(?any).


%% @spec t_is_identifier(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>identifier()</code>, or <code>false</code> otherwise.
%%
%% @see t_identifier/0

-ifndef(NO_UNUSED).
t_is_identifier(?is_identifier(_)) -> true;
t_is_identifier(_) -> false.
-endif.
%% @clear


%% @spec t_pid() -> type()
%% @doc Returns the <code>pid()</code> type.
%% @see t_is_pid/1

t_pid() -> ?identifier(?identifier_class_pid).

%% @spec t_is_pid(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>pid()</code>, or <code>false</code> otherwise.
%%
%% @see t_pid/0

t_is_pid(?is_identifier(?identifier_class_is_pid)) -> true;
t_is_pid(_) -> false.


%% @spec t_port() -> type()
%% @doc Returns the <code>port()</code> type.
%% @see t_is_port/1

t_port() -> ?identifier(?identifier_class_port).


%% @spec t_is_port(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>port()</code>, or <code>false</code> otherwise.
%%
%% @see t_port/0

t_is_port(?is_identifier(?identifier_class_is_port)) -> true;
t_is_port(_) -> false.


%% @spec t_ref() -> type()
%% @doc Returns the <code>ref()</code> type.
%% @see t_is_ref/1

t_ref() -> ?identifier(?identifier_class_ref).


%% @spec t_is_ref(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>ref()</code>, or <code>false</code> otherwise.
%%
%% @see t_ref/0

t_is_ref(?is_identifier(?identifier_class_is_ref)) -> true;
t_is_ref(_) -> false.


%% ---------------------------------------------------------------------

%% @spec t_is_data(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is a data
%% constructor type, or <code>false</code> otherwise. The data
%% constructor types are tuples, lists (cons cells and the empty list),
%% numbers and atoms.
%%
%% @see t_data_arity/1
%% @see t_data_args/1

-ifndef(NO_UNUSED).
t_is_data(?is_number(_, _)) -> true;
t_is_data(?is_atom(_, _)) -> true;
t_is_data(?is_tuple(_)) -> true;
t_is_data(?is_list(_, _, _)) -> true;
t_is_data(_) -> false.
-endif.
%% @clear


%% @spec t_data_arity(T::type()) -> integer() | any
%%
%% @doc Returns the arity of a data constructor type <code>T</code>, or
%% <code>any</code> if unknown.
%%
%% @see t_is_data/1
%% @see t_data_args/1
%% @see t_tuple_arity/1

-ifndef(NO_UNUSED).
t_data_arity(?is_tuple(?tuple_arities([?n_tuple(_, _, N, _)]))) -> N;
t_data_arity(?is_tuple(_)) -> any;
t_data_arity(?is_nil) -> 0;
t_data_arity(?is_cons(_, _)) -> 2;
t_data_arity(?is_list(_, _, _)) -> any;
t_data_arity(_) -> 0.
-endif.
%% @clear


%% @spec t_data_args(T::type()) -> [type()] | any
%%
%% @doc Returns the list of arguments of a data constructor type
%% <code>T</code>, or <code>any</code> if unknown.
%%
%% @see t_is_data/1
%% @see t_data_arity/1
%% @see t_tuple_argsp/1

-ifndef(NO_UNUSED).
t_data_args(?is_tuple(?tuple_arities([?n_tuple(_, _, _, As)]))) -> As;
t_data_args(?is_tuple(_)) -> any;
t_data_args(?is_nil) -> [];
t_data_args(?is_nonempty_proper_list(T)) -> [T, t_list(T)];
t_data_args(?is_cons(T, _)) -> [T, ?any];
t_data_args(?is_list(_, _, _)) -> any;
t_data_args(_) -> [].
-endif.
%% @clear


%% ---------------------------------------------------------------------

%% @spec t_is_equal(T1::type(), T2::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T1</code> is a subset of
%% <code>T2</code> and also <code>T2</code> is a subset of
%% <code>T1</code>, otherwise <code>false</code>.

t_is_equal(T1, T2)->
    t_is_subtype(T1, T2) andalso t_is_subtype(T2, T1).


%% @spec t_is_subtype(T1::type(), T2::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T1</code> is a subtype of
%% <code>T2</code>, otherwise <code>false</code>.

t_is_subtype(?dunion(As1), ?dunion(As2)) ->
    t_is_subtype_dunion(As1, As2);
t_is_subtype(?value_set(S1=#set{}), ?value_set(S2=#set{})) ->
    is_subset(S1, S2);
t_is_subtype(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    t_is_subtype_lists(As1, As2);
t_is_subtype(?tuple_mask(M1, As1), ?tuple_mask(M2, As2)) 
  when M1 /= M2  ->
    M = M1 band M2,
    t_is_subtype_dunion(reduce_tuple_keys_unique(M, M1, As1), 
			reduce_tuple_keys_unique(M, M2, As2));
t_is_subtype(?none, _) ->
    true;
t_is_subtype(_, ?any) ->
    true;
t_is_subtype(_, _) ->
    false.

t_is_subtype_lists([T1 | Ts1], [T2 | Ts2]) ->
    case t_is_subtype(T1, T2) of
	true -> t_is_subtype_lists(Ts1, Ts2);
	false -> false
    end;
t_is_subtype_lists([], []) ->
    true.

t_is_subtype_dunion([T1 = #c{c = C1, n = N1} | Ts1] = As1,
		    [T2 = #c{c = C2, n = N2} | Ts2]) ->
    if C1 < C2 ->
	    false;
       C1 > C2 ->
	    t_is_subtype_dunion(As1, Ts2);
       N1 < N2 ->
	    false;
       N1 > N2 ->
	    t_is_subtype_dunion(As1, Ts2);
       true ->
	    case t_is_subtype(T1, T2) of
		true ->
		    t_is_subtype_dunion(Ts1, Ts2);
		false ->
		    false
	    end
    end;
t_is_subtype_dunion([], _Ts) ->
    true;
t_is_subtype_dunion(_Ts, []) ->
    false.


%% @spec t_sup([type()]) -> type()
%% @doc Returns the supremum of a list of types.
%% @see t_sup/2

t_sup([T | Ts]) -> t_sup(T, t_sup(Ts));
t_sup([]) -> ?none.


%% @spec t_sup(type(), type()) -> type()
%%
%% @doc Returns the supremum (least upper bound, or "join") of two
%% types. This is analogous to set union. Occurrences of type variables
%% are regarded as representing the <code>any()</code> type.
%%
%% @see t_sup/1
%% @see t_inf/2

t_sup(?dunion(As1), ?dunion(As2)) ->
    case t_sup_dunion(As1, As2) of
	[] -> ?any;
	As -> ?dunion(As)
    end;
t_sup(?value_set(S1=#set{}), ?value_set(S2=#set{})) ->
    case set_union(S1, S2) of
	?any -> ?any;
	SU -> ?value_set(SU)
    end;
t_sup(?tuple_mask(M, As1), ?tuple_mask(M, As2)) ->
    t_sup_tuples(M, As1, As2);
t_sup(?tuple_mask(M1, As1), ?tuple_mask(M2, As2)) ->
    t_sup_tuples(M1, M2, As1, As2);
t_sup(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    #c{c = C, n = N, as = t_sup_lists(As1, As2)};
t_sup(?none, T) ->
    T;
t_sup(T, ?none) ->
    T;
t_sup(_, _) ->
    ?any.

t_sup_lists([T1 | Ts1], [T2 | Ts2]) ->
    [t_sup(T1, T2) | t_sup_lists(Ts1, Ts2)];
t_sup_lists([], []) ->
    [].

t_sup_dunion([T1 = #c{c = C1, n = N1} | Ts1] = As1,
	     [T2 = #c{c = C2, n = N2} | Ts2] = As2) ->
    if C1 < C2 ->
	    [T1 | t_sup_dunion(Ts1, As2)];
       C1 > C2 ->
	    [T2 | t_sup_dunion(As1, Ts2)];
       N1 < N2 ->
	    [T1 | t_sup_dunion(Ts1, As2)];
       N1 > N2 ->
	    [T2 | t_sup_dunion(As1, Ts2)];
       true ->
	    %% Note that sup of T1 and T2 may never be any().
	    [t_sup(T1, T2) | t_sup_dunion(Ts1, Ts2)]
    end;
t_sup_dunion([], Ts) ->
    Ts;
t_sup_dunion(Ts, []) ->
    Ts.

t_sup_tuples(M, As1, As2) ->
    As = t_sup_dunion(As1, As2),
    if length(As) > ?DUNION_LIMIT ->
	    NewM = reduce_key_mask(M),
	    t_sup_tuples(NewM, 
			 reduce_tuple_keys_unique(NewM, M, As1),
			 reduce_tuple_keys_unique(NewM, M, As2));
       true ->
	    ?tuple_mask(M, As)
    end.

%% This is only called when M1 =/= M2.

t_sup_tuples(M1, M2, As1, As2) ->
    M = M1 band M2,
    As = t_sup_dunion(reduce_tuple_keys_unique(M, M1, As1),
		      reduce_tuple_keys_unique(M, M2, As2)),
    ?tuple_mask(M, As).

reduce_key_mask(M) ->
    reduce_key_mask_1(M, 0).

reduce_key_mask_1(M, S) when (M band 1) =:= 1 ->
    (M bxor 1) bsl S;
reduce_key_mask_1(M, S) when M > 0->
    reduce_key_mask_1(M bsr 1, S + 1);
reduce_key_mask_1(0, _S) ->
    0.
    
%% Note that removing one or more fields from the keys can change the
%% relative order and also create duplicates, so we must sort the lists
%% afterwards, and merge adjacent elements with the same key.

reduce_tuple_keys_unique(M, M, As) ->
    As;
reduce_tuple_keys_unique(M, B, As) ->
    unique(reduce_tuple_keys(M, B, As)).

reduce_tuple_keys(M, M, As) ->
    As;
reduce_tuple_keys(M, B, As) ->
    lists:keysort(#c.c, reduce_tuple_keys_nosort(M, B, As)).

reduce_tuple_keys_nosort(M, B, [?tuple_type(K, N, Ts) | As]) ->
    [?tuple_type(reduce_key(M, B, K), N, Ts)
     | reduce_tuple_keys_nosort(M, B, As)];
reduce_tuple_keys_nosort(_, _, []) ->
    [].

reduce_key(M, B, K) ->
    As = lists:reverse(tuple_to_list(K)),
    list_to_tuple(reduce_key_1(M, B, As, [])).

reduce_key_1(0, _B, _Xs, As) -> As;
reduce_key_1(M, B, Xs = [X | Xs1], As) ->
    M1 = M bsr 1,
    B1 = B bsr 1,
    if (B band 1) == 0 ->
	    reduce_key_1(M1, B1, Xs, As);
       true ->
	    if (M band 1) == 0 ->
		    reduce_key_1(M1, B1, Xs1, As);
	       true ->
		    reduce_key_1(M1, B1, Xs1, [X | As])
	    end
    end.

%% Note that sup of T1 and T2 may never become any() here.

unique([T1 = #c{c = C, n = N}, T2 = #c{c = C, n = N} | Ts]) ->
    unique([t_sup(T1, T2) | Ts]);
unique([T | Ts]) ->
    [T | unique(Ts)];
unique([]) ->
    [].


-ifndef(NO_UNUSED).
%% @spec t_inf([type()]) -> type()
%% @doc Returns the infimum of a list of types.
%% @see t_inf/2

t_inf([T | Ts]) -> t_inf(T, t_inf(Ts));
t_inf([]) -> ?any.
-endif.
%% @clear

%% @spec t_inf(type(), type()) -> type()
%%
%% @doc Returns the infimum (greatest lower bound, or "meet") of two
%% types. This is analogous to set intersection. Occurrences of type
%% variables are regarded as representing the <code>any()</code> type.
%%
%% @see t_inf/1
%% @see t_sup/2

t_inf(?c_atom(V1, _C1), ?c_atom(V2, _C2)) ->
    %% The booleans are a finite, small subset, which makes it simple to
    %% just drop the old class and recreate the new class from the
    %% resulting value set; the latter does not depend on the former.
    case t_inf(V1, V2) of
	?none -> ?none;
	?any -> ?c_atom(?any, ?any);
	S = ?value_set(#set{s = S1}) ->
	    C = case S1 of
		    [true] -> ?atom_class_bool;
		    [false] -> ?atom_class_bool;
		    [false, true] -> ?atom_class_bool;
		    _ -> ?any
		 end,
	    ?c_atom(S, C)
    end;
t_inf(?c_number(?any, C1), ?c_number(?any, C2)) -> 
    %% The simplest case.
    case t_inf(C1, C2) of
	?none -> ?none;
	C -> ?c_number(?any, C)
    end;
t_inf(?c_number(?value_set(S1=#set{}), C1),
      ?c_number(?value_set(S2=#set{}), C2)) ->
    %% In this case, we know that if the set intersection is nonempty,
    %% the infimum of the class fields is correct (and not none).
    case set_intersection(S1, S2) of
	#set{n = 0} -> ?none;
	S -> ?c_number(?value_set(S), t_inf(C1, C2))
    end;
t_inf(?c_number(V1, C1), ?c_number(V2, C2)) ->
    %% Here, one but not the other is a value set (so t_inf(V1, V2) will
    %% return that set). However, the values must be filtered using the
    %% resulting class (which could be none).
    C = t_inf(C1, C2),
    ?value_set(S) = t_inf(V1, V2),
    case filter_numbers(S, C) of
	#set{n = 0} -> ?none;
	S1 -> ?c_number(?value_set(S1), C)
    end;
t_inf(?c_list(E1, C1, T1), ?c_list(E2, C2, T2)) ->
    %% The element type can be none() only for nil, not for any
    %% other list type.
    E = t_inf(E1, E2),
    C = t_inf(C1, C2),
    T = t_inf(T1, T2),
    if C == ?list_start_nil -> ?c_list(E, C, T);
       E == ?none -> ?none;
       C == ?none -> ?none;
       T == ?none -> ?none;
       true -> ?c_list(E, C, T)
    end;
t_inf(?c_fun(?fun_domain_args(N, Ts1), R1),
      ?c_fun(?fun_domain_args(N, Ts2), R2)) ->
    %% The function domain and range can be none().
    ?c_fun(?fun_domain_args(N, t_inf_lists(Ts1, Ts2)), t_inf(R1, R2));
t_inf(?c_fun(?fun_domain_args(N1, _Ts1), _R1),
      ?c_fun(?fun_domain_args(N2, _Ts2), _R2)) when N1 =/= N2 ->
    ?none;
t_inf(?c_fun(D1, R1), ?c_fun(D2, R2)) ->
    %% The function domain and range can be any() or none().
    case D1 of
	?any -> ?c_fun(D2, t_inf(R1, R2));
	?none -> ?c_fun(?none, t_inf(R1, R2));
	_ ->
	    case D2 of
		?any -> ?c_fun(D1, t_inf(R1, R2));
		?none -> ?c_fun(?none, t_inf(R1, R2))
	    end
    end;
t_inf(?union(Ts1), ?union(Ts2)) ->
    %% Unions map to none() only if all elements are none().
    case t_inf_lists_all(Ts1, Ts2) of
	none -> ?none;
	Ts -> ?union(Ts)
    end;
t_inf(?dunion(As1), ?dunion(As2)) ->
    case t_inf_dunion(As1, As2) of
	[] -> ?none;
	As -> ?dunion(As)
    end;
t_inf(?value_set(S1=#set{}), ?value_set(S2=#set{})) ->
    case set_intersection(S1, S2) of
	#set{n = 0} -> ?none;
	S -> ?value_set(S)
    end;
t_inf(?tuple_types(M1, N, As1), ?tuple_types(M2, N, As2))
  when M1 =/= M2 ->
    t_inf_tuples(M1, M2, As1, As2, N);
t_inf(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    %% Default case maps to none() if any element is none().
    case t_inf_lists_any(As1, As2) of
	none -> ?none;
	As -> #c{c = C, n = N, as = As}
    end;
t_inf(?any, T) ->
    T;
t_inf(#var{}, T) ->
    T;
t_inf(T, ?any) ->
    T;
t_inf(T, #var{}) ->
    T;
t_inf(_, _) ->
    ?none.

t_inf_lists([T1 | Ts1], [T2 | Ts2]) ->
    [t_inf(T1, T2) | t_inf_lists(Ts1, Ts2)];
t_inf_lists([], []) ->
    [].

t_inf_lists_any(Ts1, Ts2) ->
    t_inf_lists_any(Ts1, Ts2, []).

t_inf_lists_any([T1 | Ts1], [T2 | Ts2], As) ->
    case t_inf(T1, T2) of
	?none -> none;
	A -> t_inf_lists_any(Ts1, Ts2, [A | As])
    end;
t_inf_lists_any([], [], As) ->
    lists:reverse(As).

t_inf_lists_all(Ts1, Ts2) ->
    t_inf_lists_all(Ts1, Ts2, [], true).

t_inf_lists_all([T1 | Ts1], [T2 | Ts2], As, B) ->
    case t_inf(T1, T2) of
	?none = A ->
	    t_inf_lists_all(Ts1, Ts2, [A | As], B);
	A ->
	    t_inf_lists_all(Ts1, Ts2, [A | As], false)
    end;
t_inf_lists_all([], [], As, false) ->
    lists:reverse(As);
t_inf_lists_all([], [], _, true) ->
    none.

t_inf_dunion([T1 = #c{c = C1, n = N1} | Ts1] = As1,
	     [T2 = #c{c = C2, n = N2} | Ts2] = As2) ->
    if C1 < C2 ->
	    t_inf_dunion(Ts1, As2);
       C1 > C2 ->
	    t_inf_dunion(As1, Ts2);
       N1 < N2 ->
	    t_inf_dunion(Ts1, As2);
       N1 > N2 ->
	    t_inf_dunion(As1, Ts2);
       true ->
	    case t_inf(T1, T2) of
		?none ->
		    t_inf_dunion(Ts1, Ts2);
		T ->
		    [T | t_inf_dunion(Ts1, Ts2)]
	    end
    end;
t_inf_dunion([], _Ts) ->
    [];
t_inf_dunion(_Ts, []) ->
    [].

%% This is only called when M1 =/= M2.
%%
%% Note that we don't recalculate the signature bit masks from scratch,
%% even though some field could suddenly become a singleton. This is
%% intentional - such fields should not be regarded as keys.

t_inf_tuples(M1, M2, As1, As2, N) ->
    Ts = case M1 band M2 of
  	     0 ->
 		 %% No common key fields - we have to generate all
 		 %% combinations of lhs and rhs tuples, which could be
 		 %% expensive (it's of course quadratic). Doing a 'kill'
 		 %% instead of a 'reduce' on the keys saves some work.
  		 t_inf_combine(kill_tuple_keys(As1),
  			       kill_tuple_keys(As2));
  	     B ->
  		 %% They have some constant field in common. This makes
  		 %% it possible to reduce the complexity somewhat. Note
  		 %% that we cannot do the 'unique' operation just after
  		 %% the 'rekey' as we do in the t_sup_tuples function -
  		 %% that would lose too much info.
  		 t_inf_dunion_dup(reduce_tuple_keys(B, M1, As1),
  				  reduce_tuple_keys(B, M2, As2))
  	 end,
    %% Compute the real new signatures
    if Ts == [] ->
 	    ?none;
       true ->
 	    M = M1 bor M2,
 	    ?tuple_types(M, N, unique(update_tuple_keys(Ts, M)))
    end.

kill_tuple_keys([?tuple_type(_, N, Ts) | As]) ->
    [?tuple_type({}, N, Ts) | kill_tuple_keys(As)];
kill_tuple_keys([]) ->
    [].

%% Generate all combinations. All elements must have the same key.

t_inf_combine(T1, Ts2) ->
    t_inf_combine(T1, Ts2, []).

t_inf_combine([T1 | Ts1], Ts2, As) ->
    t_inf_combine_1(T1, Ts2, Ts1, Ts2, As);
t_inf_combine([], _, As) ->
    As.    % no need to reverse

t_inf_combine_1(T1, [T2 | Ts2], Ts1, Ts, As) ->
    case t_inf(T1, T2) of
	?none -> 
	    t_inf_combine_1(T1, Ts2, Ts1, Ts, As);
	T ->
	    t_inf_combine_1(T1, Ts2, Ts1, Ts, [T | As])
    end;
t_inf_combine_1(_, [], Ts1, Ts2, As) ->
    t_inf_combine(Ts1, Ts2, As).

%% Calculating a new key when the bit mask is known. The result is
%% sorted.

update_tuple_keys(As, M) ->
    lists:keysort(#c.c, update_tuple_keys_nosort(As, M)).

update_tuple_keys_nosort([?tuple_type(_, N, Ts) | As], M) ->
    [?tuple_type(update_key(Ts, M), N, Ts)
     | update_tuple_keys_nosort(As, M)];
update_tuple_keys_nosort([], _) ->
    [].

update_key(As, M) ->
    list_to_tuple(update_key(lists:reverse(As), M, [])).

update_key([A | As], M, Ts) ->
    M1 = M bsr 1,
    if (M band 1) == 0 ->
	    update_key(As, M1, Ts);
       true ->
	    case A of
		?is_atom(?singleton(T), _) ->
		    update_key(As, M1, [T | Ts]);
		?is_integer(?singleton(T), _) ->
		    update_key(As, M1, [T | Ts])
	    end
    end;
update_key([], _, Ts) ->
    Ts.

%% When we can have duplicate keys in the lists, we must generate all
%% (n*m) combinations of possible infs of same-key tuples.

t_inf_dunion_dup([T1 = #c{c = C1, n = N1} | Ts1] = As1,
		 [T2 = #c{c = C2, n = N2} | Ts2] = As2) ->
    if C1 < C2 ->
	    t_inf_dunion_dup(Ts1, As2);
       C1 > C2 ->
	    t_inf_dunion_dup(As1, Ts2);
       N1 < N2 ->
	    t_inf_dunion_dup(Ts1, As2);
       N1 > N2 ->
	    t_inf_dunion_dup(As1, Ts2);
       true ->
	    {Ss1, Rs1} = take_while(Ts1, C1, N1),
	    {Ss2, Rs2} = take_while(Ts2, C1, N1),
	    t_inf_combine([T1 | Ss1], [T2 | Ss2])
		++ t_inf_dunion_dup(Rs1, Rs2)
    end;
t_inf_dunion_dup([], _Ts) ->
    [];
t_inf_dunion_dup(_Ts, []) ->
    [].

take_while(Ts, C, N) ->
    take_while(Ts, C, N, []).

take_while([T = #c{c = C, n = N} | Ts], C, N, As) ->
    take_while(Ts, C, N, [T | As]);
take_while(Ts, _, _, As) ->
    {As, Ts}.


%% @spec t_subtract(T1::type(), T2::type()) -> type()
%%
%% @doc Returns the difference <code>T1 \ T2</code> between two
%% types. (The result is an upper approximation of the difference, since
%% in general we cannot represent difference types.) This is analogous
%% to set subtraction. Occurrences of type variables are regarded as
%% representing the <code>any()</code> type.
%%
%% @see t_inf/1
%% @see t_sup/2

%% Note: The standard meaning of the default constructor is a
%% conjunction, i.e., a cartesian product of the sets represented by the
%% subterms. E.g., the type {a|b,c|d} means all tuples [{a,c}, {a,d},
%% {b,c}, {b,d}]. Thus, subtraction is not so straighforward as when we
%% have a disjunction - it can not be done individually for each
%% subterm. E.g., {a|b,c|d} \ {a,d} = [{a,c},{b,c},{b,d}], and not
%% simply {(a|b)\a,(c|d)\d}, which would give the result [{b,c}].
%%
%% The most exact way of computing subtraction would be to completely
%% enumerate all terms represented by the operands, then do set
%% subtraction, and compute the final type from the resulting set. But
%% this quickly becomes intractable.
%%
%% A faster approximation of subtraction of products can be computed
%% as follows:
%%   1. Do pairwise subtraction on the subterms.
%%   2. If all the resulting types are none(), the result is none().
%%   3. If all but one of the resulting types are none(), the result
%%      is the original lhs type with only the non-none() subterm
%%      replaced with the result of the subtraction. E.g., {a|b,c|d,e|f}
%%      \ {a|b,d,e|f} = {a|b,c,e|f}, since every combination in the lhs
%%      that contains a 'd' must also be in the rhs, which means that
%%      'd' can be eliminated.
%%   4. Otherwise, the result is the lhs without changes.
%% This will handle many important common cases such as e.g.
%% {bool(),integer()} \ {true,integer()} -> {false,integer()}.

t_subtract(?c_atom(V1, _C1), ?c_atom(V2, _C2)) ->
    %% See t_inf/2 for more information.
    case t_subtract(V1, V2) of
	?none -> ?none;
	?any -> ?c_atom(?any, ?any);
	S = ?value_set(#set{s = S1}) ->
	    C = case S1 of
		    [true] -> ?atom_class_bool;
		    [false] -> ?atom_class_bool;
		    [false, true] -> ?atom_class_bool;
		    _ -> ?any
		 end,
	    ?c_atom(S, C)
    end;
t_subtract(?c_number(?any, C1), ?c_number(?any, C2)) -> 
    %% The simplest case.
    case t_subtract(C1, C2) of
	?none -> ?none;
	C -> ?c_number(?any, C)
    end;
t_subtract(?c_number(?any, C1), ?c_number(?value_set(_S), _C2)) -> 
    %% We assume that e.g. "all bytes" minus "this set of numbers"
    %% cannot yield an empty set, i.e., that the size of value sets is
    %% always (significantly) smaller than 256. Thus, if the LHS has
    %% any() as Value, the result also has any() as Value, and the Class
    %% of the LHS is preserved.
    ?c_number(?any, C1);
t_subtract(?c_number(?value_set(S1=#set{}), _C1),
	   ?c_number(?value_set(S2=#set{}), _C2)) ->
    %% In this case, if the set difference is nonempty, we must compute
    %% the new class for the result.
    S = set_subtract(S1, S2),
    case classify_numbers(S) of
	?none -> ?none;
	C -> ?c_number(?value_set(S), C)
    end;
t_subtract(?c_number(?value_set(S0=#set{}), C1), ?c_number(?any, C2)) ->
    case t_subtract(C1, C2) of
	?none -> ?none;
	C ->
	    %% Here, we need to subtract those elements of the set that
	    %% are of the RHS class. (The result can be empty.)
	    S1 = filter_numbers(S0, C2),
	    S = set_subtract(S0, S1),
	    case classify_numbers(S) of
		?none -> ?none;
		C -> ?c_number(?value_set(S), C)
	    end
    end;
t_subtract(T1, ?c_nil) ->
    %% Special case, since the element type of ?nil is ?none although
    %% nil can be subtracted from all proper lists.
    case T1 of
	?c_nil -> ?none;
	?c_list(E1, _, ?list_end_is_nil) -> 
	    ?c_list(E1, ?list_start_cons, ?list_end_nil);
	_ -> T1
    end;
t_subtract(?c_list(E1, C1, T1) = L1, ?c_list(E2, C2, T2)) ->
    case t_is_subtype(E1, E2) of
	true ->
	    case t_is_subtype(T1, T2) of
		true ->
		    %% Here we know that for the head constructor,
		    %% ?any\?list_start_cons = ?list_start_nil. Note
		    %% that C2 = ?list_start_nil is caught above.
		    case C2 of
			?any ->
			    ?none;
			?list_start_is_cons ->
			    case C1 of
				?any ->
				    ?c_nil;
				?list_start_is_cons ->
				    ?none;
			        _ ->
				    L1
			    end
		    end;
		false ->
		    %% This only happens if T1 is ?any and T2 is
		    %% ?list_end_nil, thus there is no change to L1.
		    L1
	    end;
	false ->
	    %% Note that e.g. list(foo|bar)\list(foo) =/= list(bar),
	    %% since list(foo) does not include [foo,bar,foo,bar] etc.
	    %% Hence, we cannot do anything in this case.
	    L1
    end;
t_subtract(?c_fun(?fun_domain_args(N, Ts1), R1),
	   ?c_fun(?fun_domain_args(N, Ts2), R2)) ->
    %% Either or both of function domain and range can be none().
    As1 = [R1 | Ts1],
    As2 = [R2 | Ts2],
    case t_subtract_lists(As1, As2) of
	none -> ?none;
	[R | Ts] -> ?c_fun(?fun_domain_args(N, Ts), R)
    end;
t_subtract(?union(Ts1), ?union(Ts2)) ->
    %% Unions map to none() only if all elements are none().
    case t_subtract_lists_all(Ts1, Ts2) of
	{none, _} -> ?none;
	{Ts, _} -> ?union(Ts)
    end;
t_subtract(?dunion(As1), ?dunion(As2)) ->
    case t_subtract_dunion(As1, As2) of
	[] -> ?none;
	As -> ?dunion(As)
    end;
t_subtract(?value_set(S1=#set{}), ?value_set(S2=#set{})) ->
    case set_subtract(S1, S2) of
	#set{n = 0} -> ?none;
	S -> ?value_set(S)
    end;
t_subtract(?tuple_types(M1, N, As1), ?tuple_types(M2, N, As2))
  when M1 =/= M2 ->
    t_subtract_tuples(M1, M2, As1, As2, N);
t_subtract(#c{c = C, n = 0, as = []}, #c{c = C, n = 0, as = []}) ->
    %% Special case for speed
    ?none;    % X \ X = none()
t_subtract(#c{c = C, n = 1, as = [T1]}, #c{c = C, n = 1, as = [T2]}) ->
    %% Special case for speed
    case t_subtract(T1, T2) of
	?none -> ?none;
	T -> #c{c = C, n = 1, as = [T]}
    end;
t_subtract(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    case t_subtract_lists(As1, As2) of
	none -> ?none;
	As -> #c{c = C, n = N, as = As}
    end;
t_subtract(_, ?any) ->
    ?none;
t_subtract(_, #var{}) ->
    ?none;
t_subtract(?any, _) ->
    ?any;
t_subtract(#var{}, _) ->
    ?any;
t_subtract(T, _) -> T.

%% Se discussion on subtraction of products above.

t_subtract_lists(As1, As2) ->
    case t_subtract_lists_all(As1, As2) of
	{none, 0} ->
	    none;
	{As, 1} ->
	    update_list(As1, As);
	{_, _} ->
	    As1  % keep As1
    end.

update_list([A | As], [?none | As1]) ->
    [A | update_list(As, As1)];
update_list([_ | As], [A1 | As1]) ->
    [A1 | update_list(As, As1)];
update_list([], []) ->
    [].

%% This also counts the number of non-none() elements in the result.

t_subtract_lists_all(Ts1, Ts2) ->
    t_subtract_lists_all(Ts1, Ts2, [], 0).

t_subtract_lists_all([T1 | Ts1], [T2 | Ts2], As, N) ->
    case t_subtract(T1, T2) of
	?none = A ->
	    t_subtract_lists_all(Ts1, Ts2, [A | As], N);
	A ->
	    t_subtract_lists_all(Ts1, Ts2, [A | As], N + 1)
    end;
t_subtract_lists_all([], [], _, 0) ->
    {none, 0};
t_subtract_lists_all([], [], As, N) ->
    {lists:reverse(As), N}.

t_subtract_dunion([T1 = #c{c = C1, n = N1} | Ts1] = As1,
		  [T2 = #c{c = C2, n = N2} | Ts2] = As2) ->
    if C1 < C2 ->
	    [T1 | t_subtract_dunion(Ts1, As2)];
       C1 > C2 ->
	    t_subtract_dunion(As1, Ts2);
       N1 < N2 ->
	    [T1 | t_subtract_dunion(Ts1, As2)];
       N1 > N2 ->
	    t_subtract_dunion(As1, Ts2);
       true ->
	    case t_subtract(T1, T2) of
		?none ->
		    t_subtract_dunion(Ts1, Ts2);
		T ->
		    [T | t_subtract_dunion(Ts1, Ts2)]
	    end
    end;
t_subtract_dunion([], _Ts) ->
    [];
t_subtract_dunion(Ts, []) ->
    Ts.

%% This is only called when M1 =/= M2.
%%
%% Note that we don't recalculate the signature bit masks from scratch,
%% even though some field could suddenly become a singleton. This is
%% intentional - such fields should not be regarded as keys.

t_subtract_tuples(M1, M2, As1, As2, N) ->
     Ts = case M1 band M2 of
  	     0 ->
 		 %% No common key fields - we have to generate all
 		 %% combinations of lhs and rhs tuples, which could be
 		 %% expensive (it's of course quadratic). Doing a 'kill'
 		 %% instead of a 'reduce' on the keys saves some work.
  		 t_subtract_combine(kill_tuple_keys(As1),
 				    kill_tuple_keys(As2));
  	     B ->
  		 %% They have some constant field in common. This makes
  		 %% it possible to reduce the complexity somewhat. Note
  		 %% that we cannot do the 'unique' operation just after
  		 %% the 'rekey' as we do in the t_sup_tuples function -
  		 %% that would lose too much info.
  		 t_subtract_dunion_dup(reduce_tuple_keys(B, M1, As1),
 				       reduce_tuple_keys(B, M2, As2))
  	 end,
     %% Compute the real new signatures (using the lhs mask)
     if Ts == [] ->
 	    ?none;
        true ->
 	    ?tuple_types(M1, N, unique(update_tuple_keys(Ts, M1)))
     end.

%% Generate all combinations. All elements must have the same key.

t_subtract_combine(T1, Ts2) ->
    t_subtract_combine(T1, Ts2, []).

t_subtract_combine([T1 | Ts1], Ts2, As) ->
    t_subtract_combine_1(T1, Ts2, Ts1, Ts2, As);
t_subtract_combine([], _, As) ->
    As.    % no need to reverse

t_subtract_combine_1(T1, [T2 | Ts2], Ts1, Ts, As) ->
    case t_subtract(T1, T2) of
	?none -> 
	    t_subtract_combine_1(T1, Ts2, Ts1, Ts, As);
	T ->
	    t_subtract_combine_1(T1, Ts2, Ts1, Ts, [T | As])
    end;
t_subtract_combine_1(_, [], Ts1, Ts2, As) ->
    t_subtract_combine(Ts1, Ts2, As).

%% When we can have duplicate keys in the lists, we must generate all
%% (n*m) combinations of possible infs of same-key tuples.

t_subtract_dunion_dup([T1 = #c{c = C1, n = N1} | Ts1] = As1,
		      [T2 = #c{c = C2, n = N2} | Ts2] = As2) ->
    if C1 < C2 ->
	    [T1 | t_subtract_dunion_dup(Ts1, As2)];
       C1 > C2 ->
	    t_subtract_dunion_dup(As1, Ts2);
       N1 < N2 ->
	    [T1 | t_subtract_dunion_dup(Ts1, As2)];
       N1 > N2 ->
	    t_subtract_dunion_dup(As1, Ts2);
       true ->
	    {Ss1, Rs1} = take_while(Ts1, C1, N1),
	    {Ss2, Rs2} = take_while(Ts2, C1, N1),
	    t_subtract_combine([T1 | Ss1], [T2 | Ss2])
		++ t_subtract_dunion_dup(Rs1, Rs2)
    end;
t_subtract_dunion_dup([], _Ts) ->
    [];
t_subtract_dunion_dup(Ts, []) ->
    Ts.


%% ---------------------------------------------------------------------

%% @spec t_unify(type(), type()) -> {type(), [{Name, Type}]}
%%         Name = term()
%%         Type = type()
%%
%% @doc Unifies two types. If successful, returns a tuple <code>{T,
%% S}</code> with the unified type representation <code>T</code>, and a
%% substitution list <code>S</code> mapping variable names to types
%% (ordered by variable name), or otherwise throws <code>{mismatch, T1,
%% T2}</code> if subtypes <code>T1</code> and <code>T2</code> cannot be
%% unified.
%%
%% @see t_subst/2
%% @see t_var/1

t_unify(T1, T2) ->
    {T, Subs} = t_unify(T1, T2, dict:new()),
    {t_subst(T, Subs), lists:keysort(1,dict:to_list(Subs))}.

t_unify(?value_set(S1=#set{}), ?value_set(S2=#set{}), Subs) ->
    case set_is_equal(S1, S2) of
	true -> {?value_set(S1), Subs};
	false -> throw({mismatch, set_to_list(S1), set_to_list(S2)})
    end;
t_unify(?dunion(As1), ?dunion(As2), Subs) ->
    {As, Subs1} = t_unify_list(As1, As2, Subs),
    {?dunion(As), Subs1};
t_unify(?tuple_mask(M1, As1), ?tuple_mask(M2, As2), Subs)
  when M1 =/= M2 ->
    M = M1 band M2,
    {As, Subs1} = t_unify_list(reduce_tuple_keys_unique(M, M1, As1),
			       reduce_tuple_keys_unique(M, M2, As2), Subs),
    {?tuple_mask(M, As), Subs1};
t_unify(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}, Subs) ->
    {As, Subs1} = t_unify_list(As1, As2, Subs),
    {#c{c = C, n = N, as = As}, Subs1};
t_unify(T = ?none, ?none, Subs) -> {T, Subs};
t_unify(T = ?any, ?any, Subs) -> {T, Subs};
t_unify(T = #var{n = N}, #var{n = N}, Subs) -> {T, Subs};
t_unify(T1 = #var{n = N1}, T2 = #var{n = N2}, Subs) ->
    case dict:find(N1, Subs) of
	{ok, X1} ->
	    case dict:find(N2, Subs) of
		{ok, X2} -> t_unify(X1, X2, Subs);
		error -> t_unify(X1, T2, Subs)
	    end;
	error ->
	    case dict:find(N2, Subs) of
		{ok, X2} -> t_unify(T1, X2, Subs);
		error -> {T1, dict:store(N2, T1, Subs)}
	    end
    end;
t_unify(#var{n = N}, T, Subs) -> t_unify_var(N, T, Subs);
t_unify(T, #var{n = N}, Subs) -> t_unify_var(N, T, Subs);
t_unify(T1, T2, _) -> throw({mismatch, T1, T2}).

t_unify_list([T1 | Ts1], [T2 | Ts2], Subs) ->
    {T, Subs1} = t_unify(T1, T2, Subs),
    {Ts, Subs2} = t_unify_list(Ts1, Ts2, Subs1),
    {[T | Ts], Subs2};
t_unify_list([], [], Subs) ->
    {[], Subs}.

t_unify_var(N, T, Subs) ->
    case dict:find(N, Subs) of
	{ok, T1} -> t_unify(T, T1, Subs);
	error -> {T, dict:store(N, T, Subs)}
    end.


%% @spec t_subst(T::type(), Dict::dict()) -> type()
%%     dict() = dict:dict()
%%
%% @doc Substitutes type variables in type descriptor
%% <code>T</code>. <code>Dict</code> must be a dictionary (see module
%% <code>dict</code>) mapping variable names to types.
%%
%% @see t_var/1
%% @see t_unify/2
%% @see dict

t_subst(?dunion(As), Subs) ->
    ?dunion([t_subst(A, Subs) || A <- As]);
t_subst(#c{c = C, n = N, as = As}, Subs) ->
    #c{c = C, n = N, as = [t_subst(A, Subs) || A <- As]};
t_subst(T = ?none, _Subs) -> T;
t_subst(T = ?any, _Subs) -> T;
t_subst(T = #var{n = N}, Subs) ->
    case dict:find(N, Subs) of
	{ok, T1} -> T1; 
	error -> T
    end.


%% ---------------------------------------------------------------------

%% @spec t_limit(type(), K::integer()) -> type()
%%
%% @doc Restricts the size of a type descriptor to depth <code>K</code>.
%% The top level has depth zero. Only list and tuple constructors, and
%% function types, have a nonzero depth. Any subtype at depth
%% <code>K</code> will be changed to the <code>any()</code> type.
%%
%% @see t_from_term/2

t_limit(_T, K) when K =< 0 -> ?any;
t_limit(T, K) ->
    t_limit_1(T, K).

t_limit_1(?c_list(_, ?list_start_nil, _) = T, _K) -> T;
t_limit_1(?c_list(E, C, T), K) ->
    if K =< 1 -> ?c_list(?any, C, T);
       true ->
	    ?c_list(t_limit_1(E, K - 1), C, T)
    end;
t_limit_1(?c_tuple(?any) = T, _K) -> T;
t_limit_1(?c_tuple(?tuple_arities(Ts)), K) ->
    if K =< 1 ->
	    Ts1 = [?n_tuple_notag(N, lists:duplicate(N, ?any))
		   || ?tuple_types(_, N, _) <- Ts],
	    ?c_tuple(?tuple_arities(Ts1));
       true ->
	    ?c_tuple(?tuple_arities([t_limit_1(T, K - 1) || T <- Ts]))
    end;
t_limit_1(?c_fun(D, R), K) ->
    if K =< 1 ->
 	    case D of
 		?any -> ?c_fun(D, D);
 		?fun_domain_args(N, _) ->
		    ?c_fun(?fun_domain_args(N, mk_any_list(N)), ?any)
	    end;
       true -> 
	    K1 = K - 1,
	    ?c_fun(t_limit_1(D, K1), t_limit_1(R, K1))
    end;
t_limit_1(?dunion(Ts), K) ->
    ?dunion([t_limit_1(T, K) || T <- Ts]);
t_limit_1(#c{c = C, n = N, as = As}, K) ->
    #c{c = C, n = N, as = [t_limit_1(A, K) || A <- As]};
t_limit_1(T, _K) -> T.


%% @spec t_from_term(term()) -> type()
%% @equiv t_from_term(T, -1)

t_from_term(T) -> t_from_term(T, -1).

%% @spec t_from_term(T::term(), K::integer()) -> type()
%%
%% @doc Returns a type descriptor corresponding to term <code>T</code>,
%% limited to depth <code>K</code>. If <code>K</code> is negative, no
%% limiting is done. The limiting is similar to the effect of the
%% <code>t_limit/2</code> function, but here, <code>K</code> refers to
%% the depth of the given term. Note that only the head element of cons
%% cells is regarded as being deeper - the tail element is considered to
%% be at the same level as the cell itself.
%%
%% @see t_from_term/1
%% @see t_limit/2

t_from_term(_T, 0) -> ?any;
t_from_term([H | T], K) ->
    t_cons(t_from_term(H, K - 1), t_from_term(T, K));
t_from_term([], _K) -> t_nil();
t_from_term(T, K) when is_tuple(T) ->
    K1 = K - 1,
    t_tuple([t_from_term(E, K1) || E <- tuple_to_list(T)]);
t_from_term(T, _K) when is_integer(T) -> t_integer(T);
t_from_term(T, _K) when is_float(T) -> t_float();
t_from_term(T, _K) when is_atom(T) -> t_atom(T);
t_from_term(T, _K) when is_function(T) -> t_fun();
t_from_term(T, _K) when is_binary(T) -> t_binary();
t_from_term(T, _K) when is_pid(T) -> t_pid();
t_from_term(T, _K) when is_port(T) -> t_port();
t_from_term(T, _K) when is_reference(T) -> t_ref().


%% ---------------------------------------------------------------------

%% @spec t_to_string(type()) -> string()
%%
%% @doc Returns a string describing the type.

-define(sel(T, E), if T == ?none -> ?none;
		      true -> E
		   end).

-define(UNION_SEP, " | ").

t_to_string(?any) ->
    "any()";
t_to_string(?none) ->
    "none()";
t_to_string(#var{n = N}) ->
    ID =
	if is_atom(N) -> atom_to_list(N);
	   is_number(N) -> integer_to_list(N)
	end,
    "Var("++ID++")";
t_to_string(?product(_, As)) ->
    "<" ++ seq(fun t_to_string/1, As) ++ ">";
t_to_string(?union([A, N, L, T, F, B, I])) ->
    Es = [?sel(F, ?u_fun(?none, F)),
	  ?sel(I, ?u_identifier(?none, I)),
	  ?sel(B, ?u_binary(?none, B)),
	  ?sel(L, ?u_list(?none, L)),
	  ?sel(T, ?u_tuple(?none, T)),
	  ?sel(N, ?u_number(?none, N)),
	  ?sel(A, ?u_atom(?none, A))],
    Ts = [T || T <- Es, T =/= ?none],
    if Ts == [] -> "<** bad type **>";
       true -> seq(fun t_to_string_1/1, Ts, ?UNION_SEP)
    end.


t_to_string_1(?is_number(?singleton(V), _)) ->
    lists:flatten(io_lib:write(V));
t_to_string_1(?is_number(?value_set(S), _)) ->
    seq(fun t_to_string/1,
	[t_number(V) || V <- set_to_list(S)], ?UNION_SEP);
t_to_string_1(?is_byte(_)) ->
    "byte()";
t_to_string_1(?is_char(_, _)) ->
    "char()";
t_to_string_1(?is_integer(_, _)) ->
    "integer()";
t_to_string_1(?is_float) ->
    "float()";
t_to_string_1(?is_number(_, _)) ->
    "number()";
t_to_string_1(?is_atom(?singleton(V), _)) ->
    io_lib:write_string(atom_to_list(V), $');
t_to_string_1(?is_bool) ->
    "bool()";
t_to_string_1(?is_atom(?value_set(S), _)) ->
    seq(fun t_to_string/1,
	[t_atom(V) || V <- set_to_list(S)], ?UNION_SEP);
t_to_string_1(?is_atom(_, _)) ->
    "atom()";
t_to_string_1(?is_tuple(?tuple_arities(As))) ->
    seq(fun (?tuple_types(_, _, As)) ->
		seq(fun (?tuple_type(_, _, As)) ->
			    "{" ++ seq(fun(?any)->"_";(X) -> t_to_string(X)end,
				       As) ++ "}"
		    end,
		    As, ?UNION_SEP)
	end,
	As, ?UNION_SEP);
t_to_string_1(?is_tuple(_)) ->
    "tuple()";
t_to_string_1(?is_nil) ->
    "[]";
t_to_string_1(?is_nonempty_proper_list(?any)) ->
    "[any(),...]";
t_to_string_1(?is_nonempty_proper_list(T)) ->
    case t_is_char(T) andalso not t_is_byte(T) of
	true ->
	    "nonempty_string()";
	false ->
	    "[" ++ t_to_string(T) ++ ",...]"
    end;
t_to_string_1(?is_cons(?any, _)) ->
    "nonempty_possibly_improper_list()";
t_to_string_1(?is_cons(T, _)) ->
    S = t_to_string(T),
    "nonempty_possibly_improper_list(" ++ S ++ ")";
t_to_string_1(?is_proper_list(?any)) ->
    "[any()]";
t_to_string_1(?is_proper_list(T)) ->
    case t_is_char(T) andalso not t_is_byte(T) of
	true ->
	    "string()";
	false ->
	    "[" ++ t_to_string(T) ++ "]"
    end;
t_to_string_1(?is_improper_list(?any)) ->
    "possibly_improper_list()";
t_to_string_1(?is_improper_list(T)) ->
    "possibly_improper_list(" ++ t_to_string(T) ++ ")";
t_to_string_1(?is_fun(?any, ?any)) ->
    "function()";
t_to_string_1(?is_fun(?any, R)) ->
    "((...) -> " ++ t_to_string(R) ++ ")";
t_to_string_1(?is_fun(?fun_domain_args(_, As), R)) ->
    Fun = fun(?any) -> "_";
	     (X) -> t_to_string(X)
	  end,
    "((" ++ seq(Fun, As) ++ ") -> " ++ t_to_string(R) ++ ")";
t_to_string_1(?is_binary) ->
    "binary()";
t_to_string_1(?is_identifier(?identifier_class_is_pid)) ->
    "pid()";
t_to_string_1(?is_identifier(?identifier_class_is_port)) ->
    "port()";
t_to_string_1(?is_identifier(?identifier_class_is_ref)) ->
    "reference()";
t_to_string_1(?is_identifier(_)) ->
    "identifier()".

seq(F, Ts) ->
    seq(F, Ts, ",").

seq(F, [T], _Sep) ->
    F(T);
seq(F, [T | Ts], Sep) ->
    F(T) ++ Sep ++ seq(F, Ts, Sep);
seq(_F, [], _Sep) ->
    "".

%% ---------------------------------------------------------------------

%% Utility functions

mk_any_list(N) ->
    lists:duplicate(N, ?any).

any_t_none([?none | _]) ->
    true;
any_t_none([_ | Ts]) ->
    any_t_none(Ts);
any_t_none([]) ->
    false.

all_t_none(Ts) ->
    all_t_none(Ts, true).

all_t_none([?none | Ts], B) ->
    all_t_none(Ts, B);
all_t_none([_ | Ts], _B) ->
    all_t_none(Ts, false);
all_t_none([], B) ->
    B.

%% Handling of sets of values. Since we are generally going to do union
%% and intersection operations, ordered lists are most efficient.

set_to_list(#set{s = S}) -> S.

list_to_set(S) ->
    N = length(S),
    if N > ?SET_LIMIT ->
	    ?any;
       true ->
	    #set{n = N, s = S}
    end.

is_subset(S1, S2) ->
    ordsets:is_subset(set_to_list(S1), set_to_list(S2)).

set_is_equal(#set{n = N, s = S1}, #set{n = N, s = S2}) ->
    S1 == S2;
set_is_equal(_, _) ->
    false.

%% Note that union can return 'any()'

set_union(S1, S2) ->
    list_to_set(ordsets:union(set_to_list(S1), set_to_list(S2))).

%% Note that intersection/subtraction/filter can return the empty set.

set_intersection(S1, S2) ->
    list_to_set(ordsets:intersection(set_to_list(S1), set_to_list(S2))).

set_subtract(S1, S2) ->
    list_to_set(ordsets:subtract(set_to_list(S1), set_to_list(S2))).

set_filter(F, S) ->
    list_to_set([X || X <- set_to_list(S), F(X)]).
