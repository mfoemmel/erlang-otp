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
%%   <dt><a href="#t_undefined-0"><code>undefined()</code></a></dt>
%%     <dd>The undefined type. If some type <code>T</code> is a subtype
%%     of <code>undefined()</code>, then <code>T =
%%     undefined()</code>. In general, type constructors are strict, so
%%     that if some component is <code>undefined()</code>, the result is
%%     also <code>undefined()</code>, unless otherwise noted.</dd>
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
%%   <dt><a href="#t_cons_or_nil-0"><code>cons_or_nil()</code></a></dt>
%%     <dd>All proper and non-proper lists, i.e., all <code>[T, ...,
%%     T]</code> and <code>[T, ..., T | T]</code> (where the degree of
%%     <code>T</code> is 1), including the empty list
%%     <code>[]</code>. Note that for non-proper lists, the "tail" is
%%     regarded as one of the elements.  The list type constructors
%%     are strict.
%%     <dl>
%%       <dt><a href="#t_cons-0"><code>cons()</code></a></dt>
%%         <dd>All non-empty proper and non-proper lists <code>[... |
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

-module(erl_types).

-export([t_any/0, t_atom/0, t_atom/1, t_atom_vals/1, t_binary/0,
	 t_bool/0, t_bool/1, t_byte/0, t_byte/1, t_char/0, t_char/1,
	 t_components/1, t_cons/0, t_cons/1, t_cons/2, t_cons_or_nil/0,
	 t_cons_or_nil/1, t_cons_hd/1, t_cons_tl/1, t_data_args/1,
	 t_data_arity/1, t_degree/1, t_float/0, t_from_term/1,
	 t_from_term/2, t_fun/0, t_fun/1, t_fun/2, t_fun_args/1,
	 t_fun_arity/1, t_fun_range/1, t_identifier/0, t_inf/1, t_inf/2,
	 t_integer/0, t_integer/1, t_is_any/1, t_is_atom/1, t_is_atom/2,
	 t_is_binary/1, t_is_bool/1, t_is_byte/1, t_is_char/1,
	 t_is_cons/1, t_is_cons_or_nil/1, t_is_data/1, t_is_undefined/1,
	 t_is_float/1, t_is_fun/1, t_is_identifier/1, t_is_integer/1,
	 t_is_list/1, t_is_nonempty_list/1, t_is_nil/1, t_is_number/1,
	 t_is_number/2, t_is_pid/1, t_is_port/1, t_is_ref/1,
	 t_is_string/1, t_is_subtype/2, t_is_tuple/1, t_is_var/1,
	 t_limit/2, t_list/0, t_list/1, t_list_elements/1, t_nil/0,
	 t_nonempty_list/0, t_nonempty_list/1, t_nonempty_string/0,
	 t_number/0, t_number/1, t_number_vals/1, t_pid/0, t_port/0,
	 t_product/1, t_ref/0, t_string/0, t_subtract/2, t_sup/1,
	 t_sup/2, t_unify/2, t_subst/2, t_to_string/1, t_tuple/0,
	 t_tuple/1, t_tuple_args/1, t_tuple_arity/1, t_undefined/0,
	 t_var/1, t_var_name/1]).


%% ---------------------------------------------------------------------

%% Equality on type descriptors is the normal Erlang term equality.

%% Internal generic type representation definitions

-define(undefined, undefined).	%% no type
-define(any, any).		%% any-value

-record(var, {n}).		%% type variable
-record(c, {c, n, as}).		%% generic constructor type
-record(set, {n, s}).		%% value-set type (always wrapped)

%% Value set macros
-define(value_set(S), #c{c = S, n = 0, as = []}).
-define(singleton(V), ?value_set(#set{n = 1, s = [V]})).
-define(booleans, ?value_set(#set{n = 2, s = [false,true]})).


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

%% @spec t_undefined() -> type()
%%
%% @doc Returns the <code>undefined()</code>, i.e., "empty" type. No
%% other type is a subtype of <code>undefined()</code>
%%
%% @see t_is_undefined/1
%% @see t_any/0

t_undefined() -> ?undefined.


%% @spec t_is_undefined(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is the
%% <code>undefined()</code> type, otherwise <code>false</code>.
%%
%% @see t_undefined/0
 
t_is_undefined(?undefined) -> true;
t_is_undefined(_) -> false.


%% @spec t_any() -> type()
%%
%% @doc Returns the <code>any()</code>, i.e., "any value" type. All
%% types are subtypes of <code>any()</code>.
%%
%% @see t_is_any/1
%% @see t_undefined/0

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
%% <code>undefined()</code>, the result is also
%% <code>undefined()</code>. Note that if the degree <code>n</code> is
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
%% @see t_undefined/0
%% @see t_degree/1
%% @see t_components/1

t_product([]) -> ?product(0, []);
t_product([A]) -> A;
t_product(As = [_ | _]) ->
    case all_t_undefined(As) of
	true ->
	    ?undefined;
	false ->
	    ?product(length(As), As)
    end;
t_product(N) when is_integer(N), N >= 0 ->
    ?product(N, mk_any_list(N)).


%% @spec t_degree(T::type()) -> integer() | any | undefined
%%
%% @doc Returns the degree of a (product) type. If <code>T</code> is
%% <code>&lt;T1, ..., Tn&gt;</code>, the result is the integer
%% <code>n</code>. If <code>T</code> is <code>any()</code>, the result
%% is <code>any</code>. If <code>T</code> is <code>undefined()</code>,
%% the result is <code>undefined</code>.
%%
%% <p>Note that this is defined on all types: e.g.,
%% <code>t_degree(t_integer()) = 1</code>.</p>
%%
%% @see t_product/1
%% @see t_components/1

t_degree(?product(N, _)) -> N;
t_degree(?any) -> any;
t_degree(?undefined) -> undefined;
t_degree(_) -> 1.


%% @spec t_components(T::type()) -> [type()] | any | undefined
%%
%% @doc Returns the component types of a (product) type. If
%% <code>T</code> is <code>&lt;T1, ..., Tn&gt;</code>, the result is
%% <code>[T1, ..., Tn]</code>. If <code>T</code> is
%% <code>any()</code>, the result is <code>any</code>.  If
%% <code>T</code> is <code>undefined()</code>, the result is
%% <code>undefined</code>.
%%
%% <p>Note that this is defined on all types: e.g.,
%% <code>t_components(t_integer()) = [t_integer()]</code>.</p>
%%
%% @see t_product/1
%% @see t_degree/1

t_components(?product(_, As)) -> As;
t_components(?any) -> any;
t_components(?undefined) -> undefined;
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
-define(number(V, C), ?u_number(?undefined, ?c_number(V, C))).
-define(is_number(V, C), ?u_number(?undefined, ?c_number(V, C))).
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

%% Note that we need not check for floats - they don't have value sets.
filter_numbers(S, ?number_class_is_integer(C)) ->
    case C of
	?integer_class_is_char(C1) ->
	    case C1 of
		?char_class_is_byte ->
		    F = fun (X) when X >= 0, X =< 255 -> true;
			    (_) -> false
			end,
		    set_filter(F, S);
		?any ->
		    F = fun (X) when X >= 0, X =< 16#10ffff -> true;
			    (_) -> false
			end,
		    set_filter(F, S)
	    end;
	?any ->
	    S
    end;
filter_numbers(_, ?undefined) -> ?undefined;
filter_numbers(S, _) -> S.

%% Note that we need not check for floats - they don't have value sets.
classify_numbers(?any) -> ?any;
classify_numbers(#set{n = 0}) -> ?undefined;
classify_numbers(#set{s = S}) ->
    A = ?number_class_integer(?integer_class_char(?char_class_byte)),
    classify_numbers_1(S, A).

classify_numbers_1([N | Ns], A) when N >= 0, N =< 255 ->
    classify_numbers_1(Ns, A);
classify_numbers_1([_ | _] = Ns, _) ->
    A = ?number_class_integer(?integer_class_char(?any)),
    classify_numbers_2(Ns, A);
classify_numbers_1([], A) -> A.

classify_numbers_2([N | Ns], A) when N >= 0, N =< 16#10ffff ->
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

t_is_number(N, ?is_number(?singleton(N), _)) -> true;
t_is_number(_, _) -> false.


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

t_integer(V) when is_integer(V), V >= 0, V =< 255 ->
    t_byte(V);
t_integer(V) when is_integer(V), V >= 0, V =< 16#10ffff ->
    t_char(V);
t_integer(V) when is_integer(V) ->
    ?integer(?singleton(V), ?any).


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

t_char(V) when is_integer(V), V >= 0, V =< 255 ->
    t_byte(V);
t_char(V) when is_integer(V), V >= 0, V =< 16#10ffff ->
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

t_byte(V) when is_integer(V), V >= 0, V =< 255 ->
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
-define(atom(V, C), ?u_atom(?undefined, ?c_atom(V, C))).
-define(is_atom(V, C), ?u_atom(?undefined, ?c_atom(V, C))).
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
t_atom(V) -> ?atom(?singleton(V), ?any).


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

t_is_atom(A, ?is_atom(?singleton(A), _)) -> true;
t_is_atom(_, _) -> false.


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

t_bool(true) -> t_atom(true);
t_bool(false) -> t_atom(false).


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
%%    Elements: Any
%%	        |
%%              Types: Type list

-define(c_tuple(T), #c{c = tuple, n = 1, as = [T]}).
-define(tuple(T), ?u_tuple(?undefined, ?c_tuple(T))).
-define(is_tuple(T), ?u_tuple(?undefined, ?c_tuple(T))).
-define(tuple_elements_types(N, Ts), #c{c = types, n = N, as = Ts}).


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
%% <code>undefined()</code>, the result is also
%% <code>undefined()</code>.
%%
%% @see t_tuple/0
%% @see t_is_tuple/1
%% @see t_tuple_args/1

t_tuple(As) when is_list(As) ->
    case any_t_undefined(As) of
	true ->
	    ?undefined;
	false ->
	    ?tuple(?tuple_elements_types(length(As), As))
    end;
t_tuple(N) when is_integer(N), N >= 0 ->
    ?tuple(?tuple_elements_types(N, mk_any_list(N))).


%% @spec t_is_tuple(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>tuple()</code>, or <code>false</code> otherwise.
%%
%% @see t_tuple/0

t_is_tuple(?is_tuple(_)) -> true;
t_is_tuple(_) -> false.


%% @spec t_tuple_arity(T::type()) -> integer() | any
%%
%% @doc Returns the arity of a tuple type. If <code>T</code> is
%% <code>{T1, ..., Tn}</code>, the result is the integer <code>n</code>,
%% otherwise the result is <code>any</code>.
%%
%% @see t_tuple/1
%% @see t_tuple_args/1

t_tuple_arity(?is_tuple(?any)) -> any;
t_tuple_arity(?is_tuple(?tuple_elements_types(N, _))) -> N.


%% @spec t_tuple_args(T::type()) -> [type()] | any
%%
%% @doc Returns the list of arguments of a tuple type. If <code>T</code>
%% is <code>{T1, ..., Tn}</code>, the result is <code>[T1, ...,
%% Tn]</code>, otherwise the result is <code>any</code>.
%%
%% @see t_tuple/1
%% @see t_tuple_arity/1

t_tuple_args(?is_tuple(?any)) -> any;
t_tuple_args(?is_tuple(?tuple_elements_types(_, As))) -> As.


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
%% The Type component is always undefined() for the empty list (nil),
%% but never for any other list type.

-define(c_list(E, C, T), #c{c = list, n = 3, as = [E, C, T]}).
-define(list(E, C, T), ?u_list(?undefined,  ?c_list(E, C, T))).
-define(is_list(E, C, T), ?u_list(?undefined, ?c_list(E, C, T))).
-define(list_start_cons, #c{c = cons, n = 0, as = []}).
-define(list_start_is_cons, #c{c = cons}).
-define(list_start_nil, #c{c = nil, n = 0, as = []}).
-define(list_start_is_nil, #c{c = nil}).
-define(list_end_nil, #c{c = nil, n = 0, as = []}).
-define(list_end_is_nil, #c{c = nil}).

-define(proper_list(E, C), ?list(E, C, ?list_end_nil)).
-define(nil, ?proper_list(?undefined, ?list_start_nil)).
-define(cons(E, T), ?list(E, ?list_start_cons, T)).
-define(nonempty_proper_list(E), ?proper_list(E, ?list_start_cons)).
-define(is_proper_list(E), ?is_list(E, _, ?list_end_is_nil)).
-define(is_nil, ?is_list(_, ?list_start_is_nil, _)).
-define(is_cons(E, T), ?is_list(E, ?list_start_is_cons, T)).
-define(is_nonempty_proper_list(E),
	?is_list(E, ?list_start_is_cons, ?list_end_is_nil)).


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

t_is_nonempty_list(?is_nonempty_proper_list(_)) -> true;
t_is_nonempty_list(_) -> false.


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
%% <code>cons()</code>, otherwise <code>false</code>.
%%
%% @see t_cons/0

t_is_cons(?is_cons(_, _)) -> true;
t_is_cons(_) -> false.


%% @spec t_is_cons_or_nil(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>cons_or_nil()</code>, otherwise <code>false</code>.
%%
%% @see t_cons_or_nil/0

t_is_cons_or_nil(?is_list(_, _, _)) -> true;
t_is_cons_or_nil(_) -> false.


%% @spec t_is_string(T::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T</code> is in
%% <code>string()</code>, otherwise <code>false</code>.
%%
%% @see t_string/0

t_is_string(?is_nil) -> true;
t_is_string(?is_proper_list(T)) -> t_is_char(T);
t_is_string(_) -> false.


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
%% @see t_cons/0
%% @see t_nil/0
%% @see t_nonempty_list/0
%% @see t_cons_or_nil/0
%% @see t_string/0

t_list() -> ?proper_list(?any, ?any).


%% @spec t_list(T::type()) -> type()
%%
%% @doc Returns the <code>list(T)</code> type. This represents all
%% proper lists <code>[T, ..., T]</code>, i.e., whose elements have type
%% T. If <code>T</code> is <code>undefined()</code>, the result is also
%% <code>undefined()</code>.
%%
%% @see t_list/0
%% @see t_is_list/1
%% @see t_list_elements/1
%% @see t_cons/2
%% @see t_nil/0
%% @see t_nonempty_list/1
%% @see t_cons_or_nil/1
%% @see t_string/0

t_list(?undefined) -> ?undefined;
t_list(T) -> ?proper_list(T, ?any).


%% @spec t_list_elements(type()) -> type()
%%
%% @doc Returns the type of the elements of any list type. This includes
%% all proper and non-proper lists. Note that the element type of the
%% empty list is <code>undefined()</code>.
%%
%% @see t_list/1
%% @see t_cons_or_nil/1
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


%% @spec t_cons() -> type()
%%
%% @doc Returns the <code>cons()</code> type. This represents all
%% non-empty proper and non-proper lists <code>[... | ...]</code>.
%%
%% @see t_cons/2
%% @see t_list/0

t_cons() -> ?cons(?any, ?any).


%% @spec t_cons(T::type()) -> type()
%% @equiv t_cons(T, T)
%% @see t_cons/2

t_cons(T) -> ?cons(T, ?any).


%% @spec t_cons(Head::type(), Tail::type()) -> type()
%%
%% @doc Returns a <code>nonempty_list()</code> or <code>cons()</code>
%% type, depending on the type of <code>Tail</code>. If
%% <code>Tail</code> is in <code>list(T)</code>, the result is in
%% <code>nonempty_list(U)</code>, where <code>U = t_sup(T,
%% Head)</code>. Otherwise, the result is in <code>cons(V)</code>, where
%% <code>V = t_sup(Head, Tail)</code>. If <code>Head</code> or
%% <code>Tail</code> is <code>undefined()</code>, the result is also
%% <code>undefined()</code>.
%%
%% @see t_cons/0
%% @see t_cons_hd/1
%% @see t_cons_tl/1
%% @see t_list/1
%% @see t_nonempty_list/1
%% @see t_cons_or_nil/1
%% @see t_sup/2

t_cons(?undefined, _) -> ?undefined;
t_cons(_, ?undefined) -> ?undefined;
t_cons(_, ?any) -> ?cons(?any, ?any);
t_cons(T1, ?is_list(T2, _, E)) -> ?cons(t_sup(T1, T2), E);
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
%% @see t_cons_or_nil/1

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
%% @see t_cons_or_nil/1

t_cons_tl(?is_cons(T, ?list_end_is_nil)) -> t_list(T);
t_cons_tl(?is_cons(T, _)) -> T.


%% @spec t_cons_or_nil() -> type()
%%
%% @doc Returns the <code>cons_or_nil()</code> type. This represents all
%% proper and non-proper lists <code>[... | ...]</code>, including the
%% empty list <code>[]</code>.
%%
%% @see t_cons_or_nil/1
%% @see t_is_cons_or_nil/1
%% @see t_list_elements/1
%% @see t_cons/0
%% @see t_nil/0
%% @see t_list/0

t_cons_or_nil() -> ?list(?any, ?any, ?any).


%% @spec t_cons_or_nil(T::type()) -> type()
%%
%% @doc Returns the <code>cons_or_nil(T)</code> type. This represents
%% all proper and non-proper lists <code>[T, ..., T | T]</code>,
%% including the empty list <code>[]</code>. If <code>T</code> is
%% <code>undefined()</code>, the result is also
%% <code>undefined()</code>.
%%
%% @see t_cons_or_nil/0
%% @see t_is_cons_or_nil/1
%% @see t_list_elements/1
%% @see t_cons/0
%% @see t_nil/0
%% @see t_list/1

t_cons_or_nil(T) -> ?list(T, ?any, ?any).


%% @spec t_nonempty_list() -> type()
%%
%% @doc Returns the <code>nonempty_list()</code> type. This represents
%% all nonempty proper lists.
%%
%% @see t_nonempty_list/1
%% @see t_nonempty_string/0
%% @see t_is_nonempty_list/1
%% @see t_list/0

t_nonempty_list() -> ?nonempty_proper_list(?any).


%% @spec t_nonempty_list(T::type()) -> type()
%%
%% @doc Returns the <code>nonempty_list(T)</code> type. This represents
%% all nonempty proper lists <code>[T, ..., T]</code>. If <code>T</code>
%% is <code>undefined()</code>, the result is also
%% <code>undefined()</code>.
%%
%% @see t_nonempty_list/0
%% @see t_is_nonempty_list/1
%% @see t_list/0

t_nonempty_list(?undefined) -> ?undefined;
t_nonempty_list(T) -> ?nonempty_proper_list(T).


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

t_nonempty_string() -> ?nonempty_proper_list(t_char()).


%% ---------------------------------------------------------------------

%% Representation of functions:
%%
%%    Domain: Any
%%	      |
%%            Arguments: Type list
%%
%%    Range: Type

-define(c_fun(D, R), #c{c = 'fun', n = 2, as = [D, R]}).
-define('fun'(D, R), ?u_fun(?undefined, ?c_fun(D, R))).
-define(is_fun(D, R), ?u_fun(_, ?c_fun(D, R))).
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
%% <code>undefined()</code> type; a function type can represent a
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

t_fun_arity(?is_fun(?any, _)) -> any;
t_fun_arity(?is_fun(?fun_domain_args(N, _), _)) -> N.


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
-define(binary, ?u_binary(?undefined, ?c_binary)).
-define(is_binary, ?u_binary(_, ?c_binary)).

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
-define(identifier(C), ?u_identifier(?undefined, ?c_identifier(C))).
-define(is_identifier(C), ?u_identifier(?undefined, ?c_identifier(C))).
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

t_is_identifier(?is_identifier(_)) -> true;
t_is_identifier(_) -> false.


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

t_is_data(?is_number(_, _)) -> true;
t_is_data(?is_atom(_, _)) -> true;
t_is_data(?is_tuple(_)) -> true;
t_is_data(?is_list(_, _, _)) -> true;
t_is_data(_) -> false.


%% @spec t_data_arity(T::type()) -> integer() | any
%%
%% @doc Returns the arity of a data constructor type <code>T</code>, or
%% <code>any</code> if unknown.
%%
%% @see t_is_data/1
%% @see t_data_args/1
%% @see t_tuple_arity/1

t_data_arity(?is_tuple(?tuple_elements_types(N, _))) -> N;
t_data_arity(?is_tuple(_)) -> any;
t_data_arity(?is_nil) -> 0;
t_data_arity(?is_cons(_, _)) -> 2;
t_data_arity(?is_list(_, _, _)) -> any;
t_data_arity(_) -> 0.


%% @spec t_data_args(T::type()) -> [type()] | any
%%
%% @doc Returns the list of arguments of a data constructor type
%% <code>T</code>, or <code>any</code> if unknown.
%%
%% @see t_is_data/1
%% @see t_data_arity/1
%% @see t_tuple_argsp/1

t_data_args(?is_tuple(?tuple_elements_types(_, As))) -> As;
t_data_args(?is_tuple(_)) -> any;
t_data_args(?is_nil) -> [];
t_data_args(?is_nonempty_proper_list(T)) -> [T, t_list(T)];
t_data_args(?is_cons(T, _)) -> [T, ?any];
t_data_args(?is_list(_, _, _)) -> any;
t_data_args(_) -> [].


%% ---------------------------------------------------------------------

%% @spec t_is_subtype(T1::type(), T2::type()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>T1</code> is a subtype of
%% <code>T2</code>, otherwise <code>false</code>.

t_is_subtype(#c{c = S1=#set{}, n = N, as = As1},
	     #c{c = S2=#set{}, n = N, as = As2}) ->
    case is_subset(S1, S2) of
	true -> t_is_subtype_lists(As1, As2);
	false -> false
    end;
t_is_subtype(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    t_is_subtype_lists(As1, As2);
t_is_subtype(?undefined, _) ->
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


%% @spec t_sup([type()]) -> type()
%% @doc Returns the supremum of a list of types.
%% @see t_sup/2

t_sup([T | Ts]) -> t_sup(T, t_sup(Ts));
t_sup([]) -> ?undefined.


%% @spec t_sup(type(), type()) -> type()
%%
%% @doc Returns the supremum (least upper bound, or "join") of two
%% types. This is analogous to set union. Occurrences of type variables
%% are regarded as representing the <code>any()</code> type.
%%
%% @see t_sup/1
%% @see t_inf/2

t_sup(?value_set(S1=#set{}), ?value_set(S2=#set{})) ->
    case set_union(S1, S2) of
	any -> ?any;
	S -> ?value_set(S)
    end;
t_sup(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    #c{c = C, n = N, as = t_sup_lists(As1, As2)};
t_sup(?undefined, T) ->
    T;
t_sup(T, ?undefined) ->
    T;
t_sup(_, _) ->
    ?any.

t_sup_lists([T1 | Ts1], [T2 | Ts2]) ->
    [t_sup(T1, T2) | t_sup_lists(Ts1, Ts2)];
t_sup_lists([], []) ->
    [].


%% @spec t_inf([type()]) -> type()
%% @doc Returns the infimum of a list of types.
%% @see t_inf/2

t_inf([T | Ts]) -> t_inf(T, t_inf(Ts));
t_inf([]) -> ?any.


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
	?undefined -> ?undefined;
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
	?undefined -> ?undefined;
	C -> ?c_number(?any, C)
    end;
t_inf(?c_number(?value_set(S1=#set{}), C1),
      ?c_number(?value_set(S2=#set{}), C2)) ->
    %% In this case, we know that if the set intersection is nonempty,
    %% the infimum of the class fields is correct (and not undefined).
    case set_intersection(S1, S2) of
	#set{n = 0} -> ?undefined;
	S -> ?c_number(?value_set(S), t_inf(C1, C2))
    end;
t_inf(?c_number(V1, C1), ?c_number(V2, C2)) ->
    %% Here, one but not the other is a value set (so t_inf(V1, V2) will
    %% return that set). However, the values must be filtered using the
    %% resulting class (which could be undefined).
    C = t_inf(C1, C2),
    ?value_set(S) = t_inf(V1, V2),
    case filter_numbers(S, C) of
	#set{n = 0} -> ?undefined;
	S1 -> ?c_number(?value_set(S1), C)
    end;
t_inf(?c_list(E1, C1, T1), ?c_list(E2, C2, T2)) ->
    %% The element type can be undefined() only for nil, not for any
    %% other list type.
    E = t_inf(E1, E2),
    C = t_inf(C1, C2),
    T = t_inf(T1, T2),
    if C == ?list_start_nil -> ?c_list(E, C, T);
       E == ?undefined -> ?undefined;
       C == ?undefined -> ?undefined;
       T == ?undefined -> ?undefined;
       true -> ?c_list(E, C, T)
    end;
t_inf(?c_fun(?fun_domain_args(N, Ts1), R1),
      ?c_fun(?fun_domain_args(N, Ts2), R2)) ->
    %% The function domain and range can be undefined().
    ?c_fun(?fun_domain_args(N, t_inf_lists(Ts1, Ts2)), t_inf(R1, R2));
t_inf(?union(Ts1), ?union(Ts2)) ->
    %% Unions map to undefined() only if all elements are undefined().
    case t_inf_lists_all(Ts1, Ts2) of
	undefined -> ?undefined;
	Ts -> ?union(Ts)
    end;
t_inf(?value_set(S1=#set{}), ?value_set(S2=#set{})) ->
    case set_intersection(S1, S2) of
	#set{n = 0} -> ?undefined;
	S -> ?value_set(S)
    end;
t_inf(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    %% Default case maps to undefined() if any element is undefined().
    case t_inf_lists_any(As1, As2) of
	undefined -> ?undefined;
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
    ?undefined.

t_inf_lists([T1 | Ts1], [T2 | Ts2]) ->
    [t_inf(T1, T2) | t_inf_lists(Ts1, Ts2)];
t_inf_lists([], []) ->
    [].

t_inf_lists_any(Ts1, Ts2) ->
    t_inf_lists_any(Ts1, Ts2, []).

t_inf_lists_any([T1 | Ts1], [T2 | Ts2], As) ->
    case t_inf(T1, T2) of
	?undefined -> undefined;
	A -> t_inf_lists_any(Ts1, Ts2, [A | As])
    end;
t_inf_lists_any([], [], As) ->
    lists:reverse(As).

t_inf_lists_all(Ts1, Ts2) ->
    t_inf_lists_all(Ts1, Ts2, [], true).

t_inf_lists_all([T1 | Ts1], [T2 | Ts2], As, B) ->
    case t_inf(T1, T2) of
	?undefined = A ->
	    t_inf_lists_all(Ts1, Ts2, [A | As], B);
	A ->
	    t_inf_lists_all(Ts1, Ts2, [A | As], false)
    end;
t_inf_lists_all([], [], As, false) ->
    lists:reverse(As);
t_inf_lists_all([], [], _, true) ->
    undefined.


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

t_subtract(?c_atom(V1, _C1), ?c_atom(V2, _C2)) ->
    %% See t_inf/2 for more information.
    case t_subtract(V1, V2) of
	?undefined -> ?undefined;
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
	?undefined -> ?undefined;
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
	?undefined -> ?undefined;
	C -> ?c_number(?value_set(S), C)
    end;
t_subtract(?c_number(?value_set(S0=#set{}), C1), ?c_number(?any, C2)) ->
    case t_subtract(C1, C2) of
	?undefined -> ?undefined;
	C ->
	    %% Here, we need to subtract those elements of the set that
	    %% are of the RHS class. (The result can be empty.)
	    S1 = filter_numbers(S0, C2),
	    S = set_subtract(S0, S1),
	    case classify_numbers(S) of
		?undefined -> ?undefined;
		C -> ?c_number(?value_set(S), C)
	    end
    end;
t_subtract(?c_list(E1, C1, T1), ?c_list(E2, C2, T2)) ->
    %% The element type can be undefined() only for nil, not for any
    %% other list type.
    E = t_subtract(E1, E2),
    C = t_subtract(C1, C2),
    T = t_subtract(T1, T2),
    if T == ?undefined -> ?undefined;
       C == ?undefined -> ?undefined;
       C == ?list_start_nil -> ?c_list(E, C, T);
       E == ?undefined -> ?undefined;
       true -> ?c_list(E, C, T)
    end;
t_subtract(?c_fun(?fun_domain_args(N, Ts1), R1),
	   ?c_fun(?fun_domain_args(N, Ts2), R2)) ->
    %% Function domain and range can be undefined().
    ?c_fun(?fun_domain_args(N, t_subtract_lists(Ts1, Ts2)),
           t_subtract(R1, R2));
t_subtract(?union(Ts1), ?union(Ts2)) ->
    %% Unions map to undefined() only if all elements are undefined().
    case t_subtract_lists_all(Ts1, Ts2) of
	undefined -> ?undefined;
	Ts -> ?union(Ts)
    end;
t_subtract(?value_set(S1=#set{}), ?value_set(S2=#set{})) ->
    case set_subtract(S1, S2) of
	#set{n = 0} -> ?undefined;
	S -> ?value_set(S)
    end;
t_subtract(#c{c = C, n = 0, as = []}, #c{c = C, n = 0, as = []}) ->
    ?undefined;    % X \ X = undefined()
t_subtract(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}) ->
    %% Default case maps to undefined() if any element is undefined().
    case t_subtract_lists_any(As1, As2) of
	undefined -> ?undefined;
	As -> #c{c = C, n = N, as = As}
    end;
t_subtract(?any, _) ->
    ?any;
t_subtract(#var{}, _) ->
    ?any;
t_subtract(_, ?any) ->
    ?undefined;
t_subtract(_, #var{}) ->
    ?undefined;
t_subtract(T, _) -> T.

t_subtract_lists([T1 | Ts1], [T2 | Ts2]) ->
    [t_subtract(T1, T2) | t_subtract_lists(Ts1, Ts2)];
t_subtract_lists([], []) ->
    [].

t_subtract_lists_any(Ts1, Ts2) ->
    t_subtract_lists_any(Ts1, Ts2, []).

t_subtract_lists_any([T1 | Ts1], [T2 | Ts2], As) ->
    case t_subtract(T1, T2) of
	?undefined -> undefined;
	A -> t_subtract_lists_any(Ts1, Ts2, [A | As])
    end;
t_subtract_lists_any([], [], As) ->
    lists:reverse(As).

t_subtract_lists_all(Ts1, Ts2) ->
    t_subtract_lists_all(Ts1, Ts2, [], true).

t_subtract_lists_all([T1 | Ts1], [T2 | Ts2], As, B) ->
    case t_subtract(T1, T2) of
	?undefined = A ->
	    t_subtract_lists_all(Ts1, Ts2, [A | As], B);
	A ->
	    t_subtract_lists_all(Ts1, Ts2, [A | As], false)
    end;
t_subtract_lists_all([], [], As, false) ->
    lists:reverse(As);
t_subtract_lists_all([], [], _, true) ->
    undefined.


%% ---------------------------------------------------------------------

%% @spec t_unify(type(), type()) -> {type(), [{Name, Type}]}
%%         Name = term()
%%         Type = type()
%%
%% @doc Unifies two types. If successful, returns a tuple <code>{T,
%% S}</code> with the unified type representation <code>T</code>, and a
%% substitution list <code>S</code> mapping variable names to types, or
%% otherwise throws <code>{mismatch, T1, T2}</code> if subtypes
%% <code>T1</code> and <code>T2</code> cannot be unified.
%%
%% @see t_subst/2
%% @see t_var/1

t_unify(T1, T2) ->
    {T, Subs} = t_unify(T1, T2, dict:new()),
    {t_subst(T, Subs), dict:to_list(Subs)}.

t_unify(?value_set(S1=#set{}), ?value_set(S2=#set{}), Subs) ->
    case set_is_equal(S1, S2) of
	true -> {?value_set(S1), Subs};
	false -> throw({mismatch, set_to_list(S1), set_to_list(S2)})
    end;
t_unify(#c{c = C, n = N, as = As1}, #c{c = C, n = N, as = As2}, Subs) ->
    {As, Subs1} = t_unify_list(As1, As2, Subs),
    {#c{c = C, n = N, as = As}, Subs1};
t_unify(T = ?undefined, ?undefined, Subs) -> {T, Subs};
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

t_subst(#c{c = C, n = N, as = As}, Subs) ->
    #c{c = C, n = N, as = [t_subst(A, Subs) || A <- As]};
t_subst(T = ?undefined, _Subs) -> T;
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
%% The top level has depth zero. Only list and tuple constructors have a
%% nonzero depth. Any subtype at depth <code>K</code> will be changed to
%% the <code>any()</code> type.
%%
%% @see t_from_term/2

t_limit(?c_list(_, ?list_start_nil, _) = T, _K) -> T;
t_limit(?c_list(E, C, T), K) ->
    if K =< 0 -> ?c_list(?any, C, T);
       true ->
	    ?c_list(t_limit(E, K - 1), C, T)
    end;
t_limit(?c_tuple(?tuple_elements_types(N, Ts)), K) ->
    if K =< 0 -> ?c_tuple(?any);
       true ->
	    K1 = K - 1,
	    ?c_tuple(?tuple_elements_types(N, [t_limit(T, K1) || T <- Ts]))
    end;
% t_limit(#c{}, K) when K =< 0 -> ?any;
% t_limit(?union(Ts), K) ->
%     %% We must not get 'any'-types as union subtypes.
%      ?union([t_limit(T, K) || T <- Ts]);
t_limit(#c{c = C, n = N, as = As}, K) ->
    #c{c = C, n = N, as = [t_limit(A, K) || A <- As]};
% t_limit(#c{c = C, n = N, as = As}, K) ->
%     K1 = K - 1,
%     As1 = [t_limit(A, K1) || A <- As],
%     #c{c = C, n = N, as = As1};
t_limit(T, _K) -> T.


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
t_from_term(T, K) ->
    t_from_term_1(T, K).

t_from_term_1([H | T], K) ->
    t_cons(t_from_term(H, K - 1), t_from_term(T, K));
t_from_term_1(T, _K) when is_integer(T) -> t_integer(T);
t_from_term_1(T, _K) when is_float(T) -> t_float();
t_from_term_1(T, _K) when is_atom(T) -> t_atom(T);
t_from_term_1([], _K) -> t_nil();
t_from_term_1(T, K) when is_tuple(T) ->
    K1 = K - 1,
    t_tuple([t_from_term(E, K1) || E <- tuple_to_list(T)]);
t_from_term_1(T, _K) when is_function(T) -> t_fun();
t_from_term_1(T, _K) when is_binary(T) -> t_binary();
t_from_term_1(T, _K) when is_pid(T) -> t_pid();
t_from_term_1(T, _K) when is_port(T) -> t_port();
t_from_term_1(T, _K) when is_reference(T) -> t_ref().


%% ---------------------------------------------------------------------

%% @spec t_to_string(type()) -> string()
%%
%% @doc Returns a string describing the type.

-define(sel(T, E), if T == ?undefined -> ?undefined;
		      true -> E
		   end).

t_to_string(?any) ->
    "any()";
t_to_string(?undefined) ->
    "undefined()";
t_to_string(#var{n = N}) ->
    lists:flatten(io_lib:format("~w", [N]));
t_to_string(?product(_, As)) ->
    "<" ++ seq(fun t_to_string/1, As) ++ ">";
t_to_string(?union([A, N, L, T, F, B, I])) ->
    Es = [?sel(F, ?u_fun(?undefined, F)),
	  ?sel(I, ?u_identifier(?undefined, I)),
	  ?sel(B, ?u_binary(?undefined, B)),
	  ?sel(L, ?u_list(?undefined, L)),
	  ?sel(T, ?u_tuple(?undefined, T)),
	  ?sel(N, ?u_number(?undefined, N)),
	  ?sel(A, ?u_atom(?undefined, A))],
    Ts = [T || T <- Es, T =/= ?undefined],
    if Ts == [] -> "<** bad type **>";
       true -> seq(fun t_to_string_1/1, Ts, " | ")
    end.


t_to_string_1(?is_number(?singleton(V), _)) ->
    lists:flatten(io_lib:format("~w", [V]));
t_to_string_1(?is_number(?value_set(S), _)) ->
    seq(fun t_to_string/1,
	[t_number(V) || V <- set_to_list(S)], " | ");
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
    lists:flatten(io_lib:format("~w", [V]));
t_to_string_1(?is_bool) ->
    "bool()";
t_to_string_1(?is_atom(?value_set(S), _)) ->
    seq(fun t_to_string/1,
	[t_atom(V) || V <- set_to_list(S)], " | ");
t_to_string_1(?is_atom(_, _)) ->
    "atom()";
t_to_string_1(?is_tuple(?tuple_elements_types(_, As))) ->
    "{" ++ seq(fun t_to_string/1, As) ++ "}";
t_to_string_1(?is_tuple(_)) ->
    "tuple()";
t_to_string_1(?is_nil) ->
    "[]";
t_to_string_1(?is_nonempty_proper_list(?any)) ->
    "nonempty_list()";
t_to_string_1(?is_nonempty_proper_list(T)) ->
    case t_is_char(T) andalso not t_is_byte(T) of
	true ->
	    "nonempty_string()";
	false ->
	    "nonempty_list(" ++ t_to_string(T) ++ ")"
    end;
t_to_string_1(?is_cons(T, _)) ->
    S = t_to_string(T),
    "cons(" ++ S ++ ")";
t_to_string_1(?is_proper_list(?any)) ->
    "list()";
t_to_string_1(?is_proper_list(T)) ->
    case t_is_char(T) andalso not t_is_byte(T) of
	true ->
	    "string()";
	false ->
	    "list(" ++ t_to_string(T) ++ ")"
    end;
t_to_string_1(?is_list(?any, _, _)) ->
    "cons_or_nil()";
t_to_string_1(?is_list(T, _, _)) ->
    "cons_or_nil(" ++ t_to_string(T) ++ ")";
t_to_string_1(?is_fun(?any, R)) ->
    "(*) -> " ++ t_to_string(R);
t_to_string_1(?is_fun(?fun_domain_args(_, As), R)) ->
    "(" ++ seq(fun t_to_string/1, As) ++ ") -> " ++ t_to_string(R);
t_to_string_1(?is_binary) ->
    "binary()";
t_to_string_1(?is_identifier(?identifier_class_is_pid)) ->
    "pid()";
t_to_string_1(?is_identifier(?identifier_class_is_port)) ->
    "port()";
t_to_string_1(?is_identifier(?identifier_class_is_ref)) ->
    "ref()";
t_to_string_1(?is_identifier(_)) ->
    "identifier()".

seq(F, Ts) ->
    seq(F, Ts, ", ").

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

any_t_undefined([?undefined | _]) ->
    true;
any_t_undefined([_ | Ts]) ->
    any_t_undefined(Ts);
any_t_undefined([]) ->
    false.

all_t_undefined(Ts) ->
    all_t_undefined(Ts, true).

all_t_undefined([?undefined | Ts], B) ->
    all_t_undefined(Ts, B);
all_t_undefined([_ | Ts], _B) ->
    all_t_undefined(Ts, false);
all_t_undefined([], B) ->
    B.

%% Handling of sets of values. Since we are generally going to do union
%% and intersection operations, ordered lists are most efficient.  The
%% upper set size limit should at least be strictly smaller than 'byte',
%% and large enough for most sets of constants used for switching.

-define(SET_LIMIT, 64).

set_to_list(#set{s = S}) -> S.

list_to_set(S) ->
    N = length(S),
    if N > ?SET_LIMIT ->
	    any;
       true ->
	    #set{n = N, s = S}
    end.

is_subset(S1, S2) ->
    ordsets:is_subset(set_to_list(S1), set_to_list(S2)).

set_is_equal(#set{n = N, s = S1}, #set{n = N, s = S2}) ->
    S1 == S2;
set_is_equal(_, _) ->
    false.

%% Note that union can return 'any'

set_union(S1, S2) ->
    list_to_set(ordsets:union(set_to_list(S1), set_to_list(S2))).

%% Note that intersection/subtraction/filter can return the empty set.

set_intersection(S1, S2) ->
    list_to_set(ordsets:intersection(set_to_list(S1), set_to_list(S2))).

set_subtract(S1, S2) ->
    list_to_set(ordsets:subtract(set_to_list(S1), set_to_list(S2))).

set_filter(F, S) ->
    list_to_set([X || X <- set_to_list(S), F(X)]).
