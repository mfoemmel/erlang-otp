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
%% The Initial Developer of the Original Code is Richard Carlsson.
%% Copyright (C) 1999-2001 Richard Carlsson.
%% Portions created by Ericsson are Copyright 2001, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$

%% =====================================================================
%% Core Erlang abstract syntax trees
%%
%% NOTES:
%%
%% This module deals with the composition and decomposition of
%% *syntactic* entities; its purpose is to hide all direct references to
%% the data structures used to represent these entities. With few
%% exceptions, the functions of this module perform no semantic
%% interpretation whatsoever of their arguments, and in general, no type
%% checking is done by these functions - the user is assumed to pass
%% type-correct arguments, and if this is not done, the effects are not
%% defined.
%%
%% *All* internal representations are prone to change without notice,
%% and should not be documented outside this module. *Nor* do we give
%% any guarantees regarding how a syntax tree of any kind may or may not
%% be represented, *with the following exceptions*: no syntax tree is
%% represented by a single atom, such as `none', by a list constructor
%% `[X | Y]', or by the empty list `[]'.
%%
%% META-NOTATION: Source code examples are written within matching
%% single quotes, as in e.g. `foo(X)'. Within code examples, angular
%% brackets ("less than"/"greater than") surround metavariables, as in
%% e.g. `-module(<Name>).'. The characters "less than", "greater than",
%% and single quote characters must be escaped by a backslash, as in
%% `{\'EXIT\', R}'. The notation <`...'> is used to embed arbitrary
%% text, as in e.g. `<Guard> <`->'> <Body>', where the text between
%% `\<\`' and the earliest following `\'\>' is interpreted as an Erlang
%% string that can contain any printable characters, Erlang escape
%% sequences, and whitespace; where one or more consecutive whitespace
%% characters represent a single space character.
%% =====================================================================

-module(cerl).

-export([abstract/1, add_ann/2, alias_pat/1, alias_var/1, ann_c_alias/3,
	 ann_c_apply/3, ann_c_atom/2, ann_c_call/4, ann_c_case/3,
	 ann_c_catch/2, ann_c_char/2, ann_c_clause/3, ann_c_clause/4,
	 ann_c_cons/3, ann_c_float/2, ann_c_fname/3, ann_c_fname/4,
	 ann_c_fun/3, ann_c_int/2, ann_c_let/4, ann_c_letrec/3,
	 ann_c_module/4, ann_c_module/5, ann_c_nil/1,
	 ann_c_nonlit_cons/3, ann_c_nonlit_tuple/2, ann_c_primop/3,
	 ann_c_receive/2, ann_c_receive/4, ann_c_seq/3, ann_c_string/2,
	 ann_c_try/4, ann_c_tuple/2, ann_c_values/2, ann_c_var/2,
	 ann_c_var/3, ann_make_data/3, apply_args/1, apply_arity/1,
	 apply_op/1, atom_lit/1, atom_name/1, atom_val/1, c_alias/2,
	 c_apply/2, c_atom/1, c_call/3, c_case/2, c_catch/1, c_char/1,
	 c_clause/2, c_clause/3, c_cons/2, c_float/1, c_fname/2,
	 c_fname/3, c_fun/2, c_int/1, c_let/3, c_letrec/2, c_module/3,
	 c_module/4, c_nil/0, c_nonlit_cons/2, c_nonlit_tuple/1,
	 c_primop/2, c_receive/1, c_receive/3, c_seq/2, c_string/1,
	 c_try/3, c_tuple/1, c_values/1, c_var/1, c_var/2, call_args/1,
	 call_arity/1, call_module/1, call_name/1, case_arg/1,
	 case_arity/1, case_clauses/1, catch_body/1, char_lit/1,
	 char_val/1, clause_arity/1, clause_body/1, clause_guard/1,
	 clause_pats/1, clause_vars/1, clone_fname/2, clone_fname/3,
	 clone_var/2, concrete/1, cons_hd/1, cons_tl/1, copy_ann/2,
	 data_arity/1, data_es/1, data_type/1, float_lit/1, float_val/1,
	 fname_arity/1, fname_id/1, fold_literal/1, from_records/1,
	 fun_arity/1, fun_body/1, fun_vars/1, get_ann/1, int_lit/1,
	 int_val/1, is_c_alias/1, is_c_apply/1, is_c_atom/1,
	 is_c_atom/2, is_c_call/1, is_c_case/1, is_c_catch/1,
	 is_c_char/1, is_c_char/2, is_c_clause/1, is_c_cons/1,
	 is_c_float/1, is_c_float/2, is_c_fname/1, is_c_fun/1,
	 is_c_int/1, is_c_int/2, is_c_let/1, is_c_letrec/1, is_c_list/1,
	 is_c_module/1, is_c_nil/1, is_c_primop/1, is_c_print_char/1,
	 is_c_print_string/1, is_c_receive/1, is_c_seq/1, is_c_string/1,
	 is_c_try/1, is_c_tuple/1, is_c_values/1, is_c_var/1, is_data/1,
	 is_leaf/1, is_literal/1, is_literal_term/1, let_arg/1,
	 let_arity/1, let_body/1, let_vars/1, letrec_body/1,
	 letrec_defs/1, letrec_vars/1, list_elements/1, list_length/1,
	 make_data/2, make_list/1, make_list/2, make_nonlit_data/2,
	 make_tree/2, meta/1, module_attrs/1, module_defs/1,
	 module_exports/1, module_name/1, module_vars/1,
	 pat_list_vars/1, pat_vars/1, primop_args/1, primop_arity/1,
	 primop_name/1, receive_action/1, receive_clauses/1,
	 receive_timeout/1, seq_arg/1, seq_body/1, set_ann/2,
	 string_lit/1, string_val/1, subtrees/1, to_records/1,
	 try_body/1, try_expr/1, try_vars/1, tuple_arity/1, tuple_es/1,
	 type/1, update_c_alias/3, update_c_apply/3, update_c_call/4,
	 update_c_case/3, update_c_catch/2, update_c_clause/4,
	 update_c_cons/3, update_c_fname/3, update_c_fun/3,
	 update_c_let/4, update_c_letrec/3, update_c_module/5,
	 update_c_primop/3, update_c_receive/4, update_c_seq/3,
	 update_c_try/4, update_c_tuple/2, update_c_values/2,
	 update_c_var/2, update_data/3, values_arity/1, values_es/1,
	 var_class/1, var_name/1]).


%% Binary-syntax support. This is still experimental.

-export([c_binary/1, update_c_binary/2, ann_c_binary/2, is_c_binary/1,
	 binary_segs/1, c_bin_seg/5, update_c_bin_seg/6,
	 ann_c_bin_seg/6, is_c_bin_seg/1, bin_seg_val/1, bin_seg_size/1,
	 bin_seg_unit/1, bin_seg_type/1, bin_seg_flags/1]).

-include("cerl.hrl").


%% =====================================================================
%% Representation (general)
%%
%% All nodes are represented by tuples of arity 2 or (generally)
%% greater, whose first element is an atom which uniquely identifies the
%% type of the node, and whose second element is a (proper) list of
%% annotation terms associated with the node - this is by default empty.
%%
%% For most node constructor functions, there are analogous functions
%% named `ann_...', taking one extra argument `As' (always the first
%% argument), specifying an annotation list at node creation time.
%% Similarly, for many constructors there are also functions named
%% `update_...', taking one extra argument `Old', specifying a node
%% (which should be of the same type as the one being created) from
%% which all fields not explicitly given as arguments should be copied
%% (usually, this is only the annotation field, but for variables it
%% also includes the `Class' field).
%% =====================================================================

%% This defines the general representation of constant literals:

-record(literal, {ann = [], val}).


%% =====================================================================
%% type(Node) -> atom()
%%
%%	    Node = coreErlang()
%%
%%	Returns the type tag of `Node', if `Node' represents a Core
%%	Erlang syntax tree. Current node types are:
%%
%%	    alias
%%	    apply
%%	    call
%%	    case
%%	    catch
%%	    clause
%%	    cons
%%	    fun
%%	    let
%%	    letrec
%%	    literal
%%	    module
%%	    primop
%%	    receive
%%	    seq
%%	    try
%%	    tuple
%%	    values
%%	    var

type(Node) ->
    element(1, Node).


%% =====================================================================
%% is_leaf(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is a leaf node, otherwise `false'.
%%
%%	Note: all literals (cf. `is_literal) are leaf nodes, even if
%%	they represent structured (constant) values such as `{foo, [bar,
%%	baz]}'. Also note that variables are leaf nodes but not
%%	literals.

is_leaf(Node) ->
    case type(Node) of
	literal -> true;
	var -> true;
	_ -> false
    end.


%% =====================================================================
%% get_ann(Node) -> [term()]
%%
%%	    Node = coreErlang()
%%
%%	Returns the list of annotation terms associated with `Node'.
%%
%% set_ann(Node, List) -> Node1
%%
%%	    Node = Node1 = coreErlang()
%%	    List = [term()]
%%
%%	`Node1' is `Node' with its associated list of annotation terms
%%	set to `List'.
%%
%% add_ann(Terms, Node) -> Node1
%%
%%	    Terms = [term()]
%%	    Node = Node1 = coreErlang()
%%
%%	`Node1' is `Node' with `Terms' prefixed to its associated list
%%	of annotation terms. Duplicates are not removed.
%%
%% copy_ann(Source, Target) -> Node
%%
%%	    Source = Target = Node = coreErlang()
%%
%%	`Node' is `Target' with its associated list of annotation terms
%%	set to that of `Source'.
%%
%%	Note: this is equivalent to `set_ann(Target, get_ann(Source))',
%%	but potentially more efficient.

get_ann(Node) ->
    element(2, Node).

set_ann(Node, List) ->
    setelement(2, Node, List).

add_ann(Terms, Node) ->
    set_ann(Node, Terms ++ get_ann(Node)).

copy_ann(Source, Target) ->
    set_ann(Target, get_ann(Source)).


%% =====================================================================
%% abstract(Term) -> coreErlang()
%%
%%	    Term = term()
%%
%%	Returns an abstract syntax tree representation of `Term', which
%%	must be a literal term, i.e., which can be represented as a
%%	source code constant. Thus, `Term' may not contain a PID, port,
%%	reference ("ref"), binary or function value as a subterm.
%%
%%	Note: This function executes in constant time.
%%
%% is_literal_term(Term) -> bool()
%%
%%	    Term = term()
%%
%%	Returns `true' if `Term' can be represented as a literal,
%%	otherwise `false'. This function takes time linear in proportion
%%	to the size of `Term'.

abstract(T) ->
    #literal{val = T}.

is_literal_term(T) when integer(T) -> true;
is_literal_term(T) when float(T) -> true;
is_literal_term(T) when atom(T) -> true;
is_literal_term([]) -> true;
is_literal_term([H | T]) ->
    case is_literal_term(H) of
	true ->
	    is_literal_term(T);
	false ->
	    false
    end;
is_literal_term(T) when tuple(T) ->
    is_literal_term_list(tuple_to_list(T));
is_literal_term(T) ->
    false.

is_literal_term_list([T | Ts]) ->
    case is_literal_term(T) of
	true ->
	    is_literal_term_list(Ts);
	false ->
	    false
    end;
is_literal_term_list([]) ->
    true.


%% =====================================================================
%% concrete(Node) -> term()
%%
%%	    Node = coreErlang()
%%
%%	Returns the concrete term represented by `Node', if `Node'
%%	represents a literal. An exception is thrown if `Node' does not
%%	represent a literal term.
%%
%%	Note: This function executes in constant time.
%%
%% is_literal(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' whenever `Node' represents a literal, otherwise
%%	`false'.
%%
%%	Note: This function executes in constant time.
%%
%% fold_literal(Node) -> Node1
%%
%%	    Node = Node1 = coreErlang()
%%
%%	This is occasionally useful if `c_nonlit_cons' or
%%	`c_nonlit_tuple' were used to construct `Node', and you want to
%%	revert to the normal "folded" representation of literals. If
%%	`Node' represents a tuple or list constructor, its elements are
%%	rewritten recursively, and the node is reconstructed using
%%	`c_cons' or `c_tuple', respectively; otherwise, `Node' is not
%%	changed.

%% Because the normal tuple and list constructor operations always
%% return a literal if the arguments are literals, `concrete' and
%% `is_literal' never need to traverse the structure.

concrete(#literal{val = V}) ->
    V.

is_literal(#literal{}) ->
    true;
is_literal(T) ->
    false.

fold_literal(Node) ->
    case type(Node) of
	tuple ->
	    update_c_tuple(Node, fold_literal_list(tuple_es(Node)));
	cons ->
	    update_c_cons(Node, fold_literal(cons_hd(Node)),
			  fold_literal(cons_tl(Node)));
	Node ->
	    Node    
    end.

fold_literal_list([E | Es]) ->
    [fold_literal(E) | fold_literal_list(Es)];
fold_literal_list([]) ->
    [].


%% =====================================================================
%% c_module(Name, Exports, Definitions) -> Node
%% c_module(Name, Exports, Attributes, Definitions) -> Node
%% ann_c_module(As, Name, Exports, Definitions) -> Node
%% ann_c_module(As, Exports, Attributes, Definitions) -> Node
%% update_c_module(Old, Name, Exports, Attributes, Definitions) -> Node
%%
%%	    Name = Node = coreErlang()
%%	    Exports = [Var]
%%	    Var = coreErlang()
%%	    Attributes = [{Atom, Term}]
%%	    Atom = Term = coreErlang()
%%	    Definitions = [{Var, Fun}]
%%	    Fun = coreErlang()
%%	    type(Node) = module
%%
%%	`Node' is an abstract module definition, representing:
%%
%%	    `module <Name> [<E1>, ..., <Ek>]
%%	       attributes [<K1> = <T1>, ..., <Km> = <Tm>]
%%	       <V1> = <F1>
%%	       ...
%%	       <Vn> = <Fn>
%%	     end'
%%
%%	if `Exports' is `[E1, ..., Ek]', `Attributes' is `[{K1, T1},
%%	..., {Km, Tm}]', and `Definitions' = `[{V1, F1}, ..., {Vn,
%%	Fn}]'. Leaving out `Attributes' is equivalent to passing the
%%	empty list.
%%
%% is_c_module(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract module definition;
%%	otherwise `false'.
%%
%% module_name(Node) -> Name
%%
%%	    Node = Name = coreErlang()
%%	    is_c_module(Node)
%%
%%	Returns the `Name' subtree of the abstract module definition
%%	`Node'.
%%
%% module_exports(Node) -> Exports
%%
%%	    Node = coreErlang()
%%	    Exports = [Var]
%%	    Var = coreErlang()
%%	    is_c_module(Node)
%%
%%	Returns the list `[E1, ..., Ek]', if `Node' represents `module
%%	<Name> [<E1>, ..., <Ek>] ... end'.
%%
%% module_attrs(Node) -> Attributes
%%
%%	    Node = coreErlang()
%%	    Attributes = [{Atom, Term}]
%%	    Atom = Term = coreErlang()
%%	    is_c_module(Node)
%%
%%	Returns the list `[{K1, T1}, ..., {Km, Tm}]', if `Node'
%%	represents `module <Name> [...] attributes [<K1> = <T1>, ...,
%%	<Km> = <Tm>] ... end'.
%%
%% module_defs(Node) -> Definitions
%%
%%	    Node = coreErlang()
%%	    Definitions = [{Var, Fun}]
%%	    Var = Fun = coreErlang()
%%	    is_c_module(Node)
%%
%%	Returns the list `[{V1, F1}, ..., {Vn, Fn}]', if `Node'
%%	represents `module <Name> [...] attributes [...] <V1> = <F1> ...
%%	<Vn> = <Fn> end'.
%%
%% module_vars(Node) -> Variables
%%
%%	    Node = coreErlang()
%%	    Variables = [Var]
%%	    Var = coreErlang()
%%	    is_c_module(Node)
%%
%%	Returns the list `[V1, ..., Vn]', if `Node' represents `module
%%	<Name> [...] attributes [...] <V1> = <F1> ... <Vn> = <Fn> end'.

%% Representation:
%%
%% {module, A, Name, Exports, Attributes, Definitions}
%%
%%	Node = Name = coreErlang()
%%	Exports = [Var]
%%	Var = coreErlang()
%%	Attributes = [{Atom, Term}]
%%	Atom = Term = coreErlang()
%%	Definitions = [{Var, Fun}]
%%	Fun = coreErlang()

-record(module, {ann = [], name, exports, attrs, defs}).

c_module(Name, Exports, Es) ->
    #module{name = Name, exports = Exports, attrs = [], defs = Es}.

c_module(Name, Exports, Attrs, Es) ->
    #module{name = Name, exports = Exports, attrs = Attrs, defs = Es}.

ann_c_module(As, Name, Exports, Es) ->
    #module{name = Name, exports = Exports, attrs = [], defs = Es,
	    ann = As}.

ann_c_module(As, Name, Exports, Attrs, Es) ->
    #module{name = Name, exports = Exports, attrs = Attrs, defs = Es,
	    ann = As}.

update_c_module(Node, Name, Exports, Attrs, Es) ->
    #module{name = Name, exports = Exports, attrs = Attrs, defs = Es,
	    ann = get_ann(Node)}.

is_c_module(#module{}) ->
    true;
is_c_module(_) ->
    false.

module_name(Node) ->
    Node#module.name.

module_exports(Node) ->
    Node#module.exports.

module_attrs(Node) ->
    Node#module.attrs.

module_defs(Node) ->
    Node#module.defs.

module_vars(Node) ->
    [F || {F, _} <- module_defs(Node)].


%% =====================================================================
%% c_int(Value) -> Node
%% ann_c_int(As, Value) -> Node
%%
%%	    Value = integer()
%%	    Node = coreErlang()
%%	    type(Node) = literal
%%	    is_c_int(Node, Value)
%%
%%	`Node' is an abstract integer literal, representing the
%%	canonical decimal numeral of `Value', as given by
%%	`erlang:integer_to_list/1'.
%%
%% is_c_int(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract integer literal,
%%	otherwise `false'.
%%
%% is_c_int(Node, Value) -> bool()
%%
%%	    Node = coreErlang()
%%	    Value = integer()
%%
%%	Returns `true' if `Node' is an abstract integer literal such
%%	that `int_val(Node)' is equal to `Value', otherwise `false'.
%%
%% int_val(Node) -> integer()
%%
%%	    Node = coreErlang()
%%	    is_c_int(Node)
%%
%%	Returns the value of the integer literal represented by `Node'.
%%
%% int_lit(Node) -> string()
%%
%%	    Node = coreErlang()
%%	    is_c_int(Node)
%%
%%	Returns the character sequence of the integer numeral
%%	represented by `Node'.

%% Representation:
%%
%% {literal, A, Value}
%%
%%	Value = integer()
%%
%% Note: if the underlying implementation of Erlang does not have
%% "bignums", then not all integer numerals can be represented.

c_int(Value) ->
    #literal{val = Value}.

ann_c_int(As, Value) ->
    #literal{val = Value, ann = As}.

is_c_int(#literal{val = V}) when integer(V) ->
    true;
is_c_int(_) ->
    false.

is_c_int(#literal{val = V}, V1) when integer(V), V =:= V1 ->
    true;
is_c_int(_, _) ->
    false.

int_val(Node) ->
    Node#literal.val.

int_lit(Node) ->
    integer_to_list(int_val(Node)).


%% =====================================================================
%% c_float(Value) -> Node
%% ann_c_float(As, Value) -> Node
%%
%%	    Value = float()
%%	    Node = coreErlang()
%%	    type(Node) = literal
%%	    is_c_float(Node, Value)
%%
%%	`Node' is an abstract floating-point literal, representing the
%%	decimal floating-point numeral of `Value' as given by
%%	`erlang:float_to_list/1'.
%%
%% is_c_float(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract floating-point numeral;
%%	otherwise `false'.
%%	
%% is_c_float(Node, Value) -> bool()
%%
%%	    Node = coreErlang()
%%	    Value = float()
%%
%%	Returns `true' if `Node' is an abstract floating-point literal
%%	such that `float_val(Node)' is equal to `Value', otherwise
%%	`false'.
%%
%% float_val(Node) -> float()
%%
%%	    Node = coreErlang()
%%	    is_c_float(Node)
%%
%%	Returns the value of the floating-point literal represented by
%%	`Node'.
%%
%% float_lit(Node) -> string()
%%
%%	    Node = coreErlang()
%%	    is_c_float(Node)
%%
%%	Returns the character sequence of the floating-point numeral
%%	represented by `Node'.

%% Representation:
%%
%% {literal, A, Value}
%%
%%	Value = float()
%%
%% Note that not all floating-point numerals can be represented with
%% full precision.

c_float(Value) ->
    #literal{val = Value}.

ann_c_float(As, Value) ->
    #literal{val = Value, ann = As}.

is_c_float(#literal{val = V}) when float(V) ->
    true;
is_c_float(_) ->
    false.

is_c_float(#literal{val = V}, V1) when float(V), V =:= V1 ->
    true;
is_c_float(_, _) ->
    false.

float_val(Node) ->
    Node#literal.val.

float_lit(Node) ->
    float_to_list(float_val(Node)).


%% =====================================================================
%% c_atom(Name) -> Node
%% ann_c_atom(As, Name) -> Node
%%
%%	    Name = atom() | string()
%%	    Node = coreErlang()
%%	    type(Node) = literal
%%	    is_c_atom(Node, Name)
%%
%%	`Node' is an abstract atom literal corresponding to `Name'. If
%%	`Name' is an atom, `Node' simply represents the corresponding
%%	atom literal (always written within single-quotes). If `Name' is
%%	a string, `Node' represents exactly the atom literal formed by
%%	enclosing the character sequence of `Name' within single-quote
%%	characters.
%%
%%	Note: in order to make comparison of abstract atoms a quick
%%	operation, the atom names are internally represented as actual
%%	atoms. This implies that passing a string as `Name' argument to
%%	this constructor function causes a corresponding actual atom to
%%	be created.
%%
%% is_c_atom(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract atom literal, otherwise
%%	`false'.
%%	
%% is_c_atom(Node, Value) -> bool()
%%
%%	    Node = coreErlang()
%%	    Value = atom()
%%
%%	Returns `true' if `Node' is an abstract atom literal such that
%%	`atom_val(Node)' is equal to `Value', otherwise `false'.
%%
%% atom_val(Node)-> atom()
%%
%%	    Node = coreErlang()
%%	    is_c_atom(Node)
%%
%%	Returns the actual atom corresponding to the atom literal
%%	represented by `Node'.
%%
%% atom_name(Node) -> string()
%%
%%	    Node = coreErlang()
%%	    is_c_atom(Node)
%%
%%	Returns the printname (not including the surrounding
%%	single-quote characters) corresponding to the atom literal
%%	represented by `Node'.
%%
%% atom_lit(Node) -> string()
%%
%%	    Node = coreErlang()
%%	    is_c_atom(Node)
%%
%%	Returns the character sequence of the atom literal corresponding
%%	to `Node' (always including surrounding single-quote
%%	characters). E.g., `atom_lit(c_atom("a\012b"))' might yield
%%	`"\'a\\nb\'"'. Note that one abstract atom may have several
%%	literal representations, and that the representation yielded by
%%	this function is not fixed.

%% Representation:
%%
%% {literal, A, Name}
%%
%%	Name = atom()

c_atom(Name) when atom(Name) ->
    #literal{val = Name};
c_atom(Name) ->
    #literal{val = list_to_atom(Name)}.

ann_c_atom(As, Name) when atom(Name) ->
    #literal{val = Name, ann = As};
ann_c_atom(As, Name) ->
    #literal{val = list_to_atom(Name), ann = As}.

is_c_atom(#literal{val = V}) when atom(V) ->
    true;
is_c_atom(_) ->
    false.

is_c_atom(#literal{val = V}, V1) when atom(V), V =:= V1 ->
    true;
is_c_atom(_, _) ->
    false.

atom_val(Node) ->
    Node#literal.val.

atom_name(Node) ->
    atom_to_list(atom_val(Node)).

%% TODO: replace the use of the unofficial `write_string/2'.

atom_lit(Node) ->
    io_lib:write_string(atom_name(Node), $').


%% =====================================================================
%% c_char(Char) -> Node
%% ann_c_char(As, Char) -> Node
%%
%%	    Char = char() | integer()
%%	    Node = coreErlang()
%%	    type(Node) = literal
%%	    is_c_char(Node, Char)
%%
%%	`Node' is an abstract character literal corresponding to `Char'.
%%
%%	Note: the literal corresponding to a particular character value
%%	is not uniquely defined. E.g., the character `a' can be written
%%	both as `$a' and `$\141', and a Tab character can be written as
%%	`$\11' or `$\t', etc.
%%
%% is_c_char(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' may represent a character literal;
%%	otherwise `false'. If the Erlang implementation does not
%%	distinguish between integers and characters, then
%%	`is_c_int(<Node>)' will also yield `true', and in this case
%%	`is_c_char(<Node>)' will yield `true' if `int_val(<Node>)' is
%%	an integer in the proper range for representing a character.
%%	
%% is_c_char(Node, Value) -> bool()
%%
%%	    Node = coreErlang()
%%	    Value = char()
%%
%%	Returns `true' if `is_c_char(Node)' yields `true' and
%%	`char_val(Node)' is equal to `Value', otherwise `false'.
%%
%% is_c_print_char(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' whenever `is_c_char(Node)' yields `true' and
%%	`concrete(Node)' is a "printing" character value, i.e., that
%%	denotes a character with either a given graphical representation
%%	or a "named" escape sequence such as `\n' or '\\'. Currently,
%%	ISO 8859-1 (Latin-1) character values are being recognized.
%%	
%% char_val(Node) -> char()
%%
%%	    Node = coreErlang()
%%	    is_c_char(Node)
%%
%%	Returns the value corresponding to the character literal
%%	represented by `Node'.
%%
%% char_lit(Node) -> string()
%%
%%	    Node = coreErlang()
%%	    is_c_char(Node)
%%
%%	Returns the character sequence of the character literal
%%	corresponding to `Node' (including a leading `$' character).

%% Representation:
%%
%% {literal, A, Code}
%%
%%	Code = char()
%%
%% representing a character literal corresponding to `Code', if `Code'
%% has type char() but not type integer(), or the character whose code
%% is given by `Code', if `Code' has type integer().

c_char(Char) ->
    #literal{val = Char}.

ann_c_char(As, Char) ->
    #literal{val = Char, ann = As}.

is_c_char(#literal{val = V}) when integer(V) ->
    is_char_value(V);
is_c_char(_) ->
    false.

is_c_char(#literal{val = V}, V1) when integer(V), V =:= V1 ->
    is_char_value(V);
is_c_char(_, _) ->
    false.

is_c_print_char(#literal{val = V}) when integer(V) ->
    is_print_char_value(V);
is_c_print_char(_) ->
    false.

char_val(Node) ->
    Node#literal.val.

char_lit(Node) ->
    io_lib:write_char(char_val(Node)).


%% =====================================================================
%% c_string(String) -> Node
%% ann_c_string(As, String) -> Node
%%
%%	    String = string()
%%	    Node = coreErlang()
%%	    type(Node) = literal
%%
%%	`Node' is an abstract string literal corresponding to the
%%	sequence of characters in `String', but not representing a
%%	*specific* string literal. E.g., `c_string("x\ny")' represents
%%	any and all of `"x\ny"', `"x\12y"', `"x\012y"' and `"x\^Jy"'.
%%	(Cf. `char'.)
%%
%% is_c_string(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' represents either `[]' or `[<Head> |
%%	<Tail>]', where in the latter case `is_c_char(Head)' yields
%%	`true' and recursively `is_c_string(Tail)' also yields `true'.
%%	Returns `false' otherwise.
%%
%% is_c_print_string(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' whenever `is_c_string(Node)' yields `true' and
%%	`concrete(Node)' is a sequence of "printing" character values,
%%	i.e., that denote characters with either a given graphical
%%	representation or a "named" escape sequence such as `\n' or
%%	'\\'. Currently, ISO 8859-1 (Latin-1) character values are being
%%	recognized. (Cf. `is_c_print_char'.)
%%
%% string_val(Node) -> string()
%%
%%	    Node = coreErlang()
%%	    is_c_string(Node)
%%
%%	Returns the value of the string literal represented by `Node'.
%%
%% string_lit(Node) -> string()
%%
%%	    Node = coreErlang()
%%	    is_c_string(Node)
%%
%%	Returns the character sequence of the literal represented by
%%	`Node'; this includes surrounding double-quote characters.

%% Representation:
%%
%% {literal, A, Value}
%%
%%	Value = string()

c_string(Chars) ->
    #literal{val = Chars}.

ann_c_string(As, Chars) ->
    #literal{val = Chars, ann = As}.

%% Note that strings must be literals, not abstract cons cells.

is_c_string(#literal{val = V}) ->
    is_char_list(V);
is_c_string(_) ->
    false.

is_c_print_string(#literal{val = V}) ->
    is_print_char_list(V);
is_c_print_string(_) ->
    false.

string_val(Node) ->
    concrete(Node).

string_lit(Node) ->
    io_lib:write_string(string_val(Node)).


%% =====================================================================
%% c_nil() -> Node
%% ann_c_nil(As) -> Node
%%
%%	    Node = coreErlang()
%%	    type(Node) = literal
%%
%%	`Node' is an abstract empty list (the empty list is
%%	traditionally called "nil"), representing `[]'.
%%
%% is_c_nil(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract empty list, otherwise
%%	`false'.

%% Representation:
%%
%% {literal, A, []}

c_nil() ->
    #literal{val = []}.

ann_c_nil(As) ->
    #literal{val = [], ann = As}.

is_c_nil(#literal{val = []}) ->
    true;
is_c_nil(_) ->
    false.


%% =====================================================================
%% Binary-syntax support (still experimental)

-record(binary, {ann = [], segs}).

c_binary(Segments) ->
    #binary{segs = Segments}.

ann_c_binary(As, Segments) ->
    #binary{segs = Segments, ann = As}.

update_c_binary(Node, Segments) ->
    #binary{segs = Segments, ann = get_ann(Node)}.

is_c_binary(#binary{}) ->
    true;
is_c_binary(_) ->
    false.

binary_segs(Node) ->
    Node#binary.segs.


-record(bin_seg, {ann = [], val, size, unit, type, flags}).

c_bin_seg(Val, Size, Unit, Type, Flags) ->
    #bin_seg{val = Val, size = Size, unit = Unit, type = Type,
	     flags = Flags}.

ann_c_bin_seg(As, Val, Size, Unit, Type, Flags) ->
    #bin_seg{val = Val, size = Size, unit = Unit, type = Type,
	     flags = Flags, ann = As}.

update_c_bin_seg(Node, Val, Size, Unit, Type, Flags) ->
    #bin_seg{val = Val, size = Size, unit = Unit, type = Type,
	     flags = Flags, ann = get_ann(Node)}.

is_c_bin_seg(#bin_seg{}) ->
    true;
is_c_bin_seg(_) ->
    false.

bin_seg_val(Node) ->
    Node#bin_seg.val.

bin_seg_size(Node) ->
    Node#bin_seg.size.

bin_seg_unit(Node) ->
    Node#bin_seg.unit.

bin_seg_type(Node) ->
    Node#bin_seg.type.

bin_seg_flags(Node) ->
    Node#bin_seg.flags.


%% =====================================================================
%% c_cons(Head, Tail) -> Node
%% ann_c_cons(As, Head, Tail) -> Node
%% update_c_cons(Old, Head, Tail) -> Node
%%
%%	    Head = Tail = Node = coreErlang()
%%	    type(Node) = cons | literal
%%
%%	`Node' is an abstract list constructor representing `[<Head> |
%%	<Tail>]'. Recall that in Erlang, the tail element of a list
%%	constructor is not necessarily a list. Note that if both `Head'
%%	and `Tail' have type `literal', then `Node' will also have type
%%	`literal', and all annotations on `Head' and `Tail' are lost.
%%
%% c_nonlit_cons(Head, Tail) -> Node
%% ann_c_nonlit_cons(As, Head, Tail) -> Node
%%
%%	    Head = Tail = Node = coreErlang()
%%	    type(Node) = cons
%%
%%	This function is occasionally useful when it is necessary to
%%	keep annotations on the subnodes of a list constructor node,
%%	even if both subnodes are constant literals. `Node' is an
%%	abstract list constructor representing `[<Head> | <Tail>]'. Note
%%	however that `Node' will always have type `cons',
%%	`is_literal(Node)' will yield `false' and `concrete(Node)' will
%%	fail.
%%
%% is_c_cons(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract list constructor;
%%	otherwise `false'.
%%
%% cons_hd(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_cons(Node)
%%
%%	Returns the `Head' subtree of the abstract list constructor
%%	`Node'.
%%
%% cons_tl(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_cons(Node)
%%
%%	Returns the `Tail' subtree of the abstract list constructor
%%	`Node'. (Recall that the tail does not necessarily represent a
%%	proper list.)
%%
%% is_c_list(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' represents either `[]' or `[<Head> |
%%	<Tail>]', where in the latter case recursively
%%	`is_c_list(Tail)' = `true'. Returns `false' otherwise.
%%
%%	Note: Because `Node' is a syntax tree, the actual runtime values
%%	represented by its subtrees are often partially or completely
%%	unknown. Thus, if `Node' represents e.g. `[N | Ns]' then
%%	`is_c_list(Node)' yields `false', because `Ns' is a variable,
%%	which might not be bound to a list. If `Node' represents a
%%	nil-terminated list such as `[foo, bar]' or `[X, Y]', then
%%	`is_c_list(Node)' can safely yield `true'.
%%
%% list_elements(Node) -> [coreErlang()]
%%
%%	    Node = coreErlang()
%%	    is_c_list(Node)
%%
%%	Returns the list of "list element" subtrees, in left-to-right
%%	order, of the (proper) list represented by `Node'. E.g., if
%%	`Node' represents `[<X1> | [<X2> | [<X3> | []]]]', then
%%	`list_elements(Node)' yields the list `[X1, X2, X3]'.
%%
%% list_length(Node) -> integer()
%%
%%	    Node = coreErlang()
%%	    is_c_list(Node)
%%
%%	Returns the number of "list element" subtrees of the (proper)
%%	list represented by `Node'. E.g., if `Node' represents `[X1 |
%%	[X2 | [X3 | [X4, X5, X6]]]]', then `list_length(Node)' yields
%%	the integer 6.
%%
%% make_list(List) -> Node
%% make_list(List, Tail) -> Node
%%
%%	    List = [coreErlang()]
%%	    Tail = coreErlang() | none
%%	    Node = coreErlang()
%%
%%	Returns a syntax tree representing the list whose prefix
%%	consists of the syntax trees in `List' (in order), and whose
%%	tail is represented by `Tail'. If `Tail' is `none', the
%%	resulting abstract list will be proper (nil-terminated). Leaving
%%	out `Tail' is equivalent to passing `none'.

%% Representation:
%%
%% {cons, A, Head, Tail}
%%
%%	Head = Tail = coreErlang()

-record(cons, {ann = [], hd, tl}).

%% *Always* collapse literals.

c_cons(#literal{val = Head}, #literal{val = Tail}) ->
    #literal{val = [Head | Tail]};
c_cons(Head, Tail) ->
    #cons{hd = Head, tl = Tail}.

ann_c_cons(As, #literal{val = Head}, #literal{val = Tail}) ->
    #literal{val = [Head | Tail], ann = As};
ann_c_cons(As, Head, Tail) ->
    #cons{hd = Head, tl = Tail, ann = As}.

update_c_cons(Node, #literal{val = Head}, #literal{val = Tail}) ->
    #literal{val = [Head | Tail], ann = get_ann(Node)};
update_c_cons(Node, Head, Tail) ->
    #cons{hd = Head, tl = Tail, ann = get_ann(Node)}.

%% *Never* collapse literals.

c_nonlit_cons(Head, Tail) ->
    #cons{hd = Head, tl = Tail}.

ann_c_nonlit_cons(As, Head, Tail) ->
    #cons{hd = Head, tl = Tail, ann = As}.

is_c_cons(#cons{}) ->
    true;
is_c_cons(#literal{val = [_ | _]}) ->
    true;
is_c_cons(_) ->
    false.

cons_hd(#cons{hd = Head}) ->
    Head;
cons_hd(#literal{val = [Head | _]}) ->
    #literal{val = Head}.

cons_tl(#cons{tl = Tail}) ->
    Tail;
cons_tl(#literal{val = [_ | Tail]}) ->
    #literal{val = Tail}.

is_c_list(#cons{tl = Tail}) ->
    is_c_list(Tail);
is_c_list(#literal{val = V}) ->
    is_proper_list(V);
is_c_list(_) ->
    false.

is_proper_list([_ | Tail]) ->
    is_proper_list(Tail);
is_proper_list([]) ->
    true;
is_proper_list(_) ->
    false.

list_elements(#cons{hd = Head, tl = Tail}) ->
    [Head | list_elements(Tail)];
list_elements(#literal{val = V}) ->
    abstract_list(V).

abstract_list([X | Xs]) ->
    [abstract(X) | abstract_list(Xs)];
abstract_list([]) ->
    [].

list_length(L) ->
    list_length(L, 0).

list_length(#cons{hd = Head, tl = Tail}, A) ->
    list_length(Tail, A + 1);
list_length(#literal{val = V}, A) ->
    A + length(V).

make_list(List) ->
    make_list(List, none).

make_list([H | T], Tail) ->
    c_cons(H, make_list(T, Tail));    % `c_cons' folds literals
make_list([], none) ->
    c_nil();
make_list([], Node) ->
    Node.


%% =====================================================================
%% c_tuple(List) -> Node
%% ann_c_tuple(As, List) -> Node
%% update_c_tuple(Old, List) -> Node
%%
%%	    List = [coreErlang()]
%%	    Node = coreErlang()
%%	    type(Node) = tuple | literal
%%
%%	`Node' is an abstract tuple representing `{<E1>, ..., <En>}', if
%%	`List' is `[E1, ..., En]'. Note that if all elements in `List'
%%	have type `literal', or if `List' is empty, then `Node' will
%%	also have type `literal' and all annotations on elements in
%%	`List' are lost.
%%
%% c_nonlit_tuple(List) -> Node
%% ann_c_nonlit_tuple(List) -> Node
%%
%%	    List = [coreErlang()]
%%	    Node = coreErlang()
%%	    type(Node) = tuple
%%
%%	This function is occasionally useful when it is necessary to
%%	keep annotations on the subnodes of a tuple node, even if all
%%	the subnodes are constant literals. `Node' is an abstract tuple
%%	representing `{<E1>, ..., <En>}', if `List' is `[E1, ..., En]'.
%%	Note however that `Node' will always have type `tuple',
%%	`is_literal(Node)' will yield `false' and `concrete(Node)' will
%%	fail.
%%
%% is_c_tuple(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract tuple; otherwise
%%	`false'.
%%
%% tuple_es(Node) -> [coreErlang()]
%%
%%	    Node = coreErlang()
%%	    is_c_tuple(Node)
%%
%%	Returns the list `[E1, ..., En]' if `Node' represents `{<E1>,
%%	..., <En>}'.
%%
%% tuple_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_tuple(Node)
%%
%%	Returns the number `n', if `Node' represents `{<E1>, ...,
%%	<En>}'.
%%
%%	Note: this is equivalent to `erlang:length(tuple_es(Node))', but
%%	potentially more efficient.

%% Representation:
%%
%% {tuple, A, List}
%%
%%	List = [coreErlang()]

-record(tuple, {ann = [], es}).

%% *Always* collapse literals.

c_tuple(Es) ->
    case is_lit_list(Es) of
	false ->
	    #tuple{es = Es};
	true ->
	    #literal{val = list_to_tuple(lit_list_vals(Es))}
    end.

ann_c_tuple(As, Es) ->
    case is_lit_list(Es) of
	false ->
	    #tuple{es = Es, ann = As};
	true ->
	    #literal{val = list_to_tuple(lit_list_vals(Es)), ann = As}
    end.

update_c_tuple(Node, Es) ->
    case is_lit_list(Es) of
	false ->
	    #tuple{es = Es, ann = get_ann(Node)};
	true ->
	    #literal{val = list_to_tuple(lit_list_vals(Es)),
		     ann = get_ann(Node)}
    end.

%% *Never* collapse literals.

c_nonlit_tuple(Es) ->
    #tuple{es = Es}.

ann_c_nonlit_tuple(As, Es) ->
    #tuple{es = Es, ann = As}.

is_c_tuple(#tuple{}) ->
    true;
is_c_tuple(#literal{val = V}) when tuple(V) ->
    true;
is_c_tuple(_) ->
    false.

tuple_es(#tuple{es = Es}) ->
    Es;
tuple_es(#literal{val = V}) ->
    make_lit_list(tuple_to_list(V)).

tuple_arity(#tuple{es = Es}) ->
    length(Es);
tuple_arity(#literal{val = V}) when tuple(V) ->
    size(V).


%% =====================================================================
%% c_var(Name) -> Node
%% c_var(Name, Class) -> Node
%% ann_c_var(As, Name) -> Node
%% ann_c_var(As, Name, Class) -> Node
%% update_c_var(Old, Name) -> Node
%%
%%	    Name = Class = integer() | atom() | {atom(), integer()}
%%	    Node = coreErlang()
%%	    type(Node) = var
%%	    is_c_var(Node)
%%
%%	`Node' is an abstract variable whose name is given by `Name'. In
%%	addition, each variable has a class field, which can be
%%	specified with the optional `Class' parameter. If `Class' is
%%	left out, the class field will be set to `Name'. If a name is
%%	given as a single atom, it should either be a "simple" atom
%%	which does not need to be single-quoted in Erlang, or otherwise
%%	its print name should correspond to a proper Erlang variable,
%%	i.e., begin with an uppercase character or an underscore. Names
%%	`{A, N}' represent function names; these are special variables
%%	which may be bound in the function definitions of a module or
%%	`letrec', and may be listed in the exports of a module. They may
%%	not be bound in `let' expressions and cannot occur in patterns.
%%	The atom `A' in a function name may be any atom; the integer `N'
%%	must be nonnegative.
%%
%%	The intention behind the class field is to preserve information
%%	about the "original variable name", even when the variable has
%%	been renamed. The class can be inherited from one variable to
%%	another by using the `clone_var' function below.
%%
%%	When printing variable names, they must have the form of proper
%%	Core Erlang variables and function names. E.g., an integer `42'
%%	could be formatted as `_42', an atom `'Xxx'' simply as `Xxx' and
%%	an atom `foo' as `_foo'. However, one must assure that any two
%%	valid distinct names are never mapped to the same strings.
%%	Tuples such as `{foo, 2}' are simply formatted as `'foo'/2',
%%	with no risk of conflicts.
%%
%% c_fname(Atom, Arity) -> Node
%% c_fname(Atom, Arity, Class) -> Node
%% ann_c_fname(As, Atom, Arity) -> Node
%% ann_c_fname(As, Atom, Arity, Class) -> Node
%% update_c_fname(Old, Atom, Arity) -> Node
%%
%%	    Atom = atom()
%%	    Arity = integer()
%%	    Class = Identifier | {Identifier, integer()}
%%	    Identifier = string() | atom() 
%%	    Node = coreErlang()
%%	    type(Node) = var
%%	    is_c_var(Node)
%%	    is_c_fname(Node)
%%
%% is_c_var(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract variable, otherwise
%%	`false'.
%%
%% is_c_fname(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract variable representing a
%%	function name, otherwise `false'.
%%
%% var_name(Node) -> Name
%%
%%	    Node = coreErlang()
%%	    Name = integer() | atom() | {atom(), integer()}
%%	    is_c_var(Node)
%%
%%	Returns the name of the abstract variable `Node'.
%%
%% var_class(Node) -> Class
%%
%%	    Node = coreErlang()
%%	    Class = Identifier | {Identifier, Arity}
%%	    Identifier = string() | atom() 
%%	    Arity = integer()
%%	    is_c_var(Node)
%%
%%	Returns the class field of the abstract variable `Node'.
%%
%% fname_id(Node) -> Atom
%%
%%	    Node = coreErlang()
%%	    Atom = atom()
%%	    is_c_fname(Node)
%%
%%	Returns the identifier field of the abstract variable `Node', if
%%	it represents a function name.
%%
%% fname_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_fname(Node)
%%
%%	Returns the arity field of the abstract variable `Node', if it
%%	represents a function name.
%%
%% clone_var(Node, Name) -> Node1
%%
%%	    Node = Node1 = coreErlang()
%%	    Name = integer() | atom() | {atom(), integer()}
%%	    is_c_var(Node)
%%	    is_c_var(Node1)
%%
%%	`Node1' is an abstract variable with the name given by `Name'
%%	but with the same class field as `Node'.
%%
%%	Note: Annotations are *not* copied from `Node' to `Node1'. Use
%%	`update_c_var' for this.
%%
%% clone_fname(Node, Atom) -> Node1
%% clone_fname(Node, Atom, Arity) -> Node1
%%
%%	    Node = Node1 = coreErlang()
%%	    Name = integer() | atom() | {atom(), integer()}
%%	    is_c_fname(Node)
%%	    is_c_fname(Node1)
%%
%%	`Node1' is an abstract variable with the name `{Atom, Arity}'
%%	but with the same class field as `Node'. If `Arity' is left out,
%%	the arity of `Node' is used.
%%
%%	Note: Annotations are *not* copied from `Node' to `Node1'. Use
%%	`update_c_fname' for this.

%% Representation:
%%
%% {var, A, Name, Class}
%%
%%	Name = integer() | atom() | {atom(), integer()}
%%	Class = Identifier | {Identifier, integer()}
%%	Identifier = string() | atom() 

-record(var, {ann = [], name, class}).

c_var(Name) ->
    #var{name = Name, class = Name}.

c_var(Name, Class) ->
    #var{name = Name, class = Class}.

ann_c_var(As, Name) ->
    #var{name = Name, class = Name, ann = As}.

ann_c_var(As, Name, Class) ->
    #var{name = Name, class = Class, ann = As}.

update_c_var(Node, Name) ->
    #var{name = Name, class = var_class(Node), ann = get_ann(Node)}.

is_c_var(#var{}) ->
    true;
is_c_var(_) ->
    false.

c_fname(Atom, Arity) ->
    c_fname(Atom, Arity, {Atom, Arity}).

c_fname(Atom, Arity, Class) ->
    #var{name = {Atom, Arity}, class = Class}.

ann_c_fname(As, Atom, Arity) ->
    ann_c_fname(As, Atom, Arity, {Atom, Arity}).

ann_c_fname(As, Atom, Arity, Class) ->
    #var{name = {Atom, Arity}, class = Class, ann = As}.

update_c_fname(Node, Atom, Arity) ->
    #var{name = {Atom, Arity}, class = var_class(Node),
	 ann = get_ann(Node)}.

is_c_fname(#var{name = {A, N}}) when atom(A), integer(N), N >= 0 ->
    true;
is_c_fname(_) ->
    false.

var_name(Node) ->
    Node#var.name.

var_class(Node) ->
    Node#var.class.

fname_id(#var{name={A,_}}) ->
    A.

fname_arity(#var{name={_,N}}) ->
    N.

clone_var(#var{class = Class}, Name) ->
    #var{name = Name, class = Class}.

clone_fname(#var{name = {_, _}, class = Class}, Atom, Arity) ->
    #var{name = {Atom, Arity}, class = Class}.

clone_fname(#var{name = {_, Arity}, class = Class}, Atom) ->
    #var{name = {Atom, Arity}, class = Class}.


%% =====================================================================
%% c_values(List) -> Node
%% ann_c_values(As, List) -> Node
%% update_c_values(Old, List) -> Node
%%
%%	    List = [coreErlang()]
%%	    Node = coreErlang()
%%	    type(Node) = values
%%
%%	`Node' is an abstract value list representing `\< <E1>, ...,
%%	<En> \>', if `List' is `[E1, ..., En]'.
%%
%% is_c_values(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract value list; otherwise
%%	`false'.
%%
%% values_es(Node) -> [coreErlang()]
%%
%%	    Node = coreErlang()
%%	    is_c_values(Node)
%%
%%	Returns the list `[E1, ..., En]' if `Node' represents `\< <E1>,
%%	..., <En> \>'.
%%
%% values_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_values(Node)
%%
%%	Returns the number `n', if `Node' represents `\< <E1>, ..., <En>
%%	\>'.
%%
%%	Note: This is equivalent to `erlang:length(values_es(Node))',
%%	but potentially more efficient.

%% Representation:
%%
%% {values, A, List}
%%
%%	List = [coreErlang()]

-record(values, {ann = [], es}).

c_values(Es) ->
    #values{es = Es}.

ann_c_values(As, Es) ->
    #values{es = Es, ann = As}.

update_c_values(Node, Es) ->
    #values{es = Es, ann = get_ann(Node)}.

is_c_values(#values{}) ->
    true;
is_c_values(_) ->
    false.

values_es(Node) ->
    Node#values.es.

values_arity(Node) ->
    length(values_es(Node)).


%% =====================================================================
%% c_fun(Variables, Body) -> Node
%% ann_c_fun(As, Variables, Body) -> Node
%% update_c_fun(Old, Variables, Body) -> Node
%%
%%	    Variables = [Variable]
%%	    Variable = Body = Node = coreErlang()
%%	    type(Node) = fun
%%
%%	`Node' is an abstract fun-expression representing `fun (<V1>,
%%	..., <Vn>) <`->'> <Body>', if `Variables' is `[V1, ..., Vn]'.
%%
%% is_c_fun(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract fun-expression;
%%	otherwise `false'.
%%
%% fun_vars(Node) -> Variables
%%
%%	    Node = coreErlang()
%%	    Variables = [Variable]
%%	    Variable = coreErlang()
%%	    is_c_fun(Node)
%%
%%	Returns the list `[V1, ..., Vn]', if `Node' represents `fun
%%	(<V1>, ..., <Vn>) <`->'> <Body>'.
%%
%% fun_body(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_fun(Node)
%%
%%	Returns the `Body' subtree of the abstract fun-expression
%%	`Node'.
%%
%% fun_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_fun(Node)
%%
%%	Returns the number `n', if `Node' represents `fun (<V1>, ...,
%%	<Vn>) <`->'> <Body>'.
%%
%%	Note: this is equivalent to `erlang:length(fun_vars(Node))', but
%%	potentially more efficient.

%% Representation:
%%
%% {'fun', A, Variables, Body}
%%
%%	Variables = [Variable]
%%	Variable = Body = coreErlang()

-record('fun', {ann = [], vars, body}).

c_fun(Variables, Body) ->
    #'fun'{vars = Variables, body = Body}.

ann_c_fun(As, Variables, Body) ->
    #'fun'{vars = Variables, body = Body, ann = As}.

update_c_fun(Node, Variables, Body) ->
    #'fun'{vars = Variables, body = Body, ann = get_ann(Node)}.

is_c_fun(#'fun'{}) ->
    true;		% Now this is fun!
is_c_fun(_) ->
    false.

fun_vars(Node) ->
    Node#'fun'.vars.

fun_body(Node) ->
    Node#'fun'.body.

fun_arity(Node) ->
    length(fun_vars(Node)).


%% =====================================================================
%% c_seq(Argument, Body) -> Node
%% ann_c_seq(As, Argument, Body) -> Node
%% update_c_seq(Old, Argument, Body) -> Node
%%
%%	    Argument = Body = Node = coreErlang()
%%	    type(Node) = seq
%%
%%	`Node' is an abstract sequencing expression representing `do
%%	<Argument> <Body>'.
%%
%% is_c_seq(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract sequencing expression;
%%	otherwise `false'.
%%
%% seq_arg(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_seq(Node)
%%
%%	Returns the `Argument' subtree of the abstract sequencing
%%	expression `Node'.
%%
%% seq_body(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_seq(Node)
%%
%%	Returns the `Body' subtree of the abstract sequencing expression
%%	`Node'.

%% Representation:
%%
%% {seq, A, Argument, Body}
%%
%%	Argument = Body = coreErlang()

-record(seq, {ann = [], arg, body}).

c_seq(Argument, Body) ->
    #seq{arg = Argument, body = Body}.

ann_c_seq(As, Argument, Body) ->
    #seq{arg = Argument, body = Body, ann = As}.

update_c_seq(Node, Argument, Body) ->
    #seq{arg = Argument, body = Body, ann = get_ann(Node)}.

is_c_seq(#seq{}) ->
    true;
is_c_seq(_) ->
    false.

seq_arg(Node) ->
    Node#seq.arg.

seq_body(Node) ->
    Node#seq.body.


%% =====================================================================
%% c_let(Variables, Argument, Body) -> Node
%% ann_c_let(As, Variables, Argument, Body) -> Node
%% update_c_let(Old, Variables, Argument, Body) -> Node
%%
%%	    Argument = Body = Node = coreErlang()
%%	    Variables = [Variable]
%%	    Variable = coreErlang()
%%	    type(Node) = let
%%
%%	`Node' is an abstract let-expression representing `let \< <V1>,
%%	..., <Vn> \> = <Argument> in <Body>', if `Variables' is `[V1,
%%	..., Vn]'.
%%
%% is_c_let(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract let-expression;
%%	otherwise `false'.
%%
%% let_vars(Node) -> Variables
%%
%%	    Node = coreErlang()
%%	    Variables = [Variable]
%%	    Variable = coreErlang()
%%	    is_c_let(Node)
%%
%%	Returns the list `[V1, ..., Vn]', if `Node' represents `let \<
%%	<V1>, ..., <Vn> \> = <Argument> in <Body>'.
%%
%% let_arg(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_let(Node)
%%
%%	Returns the `Argument' subtree of the abstract let-expression
%%	`Node'
%%
%% let_body(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_let(Node)
%%
%%	Returns the `Body' subtree of the abstract let-expression
%%	`Node'.
%%
%% let_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_let(Node)
%%
%%	Returns the number `n', if `Node' represents `let \< <V1>, ...,
%%	<Vn> \> = <Argument> in <Body>'.
%%
%%	Note: this is equivalent to `erlang:length(let_vars(Node))', but
%%	potentially more efficient.

%% Representation:
%%
%% {'let', A, Variables, Argument, Body}
%%
%%	Variables = [Variable]
%%	Variable = Argument = Body = coreErlang()

-record('let', {ann = [], vars, arg, body}).

c_let(Variables, Argument, Body) ->
    #'let'{vars = Variables, arg = Argument, body = Body}.

ann_c_let(As, Variables, Argument, Body) ->
    #'let'{vars = Variables, arg = Argument, body = Body, ann = As}.

update_c_let(Node, Variables, Argument, Body) ->
    #'let'{vars = Variables, arg = Argument, body = Body,
	   ann = get_ann(Node)}.

is_c_let(#'let'{}) ->
    true;
is_c_let(_) ->
    false.

let_vars(Node) ->
    Node#'let'.vars.

let_arg(Node) ->
    Node#'let'.arg.

let_body(Node) ->
    Node#'let'.body.

let_arity(Node) ->
    length(let_vars(Node)).


%% =====================================================================
%% c_letrec(Definitions, Body) -> Node
%% ann_c_letrec(As, Definitions, Body) -> Node
%% update_c_letrec(Old, Definitions, Body) -> Node
%%
%%	    Body = Node = coreErlang()
%%	    Definitions = [{Variable, Fun}]
%%	    Variable = Fun = coreErlang()
%%	    type(Node) = letrec
%%
%%	`Node' is an abstract letrec-expression representing `letrec
%%	<V1> = <F1> ... <Vn> = <Fn> in <Body>', if `Definitions' =
%%	`[{V1, F1}, ..., {Vn, Fn}]'.
%%
%% is_c_letrec(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract letrec-expression;
%%	otherwise `false'.
%%
%% letrec_defs(Node) -> Definitions
%%
%%	    Node = coreErlang()
%%	    Definitions = [{Variable, Fun}]
%%	    Variable = Fun = coreErlang()
%%	    is_c_letrec(Node)
%%
%%	Returns the list `[{V1, F1}, ..., {Vn, Fn}]' if `Node'
%%	represents `letrec <V1> = <F1> ... <Vn> = <Fn> in <Body>'.
%%
%% letrec_body(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_letrec(Node)
%%
%%	Returns the `Body' subtree of the abstract letrec-expression
%%	`Node'.
%%
%% letrec_vars(Node) -> Variables
%%
%%	    Node = coreErlang()
%%	    Variables = [Variable]
%%	    Variable = coreErlang()
%%	    is_c_letrec(Node)
%%
%%	Returns the list `[V1, ..., Vn]', if `Node' represents `letrec
%%	<V1> = <F1> ... <Vn> = <Fn> in <Body>'.

%% Representation:
%%
%% {letrec, A, Definitions, Body}
%%
%%	Definitions = [{Variable, Fun}]
%%	Body = Variable = Fun = coreErlang()

-record(letrec, {ann = [], defs, body}).

c_letrec(Defs, Body) ->
    #letrec{defs = Defs, body = Body}.

ann_c_letrec(As, Defs, Body) ->
    #letrec{defs = Defs, body = Body, ann = As}.

update_c_letrec(Node, Defs, Body) ->
    #letrec{defs = Defs, body = Body, ann = get_ann(Node)}.

is_c_letrec(#letrec{}) ->
    true;
is_c_letrec(_) ->
    false.

letrec_defs(Node) ->
    Node#letrec.defs.

letrec_body(Node) ->
    Node#letrec.body.

letrec_vars(Node) ->
    [F || {F, _} <- letrec_defs(Node)].


%% =====================================================================
%% c_case(Argument, Clauses) -> Node
%% ann_c_case(As, Argument, Clauses) -> Node
%% update_c_case(Old, Argument, Clauses) -> Node
%%
%%	    Argument = Node = coreErlang()
%%	    Clauses = [Clause] - []
%%	    Clause = coreErlang()
%%	    type(Node) = case
%%
%%	`Node' is an abstract case-expression representing `case
%%	<Argument> of <C1> ... <Cn> end', if `Clauses' is a nonempty
%%	list `[C1, ..., Cn]'.
%%
%% is_c_case(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract case-expression;
%%	otherwise `false'.
%%
%% case_arg(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_case(Node)
%%
%%	Returns the `Argument' subtree of the abstract case-expression
%%	`Node'.
%%
%% case_clauses(Node) -> Clauses
%%
%%	    Node = coreErlang()
%%	    Clauses = [Clause] - []
%%	    Clause = coreErlang()
%%	    is_c_case(Node)
%%
%%	Returns a nonempty list `[C1, ..., Cn]', if `Node' represents
%%	`case <Expr> of <C1> ... <Cn> end'.
%%
%% case_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_case(Node)
%%
%%	This is equivalent to `clause_arity(hd(case_clauses(Node)))',
%%	but potentially more efficient.

%% Representation:
%%
%% {'case', A, Argument, Clauses}
%%
%%	Argument = coreErlang()
%%	Clauses = [Clause] - []
%%	Clause = coreErlang()

-record('case', {ann = [], arg, clauses}).

c_case(Expr, Clauses) ->
    #'case'{arg = Expr, clauses = Clauses}.

ann_c_case(As, Expr, Clauses) ->
    #'case'{arg = Expr, clauses = Clauses, ann = As}.

update_c_case(Node, Expr, Clauses) ->
    #'case'{arg = Expr, clauses = Clauses, ann = get_ann(Node)}.

is_c_case(#'case'{}) ->
    true;
is_c_case(_) ->
    false.

case_arg(Node) ->
    Node#'case'.arg.

case_clauses(Node) ->
    Node#'case'.clauses.

case_arity(Node) ->
    clause_arity(hd(case_clauses(Node))).


%% =====================================================================
%% c_clause(Patterns, Body) -> Node
%% c_clause(Patterns, Guard, Body) -> Node
%% ann_c_clause(As, Patterns, Body) -> Node
%% ann_c_clause(As, Patterns, Guard, Body) -> Node
%% update_c_clause(Old, Patterns, Guard, Body) -> Node
%%
%%	    Guard = Body = Node = coreErlang()
%%	    Patterns = [coreErlang()]
%%	    type(Node) = clause
%%
%%	`Node' is an abstract clause representing `\< <P1>, ..., <Pn> \>
%%	when <Guard> <`->'> <Body>', if `Patterns' is `[P1, ..., Pn]'.
%%	Leaving out the `Guard' argument is equivalent to supplying the
%%	value `c_atom(true)'.
%%
%% is_c_clause(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract clause; otherwise
%%	`false'.
%%
%% clause_pats(Node) -> [coreErlang()]
%%
%%	    Node = coreErlang()
%%	    is_c_clause(Node)
%%
%%	Returns the list `[P1, ..., Pn]', if `Node' represents `\< <P1>,
%%	..., <Pn> \> when <Guard> <`->'> <Body>'.
%%
%% clause_guard(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_clause(Node)
%%
%%	Returns the `Guard' subtree of the abstract clause `Node'.
%%
%% clause_body(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_clause(Node)
%%
%%	Returns the `Body' subtree of the abstract clause `Node'.
%%
%% clause_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_clause(Node)
%%
%%	Returns the number `n', if `Node' represents `\< <P1>, ..., <Pn>
%%	\> when <Guard> <`->'> <Body>'.
%%
%%	Note: this is equivalent to `erlang:length(clause_pats(Node))',
%%	but potentially more efficient.
%%
%% clause_vars(Node) -> [Variable]
%%
%%	    Node = Variable = coreErlang()
%%	    is_c_clause(Node)
%%	    is_c_var(Variable)
%%
%%	Returns the list of all abstract variables occurring in P1, ...,
%%	Pn, if `Node' represents `\< <P1>, ..., <Pn> \> when <Guard>
%%	<`->'> <Body>'. An exception is thrown if `Node' does not
%%	represent a well-formed Core Erlang clause.
%%
%% pat_vars(Node) -> [Variable]
%%
%%	    Node = Variable = coreErlang()
%%	    is_c_var(Variable)
%%
%%	Returns a list of the abstract variables occurring in `Node'. An
%%	exception is thrown if `Node' does not represent a well-formed
%%	Core Erlang pattern. The resulting list may contain multiple
%%	entries having the same variable name.
%%
%% pat_list_vars(List) -> [Variable]
%%
%%	    List = [coreErlang()]
%%	    Variable = coreErlang()
%%	    is_c_var(Variable)
%%
%%	Returns a list of the abstract variables occurring in the syntax
%%	trees in `List'. An exception is thrown if some element in
%%	`List' does not represent a well-formed Core Erlang pattern. The
%%	resulting list may contain multiple entries having the same
%%	variable name.

%% Representation:
%%
%% {clause, A, Patterns, Guard, Body}
%%
%%	Patterns = [coreErlang()]
%%	Guard = Body = coreErlang()

-record(clause, {ann = [], pats, guard, body}).

c_clause(Patterns, Body) ->
    #clause{pats = Patterns, guard = c_atom(true), body = Body}.

c_clause(Patterns, Guard, Body) ->
    #clause{pats = Patterns, guard = Guard, body = Body}.

ann_c_clause(As, Patterns, Body) ->
    #clause{pats = Patterns, guard = c_atom(true), body = Body,
	    ann = As}.

ann_c_clause(As, Patterns, Guard, Body) ->
    #clause{pats = Patterns, guard = Guard, body = Body, ann = As}.

update_c_clause(Node, Patterns, Guard, Body) ->
    #clause{pats = Patterns, guard = Guard, body = Body,
	    ann = get_ann(Node)}.

is_c_clause(#clause{}) ->
    true;
is_c_clause(_) ->
    false.

clause_pats(Node) ->
    Node#clause.pats.

clause_guard(Node) ->
    Node#clause.guard.

clause_body(Node) ->
    Node#clause.body.

clause_arity(Node) ->
    length(clause_pats(Node)).

clause_vars(Clause) ->
    pat_list_vars(clause_pats(Clause)).

pat_vars(Node) ->
    pat_vars(Node, []).

pat_vars(Node, Vs) ->
    case type(Node) of
	var ->
	    [Node | Vs];
	literal ->
	    Vs;
	cons ->
	    pat_vars(cons_hd(Node), pat_vars(cons_tl(Node), Vs));
	tuple ->
	    pat_list_vars(tuple_es(Node), Vs);
	binary ->
	    pat_list_vars(binary_segs(Node), Vs);
	bin_seg ->
	    pat_vars(bin_seg_val(Node), Vs);
	alias ->
	    pat_vars(alias_pat(Node), [alias_var(Node) | Vs])
    end.

pat_list_vars(Ps) ->
    pat_list_vars(Ps, []).

pat_list_vars([P | Ps], Vs) ->
    pat_list_vars(Ps, pat_vars(P, Vs));
pat_list_vars([], Vs) ->
    Vs.


%% =====================================================================
%% c_alias(Variable, Pattern) -> Node
%% ann_c_alias(As, Variable, Pattern) -> Node
%% update_c_alias(Old, Variable, Pattern) -> Node
%%
%%	    Variable = Pattern = Node = coreErlang()
%%	    type(Node) = alias
%%
%%	`Node' is an abstract pattern alias representing
%%	`<Variable> = <Pattern>'.
%%
%% is_c_alias(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract pattern alias; otherwise
%%	`false'.
%%
%% alias_var(Node) -> Variable
%%
%%	    Node = Variable = coreErlang()
%%	    is_c_alias(Node)
%%
%%	Returns the `Variable' subtree of the abstract pattern alias
%%	`Node'.
%%
%% alias_pat(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_alias(Node)
%%
%%	Returns the `Pattern' subtree of the abstract pattern alias
%%	`Node'.

%% Representation:
%%
%% {alias, A, Variable, Pattern}
%%
%%	Variable = Pattern = coreErlang()

-record(alias, {ann = [], var, pat}).

c_alias(Var, Pattern) ->
    #alias{var = Var, pat = Pattern}.

ann_c_alias(As, Var, Pattern) ->
    #alias{var = Var, pat = Pattern, ann = As}.

update_c_alias(Node, Var, Pattern) ->
    #alias{var = Var, pat = Pattern, ann = get_ann(Node)}.

is_c_alias(#alias{}) ->
    true;
is_c_alias(_) ->
    false.

alias_var(Node) ->
    Node#alias.var.

alias_pat(Node) ->
    Node#alias.pat.


%% =====================================================================
%% c_receive(Clauses) -> Node
%% c_receive(Clauses, Timeout, Action) -> Node
%% ann_c_receive(As, Clauses) -> Node
%% ann_c_receive(As, Clauses, Timeout, Action) -> Node
%% update_c_receive(Old, Clauses, Timeout, Action) -> Node
%%
%%	    Clauses = [Clause]
%%	    Clause = Timeout = Action = Node = coreErlang()
%%	    type(Node) = receive
%%
%%	`Node' is an abstract receive-expression representing `receive
%%	<C1> ... <Cn> after <Timeout> <`->'> <Action> end', if `Clauses'
%%	is `[C1, ..., Cn]'. Leaving out `Timeout' and `Action' is
%%	equivalent to supplying the values of `c_atom(infinity)' and
%%	`c_atom(true)', respectively (where the latter is arbitrary).
%%
%% is_c_receive(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract receive-expression;
%%	otherwise `false'.
%%
%% receive_clauses(Node) -> Clauses
%%
%%	    Node = coreErlang()
%%	    Clauses = [Clause]
%%	    Clause = coreErlang()
%%	    is_c_receive(Node)
%%
%%	Returns the (possibly empty) list `[C1, ..., Cn]', if `Node'
%%	represents `receive <C1> ... <Cn> after <Timeout> <`->'>
%%	<Action> end'.
%%
%% receive_timeout(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_receive(Node)
%%
%%	Returns the `Timeout' subtree of the abstract receive-expression
%%	`Node'.
%%
%% receive_action(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_receive(Node)
%%
%%	Returns the `Action' subtree of the abstract receive-expression
%%	`Node'.

%% Representation:
%%
%% {'receive', A, Clauses, Timeout, Action}
%%
%%	Clauses = [Clause]
%%	Clause = Timeout = Action = Node = coreErlang()

-record('receive', {ann = [], clauses, timeout, action}).

c_receive(Clauses) ->
    #'receive'{clauses = Clauses, timeout = c_atom(infinity),
	       action = c_atom(true)}.

c_receive(Clauses, Timeout, Action) ->
    #'receive'{clauses = Clauses, timeout = Timeout, action = Action}.

ann_c_receive(As, Clauses) ->
    #'receive'{clauses = Clauses, timeout = c_atom(infinity),
	       action = c_atom(true), ann = As}.

ann_c_receive(As, Clauses, Timeout, Action) ->
    #'receive'{clauses = Clauses, timeout = Timeout, action = Action,
	       ann = As}.

update_c_receive(Node, Clauses, Timeout, Action) ->
    #'receive'{clauses = Clauses, timeout = Timeout, action = Action,
	       ann = get_ann(Node)}.

is_c_receive(#'receive'{}) ->
    true;
is_c_receive(_) ->
    false.

receive_clauses(Node) ->
    Node#'receive'.clauses.

receive_timeout(Node) ->
    Node#'receive'.timeout.

receive_action(Node) ->
    Node#'receive'.action.


%% =====================================================================
%% c_apply(Operator, Arguments) -> Node
%% ann_c_apply(As, Operator, Arguments) -> Node
%% update_c_apply(Old, Operator, Arguments) -> Node
%%
%%	    Operator = Node = coreErlang()
%%	    Arguments = [coreErlang()]
%%	    type(Node) = apply
%%
%%	`Node' is an abstract function application expression
%%	representing `(<Operator>)(<A1>, ..., <An>)', if `Arguments' is
%%	`[A1, ..., An]'.
%%
%% is_c_apply(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract function application
%%	expression; otherwise `false'.
%%
%% apply_op(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_apply(Node)
%%
%%	Returns the `Operator' subtree of the abstract function
%%	application expression `Node'.
%%
%% apply_args(Node) -> Arguments
%%
%%	    Node = coreErlang()
%%	    Arguments = [coreErlang()]
%%	    is_c_apply(Node)
%%
%%	Returns the list `[A1, ..., An]', if `Node' represents
%%	`(<Operator>)(<A1>, ..., <An>)'.
%%
%% apply_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_apply(Node)
%%
%%	Returns the number `n', if `Node' represents `(<Operator>)(<A1>,
%%	..., <An>)'.
%%
%%	Note: this is equivalent to `erlang:length(apply_args(Node))',
%%	but potentially more efficient.

%% Representation:
%%
%% {apply, A, Operator, Arguments}
%%
%%	Operator = coreErlang()
%%	Arguments = [coreErlang()]

-record(apply, {ann = [], op, args}).

c_apply(Operator, Arguments) ->
    #apply{op = Operator, args = Arguments}.

ann_c_apply(As, Operator, Arguments) ->
    #apply{op = Operator, args = Arguments, ann = As}.

update_c_apply(Node, Operator, Arguments) ->
    #apply{op = Operator, args = Arguments, ann = get_ann(Node)}.

is_c_apply(#apply{}) ->
    true;
is_c_apply(_) ->
    false.

apply_op(Node) ->
    Node#apply.op.

apply_args(Node) ->
    Node#apply.args.

apply_arity(Node) ->
    length(apply_args(Node)).


%% =====================================================================
%% c_call(Module, Name, Arguments) -> Node
%% ann_c_call(As, Module, Name, Arguments) -> Node
%% update_c_call(Old, Module, Name, Arguments) -> Node
%%
%%	    Module = Name = Node = coreErlang()
%%	    Arguments = [coreErlang()]
%%	    type(Node) = call
%%
%%	`Node' is an abstract inter-module call expression representing
%%	`call <Module>:<Name>(<A1>, ..., <An>)', if `Arguments' is `[A1,
%%	..., An]'.
%%
%% is_c_call(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract inter-module call
%%	expression; otherwise `false'.
%%
%% call_module(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_call(Node)
%%
%%	Returns the `Module' subtree of the abstract inter-module call
%%	expression `Node'.
%%
%% call_name(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_call(Node)
%%
%%	Returns the `Name' subtree of the abstract inter-module call
%%	expression `Node'.
%%
%% call_args(Node) -> Arguments
%%
%%	    Node = coreErlang()
%%	    Arguments = [coreErlang()]
%%	    is_c_call(Node)
%%
%%	Returns the list `[A1, ..., An]', if `Node' represents `call
%%	<Module>:<Name>(<A1>, ..., <An>)'.
%%
%% call_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_call(Node)
%%
%%	Returns the number `n', if `Node' represents `call
%%	<Module>:<Name>(<A1>, ..., <An>)'.
%%
%%	Note: this is equivalent to `erlang:length(call_args(Node))',
%%	but potentially more efficient.

%% Representation:
%%
%% {call, A, Module, Name, Arguments}
%%
%%	Module = Name = coreErlang()
%%	Arguments = [coreErlang()]

-record(call, {ann = [], module, name, args}).

c_call(Module, Name, Arguments) ->
    #call{module = Module, name = Name, args = Arguments}.

ann_c_call(As, Module, Name, Arguments) ->
    #call{module = Module, name = Name, args = Arguments, ann = As}.

update_c_call(Node, Module, Name, Arguments) ->
    #call{module = Module, name = Name, args = Arguments,
	  ann = get_ann(Node)}.

is_c_call(#call{}) ->
    true;
is_c_call(_) ->
    false.

call_module(Node) ->
    Node#call.module.

call_name(Node) ->
    Node#call.name.

call_args(Node) ->
    Node#call.args.

call_arity(Node) ->
    length(call_args(Node)).


%% =====================================================================
%% c_primop(Name, Arguments) -> Node
%% ann_c_primop(As, Name, Arguments) -> Node
%% update_c_primop(Old, Name, Arguments) -> Node
%%
%%	    Name = Node = coreErlang()
%%	    Arguments = [coreErlang()]
%%	    type(Node) = primop
%%
%%	`Node' is an abstract primitive operation expression
%%	representing `primop <Name>(<A1>, ..., <An>)', if `Arguments' is
%%	`[A1, ..., An]'.
%%
%% is_c_primop(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract primitive operation
%%	expression; otherwise `false'.
%%
%% primop_name(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_primop(Node)
%%
%%	Returns the `Name' subtree of the abstract primitive operation
%%	expression `Node'.
%%
%% primop_args(Node) -> Arguments
%%
%%	    Node = coreErlang()
%%	    Arguments = [coreErlang()]
%%	    is_c_primop(Node)
%%
%%	Returns the list `[A1, ..., An]', if `Node' represents `primop
%%	<Name>(<A1>, ..., <An>)'.
%%
%% primop_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%	    is_c_primop(Node)
%%
%%	Returns the number `n', if `Node' represents `primop
%%	<Name>(<A1>, ..., <An>)'.
%%
%%	Note: this is equivalent to `erlang:length(primop_args(Node))',
%%	but potentially more efficient.

%% Representation:
%%
%% {primop, A, Name, Arguments}
%%
%%	Name = coreErlang()
%%	Arguments = [coreErlang()]

-record(primop, {ann = [], name, args}).

c_primop(Name, Arguments) ->
    #primop{name = Name, args = Arguments}.

ann_c_primop(As, Name, Arguments) ->
    #primop{name = Name, args = Arguments, ann = As}.

update_c_primop(Node, Name, Arguments) ->
    #primop{name = Name, args = Arguments, ann = get_ann(Node)}.

is_c_primop(#primop{}) ->
    true;
is_c_primop(_) ->
    false.

primop_name(Node) ->
    Node#primop.name.

primop_args(Node) ->
    Node#primop.args.

primop_arity(Node) ->
    length(primop_args(Node)).


%% =====================================================================
%% c_try(Expression, Variables, Body) -> Node
%% ann_c_try(As, Expression, Variables, Body) -> Node
%% update_c_try(Old, Expression, Variables, Body) -> Node
%%
%%	    Expression = Body = Node = coreErlang()
%%	    Variables = [Var]
%%	    Var = coreErlang()
%%	    type(Node) = try
%%
%%	`Node' is an abstract try-expression representing `try
%%	<Expression> catch (<V1>, ..., <Vn>) <`->'> <Body>', if
%%	`Variables' is `[V1, ..., Vn]'.
%%
%% is_c_try(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract try-expression;
%%	otherwise `false'.
%%
%% try_expr(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_try(Node)
%%
%%	Returns the `Expression' subtree of the abstract try-expression
%%	`Node'.
%%
%% try_vars(Node) -> Variables
%%
%%	    Node = coreErlang()
%%	    Variables = [Var]
%%	    Var = coreErlang()
%%	    is_c_try(Node)
%%
%%	Returns the list `[V1, ..., Vn]', if `Node' represents `try
%%	<Expression> catch (<V1>, ..., <Vn>) <`->'> <Body>'.
%%
%% try_body(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_try(Node)
%%
%%	Returns the `Body' subtree of the abstract try-expression
%%	`Node'.

%% Representation:
%%
%% {'try', A, Expression, Variables, Body}
%%
%%	Expression = Body = coreErlang()
%%	Variables = [Var]
%%	Var = coreErlang()

-record('try', {ann = [], expr, vars, body}).

c_try(Expr, Vs, Body) ->
    #'try'{expr = Expr, vars = Vs, body = Body}.

ann_c_try(As, Expr, Vs, Body) ->
    #'try'{expr = Expr, vars = Vs, body = Body, ann = As}.

update_c_try(Node, Expr, Vs, Body) ->
    #'try'{expr = Expr, vars = Vs, body = Body, ann = get_ann(Node)}.

is_c_try(#try{}) ->
    true;
is_c_try(_) ->
    false.

try_expr(Node) ->
    Node#'try'.expr.

try_vars(Node) ->
    Node#'try'.vars.

try_body(Node) ->
    Node#'try'.body.


%% =====================================================================
%% c_catch(Body) -> Node
%% ann_c_catch(As, Body) -> Node
%% update_c_catch(Old, Body) -> Node
%%
%%	    Body = Node = coreErlang()
%%	    type(Node) = catch
%%
%%	`Node' is an abstract catch-expression representing `catch
%%	<Body>'.
%%
%% is_c_catch(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' is an abstract catch-expression;
%%	otherwise `false'.
%%
%% catch_body(Node) -> coreErlang()
%%
%%	    Node = coreErlang()
%%	    is_c_catch(Node)
%%
%%	Returns the `Body' subtree of the abstract catch-expression
%%	`Node'.

%% Representation:
%%
%% {'catch', A, Body}
%%
%%	Body = coreErlang()

-record('catch', {ann = [], body}).

c_catch(Body) ->
    #'catch'{body = Body}.

ann_c_catch(As, Body) ->
    #'catch'{body = Body, ann = As}.

update_c_catch(Node, Body) ->
    #'catch'{body = Body, ann = get_ann(Node)}.

is_c_catch(#'catch'{}) ->
    true;
is_c_catch(_) ->
    false.

catch_body(Node) ->
    Node#'catch'.body.


%% =====================================================================
%% to_records(Tree) -> record(Type)
%%
%%	    Tree = coreErlang()
%%	    Type = c_alias | c_apply | c_call | c_case | c_catch |
%%		   c_clause | c_cons | c_fun | c_let | c_letrec |
%%		   c_lit | c_module | c_primop | c_receive | c_seq |
%%		   c_try | c_tuple | c_values | c_var
%%
%%	Translates a Core Erlang abstract syntax tree to a corresponding
%%	tree constructed from the records defined in the file
%%	`cerl.hrl'. Annotation term lists are copied to the
%%	corresponding target nodes.

to_records(Node) ->
    A = get_ann(Node),
    case type(Node) of
	literal ->
	    lit_to_records(concrete(Node), A);
	binary ->
	    #c_binary{anno = A,
		      segs = list_to_records(binary_segs(Node))};
	bin_seg ->
	    #c_bin_seg{anno = A,
		       val = to_records(bin_seg_val(Node)),
		       size = to_records(bin_seg_size(Node)),
		       unit = concrete(bin_seg_unit(Node)),
		       type = concrete(bin_seg_type(Node)),
		       flags = [concrete(F)
				|| F <- bin_seg_flags(Node)]};
	cons ->
	    #c_cons{anno = A,
		    hd = to_records(cons_hd(Node)),
		    tl = to_records(cons_tl(Node))};
	tuple ->
	    #c_tuple{anno = A,
		     es = list_to_records(tuple_es(Node))};
	var ->
	    case is_c_fname(Node) of
		true ->
		    #c_fname{anno = A,
			     id = fname_id(Node),
			     arity = fname_arity(Node)};
		false ->
		    #c_var{anno = A, name = var_name(Node)}
	    end;
	values ->
	    #c_values{anno = A,
		      es = list_to_records(values_es(Node))};
	'fun' ->
	    #c_fun{anno = A,
		   vars = list_to_records(fun_vars(Node)),
		   body = to_records(fun_body(Node))};
	seq ->
	    #c_seq{anno = A,
		   arg = to_records(seq_arg(Node)),
		   body = to_records(seq_body(Node))};
	'let' ->
	    #c_let{anno = A,
		   vars = list_to_records(let_vars(Node)),
		   arg = to_records(let_arg(Node)),
		   body = to_records(let_body(Node))};
	letrec ->
	    #c_letrec{anno = A,
		      defs = [#c_def{name = to_records(N),
				     val = to_records(F)}
			      || {N, F} <- letrec_defs(Node)],
		      body = to_records(letrec_body(Node))};
	'case' ->
	    #c_case{anno = A,
		    arg = to_records(case_arg(Node)),
		    clauses =
		    list_to_records(case_clauses(Node))};
	clause ->
	    #c_clause{anno = A,
		      pats = list_to_records(clause_pats(Node)),
		      guard = to_records(clause_guard(Node)),
		      body = to_records(clause_body(Node))};
	alias ->
	    #c_alias{anno = A,
		     var = to_records(alias_var(Node)),
		     pat = to_records(alias_pat(Node))};
	'receive' ->
	    #c_receive{anno = A,
		       clauses = 
		       list_to_records(receive_clauses(Node)),
		       timeout =
		       to_records(receive_timeout(Node)),
		       action = 
		       to_records(receive_action(Node))};
	apply ->
	    #c_apply{anno = A,
		     op = to_records(apply_op(Node)),
		     args = list_to_records(apply_args(Node))};
	call ->
	    #c_call{anno = A,
		    module = to_records(call_module(Node)),
		    name = to_records(call_name(Node)),
		    args = list_to_records(call_args(Node))};
	primop ->
	    #c_primop{anno = A,
		      name = to_records(primop_name(Node)),
		      args = list_to_records(primop_args(Node))};
	'try' ->
	    #c_try{anno = A,
		   expr = to_records(try_expr(Node)),
		   vars = list_to_records(try_vars(Node)),
		   body = to_records(try_body(Node))};
	'catch' ->
	    #c_catch{anno = A,
		     body = to_records(catch_body(Node))};
	module ->
	    #c_module{anno = A,
		      name = to_records(module_name(Node)),
		      exports = list_to_records(
				  module_exports(Node)),
		      attrs = [#c_def{name = to_records(K),
				      val = to_records(V)}
			       || {K, V} <- module_attrs(Node)],
		      defs = [#c_def{name = to_records(N),
				     val = to_records(F)}
			      || {N, F} <- module_defs(Node)]}
    end.

list_to_records([T | Ts]) ->
    [to_records(T) | list_to_records(Ts)];
list_to_records([]) ->
    [].

lit_to_records(V, A) when integer(V) ->
    #c_int{anno = A, val = V};
lit_to_records(V, A) when float(V) ->
    #c_float{anno = A, val = V};
lit_to_records(V, A) when atom(V) ->
    #c_atom{anno = A, val = V};
lit_to_records([H | T] = V, A) ->
    case is_print_char_list(V) of
	true ->
	    #c_string{anno = A, val = V};
	false ->
	    #c_cons{anno = A,
		    hd = lit_to_records(H, []),
		    tl = lit_to_records(T, [])}
    end;
lit_to_records([], A) ->
    #c_nil{anno = A};
lit_to_records(V, A) when tuple(V) ->
    #c_tuple{anno = A, es = lit_list_to_records(tuple_to_list(V))}.

lit_list_to_records([T | Ts]) ->
    [lit_to_records(T, []) | lit_list_to_records(Ts)];
lit_list_to_records([]) ->
    [].


%% =====================================================================
%% from_records(Tree) -> coreErlang()
%%
%%	    Tree = record(Type)
%%	    Type = c_alias | c_apply | c_call | c_case | c_catch |
%%		   c_clause | c_cons | c_fun | c_let | c_letrec |
%%		   c_lit | c_module | c_primop | c_receive | c_seq |
%%		   c_try | c_tuple | c_values | c_var
%%
%%	Translates a syntax tree constructed from the records defined in
%%	the file `cerl.hrl' to a corresponding abstract syntax tree.
%%	Annotation term lists are copied to the corresponding target
%%	nodes.

from_records(#c_int{val = V, anno = As}) ->
    ann_c_int(As, V);
from_records(#c_float{val = V, anno = As}) ->
    ann_c_float(As, V);
from_records(#c_atom{val = V, anno = As}) ->
    ann_c_atom(As, V);
from_records(#c_char{val = V, anno = As}) ->
    ann_c_char(As, V);
from_records(#c_string{val = V, anno = As}) ->
    ann_c_string(As, V);
from_records(#c_nil{anno = As}) ->
    ann_c_nil(As);
from_records(#c_binary{segs = Ss, anno = As}) ->
    ann_c_binary(As, from_records_list(Ss));
from_records(#c_bin_seg{val = V, size = S, unit = U, type = T,
			flags = Fs, anno = As}) ->
    ann_c_bin_seg(As, from_records(V), from_records(S), abstract(U),
		  abstract(T), [abstract(F) || F <- Fs]);
from_records(#c_cons{hd = H, tl = T, anno = As}) ->
    ann_c_cons(As, from_records(H), from_records(T));
from_records(#c_tuple{es = Es, anno = As}) ->
    ann_c_tuple(As, from_records_list(Es));
from_records(#c_var{name = Name, anno = As}) ->
    ann_c_var(As, Name);
from_records(#c_fname{id = Id, arity = Arity, anno = As}) ->
    ann_c_fname(As, Id, Arity);
from_records(#c_values{es = Es, anno = As}) ->
    ann_c_values(As, from_records_list(Es));
from_records(#c_fun{vars = Vs, body = B, anno = As}) ->
    ann_c_fun(As, from_records_list(Vs), from_records(B));
from_records(#c_seq{arg = A, body = B, anno = As}) ->
    ann_c_seq(As, from_records(A), from_records(B));
from_records(#c_let{vars = Vs, arg = A, body = B, anno = As}) ->
    ann_c_let(As, from_records_list(Vs), from_records(A),
	      from_records(B));
from_records(#c_letrec{defs = Fs, body = B, anno = As}) ->
    ann_c_letrec(As, [{from_records(N), from_records(F)}
		      || #c_def{name = N, val = F} <- Fs],
		 from_records(B));
from_records(#c_case{arg = A, clauses = Cs, anno = As}) ->
    ann_c_case(As, from_records(A), from_records_list(Cs));
from_records(#c_clause{pats = Ps, guard = G, body = B, anno = As}) ->
    ann_c_clause(As, from_records_list(Ps), from_records(G),
		 from_records(B));
from_records(#c_alias{var = V, pat = P, anno = As}) ->
    ann_c_alias(As, from_records(V), from_records(P));
from_records(#c_receive{clauses = Cs, timeout = T, action = A,
			anno = As}) ->
    ann_c_receive(As, from_records_list(Cs), from_records(T),
		  from_records(A));
from_records(#c_apply{op = Op, args = Es, anno = As}) ->
    ann_c_apply(As, from_records(Op), from_records_list(Es));
from_records(#c_call{module = M, name = N, args = Es, anno = As}) ->
    ann_c_call(As, from_records(M), from_records(N),
	       from_records_list(Es));
from_records(#c_primop{name = N, args = Es, anno = As}) ->
    ann_c_primop(As, from_records(N), from_records_list(Es));
from_records(#c_try{expr = E, vars = Vs, body = B, anno = As}) ->
    ann_c_try(As, from_records(E), from_records_list(Vs),
	      from_records(B));
from_records(#c_catch{body = B, anno = As}) ->
    ann_c_catch(As, from_records(B));
from_records(#c_module{name = N, exports = Es, attrs = Ds, defs = Fs,
		       anno = As}) ->
    ann_c_module(As, from_records(N),
		 from_records_list(Es),
		 [{from_records(K), from_records(V)}
		  || #c_def{name = K, val = V} <- Ds],
		 [{from_records(V), from_records(F)}
		  || #c_def{name = V, val = F} <- Fs]).

from_records_list([T | Ts]) ->
    [from_records(T) | from_records_list(Ts)];
from_records_list([]) ->
    [].


%% =====================================================================
%% is_data(Node) -> bool()
%%
%%	    Node = coreErlang()
%%
%%	Returns `true' if `Node' represents a data constructor, i.e., an
%%	atomic literal, cons or tuple; otherwise `false'.
%%
%% data_type(Node) -> CType
%%
%%	    Node = coreErlang()
%%	    CType = cons | tuple | {atomic, Value}
%%	    Value = integer() | float() | atom() | []
%%
%%	    is_data(Node)
%%
%%	Returns a type descriptor for the data constructor represented
%%	by `Node'. This is mainly useful for comparing types and for
%%	constructing new nodes of the same type (cf. `make_data'). If
%%	`Node' represents an integer, floating-point number, atom or
%%	empty list, `CType' is `{atomic, Value}', where `Value' is the
%%	value of `concrete(Node)'; otherwise `CType' is `cons' or
%%	`tuple'.
%%
%%	Type descriptors can be compared for equality or order (in the
%%	Erlang term order), but remember that floating-point values
%%	should in general never be tested for equality.
%%
%% data_es(Node) -> Elements
%%
%%	    Node = coreErlang()
%%	    Elements = [coreErlang()]
%%
%%	    is_data(Node)
%%
%%	Returns the subtrees of `Node', in left-to-right order, if
%%	`Node' represents a data constructor. If the arity of the
%%	constructor is zero, the result is the empty list.
%%
%%	Note: if `data_type(Node)' is `cons', the number of subtrees is
%%	exactly two. If `data_type(Node)' is `{atomic, Value}', the
%%	number of subtrees is zero.
%%
%% data_arity(Node) -> Arity
%%
%%	    Node = coreErlang()
%%	    Arity = integer()
%%
%%	    is_data(Node)
%%
%%	Returns the number of subtrees of a data constructor `Node'.
%%	This is equivalent to `length(data_es(Node))', but potentially
%%	more efficient.
%%
%% make_data(CType, Elements) -> Node
%% ann_make_data(As, CType, Elements) -> Node
%% update_data(Old, CType, Elements) -> Node
%%
%%	    CType = cons | tuple | {atomic, Value}
%%	    Value = integer() | float() | atom() | []
%%	    Elements = [coreErlang()]
%%	    Node = coreErlang()
%%
%%	    is_data(Node)
%%
%%	`Node' represents the data constructor (cf. `is_data') whose
%%	type descriptor (cf. `data_type') is `CType', and whose subtrees
%%	are exactly those in the list `Elements', in left-to-right
%%	order, if this is possible (see `data_arity' for arity
%%	limitations on constructor types). An exception is thrown if the
%%	length of `Elements' is invalid for the given `CType'.
%%
%% make_nonlit_data(CType, Elements) -> Node
%%
%%	Like `make_data', but analogous to `c_nonlit_tuple' and
%%	`c_nonlit_cons'.

is_data(#literal{}) ->
    true;
is_data(#cons{}) ->
    true;
is_data(#tuple{}) ->
    true;
is_data(_) ->
    false.

data_type(#literal{val = V}) ->
    case V of
	[_ | _] ->
	    cons;
	_ when tuple(V) ->
	    tuple;
	_ ->
	    {atomic, V}
    end;
data_type(#cons{}) ->
    cons;
data_type(#tuple{}) ->
    tuple.

data_es(#literal{val = V}) ->
    case V of
	[Head | Tail] ->
	    [#literal{val = Head}, #literal{val = Tail}];
	_ when tuple(V) ->
	    make_lit_list(tuple_to_list(V));
	_ ->
	    []
    end;
data_es(#cons{hd = H, tl = T}) ->
    [H, T];
data_es(#tuple{es = Es}) ->
    Es.

data_arity(#literal{val = V}) ->
    case V of
	[_ | _] ->
	    2;
	_ when tuple(V) ->
	    size(V);
	_ ->
	    0
    end;
data_arity(#cons{}) ->
    2;
data_arity(#tuple{es = Es}) ->
    length(Es).

make_data({atomic, V}, []) -> #literal{val = V};
make_data(cons, [H, T]) -> c_cons(H, T);
make_data(tuple, Es) -> c_tuple(Es).

ann_make_data(As, {atomic, V}, []) -> #literal{val = V, ann = As};
ann_make_data(As, cons, [H, T]) -> ann_c_cons(As, H, T);
ann_make_data(As, tuple, Es) -> ann_c_tuple(As, Es).

update_data(Node, {atomic, V}, []) ->
    #literal{val = V, ann = get_ann(Node)};
update_data(Node, cons, [H, T]) ->
    update_c_cons(Node, H, T);
update_data(Node, tuple, Es) ->
    update_c_tuple(Node, Es).

make_nonlit_data({atomic, V}, []) -> #literal{val = V};
make_nonlit_data(cons, [H, T]) -> c_nonlit_cons(H, T);
make_nonlit_data(tuple, Es) -> c_nonlit_tuple(Es).


%% =====================================================================
%% subtrees(Node) -> Components
%%
%%	    Node = coreErlang()
%%	    Components = [] | Groups
%%	    Groups = [[coreErlang()]] - []
%%
%%	Returns the empty list `[]' if `Node' is a leaf node (cf.
%%	`is_leaf'); otherwise returns a *nonempty* list of lists of
%%	syntax trees, representing the subtrees of `Node' *in
%%	left-to-right order as they would occur in the printed program
%%	text*, and grouped by category. Often, each group contains only
%%	a single subtree.
%%
%%	Depending on the type of `Node', the size of some groups may
%%	vary (e.g., the group consisting of the argument expressions of
%%	a primop-call), while others always contain the same number of
%%	elements - usually exactly one (e.g., the group containing the
%%	argument expression of a `case' expression). Note, though, that
%%	the exact structure of the returned list (for a given node type)
%%	should in general not be depended upon, since it might be
%%	subject to change without notice.
%%
%% make_tree(Type, Groups) -> Node
%%
%%	    Type = atom()
%%	    Groups = [[coreErlang()]] - []
%%	    Node = coreErlang()
%%
%%	`Type' is a node type name (cf. `type') that does not denote a
%%	leaf node (cf. `is_leaf'). `Groups' is a *nonempty* list of
%%	lists of syntax trees, representing the subtrees of a node
%%	corresponding to `Type', *in left-to-right order as they would
%%	occur in the printed program text*, grouped by category as done
%%	by the function `subtrees'.
%%
%%	The result from evaluating the expression `make_tree(type(Node),
%%	subtrees(Node))' represents the same source code text as `Node'
%%	itself, *given that `subtrees(Node)' yields a nonempty list*
%%	(equivalently, that `is_leaf(Node)' yields `false'), but the
%%	result does not necessarily have the exact same data
%%	representation as `Node', and its list of annotations is empty.
%%
%% The above functions can be a great help if one wants to traverse a
%% syntax tree, visiting all its subtrees, but treat nodes of the tree
%% in a uniform way in some or all cases. Using `subtrees' and the
%% constructor function `make_tree' makes this simple, and also assures
%% that the code is not overly sensitive to extensions of the syntax
%% tree data type, because any node types not explicitly handled by your
%% code can be left to a default case for nodes in general.
%%
%% Example:
%%
%%	bottomup(F, Tree) ->
%%	    F(case subtrees(Tree) of
%%		[] ->
%%		    Tree;
%%		List ->
%%		    make_tree(type(Tree),
%%			      [[bottomup(F, Subtree)
%%				|| Subtree <- Group]
%%			       || Group <- List])
%%	      end).
%%
%% maps the function `F' on `Tree' and all its subtrees, in a bottom-up
%% order. For a simple function like e.g.:
%%
%%	f(Node) ->
%%	    case is_c_atom(Node) of
%%		true ->
%%		    c_atom("a_" ++ atom_name(Node));
%%		false ->
%%		    Node
%%	    end.
%%
%% the call `bottomup(fun f/1, Tree)' will yield the syntax tree `Tree'
%% in which all atom names have been extended with the prefix `a_'.
%% (Note, though, that annotations on the original tree are not
%% preserved by the above example, in order to keep it simple.)

subtrees(T) ->
    case is_leaf(T) of
	true ->
	    [];
	false ->
	    case type(T) of
		values ->
		    [values_es(T)];
		binary ->
		    [binary_segs(T)];
		bin_seg ->
		    [[bin_seg_val(T)], [bin_seg_size(T)],
		     [bin_seg_unit(T)], [bin_seg_type(T)],
		     bin_seg_flags(T)];
		cons ->
		    [[cons_hd(T)], [cons_tl(T)]];
		tuple ->
		    [tuple_es(T)];
		'let' ->
		    [let_vars(T), [let_arg(T)], [let_body(T)]];
		seq ->
		    [[seq_arg(T)], [seq_body(T)]];
		apply ->
		    [[apply_op(T)], apply_args(T)];
		call ->
		    [[call_module(T)], [call_name(T)],
		     call_args(T)];
		primop ->
		    [[primop_name(T)], primop_args(T)];
		'case' ->
		    [[case_arg(T)], case_clauses(T)];
		clause ->
		    [clause_pats(T), [clause_guard(T)],
		     [clause_body(T)]];
		alias ->
		    [[alias_var(T)], [alias_pat(T)]];
		'fun' ->
		    [fun_vars(T), [fun_body(T)]];
		'receive' ->
		    [receive_clauses(T), [receive_timeout(T)],
		     [receive_action(T)]];
		'try' ->
		    [[try_expr(T)], try_vars(T), [try_body(T)]];
		'catch' ->
		    [[catch_body(T)]];
		letrec ->
		    Es = unfold_tuples(letrec_defs(T)),
		    [Es, [letrec_body(T)]];
		module ->
		    As = unfold_tuples(module_attrs(T)),
		    Es = unfold_tuples(module_defs(T)),
		    [[module_name(T)], module_exports(T), As, Es]
	    end
    end.

make_tree(values, [Es]) -> c_values(Es);
make_tree(binary, [Ss]) -> c_binary(Ss);
make_tree(bin_seg, [[V],[S],[U],[T],Fs]) -> c_bin_seg(V, S, U, T, Fs);
make_tree(cons, [[H], [T]]) -> c_cons(H, T);
make_tree(tuple, [Es]) -> c_tuple(Es);
make_tree('let', [Vs, [A], [B]]) -> c_let(Vs, A, B);
make_tree(seq, [[A], [B]]) -> c_seq(A, B);
make_tree(apply, [[Op], As]) -> c_apply(Op, As);
make_tree(call, [[M], [N], As]) -> c_call(M, N, As);
make_tree(primop, [[N], As]) -> c_primop(N, As);
make_tree('case', [[A], Cs]) -> c_case(A, Cs);
make_tree(clause, [Ps, [G], [B]]) -> c_clause(Ps, G, B);
make_tree(alias, [[V], [P]]) -> c_alias(V, P);
make_tree('fun', [Vs, [B]]) -> c_fun(Vs, B);
make_tree('receive', [Cs, [T], [A]]) -> c_receive(Cs, T, A);
make_tree('try', [[E], Vs, [B]]) -> c_try(E, Vs, B);
make_tree('catch', [[B]]) -> c_catch(B);
make_tree(letrec, [Es, [B]]) -> c_letrec(fold_tuples(Es), B);
make_tree(module, [[N], Xs, As, Es]) ->
    c_module(N, Xs, fold_tuples(As), fold_tuples(Es)).


%% =====================================================================
%% meta(Tree) -> MetaTree
%%
%%	    Tree = MetaTree = coreErlang()
%%
%%	`MetaTree' is a meta-representation of the syntax tree `Tree';
%%	it represents an Erlang expression `<MetaTree>' which, if
%%	evaluated, yields a representation of the same syntax tree as
%%	`Tree' (but whose actual underlying representation may differ).
%%	The expression represented by MetaTree is guaranteed to be
%%	*implementation independent* with regard to the data structures
%%	used by this module. Annotations on nodes of `Tree' will be
%%	preserved, except for on subterms of constant compound literals.
%%
%%	Any node in `Tree' of type `var' whose list of annotations
%%	contains the atom `meta_var' will remain unchanged in
%%	`MetaTree', except that exactly one occurrence of `meta_var' is
%%	removed from its list of annotations.
%%
%%	The main use of the `meta' function is to transform the *data
%%	structure* `Tree', which represents a piece of program code,
%%	into a form that is *representation independent when printed*.
%%	E.g., suppose `Tree' represents a variable `V'. Then (assuming a
%%	function `print' exists), evaluating `print(abstract(Tree))' -
%%	simply using `abstract' to map the data structure onto a syntax
%%	tree representation - would output a string that might look
%%	something like `{var, ...'V'...}', which is obviously dependent
%%	on the implementation of this module. E.g., for caching syntax
%%	trees, this could be useful. However, in some situations (e.g.,
%%	in a program generator generator), it may be unacceptable. Using
%%	`print(meta(Tree))' instead would output a *representation
%%	independent* string; in the case above, something like
%%	`cerl:c_int('V')'.
%%
%%	The implementation tries to generate compact code with respect
%%	to literals and lists.

meta(Node) ->
    %% First of all we check for metavariables:
    case type(Node) of
	var ->
	    case lists:member(meta_var, get_ann(Node)) of
		false ->
		    meta_0(var, Node);
		true ->
		    %% A meta-variable: remove the first found
		    %% `meta_var' annotation, but otherwise leave
		    %% the node unchanged.
		    set_ann(Node, lists:delete(meta_var, get_ann(Node)))
	    end;
	Type ->
	    meta_0(Type, Node)
    end.

meta_0(Type, Node) ->
    case get_ann(Node) of
	[] ->
	    meta_1(Type, Node);
	As ->
	    meta_call(set_ann, [meta_1(Type, Node), abstract(As)])
    end.

meta_1(literal, Node) ->
    %% We handle atomic literals separately, to get a bit
    %% more compact code. For the rest, we use `abstract'.
    case concrete(Node) of
	V when atom(V) ->
	    meta_call(c_atom, [Node]);
	V when integer(V) ->
	    meta_call(c_int, [Node]);
	V when float(V) ->
	    meta_call(c_float, [Node]);
	[] ->
	    meta_call(c_nil, []);
	_ ->
	    meta_call(abstract, [Node])
    end;
meta_1(var, Node) ->
    %% A normal variable or function name. Unless the class field is the
    %% empty string, we preserve both name and class.
    As = case var_class(Node) of
	     "" ->
		 [];
	     Class ->
		 [abstract(Class)]
	 end,
    meta_call(c_var, [abstract(var_name(Node)) | As]);
meta_1(values, Node) ->
    meta_call(c_values,
	      [make_list(meta_list(values_es(Node)))]);
meta_1(binary, Node) ->
    meta_call(c_binary,
	      [make_list(meta_list(binary_segs(Node)))]);
meta_1(bin_seg, Node) ->
    meta_call(c_bin_seg,
	      [meta(bin_seg_val(Node)),
	       meta(bin_seg_size(Node)),
	       meta(bin_seg_unit(Node)),
	       meta(bin_seg_type(Node)),
	       make_list(meta_list(bin_seg_flags(Node)))]);
meta_1(cons, Node) ->
    %% The list is split up if some sublist has annotatations. If
    %% we get exactly one element, we generate a `c_cons' call
    %% instead of `make_list' to reconstruct the node.
    case split_list(Node) of
	{[H], none} ->
	    meta_call(c_cons, [meta(H), meta(c_nil())]);
	{[H], Node1} ->
	    meta_call(c_cons, [meta(H), meta(Node1)]);
	{L, none} ->
	    meta_call(make_list, [make_list(meta_list(L))]);
	{L, Node1} ->
	    meta_call(make_list,
		      [make_list(meta_list(L)), meta(Node1)])
    end;
meta_1(tuple, Node) ->
    meta_call(c_tuple,
	      [make_list(meta_list(tuple_es(Node)))]);
meta_1('let', Node) ->
    meta_call(c_let,
	      [make_list(meta_list(let_vars(Node))),
	       meta(let_arg(Node)), meta(let_body(Node))]);
meta_1(seq, Node) ->
    meta_call(c_seq,
	      [meta(seq_arg(Node)), meta(seq_body(Node))]);
meta_1(apply, Node) ->
    meta_call(c_apply,
	      [meta(apply_op(Node)),
	       make_list(meta_list(apply_args(Node)))]);
meta_1(call, Node) ->
    meta_call(c_call,
	      [meta(call_module(Node)), meta(call_name(Node)),
	       make_list(meta_list(call_args(Node)))]);
meta_1(primop, Node) ->
    meta_call(c_primop,
	      [meta(primop_name(Node)),
	       make_list(meta_list(primop_args(Node)))]);
meta_1('case', Node) ->
    meta_call(c_case,
	      [meta(case_arg(Node)),
	       make_list(meta_list(case_clauses(Node)))]);
meta_1(clause, Node) ->
    meta_call(c_clause,
	      [make_list(meta_list(clause_pats(Node))),
	       meta(clause_guard(Node)),
	       meta(clause_body(Node))]);
meta_1(alias, Node) ->
    meta_call(c_alias,
	      [meta(alias_var(Node)), meta(alias_pat(Node))]);
meta_1('fun', Node) ->
    meta_call(c_fun,
	      [make_list(meta_list(fun_vars(Node))),
	       meta(fun_body(Node))]);
meta_1('receive', Node) ->
    meta_call(c_receive,
	      [make_list(meta_list(receive_clauses(Node))),
	       meta(receive_timeout(Node)),
	       meta(receive_action(Node))]);
meta_1('try', Node) ->
    meta_call(c_try,
	      [meta(try_expr(Node)),
	       make_list(meta_list(try_vars(Node))),
	       meta(try_body(Node))]);
meta_1('catch', Node) ->
    meta_call(c_catch, [meta(catch_body(Node))]);
meta_1(letrec, Node) ->
    meta_call(c_letrec,
	      [make_list([c_tuple([meta(N), meta(F)])
			  || {N, F} <- letrec_defs(Node)]),
	       meta(letrec_body(Node))]);
meta_1(module, Node) ->
    meta_call(c_module,
	      [meta(module_name(Node)),
	       make_list(meta_list(module_exports(Node))),
	       make_list([c_tuple([meta(A), meta(V)])
			  || {A, V} <- module_attrs(Node)]),
	       make_list([c_tuple([meta(N), meta(F)])
			  || {N, F} <- module_defs(Node)])]).

meta_call(F, As) ->
    c_call(c_atom(this_module()), c_atom(F), As).

meta_list([T | Ts]) ->
    [meta(T) | meta_list(Ts)];
meta_list([]) ->
    [].

split_list(Node) ->
    split_list(set_ann(Node, []), []).

split_list(Node, L) ->
    A = get_ann(Node),
    case type(Node) of
	cons when A == [] ->
	    split_list(cons_tl(Node), [cons_hd(Node) | L]);
	nil when A == [] ->
	    {lists:reverse(L), none};
	_ ->
	    {lists:reverse(L), Node}
    end.


%% =====================================================================
%% General utilities

is_lit_list([#literal{} | Es]) ->
    is_lit_list(Es);
is_lit_list([E | Es]) ->
    false;
is_lit_list([]) ->
    true.

lit_list_vals([#literal{val = V} | Es]) ->
    [V | lit_list_vals(Es)];
lit_list_vals([]) ->
    [].

make_lit_list([V | Vs]) ->
    [#literal{val = V} | make_lit_list(Vs)];
make_lit_list([]) ->
    [].

%% The following tests are the same as done by `io_lib:char_list' and
%% `io_lib:printable_list', respectively, but for a single character.

is_char_value(V) when V >= $\000, V =< $\377 -> true;
is_char_value(_) -> false.

is_print_char_value(V) when V >= $\040, V =< $\176 -> true;
is_print_char_value(V) when V >= $\240, V =< $\377 -> true;
is_print_char_value(V) when V =:= $\b -> true;
is_print_char_value(V) when V =:= $\d -> true;
is_print_char_value(V) when V =:= $\e -> true;
is_print_char_value(V) when V =:= $\f -> true;
is_print_char_value(V) when V =:= $\n -> true;
is_print_char_value(V) when V =:= $\r -> true;
is_print_char_value(V) when V =:= $\s -> true;
is_print_char_value(V) when V =:= $\t -> true;
is_print_char_value(V) when V =:= $\v -> true;
is_print_char_value(V) when V =:= $\" -> true;
is_print_char_value(V) when V =:= $\' -> true;
is_print_char_value(V) when V =:= $\\ -> true;
is_print_char_value(_) -> false.

is_char_list([V | Vs]) when integer(V) ->
    case is_char_value(V) of
	true ->
	    is_char_list(Vs);
	false ->
	    false
    end;
is_char_list([]) ->
    true;
is_char_list(_) ->
    false.

is_print_char_list([V | Vs]) when integer(V) ->
    case is_print_char_value(V) of
	true ->
	    is_print_char_list(Vs);
	false ->
	    false
    end;
is_print_char_list([]) ->
    true;
is_print_char_list(_) ->
    false.

unfold_tuples([{X, Y} | Ps]) ->
    [X, Y | unfold_tuples(Ps)];
unfold_tuples([]) ->
    [].

fold_tuples([X, Y | Es]) ->
    [{X, Y} | fold_tuples(Es)];
fold_tuples([]) ->
    [].

this_module() ->
    module_info(module).

%% =====================================================================
