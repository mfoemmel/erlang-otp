%% -*- erlang-indent-level: 2 -*-
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
%% Original author: Richard Carlsson. Rewritten in whole based on the 
%%                                    original implementation 
%%                                    by Tobias Lindahl
%%
%% Author contact: tobiasl@it.uu.se, richardc@it.uu.se
%%
%% $Id$
%%

-module(erl_types).

-define(WIDENING_LIMIT, 7).
-define(UNIT_MULTIPLIER, 8).

-define(TAG_IMMED1_SIZE, 4).
-define(BITS, (hipe_rtl_arch:word_size() * 8) - ?TAG_IMMED1_SIZE).

%-define(widening_debug, fun(X, Y) -> io:format("widening: ~s ~p~n", [X, Y]) end).
-define(widening_debug, fun(_, _) -> ok end).

-export([
	 lookup_record/3,
	 type_is_defined/2,
	 max/2,
	 min/2,
	 number_max/1,
	 number_min/1,
	 t_abstract_records/2,
	 t_any/0,
	 t_arity/0,
	 t_atom/0,
	 t_atom/1,
	 t_atom_vals/1,
	 t_binary/0,
	 t_bitstr/0,
	 t_bitstr/2,
	 t_bitstr_base/1,
	 t_bitstr_concat/1,
	 t_bitstr_concat/2,
	 t_bitstr_match/2,
	 t_bitstr_unit/1,
	 t_bool/0,
	 t_byte/0,
	 t_char/0,
	 t_collect_vars/1,
	 t_components/1,
	 t_cons/0,
	 t_cons/2,
	 t_cons_hd/1,
	 t_cons_tl/1,
	 t_constant/0,
	 t_fixnum/0,
	 t_map/2,
	 t_non_neg_fixnum/0,
	 t_pos_fixnum/0,
	 t_float/0,
	 t_form_to_string/1,
	 t_from_form/1,
	 t_from_form/2,
	 t_from_form/3,
	 t_from_range/2,
	 t_from_range_unsafe/2,
	 t_from_term/1,
	 t_fun/0,
	 t_fun/1,
	 t_fun/2,
	 t_fun_args/1,
	 t_fun_arity/1,
	 t_fun_range/1,
	 t_has_var/1,
	 t_identifier/0,
	 %% t_improper_list/2,
	 t_inf/2,
	 t_inf_lists/2,
	 t_integer/0,
	 t_integer/1,
	 t_iolist/0,
	 t_non_neg_integer/0,
	 t_pos_integer/0,
	 t_integers/1,
	 t_is_any/1,
	 t_is_atom/1,
	 t_is_atom/2,
	 t_is_binary/1,
	 t_is_bitstr/1,
	 t_is_bitwidth/1,
	 t_is_bool/1,
	 %% t_is_byte/1,
	 %% t_is_char/1,
	 t_is_cons/1,
	 t_is_constant/1,
	 t_is_equal/2,
	 t_is_fixnum/1,
	 t_is_float/1,
	 t_is_fun/1,
	 t_is_integer/1,
	 t_is_list/1,
	 t_is_matchstate/1,
	 t_is_nil/1,
	 t_is_non_neg_integer/1,
	 t_is_none/1,
	 t_is_none_or_unit/1,
	 t_is_number/1,	 
	 t_is_pid/1,
	 t_is_port/1,
	 t_is_maybe_improper_list/1,
	 t_is_ref/1,
	 t_is_string/1,
	 t_is_subtype/2,
	 t_is_tuple/1,
	 t_is_unit/1,
	 t_is_var/1,
	 t_limit/2,
	 t_list/0,
	 t_list/1,
	 t_list_elements/1,
	 t_list_termination/1,
	 t_matchstate/0,
	 t_matchstate/2,
	 t_matchstate_present/1,
	 t_matchstate_slot/2,
	 t_matchstate_slots/1,
	 t_matchstate_update_present/2,
	 t_matchstate_update_slot/3,
	 t_mfa/0,
	 t_nil/0,	 
	 t_none/0,
	 t_nonempty_list/0,
	 t_nonempty_list/1,
	 t_number/0,
	 t_number/1,
	 t_number_vals/1,
	 t_pid/0,
	 t_port/0,
	 t_maybe_improper_list/0,
	 %% t_maybe_improper_list/2,
	 t_product/1,
	 t_ref/0,
	 t_string/0,
	 t_subst/2,
	 t_subtract/2,
	 t_subtract_list/2,
	 t_sup/1,
	 t_sup/2,
	 t_to_string/1,
	 t_to_string/2,
	 t_tuple/0,
	 t_tuple/1,
	 t_tuple_args/1,
	 t_tuple_arities/1,
	 t_tuple_arity/1,
	 t_tuple_subtypes/1,
	 t_unify/2,
	 t_unit/0,
	 t_var/1,
	 t_var_name/1,
	 %%t_assign_variables_to_subtype/2,
	 subst_all_vars_to_any/1,
	 any_none_or_unit/1,
	 lift_list_to_pos_empty/1
	]).

%%-define(DO_ERL_TYPES_TEST, true).

-ifdef(DO_ERL_TYPES_TEST).
-export([test/0]).
-else.
-define(NO_UNUSED, true).
-endif.

-ifndef(NO_UNUSED).
-export([t_is_identifier/1]).
-endif.

%%============================================================================
%% 
%% Definition of the type structure
%%
%%============================================================================

%%-----------------------------------------------------------------------------
%% Limits
%%

-define(TUPLE_TAG_LIMIT, 5).
-define(TUPLE_ARITY_LIMIT, 10).
-define(SET_LIMIT, 13).
-define(MAX_BYTE, 255).
-define(MAX_CHAR, 16#10ffff).

%%-----------------------------------------------------------------------------
%% Tags
%%

-define(atom_tag,       atom).
-define(binary_tag,     binary).
-define(float_tag,      float).
-define(function_tag,   function).
-define(identifier_tag, identifier).
-define(int_tag,        int).
-define(list_tag,       list).
-define(nil_tag,        nil).
-define(nonempty_tag,   nonempty).
-define(number_tag,     number).
-define(pid_tag,        pid).
-define(port_tag,       port).
-define(product_tag,    product).
-define(ref_tag,        ref).
-define(tuple_set_tag,  tuple_set).
-define(tuple_tag,      tuple).
-define(record_tag,     record).
-define(union_tag,      union).
-define(var_tag,        var).
-define(matchstate_tag, matchstate).

%%-----------------------------------------------------------------------------
%% Primitive types
%%

-define(any, any).
-define(none, none).
-define(unit, unit).

-record(c, {tag::atom(), elements=[], qualifier=?any}). %% Generic constructor.
-record(int_set, {set :: list()}).
-record(int_range, {from :: 'pos_inf' | 'neg_inf' | integer(),
		    to   :: 'pos_inf' | 'neg_inf' | integer()}).

-define(atom(Set),                 #c{tag=?atom_tag, elements=Set}).
-define(bitstr(Unit,Base),         #c{tag=?binary_tag, elements=[Unit,Base]}).
-define(float,                     ?number(?any, ?float_tag)).
-define(function(Domain, Range),   #c{tag=?function_tag, 
				      elements=[Domain, Range]}).
-define(identifier(Types),         #c{tag=?identifier_tag, elements=Types}).
-define(integer(Types),            ?number(Types, ?int_tag)).
-define(int_range(From, To),       ?integer(#int_range{from=From, to=To})).
-define(int_set(Set),              ?integer(#int_set{set=Set})).
-define(list(Types, Term, Size),   #c{tag=?list_tag, elements=[Types,Term],
				      qualifier=Size}).
-define(nil,                       #c{tag=?nil_tag}).
-define(nonempty_list(Types, Term),?list(Types, Term, ?nonempty_tag)).
-define(number(Set, Tag),          #c{tag=?number_tag, elements=Set, 
				      qualifier=Tag}.
-define(product(Types),            #c{tag=?product_tag, elements=Types}).
-define(tuple(Types, Arity, Tag),  #c{tag=?tuple_tag, elements=Types, 
				      qualifier={Arity, Tag}}).
-define(tuple_set(Tuples),         #c{tag=?tuple_set_tag, elements=Tuples}).
-define(var(Id),                   #c{tag=?var_tag, elements=Id}).
-define(matchstate(P,Slots),	   #c{tag=?matchstate_tag, elements=[P,Slots]}).
-define(any_matchstate,            ?matchstate(t_bitstr(), ?any)).

-define(byte,                      ?int_range(0, ?MAX_BYTE)).
-define(char,                      ?int_range(0, ?MAX_CHAR)).
-define(integer_pos,               ?int_range(1, pos_inf)).
-define(integer_non_neg,           ?int_range(0, pos_inf)).
-define(integer_neg,               ?int_range(neg_inf, -1)).


%%-----------------------------------------------------------------------------
%% Unions
%%

-define(union(List),        #c{tag=?union_tag, elements=[_,_,_,_,_,_,_,_]=List}).

-define(atom_union(T),        ?union([T,?none,?none,?none,?none,?none,?none,?none])).
-define(bitstr_union(T),      ?union([?none,T,?none,?none,?none,?none,?none,?none])).
-define(function_union(T),    ?union([?none,?none,T,?none,?none,?none,?none,?none])).
-define(identifier_union(T),  ?union([?none,?none,?none,T,?none,?none,?none,?none])).
-define(list_union(T),        ?union([?none,?none,?none,?none,T,?none,?none,?none])).
-define(number_union(T),      ?union([?none,?none,?none,?none,?none,T,?none,?none])).
-define(tuple_union(T),       ?union([?none,?none,?none,?none,?none,?none,T,?none])).
-define(matchstate_union(T),  ?union([?none,?none,?none,?none,?none,?none,?none,T])).

-define(integer_union(T),     ?number_union(T)).
-define(float_union(T),       ?number_union(T)).
-define(nil_union(T),         ?list_union(T)).


%%=============================================================================
%% 
%% Primitive operations such as type construction and type tests.
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Top and bottom
%%

t_any() ->
  ?any.

t_is_any(?any) -> true;
t_is_any(_) -> false.

t_none() ->
  ?none.

t_is_none(?none) -> true;
t_is_none(_) -> false.

%%-----------------------------------------------------------------------------
%% Unit type. Signals non termination.
%%

t_unit() ->
  ?unit.

t_is_unit(?unit) -> true;
t_is_unit(_) -> false.  

t_is_none_or_unit(?none) -> true;
t_is_none_or_unit(?unit) -> true;
t_is_none_or_unit(_) -> false.
  
%%-----------------------------------------------------------------------------
%% Atoms and the derived type bool.
%%

t_atom() ->
  ?atom(?any).

t_atom(A) when is_atom(A) ->
  ?atom(set_singleton(A)).

t_atom_vals(?atom(Set)) ->
  set_to_list(Set);
t_atom_vals(Other) ->
  case t_inf(t_atom(), Other) of
    ?atom(Set) -> set_to_list(Set);
    ?none -> ?none
  end.
      
t_is_atom(?atom(_)) -> true;
t_is_atom(_) -> false.

t_is_atom(Atom, ?atom(Set)) when is_atom(Atom) ->
  (set_size(Set) =:= 1) andalso (set_is_element(Atom, Set));
t_is_atom(Atom, _) when is_atom(Atom) ->
  false.

%%------------------------------------

t_bool() ->
  ?atom(set_from_list([false, true])).

t_is_bool(?atom(?any)) -> false;
t_is_bool(?atom(Set)) ->
  case set_size(Set) of
    1 -> set_is_element(true, Set) orelse set_is_element(false, Set);
    2 -> set_is_element(true, Set) andalso set_is_element(false, Set);
    N when is_integer(N), N > 2 -> false
  end;
t_is_bool(_) -> false.

%%-----------------------------------------------------------------------------
%% Binaries
%%

t_binary() ->
  ?bitstr(8,0).

t_is_binary(?bitstr(U,B)) -> 
  ((U rem 8) =:= 0) and ((B rem 8) =:= 0);
t_is_binary(_) -> false.


%%-----------------------------------------------------------------------------
%% Binaries
%%

t_bitstr() ->
  ?bitstr(1,0).

t_bitstr(U,B) ->
  NewB = 
    if 
      U =:= 0 -> B;
      B >= (U * (?UNIT_MULTIPLIER+1)) ->
	(B rem U) + U * ?UNIT_MULTIPLIER;
       true ->
	B
    end,
  ?bitstr(U,NewB).

t_bitstr_base(?bitstr(_,B)) -> B.

t_bitstr_concat(List) ->
  t_bitstr_concat_1(List, t_bitstr(0,0)).

t_bitstr_concat_1([T|Left], Acc) ->
  t_bitstr_concat_1(Left, t_bitstr_concat(Acc, T));
t_bitstr_concat_1([], Acc) ->
  Acc.

t_bitstr_concat(T1,T2) ->
  T1p = t_inf(t_bitstr(),T1),
  T2p = t_inf(t_bitstr(),T2),
  bitstr_concat(T1p,T2p).

t_bitstr_match(T1,T2) ->
  T1p = t_inf(t_bitstr(),T1),
  T2p = t_inf(t_bitstr(),T2),
  bitstr_match(T1p,T2p).

t_bitstr_unit(?bitstr(U,_)) -> U.

t_is_bitstr(?bitstr(_,_)) -> true;
t_is_bitstr(_) -> false.


%%-----------------------------------------------------------------------------
%% Matchstates
%%


t_matchstate() ->
  ?any_matchstate.

t_matchstate(Init,0) ->
  ?matchstate(Init,Init);
t_matchstate(Init,Max) when is_integer(Max) ->
  Slots = [Init|[?none || _ <- lists:seq(1,Max)]],
  ?matchstate(Init,t_product(Slots)).

t_is_matchstate(?matchstate(_,_)) -> true;
t_is_matchstate(_) -> false.

t_matchstate_present(Type) -> 
  case t_inf(t_matchstate(),Type) of
    ?matchstate(P,_) -> P;
    _ -> ?none
  end.

t_matchstate_slot(Type, Slot) ->
  RealSlot = Slot+1,
  case t_inf(t_matchstate(),Type) of
    ?matchstate(_,?any) -> ?any;
    ?matchstate(_,?product(Vals)) when length(Vals) >= RealSlot ->
      lists:nth(RealSlot,Vals);
    ?matchstate(_,?product(_)) ->
      ?none;
    ?matchstate(_,SlotType) when RealSlot =:= 1 ->
      SlotType;
    _ ->
      ?none
  end.

t_matchstate_slots(?matchstate(_,Slots)) ->
  Slots.

t_matchstate_update_present(New, Type) -> 
  case t_inf(t_matchstate(),Type) of
    ?matchstate(_,Slots) ->
      ?matchstate(New,Slots);
    _ -> ?none
  end.
t_matchstate_update_slot(New, Type, Slot) -> 
  RealSlot = Slot+1,
   case t_inf(t_matchstate(),Type) of
    ?matchstate(Pres,Slots) ->
       NewSlots = 
	 case Slots of
	   ?any ->
	     ?any;
	   ?product(Vals) when length(Vals) >= RealSlot ->
	     NewVals = tuple_to_list(setelement(RealSlot,
						list_to_tuple(Vals),
						New)),
	     ?product(NewVals);
	   ?product(_) ->
	     ?none;
	   _ when RealSlot =:= 1 ->
	     New;
	   _ ->
	     ?none
	 end,
       ?matchstate(Pres,NewSlots);
     _ ->
       ?none
   end.

%%-----------------------------------------------------------------------------
%% Functions
%%

t_fun() ->
  ?function(?any, ?any).

t_fun(Range) ->
  ?function(?any, Range).

t_fun(Domain, Range) when is_list(Domain) ->
  ?function(?product(Domain), Range);
t_fun(Arity, Range) when is_integer(Arity) ->
  ?function(?product(lists:duplicate(Arity, ?any)), Range).

t_fun_args(?function(?product(Domain), _)) -> 
  Domain;
t_fun_args(?function(?any, _)) ->
  ?any.

t_fun_arity(?function(?any, _)) ->
  ?any;
t_fun_arity(?function(?product(Domain), _)) ->
  length(Domain).

t_fun_range(?function(_, Range)) ->
  Range.

t_is_fun(?function(_, _)) -> true;
t_is_fun(_) -> false.

%%-----------------------------------------------------------------------------
%% Identifiers. Includes ports, pids and refs.
%% 

t_identifier() ->
  ?identifier(?any).

-ifdef(DO_ERL_TYPES_TEST).
t_is_identifier(?identifier(_)) -> true;
t_is_identifier(_) -> false.
-endif.

%%------------------------------------

t_port() ->
  ?identifier(set_singleton(?port_tag)).

t_is_port(?identifier(Set)) -> 
  (set_size(Set) =:= 1) andalso set_is_element(?port_tag, Set);
t_is_port(_) -> false.

%%------------------------------------

t_pid() ->
  ?identifier(set_singleton(?pid_tag)).

t_is_pid(?identifier(Set)) -> 
  (set_size(Set) =:= 1) andalso set_is_element(?pid_tag, Set);
t_is_pid(_) -> false.

%%------------------------------------

t_ref() ->
  ?identifier(set_singleton(?ref_tag)).

t_is_ref(?identifier(Set)) -> 
  (set_size(Set) =:= 1) andalso set_is_element(?ref_tag, Set);
t_is_ref(_) -> false.

%%-----------------------------------------------------------------------------
%% Numbers are divided into floats, integers, chars and bytes.
%%

t_number() ->
  ?number(?any, ?number_tag).

t_number(X) when is_integer(X) ->
  t_integer(X).

t_is_number(?number(_, _)) -> true;
t_is_number(_) -> false.

t_number_vals(?int_set(Set)) -> set_to_list(Set);
t_number_vals(?number(_, _)) -> ?any;
t_number_vals(Other) ->
  Inf = t_inf(Other, t_number()),
  case t_is_none(Inf) of
    true -> [];
    false -> t_number_vals(Inf)
  end.

%%------------------------------------

t_float() ->
  ?float.

t_is_float(?float) -> true;
t_is_float(_) -> false.

%%------------------------------------

t_integer() ->
  ?integer(?any).

t_integer(I) when is_integer(I) ->
  ?int_set(set_singleton(I)).

t_integers(List) when is_list(List) ->
  t_sup([t_integer(I) || I <- List]).

t_is_integer(?integer(_)) -> true;
t_is_integer(_) -> false.

%%------------------------------------

t_byte() ->
  ?byte.

-ifdef(DO_ERL_TYPES_TEST).
t_is_byte(?int_range(neg_inf, _)) -> false;
t_is_byte(?int_range(_, pos_inf)) -> false;
t_is_byte(?int_range(From, To))
  when is_integer(From), From >= 0, is_integer(To), To =< ?MAX_BYTE -> true;
t_is_byte(?int_set(Set)) -> 
  (set_min(Set) >= 0) andalso (set_max(Set) =< ?MAX_BYTE);
t_is_byte(_) -> false.
-endif.

%%------------------------------------

t_char() ->
  ?char.

t_is_char(?int_range(neg_inf, _)) -> false;
t_is_char(?int_range(_, pos_inf)) -> false;
t_is_char(?int_range(From, To))
  when is_integer(From), From >= 0, is_integer(To), To =< ?MAX_CHAR -> true;
t_is_char(?int_set(Set)) -> 
  (set_min(Set) >= 0) andalso (set_max(Set) =< ?MAX_CHAR);
t_is_char(_) -> false.

%%-----------------------------------------------------------------------------
%% Lists
%%

t_cons() ->
  ?nonempty_list(?any, ?any).


%% Note that if the tail argument can be a list, we must collapse the
%% content of the list to include both the content of the tail list
%% and the head of the cons. If for example the tail argument is any()
%% then there can be any list in the tail and the content of the
%% returned list must be any().

t_cons(?none,  _) -> ?none;
t_cons(_, ?none) -> ?none;
t_cons(?unit, _) -> ?none;
t_cons(_, ?unit) -> ?none;
t_cons(Hd, ?nil) ->
  ?nonempty_list(Hd, ?nil);
t_cons(Hd, ?list(Contents, Termination, _)) ->
  ?nonempty_list(t_sup(Contents, Hd), Termination);
t_cons(Hd, Tail) ->
  case t_inf(Tail, t_maybe_improper_list()) of
    ?list(Contents, Termination, _Size) ->
      %% Collapse the list part of the termination but keep the
      %% non-list part intact.
      NewTermination = t_sup(t_subtract(Tail, t_maybe_improper_list()), 
			     Termination),
      ?nonempty_list(t_sup(Hd, Contents), NewTermination);
    ?nil -> ?nonempty_list(Hd, Tail);
    ?none -> ?nonempty_list(Hd, Tail);
    ?unit -> ?none
  end.
  
t_is_cons(?nonempty_list(_, _)) -> true;
t_is_cons(_) -> false.  

t_cons_hd(?nonempty_list(Contents, _Termination)) -> Contents.

t_cons_tl(T = ?nonempty_list(_Contents, Termination)) ->
  t_sup(Termination, T).

t_nil() ->
  ?nil.

t_is_nil(?nil) -> true;
t_is_nil(_) -> false.

t_list() ->  
  ?list(?any, ?nil, ?any).

t_list(?none) -> ?none;
t_list(?unit) -> ?none;
t_list(Contents) ->
  ?list(Contents, ?nil, ?any).

t_list_elements(?list(Contents, _, _)) -> Contents;
t_list_elements(?nil) -> ?none.


t_list_termination(?nil) -> ?nil;
t_list_termination(?list(_, Term, _)) -> Term.

t_is_list(?list(_Contents, ?nil, _)) -> true;
t_is_list(?nil) -> true;
t_is_list(_) -> false.

t_nonempty_list() ->
  t_cons(?any, ?nil).

t_nonempty_list(Type) ->
  t_cons(Type, ?nil).

t_string() ->
  t_list(t_char()).

t_is_string(X) ->
  t_is_list(X) andalso (t_is_char(t_list_elements(X))).

t_maybe_improper_list() ->
  ?list(?any, ?any, ?any).

%% Should only be used if you know what you are doing. See t_cons/2
t_maybe_improper_list(_Content, ?unit) -> ?none;
t_maybe_improper_list(?unit, _Termination) -> ?none;
t_maybe_improper_list(Content, Termination) ->
  %% Safety check
  true = t_is_subtype(t_nil(), Termination),
  ?list(Content, Termination, ?any).

t_is_maybe_improper_list(?list(_, _, _)) -> true;
t_is_maybe_improper_list(?nil) -> true;
t_is_maybe_improper_list(_) -> false.

%% %% Should only be used if you know what you are doing. See t_cons/2
%% t_improper_list(?unit, _Termination) -> ?none;
%% t_improper_list(_Content, ?unit) -> ?none;
%% t_improper_list(Content, Termination) ->
%%   %% Safety check
%%   false = t_is_subtype(t_nil(), Termination),
%%   ?list(Content, Termination, ?any).  

lift_list_to_pos_empty(?nil) -> ?nil;
lift_list_to_pos_empty(?list(Content, Termination, _)) -> 
  ?list(Content, Termination, ?any).
  

%%-----------------------------------------------------------------------------
%% Tuples
%%

t_tuple() ->
  ?tuple(?any, ?any, ?any).

t_tuple(N) when is_integer(N) ->
  ?tuple(lists:duplicate(N, ?any), N, ?any);
t_tuple(List) ->
  case any_none_or_unit(List) of
    true -> t_none();
    false ->
      Arity = length(List),
      case get_tuple_tag(List) of
	?any -> ?tuple(List, Arity, ?any);
	[Tag] -> ?tuple(List, Arity, Tag);
	TagList -> 
	  SortedTagList = lists:sort(TagList),
	  Tuples = [?tuple([T|tl(List)], Arity, T) || T <- SortedTagList],
	  ?tuple_set([{Arity, Tuples}])
      end
  end.

get_tuple_tag([?atom(?any)|_]) -> ?any;
get_tuple_tag([?atom(Set)|_]) ->
  case set_size(Set) > ?TUPLE_TAG_LIMIT of
    true -> ?any;
    false -> [t_atom(A) || A <- set_to_list(Set)]
  end;
get_tuple_tag(_) -> ?any.

t_tuple_args(?tuple(?any, ?any, ?any)) ->
  ?any;
t_tuple_args(?tuple(List, _, _)) ->
  List.

t_tuple_arity(?tuple(?any, ?any, ?any)) ->
  ?any;
t_tuple_arity(?tuple(_, Arity, _)) when is_integer(Arity) ->
  Arity.

t_tuple_arities(?tuple(?any, ?any, ?any)) ->
  ?any;
t_tuple_arities(?tuple(_, Arity, _)) when is_integer(Arity) ->
  [Arity];
t_tuple_arities(?tuple_set(List)) ->
  [Arity || {Arity, _} <- List].

t_tuple_subtypes(?tuple(?any, ?any, ?any)) -> ?any;
t_tuple_subtypes(T = ?tuple(_, _, _)) -> [T];
t_tuple_subtypes(?tuple_set(List)) -> 
  lists:append([Tuple || {_Arity, Tuple} <- List]).

t_is_tuple(?tuple(_, _, _)) -> true;
t_is_tuple(?tuple_set(_)) -> true;
t_is_tuple(_) -> false.

%%-----------------------------------------------------------------------------
%% Non-primitive types, including some handy syntactic-sugar types
%%

t_constant() ->
  t_sup([t_number(), t_identifier(), t_atom(), t_fun(), t_binary()]).

t_is_constant(X) ->
  t_is_subtype(X, t_constant()).

t_arity() ->
  t_from_range(0, 255).	% was t_byte().

t_pos_integer() ->
  t_from_range(1, pos_inf).

t_non_neg_integer() ->
  t_from_range(0, pos_inf).

t_is_non_neg_integer(?integer(_) = T) ->
  t_is_subtype(T, t_non_neg_integer());
t_is_non_neg_integer(_) -> false.

t_neg_integer() ->
  t_from_range(neg_inf, -1).

t_fixnum() ->
  t_integer(). % Gross over-approximation

t_pos_fixnum() ->
  t_pos_integer().  % Gross over-approximation

t_non_neg_fixnum() ->
  t_non_neg_integer().  % Gross over-approximation

t_mfa() ->
  t_tuple([t_atom(), t_atom(), t_arity()]).

t_iolist() ->
  t_iolist(1).

t_iolist(N) when N > 0 ->
  t_maybe_improper_list(t_sup([t_iolist(N-1),t_binary(),t_byte()]),
		      t_sup(t_binary(),t_nil()));
t_iolist(0) ->
  t_maybe_improper_list(t_any(),t_sup(t_binary(),t_nil())).

%%------------------------------------

%% ?none is allowed in products. A product of size 1 is not a product.

t_product([T]) -> T;
t_product(Types) when is_list(Types) ->
  ?product(Types).

t_components(?product(Types)) -> Types;
t_components(?any) -> ?any;
t_components(?none) -> ?none;
t_components(?unit) -> ?unit;
t_components(T) -> [T].
  
%%------------------------------------

t_var(Atom) when is_atom(Atom) -> ?var(Atom);
t_var(Int) when is_integer(Int)-> ?var(Int).

t_is_var(?var(_)) -> true;
t_is_var(_) -> false.

t_var_name(?var(Id)) -> Id.

t_has_var(?var(_)) -> true;
t_has_var(?function(Domain, Range)) -> 
  t_has_var(Domain) orelse t_has_var(Range);
t_has_var(?list(Contents, Termination, _)) ->
  t_has_var(Contents) orelse t_has_var(Termination);
t_has_var(?product(Types)) -> t_has_var_list(Types);
t_has_var(?tuple(?any, ?any, ?any)) -> false;
t_has_var(?tuple(Elements, _, _)) ->
  t_has_var_list(Elements);
t_has_var(T = ?tuple_set(_)) ->
  t_has_var_list(t_tuple_subtypes(T));
t_has_var(_) -> false.

t_has_var_list([T|Left]) ->
  t_has_var(T) orelse t_has_var_list(Left);
t_has_var_list([]) -> false.

t_collect_vars(T) ->
  t_collect_vars(T, []).

t_collect_vars(Var = ?var(_), Acc) -> 
  ordsets:add_element(Var, Acc);
t_collect_vars(?function(Domain, Range), Acc) -> 
  ordsets:union(t_collect_vars(Domain, Acc), t_collect_vars(Range, []));
t_collect_vars(?list(Contents, Termination, _), Acc) ->
  ordsets:union(t_collect_vars(Contents, Acc), t_collect_vars(Termination, []));
t_collect_vars(?product(Types), Acc) -> 
  lists:foldl(fun(T, TmpAcc) -> t_collect_vars(T, TmpAcc) end, Acc, Types);
t_collect_vars(?tuple(?any, ?any, ?any), Acc) -> 
  Acc;
t_collect_vars(?tuple(Types, _, _), Acc) ->
  lists:foldl(fun(T, TmpAcc) -> t_collect_vars(T, TmpAcc) end, Acc, Types);
t_collect_vars(TS = ?tuple_set(_), Acc) ->
  lists:foldl(fun(T, TmpAcc) -> t_collect_vars(T, TmpAcc) end, Acc, 
	      t_tuple_subtypes(TS));
t_collect_vars(_, Acc) -> 
  Acc.


%%============================================================================
%% 
%% Type construction from Erlang terms.
%%
%%============================================================================

%%-----------------------------------------------------------------------------
%% Make a type from a term. No type depth is enforced.
%%

t_from_term([H|T]) ->                  t_cons(t_from_term(H), t_from_term(T));
t_from_term([]) ->                     t_nil();
t_from_term(T) when is_atom(T) ->      t_atom(T);
t_from_term(T) when is_bitstring(T) -> t_bitstr(0, erlang:bit_size(T));
t_from_term(T) when is_float(T) ->     t_float();
t_from_term(T) when is_function(T) ->
  {arity, Arity} = erlang:fun_info(T, arity),
  t_fun(Arity, t_any());
t_from_term(T) when is_integer(T) ->   t_integer(T);
t_from_term(T) when is_pid(T) ->       t_pid();
t_from_term(T) when is_port(T) ->      t_port();
t_from_term(T) when is_reference(T) -> t_ref();
t_from_term(T) when is_tuple(T) ->     t_tuple([t_from_term(E) 
						|| E <- tuple_to_list(T)]).

%%-----------------------------------------------------------------------------
%% Integer types from a range.
%%-----------------------------------------------------------------------------

%%-define(USE_UNSAFE_RANGES, true).

-ifdef(USE_UNSAFE_RANGES).

t_from_range(X,Y) ->
  t_from_range_unsafe(X,Y).

-else.
t_from_range(neg_inf, pos_inf) -> t_integer();
t_from_range(neg_inf, Y) when is_integer(Y), Y < 0  -> ?integer_neg;
t_from_range(neg_inf, Y) when is_integer(Y), Y >= 0 -> t_integer();
t_from_range(X, pos_inf) when is_integer(X), X >= 1 -> ?integer_pos;
t_from_range(X, pos_inf) when is_integer(X), X >= 0 -> ?integer_non_neg;
t_from_range(X, pos_inf) when is_integer(X), X < 0  -> t_integer();
t_from_range(X, Y) when is_integer(X), is_integer(Y), X > Y -> t_none();
t_from_range(X, Y) when is_integer(X), is_integer(Y) ->
  case ((Y - X) < ?SET_LIMIT) of 
    true -> t_integers(lists:seq(X, Y));
    false ->
      case X >= 0 of
	false -> 
	  if Y < 0 -> ?integer_neg;
	     true -> t_integer()
	  end;
	true ->
	  if Y =< ?MAX_BYTE, X >= 1 -> ?int_range(1, ?MAX_BYTE);
	     Y =< ?MAX_BYTE -> t_byte();
	     Y =< ?MAX_CHAR, X >= 1 -> ?int_range(1, ?MAX_CHAR);
	     Y =< ?MAX_CHAR -> t_char();
	     X >= 1         -> ?integer_pos;
	     X >= 0         -> ?integer_non_neg
	  end
      end
  end;
t_from_range(pos_inf, neg_inf) -> t_none().

-endif.

t_from_range_unsafe(neg_inf, pos_inf) -> t_integer();
t_from_range_unsafe(neg_inf, Y) -> ?int_range(neg_inf, Y);
t_from_range_unsafe(X, pos_inf) -> ?int_range(X, pos_inf);
t_from_range_unsafe(X, Y) when is_integer(X), is_integer(Y), X =< Y ->
  if (Y - X) < ?SET_LIMIT -> t_integers(lists:seq(X, Y));
     true -> ?int_range(X, Y)
  end;
t_from_range_unsafe(X, Y) when is_integer(X), is_integer(Y) -> t_none();
t_from_range_unsafe(pos_inf, neg_inf) -> t_none().

t_is_fixnum(?int_range(neg_inf, _)) -> false;
t_is_fixnum(?int_range(_, pos_inf)) -> false;
t_is_fixnum(?int_range(From, To)) ->
  hipe_tagscheme:is_fixnum(From) andalso hipe_tagscheme:is_fixnum(To);
t_is_fixnum(?int_set(Set)) ->
  hipe_tagscheme:is_fixnum(set_min(Set)) 
    andalso hipe_tagscheme:is_fixnum(set_max(Set));
t_is_fixnum(_) -> false.

infinity_geq(pos_inf, _) -> true;
infinity_geq(_, pos_inf) -> false;
infinity_geq(_, neg_inf) -> true;
infinity_geq(neg_inf, _) -> false;
infinity_geq(A, B) -> A >= B.

t_is_bitwidth(?int_range(neg_inf, _)) -> false;
t_is_bitwidth(?int_range(_, pos_inf)) -> false;
t_is_bitwidth(?int_range(From, To)) ->
  infinity_geq(From, 0) andalso infinity_geq(?BITS, To);
t_is_bitwidth(?int_set(Set)) ->
  infinity_geq(set_min(Set), 0) andalso infinity_geq(?BITS, set_max(Set));
t_is_bitwidth(_) -> false.

number_min(?int_range(From, _)) -> From;
number_min(?int_set(Set)) -> set_min(Set);
number_min(?number(?any, _Tag)) -> neg_inf.

number_max(?int_range(_, To)) -> To;
number_max(?int_set(Set)) -> set_max(Set);
number_max(?number(?any, _Tag)) -> pos_inf.


%% int_range(neg_inf, pos_inf)         -> t_integer();
%% int_range(neg_inf, To)              -> ?int_range(neg_inf, To);
%% int_range(From, pos_inf)            -> ?int_range(From, pos_inf);
%% int_range(From, To) when From =< To -> t_from_range(From, To);
%% int_range(From, To) when To < From  -> ?none.  

in_range(_, ?int_range(neg_inf, pos_inf)) -> true;
in_range(X, ?int_range(From, pos_inf))    -> X >= From;
in_range(X, ?int_range(neg_inf, To))      -> X =< To;
in_range(X, ?int_range(From, To))         -> (X >= From) andalso (X =< To).

max(neg_inf, Y) -> Y;
max(X, neg_inf) -> X;
max(pos_inf, _) -> pos_inf;
max(_, pos_inf) -> pos_inf;
max(X, Y) when X =< Y -> Y;
max(X, _) -> X.

min(neg_inf, _) -> neg_inf;
min(_, neg_inf) -> neg_inf;
min(pos_inf, Y) -> Y;
min(X, pos_inf) -> X;
min(X, Y) when X =< Y -> X;
min(_, Y) -> Y.
  
expand_range_from_set(Range = ?int_range(From, To), Set) ->
  Min = min(set_min(Set), From),
  Max = max(set_max(Set), To),
  if From =:= Min, To =:= Max -> Range;
     true -> t_from_range(Min, Max)
  end.

%%============================================================================
%% 
%% Lattice operations
%%
%%============================================================================

%%-----------------------------------------------------------------------------
%% Supremum
%%

t_sup([?any|_]) ->
  ?any;
t_sup([H1, H2|T]) ->
  t_sup([t_sup(H1, H2)|T]);
t_sup([H]) ->
  subst_all_vars_to_any(H);
t_sup([]) ->
  ?none.  

t_sup(?any, _) -> ?any;
t_sup(_, ?any) -> ?any;
t_sup(?none, T) -> T;
t_sup(T, ?none) -> T;
t_sup(?unit, T) -> T;
t_sup(T, ?unit) -> T;
t_sup(T, T) -> subst_all_vars_to_any(T);
t_sup(?var(_), _) -> ?any;
t_sup(_, ?var(_)) -> ?any;
t_sup(?atom(Set1), ?atom(Set2)) ->
  ?atom(set_union(Set1, Set2));
t_sup(?bitstr(U1,B1),?bitstr(U2,B2)) ->
  t_bitstr(gcd(gcd(U1,U2),abs(B1-B2)), lists:min([B1,B2]));
t_sup(?function(Domain1, Range1), ?function(Domain2, Range2)) ->
  %% The domain is either a product or any.
  ?function(t_sup(Domain1, Domain2), t_sup(Range1, Range2));
t_sup(?identifier(Set1), ?identifier(Set2)) ->
  ?identifier(set_union(Set1, Set2));
t_sup(?matchstate(Pres1,Slots1),
      ?matchstate(Pres2,Slots2)) ->
  ?matchstate(t_sup(Pres1,Pres2),
	      t_sup(Slots1,Slots2));
t_sup(?nil, ?nil) -> ?nil;
t_sup(?nil, ?list(Contents, Termination, _)) ->
  ?list(Contents, t_sup(?nil, Termination), ?any);
t_sup(?list(Contents, Termination, _), ?nil) ->
  ?list(Contents, t_sup(?nil, Termination), ?any);
t_sup(?list(Contents1, Termination1, Size1), 
      ?list(Contents2, Termination2, Size2)) ->
  NewSize =
    case {Size1, Size2} of
      {?any, ?any} -> ?any;
      {?any, ?nonempty_tag} -> ?any;
      {?nonempty_tag, ?any} -> ?any;
      {?nonempty_tag, ?nonempty_tag} -> ?nonempty_tag
    end,
  NewContents = t_sup(Contents1, Contents2),
  NewTermination = t_sup(Termination1, Termination2),
  TmpList = t_cons(NewContents, NewTermination),
  case NewSize of
    ?nonempty_tag -> TmpList;
    ?any -> 
      ?list(FinalContents, FinalTermination, _) = TmpList,
      ?list(FinalContents, FinalTermination, ?any)
  end;
t_sup(?number(_, _), T = ?number(?any, ?number_tag)) -> T;  
t_sup(T = ?number(?any, ?number_tag), ?number(_, _)) -> T;
t_sup(?float, ?float) -> ?float;
t_sup(?float, ?integer(_)) -> t_number();
t_sup(?integer(_), ?float) -> t_number();
t_sup(T = ?integer(?any), ?integer(_)) -> T;
t_sup(?integer(_), T = ?integer(?any)) -> T;
t_sup(?int_set(Set1), ?int_set(Set2)) ->
  case set_union(Set1, Set2) of
    ?any ->
      t_from_range(min(set_min(Set1), set_min(Set2)), 
		   max(set_max(Set1), set_max(Set2)));
    Set -> ?int_set(Set)
  end;
t_sup(?int_range(From1, To1), ?int_range(From2, To2)) ->
  t_from_range(min(From1, From2), max(To1, To2));
t_sup(Range = ?int_range(_, _), ?int_set(Set)) ->
  expand_range_from_set(Range, Set);
t_sup(?int_set(Set), Range = ?int_range(_, _)) ->
  expand_range_from_set(Range, Set);
t_sup(?product(Types1), ?product(Types2)) ->
  L1 = length(Types1),
  L2 = length(Types2),
  if L1 =:= L2 -> ?product(t_sup_lists(Types1, Types2));
     true -> ?any
  end;
t_sup(?product(_), _) ->
  ?any;
t_sup(_, ?product(_)) ->
  ?any;
t_sup(T = ?tuple(?any, ?any, ?any), ?tuple(_, _, _)) -> T;
t_sup(?tuple(_, _, _), T = ?tuple(?any, ?any, ?any)) -> T;
t_sup(T = ?tuple(?any, ?any, ?any), ?tuple_set(_)) -> T;
t_sup(?tuple_set(_), T = ?tuple(?any, ?any, ?any)) -> T;
t_sup(T1 = ?tuple(Elements1, Arity, Tag1), 
      T2 = ?tuple(Elements2, Arity, Tag2)) ->
  if Tag1 =:= Tag2 -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag1 =:= ?any -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag2 =:= ?any -> t_tuple(t_sup_lists(Elements1, Elements2));
     Tag1 < Tag2 -> ?tuple_set([{Arity, [T1, T2]}]);
     Tag1 > Tag2 -> ?tuple_set([{Arity, [T2, T1]}])
  end;
t_sup(T1 = ?tuple(_, Arity1, _), T2 = ?tuple(_, Arity2, _)) ->
  sup_tuple_sets([{Arity1, [T1]}], [{Arity2, [T2]}]);
t_sup(?tuple_set(List1), ?tuple_set(List2)) ->
  sup_tuple_sets(List1, List2);
t_sup(?tuple_set(List1), T2 = ?tuple(_, Arity, _)) ->
  sup_tuple_sets(List1, [{Arity, [T2]}]);
t_sup(T1 = ?tuple(_, Arity, _), ?tuple_set(List2)) ->
  sup_tuple_sets([{Arity, [T1]}], List2);
t_sup(T1, T2) ->
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  sup_union(U1, U2).

t_sup_lists([T1|Left1], [T2|Left2]) ->
  [t_sup(T1, T2)|t_sup_lists(Left1, Left2)];
t_sup_lists([], []) ->
  [].

sup_tuple_sets(L1, L2) ->
  TotalArities = ordsets:union([Arity || {Arity, _} <- L1],
			       [Arity || {Arity, _} <- L2]),
  if length(TotalArities) > ?TUPLE_ARITY_LIMIT -> t_tuple();
     true ->
      case sup_tuple_sets(L1, L2, []) of
	[{_Arity, [OneTuple = ?tuple(_, _, _)]}] -> OneTuple;
	List -> ?tuple_set(List)
      end
  end.

sup_tuple_sets([{Arity, Tuples1}|Left1], [{Arity, Tuples2}|Left2], Acc) ->
  NewAcc = [{Arity, sup_tuples_in_set(Tuples1, Tuples2)}|Acc],
  sup_tuple_sets(Left1, Left2, NewAcc);
sup_tuple_sets(L1 = [T1 = {Arity1, _}|Left1], 
	       L2 = [T2 = {Arity2, _}|Left2], Acc) ->
  if Arity1 < Arity2 -> sup_tuple_sets(Left1, L2, [T1|Acc]);
     Arity1 > Arity2 -> sup_tuple_sets(L1, Left2, [T2|Acc])
  end;
sup_tuple_sets([], L2, Acc) -> lists:reverse(Acc, L2);
sup_tuple_sets(L1, [], Acc) -> lists:reverse(Acc, L1).


sup_tuples_in_set([T = ?tuple(_, _, ?any)], L) ->
  [t_tuple(sup_tuple_elements([T|L]))];
sup_tuples_in_set(L, [T = ?tuple(_, _, ?any)]) ->
  [t_tuple(sup_tuple_elements([T|L]))];
sup_tuples_in_set(L1, L2) ->
  FoldFun = fun(?tuple(_, _, Tag), AccTag) -> t_sup(Tag, AccTag) end,
  TotalTag0 = lists:foldl(FoldFun, ?none, L1),
  TotalTag  = lists:foldl(FoldFun, TotalTag0, L2),
  case TotalTag of
    ?atom(?any) -> 
      %% We will reach the set limit. Widen now.
      [t_tuple(sup_tuple_elements(L1++L2))];
    ?atom(Set) ->
      case set_size(Set) > ?TUPLE_TAG_LIMIT of
	true ->
	  %% We will reach the set limit. Widen now.
	  [t_tuple(sup_tuple_elements(L1++L2))];
	false ->
	  %% We can go on and build the tuple set.
	  sup_tuples_in_set(L1, L2, [])
      end
  end.

sup_tuple_elements([?tuple(Elements, _, _)|L]) ->
  lists:foldl(fun(?tuple(Es, _, _), Acc) -> t_sup_lists(Es, Acc) end,
	      Elements, L).

sup_tuples_in_set(L1 = [T1 = ?tuple(Elements1, Arity, Tag1)|Left1], 
		  L2 = [T2 = ?tuple(Elements2, Arity, Tag2)|Left2], Acc) ->
  if 
    Tag1 < Tag2   -> sup_tuples_in_set(Left1, L2, [T1|Acc]);
    Tag1 > Tag2   -> sup_tuples_in_set(L1, Left2, [T2|Acc]);
    Tag2 =:= Tag2 -> NewElements = t_sup_lists(Elements1, Elements2),
		     NewAcc = [?tuple(NewElements, Arity, Tag1)|Acc],
		     sup_tuples_in_set(Left1, Left2, NewAcc)
  end;
sup_tuples_in_set([], L2, Acc) -> lists:reverse(Acc, L2);
sup_tuples_in_set(L1, [], Acc) -> lists:reverse(Acc, L1).

sup_union(U1, U2) ->
  sup_union(U1, U2, 0, []).

sup_union([?none|Left1], [?none|Left2], N, Acc) ->
  sup_union(Left1, Left2, N, [?none|Acc]);
sup_union([T1|Left1], [T2|Left2], N, Acc) ->
  sup_union(Left1, Left2, N+1, [t_sup(T1, T2)|Acc]);
sup_union([], [], N, Acc) ->
  if N =:= 0 -> ?none;
     N =:= 1 -> 
      [Type] = [T || T <- Acc, T=/=?none],
      Type;
     N =:= length(Acc)  -> ?any;
     true -> ?union(lists:reverse(Acc))
  end.

force_union(T = ?atom(_)) ->        ?atom_union(T);
force_union(T = ?bitstr(_,_)) ->    ?bitstr_union(T); 
force_union(T = ?function(_, _)) -> ?function_union(T);
force_union(T = ?identifier(_)) ->  ?identifier_union(T);
force_union(T = ?list(_, _, _)) ->  ?list_union(T);
force_union(T = ?nil) ->            ?list_union(T);
force_union(T = ?number(_,_)) ->    ?number_union(T);
force_union(T = ?tuple(_, _, _)) -> ?tuple_union(T);
force_union(T = ?tuple_set(_)) ->   ?tuple_union(T);
force_union(T = ?matchstate(_,_)) ->   ?matchstate_union(T);
force_union(T = ?union(_)) ->       T.

%%-----------------------------------------------------------------------------
%% Infimum
%%

t_inf([H1, H2|T]) ->
  case t_inf(H1, H2) of
    ?none -> ?none;
    NewH -> t_inf([NewH|T])
  end;
t_inf([H]) -> H;
t_inf([]) -> ?none.

t_inf(?var(_), ?var(_)) -> ?any;
t_inf(?var(_), T) -> subst_all_vars_to_any(T);
t_inf(T, ?var(_)) -> subst_all_vars_to_any(T);
t_inf(?any, T) -> subst_all_vars_to_any(T);
t_inf(T, ?any) -> subst_all_vars_to_any(T);
t_inf(?unit, _) -> ?unit;
t_inf(_, ?unit) -> ?unit;
t_inf(?none, _) -> ?none;
t_inf(_, ?none) -> ?none;
t_inf(T, T) -> subst_all_vars_to_any(T);
t_inf(?atom(Set1), ?atom(Set2)) ->
  case set_intersection(Set1, Set2) of
    ?none ->  ?none;
    NewSet -> ?atom(NewSet)
  end;
t_inf(?bitstr(U1,B1),?bitstr(0,B2)) ->
  if B2 >= B1 andalso (B2-B1) rem U1 =:= 0 -> t_bitstr(0,B2);
     true -> ?none
  end;
t_inf(?bitstr(0,B1),?bitstr(U2,B2)) ->
  if B1 >= B2 andalso (B1-B2) rem U2 =:= 0 -> t_bitstr(0,B1);
     true -> ?none
  end;
t_inf(?bitstr(U1,B1),?bitstr(U1,B1)) ->
  t_bitstr(U1,B1);
t_inf(?bitstr(U1,B1),?bitstr(U2,B2)) when U2 > U1 ->
  inf_bitstr(U2,B2,U1,B1);
t_inf(?bitstr(U1,B1),?bitstr(U2,B2)) ->
  inf_bitstr(U1,B1,U2,B2);
t_inf(?function(Domain1, Range1), ?function(Domain2, Range2)) ->
  case t_inf(Domain1, Domain2) of
    ?none -> ?none;
    Domain ->
      ?function(Domain, t_inf(Range1, Range2))
  end;
t_inf(?identifier(Set1), ?identifier(Set2)) ->
  case set_intersection(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_inf(?matchstate(Pres1,Slots1),
      ?matchstate(Pres2,Slots2)) ->
  ?matchstate(t_inf(Pres1,Pres2),
	      t_inf(Slots1,Slots2));
t_inf(?nil, ?nil) -> ?nil;
t_inf(?nil, ?nonempty_list(_, _)) ->
  ?none;
t_inf(?nonempty_list(_, _), ?nil) ->
  ?none;
t_inf(?nil, ?list(_Contents, Termination, _)) ->
  t_inf(?nil, Termination);
t_inf(?list(_Contents, Termination, _), ?nil) ->
  t_inf(?nil, Termination);
t_inf(?list(Contents1, Termination1, Size1), 
      ?list(Contents2, Termination2, Size2)) ->  
  case t_inf(Termination1, Termination2) of
    ?none -> ?none;
    Termination ->
      case t_inf(Contents1, Contents2) of
	?none -> 
	  %% If none of the lists are nonempty, then the infimum is nil.
	  case (Size1 =:= ?any) andalso (Size2 =:= ?any) of
	    true -> t_nil();
	    false -> ?none
	  end;
	Contents -> 
	  Size =
	    case {Size1, Size2} of
	      {?any, ?any} -> ?any;
	      {?any, ?nonempty_tag} -> ?nonempty_tag;
	      {?nonempty_tag, ?any} -> ?nonempty_tag;
	      {?nonempty_tag, ?nonempty_tag} -> ?nonempty_tag
	    end,
	  ?list(Contents, Termination, Size)
      end
  end;
t_inf(T1 = ?number(_, _), T2 = ?number(_, _)) ->
  case {T1, T2} of
    {T, T}                           -> T;
    {_, ?number(?any, ?number_tag)}  -> T1;
    {?number(?any, ?number_tag), _}  -> T2;
    {?float, ?integer(_)}            -> ?none;
    {?integer(_), ?float}            -> ?none;
    {?integer(?any), ?integer(_)}    -> T2;
    {?integer(_), ?integer(?any)}    -> T1;
    {?int_set(Set1), ?int_set(Set2)} -> 
      case set_intersection(Set1, Set2) of
	?none -> ?none;
	Set -> ?int_set(Set)
      end;
    {?int_range(From1, To1), ?int_range(From2, To2)} -> 
      t_from_range(max(From1, From2), min(To1, To2));
    {Range = ?int_range(_, _), ?int_set(Set)} ->
      %%io:format("t_inf range, set args ~p ~p ~n", [T1, T2]),
      Ans2 = 
	case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	  ?none -> ?none;
	  NewSet -> ?int_set(NewSet)
	end,
      %%io:format("Ans2 ~p ~n", [Ans2]),
      Ans2;
    {?int_set(Set), Range = ?int_range(_, _)} ->
      case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	?none -> ?none;
	NewSet -> ?int_set(NewSet)
      end
  end;
t_inf(?product(Types1), ?product(Types2)) ->
  L1 = length(Types1),
  L2 = length(Types2),
  if L1 =:= L2 -> ?product(t_inf_lists(Types1, Types2));
     true -> ?none
  end;
t_inf(?product(_), _) ->
  ?none;
t_inf(_, ?product(_)) ->
  ?none;
t_inf(?tuple(?any, ?any, ?any), T = ?tuple(_, _, _)) -> T;
t_inf(T = ?tuple(_, _, _), ?tuple(?any, ?any, ?any)) -> T;
t_inf(?tuple(?any, ?any, ?any), T = ?tuple_set(_)) -> T;
t_inf(T = ?tuple_set(_), ?tuple(?any, ?any, ?any)) -> T;
t_inf(?tuple(Elements1, Arity, _Tag1), ?tuple(Elements2, Arity, _Tag2)) ->
  case t_inf_lists_strict(Elements1, Elements2) of
    ?none -> ?none;
    NewElements -> t_tuple(NewElements)
  end;
t_inf(?tuple_set(List1), ?tuple_set(List2)) ->
  inf_tuple_sets(List1, List2);
t_inf(?tuple_set(List), T = ?tuple(_, Arity, _)) ->
  inf_tuple_sets(List, [{Arity, [T]}]);
t_inf(T = ?tuple(_, Arity, _), ?tuple_set(List)) ->
  inf_tuple_sets(List, [{Arity, [T]}]);
t_inf(?union(U1), T) ->
  ?union(U2) = force_union(T),
  inf_union(U1, U2);
t_inf(T, ?union(U2)) ->
  ?union(U1) = force_union(T),
  inf_union(U1, U2);
t_inf(#c{}, #c{}) ->
  ?none.

t_inf_lists(L1, L2) ->
  t_inf_lists(L1, L2, []).

t_inf_lists([T1|Left1], [T2|Left2], Acc) ->
  t_inf_lists(Left1, Left2, [t_inf(T1, T2)|Acc]);
t_inf_lists([], [], Acc) ->
  lists:reverse(Acc).

%% Infimum of lists with strictness. If any element is none, the whole
%% type is none.

t_inf_lists_strict(L1, L2) ->
  t_inf_lists_strict(L1, L2, []).

t_inf_lists_strict([T1|Left1], [T2|Left2], Acc) ->
  case t_inf(T1, T2) of
    ?none -> ?none;
    T -> t_inf_lists_strict(Left1, Left2, [T|Acc])
  end;
t_inf_lists_strict([], [], Acc) ->
  lists:reverse(Acc).

inf_tuple_sets(L1, L2) ->
  case inf_tuple_sets(L1, L2, []) of
    [] -> ?none;
    [{_Arity, [OneTuple = ?tuple(_, _, _)]}] -> OneTuple;
    List -> ?tuple_set(List)
  end.

inf_tuple_sets([{Arity, Tuples1}|Left1], [{Arity, Tuples2}|Left2], Acc) ->
  case inf_tuples_in_sets(Tuples1, Tuples2) of
    [] -> inf_tuple_sets(Left1, Left2, Acc);
    NewTuples -> inf_tuple_sets(Left1, Left2, [{Arity, NewTuples}|Acc])
  end;
inf_tuple_sets(L1 = [{Arity1, _}|Left1], L2 = [{Arity2, _}|Left2], Acc) ->
  if Arity1 < Arity2 -> inf_tuple_sets(Left1, L2, Acc);
     Arity1 > Arity2 -> inf_tuple_sets(L1, Left2, Acc)
  end;
inf_tuple_sets([], _, Acc) -> lists:reverse(Acc);
inf_tuple_sets(_, [], Acc) -> lists:reverse(Acc).
      
inf_tuples_in_sets([?tuple(Elements1, _, ?any)], L2) ->
  NewList = [t_inf_lists_strict(Elements1, Elements2)
	     || ?tuple(Elements2, _, _) <- L2],
  [t_tuple(Es) || Es <- NewList, Es =/= ?none];
inf_tuples_in_sets(L1, [?tuple(Elements2, _, ?any)]) ->
  NewList = [t_inf_lists_strict(Elements1, Elements2)
	     || ?tuple(Elements1, _, _) <- L1],
  [t_tuple(Es) || Es <- NewList, Es =/= ?none];
inf_tuples_in_sets(L1, L2) ->
  inf_tuples_in_sets(L1, L2, []).

inf_tuples_in_sets([?tuple(Elements1, Arity, Tag)|Left1], 
		   [?tuple(Elements2, Arity, Tag)|Left2], Acc) ->
  case t_inf_lists_strict(Elements1, Elements2) of
    ?none -> inf_tuples_in_sets(Left1, Left2, Acc);
    NewElements -> 
      inf_tuples_in_sets(Left1, Left2, [?tuple(NewElements, Arity, Tag)|Acc])
  end;
inf_tuples_in_sets(L1 = [?tuple(_, _, Tag1)|Left1], 
		   L2 = [?tuple(_, _, Tag2)|Left2], Acc) ->
  if Tag1 < Tag2 -> inf_tuples_in_sets(Left1, L2, Acc);
     Tag1 > Tag2 -> inf_tuples_in_sets(L1, Left2, Acc)
  end;
inf_tuples_in_sets([], _, Acc) -> lists:reverse(Acc);
inf_tuples_in_sets(_, [], Acc) -> lists:reverse(Acc).

inf_union(U1, U2) ->
  inf_union(U1, U2, 0, []).

inf_union([?none|Left1], [?none|Left2], N, Acc) ->
  inf_union(Left1, Left2, N, [?none|Acc]);
inf_union([T1|Left1], [T2|Left2], N, Acc) ->
  case t_inf(T1, T2) of
    ?none -> inf_union(Left1, Left2, N, [?none|Acc]);
    T     -> inf_union(Left1, Left2, N+1, [T|Acc])
  end;
inf_union([], [], N, Acc) ->
  if N =:= 0 -> ?none;
     N =:= 1 ->
      [Type] = [T || T <- Acc, T=/=?none],
      Type;
     N >= 2  -> ?union(lists:reverse(Acc))
  end.

inf_bitstr(U1, B1, U2, B2) ->
  GCD = gcd(U1, U2),
    case (B2-B1) rem GCD of
      0 ->
	U = (U1*U2) div GCD,
	B = findfirst(0, 0, U1, B1, U2, B2),
	t_bitstr(U, B);
      _ ->
	?none
  end.

findfirst(N1, N2, U1, B1, U2, B2) ->
  Val1 = U1*N1+B1,
  Val2 = U2*N2+B2,
  if Val1 =:= Val2 ->
      Val1;
     Val1 > Val2 ->
      findfirst(N1, N2+1, U1, B1, U2, B2);
     Val1 < Val2 ->
      findfirst(N1+1, N2, U1, B1, U2, B2)
  end.

%%-----------------------------------------------------------------------------
%% Substituting variables.
%%

t_subst(T, Dict) ->
  case t_has_var(T) of
    true -> t_subst(T, Dict, fun(X) -> X end);
    false -> T
  end.

subst_all_vars_to_any(T) ->
  case t_has_var(T) of
    true -> t_subst(T, dict:new(), fun(_) -> ?any end);
    false -> T
  end.

t_subst(T = ?var(Id), Dict, Fun) ->
  case dict:find(Id, Dict) of
    error -> Fun(T);
    {ok, Type} -> Type
  end;
t_subst(?list(Contents, Termination, Size), Dict, Fun) ->
  case t_subst(Contents, Dict, Fun) of
    ?none -> ?none;
    NewContents ->
      %% Be careful here to make the termination collapse if necessary.
      case t_subst(Termination, Dict, Fun) of
	?nil -> ?list(NewContents, ?nil, Size);
	?any -> ?list(NewContents, ?any, Size);
	Other ->
	  ?list(NewContents, NewTermination, _) = t_cons(NewContents, Other),
	  ?list(NewContents, NewTermination, Size)
      end
  end;
t_subst(?function(Domain, Range), Dict, Fun) ->
  ?function(t_subst(Domain, Dict, Fun), t_subst(Range, Dict, Fun));
t_subst(?product(Types), Dict, Fun) -> 
  ?product([t_subst(T, Dict, Fun) || T <- Types]);
t_subst(T = ?tuple(?any, ?any, ?any), _Dict, _Fun) ->
  T;
t_subst(?tuple(Elements, _Arity, _Tag), Dict, Fun) ->
  t_tuple([t_subst(E, Dict, Fun) || E <- Elements]);
t_subst(T = ?tuple_set(_), Dict, Fun) ->
  t_sup([t_subst(T, Dict, Fun) || T <- t_tuple_subtypes(T)]);
t_subst(T, _Dict, _Fun) -> 
  T.

	      
%%-----------------------------------------------------------------------------
%% Unification
%%

t_unify(T1, T2) ->
  {T, Dict} = t_unify(T1, T2, dict:new()),
  {t_subst(T, Dict), lists:keysort(1,dict:to_list(Dict))}.

t_unify(T = ?var(Id), ?var(Id), Dict) ->
  {T, Dict};
t_unify(T = ?var(Id1), ?var(Id2), Dict) ->
  case dict:find(Id1, Dict) of
    error -> 
      case dict:find(Id2, Dict) of
	error -> {T, dict:store(Id2, T, Dict)};
	{ok, Type} -> {Type, t_unify(T, Type, Dict)}
      end;
    {ok, Type1} ->
      case dict:find(Id2, Dict) of
	error -> {Type1, dict:store(Id2, T, Dict)};
	{ok, Type2} -> t_unify(Type1, Type2, Dict)
      end
  end;
t_unify(?var(Id), Type, Dict) ->
  case dict:find(Id, Dict) of
    error -> {Type, dict:store(Id, Type, Dict)};
    {ok, VarType} -> t_unify(VarType, Type, Dict)
  end;
t_unify(Type, ?var(Id), Dict) ->
  case dict:find(Id, Dict) of
    error -> {Type, dict:store(Id, Type, Dict)};
    {ok, VarType} -> t_unify(VarType, Type, Dict)
  end;
t_unify(?function(Domain1, Range1), ?function(Domain2, Range2), Dict) ->
  {Domain, Dict1} = t_unify(Domain1, Domain2, Dict),
  {Range, Dict2} = t_unify(Range1, Range2, Dict1),
  {?function(Domain, Range), Dict2};
t_unify(?list(Contents1, Termination1, Size), 
	?list(Contents2, Termination2, Size), Dict) ->
  {Contents, Dict1} = t_unify(Contents1, Contents2, Dict),
  {Termination, Dict2} = t_unify(Termination1, Termination2, Dict1),
  {?list(Contents, Termination, Size), Dict2};
t_unify(?product(Types1), ?product(Types2), Dict) -> 
  {Types, Dict1} = unify_lists(Types1, Types2, Dict),
  {?product(Types), Dict1};
t_unify(T = ?tuple(?any, ?any, ?any), ?tuple(?any, ?any, ?any), Dict) ->
  {T, Dict};
t_unify(?tuple(Elements1, Arity, _), 
	?tuple(Elements2, Arity, _), Dict) when Arity =/= ?any ->
  {NewElements, Dict1} = unify_lists(Elements1, Elements2, Dict),
  {t_tuple(NewElements), Dict1};
t_unify(T1 = ?tuple_set([{Arity, _}]), 
	T2 = ?tuple(_, Arity, _), Dict) when Arity =/= ?any ->
  unify_tuple_set_and_tuple(T1, T2, Dict);
t_unify(T1 = ?tuple(_, Arity, _),
	T2 = ?tuple_set([{Arity, _}]), Dict) when Arity =/= ?any ->
  unify_tuple_set_and_tuple(T2, T1, Dict);
t_unify(?tuple_set(List1), ?tuple_set(List2), Dict) ->
  unify_lists(lists:append([T || {_Arity, T} <- List1]), 
	      lists:append([T || {_Arity, T} <- List2]), Dict);
t_unify(T, T, Dict) ->
  {T, Dict};
t_unify(T1, T2, _) ->
  throw({mismatch, T1, T2}).

unify_tuple_set_and_tuple(?tuple_set([{Arity, List}]), 
			  ?tuple(Elements2, Arity, _), Dict) ->
  %% Can only work if the single tuple has variables at correct places.
  %% Collapse the tuple set.
  {NewElements, Dict1} = unify_lists(sup_tuple_elements(List), Elements2, Dict),
  {t_tuple(NewElements), Dict1}.


unify_lists(L1, L2, Dict) ->
  unify_lists(L1, L2, Dict, []).

unify_lists([T1|Left1], [T2|Left2], Dict, Acc) ->
  {NewT, NewDict} = t_unify(T1, T2, Dict),
  unify_lists(Left1, Left2, NewDict, [NewT|Acc]);
unify_lists([], [], Dict, Acc) ->
  {lists:reverse(Acc), Dict}.

%%t_assign_variables_to_subtype(T1, T2) ->
%%  try 
%%    Dict = assign_vars(T1, T2, dict:new()),
%%    {ok, dict:map(fun(_Param, List) -> t_sup(List) end, Dict)}
%%  catch
%%    throw:error -> error
%%  end.

%%assign_vars(_, ?var(_), _Dict) ->
%%  erlang:error("Variable in right hand side of assignment");
%%assign_vars(?any, _, Dict) ->
%%  Dict;
%%assign_vars(Var = ?var(_), Type, Dict) ->
%%  store_var(Var, Type, Dict);
%%assign_vars(?function(Domain1, Range1), ?function(Domain2, Range2), Dict) ->
%%  DomainList =
%%    case Domain2 of
%%      ?any -> [];
%%      ?product(List) -> List
%%    end,
%%  case any_none([Range2|DomainList]) of
%%    true -> throw(error);
%%    false ->
%%      Dict1 = assign_vars(Domain1, Domain2, Dict),
%%      assign_vars(Range1, Range2, Dict1)
%%  end;
%%assign_vars(?list(_Contents, _Termination, ?any), ?nil, Dict) ->
%%  Dict;
%%assign_vars(?list(Contents1, Termination1, Size1), 
%%	    ?list(Contents2, Termination2, Size2), Dict) ->
%%  Dict1 = assign_vars(Contents1, Contents2, Dict),
%%  Dict2 = assign_vars(Termination1, Termination2, Dict1),
%%  case {Size1, Size2} of
%%    {S, S} -> Dict2;
%%    {?any, ?nonempty_tag} -> Dict2;
%%    {_, _} -> throw(error)
%%  end;
%%assign_vars(?product(Types1), ?product(Types2), Dict) -> 
%%  case length(Types1) =:= length(Types2) of
%%    true -> assign_vars_lists(Types1, Types2, Dict);
%%    false -> throw(error)
%%  end;
%%assign_vars(?tuple(?any, ?any, ?any), ?tuple(?any, ?any, ?any), Dict) ->
%%  Dict;
%%assign_vars(?tuple(?any, ?any, ?any), ?tuple(_, _, _), Dict) ->
%%  Dict;
%%assign_vars(?tuple(Elements1, Arity, _), 
%%	    ?tuple(Elements2, Arity, _), Dict) when Arity =/= ?any ->
%%  assign_vars_lists(Elements1, Elements2, Dict);
%%assign_vars(T = ?tuple_set(_), ?tuple_set(List2), Dict) ->
%%  %% All Rhs tuples must already be subtypes of Lhs, so we can take
%%  %% each one separatly.
%%  assign_vars_lists(lists:duplicate(length(List2), T), List2, Dict);
%%assign_vars(?tuple(?any, ?any, ?any), ?tuple_set(_), Dict) ->
%%  Dict;
%%assign_vars(T1 = ?tuple(_, Arity, _), ?tuple_set(List), Dict) ->
%%  case reduce_tuple_tags(List) of
%%    [Tuple = ?tuple(_, Arity, _)] -> assign_vars(T1, Tuple, Dict);
%%    _ -> throw(error)
%%  end;
%%assign_vars(?tuple_set(List), T2 = ?tuple(_, Arity, Tag), Dict) ->
%%  case [T || T = ?tuple(_, Arity1, Tag1) <- List, 
%%	     Arity1 =:= Arity, Tag1 =:= Tag] of
%%    [] -> throw(error);
%%    [T1] -> assign_vars(T1, T2, Dict)
%%  end;
%%assign_vars(?union(U1), T2, Dict) ->
%%  ?union(U2) = force_union(T2),
%%  assign_vars_lists(U1, U2, Dict);
%%assign_vars(T, T, Dict) ->
%%  Dict;
%%assign_vars(T1, T2, Dict) ->
%%  case t_is_subtype(T2, T1) of
%%    false -> throw(error);
%%    true -> Dict
%%  end.

%%assign_vars_lists([T1|Left1], [T2|Left2], Dict) ->
%%  assign_vars_lists(Left1, Left2, assign_vars(T1, T2, Dict));
%%assign_vars_lists([], [], Dict) ->
%%  Dict.

%%store_var(?var(Id), Type, Dict) ->
%%  case dict:find(Id, Dict) of
%%    error -> dict:store(Id, [Type], Dict);
%%    {ok, _VarType0} -> dict:update(Id, fun(X) -> [Type|X] end, Dict)
%%  end.
  
%%-----------------------------------------------------------------------------
%% Subtraction. 
%%
%% Note that the subtraction is an approximation since we do not have
%% negative types. Also, tuples and products should be handled using
%% the cartesian product of the elements, but this is not feasible to
%% do.
%% 
%% Example: {a|b,c|d}\{a,d} = {a,c}|{a,d}|{b,c}|{b,d} \ {a,d} = 
%%                          = {a,c}|{b,c}|{b,d} = {a|b,c|d}
%%
%% Instead, we can subtract if all elements but one becomes none after
%% subtracting element-wise.
%% 
%% Example: {a|b,c|d}\{a|b,d} = {a,c}|{a,d}|{b,c}|{b,d} \ {a,d}|{b,d} = 
%%                            = {a,c}|{b,c} = {a|b,c}



t_subtract_list(T1, [T2|Left]) ->
  t_subtract_list(t_subtract(T1, T2), Left);
t_subtract_list(T, []) ->
  T.

t_subtract(_, ?any) -> ?none;
t_subtract(?any, _) -> ?any;
t_subtract(T, ?unit) -> T;
t_subtract(?unit, _) -> ?unit;
t_subtract(?none, _) -> ?none;
t_subtract(T, ?none) -> T;
t_subtract(?atom(Set1), ?atom(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?atom(Set)
  end;
t_subtract(?bitstr(U1,B1),?bitstr(U2,B2)) ->
  subtract_bin(t_bitstr(U1,B1),t_inf(t_bitstr(U1,B1),t_bitstr(U2,B2)));
t_subtract(T1 = ?function(_, _), T2 = ?function(_, _)) ->
  case t_is_subtype(T1, T2) of
    true -> ?none;
    false -> T1
  end;
t_subtract(?identifier(Set1), ?identifier(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_subtract(?matchstate(Pres1,Slots1),
	   ?matchstate(Pres2,_Slots2)) ->
  Pres = t_subtract(Pres1,Pres2),
  case t_is_none(Pres) of
    true -> ?none;
    false -> ?matchstate(Pres,Slots1)
  end;
t_subtract(?matchstate(Present,Slots),_) ->
  ?matchstate(Present,Slots);
t_subtract(?nil, ?nil) ->
  ?none;
t_subtract(?nil, ?nonempty_list(_, _)) ->
  ?nil;
t_subtract(?nil, ?list(_, _, _)) ->
  ?none;
t_subtract(T = ?list(Contents, Termination, _Size), ?nil) ->
  case Termination =:= ?nil of
    true -> ?nonempty_list(Contents, Termination);
    false -> T
  end;
t_subtract(T = ?list(Contents1, Termination1, Size1), 
	   ?list(Contents2, Termination2, Size2)) ->
  case t_is_subtype(Contents1, Contents2) of
    true ->
      case t_is_subtype(Termination1, Termination2) of
	true ->
	  case {Size1, Size2} of
	    {?nonempty_tag, ?any} -> ?none;
	    {?any, ?nonempty_tag} -> Termination1;
	    {S, S} -> ?none
	  end;
	false ->
	  %% If the termination is not covered by the subtracted type
	  %% we cannot really say anything about the result.
	  T
      end;
    false ->
      %% All contents must be covered if there is going to be any
      %% change to the list.
      T
  end;
t_subtract(?float, ?float) -> ?none;
t_subtract(T1 = ?number(_, _), ?float) -> t_inf(T1, t_integer());
t_subtract(?float, ?number(_Set, Tag)) ->
  case Tag of
    ?any -> ?none;
    _ -> ?float
  end;
t_subtract(?number(_, _), ?number(?any, ?number_tag)) -> ?none;
t_subtract(T1 = ?number(?any, ?number_tag), ?number(_, _)) -> T1;
t_subtract(?int_set(Set1), ?int_set(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?int_set(Set)
  end;
t_subtract(T1 = ?int_range(From1, To1), T2 = ?int_range(_, _)) ->
  case t_inf(T1, T2) of
    ?none -> T1;
    ?int_range(From1, To1) -> ?none;
    ?int_range(neg_inf, To) -> t_from_range(To + 1, To1);
    ?int_range(From, pos_inf) -> t_from_range(From1, From - 1);
    ?int_range(From, To) -> t_sup(t_from_range(From1, From - 1), 
				  t_from_range(To + 1, To))
  end;
t_subtract(T1 = ?int_range(From, To), _T2 = ?int_set(Set)) ->
  NewFrom = case set_is_element(From, Set) of
	      true -> From + 1;
	      false -> From
	    end,
  NewTo = case set_is_element(To, Set) of
	    true -> To - 1;
	    false -> To
	  end,
  if (NewFrom =:= From) and (NewTo =:= To) -> T1;
     true -> t_from_range(NewFrom, NewTo)
  end;
t_subtract(?int_set(Set), ?int_range(From, To)) ->
  case set_filter(fun(X) -> not ((X =< From) orelse (X >= To))end, Set) of
    ?none -> ?none;
    NewSet -> ?int_set(NewSet)
  end;
t_subtract(?integer(_), ?integer(?any)) -> ?none;
t_subtract(T1 = ?integer(?any), ?integer(_)) -> T1;
t_subtract(?tuple(_, _, _), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract(?tuple_set(_), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract(T1 = ?tuple(?any, ?any, ?any), ?tuple_set(_)) -> T1;
t_subtract(T1 = ?tuple(Elements1, Arity1, _Tag1), 
	   ?tuple(Elements2, Arity2, _Tag2)) ->
  if Arity1 =/= Arity2 -> T1;
     Arity1 =:= Arity2 ->
      NewElements = t_subtract_lists(Elements1, Elements2),
      case [E || E <- NewElements, E =/= ?none] of
	[] -> ?none;
	[_] -> t_tuple(replace_nontrivial_element(Elements1, NewElements));
	_ -> T1
      end
  end;
t_subtract(T1 = ?tuple_set(List1), T2 = ?tuple(_, Arity, _)) ->
  case orddict:find(Arity, List1) of
    error -> T1;
    {ok, List2} ->
      TuplesLeft0 = [Tuple || {_Arity, Tuple} <- orddict:erase(Arity, List1)],
      TuplesLeft1 = lists:append(TuplesLeft0),
      t_sup([t_subtract(L, T2) || L <- List2]++TuplesLeft1)
  end;
t_subtract(T1 = ?tuple(_, Arity, _), ?tuple_set(List1)) ->
  case orddict:find(Arity, List1) of
    error -> T1;
    {ok, List2} -> t_inf([t_subtract(T1, L) || L <- List2])
  end;
t_subtract(T1 = ?tuple_set(_), T2 = ?tuple_set(_)) ->
  t_sup([t_subtract(T, T2) || T <- t_tuple_subtypes(T1)]);
t_subtract(T1 = ?product(Elements1), ?product(Elements2)) ->
  Arity1 = length(Elements1),
  Arity2 = length(Elements2),
  if Arity1 =/= Arity2 -> T1;
     Arity1 =:= Arity2 ->
      NewElements = t_subtract_lists(Elements1, Elements2),
      case [E || E <- NewElements, E =/= ?none] of
	[] -> ?none;
	[_] -> t_product(replace_nontrivial_element(Elements1, NewElements));
	_ -> T1
      end
  end;
t_subtract(?product(P1), _) ->
  ?product(P1);
t_subtract(T, ?product(_)) ->
  T;
t_subtract(?union(U1), ?union(U2)) ->
  subtract_union(U1, U2);
t_subtract(T1, T2) ->  
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  subtract_union(U1, U2).


t_subtract_lists(L1, L2) ->
  t_subtract_lists(L1, L2, []).

t_subtract_lists([T1|Left1], [T2|Left2], Acc) ->
  t_subtract_lists(Left1, Left2, [t_subtract(T1, T2)|Acc]);
t_subtract_lists([], [], Acc) ->
  lists:reverse(Acc).


subtract_union(U1, U2) ->
  subtract_union(U1, U2, 0, []).

subtract_union([T1|Left1], [T2|Left2], N, Acc) ->
  case t_subtract(T1, T2) of
    ?none -> subtract_union(Left1, Left2, N, [?none|Acc]);
    T ->     subtract_union(Left1, Left2, N+1, [T|Acc])
  end;
subtract_union([], [], 0, _Acc) ->
  ?none;
subtract_union([], [], 1, Acc) ->
  [T] = [X || X <- Acc, X =/= ?none],
  T;
subtract_union([], [], N, Acc) when is_integer(N), N > 1 ->
  ?union(lists:reverse(Acc)).

replace_nontrivial_element(El1, El2) ->
  replace_nontrivial_element(El1, El2, []).

replace_nontrivial_element([T1|Left1], [?none|Left2], Acc) ->
  replace_nontrivial_element(Left1, Left2, [T1|Acc]);
replace_nontrivial_element([_|Left1], [T2|_], Acc) ->
  lists:reverse(Acc) ++ [T2|Left1].

subtract_bin(?bitstr(U1,B1),?bitstr(U1,B1)) ->
  ?none;
subtract_bin(?bitstr(U1,B1),?none) ->
  t_bitstr(U1,B1);
subtract_bin(?bitstr(U1,B1),?bitstr(0,B1)) ->
  t_bitstr(U1,B1+U1);
subtract_bin(?bitstr(U1,B1),?bitstr(U1,B2)) ->
  if (B1+U1) =/= B2 -> t_bitstr(0,B1);
     true -> t_bitstr(U1,B1)
  end;
subtract_bin(?bitstr(U1,B1),?bitstr(U2,B2)) ->
  if (2 * U1) =:= U2 ->
      if B1 =:= B2 ->
	  t_bitstr(U2,B1+U1);
	 (B1 + U1) =:= B2 ->
	  t_bitstr(U2,B1);
	 true ->
	  t_bitstr(U1,B1)
      end;
     true ->
      t_bitstr(U1,B1)
  end.

%%-----------------------------------------------------------------------------
%% Relations
%%

t_is_equal(T, T)  -> true;
t_is_equal(_, _) -> false.
  
t_is_subtype(T1, T2) ->
  Inf = t_inf(T1, T2),
  t_is_equal(T1, Inf).

%%-----------------------------------------------------------------------------
%% K-depth abstraction.
%%
%% t_limit/2 is the exported function, which checks the type of the
%% second argument and calls the module local t_limit_k/2 function.
%%

t_limit(Term, K) when is_integer(K) ->
  t_limit_k(Term, K).

t_limit_k(_, K) when K =< 0 -> ?any;
t_limit_k(T = ?tuple(?any, ?any, ?any), _K) -> T;
t_limit_k(?tuple(Elements, Arity, _), K) ->
  if K =:= 1 -> t_tuple(Arity);
     true -> t_tuple([t_limit_k(E, K-1) || E <- Elements])
  end;
t_limit_k(T = ?tuple_set(_), K) ->
  t_sup([t_limit_k(Tuple, K) || Tuple <- t_tuple_subtypes(T)]);
t_limit_k(?list(Elements, Termination, Size), K) ->
  NewTermination = 
    if K =:= 1 -> 
	%% We do not want to lose the termination information.
	t_limit_k(Termination, K);
       true -> t_limit_k(Termination, K - 1)
    end,
  NewElements = t_limit_k(Elements, K - 1),
  TmpList = t_cons(NewElements, NewTermination),
  case Size of
    ?nonempty_tag -> TmpList;
    ?any -> 
      ?list(NewElements1, NewTermination1, _) = TmpList,
      ?list(NewElements1, NewTermination1, ?any)
  end;
t_limit_k(?function(Domain, Range), K) ->
  %% The domain is either a product or any() so we do not decrease the K.
  ?function(t_limit_k(Domain, K), t_limit_k(Range, K-1));
t_limit_k(?product(Elements), K) ->
  ?product([t_limit_k(X, K - 1) || X <- Elements]);
t_limit_k(?union(Elements), K) ->
  ?union([t_limit_k(X, K) || X <- Elements]);
t_limit_k(T, _K) -> T.


%%============================================================================
%% 
%% Abstract records. Used for comparing contracts.
%%
%%============================================================================

t_abstract_records(?list(Contents, Termination, Size), RecDict) ->
  case t_abstract_records(Contents, RecDict) of
    ?none -> ?none;
    NewContents ->
      %% Be careful here to make the termination collapse if necessary.
      case t_abstract_records(Termination, RecDict) of
	?nil -> ?list(NewContents, ?nil, Size);
	?any -> ?list(NewContents, ?any, Size);
	Other ->
	  ?list(NewContents, NewTermination, _) = t_cons(NewContents, Other),
	  ?list(NewContents, NewTermination, Size)
      end
  end;
t_abstract_records(?function(Domain, Range), RecDict) ->
  ?function(t_abstract_records(Domain, RecDict), 
	    t_abstract_records(Range, RecDict));
t_abstract_records(?product(Types), RecDict) -> 
  ?product([t_abstract_records(T, RecDict) || T <- Types]);
t_abstract_records(?union(Types), RecDict) -> 
  t_sup([t_abstract_records(T, RecDict) || T <- Types]);
t_abstract_records(T = ?tuple(?any, ?any, ?any), _RecDict) ->
  T;
t_abstract_records(?tuple(Elements, Arity, Tag = ?atom(_)), RecDict) ->
  [TagAtom] = t_atom_vals(Tag),
  case lookup_record(TagAtom, Arity - 1, RecDict) of
    error -> t_tuple([t_abstract_records(E, RecDict) || E <- Elements]);
    {ok, Fields} -> t_tuple([Tag|[T || {_Name, T} <- Fields]])
  end;
t_abstract_records(?tuple(Elements, _Arity, _Tag), RecDict) ->
  t_tuple([t_abstract_records(E, RecDict) || E <- Elements]);
t_abstract_records(Tuples = ?tuple_set(_), RecDict) ->
  t_sup([t_abstract_records(T, RecDict) || T <- t_tuple_subtypes(Tuples)]);
t_abstract_records(T, _RecDict) -> 
  T.

%% Map over types. Depth first. ?list is not fully implemented so take
%% care when changing the type in Termination.
t_map(Fun, ?list(Contents, Termination, Size)) ->
  Fun(?list(t_map(Fun, Contents), t_map(Fun, Termination), Size));
t_map(Fun, ?function(Domain, Range)) ->
  Fun(?function(t_map(Fun, Domain), t_map(Fun, Range)));
t_map(Fun, ?product(Types)) -> 
  Fun(?product([t_map(Fun, T) || T <- Types]));
t_map(Fun, ?union(Types)) ->
  Fun(t_sup([t_map(Fun, T) || T <- Types]));
t_map(Fun, T = ?tuple(?any, ?any, ?any)) ->
  Fun(T);
t_map(Fun, ?tuple(Elements, _Arity, _Tag)) ->
  Fun(t_tuple([t_map(Fun, E) || E <- Elements]));
t_map(Fun, Tuples = ?tuple_set(_)) ->
  Fun(t_sup([t_map(Fun, T) || T <- t_tuple_subtypes(Tuples)]));
t_map(Fun, T) ->
  Fun(T).

%%============================================================================
%% 
%% Prettyprinter
%%
%%============================================================================

t_to_string(T) ->
  t_to_string(T, dict:new()).

t_to_string(?any, _RecDict) -> 
  "any()";
t_to_string(?none, _RecDict) ->
  "none()";
t_to_string(?unit, _RecDict) ->
  "no_return()";
t_to_string(?atom(?any), _RecDict) -> 
  "atom()";
t_to_string(?atom(Set), _RecDict)  ->
  case set_size(Set) of
    2 ->
      case set_is_element(true, Set) andalso set_is_element(false, Set) of
	true -> "bool()";
	false -> set_to_string(Set)
      end;
    _ ->
      set_to_string(Set)
  end;
t_to_string(?bitstr(8,0), _RecDict) -> 
  "binary()";
t_to_string(?bitstr(0,0), _RecDict) -> 
  "<<>>";
t_to_string(?bitstr(0,B), _RecDict) -> 
  io_lib:format("<<_:~w>>",[B]);
t_to_string(?bitstr(U,0), _RecDict) -> 
  io_lib:format("<<_:_*~w>>",[U]);
t_to_string(?bitstr(U,B), _RecDict) ->
  io_lib:format("<<_:~w,_:_*~w>>",[B,U]);
t_to_string(?function(?any, ?any), _RecDict) ->
  "fun()";
t_to_string(?function(?any, Range), RecDict) ->
  "fun((...) -> " ++ t_to_string(Range, RecDict) ++ ")";
t_to_string(?function(?product(ArgList), Range), RecDict) ->
  "fun((" ++ comma_sequence(ArgList, RecDict) ++ ") -> "
    ++t_to_string(Range, RecDict) ++ ")";
t_to_string(?identifier(Set), _RecDict) -> 
  if Set =:= ?any -> "identifier()";
     true -> sequence([io_lib:format("~w()", [T]) 
		       || T <- set_to_list(Set)], [], " | ")
  end;
t_to_string(?matchstate(Pres,Slots),RecDict) ->
  io_lib:format("ms(~s,~s)", [t_to_string(Pres,RecDict),
				 t_to_string(Slots,RecDict)]);
t_to_string(?nil, _RecDict) ->
  "[]";
t_to_string(?nonempty_list(Contents, Termination), RecDict) ->
  ContentString = t_to_string(Contents, RecDict),
  case Termination of
    ?nil ->
      case Contents of
	?char -> "nonempty_string()";
	_ -> "["++ContentString++",...]"
      end;
    ?any -> 
      %% Just a safety check.
      case Contents =:= ?any of
	true -> ok;
	false -> erlang:error({illegal_list, 
			       ?nonempty_list(Contents, Termination)})
      end,
      "nonempty_maybe_improper_list()";
    _ ->
      case t_is_subtype(t_nil(), Termination) of
	true ->
	  "nonempty_maybe_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")";
	false ->
	  "nonempty_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")"
      end
  end;
t_to_string(?list(Contents, Termination, ?any), RecDict) ->
  ContentString = t_to_string(Contents, RecDict),
  case Termination of
    ?nil ->
      case Contents of
	?char -> "string()";
	_ -> "["++ContentString++"]"
      end;
    ?any ->
      %% Just a safety check.      
      case Contents =:= ?any of
	true -> ok;
	false -> erlang:error({illegal_list, 
			       ?list(Contents, Termination, ?any)})
      end,
      "maybe_improper_list()";
    _ -> 
      case t_is_subtype(t_nil(), Termination) of
	true ->
	  "maybe_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")";
	false ->
	  "improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")"
      end
  end;
t_to_string(?int_set(Set), _RecDict) ->
  set_to_string(Set);  
t_to_string(?byte, _RecDict) -> "byte()";
t_to_string(?char, _RecDict) -> "char()";
t_to_string(?integer_pos, _RecDict) -> "pos_integer()";
t_to_string(?integer_non_neg, _RecDict) -> "non_neg_integer()";
t_to_string(?integer_neg, _RecDict) -> "neg_integer()";
t_to_string(?int_range(From, To), _RecDict) ->
  lists:flatten(io_lib:format("~w..~w", [From, To]));
t_to_string(?integer(?any), _RecDict) -> "integer()";
t_to_string(?float, _RecDict) -> "float()";
t_to_string(?number(?any, ?number_tag), _RecDict) -> "number()";
t_to_string(?product(List), RecDict) -> 
  "<" ++ comma_sequence(List, RecDict) ++ ">";
t_to_string(?tuple(?any, ?any, ?any), _RecDict) -> "tuple()";
t_to_string(?tuple(Elements, _Arity, ?any), RecDict) ->   
  "{" ++ comma_sequence(Elements, RecDict) ++ "}";
t_to_string(?tuple(Elements, Arity, Tag), RecDict) ->
  [TagAtom] = t_atom_vals(Tag),
  case lookup_record(TagAtom, Arity-1, RecDict) of
    error -> "{" ++ comma_sequence(Elements, RecDict) ++ "}";
    {ok, FieldNames} ->
      record_to_string(TagAtom, Elements, FieldNames, RecDict)
  end;
t_to_string(?tuple_set(List), RecDict) ->
  union_sequence(lists:append([Tuple || {_Arity, Tuple} <- List]), RecDict);
t_to_string(?union(Types), RecDict) ->
  union_sequence([T || T <- Types, T =/= ?none], RecDict);
t_to_string(?var(Id), _RecDict) when is_atom(Id) ->
  io_lib:format("~s", [atom_to_list(Id)]);
t_to_string(?var(Id), _RecDict) when is_integer(Id) ->
  io_lib:format("var(~w)", [Id]).

record_to_string(Tag, [_|Fields], FieldNames, RecDict) ->
  FieldStrings = record_fields_to_string(Fields, FieldNames, RecDict, []),
  "#" ++ atom_to_list(Tag)++"{" ++ sequence(FieldStrings, [], ",") ++ "}".

record_fields_to_string([Field|Left1], [{FieldName, DeclaredType}|Left2], 
			RecDict, Acc) ->
  PrintType =
    case t_is_equal(Field, DeclaredType) of
      true -> false;
      false ->
	case t_is_any(DeclaredType) andalso t_is_atom(undefined, Field) of
	  true -> false;
	  false ->
	    TmpType = t_subtract(DeclaredType, t_atom(undefined)),
	    not t_is_equal(Field, TmpType)
	end
    end,
  case PrintType of
    false -> record_fields_to_string(Left1, Left2, RecDict, Acc);
    true ->
      String = atom_to_list(FieldName) ++ "::" ++ t_to_string(Field, RecDict),
      record_fields_to_string(Left1, Left2, RecDict, [String|Acc])
  end;
record_fields_to_string([], [], _RecDict, Acc) ->
  lists:reverse(Acc).

comma_sequence(Types, RecDict) ->
  List = [case T =:= ?any of
	    true -> "_";
	    false -> t_to_string(T, RecDict)
	  end || T <- Types], 
  sequence(List, ",").

union_sequence(Types, RecDict) ->
  List = [t_to_string(T, RecDict) || T <- Types], 
  sequence(List, " | ").

sequence(List, Delimiter) ->
  sequence(List, [], Delimiter).

sequence([], [], _Delimiter) ->
  [];
sequence([T], Acc, _Delimiter) ->
  lists:flatten(lists:reverse([T|Acc]));
sequence([T|Left], Acc, Delimiter) -> 
  sequence(Left, [T ++ Delimiter|Acc], Delimiter).

  
%%============================================================================
%% 
%% Build a type from parse forms.
%%
%%============================================================================

t_from_form(Form) ->
  t_from_form(Form, dict:new()).

t_from_form(Form, RecDict) ->
  t_from_form(Form, RecDict, dict:new()).

t_from_form({var, _L, '_'}, _RecDict, _VarDict) -> t_any();
t_from_form({var, _L, Name}, _RecDict, VarDict) ->
  case dict:find(Name, VarDict) of
    error -> t_var(Name);
    {ok, Val} -> Val
  end;
t_from_form({ann_type, _L, [_Var, Type]}, RecDict, VarDict) ->
  t_from_form(Type, RecDict, VarDict);
t_from_form({paren_type, _L, [Type]}, RecDict, VarDict) ->
  t_from_form(Type, RecDict, VarDict);
t_from_form({remote_type, _L, [_Mod, _Name, _Args]}, _RecDict, _VarDict) ->
  %% TODO: Just for now
  t_any();
t_from_form({atom, _L, Atom}, _RecDict, _VarDict) -> t_atom(Atom);
t_from_form({integer, _L, Int}, _RecDict, _VarDict) -> t_integer(Int);
t_from_form({type, _L, any, []}, _RecDict, _VarDict) -> t_any();
t_from_form({type, _L, atom, []}, _RecDict, _VarDict) -> t_atom();
t_from_form({type, _L, binary, []}, _RecDict, _VarDict) -> t_binary();
t_from_form({type, _L, binary, [{integer, _, Base}, {integer, _, Unit}]}, 
	    _RecDict, _VarDict) -> 
  t_bitstr(Unit, Base);
t_from_form({type, _L, bool, []}, _RecDict, _VarDict) -> t_bool();
t_from_form({type, _L, byte, []}, _RecDict, _VarDict) -> t_byte();
t_from_form({type, _L, char, []}, _RecDict, _VarDict) -> t_char();
t_from_form({type, _L, float, []}, _RecDict, _VarDict) -> t_float();
t_from_form({type, _L, function, []}, _RecDict, _VarDict) -> t_fun();
t_from_form({type, _L, 'fun', []}, _RecDict, _VarDict) -> t_fun();
t_from_form({type, _L, 'fun', [{type, _, any, []}, Range]}, RecDict, VarDict) ->
  t_fun(t_from_form(Range, RecDict, VarDict));
t_from_form({type, _L, 'fun', [{type, _, product, Domain}, Range]}, 
	    RecDict, VarDict) -> 
  t_fun([t_from_form(D, RecDict, VarDict) || D <- Domain], 
	t_from_form(Range, RecDict, VarDict));
t_from_form({type, _L, identifier, []}, _RecDict, _VarDict) -> t_identifier();
t_from_form({type, _L, integer, []}, _RecDict, _VarDict) -> t_integer();
t_from_form({type, _L, iolist, []}, _RecDict, _VarDict) -> t_iolist();
t_from_form({type, _L, list, []}, _RecDict, _VarDict) -> t_list();
t_from_form({type, _L, list, [Type]}, RecDict, VarDict) -> 
  t_list(t_from_form(Type, RecDict, VarDict));
t_from_form({type, _L, mfa, []}, _RecDict, _VarDict) -> t_mfa();
t_from_form({type, _L, nil, []}, _RecDict, _VarDict) -> t_nil();
t_from_form({type, _L, neg_integer, []}, _RecDict, _VarDict) -> t_neg_integer();
t_from_form({type, _L, non_neg_integer, []}, _RecDict, _VarDict) -> 
  t_non_neg_integer();
t_from_form({type, _L, no_return, []}, _RecDict, _VarDict) -> t_unit();
t_from_form({type, _L, none, []}, _RecDict, _VarDict) -> t_none();
t_from_form({type, _L, nonempty_list, []}, _RecDict, _VarDict) -> 
  t_nonempty_list();
t_from_form({type, _L, nonempty_list, [Type]}, RecDict, VarDict) -> 
  t_nonempty_list(t_from_form(Type, RecDict, VarDict));
t_from_form({type, _L, nonempty_improper_list, [Cont, Term]}, 
	    RecDict, VarDict) -> 
  t_cons(t_from_form(Cont, RecDict, VarDict), 
	 t_from_form(Term, RecDict, VarDict));
t_from_form({type, _L, nonempty_maybe_improper_list, []}, _RecDict, _VarDict) ->
  t_cons(?any, ?any);
t_from_form({type, _L, nonempty_maybe_improper_list, [Cont, Term]}, 
	    RecDict, VarDict) -> 
  t_cons(t_from_form(Cont, RecDict, VarDict), 
	 t_from_form(Term, RecDict, VarDict));
t_from_form({type, _L, number, []}, _RecDict, _VarDict) -> t_number();
t_from_form({type, _L, pid, []}, _RecDict, _VarDict) -> t_pid();
t_from_form({type, _L, port, []}, _RecDict, _VarDict) -> t_port();
t_from_form({type, _L, pos_integer, []}, _RecDict, _VarDict) -> t_pos_integer();
t_from_form({type, _L, maybe_improper_list, []}, _RecDict, _VarDict) -> 
  t_maybe_improper_list();
t_from_form({type, _L, maybe_improper_list, [Content, Termination]}, 
	    RecDict, VarDict) ->
  t_maybe_improper_list(t_from_form(Content, RecDict, VarDict), 
		      t_from_form(Termination, RecDict, VarDict));
t_from_form({type, _L, product, Elements}, RecDict, VarDict) ->
  t_product([t_from_form(E, RecDict, VarDict) || E <- Elements]);
t_from_form({type, _L, range, [{integer, _, From}, {integer, _, To}]}, 
	    _RecDict, _VarDict) -> 
  t_from_range(From, To);
t_from_form({type, _L, record, [Name|Fields]}, RecDict, VarDict) -> 
  record_from_form(Name, Fields, RecDict, VarDict);
t_from_form({type, _L, ref, []}, _RecDict, _VarDict) -> t_ref();
t_from_form({type, _L, string, []}, _RecDict, _VarDict) -> t_string();
t_from_form({type, _L, tuple, any}, _RecDict, _VarDict) -> t_tuple();
t_from_form({type, _L, tuple, Args}, RecDict, VarDict) -> 
  t_tuple([t_from_form(A, RecDict, VarDict) || A <- Args]);
t_from_form({type, _L, union, Args}, RecDict, VarDict) -> 
  t_sup([t_from_form(A, RecDict, VarDict) || A <- Args]);
t_from_form({type, _L, Name, Args}, RecDict, VarDict) ->
  case lookup_type(Name, RecDict) of
    {ok, {Type, ArgNames}} when length(Args) =:= length(ArgNames) ->
      List = lists:zipwith(fun(ArgName, ArgType) -> 
			       {ArgName, t_from_form(ArgType, RecDict, VarDict)}
			   end, ArgNames, Args),
      TmpVardict = dict:from_list(List),
      t_from_form(Type, RecDict, TmpVardict);
    {ok, _} ->
      throw({error, io_lib:format("Unknown type ~w\n", [Name])});
    error -> 
      throw({error, io_lib:format("Unknown type ~w\n", [Name])}) 
  end.

record_from_form({atom, _, Name}, ModFields, RecDict, VarDict) ->
  case lookup_record(Name, RecDict) of
    {ok, DeclFields} ->
      case get_mod_record(ModFields, DeclFields, RecDict, VarDict) of
	{error, FieldName} ->
	  throw({error, io_lib:format("Illegal declaration of ~w#{~w}\n", 
				      [Name, FieldName])});
	{ok, NewFields} ->
	  t_tuple([t_atom(Name)|[Type || {_FieldName, Type} <- NewFields]])
      end;
    error -> 
      throw({error, 
	     erlang:error(io_lib:format("Unknown record #~w{}\n", [Name]))})
  end.

get_mod_record([], DeclFields, _RecDict, _VarDict) ->
  {ok, DeclFields};
get_mod_record(ModFields, DeclFields, RecDict, VarDict) ->
  DeclFieldsDict = orddict:from_list(DeclFields),
  ModFieldsDict = build_field_dict(ModFields, RecDict, VarDict),
  case get_mod_record(DeclFieldsDict, ModFieldsDict, []) of
    {error, _FieldName} = Error -> Error;
    {ok, FinalOrdDict} ->
      {ok, [{FieldName, orddict:fetch(FieldName, FinalOrdDict)}
	    || {FieldName, _} <- DeclFields]}
  end.

build_field_dict(FieldTypes, RecDict, VarDict) ->
  build_field_dict(FieldTypes, RecDict, VarDict, []).

build_field_dict([{type, _, field_type, [{atom, _, Name}, Type]}|Left], 
		 RecDict, VarDict, Acc) ->
  NewAcc = [{Name, t_from_form(Type, RecDict, VarDict)}|Acc],
  build_field_dict(Left, RecDict, VarDict, NewAcc);
build_field_dict([], _RecDict, _VarDict, Acc) ->
  orddict:from_list(Acc).

get_mod_record([{FieldName, DeclType}|Left1], 
	       [{FieldName, ModType}|Left2], Acc) ->
  case t_is_var(ModType) orelse t_is_subtype(ModType, DeclType) of
    false -> {error, FieldName};
    true -> get_mod_record(Left1, Left2, [{FieldName, ModType}|Acc])
  end;
get_mod_record([{FieldName1, _DeclType} = DT|Left1], 
	       [{FieldName2, _ModType}|_] = List2, 
	       Acc) when FieldName1 < FieldName2 ->
  get_mod_record(Left1, List2, [DT|Acc]);
get_mod_record(DeclFields, [], Acc) ->
  {ok, orddict:from_list(Acc ++ DeclFields)};
get_mod_record(_, [{FieldName2, _ModType}|_], _Acc) ->
  {error, FieldName2}.

t_form_to_string({var, _L, '_'}) -> "_";
t_form_to_string({var, _L, Name}) -> atom_to_list(Name);
t_form_to_string({atom, _L, Atom}) -> 
  io_lib:write_string(atom_to_list(Atom), $'); % To quote or not to quote... '
t_form_to_string({integer, _L, Int}) -> integer_to_list(Int);
t_form_to_string({ann_type, _L, [Var, Type]}) ->
  t_form_to_string(Var) ++ "::" ++ t_form_to_string(Type);
t_form_to_string({paren_type, _L, [Type]}) ->
  io_lib:format("(~s)", [t_form_to_string(Type)]);
t_form_to_string({remote_type, _L, [{atom, _, Mod}, {atom, _, Name}, Args]}) ->
  ArgString = "(" ++ sequence(t_form_to_string_list(Args), ",") ++ ")",
  io_lib:format("~w:~w", [Mod, Name]) ++ ArgString;
t_form_to_string({type, _L, 'fun', []}) -> "fun()";
t_form_to_string({type, _L, 'fun', [{type, _, any, []}, Range]}) -> 
  "fun(...) -> " ++ t_form_to_string(Range);
t_form_to_string({type, _L, 'fun', [{type, _, product, Domain}, Range]}) ->
  "fun((" ++ sequence(t_form_to_string_list(Domain), ",") ++ ") -> " 
    ++ t_form_to_string(Range) ++ ")";
t_form_to_string({type, _L, list, [Type]}) -> 
  "[" ++ t_form_to_string(Type) ++ "]";
t_form_to_string({type, _L, nonempty_list, [Type]}) ->
  "[" ++ t_form_to_string(Type) ++ ",...]";
t_form_to_string({type, _L, product, Elements}) ->
  "<" ++ sequence(t_form_to_string_list(Elements), ",") ++ ">";
t_form_to_string({type, _L, range, [{integer, _, From}, {integer, _, To}]}) ->
  io_lib:format("~w..~w", [From, To]);
t_form_to_string({type, _L, record, [{atom, _, Name}]}) ->
  io_lib:format("#~w{}", [Name]);
t_form_to_string({type, _L, record, [{atom, _, Name}|Fields]}) ->
  FieldString = sequence(t_form_to_string_list(Fields), ","),
  io_lib:format("#~w{~s}", [Name, FieldString]);
t_form_to_string({type, _L, field_type, [{atom, _, Name}, Type]}) ->
  io_lib:format("~w::~s", [Name, t_form_to_string(Type)]);
t_form_to_string({type, _L, tuple, any}) -> "tuple()";
t_form_to_string({type, _L, tuple, Args}) ->
  "{" ++ sequence(t_form_to_string_list(Args), ",") ++ "}";
t_form_to_string({type, _L, union, Args}) ->
  sequence(t_form_to_string_list(Args), " | ");
t_form_to_string({type, _L, mfa, []}) -> "mfa()";
t_form_to_string(T = {type, _L, Name, []}) ->
  try t_to_string(t_from_form(T))
  catch throw:{error, _} -> atom_to_list(Name) ++ "()"
  end;
t_form_to_string({type, _L, Name, List}) -> 
  io_lib:format("~w(~s)", [Name, sequence(t_form_to_string_list(List), ",")]).

t_form_to_string_list(List) ->
  t_form_to_string_list(List, []).

t_form_to_string_list([H|T], Acc) ->
  t_form_to_string_list(T, [t_form_to_string(H)|Acc]);
t_form_to_string_list([], Acc) ->
  lists:reverse(Acc).  
  
%%============================================================================
%% 
%% Utilities
%%
%%============================================================================

%% any_none([?none|_Left]) -> true;
%% any_none([_|Left]) -> any_none(Left);
%% any_none([]) -> false.

any_none_or_unit([?none|_]) -> true;
any_none_or_unit([?unit|_]) -> true;
any_none_or_unit([_|Left]) -> any_none_or_unit(Left);
any_none_or_unit([]) -> false.

lookup_record(Tag, RecDict) ->  
  case dict:find(Tag, RecDict) of
    {ok, [{_Arity, Fields}]} -> {ok, Fields};
    {ok, List} when is_list(List) ->
      %% This will have to do, since we do not know which record we
      %% are looking for.
      error;
    error ->
      error
  end.

lookup_record(Tag, Arity, RecDict) ->  
  case dict:find(Tag, RecDict) of
    {ok, [{Arity, Fields}]} -> {ok, Fields};
    {ok, OrdDict} -> orddict:find(Arity, OrdDict);
    error ->
      error
  end.

lookup_type(Name, RecDict) ->
  dict:find({type, Name}, RecDict).

type_is_defined(Name, RecDict) ->
  dict:is_key({type, Name}, RecDict).
      
%% -----------------------------------
%% Set
%%

set_singleton(Element) ->
  [Element].

set_is_element(Element, Set) ->
  ordsets:is_element(Element, Set).

set_union(?any, _) -> ?any;
set_union(_, ?any) -> ?any;
set_union(S1, S2)  -> 
  case ordsets:union(S1, S2) of
    S when length(S) =< ?SET_LIMIT -> S;
    _ -> ?any
  end.

%% The intersection and subtraction can return ?none. 
%% This should always be handled right away since ?none is not a valid set.
%% However, ?any is considered a valid set.

set_intersection(?any, S) -> S;
set_intersection(S, ?any) -> S;
set_intersection(S1, S2)  -> 
  case ordsets:intersection(S1, S2) of
    [] -> ?none;
    S -> S
  end.      

set_subtract(_, ?any) -> ?none;
set_subtract(?any, _) -> ?any;
set_subtract(S1, S2) ->
  case ordsets:subtract(S1, S2) of
    [] -> ?none;
    S -> S
  end.

set_from_list(List) ->
  case length(List) of
    L when L =< ?SET_LIMIT -> ordsets:from_list(List);
    L when L > ?SET_LIMIT -> ?any
  end.

set_to_list(Set) ->
  Set.

set_filter(Fun, Set) ->
  case lists:filter(Fun, Set) of
    [] -> ?none;
    NewSet -> NewSet
  end.

set_size(?any) ->
  ?any;
set_size(Set) ->
  length(Set).

set_to_string(Set) ->
  List = set_to_list(Set),
  List1 = 
    [case is_atom(X) of
       true -> io_lib:write_string(atom_to_list(X), $'); % This is not a quote '
       false -> io_lib:format("~w", [X])
     end|| X <- List],
  sequence(List1, [], " | ").

set_min([H|_]) -> H.

set_max(Set) ->
  hd(lists:reverse(Set)).


%%============================================================================
%% 
%% Utilities for the binary type
%%
%%============================================================================

gcd(A,B) when B > A ->
  gcd1(B,A);
gcd(A,B) ->
  gcd1(A,B).
  
gcd1(A,0) -> A;
gcd1(A,B) ->
  case A rem B of
    0 -> B;
    X -> gcd1(B,X)
  end.

bitstr_concat(?none,_) -> ?none;
bitstr_concat(_,?none) -> ?none;
bitstr_concat(?bitstr(U1,B1),?bitstr(U2,B2)) ->
  t_bitstr(gcd(U1,U2),B1+B2).

bitstr_match(?none,_) -> ?none;
bitstr_match(_,?none) -> ?none;
bitstr_match(?bitstr(0,B1),?bitstr(0,B2)) when B1 =< B2 ->
  t_bitstr(0,B2-B1);
bitstr_match(?bitstr(0,_B1),?bitstr(0,_B2)) ->
  ?none;
bitstr_match(?bitstr(0,B1),?bitstr(U2,B2)) when B1 =< B2 ->
  t_bitstr(U2,B2-B1);
bitstr_match(?bitstr(0,B1),?bitstr(U2,B2))  ->
  t_bitstr(U2,handle_base(U2,B2-B1));
bitstr_match(?bitstr(_,B1),?bitstr(0,B2)) when B1 > B2 ->
  ?none;
bitstr_match(?bitstr(U1,B1),?bitstr(U2,B2)) ->
  GCD = gcd(U1,U2),
  t_bitstr(GCD,handle_base(GCD,B2-B1)).

handle_base(Unit, Pos) when Pos >= 0 ->
  Pos rem Unit;
handle_base(Unit, Neg) ->
  (Unit+(Neg rem Unit)) rem Unit.

      
%%============================================================================
%% Consistency-testing function(s) below
%%============================================================================

-ifdef(DO_ERL_TYPES_TEST).		  

test() ->
  Atom1  = t_atom(),
  Atom2  = t_atom(foo),
  Atom3  = t_atom(bar),
  true   = t_is_atom(Atom2),

  True   = t_atom(true),
  False  = t_atom(false),
  Bool   = t_bool(),
  true   = t_is_bool(True),
  true   = t_is_bool(Bool),
  false  = t_is_bool(Atom1),

  Binary = t_binary(),
  true   = t_is_binary(Binary),

  Bitstr = t_bitstr(),
  true   = t_is_bitstr(Bitstr),
  
  Bitstr1 = t_bitstr(7,3),
  true   = t_is_bitstr(Bitstr1),
  false  = t_is_binary(Bitstr1),

  Bitstr2 = t_bitstr(16,8),
  true   = t_is_bitstr(Bitstr2),
  true   = t_is_binary(Bitstr2),
  
  ?bitstr(8,16) = t_subtract(t_bitstr(4,12),t_bitstr(8,12)),
  ?bitstr(8,16) = t_subtract(t_bitstr(4,12),t_bitstr(8,12)),

  Int1   = t_integer(),
  Int2   = t_integer(1),
  Int3   = t_integer(16#ffffffff),
  true   = t_is_integer(Int2),
  true   = t_is_byte(Int2),
  false  = t_is_byte(Int3),
  false  = t_is_byte(t_from_range(-1, 1)),
  true   = t_is_byte(t_from_range(1, ?MAX_BYTE)),
  
  Tuple1 = t_tuple(),
  Tuple2 = t_tuple(3),
  Tuple3 = t_tuple([Atom1, Int1]),
  Tuple4 = t_tuple([Tuple1, Tuple2]),
  Tuple5 = t_tuple([Tuple3, Tuple4]),
  Tuple6 = t_limit(Tuple5, 2),
  Tuple7 = t_limit(Tuple5, 3),
  true   = t_is_tuple(Tuple1),  
  
  Port   = t_port(),
  Pid    = t_pid(),
  Ref    = t_ref(),
  Identifier = t_identifier(),
  false  = t_is_ref(Port),
  true   = t_is_identifier(Port),

  Function1 = t_fun(),
  Function2 = t_fun(Pid),
  Function3 = t_fun([], Pid),
  Function4 = t_fun([Port, Pid], Pid),
  Function5 = t_fun([Pid, Atom1], Int2),
  true      = t_is_fun(Function3),  

  List1 = t_list(),
  List2 = t_list(t_bool()),
  List3 = t_cons(t_bool(), List2),
  List4 = t_cons(t_bool(), t_atom()),
  List5 = t_cons(t_bool(), t_nil()),
  List6 = t_cons_tl(List5),
  List7 = t_sup(List4, List5),
  List8 = t_inf(List7, t_list()),
  List9 = t_cons(),
  List10 = t_cons_tl(List9),
  true  = t_is_bool(t_cons_hd(List5)),
  true  = t_is_list(List5),
  false = t_is_list(List4),

  Product1 = t_product([Atom1, Atom2]),
  Product2 = t_product([Atom3, Atom1]),
  Product3 = t_product([Atom3, Atom2]),

  Union1 = t_sup(Atom2, Atom3),
  Union2 = t_sup(Tuple2, Tuple3),
  Union3 = t_sup(Int2, Atom3),
  Union4 = t_sup(Port, Pid),
  Union5 = t_sup(Union4, Int1),
  Union6 = t_sup(Function1, Function2),
  Union7 = t_sup(Function4, Function5),
  Union8 = t_sup(True, False),
  true   = t_is_bool(Union8),
  Union9 = t_sup(Int2, t_integer(2)),
  true   = t_is_byte(Union9),
  Union10 = t_sup(t_tuple([t_atom(true), ?any]), 
		  t_tuple([t_atom(false), ?any])),
  
  ?any   = t_sup(Product3, Function5),

  Atom3  = t_inf(Union3, Atom1),
  Union2 = t_inf(Union2, Tuple1),
  Int2   = t_inf(Int1, Union3),
  Union4 = t_inf(Union4, Identifier),
  Port   = t_inf(Union5, Port),
  Function4 = t_inf(Union7, Function4),
  ?none  = t_inf(Product2, Atom1),
  Product3 = t_inf(Product1, Product2),
  Function5 = t_inf(Union7, Function5),
  true   = t_is_byte(t_inf(Union9, t_number())),
  true   = t_is_char(t_inf(Union9, t_number())),

  io:format("3? ~p ~n", [?int_set([3])]),

  RecDict = dict:store({foo, 2}, [bar, baz], dict:new()),
  Record1 = t_from_term({foo, [1,2], {1,2,3}}),
  
  Types = [
	   Atom1,
	   Atom2,
	   Atom3,
	   Binary,
	   Int1,
	   Int2,
	   Tuple1,
	   Tuple2,
	   Tuple3,
	   Tuple4,
	   Tuple5,
	   Tuple6,
	   Tuple7,
	   Ref,
	   Port,
	   Pid,
	   Identifier,
	   List1,
	   List2,
	   List3,
	   List4,
	   List5,
	   List6,
	   List7,
	   List8,
	   List9,
	   List10,
	   Function1,
	   Function2,
	   Function3,
	   Function4,
	   Function5,
	   Product1,
	   Product2,
	   Record1,
	   Union1,
	   Union2,
	   Union3,
	   Union4,
	   Union5,
	   Union6,
	   Union7,
	   Union8,
	   Union10,
	   t_inf(Union10, t_tuple([t_atom(true), t_integer()]))
	  ],
  io:format("~p\n", [[t_to_string(X, RecDict) || X <- Types]]).
		       
-endif.
