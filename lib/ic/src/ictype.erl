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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(ictype).


-include("ic.hrl").
-include("icforms.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([type_check/2, scoped_lookup/4, maybe_array/5, to_uppercase/1]).

-export([name2type/2, member2type/3, isBasicTypeOrEterm/3, isEterm/3]).
-export([isBasicType/1, isBasicType/3, isString/3, isArray/3, isStruct/3, 
	 isUnion/3, isEnum/3, isSequence/3, isBoolean/3 ]).
-export([fetchTk/3, fetchType/1, tk/4]).
%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
%%-define(DBG(F,A), io:format(F,A)).
-define(DBG(F,A), true).
-define(STDDBG, ?DBG("    dbg: ~p: ~p~n", [element(1,X), ic_forms:get_id2(X)])).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

type_check(G, Forms) ->
    S = icgen:tktab(G),
    check_list(G, S, [], Forms).

scoped_lookup(G, S, N, X) ->
    Id = icgen:scoped_id_strip(X),
    case icgen:scoped_id_is_global(X) of
	true ->
	    lookup(G, S, [], X, Id);
	false ->
	    lookup(G, S, N, X, Id)
    end.

%%--------------------------------------------------------------------
%% maybe_array
%%
%%	Array declarators are indicated on the declarator and not on
%%	the type, therefore the declarator decides if the array type
%%	kind is added or not.
%%
maybe_array(G, S, N, X, TK) when record(X, array) ->
    mk_array(G, S, N, X#array.size, TK);
maybe_array(G, S, N, _, TK) -> TK.



name2type( G, Name ) ->
    S = icgen:tktab( G ),
    ScopedName = lists:reverse(string:tokens(Name,"_")),
    InfoList = ets:lookup( S, ScopedName ),
    filter( InfoList ).


%% This is en overloaded function,
%% differs in input on unions
member2type(G, X, I) when record(X, union)->
    Name = icgen:get_id2(I),
    case lists:keysearch(Name,2,element(6,X#union.tk)) of
	false ->
	    error;
	{value,Rec} ->
	    fetchType(element(3,Rec))
    end;
member2type( G, SName, MName ) ->
    S = icgen:tktab( G ),
    SNList = lists:reverse(string:tokens(SName,"_")),
    ScopedName = [MName | SNList],
    InfoList = ets:lookup( S, ScopedName ),
    case filter( InfoList ) of
	error ->
	    error;
	Other ->
	    Other
    end.


isString(G, N, T) when element(1, T) == scoped_id ->
    case icgen:get_full_scoped_name(G, N, T) of
	{FullScopedName, _, {'tk_string',_}, _} ->
	    true;
	_ ->
	    false
    end; 
isString(G, N, T)  when record(T, string) ->
    true;
isString(G, N, Other) ->
    false. 


isArray(G, N, T) when element(1, T) == scoped_id ->
    case icgen:get_full_scoped_name(G, N, T) of
	{FullScopedName, _, {'tk_array', _, _}, _} ->
	    true;
	_ ->
	    false
    end; 
isArray(G, N, T)  when record(T, array) ->
    true;
isArray(G, N, Other) ->
    false. 


isSequence(G, N, T) when element(1, T) == scoped_id ->
    case icgen:get_full_scoped_name(G, N, T) of
	{FullScopedName, _, {'tk_sequence', _, _}, _} ->
	    true;
	_ ->
	    false
    end; 
isSequence(G, N, T)  when record(T, sequence) ->
    true;
isSequence(G, N, Other) ->
    false. 


isStruct(G, N, T) when element(1, T) == scoped_id ->
    case icgen:get_full_scoped_name(G, N, T) of
	{FullScopedName, _, {'tk_struct', _, _, _}, _} ->
	    true;
	_ ->
	    false
    end; 
isStruct(G, N, T)  when record(T, struct) ->
    true;
isStruct(G, N, Other) ->
    false.


isUnion(G, N, T) when element(1, T) == scoped_id ->
    case icgen:get_full_scoped_name(G, N, T) of
        {FullScopedName, _, {'tk_union', _, _, _,_,_}, _} ->
            true;
        _Other ->
            false
    end; 
isUnion(G, N, T)  when record(T, union) ->
    true;
isUnion(G, N, _Other) ->
    false.



isEnum(G, N, T) when element(1, T) == scoped_id ->
    case icgen:get_full_scoped_name(G, N, T) of
        {FullScopedName, _, {'tk_enum',_,_,_}, _} ->
            true;
        _Other ->
            false
    end; 
isEnum(G, N, T)  when record(T, enum) ->
    true;
isEnum(G, N, _Other) ->
    false.



isBoolean(G, N, T) when element(1, T) == scoped_id ->
    {_, _, TK, _} =
	icgen:get_full_scoped_name(G, N, T),
    case fetchType(TK) of
	'boolean' ->
	    true;
	_ ->
	    false
    end;
isBoolean(_, _, {'tk_boolean',_}) ->
    true;
isBoolean(_, _, {'boolean',_}) ->
    true;
isBoolean(_, _, _) ->
    false.


%%%  Just used for C

isBasicTypeOrEterm(G, N, S) ->
    case isBasicType(G, N, S) of
	true ->
	    true;
	false ->
	    isEterm(G, N, S)
    end.

isEterm(G, N, S) when element(1, S) == scoped_id -> 
    {FullScopedName, _, TK, _} = icgen:get_full_scoped_name(G, N, S),
    case icgen:get_basetype(G, icgen:to_undersc(FullScopedName)) of
	"erlang_term" ->
	    true;
	"ETERM*" ->
	    true;
	X ->
	    false
    end;
isEterm(G, Ni, X) -> 
    false.

isBasicType(G, N, S) when element(1, S) == scoped_id -> 
    {_, _, TK, _} = icgen:get_full_scoped_name(G, N, S),
    isBasicType(fetchType(TK));
isBasicType(G, N, {string, _} ) -> 
    false;
isBasicType(G, N, {unsigned, {long, _}} ) -> 
    true;
isBasicType(G, N, {unsigned, {short, _}} ) -> 
    true;
isBasicType(G, N, {Type, _} ) -> 
    isBasicType(Type);
isBasicType(G, N, X) ->
    false.


isBasicType( G, Name ) ->
    isBasicType( name2type( G, Name ) ).


isBasicType( Type ) ->
    lists:member(Type,
		 [tk_short,short,
		  tk_long,long,
		  tk_ushort,ushort,
		  tk_ulong,ulong,
		  tk_float,float,
		  tk_double,double,
		  tk_boolean,boolean,
		  tk_char,char,
		  tk_octet,octet]).



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
check(G, S, N, X) when record(X, preproc) ->
    handle_preproc(G, N, X#preproc.cat, X),
    X;

check(G, S, N, X) when record(X, op) ->
    ?STDDBG,
    TK = tk_base(G, S, N, ic_forms:get_type(X)),
    tktab_add(G, S, N, X),
    N2 = [ic_forms:get_id2(X) | N],
    Ps = lists:map(fun(P) -> 
		     tktab_add(G, S, N2, P),
		     P#param{tk=tk_base(G, S, N, ic_forms:get_type(P))} end,
	     X#op.params),
    %% Check for exception defs.
    Raises = lists:map(fun(E) -> name_lookup(G, S, N, E) end,
		 X#op.raises),
    case icgen:is_oneway(X) of
	true ->
	    if  TK /= tk_void ->
		    icgen:error(G, {bad_oneway_type, X, TK});
		true -> ok
	    end,
	    case ic:filter_params([inout, out], X#op.params) of
		[] -> ok;			% No out parameters!
		_ ->
		    icgen:error(G, {oneway_outparams, X})
	    end,
	    case X#op.raises of
		[] -> ok;
		_ ->
		    icgen:error(G, {oneway_raises, X})
	    end;
	false -> 
	    ok
    end,
    X#op{params=Ps, tk=TK, raises=Raises};

check(G, S, N, X) when record(X, interface) ->
    ?STDDBG,
    N2 = [ic_forms:get_id2(X) | N],
    TK = {tk_objref, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X)},
    Inherit = inherit_resolve(G, S, N, X#interface.inherit, []),
    tktab_add(G, S, N, X, TK, Inherit),
    CheckedBody = check_list(G, S, N2, ic_forms:get_body(X)),
    InhBody = calc_inherit_body(G, N2, CheckedBody, Inherit, []),
    X2 = X#interface{inherit=Inherit, tk=TK, body=CheckedBody,
		     inherit_body=InhBody},
    icgen:symtab_store(G, N, X2),
    X2;

check(G, S, N, X) when record(X, forward) ->
    ?STDDBG,
    tktab_add(G, S, N, X, {tk_objref, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X)}),
    X;

check(G, S, N, X) when record(X, const) ->
    ?STDDBG,
    case tk_base(G, S, N, ic_forms:get_type(X)) of
	Err when element(1, Err) == error -> X;
	TK ->
	    check_const_tk(G, S, N, X, TK),
	    case iceval:eval_const(G, S, N, TK, X#const.val) of
		Err when element(1, Err) == error -> X;
		Val ->
		    V = iceval:get_val(Val),
		    tktab_add(G, S, N, X, TK, V),
		    X#const{val=V, tk=TK}
	    end
    end;

check(G, S, N, X) when record(X, except) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#except{tk=TK};

check(G, S, N, X) when record(X, struct) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#struct{tk=TK};

check(G, S, N, X) when record(X, enum) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#enum{tk=TK};

check(G, S, N, X) when record(X, union) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#union{tk=TK};

check(G, S, N, X) when record(X, attr) ->
    ?STDDBG,
    TK = tk_base(G, S, N, ic_forms:get_type(X)),
    XX = #id_of{type=X},
    lists:foreach(fun(Id) -> tktab_add(G, S, N, XX#id_of{id=Id}) end,
		  icgen:get_idlist(X)),
    X#attr{tk=TK};

check(G, S, N, X) when record(X, module) ->
    ?STDDBG,
    tktab_add(G, S, N, X),
    X#module{body=check_list(G, S, [ic_forms:get_id2(X) | N], ic_forms:get_body(X))};

check(G, S, N, X) when record(X, typedef) ->
    ?STDDBG,
    TKbase = tk(G, S, N, X),
    X#typedef{tk=TKbase};

check(G, S, N, X) ->
    ?DBG("    dbg: ~p~n", [element(1,X)]),
    X.

handle_preproc(G, N, line_nr, X) -> icgen:set_idlfile(G, ic_forms:get_id2(X));
handle_preproc(G, N, C, X) -> ok.


%%--------------------------------------------------------------------
%%
%% TK calculation
%%
%%--------------------------------------------------------------------

tk(G, S, N, X) when record(X, union) ->
    N2 = [ic_forms:get_id2(X) | N],
    DisrcTK = tk(G, S, N, ic_forms:get_type(X)),
    case check_switch_tk(G, S, N, X, DisrcTK) of
	true ->
	    do_special_enum(G, S, N2, ic_forms:get_type(X)),
	    BodyTK = lists:reverse(
		       tk_caselist(G, S, N2, DisrcTK, ic_forms:get_body(X))),
	    tktab_add(G, S, N, X, 
		      {tk_union, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X),
		       DisrcTK, default_count(ic_forms:get_body(X)), BodyTK});
	false ->
	    tk_void
    end;

tk(G, S, N, X) when record(X, enum) ->
    N2 = [ic_forms:get_id2(X) | N],
    tktab_add(G, S, N, X,
	      {tk_enum, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X), 
	       enum_body(G, S, N2, ic_forms:get_body(X))});

%% Note that the TK returned from this function is the base TK. It
%% must be modified for each of the identifiers in the idlist (for
%% array reasons).
tk(G, S, N, X) when record(X, typedef) ->
    TK = tk(G, S, N, ic_forms:get_body(X)),
    lists:foreach(fun(Id) ->
		    tktab_add(G, S, N, #id_of{id=Id, type=X}, 
			      maybe_array(G, S, N, Id, TK))
	    end,
	    X#typedef.id),
    TK;

tk(G, S, N, X) when record(X, struct) ->
    N2 = [ic_forms:get_id2(X) | N],
    tktab_add(G, S, N, X, {tk_struct, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X), 
			   tk_memberlist(G, S, N2, ic_forms:get_body(X))});

tk(G, S, N, X) when record(X, except) ->
    N2 = [ic_forms:get_id2(X) | N],
    tktab_add(G, S, N, X, {tk_except, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X), 
			   tk_memberlist(G, S, N2, ic_forms:get_body(X))});

tk(G, S, N, X) -> tk_base(G, S, N, X).


tk_base(G, S, N, X) when record(X, sequence) ->
    {tk_sequence, tk(G, S, N, X#sequence.type), 
     len_eval(G, S, N, X#sequence.length)};

tk_base(G, S, N, X) when record(X, string) ->
    {tk_string, len_eval(G, S, N, X#string.length)};


tk_base(G, S, N, X) when element(1, X) == scoped_id ->
    case scoped_lookup(G, S, N, X) of
	T when element(1, T) == error -> T;
	T when tuple(T) -> element(3, T);
	_ -> invalid_tk
    end;
tk_base(G, S, N, {long, _})			-> tk_long;
tk_base(G, S, N, {short, _})			-> tk_short;
tk_base(G, S, N, {'unsigned', {short, _}})	-> tk_ushort;
tk_base(G, S, N, {'unsigned', {long, _}})	-> tk_ulong;
tk_base(G, S, N, {float, _})			-> tk_float;
tk_base(G, S, N, {double, _})			-> tk_double;
tk_base(G, S, N, {boolean, _})			-> tk_boolean;
tk_base(G, S, N, {char, _})			-> tk_char;
tk_base(G, S, N, {octet, _})			-> tk_octet;
tk_base(G, S, N, {null, _})			-> tk_null;
tk_base(G, S, N, {void, _})			-> tk_void;
tk_base(G, S, N, {any, _})			-> tk_any;
tk_base(G, S, N, {'Object', _})			-> {tk_objref, "", "Object"}.


tk_list(G, S, N, [X | Xs]) ->
    F = tk(G, S, N, X),
    [F | tk_list(G, S, N, Xs)];
tk_list(G, S, N, []) -> [].


%%--------------------------------------------------------------------
%%
%% Special handling of idlists. Note that the recursion case is given
%% as accumulator to foldr. Idlists are those lists of identifiers
%% that share the same definition, i.e. multiple cases, multiple type
%% declarations, multiple member names.
%%
tk_memberlist(G, S, N, [X | Xs]) ->
    BaseTK = tk(G, S, N, ic_forms:get_type(X)),
    
    XX = #id_of{type=X},
    lists:foldr(fun(Id, Acc) ->
		  [tk_member(G, S, N, XX#id_of{id=Id}, BaseTK) | Acc] end, 
	  tk_memberlist(G, S, N, Xs), 
	  icgen:get_idlist(X));
tk_memberlist(G, S, N, []) -> [].

%% same as above but for case dcls
tk_caselist(G, S, N, DiscrTK, Xs) ->
    lists:foldl(fun(Case, Acc) ->
		  BaseTK = tk(G, S, N, ic_forms:get_type(Case)),
		  %% tktab_add for the uniqueness check of the declarator
		  tktab_add(G, S, N, Case),
		  lists:foldl(fun(Id, Acc2) ->
				case tk_case(G, S, N, Case, BaseTK,
					     DiscrTK, Id) of
				    Err when element(1, Err)==error ->
					Acc2;
				    TK ->
					unique_add_case_label(G, S, N, Id, 
							      TK, Acc2)
				end
			end, 
			Acc,
			icgen:get_idlist(Case))
	  end,
	  [],
	  Xs).


%% Handling of the things that can be in an idlist or caselist
tk_member(G, S, N, X, BaseTK) ->
    %% io:format("Into tk: ~p~n", [X]),
    tktab_add(G, S, N, X, 
	      {ic_forms:get_id2(X), maybe_array(G, S, N, X#id_of.id, BaseTK)}).


get_case_id_and_check(G, S, N, X, ScopedId) ->
    case icgen:scoped_id_is_global(ScopedId) of
	true -> icgen:error(G, {bad_scope_enum_case, ScopedId});
	false -> ok
    end,
    case icgen:scoped_id_strip(ScopedId) of
	[Id] -> Id;
	List -> icgen:error(G, {bad_scope_enum_case, ScopedId}), 
		""
    end.


tk_case(G, S, N, X, BaseTK, DiscrTK, Id) ->
    %% io:format("Into tk: ~p~n", [X]),
    case case_eval(G, S, N, DiscrTK, Id) of
	Err when element(1, Err) == error -> Err;
	Val -> 
	    case iceval:check_tk(G, DiscrTK, Val) of
		true -> 
		    {iceval:get_val(Val), ic_forms:get_id2(X),
		     maybe_array(G, S, N, X#case_dcl.id, BaseTK)};
		false ->
		    icgen:error(G, {bad_case_type, DiscrTK, X, 
				    iceval:get_val(Val)})
	    end
    end.

tktab_add(G, S, N, X) ->
    tktab_add_id(G, S, N, X, ic_forms:get_id2(X), nil, nil).
tktab_add(G, S, N, X, TK) ->
    tktab_add_id(G, S, N, X, ic_forms:get_id2(X), TK, nil).
tktab_add(G, S, N, X, TK, Aux) ->
    tktab_add_id(G, S, N, X, ic_forms:get_id2(X), TK, Aux).


tktab_add_id(G, S, N, X, Id, TK, Aux) when record(X,enumerator) ->

    %% Check if the "scl" flag is set to true
    %% if so, allow old semantics ( errornous )
    %% Warning, this is for compatibility reasons only.
    Name = case ic_options:get_opt(G, scl) of 
	       true -> 
		   [Id | N];
	       false ->
		   [Id | tl(N)]
		 end,

    UName = mk_uppercase(Name),
    case ets:lookup(S, Name) of
	[{_, forward, _, _}] when record(X, interface) -> ok;
	[XX] when record(X, forward), element(2, XX)==interface -> ok;
	[_] -> icgen:error(G, {multiply_defined, X});
	[] ->
	    case ets:lookup(S, UName) of
		[] -> ok;
		[_] -> icgen:error(G, {illegal_spelling, X})
	    end
    end,
    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
	true -> true end,
    TK;
%
% Possible fix:
%
%tktab_add_id(G, S, N, X, Id, TK, Aux) when record(X,module) ->
%    Name = [Id | N],
%    UName = mk_uppercase(Name),
%    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
%    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
%	true -> true end,
%    TK;
tktab_add_id(G, S, N, X, Id, TK, Aux) ->
    Name = [Id | N],
    UName = mk_uppercase(Name),
    case ets:lookup(S, Name) of
	[{_, forward, _, _}] when record(X, interface) -> ok;
	[XX] when record(X, forward), element(2, XX)==interface -> ok;
	[_] -> icgen:error(G, {multiply_defined, X});
	[] ->
	    case ets:lookup(S, UName) of
		[] -> ok;
		[_] -> icgen:error(G, {illegal_spelling, X})
	    end
    end,
    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
	true -> true end,
    TK.




%%--------------------------------------------------------------------
%% enum_body
%%
%%	Special because ids are treated different than usual.
%%
enum_body(G, S, N, [Enum | EnumList]) -> 
    tktab_add(G, S, N, Enum), %%%, enum_val, Enum),
%%    tktab_add(G, S, N, X, TK, V),
    [ic_forms:get_id2(Enum) | enum_body(G, S, N, EnumList)];
enum_body(G, S, N, []) -> [].


%%--------------------------------------------------------------------
%% mk_array
%%
%%	Multi dimensional arrays are written as nested tk_array
%%
mk_array(G, S, N, [Sz | Szs], TK) ->
    case iceval:eval_const(G, S, N, positive_int, Sz) of
	Err when element(1, Err) == error -> TK;
	Val ->
	    {tk_array, mk_array(G, S, N, Szs, TK), iceval:get_val(Val)}
    end;
mk_array(G, S, N, [], TK) -> TK.


%%--------------------------------------------------------------------
%% len_eval
%%
%%	Evaluates the length, which in case it has been left out is a
%%	plain 0 (zero)
%%
len_eval(G, S, N, 0) -> 0;
len_eval(G, S, N, X) -> %%iceval:eval_const(G, S, N, positive_int, X).
    case iceval:eval_const(G, S, N, positive_int, X) of
	Err when element(1, Err) == error -> 0;
	Val -> iceval:get_val(Val)
    end.


%%--------------------------------------------------------------------
%% case_eval
%%
%%	Evaluates the case label.
%%

case_eval(G, S, N, DiscrTK, X) when element(1, DiscrTK) == tk_enum,
element(1, X) == scoped_id -> 
    {tk_enum, _, _, Cases} = DiscrTK,
    Id = get_case_id_and_check(G, S, N, X, X),
    %%io:format("Matching: ~p to ~p~n", [Id, Cases]),
    case lists:member(Id, Cases) of
	true ->
	    {enum_id, Id};
	false ->
	    iceval:mk_val(scoped_lookup(G, S, N, X)) % Will generate error
    end;

case_eval(G, S, N, DiscrTK, X) -> 
    iceval:eval_e(G, S, N, X).


%% The enum declarator is in the union scope.
do_special_enum(G, S, N, X) when record(X, enum) ->
    tktab_add(G, S, N, #id_of{id=X#enum.id, type=X});
do_special_enum(G, S, N, X) ->
    ok.


unique_add_case_label(G, S, N, Id, TK, TKList) ->
    %%%io:format("check_case_labels: TK:~p TKLIST:~p ~n", [TK, TKList]),
    if  element(1, TK) == error -> 
	    TKList;
	true ->
	    case lists:keysearch(element(1, TK), 1, TKList) of
		{value, _} -> 
		    icgen:error(G, {multiple_cases, Id}),
		    TKList;
		false -> 
		    [TK | TKList]
	    end
    end.
				

%%--------------------------------------------------------------------
%% default_count
%%
%%	Returns the position of the default case.
%%
%%	Modified for OTP-2007
%%
default_count(Xs) ->
    default_count2(Xs, 0).

default_count2([X | Xs], N) -> default_count3(X#case_dcl.label, Xs, N);
default_count2([], _) -> -1.

default_count3([{default, _} | Ys], Xs, N) -> N;
default_count3([_ | Ys], Xs, N) -> default_count3(Ys, Xs, N+1);
default_count3([], Xs, N) -> default_count2(Xs, N).




%%
%% Type checks.
%%
%% Check constant type references (only for the scoped id case, others
%% are caught by the BNF)
%%
check_const_tk(G, S, N, X, tk_long) -> true;
check_const_tk(G, S, N, X, tk_short) -> true;
check_const_tk(G, S, N, X, tk_ushort) -> true;
check_const_tk(G, S, N, X, tk_ulong) -> true;
check_const_tk(G, S, N, X, tk_float) -> true;
check_const_tk(G, S, N, X, tk_double) -> true;
check_const_tk(G, S, N, X, tk_boolean) -> true;
check_const_tk(G, S, N, X, tk_char) -> true;
check_const_tk(G, S, N, X, {tk_string, Len}) -> true;
check_const_tk(G, S, N, X, TK) -> icgen:error(G, {illegal_const_t, X, TK}).


check_switch_tk(G, S, N, X, tk_long) -> true;
check_switch_tk(G, S, N, X, tk_short) -> true;
check_switch_tk(G, S, N, X, tk_ushort) -> true;
check_switch_tk(G, S, N, X, tk_ulong) -> true;
check_switch_tk(G, S, N, X, tk_boolean) -> true;
check_switch_tk(G, S, N, X, tk_char) -> true;
check_switch_tk(G, S, N, X, TK) when element(1, TK) == tk_enum -> true;
check_switch_tk(G, S, N, X, TK) -> icgen:error(G, {illegal_switch_t, X, TK}),
				   false.



%% Lookup a name
name_lookup(G, S, N, X) ->
    case scoped_lookup(G, S, N, X) of
	T when tuple(T) -> element(1, T);
	_ -> []
    end.


lookup(G, S, N, X, Id) ->
    N2 = Id ++ N,
    ?DBG("  Trying ~p ...~n", [N2]),
    case ets:lookup(S, N2) of
	[] -> 
	    case look_for_interface(G, S, [hd(N2)], tl(N2)) of
		[T] -> ?DBG("    --  found ~p~n", [T]), 
		       lookup_found(T);
		_ ->
		    if  N == [] -> icgen:error(G, {tk_not_found, X});
			true -> lookup(G, S, tl(N), X, Id)
		    end
	    end;
	[T] -> ?DBG("    --  found ~p~n", [T]),
	       lookup_found(T)
    end.

%%lookup_found(K, interface, {TK, Inh}) -> TK;
lookup_found(X) -> X.


look_for_interface(G, S, Hd, []) -> false;
look_for_interface(G, S, Hd, Tl) ->
    case ets:lookup(S, Tl) of
	[{_, interface, TK, Inh}] -> 
	    case look_in_inherit(G, S, Hd, Inh) of
		%% gather_inherit(G, S, Inh, [])) of
		[X] when tuple(X) -> [X];
		_ -> look_for_interface(G, S, Hd ++ [hd(Tl)], tl(Tl))
	    end;
	_ -> look_for_interface(G, S, Hd ++ [hd(Tl)], tl(Tl))
    end.

look_in_inherit(G, S, Id, [I | Is]) ->
    case ets:lookup(S, Id ++ I) of
	[X] when tuple(X) -> [X];
	[] ->  look_in_inherit(G, S, Id, Is)
    end;
look_in_inherit(G, S, Id, []) -> false.


%% L is a list of names
mk_uppercase(L) ->
    lists:map(fun(Z) -> lists:map(fun(X) when X>=$a, X=<$z -> X-$a+$A;
				     (X) -> X end, Z) end, L).


%%--------------------------------------------------------------------
%%
%% Inheritance stuff
%%
%%
%%--------------------------------------------------------------------

%% InhBody is an accumulating parameter

calc_inherit_body(G, N, OrigBody, [X|Xs], InhBody) ->
    case icgen:symtab_retrieve(G, X) of
	Intf when record(Intf, interface) ->
	    Body = filter_body(G, X, ic_forms:get_body(Intf), N, OrigBody, InhBody),
	    calc_inherit_body(G, N, OrigBody, Xs, [{X, Body} | InhBody]);
	XXX ->
	    io:format("Oops, not found ~p~n", [XXX]),
	    calc_inherit_body(G, N, OrigBody, Xs, InhBody)
    end;
calc_inherit_body(G, N, OrigBody, [], InhBody) -> lists:reverse(InhBody).


filter_body(G, XPath, [X | Xs], OrigPath, OrigBody, InhBody) ->
    case complex_body_member(G, XPath, X, OrigPath, OrigBody, InhBody) of
	true -> 
	    %%io:format("NOT adding ~p~n", [ic_forms:get_id2(X)]),
	    filter_body(G, XPath, Xs, OrigPath, OrigBody, InhBody);
	{false, NewX} ->			% For those with idlist
	    %%io:format("Adding from idlist~n", []),
	    [NewX | filter_body(G, XPath, Xs, OrigPath, OrigBody, InhBody)];
	false ->
	    %%io:format("Adding: ~p~n", [ic_forms:get_id2(X)]),
	    [X | filter_body(G, XPath, Xs, OrigPath, OrigBody, InhBody)]
    end;
filter_body(G, XPath, [], OrigPath, OrigBody, InhBody) -> [].


complex_body_member(G, XPath, X, OrigPath, OrigBody, InhBody) ->
    case has_idlist(X) of
	true ->
	    idlist_member(G, XPath, X, OrigPath, OrigBody, InhBody);
	false ->
	    straight_member(G, XPath, X, OrigPath, OrigBody, InhBody)
    end.


idlist_member(G, XPath, X, OrigPath, OrigBody, InhBody) ->    
    XX = #id_of{type=X},
    F = fun(Id) ->
		not(straight_member(G, XPath, XX#id_of{id=Id}, OrigPath,
				    OrigBody, InhBody))
	end,
    case lists:filter(F, icgen:get_idlist(X)) of
	[] -> 
	    true;
	IdList ->
%%%	    io:format("Idlist added: ~p~n",[IdList]),
	    {false, replace_idlist(X, IdList)}
    end.


straight_member(G, XPath, X, OrigPath, OrigBody, InhBody) ->
    %%io:format("straight member: ~p~n", [ic_forms:get_id2(X)]),
    case body_member(G, XPath, X, OrigPath, OrigBody) of
	true ->
	    true;
	false -> 
	    inh_body_member(G, XPath, X, InhBody)
    end.


inh_body_member(G, XPath, X, [{Name, Body} | InhBody]) ->
    case body_member(G, XPath, X, Name, Body) of
	true ->
	    true;
	false -> 
	    inh_body_member(G, XPath, X, InhBody)
    end;
inh_body_member(G, XPath, X, []) -> false.


body_member(G, XPath, X, YPath, [Y|Ys]) ->
    case has_idlist(Y) of
	true -> 
	    YY = #id_of{type=Y},
	    case list_and(fun(Y2) -> 
				  not(is_equal(G, XPath, X, YPath, 
					       YY#id_of{id=Y2})) end,
			  icgen:get_idlist(Y)) of
		true -> 
		    body_member(G, XPath, X, YPath, Ys);
		false ->
		    true
	    end;
	false ->
	    case is_equal(G, XPath, X, YPath, Y) of
		false ->
		    body_member(G, XPath, X, YPath, Ys);
		true ->
		    true
	    end
    end;
body_member(G, XPath, X, YPath, []) -> false.


is_equal(G, XPath, X, YPath, Y) ->
    case {ic_forms:get_id2(X), ic_forms:get_id2(Y)} of
	{ID, ID} ->
	    collision(G, XPath, X, YPath, Y),
	    true;
	_ -> 
	    false
    end.


%% X is the new item, Y is the old one. So it is X that collides with
%% Y and Y shadows X.
collision(G, XPath, X, YPath, Y) ->
    I1 = get_beef(X),
    I2 = get_beef(Y),
    if record(I1, op) -> %%, record(I2, op) ->
	    icgen:error(G, {inherit_name_collision, 
			    {YPath, Y}, {XPath, X}});
       record(I1, attr) -> %%, record(I2, attr) ->
	    icgen:error(G, {inherit_name_collision, 
			    {YPath, Y}, {XPath, X}});
       true ->
	    ?ifopt(G, warn_name_shadow, 
		   icgen:warn(G, {inherit_name_shadow, 
				  {YPath, Y}, {XPath, X}}))
    end.

has_idlist(X) when record(X, typedef) -> true;
has_idlist(X) when record(X, member) -> true;
has_idlist(X) when record(X, case_dcl) -> true;
has_idlist(X) when record(X, attr) -> true;
has_idlist(_) -> false.

replace_idlist(X, IdList) when record(X, typedef) -> X#typedef{id=IdList};
replace_idlist(X, IdList) when record(X, attr) -> X#attr{id=IdList}.

get_beef(X) when record(X, id_of) -> X#id_of.type;
get_beef(X) -> X.


%% And among all elements in list
list_and(F, [X|Xs]) ->
    case F(X) of
	true -> list_and(F, Xs);
	false -> false
    end;
list_and(F, []) -> true.





%%--------------------------------------------------------------------
%%
%%	resolve_inherit shall return a list of resolved inheritances,
%%	that is all names replaced with their global names.
%%

inherit_resolve(G, S, N, [X|Rest], Out) ->
    case scoped_lookup(G, S, N, X) of
	{Name, T, TK, Inh} ->
	    case lists:member(Name, Out) of
		true -> 
		    inherit_resolve(G, S, N, Rest, Out);
		false ->
		    case unique_append(Inh, [Name|Out]) of
			error ->
			    icgen:error(G, {inherit_resolve, X, Name}),
			    inherit_resolve(G, S, N, Rest, []);
			UA ->
			    inherit_resolve(G, S, N, Rest, UA)
		    end
	    end;
	_ -> inherit_resolve(G, S, N, Rest, Out)
    end;
inherit_resolve(G, S, N, [], Out) -> lists:reverse(Out).

unique_append([X|Xs], L) ->
    case lists:member(X, L) of
	true -> unique_append(Xs, L);
	false -> unique_append(Xs, [X|L])
    end;
unique_append([], L) -> L;
%% Error 
unique_append( _, L) -> error.




%%--------------------------------------------------------------------
%%
%%	Utilities
%%

%% Must preserve order, therefore had to write my own (instead of lists:map)
check_list(G, S, N, [X|Xs]) ->
    X1 = check(G, S, N, X),
    [X1 | check_list(G, S, N, Xs)];
check_list(G, S, N, []) -> [].



filter( [] ) ->
    error;
filter( [I | Is ] ) ->
    case I of
	{ _, member, { _, TKINFO }, _ } ->
	    fetchType( TKINFO );

        { _, struct, _, _ } ->
	    struct;

	{ _, typedef, TKINFO, _ } ->
	    fetchType( TKINFO );

	{ _, module, _, _ } ->
	    module;

	{ _, interface, _, _ } ->
	    interface;

	{ _, op, _, _ } ->
	    op;

	{ _,enum, _, _ } ->
	    enum;

	{ _, spellcheck } ->
	    filter( Is );
	
	_ ->
	    error
    end.


fetchType( { tk_sequence, _, _ } ) ->
    sequence;
fetchType( { tk_array, _, _ } ) ->
    array;
fetchType( { tk_struct, _, _, _} ) ->
    struct;
fetchType( { tk_string, _} ) ->
    string;
fetchType( tk_short ) ->
    short;
fetchType( tk_long ) ->
    long;
fetchType( tk_ushort ) ->
    ushort;
fetchType( tk_ulong ) ->
    ulong;
fetchType( tk_float ) ->
    float;
fetchType( tk_double ) ->
    double;
fetchType( tk_boolean ) ->
    boolean;
fetchType( tk_char ) ->
    char;
fetchType( tk_octet ) ->
    octet;
fetchType( { tk_enum, _, _, _ } ) ->
    enum;
fetchType( { tk_union, _, _, _, _, _ } ) ->
    union;
fetchType( tk_any ) ->
    any;
fetchType( _ ) ->
    error.

%% Z is a single name
to_uppercase(Z) ->
    lists:map(fun(X) when X>=$a, X=<$z -> X-$a+$A;
		 (X) -> X end, Z).


%%------------------------------------------------------------
%%
%% Always fetchs TK of a record.
%%
%%------------------------------------------------------------
fetchTk(G,N,X) ->
    case ic_forms:get_tk(X) of
        undefined ->
            searchTk(G,ictk:get_IR_ID(G, N, X));
        TK ->
            TK
    end.


%%------------------------------------------------------------
%%
%% seek type code when not accessible by get_tk/1
%%
%%------------------------------------------------------------
searchTk(G,IR_ID) ->
    S = icgen:tktab(G),
    case catch searchTk(S,IR_ID,typedef) of
        {value,TK} ->
            TK;
        _ -> %% false / exit
            case catch searchTk(S,IR_ID,struct) of
                {value,TK} ->
                    TK;
                _  ->  %% false / exit
                    case catch searchTk(S,IR_ID,union) of
                        {value,TK} ->
                            TK;
                        _ ->
                            undefined
                    end
            end
    end.


searchTk(S,IR_ID,Type) ->
    L = lists:flatten(ets:match(S,{'_',Type,'$1','_'})),
    case lists:keysearch(IR_ID,2,L) of
        {value,TK} ->
            {value,TK};
        false ->
            searchInsideTks(L,IR_ID)
    end.


searchInsideTks([],_IR_ID) ->
    false;
searchInsideTks([{tk_array,TK,_}|Xs],IR_ID) ->
    case searchIncludedTk(TK,IR_ID) of
        {value,TK} ->
            {value,TK};
        false ->
            searchInsideTks(Xs,IR_ID)
    end.


searchIncludedTk({tk_array,TK,_},IR_ID) ->
    searchIncludedTk(TK,IR_ID);
searchIncludedTk({tk_sequence,TK,_},IR_ID) ->
    searchIncludedTk(TK,IR_ID);
searchIncludedTk(TK,IR_ID) when atom(TK) ->
    false;
searchIncludedTk(TK,IR_ID) ->
    case element(2,TK) == IR_ID of
        true ->
            {value,TK};
        false ->
            false
    end.
        
