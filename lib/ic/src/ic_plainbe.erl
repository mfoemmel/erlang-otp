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
-module(ic_plainbe).


-export([do_gen/3]).
%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------


-import(icgen, [ mk_oe_name/2, mk_var/1, 
		 get_id/1, emit/3, to_atom/1, 
		 to_list/1, nl/1, get_id2/1, 
		 get_body/1, stubfiled/1 ]).

-import(lists, [foreach/2, map/2]).

-include("icforms.hrl").
-include("ic.hrl").

%%------------------------------------------------------------
%%
%% Generate the client side Erlang stubs.
%%
%% Each module is generated to a separate file.
%%
%% Export declarations for all interface functions must be
%% generated. Each function then needs to generate a function head and
%% a body. IDL parameters must be converted into Erlang parameters
%% (variables, capitalised) and a type signature list must be
%% generated (for later encode/decode).
%%
%%------------------------------------------------------------ 


do_gen(G, File, Form) -> 
    G2 = icgen:filename_push(G, [], mk_oe_name(G, 
					       icgen:remove_ext(to_list(File))),
			     erlang),
    gen_head(G2, [], Form),
    exportDependency(G2),
    gen(G2, [], Form),
    genDependency(G2),
    icgen:filename_pop(G2, erlang),
    ok.


gen(G, N, [X|Xs]) when record(X, preproc) ->
    NewG = ic:handle_preproc(G, N, X#preproc.cat, X),
    gen(NewG, N, Xs);

gen(G, N, [X|Xs]) when record(X, module) ->
    CD = icgen:codeDirective(G,X),
    G2 = icgen:filename_push(G, N, X, CD),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    G3 = icgen:filename_pop(G2, CD),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when record(X, interface) ->
    %% Add inheritence data to pragmatab
    ic_pragma:add_inh_data(G,N,X),
    G2 = icgen:filename_push(G, N, X, erlang),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    foreach(fun({Name, Body}) -> gen(G2, N2, Body) end, 
	    X#interface.inherit_body), 
    G3 = icgen:filename_pop(G2, erlang),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when record(X, const) ->
    N2 = [get_id2(X) | N],
    emit_constant_func(G, X#const.id, X#const.val),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, op) ->
    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
    emit_func(G, N, X, Name, ArgNames, TypeList, OutArgs),
    gen(G, N, Xs);
	

gen(G, N, [X|Xs]) when record(X, attr) ->
    emit_attr(G, N, X, fun emit_func/7),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, except) ->
    icstruct:except_gen(G, N, X, erlang),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) ->
    case may_contain_structs(X) of
	true -> icstruct:struct_gen(G, N, X, erlang);
	false -> ok
    end,
    gen(G, N, Xs);

gen(G, N, []) -> ok.


may_contain_structs(X) when record(X, typedef) -> true;
may_contain_structs(X) when record(X, struct) -> true;
may_contain_structs(X) when record(X, union) -> true;
may_contain_structs(X) -> false.




get_if([X|Rest]) when record(X, op) ->
    R = ic_forms:get_tk(X),
    IN = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		   ic:filter_params([in, inout], X#op.params)),
    OUT = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		    ic:filter_params([out, inout], X#op.params)),
    [{get_id2(X), {R, IN, OUT}} | get_if(Rest)];

get_if([X|Rest]) when record(X, attr) ->
    {GetT, SetT} = mk_attr_func_types([], X),
    AList = lists:map(fun(Id) -> 
			      {Get, Set} = mk_attr_func_names([], get_id(Id)),
			      case X#attr.readonly of
				  {readonly, _} -> 
				      {Get, GetT};
				  _ -> 
				      [{Set, SetT}, {Get, GetT}]
			      end end, icgen:get_idlist(X)),
    lists:flatten(AList) ++ get_if(Rest);

get_if([X|Rest]) -> get_if(Rest);
get_if([]) -> [].




%%------------------------------------------------------------
%%
%% Export stuff
%%
%%	Gathering of all names that should be exported from a stub
%%	file.
%%


gen_head_special(G, N, X) when record(X, interface) ->
    Fd = icgen:stubfiled(G),

    foreach(fun({Name, Body}) ->
		    icgen:comment(Fd, "Exports from ~p", 
				  [icgen:to_colon(Name)]),
		    icgen:export(Fd, exp_top(G, N, Body, [])),
		    nl(Fd)
	    end, X#interface.inherit_body),
    Fd;
	

gen_head_special(G, N, X) -> ok.

    

%% Shall generate all export declarations
gen_head(G, N, X) -> 
    case icgen:is_stubfile_open(G) of
	true -> 
	    F = stubfiled(G),
	    icgen:comment(F, "Interface functions"),
	    icgen:export(F, exp_top(G, N, X, [])), 
	    nl(F),
	    gen_head_special(G, N, X);
	false -> ok
    end.

exp_top(_G, _N, X, Acc)  when element(1, X) == preproc -> 
    Acc;
exp_top(G, N, L, Acc)  when list(L) ->
    exp_list(G, N, L, Acc);
exp_top(G, N, M, Acc)  when record(M, module) ->
    exp_list(G, N, get_body(M), Acc);
exp_top(G, N, I, Acc)  when record(I, interface) ->
    exp_list(G, N, get_body(I), Acc);
exp_top(G, N, X, Acc) ->
    exp3(G, N, X, Acc).

exp3(G, N, C, Acc)  when record(C, const) -> 
    [{get_id(C#const.id), 0} | Acc];

exp3(G, N, Op, Acc)  when record(Op, op) ->
    FuncName = get_id(Op#op.id),
    Arity = length(ic:filter_params([in, inout], Op#op.params)),
    [{FuncName, Arity} | Acc];

exp3(G, N, A, Acc)  when record(A, attr) ->
    lists:foldr(fun(Id, Acc2) ->
			{Get, Set} = mk_attr_func_names([], get_id(Id)),
			case A#attr.readonly of
			    {readonly, _} -> [{Get, 1} | Acc2];
			    _ ->             [{Get, 1}, {Set, 2} | Acc2]
			end end, Acc, icgen:get_idlist(A));

exp3(_G, _N, _X, Acc) -> Acc.

exp_list(G, N, L, OrigAcc) -> 
    lists:foldr(fun(X, Acc) -> exp3(G, N, X, Acc) end, OrigAcc, L).




%%------------------------------------------------------------
%%
%% Emit stuff
%%
%%	Low level generation primitives
%%


emit_func(G, N, X, Name, ArgNames, TypeList, OutArgs) ->
    case icgen:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = icgen:stubfiled(G),
	    OpName = list_to_atom(Name),
	    ArgList = mk_list(ArgNames),
	    emit_op_comment(G, Fd, X, OpName, ArgNames, OutArgs),
	    emit(Fd, "~p(~s) ->\n", [OpName,ArgList]),
	    emit(Fd, "    ~p:~p(~s).\n\n", [to_atom(icgen:impl(G)), OpName, ArgList])
    end.

emit_attr(G, N, X, F) ->
    XX = #id_of{type=X},
    {GetType, SetType} = mk_attr_func_types(N, X),
    lists:foreach(fun(Id) ->
			  X2 = XX#id_of{id=Id},
			  {Get, Set} = mk_attr_func_names(N, get_id(Id)),
			  F(G, N, X2, Get, [], GetType, []),
			  case X#attr.readonly of
			      {readonly, _} -> ok;
			      _ -> 
				  F(G, N, X2, Set, [icgen:mk_name(G, "Value")], 
				    SetType, [])
			  end end, icgen:get_idlist(X)).

emit_constant_func(G, Id, Val) ->
    case icgen:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = stubfiled(G),
	    N = list_to_atom(get_id(Id)),
	    emit_const_comment(G, Fd, Id, N),
	    emit(Fd, "~p() -> ~p.\n\n", [N, Val])
    end.


emit_const_comment(G, F, X, Name) ->
    icgen:mcomment_light(F,
			 [io_lib:format("Constant: ~p", [Name])]).


emit_op_comment(G, F, X, Name, InP, OutP) ->
    icgen:mcomment_light(F,
			 [io_lib:format("~s: ~p", [get_title(X), Name]),
			  "",
			  get_returns(G, X, InP, OutP) |
			  get_raises(X)]).

get_title(X) when record(X, attr) -> "Attribute Operation";
get_title(X) -> "Operation".

get_raises(X) when record(X, op) ->
    if  X#op.raises == [] -> [];
	true ->
	    ["  Raises:  " ++ 
	     mk_list(lists:map({icgen, to_colon}, X#op.raises))]
    end;
get_raises(X) -> [].

get_returns(G, X, InP, []) ->
    "  Returns: RetVal";
get_returns(G, X, InP, OutP) ->
    "  Returns: "++mk_list(["RetVal" | mk_erl_vars(G, OutP)]).




%%------------------------------------------------------------
%%
%% Utilities
%%
%% Convenient little go-get functions
%%
%%------------------------------------------------------------

%% The automaticly generated get and set operation names for an
%% attribute.
mk_attr_func_names(Scope, Name) ->
    {"_get_" ++ Name, "_set_" ++ Name}.

%% Returns TK of the Get and Set attribute functions.
mk_attr_func_types(N, X) ->
    TK = ic_forms:get_tk(X),
    {{TK, [], []}, {tk_void, [TK], []}}.
        


%%------------------------------------------------------------
%%
%% Generation utilities and common stuff
%%
%% Convenient stuff for generation
%%
%%------------------------------------------------------------


%% Input is a list of parameters (in parse form) and output is a list
%% of capitalised variable names. mk_var is in icgen
mk_erl_vars(G, Params) ->
    map(fun(P) -> mk_var(get_id(P#param.id)) end, Params).


%% mk_list produces a nice comma separated string of variable names
mk_list([]) -> [];
mk_list([Arg | Args]) ->
    Arg ++ mk_list2(Args).
mk_list2([Arg | Args]) ->
    ", " ++ Arg ++ mk_list2(Args);
mk_list2([]) -> [].


%%------------------------------------------------------------
%%
%% Parser utilities
%%
%% Called from the yecc parser. Expands the identifier list of an
%% attribute so that the attribute generator never has to handle
%% lists.
%%
%%------------------------------------------------------------




%% Export code produce for dependency function
exportDependency(G) ->
    Fd = stubfiled(G),
    icgen:export(Fd, [{oe_dependency, 0}]),
    nl(Fd).

%% Code produce for dependency function
genDependency(G) ->
    Fd = stubfiled(G),
    nl(Fd),nl(Fd),
    icgen:comment(Fd, "Idl file dependency list function"), 
    emit(Fd, "oe_dependency() ->\n", []),
    emit(Fd, "    ~p.\n\n", [ic_pragma:get_dependencies(G)]).   




extract_info(G, N, X) when record(X, op) ->
    Name	= get_id2(X),
    InArgs	= ic:filter_params([in,inout], X#op.params),
    OutArgs	= ic:filter_params([out,inout], X#op.params),
    ArgNames	= mk_erl_vars(G, InArgs),
    S = icgen:tktab(G),
    TypeList	= {ic_forms:get_tk(X),
		   map(fun(Y) -> ic_forms:get_tk(Y) end, InArgs),
		   map(fun(Y) -> ic_forms:get_tk(Y) end, OutArgs)
		  },
    {Name, ArgNames, TypeList, OutArgs}.
