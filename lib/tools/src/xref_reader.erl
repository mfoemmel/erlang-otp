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
%%     $Id $
%%
-module(xref_reader).

-export([module/5]).

-import(lists, [keysearch/3, member/2, reverse/1]).

-record(xrefr, 
	{module=[],
	 function=[],
	 def_at=[],
	 l_call_at=[],
	 x_call_at=[],
	 el=[],
	 ex=[],
	 x=[],
         df,
	 builtins_too=false,
	 funvars=[],          % records variables bound to funs
			      % (for coping with list comprehension)
	 matches=[],          % records other bound variables
	 unresolved=[],       % unresolved calls, {{mfa(),mfa()},Line}
	 %% experimental; -xref(FunEdge) is recognized.
	 lattrs=[],            % local calls, {{mfa(),mfa()},Line}
	 xattrs=[],            % external calls, -"-
	 battrs=[]             % badly formed xref attributes, term().
	 }).

-include("xref.hrl").

%% sys_pre_expand has modified the forms slightly compared to what
%% erl_id_trans recognizes.

%% The versions of the abstract code are as follows:
%% R7: abstract_v1
%% R8: abstract_v2

%% -> {ok, Module, {DefAt, CallAt, LC, XC, X, Attrs}, Unresolved}} | EXIT
%% Attrs = {ALC, AXC, Bad}
%% ALC, AXC and Bad are extracted from the attribute 'xref'. An experiment.
module(Module, Forms, CollectBuiltins, X, DF) ->
    S = #xrefr{module = Module, builtins_too = CollectBuiltins, 
               x = X, df = DF},
    forms(Forms, S).

forms([F | Fs], S) ->
    S1 = form(F, S),
    forms(Fs, S1);
forms([], S) -> 
    #xrefr{module = M, def_at = DefAt, 
	   l_call_at = LCallAt, x_call_at = XCallAt,
	   el = LC, ex = XC, x = X, df = Depr,
	   lattrs = AL, xattrs = AX, battrs = B, unresolved = U} = S,
    Attrs = {lists:reverse(AL), lists:reverse(AX), lists:reverse(B)},
    {ok, M, {DefAt, LCallAt, XCallAt, LC, XC, X, Attrs, Depr}, U}.

form({attribute, Line, xref, Calls}, S) -> % experimental
    #xrefr{module = M, function = Fun, 
	   lattrs = L, xattrs = X, battrs = B} = S,
    attr(Calls, Line, M, Fun, L, X, B, S);
form({attribute, _Line, _Attr, _Val}, S) ->
    S;
form({function, Line, _Name, _Arity, _Clauses}, S) when Line =:= 0 ->
    %% Skipping {'MNEMOSYNE RULE',1}, {'MNEMOSYNE QUERY', 2}, 
    %% {'MNEMOSYNE RECFUNDEF',1}, module_info/0,1.
    S;
form({function, Line, Name, Arity, Clauses}, S) ->
    MFA = {S#xrefr.module, Name, Arity},
    S1 = S#xrefr{function = MFA},
    S2 = S1#xrefr{def_at = [{MFA,Line} | S#xrefr.def_at]},
    S3 = clauses(Clauses, S2),
    S3#xrefr{function = []}.    

clauses(Cls, S) ->
    #xrefr{funvars = FunVars, matches = Matches} = S,
    clauses(Cls, FunVars, Matches, S).

clauses([{clause, _Line, _H, G, B} | Cs], FunVars, Matches, S) ->
    S1 = case S#xrefr.builtins_too of
	     true -> guard(G, S);
	     false -> S
	 end,
    S2 = expr_list(B, S1),
    S3 = S2#xrefr{funvars = FunVars, matches = Matches},
    clauses(Cs, S3);
clauses([], _FunVars, _Matches, S) -> 
    S.

guard([G0 | Gs], S) when list(G0) ->
    S1 = guard0(G0, S),
    guard(Gs, S1);
guard(L, S) ->
    guard0(L, S).

guard0([G0 | Gs], S) ->
    S1 = expr(G0, S),
    guard0(Gs, S1);
guard0([], S) -> S.

attr([E={From, To} | As], Ln, M, Fun, AL, AX, B, S) ->
    case mfa(From, M) of
	{_, _, MFA} when MFA == Fun; [] == Fun -> 
	    attr(From, To, Ln, M, Fun, AL, AX, B, S, As, E);
	{_, _, _} ->
	    attr(As, Ln, M, Fun, AL, AX, [E | B], S);
	_ ->
	    attr(Fun, E, Ln, M, Fun, AL, AX, B, S, As, E)
    end;
attr([To | As], Ln, M, Fun, AL, AX, B, S) ->
    attr(Fun, To, Ln, M, Fun, AL, AX, B, S, As, To);
attr([], _Ln, _M, _Fun, AL, AX, B, S) ->
    S#xrefr{lattrs = AL, xattrs = AX, battrs = B}.

attr(From, To, Ln, M, Fun, AL, AX, B, S, As, E) ->
    case {mfa(From, M), mfa(To, M)} of
	{{true,_,F}, {_,external,T}} ->
	    attr(As, Ln, M, Fun, AL, [{{F,T},Ln} | AX], B, S);
	{{true,_,F}, {_,local,T}} ->
	    attr(As, Ln, M, Fun, [{{F,T},Ln} | AL], AX, B, S);
	_ -> attr(As, Ln, M, Fun, AL, AX, [E | B], S)
    end.

mfa({F,A}, M) when atom(F), integer(A) ->
    {true, local, {M,F,A}};
mfa(MFA={M,F,A}, M1) when atom(M), atom(F), integer(A) ->
    {M==M1, external, MFA};
mfa(_, _M) -> false.

expr({var, _Line, _V}, S) -> S;
expr({char, _Line, _I}, S) -> S;		%R8.
expr({integer, _Line, _I}, S) -> S;
expr({float, _Line, _F}, S) -> S;
expr({atom, _Line, _A}, S) -> S;
expr({string, _Line, _St}, S) -> S;
expr({nil, _Line}, S) -> S;
expr({cons, _Line, H, T}, S) ->
    S1 = expr(H, S),
    expr(T, S1);
expr({lc, _Line, E, Qs}, S) ->
    S1 = lc_quals(Qs, S),
    expr(E, S1);
expr({tuple, _Line, Es}, S) ->
    expr_list(Es, S);
expr({block, _Line, Es}, S) ->
    expr_list(Es, S);
expr({'if', _Line, Cs}, S) ->
    clauses(Cs, S);
expr({'case', _Line, E, Cs}, S) ->
    S1 = expr(E, S),
    clauses(Cs, S1);
expr({'receive', _Line, Cs}, S) ->
    clauses(Cs, S);
expr({'receive', _Line, Cs, To, ToEs}, S) ->
    S1 = expr(To, S),
    S2 = expr_list(ToEs, S1),
    clauses(Cs, S2);
expr({'try',_Line,Es,Scs,Ccs,As}, S) ->
    S1 = expr_list(Es, S),
    S2 = clauses(Scs, S1),
    S3 = clauses(Ccs, S2),
    expr_list(As, S3);
expr({'fun', Line, {function, Name, Arity}, _Extra}, S) ->
    %% Added in R8.
    handle_call(local, S#xrefr.module, Name, Arity, Line, S);
expr({'fun', _Line, {clauses, Cs} ,_Extra}, S) ->
    clauses(Cs, S);
expr({call, Line, {atom, _, Name}, As}, S) ->
    S1 = handle_call(local, S#xrefr.module, Name, length(As), Line, S),
    expr_list(As, S1);
expr({call, Line, {remote, _Line, {atom,_,Mod}, {atom,_,Name}}, As}, S) ->
    external_call(Mod, Name, As, Line, false, S);
expr({call, Line, {remote, _Line, Mod, Name}, As}, S) ->
    %% Added in R8.
    external_call(erlang, apply, [Mod, Name, list2term(As)], Line, true, S);
expr({call, Line, F, As}, S) ->
    external_call(erlang, apply, [F, list2term(As)], Line, true, S);
expr({'catch', _Line, E}, S) ->
    expr(E, S);
expr({match, _Line, {var,_,Var}, {'fun', _, {clauses, Cs}, _Extra}}, S) ->
    %% This is what is needed in R7 to avoid warnings for the functions 
    %% that are passed around by the "expansion" of list comprehension.
    S1 = S#xrefr{funvars = [Var | S#xrefr.funvars]},
    clauses(Cs, S1);
expr({match, _Line, {var,_,Var}, E}, S) ->
    %% Used for resolving code like
    %%     Args = [A,B], apply(m, f, Args)
    S1 = S#xrefr{matches = [{Var, E} | S#xrefr.matches]},
    expr(E, S1);
expr({match, _Line, _P, E}, S) ->
    expr(E, S);
expr({op, _Line, _Op, A}, S) ->
    expr(A, S);
expr({op, _Line, _Op, L, R}, S) ->
    S1 = expr(L, S),
    expr(R, S1);
expr({bin, _Line, Fs}, S) ->
    expr_list(Fs, S);
expr({bin_element, _L, E1, E2, _E3}, S) ->
    S1 = expr(E1, S),
    S2 = case E2 of
	     default -> S1;
	     _ -> expr(E2, S1)
	 end,
    S2.

expr_list([E | Es], S) ->
    S1 = expr(E, S),
    expr_list(Es, S1);
expr_list([], S) -> 
    S.

lc_quals([{generate, _Line, _P, E} | Qs], S) ->
    S1 = expr(E, S),
    lc_quals(Qs, S1);
lc_quals([E | Qs], S) ->
    S1 = expr(E, S),
    lc_quals(Qs, S1);
lc_quals([], S) ->
    S.

%% Mod and Fun may not correspond to something in the abstract code,
%% which is signalled by X == true.
external_call(Mod, Fun, ArgsList, Line, X, S) ->
    Arity = length(ArgsList),
    W = case xref_utils:is_funfun(Mod, Fun, Arity) of
	    true when erlang == Mod, apply == Fun, 2 == Arity -> apply2;
	    true when erts_debug == Mod, apply == Fun, 4 == Arity -> debug4;
	    true when erlang == Mod, spawn_opt == Fun -> Arity - 1;
	    true -> Arity;
	    false when Mod == erlang ->
		case erl_internal:type_test(Fun, Arity) of
		    true -> type;
		    false -> false
		end;
	    false -> false
	end,
    S1 = if
	     W == type; X == true ->
		 S;
	     true ->
		 handle_call(external, Mod, Fun, Arity, Line, S)
	 end,
    case {W, ArgsList} of
	{false, _} ->
	    expr_list(ArgsList, S1);
	{type, _} ->
	    expr_list(ArgsList, S1);
	{apply2, [{tuple, _, [M,F]}, ArgsTerm]} ->
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, []);
	{1, [{tuple, _, [M,F]} | R]} ->	% R = [] unless spawn_opt
	    eval_args(M, F, list2term([]), Line, S1, ArgsList, R);
	{2, [Node, {tuple, _, [M,F]} | R]} -> % R = [] unless spawn_opt
	    eval_args(M, F, list2term([]), Line, S1, ArgsList, [Node | R]);
	{3, [M, F, ArgsTerm | R]} -> % R = [] unless spawn_opt
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, R);
	{4, [Node, M, F, ArgsTerm | R]} -> % R = [] unless spawn_opt
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, [Node | R]);
	{debug4, [M, F, ArgsTerm, _]} ->
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, []);
	_Else -> % apply2, 1 or 2
	    check_funarg(W, ArgsList, Line, S1)
    end.
	    
eval_args(Mod, Fun, ArgsTerm, Line, S, ArgsList, Extra) ->
    {IsSimpleCall, M, F} = mod_fun(Mod, Fun),
    case term2list(ArgsTerm, [], S) of
	undefined ->
	    S1 = unresolved(M, F, -1, Line, S),
	    expr_list(ArgsList, S1);	    
	ArgsList2 when false == IsSimpleCall ->
	    S1 = unresolved(M, F, length(ArgsList2), Line, S),
	    expr_list(ArgsList, S1);
	ArgsList2 when true == IsSimpleCall ->
	    S1 = expr_list(Extra, S),
	    external_call(M, F, ArgsList2, Line, false, S1)
    end.

mod_fun({atom,_,M1}, {atom,_,F1}) -> {true, M1, F1};
mod_fun({atom,_,M1}, _) -> {false, M1, ?VAR_EXPR};
mod_fun(_, {atom,_,F1}) -> {false, ?MOD_EXPR, F1};
mod_fun(_, _) -> {false, ?MOD_EXPR, ?VAR_EXPR}.

check_funarg(W, ArgsList, Line, S) ->
    {FunArg, Args} = fun_args(W, ArgsList),
    S1 = case funarg(FunArg, S) of
	     true ->
		 S;
	     false when integer(W) -> % 1 or 2
		 unresolved(?MOD_EXPR, ?VAR_EXPR, 0, Line, S);
	     false -> % apply2
		 N = case term2list(Args, [], S) of
			 undefined -> -1;
			 As -> length(As)
		     end,
		 unresolved(?MOD_EXPR, ?VAR_EXPR, N, Line, S)
	 end,
    expr_list(ArgsList, S1).

funarg({'fun', _, _Clauses, _Extra}, _S) -> true;
funarg({var, _, Var}, S) -> member(Var, S#xrefr.funvars);
funarg(_, _S) -> false.

fun_args(apply2, [FunArg, Args]) -> {FunArg, Args};
fun_args(1, [FunArg | Args]) -> {FunArg, Args};
fun_args(2, [_Node, FunArg | Args]) -> {FunArg, Args}.

list2term([A | As]) -> 
    {cons, 0, A, list2term(As)};
list2term([]) -> 
    {nil, 0}.

term2list({cons, _Line, H, T}, L, S) ->
    term2list(T, [H | L], S);
term2list({nil, _Line}, L, _S) -> 
    reverse(L);
term2list({var, _, Var}, L, S) ->
    case keysearch(Var, 1, S#xrefr.matches) of
	{value, {Var, E}} ->
	    term2list(E, L, S);
	false ->
	    undefined
    end;
term2list(_Else, _L, _S) ->
    undefined.

unresolved(M, F, A, Line, S) ->
    handle_call(external, {M,F,A}, Line, S, true).

handle_call(Locality, Module, Name, Arity, Line, S) ->
    case xref_utils:is_builtin(Module, Name, Arity) of
	true when S#xrefr.builtins_too == false -> S;
	_Else ->
	    To = {Module, Name, Arity},
	    handle_call(Locality, To, Line, S, false)
    end.

handle_call(_Locality, {_, 'MNEMOSYNE RULE',1}, _Line, S, _) -> S;
handle_call(_Locality, {_, 'MNEMOSYNE QUERY', 2}, _Line, S, _) -> S;
handle_call(_Locality, {_, 'MNEMOSYNE RECFUNDEF',1}, _Line, S, _) -> S;
handle_call(Locality, To, Line, S, IsUnres) ->
    From = S#xrefr.function,
    Call = {From, To},
    CallAt = {Call, Line},
    S1 = if
	     IsUnres == true ->
		 S#xrefr{unresolved = [CallAt | S#xrefr.unresolved]};
	     true ->
		 S
	 end,
    case Locality of 
	local -> 
	    S1#xrefr{el = [Call | S1#xrefr.el],
		     l_call_at = [CallAt | S1#xrefr.l_call_at]};
	external -> 
	    S1#xrefr{ex = [Call | S1#xrefr.ex],
		     x_call_at = [CallAt | S1#xrefr.x_call_at]}
    end.
