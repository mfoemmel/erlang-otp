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
-module(xref_reader).

-export([module/4]).

-import(lists, [member/2, reverse/1, sort/1]).

-record(xrefr, 
	{module=[],
	 function=[],
	 def_at=[],
	 l_call_at=[],
	 x_call_at=[],
	 el=[],
	 ex=[],
	 x=[],
	 builtins_too=false,
	 funvars=[],          % records variables bound to funs
			      % (for coping with list comprehension)
	 unresolved=[],       % [{line(),mfa()}] apply, spawn etc.
	 %% experimental; -xref(FunEdge) is recognized.
	 lattrs=[],            % local calls, {{mfa(),mfa()},Line}
	 xattrs=[],            % external calls, -"-
	 battrs=[]             % badly formed xref attributes, term().
	 }).

-include("xref.hrl").

%% sys_pre_expand has modified the forms slightly compared to what
%% erl_id_trans recognizes.

%% -> {ok, Module, {DefAt, CallAt, LC, XC, X, Attrs}, Unresolved}} | EXIT
%% Attrs = {ALC, AXC, Bad}
%% ALC, AXC and Bad are extracted from the attribute 'xref'. An experiment.
module(Module, Forms, CollectBuiltins, X) ->
    S = #xrefr{module = Module, builtins_too = CollectBuiltins, x = X},
    forms(Forms, S).

forms([F | Fs], S) ->
    S1 = form(F, S),
    forms(Fs, S1);
forms([], S) -> 
    #xrefr{module = M, def_at = DefAt, 
	   l_call_at = LCallAt, x_call_at = XCallAt,
	   el = LC, ex = XC, x = X, 
	   lattrs = AL, xattrs = AX, battrs = B, unresolved = U} = S,
    Unresolved = sort(U),
    Attrs = {AL, AX, B},
    {ok, M, {DefAt, LCallAt, XCallAt, LC, XC, X, Attrs}, Unresolved}.

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
    FunVars = S#xrefr.funvars,
    clauses(Cls, FunVars, S).

clauses([{clause, _Line, _H, G, B} | Cs], FunVars, S) ->
    S1 = case S#xrefr.builtins_too of
	     true -> guard(G, S);
	     false -> S
	 end,
    S2 = exprs(B, S1),
    S3 = S2#xrefr{funvars = FunVars},
    clauses(Cs, S3);
clauses([], _FunVars, S) -> 
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

exprs([E | Es], S) ->
    S1 = expr(E, S),
    exprs(Es, S1);
exprs([], S) -> 
    S.

expr({var, _Line, _V}, S) -> S;
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
    exprs(Es, S);
expr({'if', _Line, Cs}, S) ->
    clauses(Cs, S);
expr({'case', _Line, E, Cs}, S) ->
    S1 = expr(E, S),
    clauses(Cs, S1);
expr({'receive', _Line, Cs}, S) ->
    clauses(Cs, S);
expr({'receive', _Line, Cs, To, ToEs}, S) ->
    S1 = expr(To, S),
    S2 = exprs(ToEs, S1),
    clauses(Cs, S2);
expr({'fun', _Line, {clauses, Cs} ,_Extra}, S) ->
    clauses(Cs, S);
expr({call, Line, {atom, _, Name}, As}, S) ->
    S1 = handle_call(local, S#xrefr.module, Name, length(As), Line, S),
    expr_list(As, S1);
expr({call, Line, {remote, _Line, {atom,_,Mod}, {atom,_,Name}}, As}, S) ->
    external_call(Mod, Name, As, Line, S);
expr({call, Line, F, As}, S) ->
    external_call(erlang, apply, [F, consify(As)], Line, S);
expr({'catch', _Line, E}, S) ->
    expr(E, S);
expr({match, _Line, {var,_,Var}, {'fun', _, {clauses, Cs}, _Extra}}, S) ->
    %% This is what is needed to avoid warnings for the functions that
    %% are passed around by the "expansion" of list comprehension.
    S1 = S#xrefr{funvars = [Var | S#xrefr.funvars]},
    clauses(Cs, S1);
expr({match, _Line, _P, E}, S) ->
    expr(E, S);
expr({op, _Line, _Op, A}, S) ->
    expr(A, S);
expr({op, _Line, _Op, L, R}, S) ->
    S1 = expr(L, S),
    expr(R, S1);
expr({bin, _Line, Fs}, S) ->
    exprs(Fs, S);
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

%% Never run.
lc_quals([{generate, _Line, _P, E} | Qs], S) ->
    S1 = expr(E, S),
    lc_quals(Qs, S1);
lc_quals([E | Qs], S) ->
    S1 = expr(E, S),
    lc_quals(Qs, S1);
lc_quals([], S) ->
    S.

external_call(Mod, Fun, As, Line, S) ->
    Arity = length(As),
    MFA = {Mod, Fun, Arity},
    W = case xref_utils:is_funfun(Mod, Fun, Arity) of
	    true when {erlang, apply, 2} == MFA -> apply2;
	    true when {erlang, spawn_opt, 4} == MFA -> spawn_opt;
	    true when {erts_debug, apply, 4} == MFA -> debug4;
	    true -> Arity;
	    false when Mod == erlang ->
		case erl_internal:type_test(Fun, Arity) of
		    true -> type;
		    false -> false
		end;
	    false -> false
	end,
    case {W, As} of
	{apply2, [{tuple, _, [M,F]}, Args]} ->
	    app_call(M, F, Args, MFA, Line, S, W, As);
	{1, [{tuple, _, [M,F]}]} ->	
	    app_call(M, F, consify([]), MFA, Line, S, W, As);
	{2, [Node, {tuple, _, [M,F]}]} ->
	    S1 = expr(Node, S),
	    app_call(M, F, consify([]), MFA, Line, S1, W, As);
	{3, [M, F, Args]} ->
	    app_call(M, F, Args, MFA, Line, S, W, As);
	{4, [Node, M, F, Args]} ->
	    S1 = expr(Node, S),
	    app_call(M, F, Args, MFA, Line, S1, W, As);
	{spawn_opt, [M, F, Args, Opts]} ->
	    S1 = expr(Opts, S),
	    app_call(M, F, Args, MFA, Line, S1, W, As);
	{false, _} ->
	    S1 = handle_call(external, Mod, Fun, length(As), Line, S),
	    expr_list(As, S1);
	{type, _} ->
	    expr_list(As, S);
	{debug4, [M, F, Args, _]} ->
	    app_call(M, F, Args, MFA, Line, S, W, As);
	_Else ->
	    check_funarg(W, As, MFA, Line, S)
    end.
	    
app_call({atom,_,M}, {atom,_,F}, Args, MFA, Line, S, _W, _As) ->
    eval_args(M, F, Args, MFA, Line, S);    
app_call({atom,_,M}, {var,_,_F}, Args, MFA, Line, S, _W, _As) ->
    eval_args(M, ?VAR_EXPR, Args, MFA, Line, S);    
app_call(_M, _F, _Args, MFA, Line, S, W, As) ->
    check_funarg(W, As, MFA, Line, S).

eval_args(M, F, Args, MFA, Line, S) ->
    case eval_cons(Args, []) of
	undefined ->
	    S1 = unresolved(MFA, Line, S),
	    expr(Args, S1);
	As -> 
	    external_call(M, F, As, Line, S)
    end.

check_funarg(W, As, MFA, Line, S) ->
    S1 = case funarg(W, As, S) of
	     true -> S;
	     false -> unresolved(MFA, Line, S)
	 end,
    expr_list(As, S1).

unresolved(MFA, Line, S) ->
    %% io:format("~n~p:~p ~p", [S#xrefr.module, Line, MFA]),
    S1 = S#xrefr{unresolved=[{Line,MFA} | S#xrefr.unresolved]},
    handle_call(external, MFA, Line, S1).

funarg(Sp, As, S) ->
    case funarg(Sp, As) of
	{'fun', _, _Clauses, _Extra} ->
	    true;
	{var, _, Var} ->
	    member(Var, S#xrefr.funvars);
	_Else ->
	    false
    end.

funarg(apply2, [FunArg, _Args]) -> FunArg;
funarg(1, [FunArg]) -> FunArg;
funarg(2, [_Node, FunArg]) -> FunArg;
funarg(_, _) -> false.

consify([A | As]) -> 
    {cons, 0, A, consify(As)};
consify([]) -> 
    {nil, 0}.

eval_cons({cons, _Line, H, T}, L) ->
    eval_cons(T, [H | L]);
eval_cons({nil, _Line}, L) -> 
    reverse(L);
eval_cons(_Else, _L) ->
    undefined.

handle_call(Locality, Module, Name, Arity, Line, S) ->
    case erlang:is_builtin(Module, Name, Arity) of
	true when S#xrefr.builtins_too == false -> S;
	_Else ->
	    To = {Module, Name, Arity},
	    handle_call(Locality, To, Line, S)
    end.

handle_call(_Locality, {_, 'MNEMOSYNE RULE',1}, _Line, S) -> S;
handle_call(_Locality, {_, 'MNEMOSYNE QUERY', 2}, _Line, S) -> S;
handle_call(_Locality, {_, 'MNEMOSYNE RECFUNDEF',1}, _Line, S) -> S;
handle_call(Locality, To, Line, S) ->
    From = S#xrefr.function,
    Call = {From, To},
    CallAt = {Call, Line},
    case Locality of 
	local -> 
	    S#xrefr{el = [Call | S#xrefr.el],
		    l_call_at = [CallAt | S#xrefr.l_call_at]};
	external -> 
	    S#xrefr{ex = [Call | S#xrefr.ex],
		    x_call_at = [CallAt | S#xrefr.x_call_at]}
    end.
