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
%% Purpose : Constant folding optimisation for Core

%% Propagate atomic values and fold in values of safe calls to
%% constant arguments.  Also detect and remove literals which are
%% ignored in a 'seq'.  Could handle lets better by chasing down
%% complex 'arg' expressions and finding values.
%%
%% Try to optimise case expressions by removing unmatchable or
%% unreachable clauses.  Also change explicit tuple arg into multiple
%% values and extend clause patterns.  We must be careful here not to
%% generate cases which we know to be safe but later stages will not
%% recognise as such, e.g. the following is NOT acceptable:
%%
%%    case 'b' of
%%        <'b'> -> ...
%%    end
%%
%% Variable folding is complicated by variable shadowing, for example
%% in:
%%    'foo'/1 =
%%        fun (X) ->
%%            let <A> = X
%%            in  let <X> = Y
%%                in ... <use A>
%% If we were to simply substitute X for A then we would be using the
%% wrong X.  Our solution is to rename variables that are the values
%% of substitutions.  We could rename all shadowing variables but do
%% the minimum.  We would then get:
%%    'foo'/1 =
%%        fun (X) ->
%%            let <A> = X
%%            in  let <X1> = Y
%%                in ... <use A>
%% which is optimised to:
%%    'foo'/1 =
%%        fun (X) ->
%%            let <X1> = Y
%%            in ... <use X>
%%
%% This is done by carefully shadowing variables and substituting
%% values.  See details when defining functions.
%%
%% It would be possible to extend to replace repeated evaluation of
%% "simple" expressions by the value (variable) of the first call.
%% For example, after a "let Z = X+1" then X+1 would be replaced by Z
%% where X is valid.  The Sub uses the full Core expression as key.
%% It would complicate handling of patterns as we would have to remove
%% all values where the key contains pattern variables.

-module(sys_core_fold).

-export([module/2,function/1]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,all/2,any/2,reverse/1]).
-include("core_parse.hrl").

%% Variable value info.
-record(sub, {v=[],cexpr=none,t=[]}).		%Variable substitutions

module(#c_module{defs=Ds0}=Mod, _Opts) ->
    %% Use the compiler process dictionary to propagate this option.
    Ds1 = map(fun function/1, Ds0),
    {ok,Mod#c_module{defs=Ds1}}.

function(#c_def{val=B0}=Def) ->
    %%ok = io:fwrite("~w:~p~n", [?LINE,{Def#c_def.func}]),
    B1 = expr(B0, sub_new()),			%This must be a fun!
    Def#c_def{val=B1}.

%% body(Expr, Sub) -> Expr.
%%  No special handling of anything except valuess.

body(#c_values{anno=A,es=Es0}, Sub) ->
    Es1 = expr_list(Es0, Sub),
    #c_values{anno=A,es=Es1};
body(E, Sub) -> expr(E, Sub).

%% guard(Expr, Sub) -> Expr.
%%  Do guard expression.  These are boolean expressions with values
%%  which are tests.  These may be wrapped in a protected.  Seeing
%%  guards are side-effect free we can optimise the boolean
%%  expressions.

guard(#c_call{module=#c_atom{val=erlang},
	      name=#c_atom{val='not'},
	      args=[A]}=Call, Sub) ->
    case guard(A, Sub) of
	#c_atom{val=true} -> #c_atom{val=false};
	#c_atom{val=false} -> #c_atom{val=true};
	Arg -> Call#c_call{args=[Arg]}
    end;
guard(#c_call{module=#c_atom{val=erlang},
	      name=#c_atom{val='and'},
	      args=[A1,A2]}=Call, Sub) ->
    case {guard(A1, Sub),guard(A2, Sub)} of
	{#c_atom{val=true},Arg2} -> Arg2;
	{#c_atom{val=false},_} -> #c_atom{val=false};
	{Arg1,#c_atom{val=true}} -> Arg1;
	{_,#c_atom{val=false}} -> #c_atom{val=false};
	{Arg1,Arg2} -> Call#c_call{args=[Arg1,Arg2]}
    end;
guard(#c_call{module=#c_atom{val=erlang},
	      name=#c_atom{val='or'},
	      args=[A1,A2]}=Call, Sub) ->
    case {guard(A1, Sub),guard(A2, Sub)} of
	{#c_atom{val=true},_} -> #c_atom{val=true};
	{#c_atom{val=false},Arg2} -> Arg2;
	{_,#c_atom{val=true}} -> #c_atom{val=true};
	{Arg1,#c_atom{val=false}} -> Arg1;
	{Arg1,Arg2} -> Call#c_call{args=[Arg1,Arg2]}
    end;
guard(#c_seq{arg=Arg0,body=B0}=Seq, Sub) ->
    case {guard(Arg0, Sub),guard(B0, Sub)} of
	{#c_atom{val=true},B1} -> B1;
	{#c_atom{val=false}=False,_} -> False;
	{Arg,#c_atom{val=true}} -> Arg;
	{_,#c_atom{val=false}=False} -> False;
	{Arg,B1} -> Seq#c_seq{arg=Arg,body=B1}
    end;
guard(#c_try{arg=E0,vars=[#c_var{name=X}],body=#c_var{name=X},
	     handler=#c_atom{val=false}}=Prot, Sub) ->
    %% We can remove protected if value a simple.
    E1 = body(E0, Sub),
    case core_lib:is_simple(E1) of
	true -> E1;
	false -> Prot#c_try{arg=E1}
    end;
guard(E, Sub) -> expr(E, Sub).

%% expr(Expr, Sub) -> Expr.

expr(#c_var{}=V, Sub) ->
    sub_get_var(V, Sub);
expr(#c_char{}=C, _) -> C;
expr(#c_int{}=I, _) -> I;
expr(#c_float{}=F, _) -> F;
expr(#c_atom{}=A, _) -> A;
expr(#c_string{}=S, _) -> S;
expr(#c_nil{}=N, _) -> N;
expr(#c_cons{hd=H0,tl=T0}=Cons, Sub) ->
    H1 = expr(H0, Sub),
    T1 = expr(T0, Sub),
    eval_cons(Cons#c_cons{hd=H1,tl=T1}, H1, T1);
expr(#c_tuple{anno=A,es=Es}, Sub) ->
    #c_tuple{anno=A,es=expr_list(Es, Sub)};
expr(#c_binary{segs=Ss}=Bin, Sub) ->
    Bin#c_binary{segs=bin_seg_list(Ss, Sub)};
expr(#c_fname{}=Fname, _) -> Fname;
expr(#c_fun{vars=Vs0,body=B0}=Fun, Sub0) ->
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    B1 = body(B0, Sub1),
    Fun#c_fun{vars=Vs1,body=B1};
expr(#c_seq{arg=Arg0,body=B0}=Seq, Sub) ->
    B1 = body(B0, Sub),
    %% Optimise away pure literal arg as its value is ignored.
    case body(Arg0, Sub) of
	#c_values{es=Es}=Arg1 ->
	    case is_safe_simple_list(Es) of
		true -> B1;
		false -> Seq#c_seq{arg=Arg1,body=B1}
	    end;
	Arg1 ->
	    case is_safe_simple(Arg1) of
		true -> B1;
		false -> Seq#c_seq{arg=Arg1,body=B1}
	    end
    end;
expr(#c_let{vars=Vs0,arg=Arg0,body=B0}=Let, Sub0) ->
    Arg1 = body(Arg0, Sub0),			%This is a body
    %% Optimise let and add new substitutions.
    {Vs1,Args,Sub1} = let_substs(Vs0, Arg1, Sub0),
    B1 = body(B0, Sub1),
    %% Optimise away let if the body consists of a single variable or
    %% if no values remain to be set.
    case {Vs1,Args,B1} of
	{[#c_var{name=Vname}],Args,#c_var{name=Vname}} ->
	    core_lib:make_values(Args);
	{[],[],Body} ->
	    Body;
	_Other ->
	    opt_case_in_let(Let#c_let{vars=Vs1,
				      arg=core_lib:make_values(Args),
				      body=B1})
    end;
expr(#c_letrec{defs=Fs0,body=B0}=Letrec, Sub) ->
    Fs1 = map(fun (#c_def{val=Fb}=Fd) ->
		      Fd#c_def{val=expr(Fb, Sub)}
	      end, Fs0),
    B1 = body(B0, Sub),
    Letrec#c_letrec{defs=Fs1,body=B1};
expr(#c_case{arg=Arg0,clauses=Cs0}=Case, Sub) ->
    Arg1 = body(Arg0, Sub),
    {Arg2,Cs1} = case_opt(Arg1, Cs0),
    Cs2 = clauses(Arg2, Cs1, Sub),
    eval_case(Case#c_case{arg=Arg2,clauses=Cs2}, Sub);
expr(#c_receive{clauses=Cs0,timeout=T0,action=A0}=Recv, Sub) ->
    Cs1 = clauses(#c_var{name='_'}, Cs0, Sub),	%This is all we know
    T1 = expr(T0, Sub),
    A1 = body(A0, Sub),
    Recv#c_receive{clauses=Cs1,timeout=T1,action=A1};
expr(#c_apply{op=Op0,args=As0}=App, Sub) ->
    Op1 = expr(Op0, Sub),
    As1 = expr_list(As0, Sub),
    App#c_apply{op=Op1,args=As1};
expr(#c_call{module=M0,name=N0}=Call, Sub) ->
    M1 = expr(M0, Sub),
    N1 = expr(N0, Sub),
    call(Call#c_call{module=M1,name=N1}, M1, N1, Sub);
expr(#c_primop{args=As0}=Prim, Sub) ->
    As1 = expr_list(As0, Sub),
    Prim#c_primop{args=As1};
expr(#c_catch{body=B0}=Catch, Sub) ->
    %% We can remove catch if the value is simple
    B1 = body(B0, Sub),
    case is_safe_simple(B1) of
	true -> B1;
	false -> Catch#c_catch{body=B1}
    end;
expr(#c_try{anno=A,arg=E0,vars=Vs0,body=B0,evars=Evs0,handler=H0}=Try, Sub0) ->
    %% We can remove try if the value is simple and replace it with a let.
    E1 = body(E0, Sub0),
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    B1 = body(B0, Sub1),
    case is_safe_simple(E1) of
	true ->
	    expr(#c_let{anno=A,vars=Vs1,arg=E1,body=B1}, Sub0);
	false ->
	    {Evs1,Sub2} = pattern_list(Evs0, Sub0),
	    H1 = body(H0, Sub2),
	    Try#c_try{arg=E1,vars=Vs1,body=B1,evars=Evs1,handler=H1}
    end.

expr_list(Es, Sub) ->
    map(fun (E) -> expr(E, Sub) end, Es).

bin_seg_list(Es, Sub) ->
    map(fun (E) -> bin_segment(E, Sub) end, Es).

bin_segment(#c_bin_seg{val=Val,size=Size}=BinSeg, Sub) ->
    BinSeg#c_bin_seg{val=expr(Val, Sub),size=expr(Size, Sub)}.

%% is_safe_simple(Expr) -> true | false.
%%  A safe simple cannot fail with badarg.  Binaries are difficult to
%%  check so be very conservative here.

is_safe_simple(#c_var{}) -> true;		%Not atomic
is_safe_simple(#c_cons{hd=H,tl=T}) ->
    case is_safe_simple(H) of
	true -> is_safe_simple(T); 
	false -> false
    end;
is_safe_simple(#c_tuple{es=Es}) -> is_safe_simple_list(Es);
is_safe_simple(#c_binary{}) -> false;
% is_safe_simple(#c_binary{segs=Ss}) ->
%     is_safe_lit_bin(Ss);
is_safe_simple(E) -> core_lib:is_atomic(E).

is_safe_simple_list(Es) -> all(fun is_safe_simple/1, Es).

% is_safe_lit_bin(Ss) ->
%     {Bits,Safe} = foldl(fun (#c_bin_seg{val=#c_int{},size=#c_int{val=S},
% 					unit=U,type=integer}, {Bits,Safe}) ->
% 				Sb = S*U,
% 				{Bits + Sb,Safe};
% 			    (#c_bin_seg{val=#c_float{},size=#c_int{val=S},
% 					unit=U,type=float}, {Bits,Safe}) ->
% 				Sb = S*U,
% 				{Bits + Sb, Safe and (Sb == 64)};
% 			    (_Seg, {Bits,_Safe}) -> {Bits,false}
% 			end, {0,true}, Ss),
%     Safe and ((Bits rem 8) == 0).

%% eval_cons(Cons, Head, Tail) -> Expr.
%%  Evaluate constant part of a cons expression.

eval_cons(_Cons, #c_char{val=C}, #c_string{val=S}) ->
    #c_string{val=[C|S]};
eval_cons(_Cons, #c_char{val=C}, #c_nil{}) -> #c_string{val=[C]};
eval_cons(_Cons, #c_int{val=I}, #c_string{val=S}) when I >=0, I < 256 ->
    #c_string{val=[I|S]};
eval_cons(_Cons, #c_int{val=I}, #c_nil{}) when I >=0, I < 256 ->
    #c_string{val=[I]};
eval_cons(Cons, H, T) ->
    Cons#c_cons{hd=H,tl=T}.			%Rebuild cons arguments

%% Handling remote calls. The module/name fields have been processed.

call(#c_call{args=As}=Call, #c_atom{val=M}=M0, #c_atom{val=N}=N0, Sub) ->
    case get(no_inline_list_funcs) of
  	true ->
 	    call_0(Call, M0, N0, As, Sub);
  	false ->
  	    call_1(Call, M, N, As, Sub)
      end;
call(#c_call{args=As}=Call, M, N, Sub) ->
    call_0(Call, M, N, As, Sub).

call_0(Call, M, N, As0, Sub) ->
    As1 = expr_list(As0, Sub),
    fold_call(Call#c_call{args=As1}, M, N, As1).

%% We inline some very common higher order list operations.
%% We use the same evaluation order as the library function.

call_1(_Call, lists, all, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^all', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_atom{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_atom{val=true}], guard=#c_atom{val=true},
		    body=#c_apply{op=Loop, args=[Xs]}},
    CC2 = #c_clause{pats=[#c_atom{val=false}], guard=#c_atom{val=true},
		    body=#c_atom{val=false}},
    CC3 = #c_clause{pats=[X], guard=#c_atom{val=true},
		    body=#c_primop{name=#c_atom{val='match_fail'},
				   args=[Err1]}},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_case{arg=#c_apply{op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
		   body=#c_atom{val=true}},
    Err2 = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, any, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^any', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_atom{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_atom{val=true}], guard=#c_atom{val=true},
		    body=#c_atom{val=true}},
    CC2 = #c_clause{pats=[#c_atom{val=false}], guard=#c_atom{val=true},
		    body=#c_apply{op=Loop, args=[Xs]}},
    CC3 = #c_clause{pats=[X], guard=#c_atom{val=true},
		    body=#c_primop{name=#c_atom{val='match_fail'},
				   args=[Err1]}},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_case{arg=#c_apply{op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
		   body=#c_atom{val=false}},
    Err2 = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, foreach, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^foreach', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_seq{arg=#c_apply{op=F, args=[X]},
			       body=#c_apply{op=Loop, args=[Xs]}}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
		   body=#c_atom{val=ok}},
    Err = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, map, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^map', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_let{vars=[H], arg=#c_apply{op=F, args=[X]},
			       body=#c_cons{hd=H,
					    tl=#c_apply{op=Loop,
							args=[Xs]}}}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
		   body=#c_nil{}},
    Err = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, flatmap, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^flatmap', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_let{vars=[H],
			       arg=#c_apply{op=F, args=[X]},
			       body=#c_call{module=#c_atom{val=erlang},
					    name=#c_atom{val='++'},
					    args=[H,
						  #c_apply{op=Loop,
							   args=[Xs]}]}}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
		   body=#c_nil{}},
    Err = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, filter, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^filter', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    B = #c_var{name='B'},
    Err1 = #c_tuple{es=[#c_atom{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_atom{val=true}], guard=#c_atom{val=true},
		    body=#c_cons{hd=X, tl=Xs}},
    CC2 = #c_clause{pats=[#c_atom{val=false}], guard=#c_atom{val=true},
		    body=Xs},
    CC3 = #c_clause{pats=[X], guard=#c_atom{val=true},
		    body=#c_primop{name=#c_atom{val='match_fail'},
				   args=[Err1]}},
    Case = #c_case{arg=B, clauses = [CC1, CC2, CC3]},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_let{vars=[B],
			       arg=#c_apply{op=F, args=[X]},
			       body=#c_let{vars=[Xs],
					   arg=#c_apply{op=Loop,
							args=[Xs]},
					   body=Case}}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
		   body=#c_nil{}},
    Err2 = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, foldl, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_fname{id='lists^foldl', arity=2},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_apply{op=Loop,
				 args=[Xs, #c_apply{op=F, args=[X, A]}]}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true}, body=A},
    Err = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L, A]}}},
	 Sub);
call_1(_Call, lists, foldr, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_fname{id='lists^foldr', arity=2},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=#c_apply{op=F, args=[X, #c_apply{op=Loop,
							 args=[Xs, A]}]}},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true}, body=A},
    Err = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
			       body=#c_apply{op=Loop, args=[L, A]}}},
	 Sub);
call_1(_Call, lists, mapfoldl, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_fname{id='lists^mapfoldl', arity=2},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{pats=[P], guard=#c_atom{val=true}, body=E},
		Err = #c_tuple{es=[#c_atom{val='badmatch'}, X]},
		C2 = #c_clause{pats=[X], guard=#c_atom{val=true},
			       body=#c_primop{name=#c_atom{val='match_fail'},
					      args=[Err]}},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
		   body=Match(#c_apply{op=F, args=[X, A]},
			      #c_tuple{es=[X, A]},
%%% Tuple passing version
			      Match(#c_apply{op=Loop, args=[Xs, A]},
				    #c_tuple{es=[Xs, A]},
				    #c_tuple{es=[#c_cons{hd=X, tl=Xs}, A]})
%%% Multiple-value version
%%% 			      #c_let{vars=[Xs,A],
%%% 				     %% The tuple here will be optimised
%%% 				     %% away later; no worries.
%%% 				     arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 				     body=#c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 							A]}}
			     )},
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
%%% Tuple passing version
		   body=#c_tuple{es=[#c_nil{}, A]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_nil{}, A]}},
    Err = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
%%% Tuple passing version
			       body=#c_apply{op=Loop, args=[L, A]}}},
%%% Multiple-value version
%%% 			       body=#c_let{vars=[Xs, A],
%%% 					   arg=#c_apply{op=Loop,
%%% 							args=[L, A]},
%%% 					   body=#c_tuple{es=[Xs, A]}}}},
	 Sub);
call_1(_Call, lists, mapfoldr, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_fname{id='lists^mapfoldr', arity=2},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{pats=[P], guard=#c_atom{val=true}, body=E},
		Err = #c_tuple{es=[#c_atom{val='badmatch'}, X]},
		C2 = #c_clause{pats=[X], guard=#c_atom{val=true},
			       body=#c_primop{name=#c_atom{val='match_fail'},
					      args=[Err]}},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_atom{val=true},
%%% Tuple passing version
		   body=Match(#c_apply{op=Loop, args=[Xs, A]},
			      #c_tuple{es=[Xs, A]},
			      Match(#c_apply{op=F, args=[X, A]},
				    #c_tuple{es=[X, A]},
				    #c_tuple{es=[#c_cons{hd=X, tl=Xs}, A]}))
%%% Multiple-value version
%%% 		   body=#c_let{vars=[Xs,A],
%%% 			       %% The tuple will be optimised away
%%% 			       arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 			       body=Match(#c_apply{op=F, args=[X, A]},
%%% 					  #c_tuple{es=[X, A]},
%%% 					  #c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 						        A]})}
		  },
    C2 = #c_clause{pats=[#c_nil{}], guard=#c_atom{val=true},
%%% Tuple passing version
		   body=#c_tuple{es=[#c_nil{}, A]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_nil{}, A]}},
    Err = #c_tuple{es=[#c_atom{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_atom{val=true},
		   body=#c_primop{name=#c_atom{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[#c_def{name=Loop, val=Fun}],
%%% Tuple passing version
 			       body=#c_apply{op=Loop, args=[L, A]}}},
%%% Multiple-value version
%%% 			       body=#c_let{vars=[Xs, A],
%%% 					   arg=#c_apply{op=Loop,
%%% 							args=[L, A]},
%%% 					   body=#c_tuple{es=[Xs, A]}}}},
	 Sub);
call_1(#c_call{module=M, name=N}=Call, _, _, As, Sub) ->
    call_0(Call, M, N, As, Sub).

%% fold_call(Call, Mod, Name, Args) -> Expr.
%%  Try to safely evaluate the call.  Just try to evaluate arguments,
%%  do the call and convert return values to literals.  If this
%%  succeeds then use the new value, otherwise just fail and use
%%  original call.  Do this at every level.
%%
%%  We evaluate length/1 if the shape of the list is known.
%%
%%  We evaluate element/2 and setelement/3 if the position is constant and
%%  the shape of the tuple is known.
%%
%%  We evalute '++' if the first operand is as literal (or partly literal).

fold_call(Call, #c_atom{val=M}, #c_atom{val=F}, Args) ->
    fold_call_1(Call, M, F, Args);
fold_call(Call, _M, _N, _Args) -> Call.

fold_call_1(Call, erlang, length, [Arg]) ->
    eval_length(Call, Arg);
fold_call_1(Call, erlang, '++', [Arg1,Arg2]) ->
    eval_append(Call, Arg1, Arg2);
fold_call_1(Call, erlang, element, [Arg1,Arg2]) ->
    Ref = make_ref(),
    case catch {Ref,eval_element(Arg1, Arg2)} of
	{Ref,Val} -> Val;
	_Other -> Call
    end;
fold_call_1(Call, erlang, setelement, [Arg1,Arg2,Arg3]) ->
    Ref = make_ref(),
    case catch {Ref,eval_setelement(Arg1, Arg2, Arg3)} of
	{Ref,Val} -> Val;
	_Other -> Call
    end;
fold_call_1(Call, erlang, N, [Arg]) ->
    case catch begin
		   LitA = core_lib:literal_value(Arg),
		   {ok,core_lib:make_literal(eval_call(N, LitA))}
	       end of
	{ok,Val} -> Val;
	_Other -> Call
    end;
fold_call_1(Call, erlang, N, [Arg1,Arg2]) ->
    case catch begin
		   LitA1 = core_lib:literal_value(Arg1),
		   LitA2 = core_lib:literal_value(Arg2),
		   {ok,core_lib:make_literal(eval_call(N, LitA1, LitA2))}
	       end of
	{ok,Val} -> Val;
	_Other -> Call
    end;
fold_call_1(Call, _Mod, _Name, _Args) -> Call.

%% eval_call(Op, Arg) -> Value.
%% eval_call(Op, Arg1, Arg2) -> Value.
%%  Evaluate safe calls.  We only do arithmetic and logical operators,
%%  there are more but these are the ones that are probably
%%  worthwhile.  It would be MUCH easier if we could apply these!

eval_call('+', X) -> 0 + X;
eval_call('-', X) -> 0 - X;
eval_call('bnot', X) -> bnot X;
eval_call(abs, A) -> abs(A);
eval_call(float, A) -> float(A);
eval_call(round, A) -> round(A);
eval_call(trunc, A) -> trunc(A);
eval_call('not', X) -> not X;
eval_call(hd, L) -> hd(L);
eval_call(tl, L) -> tl(L);
eval_call(length, L) -> length(L);
eval_call(size, T) -> size(T);
eval_call(integer_to_list, I) -> integer_to_list(I);
eval_call(list_to_integer, L) -> list_to_integer(L);
eval_call(float_to_list, F) -> float_to_list(F);
eval_call(list_to_float, L) -> list_to_float(L);
eval_call(atom_to_list, A) -> atom_to_list(A);
eval_call(list_to_atom, L) -> list_to_atom(L);
eval_call(tuple_to_list, T) -> tuple_to_list(T);
eval_call(list_to_tuple, L) -> list_to_tuple(L);
eval_call(is_atom, I) -> erlang:is_atom(I);
eval_call(is_constant, I) -> erlang:is_constant(I);
eval_call(is_float, I) -> erlang:is_float(I);
eval_call(is_integer, I) -> erlang:is_integer(I);
eval_call(is_list, I) -> erlang:is_list(I);
eval_call(is_number, I) -> erlang:is_number(I);
eval_call(is_tuple, I) -> erlang:is_tuple(I).

eval_call('*', X, Y) -> X * Y;
eval_call('/', X, Y) -> X / Y;
eval_call('+', X, Y) -> X + Y;
eval_call('-', X, Y) -> X - Y;
eval_call('div', X, Y) -> X div Y;
eval_call('rem', X, Y) -> X rem Y;
eval_call('band', X, Y) -> X band Y;
eval_call('bor', X, Y) -> X bor Y;
eval_call('bxor', X, Y) -> X bxor Y;
eval_call('bsl', X, Y) -> X bsl Y;
eval_call('bsr', X, Y) -> X bsr Y;
eval_call('and', X, Y) -> X and Y;
eval_call('or',  X, Y) -> X or Y;
eval_call('xor', X, Y) -> X xor Y;
eval_call('=:=',  X, Y) -> X =:= Y;
eval_call('=/=',  X, Y) -> X =/= Y;
eval_call('==',  X, Y) -> X == Y;
eval_call('/=',  X, Y) -> X /= Y;
eval_call('=<',  X, Y) -> X =< Y;
eval_call('<',   X, Y) -> X < Y;
eval_call('>=',  X, Y) -> X >= Y;
eval_call('>',   X, Y) -> X > Y;
eval_call('++', X, Y) -> X ++ Y;
eval_call('--', X, Y) -> X -- Y;
eval_call(element, X, Y) -> element(X, Y).

%% eval_length(Call, List) -> Val.
%%  Evaluates the length for the prefix of List which has a known
%%  shape.

eval_length(Call, Core) -> eval_length(Call, Core, 0).
    
eval_length(Call, #c_nil{}, Len) -> #c_int{anno=Call#c_call.anno,val=Len};
eval_length(Call, #c_cons{tl=T}, Len) ->
    eval_length(Call, T, Len+1);
eval_length(Call, _List, 0) -> Call;		%Could do nothing
eval_length(Call, List, Len) ->
    #c_call{anno=Call#c_call.anno,
	    module=#c_atom{val=erlang},
	    name=#c_atom{val='+'},
	    args=[#c_int{val=Len},Call#c_call{args=[List]}]}.

%% eval_append(Call, FirstList, SecondList) -> Val.
%%  Evaluates the constant part of '++' expression.

eval_append(_Call, #c_string{val=Cs1}=S1, #c_string{val=Cs2}) ->
    S1#c_string{val=Cs1 ++ Cs2};
eval_append(Call, #c_string{val=Cs}, List) when length(Cs) =< 4 ->
    Anno = Call#c_call.anno,
    foldr(fun (C, L) ->
		  #c_cons{anno=Anno,hd=#c_int{val=C},tl=L}
	  end, List, Cs);
eval_append(_Call, #c_nil{}, List) -> List;
eval_append(Call, #c_cons{tl=T}=Cons, List) ->
    Cons#c_cons{tl=eval_append(Call, T, List)};
eval_append(Call, X, Y) ->
    Call#c_call{args=[X,Y]}.			%Rebuild call arguments.

%% eval_element(Pos, Tuple) -> Val.
%%  Evaluates element/2 if Pos and Tuple are literals.

eval_element(#c_int{val=Pos}, #c_tuple{es=Es}) ->
    lists:nth(Pos, Es).

%% eval_setelement(Pos, Tuple, NewVal) -> Val.
%%  Evaluates setelement/3 if Pos and Tuple are literals.

eval_setelement(#c_int{val=Pos}, #c_tuple{es=Es}=Tuple, NewVal) ->
    Tuple#c_tuple{es=eval_setelement1(Pos, Es, NewVal)}.

eval_setelement1(1, [_|T], NewVal) ->
    [NewVal|T];
eval_setelement1(Pos, [H|T], NewVal) when Pos > 1 ->
    [H|eval_setelement1(Pos-1, T, NewVal)].

%% clause(Clause, Sub) -> Clause.

clause(#c_clause{pats=Ps0,guard=G0,body=B0}=Cl, #sub{cexpr=Cexpr,t=Types}=Sub0) ->
    {Ps1,Sub1} = pattern_list(Ps0, Sub0),
    Sub2 = Sub1#sub{cexpr=none,t=update_types(Cexpr, Ps1, Types)},
    G1 = guard(G0, Sub2),
    B1 = body(B0, Sub2),
    Cl#c_clause{pats=Ps1,guard=G1,body=B1}.

%% let_substs(LetVars, LetArg, Sub) -> {[Var],[Val],Sub}.
%%  Add suitable substitutions to Sub of variables in LetVars.  First
%%  remove variables in LetVars from Sub, then fix subs.  N.B. must
%%  work out new subs in parallel and then apply then to subs.  Return
%%  the unsubstituted variables and values.

let_substs(Vs0, As0, Sub0) ->
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    {Vs2,As1,Ss} = let_substs_1(Vs1, As0, Sub1),
    {Vs2,As1,
     foldl(fun ({V,S}, Sub) -> sub_set_name(V, S, Sub) end, Sub1, Ss)}.

let_substs_1(Vs, #c_values{es=As}, Sub) ->
    let_subst_list(Vs, As, Sub);
let_substs_1([V], A, Sub) -> let_subst_list([V], [A], Sub);
let_substs_1(Vs, A, _) -> {Vs,A,[]}.

let_subst_list([V|Vs0], [A|As0], Sub) ->
    {Vs1,As1,Ss} = let_subst_list(Vs0, As0, Sub),
    case is_subst(A) of
	true -> {Vs1,As1,sub_subst_var(V, A, Sub) ++ Ss};
	false -> {[V|Vs1],[A|As1],Ss}
    end;
let_subst_list([], [], _) -> {[],[],[]}.

%% pattern(Pattern, InSub) -> {Pattern,OutSub}.
%% pattern(Pattern, InSub, OutSub) -> {Pattern,OutSub}.
%%  Variables occurring in Pattern will shadow so they must be removed
%%  from Sub.  If they occur as a value in Sub then we create a new
%%  variable and then add a substitution for that.
%%
%%  Patterns are complicated by sizes in binaries.  These are pure
%%  input variables which create no bindings.  We, therefore, need to
%%  carry around the original substitutions to get the correct
%%  handling.

%%pattern(Pat, Sub) -> pattern(Pat, Sub, Sub). 

pattern(#c_var{name=V0}=Pat, Isub, Osub) ->
    case sub_is_val(Pat, Isub) of
	true ->
	    %% Nesting saves us from using unique variable names.
	    V1 = list_to_atom("fol" ++ atom_to_list(V0)),
	    Pat1 = #c_var{name=V1},
	    {Pat1,sub_set_var(Pat, Pat1, Osub)};
	false -> {Pat,sub_del_var(Pat, Osub)}
    end;
pattern(#c_char{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_int{}=Pat, Osub, _) -> {Pat,Osub};
pattern(#c_float{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_atom{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_string{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_nil{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_cons{hd=H0,tl=T0}=Pat, Isub, Osub0) ->
    {H1,Osub1} = pattern(H0, Isub, Osub0),
    {T1,Osub2} = pattern(T0, Isub, Osub1),
    {Pat#c_cons{hd=H1,tl=T1},Osub2};
pattern(#c_tuple{es=Es0}=Pat, Isub, Osub0) ->
    {Es1,Osub1} = pattern_list(Es0, Isub, Osub0),
    {Pat#c_tuple{es=Es1},Osub1};
pattern(#c_binary{segs=V0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = bin_pattern_list(V0, Isub, Osub0),
    {Pat#c_binary{segs=V1},Osub1};
pattern(#c_alias{var=V0,pat=P0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = pattern(V0, Isub, Osub0),
    {P1,Osub2} = pattern(P0, Isub, Osub1),
    {Pat#c_alias{var=V1,pat=P1},Osub2}.

bin_pattern_list(Ps0, Isub, Osub0) ->
    mapfoldl(fun (P, Osub) -> bin_pattern(P, Isub, Osub) end, Osub0, Ps0).

bin_pattern(#c_bin_seg{val=E0,size=Size0}=Pat, Isub, Osub0) ->
    Size1 = expr(Size0, Isub),
    {E1,Osub1} = pattern(E0, Isub, Osub0),
    {Pat#c_bin_seg{val=E1,size=Size1},Osub1}.

pattern_list(Ps, Sub) -> pattern_list(Ps, Sub, Sub).

pattern_list(Ps0, Isub, Osub0) ->
    mapfoldl(fun (P, Osub) -> pattern(P, Isub, Osub) end, Osub0, Ps0).

%% is_subst(Expr) -> true | false.
%%  Test whether an expression is a suitable substitution.

is_subst(#c_tuple{es=[]}) -> true;		%The empty tuple
is_subst(#c_fname{}) -> false;			%Fun implementaion needs this
is_subst(#c_var{}) -> true;
is_subst(E) -> core_lib:is_atomic(E).

%% sub_new() -> #sub{}.
%% sub_get_var(Var, #sub{}) -> Value.
%% sub_set_var(Var, Value, #sub{}) -> #sub{}.
%% sub_set_name(Name, Value, #sub{}) -> #sub{}.
%% sub_del_var(Var, #sub{}) -> #sub{}.
%% sub_subst_var(Var, Value, #sub{}) -> [{Name,Value}].
%% sub_is_val(Var, #sub{}) -> bool().
%%  We use the variable name as key so as not have problems with
%%  annotations.  When adding a new substitute we fold substitute
%%  chains so we never have to search more than once.  Use orddict so
%%  we know the format.

sub_new() -> #sub{v=[],t=[]}.

sub_get_var(#c_var{name=V}=Var, #sub{v=S}) ->
    case v_find(V, S) of
	{ok,Val} -> Val;
	error -> Var
    end.

sub_set_var(#c_var{name=V}, Val, Sub) ->
    sub_set_name(V, Val, Sub).

sub_set_name(V, Val, #sub{v=S,t=Tdb}=Sub) ->
    Sub#sub{v=v_store(V, Val, S),t=kill_types(V, Tdb)}.

sub_del_var(#c_var{name=V}, #sub{v=S,t=Tdb}=Sub) ->
    Sub#sub{v=v_erase(V, S),t=kill_types(V, Tdb)}.

sub_subst_var(#c_var{name=V}, Val, #sub{v=S0}) ->
    %% Fold chained substitutions.
    [{V,Val}] ++ [ {K,Val} || {K,#c_var{name=V1}} <- S0, V1 =:= V ].

sub_is_val(#c_var{name=V}, #sub{v=S}) ->
    v_is_value(V, S).

v_find(Key, [{K,_}|_]) when Key < K -> error;
v_find(Key, [{K,Value}|_]) when Key == K -> {ok,Value};
v_find(Key, [{K,_}|D]) when Key > K -> v_find(Key, D);
v_find(_, []) -> error.

v_store(Key, New, [{K,_Old}=Pair|Dict]) when Key < K ->
    [{Key,New},Pair|Dict];
v_store(Key, New, [{K,_Old}|Dict]) when Key == K ->
    [{Key,New}|Dict];
v_store(Key, New, [{K,_Old}=Pair|Dict]) when Key > K ->
    [Pair|v_store(Key, New, Dict)];
v_store(Key, New, []) -> [{Key,New}].

v_erase(Key, [{K,Value}|Dict]) when Key < K -> [{K,Value}|Dict];
v_erase(Key, [{K,_}|Dict]) when Key == K -> Dict;
v_erase(Key, [{K,Value}|Dict]) when Key > K ->
    [{K,Value}|v_erase(Key, Dict)];
v_erase(_, []) -> [].

v_is_value(Var, Sub) ->
    any(fun ({_,#c_var{name=Val}}) when Val == Var -> true;
	    (_) -> false
	end, Sub).

%% clauses(E, [Clause], Sub) -> [Clause].
%%  Trim the clauses by removing all clauses AFTER the first one which
%%  is guaranteed to match.  Also remove all trivially false clauses.

clauses(E, [C0|Cs], Sub) ->
    #c_clause{pats=Ps,guard=G}=C1 = clause(C0, Sub#sub{cexpr=E}),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{E,Ps}]),
    case {will_match(E, Ps),will_succeed(G)} of
	{yes,yes} -> [C1];			%Skip the rest
	{no,_Suc} -> clauses(E, Cs, Sub);	%Skip this clause
	{_Mat,no} -> clauses(E, Cs, Sub);	%Skip this clause
	{_Mat,_Suc} -> [C1|clauses(E, Cs, Sub)]
    end;
clauses(_, [], _) -> [].

%% will_succeed(Guard) -> yes | maybe | no.
%%  Test if we know whether a guard will succeed/fail or just don't
%%  know.  Be VERY conservative!

will_succeed(#c_atom{val=true}) -> yes;
will_succeed(#c_atom{val=false}) -> no;
will_succeed(_Guard) -> maybe.

%% will_match(Expr, [Pattern]) -> yes | maybe | no.
%%  Test if we know whether a match will succeed/fail or just don't
%%  know.  Be VERY conservative!

will_match(#c_values{es=Es}, Ps) ->
    will_match_list(Es, Ps, yes);
will_match(E, [P]) ->
    will_match_1(E, P);
will_match(_, _) -> no.

will_match_1(_E, #c_var{}) -> yes;		%Will always match
will_match_1(E, #c_alias{pat=P}) ->		%Pattern decides
    will_match_1(E, P);
will_match_1(#c_var{}, _P) -> maybe;
will_match_1(#c_tuple{es=Es}, #c_tuple{es=Ps}) ->
    will_match_list(Es, Ps, yes);
will_match_1(_, _) -> maybe.

will_match_list([E|Es], [P|Ps], M) ->
    case will_match_1(E, P) of
	yes -> will_match_list(Es, Ps, M);
	maybe -> will_match_list(Es, Ps, maybe);
	no -> no
    end;
will_match_list([], [], M) -> M;
will_match_list(_, _, _) -> no.			%Different length

%% eval_case(Case) -> #c_case{} | #c_let{}.
%%  If possible, evaluate a case at compile time.  We know that the
%%  last clause is guaranteed to match so if there is only one clause
%%  with a pattern containing only variables then rewrite to a let.

eval_case(#c_case{arg=#c_var{name=V},
		  clauses=[#c_clause{pats=[P],guard=G,body=B}|_]}=Cases,
	  #sub{t=Tdb}) ->
    case orddict:find(V, Tdb) of
	{ok,Type} ->
	    case {will_match_type(P, Type),will_succeed(G)} of
		{yes,yes} ->
		    {Ps,Es} = remove_non_vars(P, Type, [], []),
		    expr(#c_let{vars=Ps,arg=#c_values{es=Es},body=B}, sub_new());
		{_,_} -> eval_case(Cases)
	    end;
	error -> eval_case(Cases)
    end;
eval_case(Cases, _) -> eval_case(Cases).

eval_case(#c_case{arg=E,clauses=[#c_clause{pats=Ps,body=B}]}=Case) ->
    case is_var_pat(Ps) of
	true -> expr(#c_let{vars=Ps,arg=E,body=B}, sub_new());
	false -> Case
    end;
eval_case(Case) -> Case.

is_var_pat(Ps) -> all(fun (#c_var{}) -> true;
			  (_Pat) -> false
		      end, Ps).

will_match_type(#c_tuple{es=Es}, #c_tuple{es=Ps}) ->
    will_match_list_type(Es, Ps);
will_match_type(#c_atom{}=Atom, #c_atom{}=Atom) -> yes;
will_match_type(#c_var{}, _) -> yes;
will_match_type(_, _) -> no.

will_match_list_type([E|Es], [P|Ps]) ->
    case will_match_type(E, P) of
	yes -> will_match_list_type(Es, Ps);
	no -> no
    end;
will_match_list_type([], []) -> yes;
will_match_list_type(_, _) -> no.		%Different length

remove_non_vars(#c_tuple{es=Ps}, #c_tuple{es=Es}, Pacc, Eacc) ->
    remove_non_vars(Ps, Es, Pacc, Eacc);
remove_non_vars([#c_var{}=Var|Ps], [#c_alias{var=Evar}|Es], Pacc, Eacc) ->
    remove_non_vars(Ps, Es, [Var|Pacc], [Evar|Eacc]);
remove_non_vars([#c_var{}=Var|Ps], [E|Es], Pacc, Eacc) ->
    remove_non_vars(Ps, Es, [Var|Pacc], [E|Eacc]);
remove_non_vars([_|Ps], [_|Es], Pacc, Eacc) ->
    remove_non_vars(Ps, Es, Pacc, Eacc);
remove_non_vars([], [], Pacc, Eacc) -> {reverse(Pacc),reverse(Eacc)}.

%% case_opt(CaseArg, [Clause]) -> {CaseArg,[Clause]}.
%%  Try and optimise case by removing building argument terms.

case_opt(#c_tuple{anno=A,es=Es}, Cs0) ->
    Cs1 = case_opt_cs(Cs0, length(Es)),
    {core_lib:set_anno(core_lib:make_values(Es), A),Cs1};
case_opt(Arg, Cs) -> {Arg,Cs}.

case_opt_cs([#c_clause{pats=Ps0,guard=G,body=B}=C|Cs], Arity) ->
    case case_tuple_pat(Ps0, Arity) of
	{ok,Ps1,Avs} ->
	    Flet = fun ({V,Sub}, Body) -> letify(V, Sub, Body) end,
	    [C#c_clause{pats=Ps1,
			guard=letify_guard(Flet, Avs, G),
			body=foldl(Flet, B, Avs)}|case_opt_cs(Cs, Arity)];
	error ->				%Can't match
	    case_opt_cs(Cs, Arity)
    end;
case_opt_cs([], _) -> [].

letify_guard(Flet, Avs, #c_call{module=#c_atom{val=erlang},
			       name=#c_atom{val='not'},
			       args=[A]}=Call) ->
    Arg = letify_guard(Flet, Avs, A),
    Call#c_call{args=[Arg]};
letify_guard(Flet, Avs, #c_call{module=#c_atom{val=erlang},
			       name=#c_atom{val='and'},
			       args=[A1,A2]}=Call) ->
    Arg1 = letify_guard(Flet, Avs, A1),
    Arg2 = letify_guard(Flet, Avs, A2),
    Call#c_call{args=[Arg1,Arg2]};
letify_guard(Flet, Avs, #c_call{module=#c_atom{val=erlang},
			       name=#c_atom{val='or'},
			       args=[A1,A2]}=Call) ->
    Arg1 = letify_guard(Flet, Avs, A1),
    Arg2 = letify_guard(Flet, Avs, A2),
    Call#c_call{args=[Arg1,Arg2]};
letify_guard(Flet, Avs, #c_try{arg=B,
			       vars=[#c_var{name=X}],body=#c_var{name=X},
			       handler=#c_atom{val=false}}=Prot) ->
    Prot#c_try{arg=foldl(Flet, B, Avs)};
letify_guard(Flet, Avs, E) -> foldl(Flet, E, Avs).

%% case_tuple_pat([Pattern], Arity) -> {ok,[Pattern],[{AliasVar,Pat}]} | error.

case_tuple_pat([#c_tuple{es=Ps}], Arity) when length(Ps) =:= Arity ->
    {ok,Ps,[]};
case_tuple_pat([#c_var{anno=A}=V], Arity) ->
    Vars = make_vars(A, 1, Arity),
    {ok,Vars,[{V,#c_tuple{es=Vars}}]};
case_tuple_pat([#c_alias{var=V,pat=P}], Arity) ->
    case case_tuple_pat([P], Arity) of
	{ok,Ps,Avs} -> {ok,Ps,[{V,#c_tuple{es=unalias_pat_list(Ps)}}|Avs]};
	error -> error
    end;
case_tuple_pat(_, _) -> error.

%% unalias_pat(Pattern) -> Pattern.
%%  Remove all the aliases in a pattern but using the alias variables
%%  instead of the values.  We KNOW they will be bound.

unalias_pat(#c_alias{var=V}) -> V;
unalias_pat(#c_cons{hd=H0,tl=T0}=Cons) ->
    H1 = unalias_pat(H0),
    T1 = unalias_pat(T0),
    Cons#c_cons{hd=H1,tl=T1};
unalias_pat(#c_tuple{es=Ps}=Tuple) ->
    Tuple#c_tuple{es=unalias_pat_list(Ps)};
unalias_pat(Atomic) -> Atomic.

unalias_pat_list(Ps) -> map(fun unalias_pat/1, Ps).

make_vars(A, I, Max) when I =< Max ->
    [make_var(A, I)|make_vars(A, I+1, Max)];
make_vars(_, _, _) -> [].
    
make_var(A, N) ->
    #c_var{anno=A,name=list_to_atom("fol" ++ integer_to_list(N))}.

letify(#c_var{name=Vname}=Var, Val, Body) ->
    case core_lib:is_var_used(Vname, Body) of
	true ->
	    A = element(2, Body),
	    #c_let{anno=A,vars=[Var],arg=Val,body=Body};
	false -> Body
    end.

%% opt_case_in_let(LetExpr) -> LetExpr'
%%  In {V1,V2,...} = case E of P -> ... {Val1,Val2,...}; ... end.
%%  avoid building tuples, by converting tuples to multiple values.
%%  (The optimisation is not done if the built tuple is used or returned.)

opt_case_in_let(#c_let{vars=Vs,arg=Arg,body=B}=Let) ->
    case catch opt_case_in_let(Vs, Arg, B) of
	{'EXIT',_} -> Let;			%Optimisation not possible.
	Other -> Other
    end;
opt_case_in_let(Other) -> Other.

opt_case_in_let([#c_var{name=V}], Arg0,
		#c_case{arg=#c_var{name=V},clauses=[C1|_]}) ->
    #c_clause{pats=[#c_tuple{es=Es}],guard=#c_atom{val=true},body=B} = C1,
    true = all(fun (#c_var{}) -> true;
		   (_) -> false end, Es),	%Only variables in tuple
    false = core_lib:is_var_used(V, B),		%Built tuple must not be used.
    Arg1 = tuple_to_values(Arg0, length(Es)),	%Might fail.
    #c_let{vars=Es,arg=Arg1,body=B}.

%% tuple_to_values(Expr, TupleArity) -> Expr' | exception
%%  Convert tuples in return position of arity TupleArity to values.

tuple_to_values(#c_primop{name=#c_atom{val=match_fail},args=[_]}=Prim,
		_Arity) ->
    Prim;
tuple_to_values(#c_call{module=#c_atom{val=erlang},
			name=#c_atom{val=exit},
			args=Args}=Call,
		_Arity) when length(Args) == 1 ->
    Call;
tuple_to_values(#c_call{module=#c_atom{val=erlang},
			name=#c_atom{val=fault},
			args=Args}=Call,
		_Arity) when length(Args) == 1 ->
    Call;
tuple_to_values(#c_call{module=#c_atom{val=erlang},
			name=#c_atom{val=fault},
			args=Args}=Call,
		_Arity) when length(Args) == 2 ->
    Call;
tuple_to_values(#c_tuple{es=Es}, Arity) when length(Es) =:= Arity ->
    core_lib:make_values(Es);
tuple_to_values(#c_case{clauses=Cs0}=Case, Arity) ->
    Cs1 = map(fun(E) -> tuple_to_values(E, Arity) end, Cs0),
    Case#c_case{clauses=Cs1};
tuple_to_values(#c_seq{body=B0}=Seq, Arity) ->
    Seq#c_seq{body=tuple_to_values(B0, Arity)};
tuple_to_values(#c_let{body=B0}=Let, Arity) ->
    Let#c_let{body=tuple_to_values(B0, Arity)};
tuple_to_values(#c_letrec{body=B0}=Letrec, Arity) ->
    Letrec#c_letrec{body=tuple_to_values(B0, Arity)};
tuple_to_values(#c_receive{clauses=Cs0,action=A0}=Rec, Arity) ->
    Cs = map(fun(E) -> tuple_to_values(E, Arity) end, Cs0),
    A = tuple_to_values(A0, Arity),
    Rec#c_receive{clauses=Cs,action=A};
tuple_to_values(#c_clause{body=B0}=Clause, Arity) ->
    B = tuple_to_values(B0, Arity),
    Clause#c_clause{body=B}.

%% update_types(Expr, Pattern, Types) -> Types'
%%  Updates the type database.
update_types(#c_var{name=V}, [#c_tuple{}=P], Types) ->
    orddict:store(V, P, Types);
update_types(_, _, Types) -> Types.

%% update_types(V, Tdb) -> Tdb'
%%  Kill any entries that references the variable,
%%  either in the key or in the value.
kill_types(V, [{V,_}|Tdb]) ->
    kill_types(V, Tdb);
kill_types(V, [{_,#c_tuple{es=Vars}}=Entry|Tdb]) ->
    case v_is_value(V, Vars) of
	true -> kill_types(V, Tdb);
	false -> [Entry|kill_types(V, Tdb)]
    end;
kill_types(_, []) -> [].
