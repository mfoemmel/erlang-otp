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

-export([module/2,format_error/1]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,all/2,any/2,
		reverse/1,reverse/2,member/2,nth/2,flatten/1]).
-include("core_parse.hrl").

%%-define(DEBUG, 1).

-ifdef(DEBUG).
-define(ASSERT(E),
	case E of
	    true -> ok;
	    false ->
		io:format("~p, line ~p: assertion failed\n", [?MODULE,?LINE]),
		exit(assertion_failed)
	end).
-else.
-define(ASSERT(E), ignore).
-endif.

%% Variable value info.
-record(sub, {v=[],				%Variable substitutions
	      s=[],				%Variables in scope
	      t=[]}).				%Types

module(#c_module{defs=Ds0}=Mod, Opts) ->
    put(bin_opt_info, member(bin_opt_info, Opts)),
    put(no_inline_list_funcs, not member(inline_list_funcs, Opts)),
    case get(new_var_num) of
	undefined -> put(new_var_num, 0);
	_ -> ok
    end,
    init_warnings(),
    Ds1 = map(fun function_1/1, Ds0),
    erase(no_inline_list_funcs),
    erase(bin_opt_info),
    {ok,Mod#c_module{defs=Ds1},get_warnings()}.

function_1({#c_fname{id=F,arity=Arity}=Name,B0}) ->
    try
	?ASSERT([] =:= core_lib:free_vars(B0)),
	B = expr(B0, value, sub_new()),			%This must be a fun!
	{Name,B}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [F,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%% body(Expr, Sub) -> Expr.
%% body(Expr, Context, Sub) -> Expr.
%%  No special handling of anything except values.

body(Body, Sub) ->
    body(Body, value, Sub).

body(#c_values{anno=A,es=Es0}, Ctxt, Sub) ->
    Es1 = expr_list(Es0, Ctxt, Sub),
    #c_values{anno=A,es=Es1};
body(E, Ctxt, Sub) ->
    ?ASSERT(verify_scope(E, Sub)),
    expr(E, Ctxt, Sub).

%% guard(Expr, Sub) -> Expr.
%%  Do guard expression.  These are boolean expressions with values
%%  which are tests.  These may be wrapped in a protected.  Seeing
%%  that guards are side-effect free we can optimise the boolean
%%  expressions.

guard(Expr, Sub) ->
    ?ASSERT(verify_scope(Expr, Sub)),
    guard_1(Expr, Sub).

guard_1(#c_call{module=#c_literal{val=erlang},
	      name=#c_literal{val='not'},
	      args=[A]}=Call, Sub) ->
    case guard_1(A, Sub) of
	#c_literal{val=true} -> #c_literal{val=false};
	#c_literal{val=false} -> #c_literal{val=true};
	Arg -> Call#c_call{args=[Arg]}
    end;
guard_1(#c_call{module=#c_literal{val=erlang},
	      name=#c_literal{val='and'},
	      args=[A1,A2]}=Call, Sub) ->
    case {guard_1(A1, Sub),guard_1(A2, Sub)} of
	{#c_literal{val=true},Arg2} -> Arg2;
	{#c_literal{val=false},_} -> #c_literal{val=false};
	{Arg1,#c_literal{val=true}} -> Arg1;
	{_,#c_literal{val=false}} -> #c_literal{val=false};
	{Arg1,Arg2} -> Call#c_call{args=[Arg1,Arg2]}
    end;
guard_1(#c_call{module=#c_literal{val=erlang},
	      name=#c_literal{val='or'},
	      args=[A1,A2]}=Call, Sub) ->
    case {guard_1(A1, Sub),guard_1(A2, Sub)} of
	{#c_literal{val=true},_} -> #c_literal{val=true};
	{#c_literal{val=false},Arg2} -> Arg2;
	{_,#c_literal{val=true}} -> #c_literal{val=true};
	{Arg1,#c_literal{val=false}} -> Arg1;
	{Arg1,Arg2} -> Call#c_call{args=[Arg1,Arg2]}
    end;
guard_1(#c_try{arg=E0,vars=[#c_var{name=X}],body=#c_var{name=X},
	     handler=#c_literal{val=false}=False}=Prot, Sub) ->
    E1 = body(E0, value, Sub),
    case will_fail(E1) of
	false ->
	    %% We can remove try/catch if value is a simple.
	    case core_lib:is_simple(E1) of
		true -> E1;
		false -> Prot#c_try{arg=E1}
	    end;
	true ->
	    %% Expression will always fail.
	    False
    end;
guard_1(E0, Sub) ->
    E = expr(E0, value, Sub),
    case will_fail(E) of
	true -> #c_literal{val=false};
	false -> E
    end.

%% expr(Expr, Sub) -> Expr.
%% expr(Expr, Context, Sub) -> Expr.

expr(Expr, Sub) ->
    expr(Expr, value, Sub).

expr(#c_var{}=V, Ctxt, Sub) ->
    %% Return void() in effect context to potentially shorten the life time
    %% of the variable and potentially generate better code
    %% (for instance, if the variable no longer needs to survive a function
    %% call, there will be no need to save it in the stack frame).
    case Ctxt of
	effect -> void();
	value -> sub_get_var(V, Sub)
    end;
expr(#c_literal{val=Val}=L, Ctxt, _Sub) ->
    case Ctxt of
	effect ->
	    case Val of
		[] ->
		    %% Keep as [] - might give slightly better code.
		    L;
		_ when is_atom(Val) ->
		    %% For cleanliness replace with void().
		    void();
		_ ->
		    %% Warn and replace with void().
		    add_warning(L, useless_building),
		    void()
	    end;
	value -> L
    end;
expr(#c_cons{hd=H0,tl=T0}=Cons, Ctxt, Sub) ->
    H1 = expr(H0, Ctxt, Sub),
    T1 = expr(T0, Ctxt, Sub),
    case Ctxt of
	effect ->
	    add_warning(Cons, useless_building),
	    expr(make_effect_seq([H1,T1]), Ctxt, Sub);
	value ->
	    eval_cons(Cons#c_cons{hd=H1,tl=T1}, H1, T1)
    end;
expr(#c_tuple{es=Es0}=Tuple, Ctxt, Sub) ->
    Es = expr_list(Es0, Ctxt, Sub),
    case Ctxt of
	effect ->
	    add_warning(Tuple, useless_building),
	    expr(make_effect_seq(Es), Ctxt, Sub);
	value ->
	    eval_tuple(Tuple, Es)
    end;
expr(#c_binary{segments=Ss}=Bin, Ctxt, Sub) ->
    case Ctxt of
	effect -> add_warning(Bin, useless_building);
	value -> ok
    end,
    %% Keep because of possible side effect (exception).
    eval_binary(Bin#c_binary{segments=bitstr_list(Ss, Sub)});
expr(#c_fname{}=Fname, _, _) -> Fname;
expr(#c_fun{}=Fun, effect, _) ->
    %% A fun is created, but not used. Warn, and replace with the void value.
    add_warning(Fun, useless_building),
    void();
expr(#c_fun{vars=Vs0,body=B0}=Fun, Ctxt0, Sub0) ->
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    Ctxt = case Ctxt0 of
	       {letrec,Ctxt1} -> Ctxt1;
	       value -> value
	   end,
    B1 = body(B0, Ctxt, Sub1),
    Fun#c_fun{vars=Vs1,body=B1};
expr(#c_seq{arg=Arg0,body=B0}=Seq0, Ctxt, Sub) ->
    %% Optimise away pure literal arg as its value is ignored.
    B1 = body(B0, Ctxt, Sub),
    Arg = body(Arg0, effect, Sub),
    case will_fail(Arg) of
	true ->
	    Arg;
	false ->
	    case Arg of
		#c_values{es=Es} ->
		    case is_safe_simple_list(Es) of
			true -> B1;
			false -> Seq0#c_seq{arg=Arg,body=B1}
		    end;
		_ ->
		    case is_safe_simple(Arg) of
			true -> B1;
			false -> Seq0#c_seq{arg=Arg,body=B1}
		    end
	    end
    end;
expr(#c_let{}=Let, Ctxt, Sub) ->
    case simplify_let(Let, Sub) of
	impossible ->
	    %% The argument for the let is "simple", i.e. has no
	    %% complex structures such as let or seq that can be entered.
	    opt_simple_let(Let, Ctxt, Sub);
	Expr ->
	    %% The let body was successfully moved into the let argument.
	    %% Now recursively re-process the new expression.
	    expr(Expr, Ctxt, sub_new(Sub))
    end;
expr(#c_letrec{defs=Fs0,body=B0}=Letrec, Ctxt, Sub) ->
    Fs1 = map(fun ({Name,Fb}) ->
		      {Name,expr(Fb, {letrec,Ctxt}, Sub)}
	      end, Fs0),
    B1 = body(B0, value, Sub),
    Letrec#c_letrec{defs=Fs1,body=B1};
expr(#c_case{}=Case0, Ctxt, Sub) ->
    case simplify_case(Case0) of
	#c_case{arg=Arg0,clauses=Cs0}=Case1 ->
	    Arg1 = body(Arg0, value, Sub),
	    {Arg2,Cs1} = case_opt(Arg1, Cs0),
	    Cs2 = clauses(Arg2, Cs1, Case1, Ctxt, Sub),
	    Case = eval_case(Case1#c_case{arg=Arg2,clauses=Cs2}, Sub),
	    bsm_an(Case);
	Other ->
	    expr(Other, Ctxt, Sub)
    end;
expr(#c_receive{clauses=Cs0,timeout=T0,action=A0}=Recv, Ctxt, Sub) ->
    Cs1 = clauses(#c_var{name='_'}, Cs0, Recv, Ctxt, Sub), %This is all we know
    T1 = expr(T0, value, Sub),
    A1 = body(A0, Ctxt, Sub),
    Recv#c_receive{clauses=Cs1,timeout=T1,action=A1};
expr(#c_apply{op=Op0,args=As0}=App, _, Sub) ->
    Op1 = expr(Op0, value, Sub),
    As1 = expr_list(As0, value, Sub),
    App#c_apply{op=Op1,args=As1};
expr(#c_call{module=M0,name=N0}=Call0, Ctxt, Sub) ->
    M1 = expr(M0, value, Sub),
    N1 = expr(N0, value, Sub),
    Call = Call0#c_call{module=M1,name=N1},
    case useless_call(Ctxt, Call) of
	no -> call(Call, M1, N1, Sub);
	{yes,Seq} -> expr(Seq, Ctxt, Sub)
    end;
expr(#c_primop{args=As0}=Prim, _, Sub) ->
    As1 = expr_list(As0, value, Sub),
    Prim#c_primop{args=As1};
expr(#c_catch{body=B0}=Catch, _, Sub) ->
    %% We can remove catch if the value is simple
    B1 = body(B0, value, Sub),
    case is_safe_simple(B1) of
	true -> B1;
	false -> Catch#c_catch{body=B1}
    end;
expr(#c_try{arg=E0,vars=[#c_var{name=X}],body=#c_var{name=X},
	    handler=#c_literal{val=false}=False}=Prot, _, Sub) ->
    %% Since guard may call expr/2, we must do some optimization of
    %% the kind of try's that occur in guards.
    E1 = body(E0, value, Sub),
    case will_fail(E1) of
	false ->
	    %% We can remove try/catch if value is a simple.
	    case core_lib:is_simple(E1) of
		true -> E1;
		false -> Prot#c_try{arg=E1}
	    end;
	true ->
	    %% Expression will always fail.
	    False
    end;
expr(#c_try{anno=A,arg=E0,vars=Vs0,body=B0,evars=Evs0,handler=H0}=Try, _, Sub0) ->
    %% Here is the general try/catch construct outside of guards.
    %% We can remove try if the value is simple and replace it with a let.
    E1 = body(E0, value, Sub0),
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    B1 = body(B0, value, Sub1),
    case is_safe_simple(E1) of
	true ->
	    expr(#c_let{anno=A,vars=Vs1,arg=E1,body=B1}, value, Sub0);
	false ->
	    {Evs1,Sub2} = pattern_list(Evs0, Sub0),
	    H1 = body(H0, value, Sub2),
	    Try#c_try{arg=E1,vars=Vs1,body=B1,evars=Evs1,handler=H1}
    end.

expr_list(Es, Ctxt, Sub) ->
    map(fun (E) -> expr(E, Ctxt, Sub) end, Es).

bitstr_list(Es, Sub) ->
    map(fun (E) -> bitstr(E, Sub) end, Es).

bitstr(#c_bitstr{val=Val,size=Size}=BinSeg, Sub) ->
    BinSeg#c_bitstr{val=expr(Val, Sub),size=expr(Size, value, Sub)}.

%% is_safe_simple(Expr) -> true | false.
%%  A safe simple cannot fail with badarg.  Binaries are difficult to
%%  check so we consider them unsafe.

is_safe_simple(#c_var{}) -> true;		%Not atomic
is_safe_simple(#c_cons{hd=H,tl=T}) ->
    is_safe_simple(H) andalso is_safe_simple(T);
is_safe_simple(#c_tuple{es=Es}) -> is_safe_simple_list(Es);
is_safe_simple(#c_literal{}) -> true;
is_safe_simple(#c_fname{}) -> true;
is_safe_simple(_) -> false.

is_safe_simple_list(Es) -> all(fun is_safe_simple/1, Es).

%% will_fail(Expr) -> true|false.
%%  Determine whether the expression will fail with an exception.
%%  Return true if the expression always will fail with an exception,
%%  i.e. never return normally.

will_fail(#c_let{arg=A,body=B}) ->
    will_fail(A) orelse will_fail(B);
will_fail(#c_call{module=#c_literal{val=Mod},name=#c_literal{val=Name},args=Args}) ->
    erl_bifs:is_exit_bif(Mod, Name, length(Args));
will_fail(#c_primop{name=#c_literal{val=match_fail},args=[_]}) -> true;
will_fail(_) -> false.

%% eval_cons(Cons, Head, Tail) -> Expr.
%%  Evaluate constant part of a cons expression.

eval_cons(#c_cons{anno=A}, #c_literal{val=C}, #c_literal{val=S}) ->
    #c_literal{anno=A,val=[C|S]};
eval_cons(Cons, H, T) ->
    Cons#c_cons{hd=H,tl=T}.			%Rebuild cons arguments

eval_tuple(#c_tuple{anno=A}=Tuple, Es0) ->
    case eval_tuple_1(Es0, []) of
	no -> Tuple#c_tuple{es=Es0};
	Es -> #c_literal{anno=A,val=list_to_tuple(Es)}
    end.

eval_tuple_1([#c_literal{val=Val}|Es], Acc) ->
    eval_tuple_1(Es, [Val|Acc]);
eval_tuple_1([_|_], _) -> no;
eval_tuple_1([], Acc) -> reverse(Acc).

eval_binary(#c_binary{anno=Anno,segments=Ss}=Bin) ->
    try
	#c_literal{anno=Anno,val=eval_binary_1(Ss, <<>>)}
    catch
	throw:impossible ->
	    Bin;
	  throw:{badarg,Warning} ->
	    add_warning(Bin, Warning),
	    #c_call{module=#c_literal{val=erlang},
		    name=#c_literal{val=error},
		    args=[#c_literal{val=badarg}]}
    end.

eval_binary_1([#c_bitstr{val=#c_literal{val=Val},size=#c_literal{val=Sz},
			 unit=#c_literal{val=Unit},type=#c_literal{val=Type},
			 flags=#c_literal{val=Flags}}|Ss], Acc0) ->
    Endian = case member(big, Flags) of
		 true ->
		     big;
		 false ->
		     case member(little, Flags) of
			 true -> little;
			 false -> throw(impossible) %Native endian.
		     end
	     end,

    %% Make sure that the size is reasonable.
    case Type of
	binary when is_bitstring(Val) ->
	    if
		Sz =:= all ->
		    ok;
		Sz*Unit =< bit_size(Val) ->
		    ok;
		true ->
		    %% Field size is greater than the actual binary - will fail.
		    throw({badarg,embedded_binary_size})
	    end;
	integer when is_integer(Val) ->
	    %% Estimate the number of bits needed to to hold the integer
	    %% literal. Check whether the field size is reasonable in
	    %% proportion to the number of bits needed.
	    if
		Sz*Unit =< 256 ->
		    %% Don't be cheap - always accept fields up to this size.
		    ok;
		true ->
		    case count_bits(Val) of
			BitsNeeded when 2*BitsNeeded >= Sz*Unit ->
			    ok;
			_ ->
			    %% More than about half of the field size will be
			    %% filled out with zeroes - not acceptable.
			    throw(impossible)
		    end
	    end;
	float when is_float(Val) ->
	    %% Bad float size.
	    case Sz*Unit of
		32 -> ok;
		64 -> ok;
		_ -> throw(impossible)
	    end;
	_ ->
	    throw(impossible)
    end,

    %% Evaluate the field.
    try eval_binary_2(Acc0, Val, Sz, Unit, Type, Endian) of
	Acc -> eval_binary_1(Ss, Acc)
    catch
	error:_ ->
	    throw(impossible)
    end;
eval_binary_1([], Acc) -> Acc;
eval_binary_1(_, _) -> throw(impossible).

eval_binary_2(Acc, Val, Size, Unit, integer, little) ->
    <<Acc/bitstring,Val:(Size*Unit)/little>>;
eval_binary_2(Acc, Val, Size, Unit, integer, big) ->
    <<Acc/bitstring,Val:(Size*Unit)/big>>;
eval_binary_2(Acc, Val, Size, Unit, float, little) ->
    <<Acc/bitstring,Val:(Size*Unit)/little-float>>;
eval_binary_2(Acc, Val, Size, Unit, float, big) ->
    <<Acc/bitstring,Val:(Size*Unit)/big-float>>;
eval_binary_2(Acc, Val, all, Unit, binary, _) ->
    case bit_size(Val) of
	Size when Size rem Unit =:= 0 ->
	    <<Acc/bitstring,Val:Size/bitstring>>;
	Size ->
	    throw({badarg,{embedded_unit,Unit,Size}})
    end;
eval_binary_2(Acc, Val, Size, Unit, binary, _) ->
    <<Acc/bitstring,Val:(Size*Unit)/bitstring>>.

%% Count the number of bits approximately needed to store Int.
%% (We don't need an exact result for this purpose.)

count_bits(Int) -> 
    count_bits_1(abs(Int), 64).

count_bits_1(0, Bits) -> Bits;
count_bits_1(Int, Bits) -> count_bits_1(Int bsr 64, Bits+64).

%% useless_call(Context, #c_call{}) -> no | {yes,Expr}
%%  Check whether the function is called only for effect,
%%  and if the function either has no effect whatsoever or
%%  the only effect is an exception. Generate appropriate
%%  warnings. If the call is "useless" (has no effect),
%%  a rewritten expression consisting of a sequence of
%%  the arguments only is returned.

useless_call(effect, #c_call{module=#c_literal{val=Mod},
			     name=#c_literal{val=Name},
			     args=Args}=Call) ->
    A = length(Args),
    case erl_bifs:is_safe(Mod, Name, A) of
	false ->
	    case erl_bifs:is_pure(Mod, Name, A) of
		true -> add_warning(Call, result_ignored);
		false -> ok
	    end,
	    no;
	true ->
	    add_warning(Call, {no_effect,{Mod,Name,A}}),
	    {yes,make_effect_seq(Args)}
    end;
useless_call(_, _) -> no.

%% make_effect_seq([Expr]) -> #c_seq{}|void()
%%  Convert a list of epressions evaluated in effect context to a chain of
%%  #c_seq{}. The body in the innermost #c_seq{} will be void().
%%  Anything that will not have any effect will be thrown away.

make_effect_seq([H|T]) ->
    case is_safe_simple(H) of
	true -> make_effect_seq(T);
	false -> #c_seq{arg=H,body=make_effect_seq(T)}
    end;
make_effect_seq([]) -> void().

%% Handling remote calls. The module/name fields have been processed.

call(#c_call{args=As}=Call, #c_literal{val=M}=M0, #c_literal{val=N}=N0, Sub) ->
    case get(no_inline_list_funcs) of
  	true ->
 	    call_0(Call, M0, N0, As, Sub);
  	false ->
  	    call_1(Call, M, N, As, Sub)
      end;
call(#c_call{args=As}=Call, M, N, Sub) ->
    call_0(Call, M, N, As, Sub).

call_0(Call, M, N, As0, Sub) ->
    As1 = expr_list(As0, value, Sub),
    fold_call(Call#c_call{args=As1}, M, N, As1).

%% We inline some very common higher order list operations.
%% We use the same evaluation order as the library function.

call_1(_Call, lists, all, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^all', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_apply{op=Loop, args=[Xs]}},
    CC2 = #c_clause{pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=#c_literal{val=false}},
    CC3 = #c_clause{pats=[X], guard=#c_literal{val=true},
		    body=#c_primop{name=#c_literal{val='match_fail'},
				   args=[Err1]}},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_case{arg=#c_apply{op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=true}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, any, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^any', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_literal{val=true}},
    CC2 = #c_clause{pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=#c_apply{op=Loop, args=[Xs]}},
    CC3 = #c_clause{pats=[X], guard=#c_literal{val=true},
		    body=#c_primop{name=#c_literal{val='match_fail'},
				   args=[Err1]}},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_case{arg=#c_apply{op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=false}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, foreach, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^foreach', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_seq{arg=#c_apply{op=F, args=[X]},
			       body=#c_apply{op=Loop, args=[Xs]}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=ok}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, map, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^map', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[H], arg=#c_apply{op=F, args=[X]},
			       body=#c_cons{hd=H,
					    tl=#c_apply{op=Loop,
							args=[Xs]}}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=[]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, flatmap, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^flatmap', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[H],
			       arg=#c_apply{op=F, args=[X]},
			       body=#c_call{module=#c_literal{val=erlang},
					    name=#c_literal{val='++'},
					    args=[H,
						  #c_apply{op=Loop,
							   args=[Xs]}]}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=[]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
	 Sub);
call_1(_Call, lists, filter, [Arg1,Arg2], Sub) ->
    Loop = #c_fname{id='lists^filter', arity=1},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    B = #c_var{name='B'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_cons{hd=X, tl=Xs}},
    CC2 = #c_clause{pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=Xs},
    CC3 = #c_clause{pats=[X], guard=#c_literal{val=true},
		    body=#c_primop{name=#c_literal{val='match_fail'},
				   args=[Err1]}},
    Case = #c_case{arg=B, clauses = [CC1, CC2, CC3]},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[B],
			       arg=#c_apply{op=F, args=[X]},
			       body=#c_let{vars=[Xs],
					   arg=#c_apply{op=Loop,
							args=[Xs]},
					   body=Case}}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
		   body=#c_literal{val=[]}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err2]}},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L]}}},
    Sub);
call_1(_Call, lists, foldl, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_fname{id='lists^foldl', arity=2},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_apply{op=Loop,
				 args=[Xs, #c_apply{op=F, args=[X, A]}]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true}, body=A},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L, A]}}},
	 Sub);
call_1(_Call, lists, foldr, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_fname{id='lists^foldr', arity=2},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_apply{op=F, args=[X, #c_apply{op=Loop,
							 args=[Xs, A]}]}},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true}, body=A},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{op=Loop, args=[L, A]}}},
	 Sub);
call_1(_Call, lists, mapfoldl, [Arg1,Arg2,Arg3], Sub) ->
    Loop = #c_fname{id='lists^mapfoldl', arity=2},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Avar = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{pats=[P], guard=#c_literal{val=true}, body=E},
		Err = #c_tuple{es=[#c_literal{val='badmatch'}, X]},
		C2 = #c_clause{pats=[X], guard=#c_literal{val=true},
			       body=#c_primop{name=#c_literal{val='match_fail'},
					      args=[Err]}},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=Match(#c_apply{op=F, args=[X, Avar]},
			      #c_tuple{es=[X, Avar]},
%%% Tuple passing version
			      Match(#c_apply{op=Loop, args=[Xs, Avar]},
				    #c_tuple{es=[Xs, Avar]},
				    #c_tuple{es=[#c_cons{hd=X, tl=Xs}, Avar]})
%%% Multiple-value version
%%% 			      #c_let{vars=[Xs,A],
%%% 				     %% The tuple here will be optimised
%%% 				     %% away later; no worries.
%%% 				     arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 				     body=#c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 							A]}}
			     )},
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
%%% Tuple passing version
		   body=#c_tuple{es=[#c_literal{val=[]}, Avar]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_literal{val=[]}, A]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, Avar],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, Avar, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
%%% Tuple passing version
			       body=#c_apply{op=Loop, args=[L, Avar]}}},
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
    Avar = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{pats=[P], guard=#c_literal{val=true}, body=E},
		Err = #c_tuple{es=[#c_literal{val='badmatch'}, X]},
		C2 = #c_clause{pats=[X], guard=#c_literal{val=true},
			       body=#c_primop{name=#c_literal{val='match_fail'},
					      args=[Err]}},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
%%% Tuple passing version
		   body=Match(#c_apply{op=Loop, args=[Xs, Avar]},
			      #c_tuple{es=[Xs, Avar]},
			      Match(#c_apply{op=F, args=[X, Avar]},
				    #c_tuple{es=[X, Avar]},
				    #c_tuple{es=[#c_cons{hd=X, tl=Xs}, Avar]}))
%%% Multiple-value version
%%% 		   body=#c_let{vars=[Xs,A],
%%% 			       %% The tuple will be optimised away
%%% 			       arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 			       body=Match(#c_apply{op=F, args=[X, A]},
%%% 					  #c_tuple{es=[X, A]},
%%% 					  #c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 						        A]})}
		  },
    C2 = #c_clause{pats=[#c_literal{val=[]}], guard=#c_literal{val=true},
%%% Tuple passing version
		   body=#c_tuple{es=[#c_literal{val=[]}, Avar]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_literal{val=[]}, A]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=#c_primop{name=#c_literal{val='match_fail'},
				  args=[Err]}},
    Fun = #c_fun{vars=[Xs, Avar],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    expr(#c_let{vars=[F, Avar, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
		body=#c_letrec{defs=[{Loop,Fun}],
%%% Tuple passing version
 			       body=#c_apply{op=Loop, args=[L, Avar]}}},
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
%%  We evalute '++' if the first operand is a literal (or partly literal).

fold_call(Call, #c_literal{val=M}, #c_literal{val=F}, Args) ->
    fold_call_1(Call, M, F, Args);
fold_call(Call, _M, _N, _Args) -> Call.

fold_call_1(Call, lists, append, [Arg1,Arg2]) ->
    eval_append(Call, Arg1, Arg2);
fold_call_1(Call, erlang, length, [Arg]) ->
    eval_length(Call, Arg);
fold_call_1(Call, erlang, '++', [Arg1,Arg2]) ->
    eval_append(Call, Arg1, Arg2);
fold_call_1(Call, erlang, element, [Arg1,Arg2]) ->
    eval_element(Call, Arg1, Arg2);
fold_call_1(Call, erlang, setelement, [Arg1,Arg2,Arg3]) ->
    try
	eval_setelement(Arg1, Arg2, Arg3)
    catch _:_ ->
	    Call
    end;
fold_call_1(Call, erlang, apply, [Mod,Func,Args]) ->
    simplify_apply(Call, Mod, Func, Args);
fold_call_1(Call, erlang, N, Args) ->
    eval_erlang_call(Call, N, Args);
fold_call_1(Call, _Mod, _Name, _Args) -> Call.

eval_erlang_call(Call, N, Args0) ->
    NumArgs = length(Args0),
    case erl_bifs:is_pure(erlang, N, NumArgs) of
	false -> Call;				%Not pure - keep call.
	true ->
	    case catch begin
			   Args = [core_lib:literal_value(A) || A <- Args0],
			   eval_erlang_call_1(Call, N, Args)
		       end of
		{ok,Val} -> Val;
		_Other ->
		    case erl_internal:comp_op(N, NumArgs) of
			false -> Call;
			true -> eval_rel_op(Call, N, Args0)
		    end
	    end
    end.

eval_erlang_call_1(Call, N, Args) ->
    case catch apply(erlang, N, Args) of
	{'EXIT',{Reason,_}} ->
	    eval_failure(Call, Reason);
	Val ->
	    {ok,core_lib:make_literal(Val)}
    end.

eval_rel_op(Call, Op, [#c_var{name=V},#c_var{name=V}]) ->
    Bool = erlang:Op(same, same),
    #c_literal{anno=core_lib:get_anno(Call),val=Bool};
eval_rel_op(Call, _, _) -> Call.

%% eval_length(Call, List) -> Val.
%%  Evaluates the length for the prefix of List which has a known
%%  shape.

eval_length(Call, Core) -> eval_length(Call, Core, 0).

eval_length(Call, #c_literal{val=Val}, Len0) ->
    try
	Len = Len0 + length(Val),
	#c_literal{anno=Call#c_call.anno,val=Len}
    catch
	_:_ ->
	    eval_failure(Call, badarg)
    end;
eval_length(Call, #c_cons{tl=T}, Len) ->
    eval_length(Call, T, Len+1);
eval_length(Call, _List, 0) ->
    Call;		%Could do nothing
eval_length(Call, List, Len) ->
    A = Call#c_call.anno,
    #c_call{anno=A,
	    module=#c_literal{anno=A,val=erlang},
	    name=#c_literal{anno=A,val='+'},
	    args=[#c_literal{anno=A,val=Len},Call#c_call{args=[List]}]}.

%% eval_append(Call, FirstList, SecondList) -> Val.
%%  Evaluates the constant part of '++' expression.

eval_append(Call, #c_literal{val=Cs1}=S1, #c_literal{val=Cs2}) ->
    try
	S1#c_literal{val=Cs1 ++ Cs2}
    catch error:badarg ->
	    eval_failure(Call, badarg)
    end;
eval_append(Call, #c_literal{val=Cs}, List) when length(Cs) =< 4 ->
    Anno = Call#c_call.anno,
    foldr(fun (C, L) ->
		  #c_cons{anno=Anno,hd=#c_literal{val=C},tl=L}
	  end, List, Cs);
eval_append(Call, #c_cons{tl=T}=Cons, List) ->
    Cons#c_cons{tl=eval_append(Call, T, List)};
eval_append(Call, X, Y) ->
    Call#c_call{args=[X,Y]}.			%Rebuild call arguments.

%% eval_element(Pos, Tuple) -> Val.
%%  Evaluates element/2 if Pos and Tuple are literals.

eval_element(Call, #c_literal{val=Pos}, #c_tuple{es=Es}) when is_integer(Pos) ->
    if
	1 =< Pos, Pos =< length(Es) ->
	    lists:nth(Pos, Es);
	true ->
	    eval_failure(Call, badarg)
    end;
eval_element(Call, #c_literal{val=Pos}, #c_literal{val=Val}=Lit)
  when is_integer(Pos), is_tuple(Val) ->
    if
	1 =< Pos, Pos =< tuple_size(Val) ->
	    Lit#c_literal{val=element(Pos, Val)};
	true ->
	    eval_failure(Call, badarg)
    end;
eval_element(Call, #c_literal{val=I}, #c_var{}) when is_integer(I) -> Call;
eval_element(Call, #c_var{}, #c_literal{}) -> Call;
eval_element(Call, #c_var{}, #c_tuple{}) -> Call;
eval_element(Call, #c_var{}, #c_var{}) -> Call;
eval_element(Call, _, _) -> eval_failure(Call, badarg).

%% eval_setelement(Pos, Tuple, NewVal) -> Val.
%%  Evaluates setelement/3 if Pos and Tuple are literals.

eval_setelement(#c_literal{val=Pos}, #c_tuple{anno=A,es=Es}, NewVal)
  when is_integer(Pos) ->
    make_tuple(A, eval_setelement_1(Pos, Es, NewVal));
eval_setelement(#c_literal{val=Pos}, #c_literal{anno=A,val=Es0}, NewVal)
  when is_integer(Pos) ->
    Es = [#c_literal{anno=A,val=E} || E <- tuple_to_list(Es0)],
    make_tuple(A, eval_setelement_1(Pos, Es, NewVal)).

eval_setelement_1(1, [_|T], NewVal) ->
    [NewVal|T];
eval_setelement_1(Pos, [H|T], NewVal) when Pos > 1 ->
    [H|eval_setelement_1(Pos-1, T, NewVal)].

eval_failure(Call, Reason) ->
    add_warning(Call, {eval_failure,Reason}),
    #c_call{module=#c_literal{val=erlang},
	    name=#c_literal{val=error},
	    args=[core_lib:make_literal(Reason)]}.

make_tuple(Anno, Es0) ->
    case core_lib:is_literal_list(Es0) of
	false ->
	    #c_tuple{anno=Anno,es=Es0};
	true ->
	    Es = core_lib:concrete_list(Es0),
	    #c_literal{anno=Anno,val=list_to_tuple(Es)}
    end.

%% simplify_apply(Call0, Mod, Func, Args) -> Call
%%  Simplify an apply/3 to a call if the number of arguments
%%  are known at compile time.

simplify_apply(Call, Mod, Func, Args) ->
    case is_atom_or_var(Mod) andalso is_atom_or_var(Func) of
	true -> simplify_apply_1(Args, Call, Mod, Func, []);
	false -> Call
    end.

simplify_apply_1(#c_literal{val=MoreArgs0}, Call, Mod, Func, Args)
  when length(MoreArgs0) >= 0 ->
    MoreArgs = core_lib:make_literal_list(MoreArgs0),
    Call#c_call{module=Mod,name=Func,args=reverse(Args, MoreArgs)};
simplify_apply_1(#c_cons{hd=Arg,tl=T}, Call, Mod, Func, Args) ->
    simplify_apply_1(T, Call, Mod, Func, [Arg|Args]);
simplify_apply_1(_, Call, _, _, _) -> Call.

is_atom_or_var(#c_literal{val=Atom}) when is_atom(Atom) -> true;
is_atom_or_var(#c_var{}) -> true;
is_atom_or_var(_) -> false.

%% clause(Clause, Cepxr, Context, Sub) -> Clause.

clause(#c_clause{pats=Ps0,guard=G0,body=B0}=Cl, Cexpr, Ctxt, Sub0) ->
    {Ps1,Sub1} = pattern_list(Ps0, Sub0),
    Sub2 = update_types(Cexpr, Ps1, Sub1),
    GSub = case {Cexpr,Ps1} of
	       {#c_var{name='_'},_} ->
		   %% In a 'receive', Cexpr is the variable '_', which represents the
		   %% message being matched. We must NOT do any extra substiutions.
		   Sub2;
	       {#c_var{},[#c_var{}=Var]} ->
		   %% The idea here is to optimize expressions such as
		   %%
		   %%   case A of A -> ...
		   %%
		   %% to get rid of the extra guard test that the compiler
		   %% added when converting to the Core Erlang representation:
		   %%
		   %%   case A of NewVar when A =:= NewVar -> ...
		   %%
		   %% By replacing NewVar with A everywhere in the guard
		   %% expression, we get
		   %%
		   %%   case A of NewVar when A =:= A -> ...
		   %%
		   %% which by constant-expression evaluation is reduced to
		   %%
		   %%   case A of NewVar when true -> ...
		   %%
		   sub_set_var(Var, Cexpr, Sub2);
	       _ ->
		   Sub2
	   end,
    G1 = guard(G0, GSub),
    B1 = body(B0, Ctxt, Sub2),
    Cl#c_clause{pats=Ps1,guard=G1,body=B1}.

%% let_substs(LetVars, LetArg, Sub) -> {[Var],[Val],Sub}.
%%  Add suitable substitutions to Sub of variables in LetVars.  First
%%  remove variables in LetVars from Sub, then fix subs.  N.B. must
%%  work out new subs in parallel and then apply them to subs.  Return
%%  the unsubstituted variables and values.

let_substs(Vs0, As0, Sub0) ->
    {Vs1,Sub1} = pattern_list(Vs0, Sub0),
    {Vs2,As1,Ss} = let_substs_1(Vs1, As0, Sub1),
    Sub2 = scope_add([V || #c_var{name=V} <- Vs2], Sub1),
    {Vs2,As1,
     foldl(fun ({V,S}, Sub) -> sub_set_name(V, S, Sub) end, Sub2, Ss)}.

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
	    V1 = make_var_name(),
	    Pat1 = #c_var{name=V1},
	    {Pat1,sub_set_var(Pat, Pat1, scope_add([V1], Osub))};
	false ->
	    {Pat,sub_del_var(Pat, scope_add([V0], Osub))}
    end;
pattern(#c_literal{}=Pat, _, Osub) -> {Pat,Osub};
pattern(#c_cons{hd=H0,tl=T0}=Pat, Isub, Osub0) ->
    {H1,Osub1} = pattern(H0, Isub, Osub0),
    {T1,Osub2} = pattern(T0, Isub, Osub1),
    {Pat#c_cons{hd=H1,tl=T1},Osub2};
pattern(#c_tuple{es=Es0}=Pat, Isub, Osub0) ->
    {Es1,Osub1} = pattern_list(Es0, Isub, Osub0),
    {Pat#c_tuple{es=Es1},Osub1};
pattern(#c_binary{segments=V0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = bin_pattern_list(V0, Isub, Osub0),
    {Pat#c_binary{segments=V1},Osub1};
pattern(#c_alias{var=V0,pat=P0}=Pat, Isub, Osub0) ->
    {V1,Osub1} = pattern(V0, Isub, Osub0),
    {P1,Osub2} = pattern(P0, Isub, Osub1),
    {Pat#c_alias{var=V1,pat=P1},Osub2}.

bin_pattern_list(Ps0, Isub, Osub0) ->
    {Ps,{_,Osub}} = mapfoldl(fun bin_pattern/2, {Isub,Osub0}, Ps0),
    {Ps,Osub}.

bin_pattern(#c_bitstr{val=E0,size=Size0}=Pat, {Isub0,Osub0}) ->
    Size1 = expr(Size0, Isub0),
    {E1,Osub} = pattern(E0, Isub0, Osub0),
    Isub = case E0 of
	       #c_var{} -> sub_set_var(E0, E1, Isub0);
	       _ -> Isub0
	   end,
    {Pat#c_bitstr{val=E1,size=Size1},{Isub,Osub}}.

pattern_list(Ps, Sub) -> pattern_list(Ps, Sub, Sub).

pattern_list(Ps0, Isub, Osub0) ->
    mapfoldl(fun (P, Osub) -> pattern(P, Isub, Osub) end, Osub0, Ps0).

%% is_subst(Expr) -> true | false.
%%  Test whether an expression is a suitable substitution.

is_subst(#c_var{}) -> true;
is_subst(#c_literal{}) -> true;
is_subst(_) -> false.

%% sub_new() -> #sub{}.
%% sub_get_var(Var, #sub{}) -> Value.
%% sub_set_var(Var, Value, #sub{}) -> #sub{}.
%% sub_set_name(Name, Value, #sub{}) -> #sub{}.
%% sub_del_var(Var, #sub{}) -> #sub{}.
%% sub_subst_var(Var, Value, #sub{}) -> [{Name,Value}].
%% sub_is_val(Var, #sub{}) -> bool().
%% sub_subst_scope(#sub{}) -> #sub{}
%%
%%  We use the variable name as key so as not have problems with
%%  annotations.  When adding a new substitute we fold substitute
%%  chains so we never have to search more than once.  Use orddict so
%%  we know the format.
%%
%%  sub_subst_scope/1 adds dummy substitutions for all variables
%%  in the scope in order to force renaming if variables in the
%%  scope occurs as pattern variables.

sub_new() -> #sub{v=orddict:new(),s=gb_trees:empty(),t=[]}.

%%sub_new(#sub{}=Sub) -> Sub#sub{v=orddict:new()}.
sub_new(#sub{}=Sub) -> Sub#sub{v=orddict:new(),t=[]}.

sub_get_var(#c_var{name=V}=Var, #sub{v=S}) ->
    case orddict:find(V, S) of
	{ok,Val} -> Val;
	error -> Var
    end.

sub_set_var(#c_var{name=V}, Val, Sub) ->
    sub_set_name(V, Val, Sub).

sub_set_name(V, Val, #sub{v=S,s=Scope,t=Tdb}=Sub) ->
    Sub#sub{v=orddict:store(V, Val, S),s=gb_sets:add(V, Scope),
	    t=kill_types(V, Tdb)}.

sub_del_var(#c_var{name=V}, #sub{v=S,t=Tdb}=Sub) ->
    Sub#sub{v=orddict:erase(V, S),t=kill_types(V, Tdb)}.

sub_subst_var(#c_var{name=V}, Val, #sub{v=S0}) ->
    %% Fold chained substitutions.
    [{V,Val}] ++ [ {K,Val} || {K,#c_var{name=V1}} <- S0, V1 =:= V].

sub_subst_scope(#sub{v=S0,s=Scope}=Sub) ->
    S = [{-1,#c_var{name=Sv}} || Sv <- gb_sets:to_list(Scope)]++S0,
    Sub#sub{v=S}.

sub_is_val(#c_var{name=V}, #sub{v=S}) ->
    v_is_value(V, S).

v_is_value(Var, Sub) ->
    any(fun ({_,#c_var{name=Val}}) when Val =:= Var -> true;
	    (_) -> false
	end, Sub).

%% clauses(E, [Clause], TopLevel, Context, Sub) -> [Clause].
%%  Trim the clauses by removing all clauses AFTER the first one which
%%  is guaranteed to match.  Also remove all trivially false clauses.

clauses(E, Cs0, TopLevel, Ctxt, Sub) ->
    Cs = clauses_1(E, Cs0, Ctxt, Sub),

    %% Here we want to warn if no clauses whatsoever will ever
    %% match, because that is probably a mistake.
    case all(fun is_compiler_generated/1, Cs) andalso
	any(fun(C) -> not is_compiler_generated(C) end, Cs0) of
	true ->
	    %% The original list of clauses did contain at least one
	    %% user-specified clause, but none of them will match.
	    %% That is probably a mistake.
	    add_warning(TopLevel, no_clause_match);
	false ->
	    %% Either there were user-specified clauses left in
	    %% the transformed clauses, or else none of the original
	    %% clauses were user-specified to begin with (as in 'andalso').
	    ok
    end,

    Cs.

clauses_1(E, [C0|Cs], Ctxt, Sub) ->
    #c_clause{pats=Ps,guard=G} = C1 = clause(C0, E, Ctxt, Sub),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{E,Ps}]),
    case {will_match(E, Ps),will_succeed(G)} of
	{yes,yes} ->
	    Line = get_line(core_lib:get_anno(C1)),
	    case core_lib:is_literal(E) of
		false ->
		    shadow_warning(Cs, Line);
		true ->
		    %% If the case expression is a literal,
		    %% it is probably OK that some clauses don't match.
		    %% It is a proably some sort of debug macro.
		    ok
	    end,
	    [C1];				%Skip the rest
	{no,_Suc} ->
	    clauses_1(E, Cs, Ctxt, Sub);	%Skip this clause
	{_Mat,no} ->
	    add_warning(C1, nomatch_guard),
	    clauses_1(E, Cs, Ctxt, Sub);	%Skip this clause
	{_Mat,_Suc} ->
	    [C1|clauses_1(E, Cs, Ctxt, Sub)]
    end;
clauses_1(_, [], _, _) -> [].

shadow_warning([C|Cs], none) ->
    add_warning(C, nomatch_shadow),
    shadow_warning(Cs, none);
shadow_warning([C|Cs], Line) ->
    add_warning(C, {nomatch_shadow, Line}),
    shadow_warning(Cs, Line);
shadow_warning([], _) -> ok.

%% will_succeed(Guard) -> yes | maybe | no.
%%  Test if we know whether a guard will succeed/fail or just don't
%%  know.  Be VERY conservative!

will_succeed(#c_literal{val=true}) -> yes;
will_succeed(#c_literal{val=false}) -> no;
will_succeed(_Guard) -> maybe.

%% will_match(Expr, [Pattern]) -> yes | maybe | no.
%%  Test if we know whether a match will succeed/fail or just don't
%%  know.  Be conservative.

will_match(#c_values{es=Es}, Ps) ->
    will_match_list(Es, Ps, yes);
will_match(E, [P]) ->
    will_match_1(E, P).

will_match_1(_E, #c_var{}) -> yes;		%Will always match
will_match_1(E, #c_alias{pat=P}) ->		%Pattern decides
    will_match_1(E, P);
will_match_1(#c_var{}, _P) -> maybe;
will_match_1(#c_tuple{es=Es}, #c_tuple{es=Ps}) ->
    will_match_list(Es, Ps, yes);
will_match_1(#c_literal{val=Lit}, P) ->
    will_match_lit(Lit, P);
will_match_1(_, _) -> maybe.

will_match_list([E|Es], [P|Ps], M) ->
    case will_match_1(E, P) of
	yes -> will_match_list(Es, Ps, M);
	maybe -> will_match_list(Es, Ps, maybe);
	no -> no
    end;
will_match_list([], [], M) -> M.

will_match_lit(Cons, #c_cons{hd=Hp,tl=Tp}) ->
    case Cons of
	[H|T] ->
	    case will_match_lit(H, Hp) of
		yes -> will_match_lit(T, Tp);
		Other -> Other
	    end;
	_ ->
	    no
    end;
will_match_lit(Tuple, #c_tuple{es=Es}) ->
    case is_tuple(Tuple) andalso tuple_size(Tuple) =:= length(Es) of
	true -> will_match_lit_list(tuple_to_list(Tuple), Es);
	false -> no
    end;
will_match_lit(Bin, #c_binary{}) ->
    case is_bitstring(Bin) of
	true -> maybe;
	false -> no
    end;
will_match_lit(_, #c_var{}) ->
    yes;
will_match_lit(Lit, #c_alias{pat=P}) ->
    will_match_lit(Lit, P);
will_match_lit(Lit1, #c_literal{val=Lit2}) ->
    case Lit1 =:= Lit2 of
	true -> yes;
	false -> no
    end;
will_match_lit(_, _) -> maybe.

will_match_lit_list([H|T], [P|Ps]) ->
    case will_match_lit(H, P) of
	yes -> will_match_lit_list(T, Ps);
	Other -> Other
    end;
will_match_lit_list([], []) -> yes.

%% simplify_case(Case) -> CoreExpr
%%  We attempt to simplify the case expression and clauses in several different
%%  ways:
%%
%%  (1) Relational operators in cases can be moved to the guard.
%%
%%    case A > B of	 			case <> of
%%      true -> TrueClause;    	       ==>        <> when A > B -> TrueClause;
%%      false -> FalseClause			  <> when true -> FalseClause
%%    end.		 			end
%%
%%  Generally, evaluting a type test or comparision operator in a guard should
%%  be faster than evaulating it in the body.
%%
%%  But if the sole purpose of the guard is to verify that the relational
%%  expression indeed returns true or false, we keep the expression and remove
%%  case:
%%
%%    case A =:= B of
%%      true -> true   	       	       ==>      A =: B
%%      false -> false
%%    end.
%%
%%  (2) We try to eliminate the 'not' operator from a case expression.
%%
%%       case not CaseExpr of                  case CaseExpr of
%%          true -> TrueBody;         =>          false -> TrueBody;
%%          false -> FalseBody			  true -> FalseBody;
%%       end					  _ -> erlang:error(badarg)
%%

simplify_case(#c_case{arg=Call}=Case) ->
    case Call of
	#c_call{module=#c_literal{val=erlang},
		name=#c_literal{val=Name},
		args=Args} ->
	    simplify_case_0(Name, Args, Call, Case);
	_ -> Case
    end.

simplify_case_0('not', [Expr], _, Case) ->
    simplify_case_not(Expr, Case);
simplify_case_0(Name, Args, Call, Case) ->
    Arity = length(Args),

    %% If the call is a comparision operator or type test (which can't fail),
    %% and the arguments are safe, move this call to the guard.
    %% Note: v3_core never generates unsafe arguments, but another
    %% code generator might.
    case (erl_internal:comp_op(Name, Arity) orelse
	erl_internal:new_type_test(Name, Arity)) andalso
	is_safe_simple_list(Args) of
	false -> Case;
	true -> simplify_case_1(Call, Case)
    end.

simplify_case_1(Call, #c_case{clauses=[_,_|Cs]=Cs0}=Case) ->
    case is_bool_case(Cs0) of
	true ->
	    %% The case is not needed. Warn if there
	    %% are any (non-compiler generated) clauses following
	    %% the true and false clauses because they will
	    %% certainly not be reached.
	    shadow_warning(Cs, none),
	    Call;				%The case is not needed.
	false -> simplify_case_2(Call, Case)
    end;
simplify_case_1(_, Case) -> Case.

simplify_case_2(Call, #c_case{clauses=[A,B|Cs]}=Case) ->
    case {A,B} of
	{#c_clause{pats=[#c_literal{val=true}],guard=#c_literal{val=true}},
	 #c_clause{pats=[#c_literal{val=false}],guard=#c_literal{val=true}}} ->
	    True = A#c_clause{pats=[],guard=Call},
	    False = B#c_clause{pats=[]},
	    Case#c_case{arg=#c_values{anno=core_lib:get_anno(Call),es=[]},
			clauses=[True,False|Cs]};
	{#c_clause{pats=[#c_literal{val=false}],guard=#c_literal{val=true}},
	 #c_clause{pats=[#c_literal{val=true}],guard=#c_literal{val=true}}} ->
	    True = B#c_clause{pats=[],guard=Call},
	    False = A#c_clause{pats=[]},
	    Case#c_case{arg=#c_values{anno=core_lib:get_anno(Call),es=[]},
			clauses=[True,False|Cs]};
	_ -> Case
    end.

%% simplify_case_not(CaseExpr, Case) -> Case'
%%  Try to simplify away a 'not' in a case expression.

simplify_case_not(Expr, #c_case{clauses=Cs0}=Case) ->
    try
	Cs = simplify_case_not_1(Cs0, false, false),
	Case#c_case{arg=Expr,clauses=Cs}
    catch
	impossible -> Case
    end.

%% simplify_case_not_1([Clause], SeenTrue, SeenFalse) -> [Clause] | impossible
%%  Throws a 'impossible' exception if there are complications that make
%%  the optimization difficult or impossible.

simplify_case_not_1([#c_clause{pats=[#c_literal{val=Lit}],
			  guard=#c_literal{val=true}}=C0|Cs], SeenT, SeenF) ->
    case is_boolean(Lit) of
	false ->
	    %% Not a boolean - this clause can't match.
	    add_warning(C0, nomatch_clause_type),
	    simplify_case_not_1(Cs, SeenT, SeenF);
	true ->
	    %% A boolean - invert it and keep the clause.
	    C = C0#c_clause{pats=[#c_literal{val=not Lit}]},
	    case Lit of
		true -> [C|simplify_case_not_1(Cs, true, SeenF)];
		false -> [C|simplify_case_not_1(Cs, SeenT, true)]
	    end
    end;
simplify_case_not_1([_|_], true, true) ->
    %% We have seen clauses that handle both 'true' and 'false'.
    %% That means the expression is not a boolean, and the original
    %% call to 'not' would have generated a 'badarg'. Therefore,
    %% ignore this clause and all following and generate a default
    %% clause that generates a 'badarg' exception.

    simplify_case_not_1([], true, true);
simplify_case_not_1([], true, true) ->
    %% Generate a new default clause that generates a 'badarg' exception.
    [#c_clause{anno=[compiler_generated],
	       pats=[#c_var{name=cor_variable}],
	       guard=#c_literal{val=true},
	       body=#c_call{module=#c_literal{val=erlang},
			    name=#c_literal{val=error},
			    args=[#c_literal{val=badarg}]}}];
simplify_case_not_1(_, _, _) ->
    %% Some complication, for instance that not both of
    %% 'true' and 'false' were matched, or there were guards, and so on.
    throw(impossible).

%% eval_case(Case) -> #c_case{} | #c_let{}.
%%  If possible, evaluate a case at compile time.  We know that the
%%  last clause is guaranteed to match so if there is only one clause
%%  with a pattern containing only variables then rewrite to a let.

eval_case(#c_case{arg=#c_var{name=V},
		  clauses=[#c_clause{pats=[P],guard=G,body=B}|_]}=Case,
	  #sub{t=Tdb}=Sub) ->
    case orddict:find(V, Tdb) of
	{ok,Type} ->
	    case {will_match_type(P, Type),will_succeed(G)} of
		{yes,yes} ->
		    {Ps,Es} = remove_non_vars(P, Type),
		    expr(#c_let{vars=Ps,arg=#c_values{es=Es},body=B}, sub_new(Sub));
		{_,_} -> eval_case_1(Case, Sub)
	    end;
	error -> eval_case_1(Case, Sub)
    end;
eval_case(Case, Sub) -> eval_case_1(Case, Sub).

eval_case_1(#c_case{arg=E,clauses=[#c_clause{pats=Ps,body=B}]}=Case, Sub) ->
    case is_var_pat(Ps) of
	true -> expr(#c_let{vars=Ps,arg=E,body=B}, sub_new(Sub));
	false -> eval_case_2(E, Ps, B, Case)
    end;
eval_case_1(Case, _) -> Case.

eval_case_2(E, [P], B, Case) ->
    %% Recall that there is only one clause and that it is guaranteed to match.
    %%   If E and P are literals, they must be the same literal and the body
    %% can be used directly as there are no variables that need to be bound.
    %%   Otherwise, P could be an alias meaning that two or more variables
    %% would be bound to E. We don't bother to optimize that case as it
    %% is rather uncommon.
    case core_lib:is_literal(E) andalso core_lib:is_literal(P) of
	false -> Case;
	true -> B
    end;
eval_case_2(_, _, _, Case) -> Case.

is_var_pat(Ps) ->
    all(fun (#c_var{}) -> true;
	    (_Pat) -> false
	end, Ps).

will_match_type(#c_tuple{es=Es}, #c_tuple{es=Ps}) ->
    will_match_list_type(Es, Ps);
will_match_type(#c_literal{val=Atom}, #c_literal{val=Atom}) -> yes;
will_match_type(#c_var{}, _) -> yes;
will_match_type(_, _) -> no.

will_match_list_type([E|Es], [P|Ps]) ->
    case will_match_type(E, P) of
	yes -> will_match_list_type(Es, Ps);
	no -> no
    end;
will_match_list_type([], []) -> yes;
will_match_list_type(_, _) -> no.		%Different length

remove_non_vars(Ps0, Es0) ->
    {Ps,Es} = remove_non_vars(Ps0, Es0, [], []),
    {reverse(Ps),reverse(Es)}.

remove_non_vars(#c_tuple{es=Ps}, #c_tuple{es=Es}, Pacc, Eacc) ->
    remove_non_vars_list(Ps, Es, Pacc, Eacc);
remove_non_vars(#c_var{}=Var, #c_alias{var=Evar}, Pacc, Eacc) ->
    {[Var|Pacc],[Evar|Eacc]};
remove_non_vars(#c_var{}=Var, E, Pacc, Eacc) ->
    {[Var|Pacc],[E|Eacc]};
remove_non_vars(P, E, Pacc, Eacc) ->
    true = core_lib:is_literal(P) andalso core_lib:is_literal(E), %Assertion.
    {Pacc,Eacc}.

remove_non_vars_list([P|Ps], [E|Es], Pacc0, Eacc0) ->
    {Pacc,Eacc} = remove_non_vars(P, E, Pacc0, Eacc0),
    remove_non_vars_list(Ps, Es, Pacc, Eacc);
remove_non_vars_list([], [], Pacc, Eacc) ->
    {Pacc,Eacc}.

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
	    add_warning(C, nomatch_clause_type),
	    case_opt_cs(Cs, Arity)
    end;
case_opt_cs([], _) -> [].

letify_guard(Flet, Avs, #c_call{module=#c_literal{val=erlang},
			       name=#c_literal{val='not'},
			       args=[A]}=Call) ->
    Arg = letify_guard(Flet, Avs, A),
    Call#c_call{args=[Arg]};
letify_guard(Flet, Avs, #c_call{module=#c_literal{val=erlang},
			       name=#c_literal{val='and'},
			       args=[A1,A2]}=Call) ->
    Arg1 = letify_guard(Flet, Avs, A1),
    Arg2 = letify_guard(Flet, Avs, A2),
    Call#c_call{args=[Arg1,Arg2]};
letify_guard(Flet, Avs, #c_call{module=#c_literal{val=erlang},
			       name=#c_literal{val='or'},
			       args=[A1,A2]}=Call) ->
    Arg1 = letify_guard(Flet, Avs, A1),
    Arg2 = letify_guard(Flet, Avs, A2),
    Call#c_call{args=[Arg1,Arg2]};
letify_guard(Flet, Avs, #c_try{arg=B,
			       vars=[#c_var{name=X}],body=#c_var{name=X},
			       handler=#c_literal{val=false}}=Prot) ->
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
    [make_var(A)|make_vars(A, I+1, Max)];
make_vars(_, _, _) -> [].

make_var(A) ->
    #c_var{anno=A,name=make_var_name()}.

make_var_name() ->
    N = get(new_var_num),
    put(new_var_num, N+1),
    list_to_atom("fol"++integer_to_list(N)).

letify(#c_var{name=Vname}=Var, Val, Body) ->
    case core_lib:is_var_used(Vname, Body) of
	true ->
	    A = element(2, Body),
	    #c_let{anno=A,vars=[Var],arg=Val,body=Body};
	false -> Body
    end.

%% opt_case_in_let(LetExpr) -> LetExpr'

opt_case_in_let(#c_let{vars=Vs,arg=Arg,body=B}=Let) ->
    opt_case_in_let_0(Vs, Arg, B, Let).

opt_case_in_let_0([#c_var{name=V}], Arg,
		  #c_case{arg=#c_var{name=V},clauses=Cs}=Case, Let) ->
    case opt_case_in_let_1(V, Arg, Cs) of
	impossible ->
	    case is_simple_case_arg(Arg) andalso 
		not core_lib:is_var_used(V, Case#c_case{arg=#c_literal{val=nil}}) of
		true ->
		    simplify_case(Case#c_case{arg=Arg});
		false ->
		    Let
	    end;
	Expr -> Expr
    end;
opt_case_in_let_0(_, _, _, Let) -> Let.

opt_case_in_let_1(V, Arg, Cs) ->
    try
	opt_case_in_let_2(V, Arg, Cs)
    catch
	_:_ -> impossible
    end.

opt_case_in_let_2(V, Arg0,
		  [#c_clause{pats=[#c_tuple{es=Es}],
			     guard=#c_literal{val=true},body=B}|_]) ->

    %%  In {V1,V2,...} = case E of P -> ... {Val1,Val2,...}; ... end.
    %%  avoid building tuples, by converting tuples to multiple values.
    %%  (The optimisation is not done if the built tuple is used or returned.)

    true = all(fun (#c_var{}) -> true;
		   (_) -> false end, Es),	%Only variables in tuple
    false = core_lib:is_var_used(V, B),		%Built tuple must not be used.
    Arg1 = tuple_to_values(Arg0, length(Es)),	%Might fail.
    #c_let{vars=Es,arg=Arg1,body=B};
opt_case_in_let_2(_, Arg, Cs) ->
    %% simplify_bool_case(Case0) -> Case
    %%  Remove unecessary cases like
    %%
    %%     case BoolExpr of
    %%       true -> true;
    %%       false -> false;
    %%       ....
    %%     end
    %%
    %%  where BoolExpr is an expression that can only return true
    %%  or false (or throw an exception).

    true = is_bool_case(Cs) andalso is_bool_expr(Arg),
    Arg.

is_bool_case([A,B|_]) ->
    (is_bool_clause(true, A) andalso is_bool_clause(false, B))
	orelse (is_bool_clause(false, A) andalso is_bool_clause(true, B)).

is_bool_clause(Bool, #c_clause{pats=[#c_literal{val=Bool}],
			       guard=#c_literal{val=true},
			       body=#c_literal{val=Bool}}) ->
    true;
is_bool_clause(_, _) -> false.

%% is_simple_case_arg(Expr) -> true|false
%%  Determine whether the Expr is simple enough to be worth
%%  substituting into a case argument. (Common substitutions
%%  of variables and literals are assumed to have been already
%%  handled by the caller.)

is_simple_case_arg(#c_cons{}) -> true;
is_simple_case_arg(#c_tuple{}) -> true;
is_simple_case_arg(#c_call{}) -> true;
is_simple_case_arg(#c_apply{}) -> true;
is_simple_case_arg(_) -> false.

%% is_bool_expr(Core) -> true|false
%%  Check whether the Core expression only can return a boolean
%%  (or throw an exception).

is_bool_expr(#c_call{module=#c_literal{val=erlang},
		     name=#c_literal{val=Name},args=Args}=Call) ->
    NumArgs = length(Args),
    erl_internal:comp_op(Name, NumArgs) orelse
	erl_internal:new_type_test(Name, NumArgs) orelse
	will_fail(Call);
is_bool_expr(#c_case{clauses=Cs}) ->
    is_bool_expr_list(Cs);
is_bool_expr(#c_clause{body=B}) ->
    is_bool_expr(B);
is_bool_expr(#c_let{body=B}) ->
    is_bool_expr(B);
is_bool_expr(#c_literal{val=false}) ->
    true;
is_bool_expr(#c_literal{val=true}) ->
    true;
is_bool_expr(_) -> false.

is_bool_expr_list([C|Cs]) ->
    is_bool_expr(C) andalso is_bool_expr_list(Cs);
is_bool_expr_list([]) -> true.

%% tuple_to_values(Expr, TupleArity) -> Expr'
%%  Convert tuples in return position of arity TupleArity to values.
%%  Throws an exception for constructs that are not handled.

tuple_to_values(#c_tuple{es=Es}, Arity) when length(Es) =:= Arity ->
    core_lib:make_values(Es);
tuple_to_values(#c_literal{val=Tuple}=Lit, Arity) when tuple_size(Tuple) =:= Arity ->
    Es = [Lit#c_literal{val=E} || E <- tuple_to_list(Tuple)],
    core_lib:make_values(Es);
tuple_to_values(#c_case{clauses=Cs0}=Case, Arity) ->
    Cs1 = map(fun(E) -> tuple_to_values(E, Arity) end, Cs0),
    Case#c_case{clauses=Cs1};
tuple_to_values(#c_seq{body=B0}=Seq, Arity) ->
    Seq#c_seq{body=tuple_to_values(B0, Arity)};
tuple_to_values(#c_let{body=B0}=Let, Arity) ->
    Let#c_let{body=tuple_to_values(B0, Arity)};
tuple_to_values(#c_receive{clauses=Cs0,timeout=Timeout,action=A0}=Rec, Arity) ->
    Cs = map(fun(E) -> tuple_to_values(E, Arity) end, Cs0),
    A = case Timeout of
	    #c_literal{val=infinity} -> A0;
	    _ -> tuple_to_values(A0, Arity)
	end,
    Rec#c_receive{clauses=Cs,action=A};
tuple_to_values(#c_clause{body=B0}=Clause, Arity) ->
    B = tuple_to_values(B0, Arity),
    Clause#c_clause{body=B};
tuple_to_values(Expr, _) ->
    case will_fail(Expr) of
	true -> Expr;
	false -> erlang:error({not_handled,Expr})
    end.

%% simplify_let(Let, Sub) -> Expr | impossible
%%  If the argument part of an let contains a complex expression, such
%%  as a let or a sequence, move the original let body into the complex
%%  expression.

simplify_let(#c_let{arg=Arg}=Let, Sub) ->
    move_let_into_expr(Let, Arg, Sub).

move_let_into_expr(#c_let{vars=InnerVs0,body=InnerBody0}=Inner,
		   #c_let{vars=OuterVs0,arg=Arg0,body=OuterBody0}=Outer, Sub0) ->
    %%
    %% let <InnerVars> = let <OuterVars> = <Arg>
    %%                   in <OuterBody>
    %% in <InnerBody>
    %%
    %%       ==>
    %%
    %% let <OuterVars> = <Arg>
    %% in let <InnerVars> = <OuterBody>
    %%    in <InnerBody>
    %%
    Arg = body(Arg0, Sub0),
    ScopeSub0 = sub_subst_scope(Sub0#sub{t=[]}),
    {OuterVs,ScopeSub} = pattern_list(OuterVs0, ScopeSub0),
    OuterBody = body(OuterBody0, ScopeSub),

    {InnerVs,Sub} = pattern_list(InnerVs0, Sub0),
    InnerBody = body(InnerBody0, Sub),
    Outer#c_let{vars=OuterVs,arg=Arg,
		body=Inner#c_let{vars=InnerVs,arg=OuterBody,body=InnerBody}};
move_let_into_expr(#c_let{vars=Lvs0,body=Lbody0}=Let,
		   #c_case{arg=Cexpr0,clauses=[Ca0,Cb0|Cs]}=Case, Sub0) ->
    %% Test if there are no more clauses than Ca0 and Cb0, or if
    %% Cb0 is guaranted to match.
    TwoClauses = Cs =:= [] orelse
	case Cb0 of
	    #c_clause{pats=[#c_var{}],guard=#c_literal{val=true}} -> true;
	    _ -> false
	end,
    case {TwoClauses,is_failing_clause(Ca0),is_failing_clause(Cb0)} of
	{true,false,true} ->
	    %% let <Lvars> = case <Case-expr> of
	    %%                  <Cvars> -> <Clause-body>;
	    %%                  <OtherCvars> -> erlang:error(...)
	    %%               end
	    %% in <Let-body>
	    %%
	    %%     ==>
	    %%
	    %% case <Case-expr> of
	    %%   <Cvars> ->
	    %%       let <Lvars> = <Clause-body>
	    %%       in <Let-body>;
	    %%   <OtherCvars> -> erlang:error(...)
	    %% end

	    Cexpr = body(Cexpr0, Sub0),
	    CaVars0 = Ca0#c_clause.pats,
	    G0 = Ca0#c_clause.guard,
	    B0 = Ca0#c_clause.body,
	    ScopeSub0 = sub_subst_scope(Sub0#sub{t=[]}),
	    {CaVars,ScopeSub} = pattern_list(CaVars0, ScopeSub0),
	    G = guard(G0, ScopeSub),
	    B1 = body(B0, ScopeSub),

	    {Lvs,B2,Sub1} = let_substs(Lvs0, B1, Sub0),
	    Lbody = body(Lbody0, Sub1),
	    B = Let#c_let{vars=Lvs,arg=core_lib:make_values(B2),body=Lbody},

	    Ca = Ca0#c_clause{pats=CaVars,guard=G,body=B},
	    Cb = clause(Cb0, Cexpr, value, Sub0),
	    Case#c_case{arg=Cexpr,clauses=[Ca,Cb]};
	{_,_,_} -> impossible
    end;
move_let_into_expr(_Let, _Expr, _Sub) -> impossible.

is_failing_clause(#c_clause{body=B}) ->
    will_fail(B).

scope_add(Vs, #sub{s=Scope0}=Sub) ->
    Scope = foldl(fun(V, S) when is_integer(V); is_atom(V) ->
			  gb_sets:add(V, S)
		  end, Scope0, Vs),
    Sub#sub{s=Scope}.

%% opt_simple_let(#c_let{}, Context, Sub) -> CoreTerm
%%  Optimize a let construct that does not contain any lets in
%%  in its argument.

opt_simple_let(#c_let{arg=Arg0}=Let, Ctxt, Sub0) ->
    Arg = body(Arg0, value, Sub0),		%This is a body
    case will_fail(Arg) of
	true -> Arg;
	false -> opt_simple_let_1(Let, Arg, Ctxt, Sub0)
    end.

opt_simple_let_1(#c_let{vars=Vs0,body=B0}=Let, Arg0, Ctxt, Sub0) ->
    %% Optimise let and add new substitutions.
    {Vs,Args,Sub1} = let_substs(Vs0, Arg0, Sub0),
    B = body(B0, Ctxt, Sub1),
    Arg = core_lib:make_values(Args),
    opt_simple_let_2(Let, Vs, Arg, B, Ctxt, Sub1).

opt_simple_let_2(Let0, Vs0, Arg0, Body0, effect, Sub) ->
    case {Vs0,Arg0,Body0} of
	{[],#c_values{es=[]},Body} ->
	    %% No variables left (because of substitutions).
	    Body;
	{[_|_],Arg,#c_literal{}} ->
	    %% The body is a literal. That means that we can ignore
	    %% it and that the return value is Arg revisited in
	    %% effect context.
	    body(Arg, effect, sub_new(Sub));
	{Vs,Arg,Body} ->
	    %% Since we are in effect context, there is a chance
	    %% that the body no longer references the variables.
	    %% In that case we can construct a sequence and visit
	    %% that in effect context:
	    %%   let <Var> = Arg in BodyWithoutVar  ==> seq Arg BodyWithoutVar
	    case is_any_var_used(Vs, Body) of
		false ->
		    expr(#c_seq{arg=Arg,body=Body}, effect, sub_new(Sub));
		true ->
		    Let = Let0#c_let{vars=Vs,arg=Arg,body=Body},
		    opt_case_in_let(Let)
	    end
    end;
opt_simple_let_2(Let, Vs0, Arg0, Body0, value, _Sub) ->
    case {Vs0,Arg0,Body0} of
	{[#c_var{name=Vname}],Arg,#c_var{name=Vname}} ->
	    %% let <Var> = Arg in <Var>  ==>  Arg
	    Arg;
	{[],#c_values{es=[]},Body} ->
	    %% No variables left.
	    Body;
	{Vs,Arg,Body} ->
	    opt_case_in_let(Let#c_let{vars=Vs,arg=Arg,body=Body})
    end.

is_any_var_used([#c_var{name=V}|Vs], Expr) ->
    case core_lib:is_var_used(V, Expr) of
	false -> is_any_var_used(Vs, Expr);
	true -> true
    end;
is_any_var_used([], _) -> false.

%% update_types(Expr, Pattern, Sub) -> Sub'
%%  Update the type database.
update_types(Expr, Pat, #sub{t=Tdb0}=Sub) ->
    Tdb = update_types_1(Expr, Pat, Tdb0),
    Sub#sub{t=Tdb}.

update_types_1(#c_var{name=V,anno=Anno}, Pat, Types) ->
    case member(reuse_for_context, Anno) of
	true ->
	    %% If a variable has been marked for reuse of binary context,
	    %% optimizations based on type information are unsafe.
	    kill_types(V, Types);
	false ->
	    case Pat of
		[#c_tuple{}=P] -> orddict:store(V, P, Types);
		_ -> Types
	    end
    end;
update_types_1(_, _, Types) -> Types.

%% kill_types(V, Tdb) -> Tdb'
%%  Kill any entries that references the variable,
%%  either in the key or in the value.
kill_types(V, [{V,_}|Tdb]) ->
    kill_types(V, Tdb);
kill_types(V, [{_,#c_tuple{}=Tuple}=Entry|Tdb]) ->
    case core_lib:is_var_used(V, Tuple) of
	false -> [Entry|kill_types(V, Tdb)];
	true -> kill_types(V, Tdb)
    end;
kill_types(_, []) -> [].


%% The atom `ok', is widely used in Erlang for "void" values.

void() -> #c_literal{val=ok}.

%%%
%%% Annotate bit syntax matching to faciliate optimization in further passes.
%%%

bsm_an(#c_case{arg=#c_var{}=V}=Case) ->
    bsm_an_1([V], Case);
bsm_an(#c_case{arg=#c_values{es=Es}}=Case) ->
    bsm_an_1(Es, Case);
bsm_an(Other) -> Other.

bsm_an_1(Vs, #c_case{clauses=Cs}=Case) ->
    case bsm_leftmost(Cs) of
	none -> Case;
	Pos -> bsm_an_2(Vs, Cs, Case, Pos)
    end.

bsm_an_2(Vs, Cs, Case, Pos) ->
    try
	bsm_ensure_no_partition(Cs, Pos),
	bsm_do_an(Vs, Pos, Cs, Case)
    catch
	throw:{problem,Where,What} ->
	    add_bin_opt_info(Where, What),
	    Case
    end.

bsm_do_an(Vs0, Pos, Cs0, Case) ->
    case nth(Pos, Vs0) of
	#c_var{name=Vname}=V0 ->
	    Cs = bsm_do_an_var(Vname, Pos, Cs0, []),
	    V = bsm_annotate_for_reuse(V0),
	    Bef = lists:sublist(Vs0, Pos-1),
	    Aft = lists:nthtail(Pos, Vs0),
	    case Bef ++ [V|Aft] of
		[_] ->
		    Case#c_case{arg=V,clauses=Cs};
		Vs ->
		    Case#c_case{arg=#c_values{es=Vs},clauses=Cs}
	    end;
	_ ->
	    Case
    end.

bsm_do_an_var(V, S, [#c_clause{pats=Ps,guard=G,body=B0}=C0|Cs], Acc) ->
    case nth(S, Ps) of
	#c_var{name=VarName} ->
	    case core_lib:is_var_used(V, G) of
		true -> bsm_problem(C0, orig_bin_var_used_in_guard);
		false -> ok
	    end,
	    case core_lib:is_var_used(VarName, G) of
		true -> bsm_problem(C0, bin_var_used_in_guard);
		false -> ok
	    end,
	    B1 = bsm_maybe_ctx_to_binary(VarName, B0),
	    B = bsm_maybe_ctx_to_binary(V, B1),
	    C = C0#c_clause{body=B},
	    bsm_do_an_var(V, S, Cs, [C|Acc]);
	#c_alias{}=P ->
	    case bsm_could_match_binary(P) of
		false ->
		    bsm_do_an_var(V, S, Cs, [C0|Acc]);
		true ->
		    bsm_problem(C0, bin_opt_alias)
	    end;
	P ->
	    case bsm_could_match_binary(P) andalso bsm_is_var_used(V, G, B0) of
		false ->
		    bsm_do_an_var(V, S, Cs, [C0|Acc]);
		true ->
		    bsm_problem(C0, bin_var_used)
	    end
    end;
bsm_do_an_var(_, _, [], Acc) -> reverse(Acc).

bsm_annotate_for_reuse(#c_var{anno=Anno}=Var) ->
    case member(reuse_for_context, Anno) of
	false -> Var#c_var{anno=[reuse_for_context|Anno]};
	true -> Var
    end.

bsm_is_var_used(V, G, B) ->
    core_lib:is_var_used(V, G) orelse core_lib:is_var_used(V, B).

bsm_maybe_ctx_to_binary(V, B) ->
    case core_lib:is_var_used(V, B) andalso not previous_ctx_to_binary(V, B) of
	false ->
	    B;
	true ->
	    #c_seq{arg=#c_primop{name=#c_literal{val=bs_context_to_binary},
				 args=[#c_var{name=V}]},
		   body=B}
    end.

previous_ctx_to_binary(V, #c_seq{arg=#c_primop{name=Name,args=As}}) ->
    case {Name,As} of
	{#c_literal{val=bs_context_to_binary},[#c_var{name=V}]} ->
	    true;
	{_,_} ->
	    false
    end;
previous_ctx_to_binary(_, _) -> false.

%% bsm_leftmost(Cs) -> none | ArgumentNumber
%%  Find the leftmost argument that does binary matching. Return
%%  the number of the argument (1-N).

bsm_leftmost(Cs) ->
    bsm_leftmost_1(Cs, none).

bsm_leftmost_1([#c_clause{pats=Ps}|Cs], Pos) ->
    bsm_leftmost_2(Ps, Cs, 1, Pos);
bsm_leftmost_1([], Pos) -> Pos.

bsm_leftmost_2(_, Cs, Pos, Pos) ->
    bsm_leftmost_1(Cs, Pos);
bsm_leftmost_2([#c_binary{}|_], Cs, N, _) ->
    bsm_leftmost_1(Cs, N);
bsm_leftmost_2([_|Ps], Cs, N, Pos) ->
    bsm_leftmost_2(Ps, Cs, N+1, Pos);
bsm_leftmost_2([], Cs, _, Pos) ->
    bsm_leftmost_1(Cs, Pos).

%% bsm_ensure_no_partition(Cs, Pos) -> ok     (exception if problem)
%%  We must make sure that binary matching is not partitioned between
%%  variables like this:
%%             foo(<<...>>) -> ...
%%             foo(Var) when ... -> ...
%%             foo(<<...>>) ->
%%  If there is such partition, we are not allowed to reuse the binary variable
%%  for the match context. Also, arguments to the left of the argument that
%%  is matched against a binary, are only allowed to be simple variables, not
%%  used in guards. The reason is that we must know that the binary is only
%%  matched in one place.

bsm_ensure_no_partition(Cs, Pos) ->
    bsm_ensure_no_partition_1(Cs, Pos, before).

%% Loop through each clause.
bsm_ensure_no_partition_1([#c_clause{pats=Ps,guard=G}|Cs], Pos, State0) ->
    State = bsm_ensure_no_partition_2(Ps, Pos, G, simple_vars, State0),
    bsm_ensure_no_partition_1(Cs, Pos, State);
bsm_ensure_no_partition_1([], _, _) -> ok.

%% Loop through each pattern for this clause.
bsm_ensure_no_partition_2([#c_binary{}=Where|_], 1, _, Vstate, State) ->
    case State of
	before when Vstate =:= simple_vars -> within;
	before -> bsm_problem(Where, Vstate);
	within when Vstate =:= simple_vars -> within;
	within -> bsm_problem(Where, Vstate);
	'after' -> bsm_problem(Where, bin_partition)
    end;
bsm_ensure_no_partition_2([#c_alias{}=Alias|_], 1, N, Vstate, State) ->
    %% Retrieve the real pattern that the alias refers to and check that.
    P = bsm_real_pattern(Alias),
    bsm_ensure_no_partition_2([P], 1, N, Vstate, State);
bsm_ensure_no_partition_2([_|_], 1, _, _Vstate, before=State) ->
    %% No binary matching yet - therefore no partition.
    State;
bsm_ensure_no_partition_2([P|_], 1, _, Vstate, State) ->
    case bsm_could_match_binary(P) of
	false ->
	    %% If clauses can be freely arranged (Vstate =:= simple_vars),
	    %% a clause that cannot match a binary will not partition the clause.
	    %% Example:
	    %%
	    %% a(Var, <<>>) -> ...
	    %% a(Var, []) -> ...
	    %% a(Var, <<B>>) -> ...
	    %%
	    %% But if the clauses can't be freely rearranged, as in
	    %%
	    %% b(Var, <<>>) -> ...
	    %% b(1, 2) -> ...
	    %%
	    %% we do have a problem.
	    %%
	    case Vstate of
		simple_vars -> State;
		_ -> bsm_problem(P, Vstate)
	    end;
	true ->
	    %% The pattern P *may* match a binary, so we must update the state.
	    %% (P must be a variable.)
	    case State of
		within -> 'after';
		'after' -> 'after'
	    end
    end;
bsm_ensure_no_partition_2([#c_var{name=V}|Ps], N, G, Vstate, S) ->
    case core_lib:is_var_used(V, G) of
	false ->
	    bsm_ensure_no_partition_2(Ps, N-1, G, Vstate, S);
	true ->
	    bsm_ensure_no_partition_2(Ps, N-1, G, bin_left_var_used_in_guard, S)
    end;
bsm_ensure_no_partition_2([_|Ps], N, G, _, S) ->
    bsm_ensure_no_partition_2(Ps, N-1, G, bin_argument_order, S).

bsm_could_match_binary(#c_alias{pat=P}) -> bsm_could_match_binary(P);
bsm_could_match_binary(#c_cons{}) -> false;
bsm_could_match_binary(#c_tuple{}) -> false;
bsm_could_match_binary(#c_literal{val=Lit}) -> is_bitstring(Lit);
bsm_could_match_binary(_) -> true.

bsm_real_pattern(#c_alias{pat=P}) -> bsm_real_pattern(P);
bsm_real_pattern(P) -> P.

bsm_problem(Where, What) ->
    throw({problem,Where,What}).

%%%
%%% Handling of warnings.
%%%

init_warnings() ->
    put({?MODULE,warnings}, []).

add_bin_opt_info(Core, Term) ->
    case get(bin_opt_info) of
	true -> add_warning(Core, Term);
	false -> ok
    end.

add_warning(Core, Term) ->
    Anno = core_lib:get_anno(Core),
    case lists:member(compiler_generated, Anno) of
	true -> ok;
	false ->
	    case get_line(Anno) of
		Line when Line >= 0 ->		%Must be positive.
                    File = get_file(Anno),
		    Key = {?MODULE,warnings},
		    case get(Key) of
			[{File,[{Line,?MODULE,Term}]}|_] ->
			    ok;			%We already an identical warning.
			Ws ->
			    put(Key, [{File,[{Line,?MODULE,Term}]}|Ws])
		    end;
		_ -> ok				%Compiler-generated code.
	    end
    end.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

is_compiler_generated(Core) ->
    Anno = core_lib:get_anno(Core),
    case lists:member(compiler_generated, Anno) of
	true -> true;
	false ->
	    case get_line(Anno) of
		Line when Line >= 0 -> false;
		_ -> true
	    end
    end.

get_warnings() ->
    ordsets:from_list((erase({?MODULE,warnings}))).

format_error({eval_failure,Reason}) ->
    flatten(io_lib:format("this expression will fail with a '~p' exception", [Reason]));
format_error(embedded_binary_size) ->
    "binary construction will fail with a 'badarg' exception "
	"(field size for binary/bitstring greater than actual size)";
format_error({embedded_unit,Unit,Size}) ->
    M = io_lib:format("binary construction will fail with a 'badarg' exception "
		      "(size ~p cannot be evenly divided by unit ~p)", [Size,Unit]),
    flatten(M);
format_error({nomatch_shadow,Line}) ->
    M = io_lib:format("this clause cannot match because a previous clause at line ~p "
		      "always matches", [Line]),
    flatten(M);
format_error(nomatch_shadow) ->
    "this clause cannot match because a previous clause always matches";
format_error(nomatch_guard) ->
    "the guard for this clause evaluates to 'false'";
format_error(no_clause_match) ->
    "no clause will ever match";
format_error(nomatch_clause_type) ->
    "this clause cannot match because of different types/sizes";
format_error({no_effect,{erlang,F,A}}) ->
    {Fmt,Args} = case erl_internal:comp_op(F, A) of
		     true ->
			 {"use of operator ~p has no effect",[F]};
		     false ->
			 case erl_internal:bif(F, A) of
			     false ->
				 {"the call to erlang:~p/~p has no effect",[F,A]};
			     true ->
				 {"the call to ~p/~p has no effect",[F,A]}
			 end
		 end,
    flatten(io_lib:format(Fmt, Args));
format_error(result_ignored) ->
    "the result of the expression is ignored";
format_error(useless_building) ->
    "a term is constructed, but never used";
format_error(bin_opt_alias) ->
    "INFO: the '=' operator will prevent delayed sub binary optimization";
format_error(bin_partition) ->
    "INFO: non-consecutive clauses that match binaries "
	"will prevent delayed sub binary optimization";
format_error(bin_left_var_used_in_guard) ->
    "INFO: a variable to the left of the binary pattern is used in a guard; "
	"will prevent delayed sub binary optimization";
format_error(bin_argument_order) ->
    "INFO: matching anything else but a plain variable to the left of "
	"binary pattern will prevent delayed sub binary optimization; "
	"SUGGEST changing argument order";
format_error(bin_var_used) ->
    "INFO: using a matched out sub binary will prevent "
	"delayed sub binary optimization";
format_error(orig_bin_var_used_in_guard) ->
    "INFO: using the original binary variable in a guard will prevent "
	"delayed sub binary optimization";
format_error(bin_var_used_in_guard) ->
    "INFO: using a matched out sub binary in a guard will prevent "
	"delayed sub binary optimization".

-ifdef(DEBUG).
%% In order for simplify_let/2 to work correctly, the list of
%% in-scope variables must always be a superset of the free variables
%% in the current expression (otherwise we might fail to rename a variable
%% when needed and get a name capture bug).

verify_scope(E, #sub{s=Scope}) ->
    Free = core_lib:free_vars(E),
    case ordsets:is_subset(core_lib:free_vars(E), gb_sets:to_list(Scope)) of
	true -> true;
	false ->
	    io:format("~p\n", [E]),
	    io:format("~p\n", [Free]),
	    io:format("~p\n", [gb_sets:to_list(Scope)]),
	    false
    end.
-endif.
