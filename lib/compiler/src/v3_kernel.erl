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
%% Purpose : Transform Core Erlang to Kernel Erlang

%% Kernel erlang is like Core Erlang with a few significant
%% differences:
%%
%% 1. It is flat!  There are no nested calls or sub-blocks.
%%
%% 2. All variables are unique in a function.  There is no scoping, or
%% rather the scope is the whole function.
%%
%% 3. Pattern matching (in cases and receives) has been compiled.
%%
%% 4. The annotations contain variable usages.  Seeing we have to work
%% this out anyway for funs we might as well pass it on for free to
%% later passes.
%%
%% The translation is done in two passes:
%%
%% 1. Basic translation, flatten completely, pattern matching
%% compilation.
%%
%% 2. Fun-lifting (lambda-lifting), variable usage annotation and
%% last-call handling.
%%
%% All new Kexprs are created in the first pass, they are just
%% annotated in the second.
%%
%% Functions and BIFs
%%
%% Functions are "call"ed or "enter"ed if it is a last call, their
%% return values may be ignored.  BIFs are things which are known to
%% be internal by the compiler and can only be called, their return
%% values cannot be ignored.
%%
%% We are very explicit in allowing Core vectors only as multiple
%% values/patterns.
%%
%% To ensure unique variable names we use a variable substitution
%% table and keep the set of all defined variables.  The nested
%% scoping of Core means that we must also nest the substitution
%% tables, but the defined set must be passed through to match the
%% flat structure of Kernel and to make sure variables with the same
%% name from different scopes get different substitutions.
%%
%% We also use these substitutions to handle the variable renaming
%% necessary in pattern matching compilation.

-module(v3_kernel).

-export([module/2]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,splitwith/2]).
-import(ordsets, [add_element/2,del_element/2,union/2,subtract/2]).

-include("core_parse.hrl").
-include("v3_kernel.hrl").

%% Internal kernel expressions and help functions.

-record(ifun, {anno=[],vars,body}).
-record(iset, {anno=[],vars,arg,body}).
-record(ialias, {anno=[],vars,pat}).
-record(iclause, {anno=[],vsb,pats,guard,body}).
-record(ireceive_accept, {anno=[],arg}).
-record(ireceive_reject, {anno=[],arg}).
-record(ireceive_next, {anno=[],arg}).

get_kanno(Kthing) -> element(2, Kthing).

%% State record for kernel translator.
-record(kern, {func,				%Current function
	       vcount=0,			%Variable counter
	       fcount=0,			%Fun counter
	       ds=[],				%Defined variables
	       funs=[]}).			%Fun functions

module(#c_mdef{anno=A,name=M,exports=Es,attributes=As,body=Fs}, Options) ->
    {Kfs,St} = mapfoldl(fun function/2, #kern{}, Fs),
    {ok,#k_mdef{anno=A,name=M,exports=Es,attributes=As,
		body=Kfs ++ St#kern.funs}}.

function(#c_fdef{anno=Af,func=F,arity=Arity,body=Body}, St0) ->
    %%ok = io:fwrite("func ~w:~p~n", [?LINE,{F,Arity}]),
    St1 = St0#kern{func={F,Arity},vcount=0,fcount=0,ds=sets:new()},
    {#ifun{anno=Ab,vars=Kvs,body=B0},[],St2} = expr(Body, new_vsub(), St1),
    {B1,Bu,St3} = ubody(B0, return, St2),
    %%B1 = B0, St3 = St2,				%Null second pass
    {#k_fdef{anno=#k{us=[],ns=[],a=Af ++ Ab},
	     func=F,arity=Arity,vars=Kvs,body=B1},St3}.

%% body(Cexpr, Vsb, State) -> {Kexpr,[PreKepxr],State}.
%%  Do the main sequence of a body.  A body ends in an atomic value or
%%  values.  Must check if vector first so do expr.

body(#c_values{anno=A,es=Ces}, Vsb, St0) ->
    %% Do this here even if only in bodies.
    {Kes,Pe,St1} = atomic_list(Ces, Vsb, St0),
    {#k_break{anno=A,args=Kes},Pe,St1};
body(#ireceive_next{anno=A}, Vsb, St) ->
    {#k_receive_next{anno=A},[],St};
body(Ce, Vsb, St0) ->
    expr(Ce, Vsb, St0).

%% guard(Cexpr, Vsb, State) -> {Kexpr,[PreKexpr],State}.
%%  Do guard sequence.

guard(Ce, Vsb, St) -> body(Ce, Vsb, St).

%% expr(Cexpr, Vsb, State) -> {Kexpr,[PreKexpr],State}.
%%  Convert a Core expression, flattening it at the same time.

expr(#c_var{anno=A,name=V}, Vsb, St) ->
    {#k_var{anno=A,name=get_vsub(V, Vsb)},[],St};
expr(#c_int{anno=A,val=I}, Vsb, St) ->
    {#k_int{anno=A,val=I},[],St};
expr(#c_float{anno=A,val=F}, Vsb, St) ->
    {#k_float{anno=A,val=F},[],St};
expr(#c_atom{anno=A,name=At}, Vsb, St) ->
    {#k_atom{anno=A,name=At},[],St};
expr(#c_char{anno=A,val=C}, Vsb, St) ->
    {#k_char{anno=A,val=C},[],St};
expr(#c_string{anno=A,val=S}, Vsb, St) ->
    {#k_string{anno=A,val=S},[],St};
expr(#c_nil{anno=A}, Vsb, St) ->
    {#k_nil{anno=A},[],St};
expr(#c_cons{anno=A,head=Ch,tail=Ct}, Vsb, St0) ->
    %% Do cons in two steps, first the expressions left to right, then
    %% any remaining literals right to left.
    {Kh0,Hp0,St1} = expr(Ch, Vsb, St0),
    {Kt0,Tp0,St2} = expr(Ct, Vsb, St1),
    {Kt1,Tp1,St3} = force_atomic(Kt0, St2),
    {Kh1,Hp1,St4} = force_atomic(Kh0, St3),
    {#k_cons{anno=A,head=Kh1,tail=Kt1},Hp0 ++ Tp0 ++ Tp1 ++ Hp1,St4};
expr(#c_tuple{anno=A,es=Ces}, Vsb, St0) ->
    {Kes,Ep,St1} = atomic_list(Ces, Vsb, St0),
    {#k_tuple{anno=A,es=Kes},Ep,St1};
expr(#c_bin{anno=A,es=Cv}, Vsb, St0) ->
    {Kv,Ep,St1} = atomic_bin(Cv, Vsb, St0, 0),
    {#k_bin{anno=A,val=Kv},Ep,St1};
expr(#c_local{anno=A,name=F,arity=Ar}, Vsb, St0) ->
    %% A local in an expression.  Hacky id.
    {Vs,St1} = new_vars(Ar, St0),
    Fun = #c_fun{anno=[{id,erlang:hash(St0, (1 bsl 27)-1)}|A],
		 vars=Vs,
		 body=#c_call{op=#c_local{name=F,arity=Ar},args=Vs}},
    expr(Fun, Vsb, St1);
expr(#c_fun{anno=A,vars=Cvs,body=Cb}, Vsb0, St0) ->
    {Kvs,Vsb1,St1} = pattern_list(Cvs, Vsb0, St0),
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{{Cvs,Vsb0,St0},{Kvs,Vsb1,St1}}]),
    {Kb,Pb,St2} = body(Cb, Vsb1, St1),
    {#ifun{anno=A,vars=Kvs,body=pre_seq(Pb, Kb)},[],St2};
expr(#c_seq{anno=A,arg=Ca,body=Cb}, Vsb, St0) ->
    {Ka,Pa,St1} = body(Ca, Vsb, St0),
    case is_exit_expr(Ka) of
	true -> {Ka,Pa,St1};
	false ->
	    {Kb,Pb,St2} = body(Cb, Vsb, St1),
	    {Kb,Pa ++ [Ka] ++ Pb,St2}
    end;
expr(#c_let{anno=A,vars=Cp,arg=Ca,body=Cb}, Vsb0, St0) ->
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{Cp,Vsb0,St0}]),
    {Ka,Pa,St1} = body(Ca, Vsb0, St0),
    case is_exit_expr(Ka) of
	true -> {Ka,Pa,St1};
	false ->
	    {Kps,Vsb1,St2} = pattern_list(Cp, Vsb0, St1),
	    %%ok = io:fwrite("~w: ~p~n", [?LINE,{Kps,Vsb1,St1,St2}]),
	    %% Break known multiple values into separate sets.
	    Sets = case Ka of
		       #k_break{args=Kas}=Br ->
			   foldr2(fun (V, Val, Sb) ->
					  [#iset{vars=[V],arg=Val}|Sb] end,
				  [], Kps, Kas);
		       Other ->
			   [#iset{anno=A,vars=Kps,arg=Ka}]
		   end,
	    {Kb,Pb,St3} = body(Cb, Vsb1, St2),
	    {Kb,Pa ++ Sets ++ Pb,St3}
    end;
expr(#c_case{anno=A,arg=Ca,clauses=Ccs}, Vsb, St0) ->
    {Ka,Pa,St1} = body(Ca, Vsb, St0),		%This is a body!
    {Kvs,Pv,St2} = match_vars(Ka, St1),		%Must have variables here!
    {Km,St3} = kmatch(Kvs, Ccs, Vsb, St2),
    Match = flatten_seq(build_match(Kvs, Km)),
    {last(Match),Pa ++ Pv ++ first(Match),St3};
expr(#c_receive{anno=A,clauses=Ccs0,timeout=Ce,action=Ca}, Vsb, St0) ->
    {Ke,Pe,St1} = atomic(Ce, Vsb, St0),		%Force this to be atomic!
    {Rvar,St2} = new_var(St1),
    %% Need to massage accept clauses and add reject clause before matching.
    Ccs1 = map(fun (#c_clause{body=B0}=C) ->
		       B1 = #c_seq{arg=#ireceive_accept{},
				   body=B0},
		       C#c_clause{body=B1}
	       end, Ccs0),
    {Mpat,St3} = new_var_name(St2),
    Rc = #c_clause{pats=[#c_var{name=Mpat}],guard=#c_atom{name=true},
		   body=#c_seq{arg=#ireceive_reject{},
			       body=#ireceive_next{}}},
    {Km,St4} = kmatch([Rvar], Ccs1 ++ [Rc], Vsb, add_var_def(Rvar, St3)),
    {Ka,Pa,St5} = body(Ca, Vsb, St4),
    {#k_receive{anno=A,var=Rvar,body=Km,timeout=Ke,action=pre_seq(Pa, Ka)},
     Pe,St5};
expr(#c_call{anno=A,op=Cop,args=Cargs}, Vsb, St) ->
    call(A, Cop, Cargs, Vsb, St);
expr(#c_catch{anno=A,body=Cb}, Vsb, St0) ->
    {Kb,Pb,St1} = body(Cb, Vsb, St0),
    {#k_catch{anno=A,body=pre_seq(Pb, Kb)},[],St1};
%% Handle internal expressions.
expr(#ireceive_accept{anno=A}, Vsb, St) -> {#k_receive_accept{anno=A},[],St};
expr(#ireceive_reject{anno=A}, Vsb, St) -> {#k_receive_reject{anno=A},[],St}.

%% expr_list([Cexpr], Vsb, State) -> {[Kexpr],[PreKexpr],State}.

%%expr_list(Ces, Vsb, St) ->
%%    foldr(fun (Ce, {Kes,Esp,St0}) ->
%%		  {Ke,Ep,St1} = expr(Ce, Vsb, St0),
%%		  {[Ke|Kes],Ep ++ Esp,St1}
%%	  end, {[],[],St}, Ces).

%% match_vars(Kexpr, State) -> {[Kvar],[PreKexpr],State}.
%%  Force return from body into a list of variables.

match_vars(#k_break{args=As}, St) ->
    foldr(fun (Ka, {Vs,Vsp,St0}) ->
		  {V,Vp,St1} = force_variable(Ka, St0),
		  {[V|Vs],Vp ++ Vsp,St1}
	  end, {[],[],St}, As);
match_vars(Ka, St0) ->
    {V,Vp,St1} = force_variable(Ka, St0),
    {[V],Vp,St1}.

%% call(A, Op, [Carg], Vsb, State) -> {Kexpr,[PreKexpr],State}.
%%  Transform calls, detect which are guaranteed to be bifs.

call(A, #c_remote{anno=Ra,mod=M,name=F,arity=Ar}, Cargs, Vsb, St0) ->
    {Kargs,Ap,St1} = atomic_list(Cargs, Vsb, St0),
    Call = case is_remote_bif(M, F, Ar) of
	       true ->
		   #k_bif{anno=A,
			  op=#k_remote{anno=Ra,mod=M,name=F,arity=Ar},
			  args=Kargs};
	       false ->
		   #k_call{anno=A,
			   op=#k_remote{anno=Ra,mod=M,name=F,arity=Ar},
			   args=Kargs}
	   end,
    {Call,Ap,St1};
call(A, #c_local{anno=Ra,name=F,arity=Ar}, Cargs, Vsb, St0) ->
    {Kargs,Ap,St1} = atomic_list(Cargs, Vsb, St0),
    {#k_call{anno=A,op=#k_local{anno=Ra,name=F,arity=Ar},args=Kargs},
     Ap,St1};
call(A, #c_internal{anno=Ra,name=F,arity=Ar}, Cargs, Vsb, St0) ->
    {Kargs,Ap,St1} = atomic_list(Cargs, Vsb, St0),
    Call = case is_internal_bif(F, Ar) of
	       true ->
		   #k_bif{anno=A,
			  op=#k_internal{anno=Ra,name=F,arity=Ar},
			  args=Kargs};
	       false ->
		   #k_call{anno=A,
			   op=#k_internal{anno=Ra,name=F,arity=Ar},
			   args=Kargs}
	   end,
    {Call,Ap,St1};
call(A, Cop, Cargs, Vsb, St0) ->
    {Kop,Op,St1} = variable(Cop, Vsb, St0),
    {Kargs,Ap,St2} = atomic_list(Cargs, Vsb, St1),
    {#k_call{anno=A,op=Kop,args=Kargs},Op ++ Ap,St2}.

flatten_seq(#iset{anno=A,vars=Vs,arg=Arg,body=B}) ->
    [#iset{anno=A,vars=Vs,arg=Arg}|flatten_seq(B)];
flatten_seq(Ke) -> [Ke].

pre_seq([#iset{anno=A,vars=Vs,arg=Arg}|Ps], K) ->
    #iset{anno=A,vars=Vs,arg=Arg,body=pre_seq(Ps, K)};
pre_seq([P|Ps], K) ->
    #iset{vars=[],arg=P,body=pre_seq(Ps, K)};
pre_seq([], K) -> K.

%% atomic(Cexpr, Vsb, State) -> {Katomic,[PreKexpr],State}.
%%  Convert a Core expression making sure the result is an atomic
%%  literal.

atomic(Ce, Vsb, St0) ->
    {Ke,Kp,St1} = expr(Ce, Vsb, St0),
    {Ka,Ap,St2} = force_atomic(Ke, St1),
    {Ka,Kp ++ Ap,St2}.

force_atomic(Ke, St0) ->
    case is_atomic(Ke) of
	true -> {Ke,[],St0}; 
	false ->
	    {V,St1} = new_var(St0),
	    {V,[#iset{vars=[V],arg=Ke}],St1}
    end.

atomic_bin([], Vsb, St, Bits) -> {#k_zero_binary{},[],St};
atomic_bin([#c_bin_elem{anno=A,val=E0,size=Size0,type=Type}|Es0],
	   Vsb, St0, Bits0) ->
    Info0 = bit_type(Type),
    {Size,A1,St1} = atomic(Size0, Vsb, St0),
    {E,A2,St2} = atomic(E0, Vsb, St1),
    {Bits,Info} = aligned(Bits0, Size, Info0),
    {Es,A3,St3} = atomic_bin(Es0, Vsb, St2, Bits),
    {#k_binary_cons{anno=A,size=Size,info=Info,head=E,tail=Es},A1++A2++A3,St3}.

%% atomic_list([Cexpr], Vsb, State) -> {[Kexpr],[PreKexpr],State}.

atomic_list(Ces, Vsb, St) ->
    foldr(fun (Ce, {Kes,Esp,St0}) ->
		  {Ke,Ep,St1} = atomic(Ce, Vsb, St0),
		  {[Ke|Kes],Ep ++ Esp,St1}
	  end, {[],[],St}, Ces).

%% is_atomic(Kexpr) -> bool().
%%  Is a Kexpr atomic?  Strings are NOT considered atomic!

is_atomic(#k_int{}) -> true;
is_atomic(#k_float{}) -> true;
is_atomic(#k_atom{}) -> true;
is_atomic(#k_char{}) -> true;
%%is_atomic(#k_string{}) -> true;
is_atomic(#k_nil{}) -> true;
is_atomic(#k_var{}) -> true;
is_atomic(Other) -> false.

%% variable(Cexpr, Vsb, State) -> {Kvar,[PreKexpr],State}.
%%  Convert a Core expression making sure the result is a literal.

variable(Ce, Vsb, St0) ->
    {Ke,Kp,St1} = expr(Ce, Vsb, St0),
    {Kv,Vp,St2} = force_variable(Ke, St1),
    {Kv,Kp ++ Vp,St2}.

force_variable(#k_var{}=Ke, St) -> {Ke,[],St};
force_variable(Ke, St0) ->
    {V,St1} = new_var(St0),
    {V,[#iset{vars=[V],arg=Ke}],St1}.

%% pattern(Cpat, Vsb, State) -> {Kpat,Vsb,State}.
%%  Convert patterns.  Variables shadow so rename variables that are
%%  already defined.

pattern(#c_var{anno=A,name=V}, Vsb, St0) ->
    case sets:is_element(V, St0#kern.ds) of
	true ->
	    {New,St1} = new_var_name(St0),
	    {#k_var{anno=A,name=New},
	     set_vsub(V, New, Vsb),
	     St1#kern{ds=sets:add_element(New, St1#kern.ds)}};
	false ->
	    {#k_var{anno=A,name=V},Vsb,
	     St0#kern{ds=sets:add_element(V, St0#kern.ds)}}
    end;
pattern(#c_int{anno=A,val=I}, Vsb, St) ->
    {#k_int{anno=A,val=I},Vsb,St};
pattern(#c_float{anno=A,val=F}, Vsb, St) ->
    {#k_float{anno=A,val=F},Vsb,St};
pattern(#c_atom{anno=A,name=At}, Vsb, St) ->
    {#k_atom{anno=A,name=At},Vsb,St};
pattern(#c_char{anno=A,val=C}, Vsb, St) ->
    {#k_char{anno=A,val=C},Vsb,St};
pattern(#c_string{anno=A,val=S}, Vsb, St) ->
    L = foldr(fun (C, T) -> #k_cons{head=#k_int{val=C},tail=T} end,
	      #k_nil{}, S),
    {L,Vsb,St};
pattern(#c_nil{anno=A}, Vsb, St) ->
    {#k_nil{anno=A},Vsb,St};
pattern(#c_cons{anno=A,head=Ch,tail=Ct}, Vsb0, St0) ->
    {Kh,Vsb1,St1} = pattern(Ch, Vsb0, St0),
    {Kt,Vsb2,St2} = pattern(Ct, Vsb1, St1),
    {#k_cons{anno=A,head=Kh,tail=Kt},Vsb2,St2};
pattern(#c_tuple{anno=A,es=Ces}, Vsb0, St0) ->
    {Kes,Vsb1,St1} = pattern_list(Ces, Vsb0, St0),
    {#k_tuple{anno=A,es=Kes},Vsb1,St1};
pattern(#c_bin{anno=A,es=Cv}, Vsb0, St0) ->
    {Kv,Vsb1,St1} = pattern_bin(Cv, Vsb0, St0),
    {#k_bin{anno=A,val=Kv},Vsb1,St1};
pattern(#c_alias{anno=A,var=Cv,pat=Cp}, Vsb0, St0) ->
    {Cvs,Cpat} = flatten_alias(Cp),
    {Kvs,Vsb1,St1} = pattern_list([Cv|Cvs], Vsb0, St0),
    {Kpat,Vsb2,St2} = pattern(Cpat, Vsb1, St1),
    {#ialias{anno=A,vars=Kvs,pat=Kpat},Vsb2,St2}.

flatten_alias(#c_alias{var=V,pat=P}) ->
    {Vs,Pat} = flatten_alias(P),
    {[V|Vs],Pat};
flatten_alias(Pat) -> {[],Pat}.

pattern_bin(Es, Vsb, St) -> pattern_bin(Es, Vsb, St, 0).

pattern_bin([], Vsb, St, Bits) ->
    {#k_zero_binary{},Vsb,St};
pattern_bin([#c_bin_elem{anno=A,val=E0,size=Size0,type=Type}|Es0], 
	    Vsb0, St0, Bits0) ->
    Info0 = bit_type(Type),
    {Size,[],St1} = expr(Size0, Vsb0, St0),
    {Bits,Info} = aligned(Bits0, Size, Info0),
    {E,Vsb1,St2} = pattern(E0, Vsb0, St1),
    {Es,Vsb2,St3} = pattern_bin(Es0, Vsb1, St2, Bits),
    {#k_binary_cons{anno=A,size=Size,info=Info,head=E,tail=Es},Vsb2,St3}.

bit_type([Type,{unit,Unit}|Flags]) -> {Unit,Type,Flags}.

%% pattern_list([Cexpr], Vsb, State) -> {[Kexpr],Vsb,State}.

pattern_list(Ces, Vsb, St) ->
    foldr(fun (Ce, {Kes,Vsb0,St0}) ->
		  {Ke,Vsb1,St1} = pattern(Ce, Vsb0, St0),
		  {[Ke|Kes],Vsb1,St1}
	  end, {[],Vsb,St}, Ces).

%% new_vsub() -> VarSub.
%% set_vsub(Name, Sub, VarSub) -> VarSub.
%% subst_vsub(Name, Sub, VarSub) -> VarSub.
%% get_vsub(Name, VarSub) -> SubName.
%%  Add/get substitute Sub for Name to VarSub.  The substitutes are a
%%  list {Name,Sub} pairs.  When adding a new substitute we fold
%%  substitute chains so we never have to search more than once.

new_vsub() -> [].

get_vsub(V, Vsub) ->
    case v_find(V, Vsub) of
	{ok,Val} -> Val;
	error -> V
    end.

set_vsub(V, S, Vsub) ->
    v_store(V, S, Vsub).

subst_vsub(V, S, Vsub0) ->
    %% Fold chained substitutions.
    Vsub1 = map(fun ({O,V1}) when V1 =:= V -> {O,S};
		    (Sub) -> Sub
		end, Vsub0),
    v_store(V, S, Vsub1).

v_find(Key, [{K,Value}|_]) when Key < K -> error;
v_find(Key, [{K,Value}|_]) when Key == K -> {ok,Value};
v_find(Key, [{K,Value}|D]) when Key > K -> v_find(Key, D);
v_find(Key, []) -> error.

v_store(Key, New, [{K,Old}=Pair|Dict]) when Key < K ->
    [{Key,New},Pair|Dict];
v_store(Key, New, [{K,Old}|Dict]) when Key == K ->
    [{Key,New}|Dict];
v_store(Key, New, [{K,Old}=Pair|Dict]) when Key > K ->
    [Pair|v_store(Key, New, Dict)];
v_store(Key, New, []) -> [{Key,New}].

%% new_var_name(State) -> {VarName,State}.

new_var_name(St) ->
    C = St#kern.vcount,
    {list_to_atom("ker" ++ integer_to_list(C)),St#kern{vcount=C + 1}}.

%% new_var(State) -> {#k_var{},State}.

new_var(St0) ->
    {New,St1} = new_var_name(St0),
    {#k_var{name=New},St1}.

%% new_vars(Count, State) -> {[#k_var{}],State}.
%%  Make Count new variables.

new_vars(N, St) -> new_vars(N, St, []).

new_vars(N, St0, Vs) when N > 0 ->
    {V,St1} = new_var(St0),
    new_vars(N-1, St1, [V|Vs]);
new_vars(0, St, Vs) -> {Vs,St}.

make_vars(Vs) -> [ #k_var{name=V} || V <- Vs ].

add_var_def(V, St) ->
    St#kern{ds=sets:add_element(V#k_var.name, St#kern.ds)}.

%%add_vars_def(Vs, St) ->
%%    Ds = foldl(fun (#k_var{name=V}, Ds) -> add_element(V, Ds) end,
%%	       St#kern.ds, Vs),
%%    St#kern{ds=Ds}.

%% is_remote_bif(Mod, Name, Arity) -> true | false.
%% is_internal_bif(Name, Arity) -> true | false.
%%  Test if function is really a BIF.

is_remote_bif(erlang, N, A) ->
    case erl_internal:guard_bif(N, A) of
	true -> true;
	false ->
	    case erl_internal:type_test(N, A) of
		true -> true;
		false ->
		    case catch erl_internal:op_type(N, A) of
			arith -> true;
			bool -> true;
			comp -> true;
			Other -> false		%List, send or not an op
		    end
	    end
    end;
is_remote_bif(M, N, A) -> false.

is_internal_bif(N, A) -> false.

%% bif_vals(Name, Arity) -> integer().
%% bif_vals(Mod, Name, Arity) -> integer().
%%  Determine how many return values a BIF has.  Provision for BIFs to
%%  return multiple values.  Only used in bodies where a BIF maybe
%%  called for effect only.

bif_vals(N, A) -> 1.

bif_vals(M, N, A) -> 1.

%% foldr2(Fun, Acc, List1, List2) -> Acc.
%%  Fold over two lists.

foldr2(Fun, Acc0, [E1|L1], [E2|L2]) ->
    Acc1 = Fun(E1, E2, Acc0),
    foldr2(Fun, Acc1, L1, L2);
foldr2(Fun, Acc, [], []) -> Acc.

%% first([A]) -> [A].
%% last([A]) -> A.

last([L]) -> L;
last([H|T]) -> last(T).

first([L]) -> [];
first([H|T]) -> [H|first(T)].

%% This code implements the algorithm for an optimizing compiler for
%% pattern matching given "The Implementation of Functional
%% Programming Languages" by Simon Peyton Jones. The code is much
%% longer as the meaning of constructors is different from the book.
%%
%% In Erlang many constructors can have different values, e.g. 'atom'
%% or 'integer', whereas in the original algorithm thse would be
%% different constructors. Our view makes it easier in later passes to
%% handle indexing over each type.
%%
%% Patterns are complicated by having alias variables.  The form of a
%% pattern is Pat | {alias,Pat,[AliasVar]}.  This is hidden by access
%% functions to pattern arguments but the code must be aware of it.
%%
%% The compilation proceeds in two steps:
%%
%% 1. The patterns in the clauses to converted to lists of kernel
%% patterns.  The Core clause is now hybrid, this is easier to work
%% with.  Remove clauses with trivially false guards, this simplifies
%% later passes.  Add local defined vars and variable subs to each
%% clause for later use.
%%
%% 2. The pattern matching is optimised.  Variable substitutions are
%% added to the VarSub structure and new variables are made visible.
%% The guard and body are then converted to Kernel form.

%% kmatch([Var], [Clause], Vsb, State) -> {Kexpr,[PreExpr],State}.

kmatch(Us, Ccs, Vsb, St0) ->
    {Cs,St1} = match_pre(Ccs, Vsb, St0),	%Convert clauses
    {Km,St2} = match(Us, Cs, kernel_error, St1),	%Do the match.
    {Km,St2}.

%% match_pre([Cclause], Vsb, State) -> {[Clause],State}.
%%  Must be careful not to generate new substitutions here now!
%%  Remove clauses with trivially false guards which will never
%%  succeed.

match_pre(Cs, Vsb0, St) ->
    foldr(fun (#c_clause{anno=A,pats=Ps,guard=G,body=B}, {Cs0,St0}) ->
		  case is_false_guard(G) of
		      true -> {Cs0,St0};
		      false ->
			  {Kps,Vsb1,St1} = pattern_list(Ps, Vsb0, St0),
			  {[#iclause{anno=A,vsb=Vsb1,pats=Kps,guard=G,body=B}|
			    Cs0],St1}
		  end
	  end, {[],St}, Cs).

%% match([Var], [Clause], Default, State) -> {MatchExpr,State}.

match([U|Us], Cs, Def, St0) ->
    Pcss = partition(Cs),
    foldr(fun (Pcs, {D,St}) -> match_varcon([U|Us], Pcs, D, St) end,
	  {Def,St0}, Pcss);
match([], Cs, Def, St) ->
    match_guard(Cs, Def, St).

%% match_guard([Clause], Default, State) -> {IfExpr,State}.
%%  Build a guard to handle guards. A guard *ALWAYS* fails if no
%%  clause matches, there will be a surrounding 'alt' to catch the
%%  failure.  Drop redundant cases, i.e. those after a true guard.

match_guard(Cs0, Def0, St0) ->
    {Cs1,Def1,St1} = match_guard_1(Cs0, Def0, St0),
    {build_alt(build_guard(Cs1), Def1),St1}.

match_guard_1([#iclause{vsb=Vsb,guard=G,body=B}|Cs0], Def0, St0) ->
    case is_true_guard(G) of
	true ->
	    {Kb,Pb,St1} = body(B, Vsb, St0),
	    {[],pre_seq(Pb, Kb),St1};
	false ->
	    {Kg,Pg,St1} = guard(G, Vsb, St0),
	    {Kb,Pb,St2} = body(B, Vsb, St1),
	    {Cs1,Def1,St3} = match_guard_1(Cs0, Def0, St2),
	    {[#k_guard_clause{guard=pre_seq(Pg, Kg),body=pre_seq(Pb, Kb)}|Cs1],
	     Def1,St3}
    end;
match_guard_1([], Def, St) -> {[],Def,St}. 

%% is_true_guard(Guard) -> bool().
%% is_false_guard(Guard) -> bool().
%%  Test if a guard is either trivially true/false.

is_true_guard(#c_atom{name=true}) -> true;
is_true_guard(Other) -> false.

is_false_guard(#c_atom{name=false}) -> true;
is_false_guard(Other) -> false.

%% partition([Clause]) -> [[Clause]].
%%  Partition a list of clauses into groups which either contain
%%  clauses with a variable first argument, or with a "constructor".

partition([C1|Cs]) ->
    case clause_con(C1) of
	k_zero_binary ->
	    [[C1]|partition(Cs)];
	k_binary_cons ->
	    Val = clause_val(C1),
	    {More,Rest} = splitwith(fun (C) -> is_same_bs_type(C, Val) end, Cs),
	    [[C1|More]|partition(Rest)];
	Type ->
	    V1 = Type =:= k_var,
	    {More,Rest} = splitwith(fun (C) -> is_var_clause(C) == V1 end, Cs),
	    [[C1|More]|partition(Rest)]
    end;
partition([]) -> [].

is_same_bs_type(C, Val) ->
    case clause_con(C) of
	k_binary_cons -> clause_val(C) =:= Val;
	k_zero_binary -> false
    end.

%% match_varcon([Var], [Clause], Def, [Var], Vsb, State) ->
%%        {MatchExpr,State}.

match_varcon(Us, [C|Cs], Def, St) ->
    case is_var_clause(C) of
	true -> match_var(Us, [C|Cs], Def, St);
	false -> match_con(Us, [C|Cs], Def, St)
    end.

%% match_var([Var], [Clause], Def, State) -> {MatchExpr,State}.
%%  Build a call to "select" from a list of clauses all containing a
%%  variable as the first argument.  We must rename the variable in
%%  each clause to be the match variable as these clause will share
%%  this variable and may have different names for it.  Rename aliases
%%  as well.

match_var([U|Us], Cs0, Def, St) ->
    Cs1 = map(fun (#iclause{vsb=Vsb0,pats=[Arg|As]}=C) ->
		      Vs = [arg_arg(Arg)|arg_alias(Arg)],
 		      Vsb1 = foldl(fun (#k_var{name=V}, Acc) ->
 					   subst_vsub(V, U#k_var.name, Acc)
 				   end, Vsb0, Vs),
		      case sets:is_element(U#k_var.name, St#kern.ds) of
			  true -> ok;
			  false ->
			      ok %%= io:fwrite("match_var:~w~n", [U#k_var.name])
		      end,
		      C#iclause{vsb=Vsb1,pats=As}
	      end, Cs0),
    match(Us, Cs1, Def, St).

%% match_con(Variables, [Clause], Default, State) -> {SelectExpr,State}.
%%  Build call to "select" from a list of clauses all containing a
%%  constructor/constant as first argument.  Group the constructors
%%  according to type, the order is really irrelevant but tries to be
%%  smart.

match_con([U|Us], Cs, Def, St0) ->
    %% Extract clauses for different constructors (types).
    %%ok = io:format("match_con ~p~n", [Cs]),
    Ttcs = [ {T,Tcs} || T <- [k_cons,k_tuple,k_atom,k_float,k_int,k_nil,
			      k_bin,
			      k_zero_binary,
			      k_binary_cons],
		       begin Tcs = select(T, Cs),
			     %%ok = io:format("match_con type1 ~p~n", [T]),
			     Tcs /= []
		       end ],
    %%ok = io:format("ttcs = ~p~n", [Ttcs]),
    {Scs,St1} =
	mapfoldl(fun ({T,Tcs}, St) ->
			 {Sc,S1} =
			     match_value([U|Us], T, Tcs, fail, St),
			 %%ok = io:format("match_con type2 ~p~n", [T]),
			 {#k_type_clause{type=T,values=Sc},S1} end,
		 St0, Ttcs),
    {build_alt(build_select(U, Scs), Def),St1}.

%% select(Con, [Clause]) -> [Clause].

select(T, Cs) -> [ C || C <- Cs, clause_con(C) == T ].

%% match_value([Var], Con, [Clause], Default, State) -> {SelectExpr,State}.
%%  At this point all the clauses have the same constructor, we must
%%  now separate them according to value.

match_value(Us, T, [], Def, St) -> {[],St};
match_value(Us, T, Cs0, Def, St0) ->
    Css = group_value(T, Cs0),
    %%ok = io:format("match_value ~p ~p~n", [T, Css]),
    {Css1,St1} = mapfoldl(fun (Cs, St) ->
				  match_clause(Us, T, Cs, Def, St) end,
			  St0, Css),
    {Css1,St1}.
    %%{#k_select_val{type=T,var=hd(Us),clauses=Css1},St1}.

%% group_value([Clause]) -> [[Clause]].
%%  Group clauses according to value.

group_value(k_cons, Cs) -> [Cs];		%These are single valued
group_value(k_nil, Cs) -> [Cs];
group_value(k_bin, Cs) -> [Cs];
group_value(k_zero_binary, Cs) -> [Cs];
group_value(T, Cs) ->
    Cd = foldl(fun (C, Gcs0) -> dict:append(clause_val(C), C, Gcs0) end,
	       dict:new(), Cs),
    dict:fold(fun (V, Vcs, Css) -> [Vcs|Css] end, [], Cd).

%% Profiling shows that this quadratic implementation account for a big amount
%% of the execution time if there are many values.
% group_value([C|Cs]) ->
%     V = clause_val(C),
%     Same = [ Cv || Cv <- Cs, clause_val(Cv) == V ], %Same value
%     Rest = [ Cv || Cv <- Cs, clause_val(Cv) /= V ], % and all the rest
%     [[C|Same]|group_value(Rest)];
% group_value([]) -> [].

%% match_clause([Var], Con, [Clause], Default, State) -> {Clause,State}.
%%  At this point all the clauses have the same "value".  Build one
%%  select clause for this value and continue matching.  Rename
%%  aliases as well.

match_clause([U|Us], T, Cs0, Def, St0) ->
    {Match,Vs,St1} = get_match(get_con(Cs0), St0),
    {Cs1,St2} = new_clauses(Cs0, U, St1),
    {B,St3} = match(Vs ++ Us, Cs1, Def, St2),
    {#k_val_clause{val=Match,body=B},St3}.

get_con([C|Cs]) -> arg_arg(clause_arg(C)).	%Get the constructor

get_match(#k_cons{}, St0) ->
    {[H,T],St1} = new_vars(2, St0),
    {#k_cons{head=H,tail=T},[H,T],St1};
get_match(M=#k_bin{}, St0) ->
    {[V]=Mes,St1} = new_vars(1, St0),
    {#k_bin{val=V},Mes,St1};
get_match(#k_binary_cons{}=Bcons, St0) ->
    {[H,T]=Mes,St1} = new_vars(2, St0),
    {Bcons#k_binary_cons{head=H,tail=T},Mes,St1};
get_match(#k_tuple{es=Es}, St0) ->
    {Mes,St1} = new_vars(length(Es), St0),
    {#k_tuple{es=Mes},Mes,St1};
get_match(M, St) ->
    {M,[],St}.

new_clauses(Cs0, U, St) ->
    Cs1 = map(fun (#iclause{vsb=Vsb0,pats=[Arg|As]}=C) ->
		      Head = case arg_arg(Arg) of
				 #k_cons{head=H,tail=T} -> [H,T|As];
				 #k_tuple{es=Es} -> Es ++ As;
				 #k_bin{val=E}  -> [E|As];
				 #k_binary_cons{head=H,tail=T} -> [H,T|As];
				 Other -> As
			     end,
		      Vs = arg_alias(Arg),
		      Vsb1 = foldl(fun (#k_var{name=V}, Acc) ->
					   subst_vsub(V, U#k_var.name, Acc)
				   end, Vsb0, Vs),
		      case sets:is_element(U#k_var.name, St#kern.ds) of
			  true -> ok;
			  false ->
			      ok %% = io:fwrite("new_clauses:~w~n", [U#k_var.name])
		      end,
		      C#iclause{vsb=Vsb1,pats=Head}
	      end, Cs0),
    {Cs1,St}.

%% build_guard([GuardClause]) -> GuardExpr.

build_guard([]) -> fail;
build_guard(Cs) -> #k_guard{clauses=Cs}.

%% build_select(Var, [ConClause]) -> SelectExpr.

build_select(V, Tcs) -> #k_select{var=V,types=Tcs}.

%% build_alt(First, Then) -> AltExpr.
%%  Build an alt, attempt some simple optimisation.

build_alt(fail, Then) -> Then;
build_alt(First, fail) -> First;
build_alt(First, Then) -> #k_alt{first=First,then=Then}.

%% build_match([MatchVar], MatchExpr) -> Kexpr.
%%  Build a match expr if there is a match.

build_match(Us, #k_alt{}=Km) -> #k_match{vars=Us,body=Km};
build_match(Us, #k_select{}=Km) -> #k_match{vars=Us,body=Km};
build_match(Us, #k_guard{}=Km) -> #k_match{vars=Us,body=Km};
build_match(Us, Km) -> Km.

%% clause_arg(Clause) -> FirstArg.
%% clause_con(Clause) -> Constructor.
%% clause_val(Clause) -> Value.
%% is_var_clause(Clause) -> bool().

clause_arg(#iclause{pats=[Arg|_]}) -> Arg.

clause_con(C) -> arg_con(clause_arg(C)).

clause_val(C) -> arg_val(clause_arg(C)).

is_var_clause(C) -> clause_con(C) == k_var.

%% arg_arg(Arg) -> Arg.
%% arg_alias(Arg) -> Aliases.
%% arg_con(Arg) -> Constructor.
%% arg_val(Arg) -> Value.
%%  These are the basic functions for obtaining fields in an argument.

arg_arg(#ialias{pat=Con}) -> Con;
arg_arg(Con) -> Con.

arg_alias(#ialias{vars=As}) -> As;
arg_alias(Con) -> [].

arg_con(Arg) ->
    case arg_arg(Arg) of
	#k_int{} -> k_int;
	#k_float{} -> k_float;
	#k_atom{} -> k_atom;
	#k_nil{} -> k_nil;
	#k_cons{} -> k_cons; 
	#k_tuple{} -> k_tuple;
	#k_bin{} -> k_bin;
	#k_zero_binary{} -> k_zero_binary;
	#k_binary_cons{} -> k_binary_cons;
	#k_var{} -> k_var
    end.

arg_val(Arg) ->
    case arg_arg(Arg) of
	#k_int{val=I} -> I;
	#k_float{val=F} -> F;
	#k_atom{name=A} -> A;
	#k_nil{} -> 0;
	#k_cons{} -> 2; 
	#k_tuple{es=Es} -> length(Es);
	#k_binary_cons{size=S,info=Info} -> {S,Info};
	#k_zero_binary{} -> 0;
	#k_bin{val=Es} -> 0
    end.

%% ubody(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Tag the body sequence with its used variables.  These bodies
%%  either end with a #k_break{}, or with #k_return{} or an expression
%%  which itself can return, #k_enter{}, #k_match{} ... .

% ubody(#iset{anno=A,vars=Vs,arg=E0,body=#k_break{args=Vs}=B0}, return, St0) ->
%     case is_enter_expr(E0) of
% 	true -> uexpr(E0, return, St0);
% 	false -> 
% 	    {E1,Eu,St1} = uexpr(E0, {break,Vs}, St0),
% 	    {B1,Bu,St2} = ubody(B0, return, St1),
% 	    Ns = pat_list_vars(Vs),
% 	    Used = union(Eu, subtract(Bu, Ns)),
% 	    {#k_seq{anno=#k{us=Used,ns=Ns,a=A},arg=E1,body=B1},Used,St2}
%     end;
ubody(#iset{anno=A,vars=Vs,arg=E0,body=B0}, Br, St0) ->
    {E1,Eu,St1} = uexpr(E0, {break,Vs}, St0),
    {B1,Bu,St2} = ubody(B0, Br, St1),
    Ns = pat_list_vars(Vs),
    Used = union(Eu, subtract(Bu, Ns)),
    {#k_seq{anno=#k{us=Used,ns=Ns,a=A},arg=E1,body=B1},Used,St2};
ubody(#k_break{anno=A,args=As}, return, St) ->
    Au = pat_list_vars(As),
    {#k_return{anno=#k{us=Au,ns=[],a=A},args=As},Au,St};
ubody(#k_break{anno=A,args=As}=Break, {break,Vbs}, St) ->
    Au = pat_list_vars(As),
    {Break#k_break{anno=#k{us=Au,ns=[],a=A}},Au,St};
ubody(E, return, St0) ->
    %% Enterable expressions need no trailing return.
    case is_enter_expr(E) of
	true -> uexpr(E, return, St0);
	false ->
	    {Ea,Pa,St1} = force_atomic(E, St0),
	    ubody(pre_seq(Pa, #k_break{args=[Ea]}), return, St1)
    end;
ubody(E, {break,Rs}, St0) ->
    %%ok = io:fwrite("ubody ~w:~p~n", [?LINE,{E,Br}]),
    %% Exiting expressions need no trailing return.
    case is_exit_expr(E) of
	true -> uexpr(E, return, St0);
	false ->
	    {Ea,Pa,St1} = force_atomic(E, St0),
	    ubody(pre_seq(Pa, #k_break{args=[Ea]}), {break,Rs}, St1)
    end.

%% is_exit_expr(Kexpr) -> bool().
%%  Test whether Kexpr always exits and never returns.

is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=exit,arity=1}}) -> true;
is_exit_expr(#k_call{op=#k_remote{mod=erlang,name=fault,arity=1}}) -> true;
is_exit_expr(#k_call{op=#k_internal{name=match_fail,arity=1}}) -> true;
is_exit_expr(#k_receive_next{}) -> true;
is_exit_expr(Other) -> false.

%% is_enter_expr(Kexpr) -> bool().
%%  Test whether Kexpr is "enterable", i.e. can handle return from
%%  within itself without extra #k_return{}.

is_enter_expr(#k_call{}) -> true;
is_enter_expr(#k_match{}) -> true;
is_enter_expr(#k_receive{}) -> true;
is_enter_expr(#k_receive_next{}) -> true;
is_enter_expr(Other) -> false.

%% uguard(Expr, State) -> {Expr,[UsedVar],State}.
%%  Tag the guard sequence with its used variables.

uguard(#iset{anno=A,vars=Vs,arg=E0,body=B0}, St0) ->
    Ns = pat_list_vars(Vs),
    {E1,Eu,St1} = uguard_expr(E0, Vs, St0),
    {B1,Bu,St2} = uguard(B0, St1),
    Used = union(Eu, subtract(Bu, Ns)),
    {#k_seq{anno=#k{us=Used,ns=Ns,a=A},arg=E1,body=B1},Used,St2};
uguard(E, St) -> uguard_expr(E, [], St).

uguard_expr(#k_bif{anno=A,op=Op,args=As}=Bif, Rs, St) ->
    Used = union(op_vars(Op), pat_list_vars(As)),
    {Bif#k_bif{anno=#k{us=Used,ns=pat_list_vars(Rs),a=A},ret=Rs},
     Used,St};
uguard_expr(Lit, Rs, St) ->
    %% Transform literals to puts here.
    Used = pat_vars(Lit),
    {#k_put{anno=#k{us=Used,ns=pat_list_vars(Rs),a=get_kanno(Lit)},
	    arg=Lit,ret=Rs},Used,St}.

%% uexpr(Expr, Break, State) -> {Expr,[UsedVar],State}.
%%  Tag an expression with its used variables.
%%  Break = return | {break,[RetVar]}.

uexpr(#k_call{anno=A,op=Op,args=As}=Call, {break,Rs}, St) ->
    Used = union(op_vars(Op), pat_list_vars(As)),
    {Call#k_call{anno=#k{us=Used,ns=pat_list_vars(Rs),a=A},ret=Rs},
     Used,St};
uexpr(#k_call{anno=A,op=Op,args=As}, return, St) ->
    Used = union(op_vars(Op), pat_list_vars(As)),
    {#k_enter{anno=#k{us=Used,ns=[],a=A},op=Op,args=As},
     Used,St};
uexpr(#k_bif{anno=A,op=Op,args=As}=Bif, {break,Rs}, St0) ->
    Used = union(op_vars(Op), pat_list_vars(As)),
    {Brs,St1} = bif_returns(Op, Rs, St0),
    {Bif#k_bif{anno=#k{us=Used,ns=pat_list_vars(Brs),a=A},ret=Brs},
     Used,St1};
uexpr(#k_match{anno=A,vars=Vs,body=B0}, Br, St0) ->
    Rs = break_rets(Br),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    {#k_match{anno=#k{us=Bu,ns=pat_list_vars(Rs),a=A},
	      vars=Vs,body=B1,ret=Rs},Bu,St1};
uexpr(#k_receive{anno=A,var=V,body=B0,timeout=T,action=A0}, Br, St0) ->
    Rs = break_rets(Br),
    Tu = pat_vars(T),				%Timeout is atomic
    {B1,Bu,St1} = umatch(B0, Br, St0),
    {A1,Au,St2} = ubody(A0, Br, St1),
    Used = del_element(V#k_var.name, union(Bu, union(Tu, Au))),
    {#k_receive{anno=#k{us=Used,ns=pat_list_vars(Rs),a=A},
		var=V,body=B1,timeout=T,action=A1,ret=Rs},
     Used,St2};
uexpr(#k_receive_accept{anno=A}, Br, St) ->
    {#k_receive_accept{anno=#k{us=[],ns=[],a=A}},[],St};
uexpr(#k_receive_reject{anno=A}, Br, St) ->
    {#k_receive_reject{anno=#k{us=[],ns=[],a=A}},[],St};
uexpr(#k_receive_next{anno=A}, Br, St) ->
    {#k_receive_next{anno=#k{us=[],ns=[],a=A}},[],St};
uexpr(#k_catch{anno=A,body=B0}, {break,Rs0}, St0) ->
    {Rb,St1} = new_var(St0),
    {B1,Bu,St2} = ubody(B0, {break,[Rb]}, St1),
    %% Guarantee ONE return variable.
    {Ns,St3} = new_vars(1 - length(Rs0), St2),
    Rs1 = Rs0 ++ Ns,
    {#k_catch{anno=#k{us=Bu,ns=pat_list_vars(Rs1),a=A},body=B1,ret=Rs1},Bu,St3};
uexpr(#ifun{anno=A,vars=Vs,body=B0}, {break,Rs}, St0) ->
    {B1,Bu,St1} = ubody(B0, return, St0),	%Return out of new function
    Ns = pat_list_vars(Vs),
    Free = subtract(Bu, Ns),			%Free variables in fun
    Fvs = make_vars(Free),
    {Fname,St2} = new_fun_name(St1),
    Arity = length(Vs)+length(Free),
    Fun = #k_fdef{anno=#k{us=[],ns=[],a=A},func=Fname,arity=Arity,
		  vars=Vs ++ Fvs,body=B1},
    {value,{id,Id}} = lists:keysearch(id, 1, A),
    {#k_call{anno=#k{us=Free,ns=pat_list_vars(Rs),a=A},
	     op=#k_internal{name=make_fun,arity=length(Free)+3},
	     args=[#k_atom{name=Fname},#k_int{val=Arity},#k_int{val=Id}|Fvs],
	     ret=Rs},
     Free,St2#kern{funs=[Fun|St2#kern.funs]}};
uexpr(Lit, {break,Rs}, St) ->
    %% Transform literals to puts here.
    %%ok = io:fwrite("uexpr ~w:~p~n", [?LINE,Lit]),
    Used = pat_vars(Lit),
    {#k_put{anno=#k{us=Used,ns=pat_list_vars(Rs),a=get_kanno(Lit)},
	    arg=Lit,ret=Rs},Used,St}.

break_rets({break,Rs}) -> Rs;
break_rets(return) -> [].

%% bif_returns(Op, [Ret], State) -> {[Ret],State}.

bif_returns(#k_remote{mod=M,name=N,arity=Ar}, Rs, St0) ->
    %%ok = io:fwrite("uexpr ~w:~p~n", [?LINE,{M,N,Ar,Rs}]),
    {Ns,St1} = new_vars(bif_vals(M, N, Ar) - length(Rs), St0),
    {Rs ++ Ns,St1};
bif_returns(#k_internal{name=N,arity=Ar}, Rs, St0) ->
    %%ok = io:fwrite("uexpr ~w:~p~n", [?LINE,{N,Ar,Rs}]),
    {Ns,St1} = new_vars(bif_vals(N, Ar) - length(Rs), St0),
    {Rs ++ Ns,St1}.

%% umatch(Match, Break, State) -> {Match,[UsedVar],State}.
%%  Tag a match expression with its used variables.

umatch(#k_alt{anno=A,first=F0,then=T0}, Br, St0) ->
    {F1,Fu,St1} = umatch(F0, Br, St0),
    {T1,Tu,St2} = umatch(T0, Br, St1),
    Used = union(Fu, Tu),
    {#k_alt{anno=#k{us=Used,ns=[],a=A},first=F1,then=T1},
     Used,St2};
umatch(#k_select{anno=A,var=V,types=Ts0}, Br, St0) ->
    {Ts1,Tus,St1} = umatch_list(Ts0, Br, St0),
    Used = add_element(V#k_var.name, Tus),
    {#k_select{anno=#k{us=Used,ns=[],a=A},var=V,types=Ts1},Used,St1};
umatch(#k_type_clause{anno=A,type=T,values=Vs0}, Br, St0) ->
    {Vs1,Vus,St1} = umatch_list(Vs0, Br, St0),
    {#k_type_clause{anno=#k{us=Vus,ns=[],a=A},type=T,values=Vs1},Vus,St1};
umatch(#k_val_clause{anno=A,val=P,body=B0}, Br, St0) ->
    {U0,Ps} = match_pat_vars(P),
    {B1,Bu,St1} = umatch(B0, Br, St0),
    Used = union(U0, subtract(Bu, Ps)),
    {#k_val_clause{anno=#k{us=Used,ns=[],a=A},val=P,body=B1},
     Used,St1};
umatch(#k_guard{anno=A,clauses=Gs0}, Br, St0) ->
    {Gs1,Gus,St1} = umatch_list(Gs0, Br, St0),
    {#k_guard{anno=#k{us=Gus,ns=[],a=A},clauses=Gs1},Gus,St1};
umatch(#k_guard_clause{anno=A,guard=G0,body=B0}, Br, St0) ->
    {G1,Gu,St1} = uguard(G0, St0),
    {B1,Bu,St2} = umatch(B0, Br, St1),
    Used = union(Gu, Bu),
    {#k_guard_clause{anno=#k{us=Used,ns=[],a=A},guard=G1,body=B1},Used,St2};
umatch(B0, Br, St0) -> ubody(B0, Br, St0).

umatch_list(Ms0, Br, St) ->
    foldr(fun (M0, {Ms1,Us,Sta}) ->
		  {M1,Mu,Stb} = umatch(M0, Br, Sta),
		  {[M1|Ms1],union(Mu, Us),Stb}
	  end, {[],[],St}, Ms0).

%% op_vars(Op) -> [VarName].

op_vars(#k_local{}) -> [];
op_vars(#k_remote{}) -> [];
op_vars(#k_internal{}) -> [];
op_vars(Atomic) -> pat_vars(Atomic).

%% pat_vars(Pattern) -> [VarName].

pat_vars(#k_var{name=N}) -> [N];
pat_vars(#k_int{}) -> [];
pat_vars(#k_float{}) -> [];
pat_vars(#k_atom{}) -> [];
pat_vars(#k_char{}) -> [];
pat_vars(#k_string{}) -> [];
pat_vars(#k_nil{}) -> [];
pat_vars(#k_cons{head=H,tail=T}) ->
    union(pat_vars(H), pat_vars(T));
pat_vars(#k_bin{val=V}) ->
    pat_vars(V);
pat_vars(#k_zero_binary{}) -> [];
pat_vars(#k_binary_cons{size=Size,head=H,tail=T}) ->
    union(pat_vars(Size), union(pat_vars(H), pat_vars(T)));
pat_vars(#k_tuple{es=Es}) ->
    pat_list_vars(Es).


pat_list_vars(Ps) ->
    foldl(fun (P, Vs) -> union(pat_vars(P), Vs) end, [], Ps).

%% match_pat_vars(Pattern) -> {[UsedVarName],[NewVarName]}.

match_pat_vars(#k_var{name=N}) -> {[],[N]};
match_pat_vars(#k_int{}) -> {[],[]};
match_pat_vars(#k_float{}) -> {[],[]};
match_pat_vars(#k_atom{}) -> {[],[]};
match_pat_vars(#k_char{}) -> {[],[]};
match_pat_vars(#k_string{}) -> {[],[]};
match_pat_vars(#k_nil{}) -> {[],[]};
match_pat_vars(#k_cons{head=H,tail=T}) ->
    match_pat_list_vars([H,T]);
match_pat_vars(#k_bin{val=V}) ->
    match_pat_vars(V);
match_pat_vars(#k_binary_cons{size=Size,head=H,tail=T}) ->
    {U1,New} = match_pat_list_vars([H,T]),
    {[],U2} = match_pat_vars(Size),
    {union(U1, U2),New};
match_pat_vars(#k_zero_binary{}) ->
    {[],[]};
match_pat_vars(#k_tuple{es=Es}) ->
    match_pat_list_vars(Es).

match_pat_list_vars(Ps) ->
    foldl(fun (P, {Used0,New0}) ->
		  {Used,New} = match_pat_vars(P),
		  {union(Used0, Used),union(New0, New)} end,
	  {[],[]}, Ps).

%% new_fun_name(State) -> {FunName,State}.

new_fun_name(#kern{func={F,A},fcount=I}=St) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)
	++ "-fun-" ++ integer_to_list(I) ++ "-",
    {list_to_atom(Name),St#kern{fcount=I+1}}.

%% Add 'aligned' to the flags if the current field is aligned.

aligned(Bits, Size, {Unit,Type,Flags}=Info) when Bits rem 8 =:= 0 ->
    {incr_bits(Bits, Size, Info),{Unit,Type,[aligned|Flags]}};
aligned(Bits, Size, Info) ->
    {incr_bits(Bits, Size, Info),Info}.

% Number of bits correct modulo 8.

incr_bits(Bits, #k_int{val=Size}, {Unit,Type,Flags}) when integer(Bits) ->
    Bits + Size*Unit;
incr_bits(Bits, #k_atom{name=all}, Info) -> 0;	%Always aligned.
incr_bits(Bits, _, {8,_,_}) -> Bits;
incr_bits(_, _, _) -> undefined.
