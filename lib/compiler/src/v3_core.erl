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
%% Purpose : Transform normal Erlang to Core Erlang

%% At this stage all preprocessing has been done. All that is left are
%% "pure" Erlang functions.
%%
%% Core transformation is done in three stages:
%%
%% 1. Flatten expressions into an internal core form without doing
%%    matching.
%%
%% 2. Step "forwards" over the icore code annotating each "top-level"
%%    thing with variable usage.  Detect bound variables in matching
%%    and replace with explicit guard test.  Annotate "internal-core"
%%    expressions with variables they use and create.  Convert matches
%%    to cases when not pure assignments.
%%
%% 3. Step "backwards" over icore code using variable usage
%%    annotations to change implicit exported variables to explicit
%%    returns.
%%
%% To ensure the evaluation order we ensure that all arguments are
%% safe.  A "safe" is basically a core_lib simple with VERY restricted
%% binaries.
%%
%% We have to be very careful with matches as these create variables.
%% While we try not to flatten things more than necessary we must make
%% sure that all matches are at the top level.  For this we use the
%% type "novars" which are non-match expressions.  Cases and receives
%% can also create problems due to exports variables so they are not
%% "novars" either.  I.e. a novars will not export variables.
%%
%% We also try to optimise nested setelement when they are safe,
%% i.e. the innermost setelement has the highest index.  In this case
%% we generate a normal setelement followed by a sequence of primop
%% 'dsetelement'.  These primops are for side-effect only and must
%% directly follow the setelement.  This makes the code a little
%% funny.  The translation is not really error equivalent as we
%% calculate all the new values first BEFORE we try to modify the
%% tuple.
%%
%% Annotations in the icore code is kept in a record, #a, not in a
%% list as in proper core.  This is easier and faster and creates no
%% problems as we have complete control over all annotations.
%%
%% In this translation:
%%
%% call ops are safes
%% call arguments are safes
%% match arguments are novars
%% case arguments are novars
%% receive timeouts are novars
%% let/set arguments are expressions
%% fun is not a safe

-module(v3_core).

-export([module/2]).

-import(lists, [map/2,all/2,foldl/3,foldr/3,mapfoldl/3,splitwith/2]).
-import(ordsets, [add_element/2,del_element/2,is_element/2,
		  union/1,union/2,intersection/2,subtract/2]).

-include("core_parse.hrl").

%% Internal core expressions and help functions.
%% N.B. annotations fields in place as normal Core expressions.

-record(iset, {anno=[],var,arg}).
-record(iletrec, {anno=[],defs,body}).
-record(imatch, {anno=[],pat,guard=[],arg,fc}).
-record(icase, {anno=[],args,clauses,fc}).
-record(iclause, {anno=[],pats,pguard=[],guard,body}).
-record(ifun, {anno=[],id,vars,clauses,fc}).
-record(iapply, {anno=[],op,args}).
-record(icall, {anno=[],module,name,args}).
-record(iprimop, {anno=[],name,args}).
-record(itry, {anno=[],body,clauses}).
-record(icatch, {anno=[],body}).
-record(ireceive1, {anno=[],clauses}).
-record(ireceive2, {anno=[],clauses,timeout,action}).
-record(iprotect, {anno=[],body}).

-record(core, {vcount=0,			%Variable counter
	       fcount=0}).			%Function counter

-record(a, {us=[],ns=[],anno=[]}).		%Internal annotation

module({Mod,Exp,Forms}, Options) ->
    Cexp = map(fun ({N,A}) -> #c_fname{id=N,arity=A} end, Exp),
    {Kfs,As} = foldr(fun form/2, {[],[]}, Forms),
    {ok,#c_module{name=#c_atom{val=Mod},exports=Cexp,attrs=As,defs=Kfs}}.

form({function,L,N,A,Cs}=F, {Fs,As}) ->
    {[function(F)|Fs],As};
form({attribute,L,N,V}=F, {Fs,As}) ->
    {Fs,[attribute(F)|As]}.

attribute({attribute,L,Name,Val}) ->
    #c_def{name=core_lib:make_literal(Name),
	   val=core_lib:make_literal(Val)}.

function({function,L,Name,Arity,Cs0}) ->
    %%ok = io:fwrite("~p - ", [{Name,Arity}]),
    St0 = #core{vcount=0},
    {B0,St1} = body(Cs0, Arity, St0),
    %%ok = io:fwrite("1", []),
    %%ok = io:fwrite("~w:~p~n", [?LINE,B0]),
    {B1,St2} = ubody(B0, St1),
    %%ok = io:fwrite("2", []),
    %%ok = io:fwrite("~w:~p~n", [?LINE,B1]),
    {B2,St3} = cbody(B1, St2),
    %%ok = io:fwrite("3~n", []),
    #c_def{name=#c_fname{id=Name,arity=Arity},val=B2}.

body(Cs0, Arity, St0) ->
    {Args,St1} = new_vars(Arity, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Fc = fail_clause(Ps, #c_tuple{es=[#c_atom{val=function_clause}|Ps]}),
    {#ifun{id=[],vars=Args,clauses=Cs1,fc=Fc},St3}.

%% clause(Clause, State) -> {Cclause,State} | noclause.
%% clauses([Clause], State) -> {[Cclause],State}.
%%  Convert clauses.  Trap bad pattern aliases and remove clause from
%%  clause list.

clauses([C0|Cs0], St0) ->
    case clause(C0, St0) of
	noclause -> clauses(Cs0, St0);
	{C,St1} ->
	    {Cs,St2} = clauses(Cs0, St1),
	    {[C|Cs],St2}
    end;
clauses([], St) -> {[],St}.

clause({clause,L,H0,G0,B0}, St0) ->
    case catch head(H0) of
	{'EXIT',_}=Exit -> exit(Exit);		%Propagate error
	nomatch -> noclause;			%Bad pattern
	H1 ->
	    {G1,St1} = guard(G0, St0),
	    {B1,St2} = exprs(B0, St1),
	    {#iclause{pats=H1,guard=G1,body=B1},St2}
    end.

%% head([P]) -> [P].

head(Ps) -> pattern_list(Ps).

%% guard([Expr], State) -> {[Cexpr],State}.
%%  Build an explict and/or tree of guard alternatives, then traverse
%%  top-level and/or tree and "protect" inner tests.

guard([], St) -> {[],St};
guard(Gs0, St0) ->
    Gs1 = foldr(fun (Gt0, Rhs) ->
			Gt1 = guard_tests(Gt0),
			{op,0,'or',Gt1,Rhs}
		end, guard_tests(last(Gs0)), first(Gs0)),
    {E,Eps,St1} = gexpr(Gs1, St0),
    {Eps ++ [E],St1}.
    
guard_tests([], St) -> {[],St};
guard_tests(Gs0, St0) ->
    Gs1 = guard_tests(Gs0),
    {E,Eps,St1} = gexpr(Gs1, St0),
    {Eps ++ [E],St1}.

guard_tests([]) -> [];
guard_tests(Gs) ->
    foldr(fun (G, Rhs) -> {op,0,'and',G,Rhs} end, last(Gs), first(Gs)).

%% gexpr(Expr, State) -> {Cexpr,[PreExp],State}.
%%  Generate an internal core expression of a guard test.  Explicitly
%%  handle outer boolean expressions and "protect" inner tests in a
%%  reasonably smart way.

gexpr({op,Line,Op,L,R}=Call, St0) ->
    case erl_internal:bool_op(Op, 2) of
	true ->
	    {Le,Lps,St1} = gexpr(L, St0),
	    {Ll,Llps,St2} = force_safe(Le, St1),
	    {Re,Rps,St3} = gexpr(R, St2),
	    {Rl,Rlps,St4} = force_safe(Re, St3),
	    {#icall{anno=#a{anno=[Line]},	%Must have an #a{}
		    module=#c_atom{val=erlang},name=#c_atom{val=Op},
		    args=[Ll,Rl]},Lps ++ Llps ++ Rps ++ Rlps,St4};
	false ->
	    gexpr_test(Call, St0)
    end;
gexpr({op,Line,Op,A}=Call, St0) ->
    case erl_internal:bool_op(Op, 1) of
	true ->
	    {Ae,Aps,St1} = gexpr(A, St0),
	    {Al,Alps,St2} = force_safe(Ae, St1),
	    {#icall{anno=#a{anno=[Line]},	%Must have an #a{}
		    module=#c_atom{val=erlang},name=#c_atom{val=Op},
		    args=[Al]},Aps ++ Alps,St2};
	false ->
	    gexpr_test(Call, St0)
    end;
gexpr(E0, St0) ->
    gexpr_test(E0, St0).

%% gexpr_test(Expr, State) -> {Cexpr,[PreExp],State}.
%%  Generate a guard test.  At this stage we must be sure that we have
%%  a proper boolean value here so wrap things with an true test if we
%%  don't know, i.e. if it is not a comparison or a type test.  This
%%  test will never generate a fault so only protect if it has sub
%%  expressions which can fail.  This is to avoid unnecessary
%%  protected's.

gexpr_test(E0, St0) ->
    {E1,Eps0,St1} = expr(E0, St0),
    %% Generate "top-level" test and argument calls.
    {E2,Eps1,St3} =
	case E1 of
	    #icall{module=#c_atom{val=erlang},name=#c_atom{val=N},args=As} ->
		Ar = length(As),
		case erl_internal:type_test(N, Ar) or
		     erl_internal:comp_op(N, Ar) of
		    true -> {E1,Eps0,St1};
		    false ->
			{New,St2} = new_var(St1),
			{#icall{anno=#a{},	%Must have an #a{}
				module=#c_atom{val=erlang},
				name=#c_atom{val='=:='},
				args=[New,#c_atom{val=true}]},
			 Eps0 ++ [#iset{var=New,arg=E1}],St2}
		end;
	    E1 ->
		case core_lib:is_simple(E1) of
		    true -> {#icall{anno=#a{},	%Must have an #a{}
				    module=#c_atom{val=erlang},
				    name=#c_atom{val='=:='},
				    args=[E1,#c_atom{val=true}]},Eps0,St1};
		    false ->
			{New,St2} = new_var(St1),
			{#icall{anno=#a{},	%Must have an #a{}
				module=#c_atom{val=erlang},
				name=#c_atom{val='=:='},
				args=[New,#c_atom{val=true}]},
			 Eps0 ++ [#iset{var=New,arg=E1}],St2}
		end
	end,
    %% Do we need to protect test?
    if  Eps1 == [] -> {E2,[],St3};
	true -> {#iprotect{body=Eps1 ++ [E2]},[],St3}
    end.

%% exprs([Expr], State) -> {[Cexpr],State}.
%%  Flatten top-level exprs.

exprs([E0|Es0], St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Es1,St2} = exprs(Es0, St1),
    {Eps ++ [E1] ++ Es1,St2};
exprs([], St) -> {[],St}.

%% expr(Expr, State) -> {Cexpr,[PreExp],State}.
%%  Generate an internal core expression.

expr({var,L,V}, St) -> {#c_var{name=V},[],St};
expr({char,L,C}, St) -> {#c_char{val=C},[],St};
expr({integer,L,I}, St) -> {#c_int{val=I},[],St};
expr({float,L,F}, St) -> {#c_float{val=F},[],St};
expr({atom,L,A}, St) -> {#c_atom{val=A},[],St};
expr({nil,L}, St) -> {#c_nil{},[],St};
expr({string,L,S}, St) -> {#c_string{val=S},[],St};
expr({cons,L,H0,T0}, St0) ->
    {H1,Hps,St1} = safe(H0, St0),
    {T1,Tps,St2} = safe(T0, St1),
    {#c_cons{hd=H1,tl=T1},Hps ++ Tps,St2};
expr({lc,L,E,Qs}, St) ->
    lc_tq(E, Qs, {nil,L}, St);
expr({tuple,L,Es0}, St0) ->
    {Es1,Eps,St1} = safe_list(Es0, St0),
    {#c_tuple{es=Es1},Eps,St1};
expr({bin,L,Es0}, St0) ->
    {Es1,Eps,St1} = expr_bin(Es0, St0),
    {#c_binary{segs=Es1},Eps,St1};
expr({block,L,Es0}, St0) ->
    %% Inline the block directly.
    {Es1,St1} = exprs(first(Es0), St0),
    {E1,Eps,St2} = expr(last(Es0), St1),
    {E1,Es1 ++ Eps,St2};
expr({'if',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    Fc = fail_clause([], #c_atom{val=if_clause}),
    {#icase{args=[],clauses=Cs1,fc=Fc},[],St1};
expr({'case',L,E0,Cs0}, St0) ->
    {E1,Eps,St1} = novars(E0, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {Fpat,St3} = new_var(St2),
    Fc = fail_clause([Fpat], #c_tuple{es=[#c_atom{val=case_clause},Fpat]}),
    {#icase{args=[E1],clauses=Cs1,fc=Fc},Eps,St3};
expr({'receive',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {#ireceive1{clauses=Cs1}, [], St1};
expr({'receive',L,Cs0,Te0,Tes0}, St0) ->
    {Te1,Teps,St1} = novars(Te0, St0),
    {Tes1,St2} = exprs(Tes0, St1),
    {Cs1,St3} = clauses(Cs0, St2),
    {#ireceive2{clauses=Cs1,timeout=Te1,action=Tes1},Teps,St3};
expr({'try',L,Es0,Cs0}, St0) ->
    {Es1,St1} = exprs(Es0, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {#itry{body=Es1,clauses=Cs1},[],St2};
expr({'catch',L,E0}, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {#icatch{body=Eps ++ [E1]},[],St1};
expr({'fun',L,{function,F,A},{Index,Uniq,Name}=Id}, St) ->
    {#c_fname{anno=[{id,Id}],id=F,arity=A},[],St};
expr({'fun',L,{clauses,Cs},{Index,Uniq,Hvs,Free,Name}}, St) ->
    %% Take the new hacky format.
    fun_tq({Index,Uniq,Name}, Cs, St);
expr({call,L,{remote,Lr,{atom,_,erlang}=M,{atom,_,setelement}=F},
      [{integer,_,I},Tuple,Expr]=As0}=Call, St0) ->
    %% Special case where we have nested setelement's and it is safe
    %% to use a destructive setelement.
    case is_dsetel_safe(I, Tuple) of
	true ->
	    {V,Eps,Tps,Dss,St1} = fold_dsetel(0, Call, St0),
	    {V,Eps ++ Tps ++ Dss,St1};
	false ->
	    {[M1,F1|As1],Aps,St1} = safe_list([M,F|As0], St0),
	    {#icall{anno=#a{anno=[L]},module=M1,name=F1,args=As1},Aps,St1}
    end;
expr({call,L,{remote,Lr,M,F},As0}, St0) ->
    {[M1,F1|As1],Aps,St1} = safe_list([M,F|As0], St0),
    {#icall{anno=#a{anno=[L]},module=M1,name=F1,args=As1},Aps,St1};
expr({call,L,{atom,Lf,F},As0}, St0) ->
    {As1,Aps,St1} = safe_list(As0, St0),
    Op = #c_fname{id=F,arity=length(As1)},
    {#iapply{op=Op,args=As1},Aps,St1};
expr({call,L,FunExp,As0}, St0) ->
    {Fun,Fps,St1} = safe(FunExp, St0),
    {As1,Aps,St2} = safe_list(As0, St1),
    {#iapply{op=Fun,args=As1},Fps ++ Aps,St2};
expr({match,L,P0,E0}, St0) ->
    %% First fold matches together to create aliases.
    {P1,E1} = fold_match(E0, P0),
    {E2,Eps,St1} = novars(E1, St0),
    P2 = (catch pattern(P1)),
    {Fpat,St2} = new_var(St1),
    Fc = fail_clause([Fpat], #c_tuple{es=[#c_atom{val=badmatch},Fpat]}),
    case P2 of
	{'EXIT',_}=Exit -> exit(Exit);		%Propagate error
	nomatch -> {#icase{args=[E2],clauses=[],fc=Fc},Eps,St2};
	_Other ->  {#imatch{pat=P2,arg=E2,fc=Fc},Eps,St2}
    end;
expr({op,L,'++',{lc,L1,E,Qs},L2}, St) ->
    %%  Optimise this here because of the list comprehension algorithm.
    lc_tq(E, Qs, L2, St);
expr({op,L,Op,A0}, St0) ->
    {A1,Aps,St1} = safe(A0, St0),
    {#icall{anno=#a{anno=[L]},			%Must have an #a{}
	    module=#c_atom{val=erlang},
	    name=#c_atom{val=Op},args=[A1]},Aps,St1};
expr({op,L,Op,L0,R0}, St0) ->
    {As,Aps,St1} = safe_list([L0,R0], St0),
    {#icall{anno=#a{anno=[L]},			%Must have an #a{}
	    module=#c_atom{val=erlang},
	    name=#c_atom{val=Op},args=As},Aps,St1}.

%% expr_bin([ArgExpr], St) -> {[Arg],[PreExpr],St}.
%%  Flatten the arguments of a bin. Do this straight left to right!

expr_bin(Es, St) ->
    foldr(fun (E, {Ces,Esp,St0}) ->
		  {Ce,Ep,St1} = bin_segment(E, St0),
		  {[Ce|Ces],Ep ++ Esp,St1}
	  end, {[],[],St}, Es).

bin_segment({bin_element,L,E0,Size0,[Type,{unit,Unit}|Flags]}, St0) ->
    {E1,Eps,St1} = safe(E0, St0),
    {Size1,Eps2,St2} = safe(Size0, St1),
    {#c_bin_seg{val=E1,size=Size1,unit=Unit,type=Type,flags=Flags},
     Eps ++ Eps2,St2}.

%% is_dsetel_safe(LastIndex, TupleExpr)
%%  Checks if destructive setelement is safe.
is_dsetel_safe(I0, {call,_,{remote,_,{atom,_,erlang},{atom,_,setelement}},
		    [{integer,_,I},_,Expr]}) when I >= I0 ->
    true;
is_dsetel_safe(I, TupleExpr) -> false.

%% fold_dsetel(LastIndex, TupleExpr, State) ->
%%      {TupleVar,[PreExp],[Dsetel],State}.
%%  Fold down over setelement tuple arguments until we reach one which
%%  is not a dset safe setelement.  On the way back up we build a
%%  sequence of dsetelements which will become a sequence and finally
%%  return the top tuple variable.

fold_dsetel(I0, {call,L,{remote,_,{atom,_,erlang}=M,{atom,_,setelement}=F},
		[{integer,_,I},Tuple,Val]}=Expr, St0) when I >= I0 ->
    case is_dsetel_safe(I, Tuple) of
	true ->
	    {V,Eps,Vps,Dss,St1} = fold_dsetel(I, Tuple, St0),
	    {A,Aps,St2} = atomic(Val, St1),
	    {V,Eps ++ Aps,Vps,Dss ++ [#iprimop{anno=#a{anno=[L]},
					       name=#c_atom{val=dsetelement},
					       args=[#c_int{val=I},V,A]}],St2};
	false ->
	    {Ce,Eps,St1} = expr(Expr, St0),
	    {V,Vps,St2} = force_variable(Ce, St1),
	    {V,Eps,Vps,[],St2}
    end.

%% fun_tq(Id, [Clauses], State) -> {Fun,[PreExp],State}.

fun_tq(Id, Cs0, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    Arity = length((hd(Cs1))#iclause.pats),
    {Args,St2} = new_vars(Arity, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Fc = fail_clause(Ps, #c_tuple{es=[#c_atom{val=function_clause}|Ps]}),
    Fun = #ifun{id=[{id,Id}],				%We KNOW!
		vars=Args,clauses=Cs1,fc=Fc},
    {Fun,[],St3}.

%% lc_tq(Exp, [Qualifier], More, State) -> {LetRec,[PreExp],State}.
%%  This TQ from Simon PJ pp 127-138.  
%%  This gets a bit messy as we must transform all directly here.  We
%%  recognise guard tests and try to fold them together and join to a
%%  preceding generators, this should give us better and more compact
%%  code.
%%  More could be transformed before calling lc_tq.

lc_tq(E, [{generate,Lg,P,G}|Qs0], More, St0) ->
    {Gs,Qs1} =  splitwith(fun is_guard_test/1, Qs0),
    {Name,St1} = new_fun_name("lc", St0),
    {Head,St2} = new_var(St1),
    {Tname,St3} = new_var_name(St2),
    Tail = #c_var{name=Tname},
    {Arg,St4} = new_var(St3),
    NewMore = {call,Lg,{atom,Lg,Name},[{var,Lg,Tname}]},
    {Guardc,St5} = guard_tests(Gs, St4),	%These are always flat!
    {Lc,Lps,St6} = lc_tq(E, Qs1, NewMore, St5),
    {Mc,Mps,St7} = expr(More, St6),
    {Nc,Nps,St8} = expr(NewMore, St7),
    Pc = pattern(P),
    {Gc,Gps,St9} = safe(G, St8),		%Will be a function argument!
    Fc = fail_clause([Arg], #c_tuple{es=[#c_atom{val=function_clause},Arg]}),
    Fun = #ifun{id=[],
		vars=[Arg],
		clauses=[#iclause{pats=[#c_cons{hd=Pc,tl=Tail}],
				  guard=Guardc,
				  body=Lps ++ [Lc]},
			 #iclause{pats=[#c_cons{hd=Head,tl=Tail}],
				  guard=[],
				  body=Nps ++ [Nc]},
			 #iclause{pats=[#c_nil{}],guard=[],
				  body=Mps ++ [Mc]}],
		fc=Fc},
    {#iletrec{defs=[{Name,Fun}],
	      body=Gps ++ [#iapply{op=#c_fname{id=Name,arity=1},args=[Gc]}]},
     [],St9};
lc_tq(E, [Fil0|Qs0], More, St0) ->
    %% Special case sequences guard tests.
    case is_guard_test(Fil0) of
	true ->
	    {Gs0,Qs1} = splitwith(fun is_guard_test/1, Qs0),
	    {Lc,Lps,St1} = lc_tq(E, Qs1, More, St0),
	    {Mc,Mps,St2} = expr(More, St1),
	    {Gs,St3} = guard_tests([Fil0|Gs0], St2), %These are always flat!
	    {#icase{args=[],
		    clauses=[#iclause{pats=[],guard=Gs,body=Lps ++ [Lc]}],
		    fc=#iclause{pats=[],guard=[],body=Mps ++ [Mc]}},
	     [],St3};
	false ->
	    {Lc,Lps,St1} = lc_tq(E, Qs0, More, St0),
	    {Mc,Mps,St2} = expr(More, St1),
	    {Fpat,St3} = new_var(St2),
	    Fc = fail_clause([Fpat], #c_tuple{es=[#c_atom{val=case_clause},Fpat]}),
	    %% Do a novars little optimisation here.
	    case Fil0 of
		{op,_,'not',Fil1} ->
		    {Filc,Fps,St4} = novars(Fil1, St3),
		    {#icase{args=[Filc],
			    clauses=[#iclause{pats=[#c_atom{val=true}],
					      guard=[],
					      body=Mps ++ [Mc]},
				     #iclause{pats=[#c_atom{val=false}],
					      guard=[],
					      body=Lps ++ [Lc]}],
			    fc=Fc},
		     Fps,St4};
		Other ->
		    {Filc,Fps,St4} = novars(Fil0, St3),
		    {#icase{args=[Filc],
			    clauses=[#iclause{pats=[#c_atom{val=true}],
					      guard=[],
					      body=Lps ++ [Lc]},
				     #iclause{pats=[#c_atom{val=false}],
					      guard=[],
					      body=Mps ++ [Mc]}],
			    fc=Fc},
		     Fps,St4}
	    end
    end;
lc_tq(E, [], More, St) ->
    expr({cons,0,E,More}, St).

%% is_guard_test(Expression) -> true | false.
%%  Test if a general expression is a guard test.  Use erl_lint here
%%  as it now allows sys_pre_expand transformed source.

is_guard_test(E) -> erl_lint:is_guard_test(E).

%% novars(Expr, State) -> {Novars,[PreExpr],State}.
%%  Generate a novars expression, basically a call or a safe.  At this
%%  level we do not need to do a deep check.

novars(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Se,Sps,St2} = force_novars(E1, St1),
    {Se,Eps ++ Sps,St2}.

force_novars(#iapply{}=App, St) -> {App,[],St};
force_novars(#icall{}=Call, St) -> {Call,[],St};
force_novars(#iprimop{}=Prim, St) -> {Prim,[],St};
force_novars(#ifun{}=Fun, St) -> {Fun,[],St};	%These are novars too
force_novars(#c_binary{}=Bin, St) -> {Bin,[],St};
force_novars(Ce, St) ->
    force_safe(Ce, St).

%% safe(Expr, State) -> {Safe,[PreExpr],State}.
%%  Generate an internal safe expression.  These are simples without
%%  binaries which can fail.  At this level we do not need to do a
%%  deep check.  Must do special things with matches here.

safe(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Se,Sps,St2} = force_safe(E1, St1),
    {Se,Eps ++ Sps,St2}.

safe_list(Es, St) ->
    foldr(fun (E, {Ces,Esp,St0}) ->
		  {Ce,Ep,St1} = safe(E, St0),
		  {[Ce|Ces],Ep ++ Esp,St1}
	  end, {[],[],St}, Es).

force_safe(#imatch{pat=P,arg=E,fc=Fc}, St0) ->
    {Le,Lps,St1} = force_safe(E, St0),
    {Le,Lps ++ [#imatch{pat=P,arg=Le,fc=Fc}],St1};
%%force_safe(#ifun{}=Fun, St) ->
%%    {Fun,[],St};
force_safe(Ce, St0) ->
    case is_safe(Ce) of
	true -> {Ce,[],St0};
	false ->
	    {V,St1} = new_var(St0),
	    {V,[#iset{var=V,arg=Ce}],St1}
    end.

is_safe(#c_cons{}) -> true;
is_safe(#c_tuple{}) -> true;
is_safe(#c_binary{segs=Ss}) ->
    %% Be very conservative here, only allow things which are REALLY safe.
    {Bits,Safe} = foldl(fun (#c_bin_seg{val=#c_int{},size=#c_int{val=S},
					unit=U,type=integer}, {Bits,Safe}) ->
				Sb = S*U,
				{Bits + Sb,Safe};
			    (#c_bin_seg{val=#c_float{},size=#c_int{val=S},
					unit=U,type=float}, {Bits,Safe}) ->
				Sb = S*U,
				{Bits + Sb, Safe and (Sb == 64)};
			    (Seg, {Bits,Safe}) -> {Bits,false}
			end, {0,true}, Ss),
    Safe and ((Bits rem 8) == 0);
is_safe(#c_var{}) -> true;
is_safe(E) -> core_lib:is_atomic(E).

%% variable(Expr, State) -> {Variable,[PreExpr],State}.
%% force_variable(Expr, State) -> {Variable,[PreExpr],State}.
%%  Generate a variable.

variable(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {V,Vps,St2} = force_variable(E1, St1),
    {V,Eps ++ Vps,St2}.

force_variable(#c_var{}=Var, St) -> {Var,[],St}; 
force_variable(Ce, St0) ->
    {V,St1} = new_var(St0),
    {V,[#iset{var=V,arg=Ce}],St1}.

%% atomic(Expr, State) -> {Atomic,[PreExpr],State}.
%% force_atomic(Expr, State) -> {Atomic,[PreExpr],State}.

atomic(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {A,Aps,St2} = force_atomic(E1, St1),
    {A,Eps ++ Aps,St2}.

force_atomic(Ce, St0) ->
    case core_lib:is_atomic(Ce) of
	true -> {Ce,[],St0};
	false ->
	    {V,St1} = new_var(St0),
	    {V,[#iset{var=V,arg=Ce}],St1}
    end.

%% fold_match(MatchExpr, Pat) -> {MatchPat,Expr}.
%%  Fold nested matches into one match with aliased patterns.

fold_match({match,L,P0,E0}, P) ->
    {P1,E1} = fold_match(E0, P),
    {{match,L,P0,P1},E1};
fold_match(E, P) -> {P,E}.

%% pattern(Pattern) -> CorePat.
%% Transform a pattern by removing line numbers.  We also normalise
%% aliases in patterns to standard form, {alias,Pat,[Var]}.

pattern({var,L,V}) -> #c_var{name=V};
pattern({char,L,C}) -> #c_char{val=C};
pattern({integer,L,I}) -> #c_int{val=I};
pattern({float,L,F}) -> #c_float{val=F};
pattern({atom,L,A}) -> #c_atom{val=A};
pattern({string,L,S}) -> #c_string{val=S};
pattern({nil,L}) -> #c_nil{};
pattern({cons,L,H,T}) ->
    #c_cons{hd=pattern(H),tl=pattern(T)};
pattern({tuple,L,Ps}) ->
    #c_tuple{es=pattern_list(Ps)};
pattern({bin,L,Ps}) ->
    #c_binary{segs=pat_bin(Ps)};
pattern({match,L,P1,P2}) ->
    pat_alias(pattern(P1), pattern(P2)).

%% bin_pattern_list([BinElement]) -> [BinSeg].

pat_bin(Ps) -> map(fun pat_segment/1, Ps).

pat_segment({bin_element,L,Term,Size,[Type,{unit,Unit}|Flags]}) ->
    #c_bin_seg{val=pattern(Term),size=pattern(Size),
		unit=Unit,type=Type,flags=Flags}.

%% pat_alias(CorePat, CorePat) -> AliasPat.
%%  Normalise aliases.  Trap bad aliases by throwing 'nomatch'.

pat_alias(#c_var{name=V1}, P2) -> #c_alias{var=#c_var{name=V1},pat=P2};
pat_alias(P1, #c_var{name=V2}) -> #c_alias{var=#c_var{name=V2},pat=P1};
pat_alias(#c_cons{hd=H1,tl=T1}, #c_cons{hd=H2,tl=T2}) ->
    #c_cons{hd=pat_alias(H1, H2), tl=pat_alias(T1, T2)};
pat_alias(#c_tuple{es=Es1}, #c_tuple{es=Es2}) ->
    #c_tuple{es=pat_alias_list(Es1, Es2)};
pat_alias(#c_alias{var=V1,pat=P1},
	   #c_alias{var=V2,pat=P2}) ->
    if V1 == V2 -> pat_alias(P1, P2);
       true -> #c_alias{var=V1,pat=#c_alias{var=V2,pat=pat_alias(P1, P2)}}
    end;
pat_alias(#c_alias{var=V1,pat=P1}, P2) ->
    #c_alias{var=V1,pat=pat_alias(P1, P2)};
pat_alias(P1, #c_alias{var=V2,pat=P2}) ->
    #c_alias{var=V2,pat=pat_alias(P1, P2)};
pat_alias(P, P) -> P;
pat_alias(_, _) -> throw(nomatch).

%% pat_alias_list([A1], [A2]) -> [A].

pat_alias_list([A1|A1s], [A2|A2s]) ->
    [pat_alias(A1, A2)|pat_alias_list(A1s, A2s)];
pat_alias_list([], []) -> [].

%% pattern_list([P]) -> [P].

pattern_list(Ps) -> map(fun pattern/1, Ps).

%% first([A]) -> [A].
%% last([A]) -> A.

first([L]) -> [];
first([H|T]) -> [H|first(T)].

last([L]) -> L;
last([H|T]) -> last(T).

%% make_vars([Name]) -> [{Var,Name}].

make_vars(Vs) -> [ #c_var{name=V} || V <- Vs ].

%% new_fun_name(Type, State) -> {FunName,State}.

new_fun_name(Type, #core{fcount=C}=St) ->
    {list_to_atom(Type ++ "^" ++ integer_to_list(C)),St#core{fcount=C+1}}.

%% new_var_name(State) -> {VarName,State}.

new_var_name(#core{vcount=C}=St) ->
    {list_to_atom("cor" ++ integer_to_list(C)),St#core{vcount=C + 1}}.

%% new_var(State) -> {{var,Name},State}.

new_var(St0) ->
    {New,St1} = new_var_name(St0),
    {#c_var{name=New},St1}.

%% new_vars(Count, State) -> {[Var],State}.
%%  Make Count new variables.

new_vars(N, St) -> new_vars(N, St, []).

new_vars(N, St0, Vs) when N > 0 ->
    {V,St1} = new_var(St0),
    new_vars(N-1, St1, [V|Vs]);
new_vars(0, St, Vs) -> {Vs,St}.

fail_clause(Pats, A) ->
    #iclause{pats=Pats,guard=[],
	     body=[#iprimop{anno=#a{},name=#c_atom{val=match_fail},args=[A]}]}.

ubody(B, St) -> uexpr(B, [], St).

%% uclauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

uclauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> uclause(Lc, Ks, St) end, St0, Lcs).

%% uclause(Lclause, [KnownVar], State) -> {Lclause,State}.

uclause(Cl0, Ks, St0) ->
    {Cl1,Pvs,Used,New,St1} = uclause(Cl0, Ks, Ks, St0),
    A = #a{us=Used,ns=New},
    {Cl1#iclause{anno=A},St1}.

uclause(#iclause{pats=Ps0,guard=G0,body=B0}, Pks, Ks0, St0) ->
    {Ps1,Pg,Pvs,Pus,St1} = upattern_list(Ps0, Pks, St0),
    Pu = union(Pus, intersection(Pvs, Ks0)),
    Pn = subtract(Pvs, Pu),
    Ks1 = union(Pn, Ks0),
    {G1,St2} = uguard(Pg, G0, Ks1, St1),
    Gu = used_in_any(G1),
    Gn = new_in_any(G1),
    Ks2 = union(Gn, Ks1),
    {B1,St3} = uexprs(B0, Ks2, St2),
    Used = intersection(union([Pu,Gu,used_in_any(B1)]), Ks0),
    New = union([Pn,Gn,new_in_any(B1)]),
    {#iclause{pats=Ps1,guard=G1,body=B1},Pvs,Used,New,St3}.

%% uguard([Test], [Kexpr], [KnownVar], State) -> {[Kexpr],State}.
%%  Build a guard expression list by folding in the equality tests.

uguard([], [], Ks, St) -> {[],St};
uguard(Pg, [], Ks, St) ->
    %% No guard, so fold together equality tests.
    uguard(first(Pg), [last(Pg)], Ks, St);
uguard(Pg, Gs0, Ks, St0) ->
    %% Gs0 must contain at least one element here.
    {Gs3,St5} = foldr(fun (T, {Gs1,St1}) ->
			      {L,St2} = new_var(St1),
			      {R,St3} = new_var(St2),
			      {[#iset{var=L,arg=T}] ++ first(Gs1) ++
			       [#iset{var=R,arg=last(Gs1)},
				#icall{anno=#a{}, %Must have an #a{}
				       module=#c_atom{val=erlang},
				       name=#c_atom{val='and'},
				       args=[L,R]}],
			       St3}
		      end, {Gs0,St0}, Pg),
    %%ok = io:fwrite("core ~w: ~p~n", [?LINE,Gs3]),
    uexprs(Gs3, Ks, St5).

%% uexprs([Kexpr], [KnownVar], State) -> {[Kexpr],State}.

uexprs([#imatch{pat=P0,arg=Arg,fc=Fc}|Les], Ks, St0) ->
    %% Optimise for simple set of unbound variable.
    case upattern(P0, Ks, St0) of
	{#c_var{},[],Pvs,Pus,St1} ->
	    %% Throw our work away and just set to iset.
	    uexprs([#iset{var=P0,arg=Arg}|Les], Ks, St0);
	Other ->
	    %% Throw our work away and set to icase.
	    if
		Les == [] ->
		    %% Need to explicitly return match "value", make
		    %% safe for efficiency.
		    {La,Lps,St1} = force_safe(Arg, St0),
		    Mc = #iclause{pats=[P0],guard=[],body=[La]},
		    uexprs(Lps ++ [#icase{args=[La],clauses=[Mc],fc=Fc}], Ks, St1);
		true ->
		    Mc = #iclause{pats=[P0],guard=[],body=Les},
		    uexprs([#icase{args=[Arg],clauses=[Mc],fc=Fc}], Ks, St0)
	    end
    end;
uexprs([Le0|Les0], Ks, St0) ->
    {Le1,St1} = uexpr(Le0, Ks, St0),
    {Les1,St2} = uexprs(Les0, union((core_lib:get_anno(Le1))#a.ns, Ks), St1),
    {[Le1|Les1],St2};
uexprs([], Ks, St) -> {[],St}.

uexpr(#iset{var=V,arg=A0}, Ks, St0) ->
    {A1,St1} = uexpr(A0, Ks, St0),
    {#iset{anno=#a{us=del_element(V#c_var.name, (core_lib:get_anno(A1))#a.us),
		   ns=add_element(V#c_var.name, (core_lib:get_anno(A1))#a.ns)},
	   var=V,arg=A1},St1};
%% imatch done in uexprs.
uexpr(#iletrec{defs=Fs0,body=B0}, Ks, St0) ->
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{Fs0,B0}]),
    {Fs1,St1} = mapfoldl(fun ({Name,F0}, St0) ->
				 {F1,St1} = uexpr(F0, Ks, St0),
				 {{Name,F1},St1}
			 end, St0, Fs0),
    {B1,St2} = uexprs(B0, Ks, St1),
    Used = used_in_any(map(fun ({Name,F}) -> F end, Fs1) ++ B1),
    {#iletrec{anno=#a{us=Used,ns=[]},defs=Fs1,body=B1},St2};
uexpr(#icase{args=As0,clauses=Cs0,fc=Fc0}, Ks, St0) ->
    %% As0 will never generate new variables.
    {As1,St1} = uexpr_list(As0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Fc1,St3} = uclause(Fc0, Ks, St2),
    Used = union(used_in_any(As1), used_in_any(Cs1)),
    New = new_in_all(Cs1),
    {#icase{anno=#a{us=Used,ns=New},args=As1,clauses=Cs1,fc=Fc1},St3};
uexpr(#ifun{id=Id,vars=As,clauses=Cs0,fc=Fc0}, Ks0, St0) ->
    Avs = lit_list_vars(As),
    Ks1 = union(Avs, Ks0),
    {Cs1,St1} = ufun_clauses(Cs0, Ks1, St0),
    {Fc1,St2} = ufun_clause(Fc0, Ks1, St1),
    Used = subtract(intersection(used_in_any(Cs1), Ks0), Avs),
    {#ifun{anno=#a{us=Used,ns=[]},id=Id,vars=As,clauses=Cs1,fc=Fc1},St2};
uexpr(#iapply{op=Op,args=As}, Ks, St) ->
    Used = union(lit_vars(Op), lit_list_vars(As)),
    {#iapply{anno=#a{us=Used},op=Op,args=As},St};
uexpr(#iprimop{anno=A,name=Name,args=As}, Ks, St) ->
    Used = lit_list_vars(As),
    {#iprimop{anno=A#a{us=Used},name=Name,args=As},St};
uexpr(#icall{anno=A,module=Mod,name=Name,args=As}, Ks, St) ->
    Used = union([lit_vars(Mod),lit_vars(Name),lit_list_vars(As)]),
    {#icall{anno=A#a{us=Used},module=Mod,name=Name,args=As},St};
uexpr(#itry{body=Es0,clauses=Cs0}, Ks, St0) ->
    {Es1,St1} = uexprs(Es0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    Used = union([used_in_any(Cs1),used_in_any(Es1)]),
    New = intersection(new_in_all(Cs1), new_in_any(Es1)),
    {#itry{anno=#a{us=Used,ns=New},body=Es1,clauses=Cs1},St2};
uexpr(#icatch{body=Es0}, Ks, St0) ->
    {Es1,St1} = uexprs(Es0, Ks, St0),
    {#icatch{anno=#a{us=used_in_any(Es1)},body=Es1},St1};
uexpr(#ireceive1{clauses=Cs0}, Ks, St0) ->
    {Cs1,St1} = uclauses(Cs0, Ks, St0),
    {#ireceive1{anno=#a{us=used_in_any(Cs1),ns=new_in_all(Cs1)},
		clauses=Cs1},St1};
uexpr(#ireceive2{clauses=Cs0,timeout=Te0,action=Tes0}, Ks, St0) ->
    %% Te0 will never generate new variables.
    {Te1,St1} = uexpr(Te0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Tes1,St3} = uexprs(Tes0, Ks, St2),
    Used = union([used_in_any(Cs1),used_in_any(Tes1),
		  (core_lib:get_anno(Te1))#a.us]),
    New = intersection(new_in_all(Cs1), new_in_any(Tes1)),
    {#ireceive2{anno=#a{us=Used,ns=New},
		clauses=Cs1,timeout=Te1,action=Tes1},St2};
uexpr(#iprotect{body=Es0}, Ks, St0) ->
    {Es1,St1} = uexprs(Es0, Ks, St0),
    Used = used_in_any(Es1),
    {#iprotect{anno=#a{us=Used},body=Es1},St1};	%No new variables escape!
uexpr(Lit, Ks, St) ->
    case core_lib:is_simple(Lit) of
	true -> true;
	false ->
	    ok = io:fwrite("~w: ~p~n", [?LINE,Lit]),
	    true = false
    end,
    %%true = core_lib:is_simple(Lit),		%Sanity check!
    Vs = lit_vars(Lit),
    Anno = core_lib:get_anno(Lit),
    {core_lib:set_anno(Lit, #a{us=Vs,anno=Anno}),St}.

uexpr_list(Les0, Ks, St0) ->
    mapfoldl(fun (Le, St) -> uexpr(Le, Ks, St) end, St0, Les0).

%% ufun_clauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

ufun_clauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> ufun_clause(Lc, Ks, St) end, St0, Lcs).

%% ufun_clause(Lclause, [KnownVar], State) -> {Lclause,State}.

ufun_clause(Cl0, Ks, St0) ->
    {Cl1,Pvs,Used,New,St1} = uclause(Cl0, [], Ks, St0),
    A = #a{us=subtract(intersection(Used, Ks), Pvs),ns=[]},
    {Cl1#iclause{anno=A},St1}.

%% upattern(Pat, [KnownVar], State) ->
%%              {Pat,[GuardTest],[NewVar],[UsedVar],State}.

upattern(#c_var{name='_'}, Ks, St0) ->
    {New,St1} = new_var_name(St0),
    {#c_var{name=New},[],[New],[],St1};
upattern(#c_var{name=V}=Var, Ks, St0) ->
    case is_element(V, Ks) of
	true ->
	    {N,St1} = new_var_name(St0),
	    New = #c_var{name=N},
	    Test = #icall{anno=#a{us=add_element(N, [V])},
			  module=#c_atom{val=erlang},
			  name=#c_atom{val='=:='},
			  args=[New,Var]},
	    %% Test doesn't need protecting.
	    {New,[Test],[N],[],St1};
	false -> {Var,[],[V],[],St0}
    end;
upattern(#c_cons{hd=H0,tl=T0}, Ks, St0) ->
    {H1,Hg,Hv,Hu,St1} = upattern(H0, Ks, St0),
    {T1,Tg,Tv,Tu,St2} = upattern(T0, union(Hv, Ks), St1),
    {#c_cons{hd=H1,tl=T1},Hg ++ Tg,union(Hv, Tv),union(Hu, Tu),St2};
upattern(#c_tuple{es=Es0}, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upattern_list(Es0, Ks, St0),
    {#c_tuple{es=Es1},Esg,Esv,Eus,St1};
upattern(#c_binary{segs=Es0}, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upat_bin(Es0, Ks, St0),
    {#c_binary{segs=Es1},Esg,Esv,Eus,St1};
upattern(#c_alias{var=V0,pat=P0}, Ks, St0) ->
    {V1,Vg,Vv,Vu,St1} = upattern(V0, Ks, St0),
    {P1,Pg,Pv,Pu,St2} = upattern(P0, union(Vv, Ks), St1),
    {#c_alias{var=V1,pat=P1},Vg ++ Pg,union(Vv, Pv),union(Vu, Pu),St2};
upattern(Other, Ks, St) -> {Other,[],[],[],St}.	%Constants

%% upattern_list([Pat], [KnownVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.

upattern_list([P0|Ps0], Ks, St0) ->
    {P1,Pg,Pv,Pu,St1} = upattern(P0, Ks, St0),
    {Ps1,Psg,Psv,Psu,St2} = upattern_list(Ps0, union(Pv, Ks), St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upattern_list([], Ks, St) -> {[],[],[],[],St}.    

%% upat_bin([Pat], [KnownVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.

upat_bin([P0|Ps0], Ks, St0) ->
    {P1,Pg,Pv,Pu,St1} = upat_element(P0, Ks, St0),
    {Ps1,Psg,Psv,Psu,St2} = upat_bin(Ps0, union(Pv, Ks), St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upat_bin([], Ks, St) -> {[],[],[],[],St}.    

upat_element(#c_bin_seg{val=H0,size=Sz}=Cons, Ks, St0) ->
    {H1,Hg,Hv,[],St1} = upattern(H0, Ks, St0),
    Us = case Sz of
	     #c_var{name=Vname} -> [Vname];
	     Other -> []
	 end,
    {Cons#c_bin_seg{val=H1},Hg,Hv,Us,St1}.

used_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((core_lib:get_anno(Le))#a.us, Ns) end,
	  [], Les).

new_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((core_lib:get_anno(Le))#a.ns, Ns) end,
	  [], Les).

new_in_all([Le|Les]) ->
    foldl(fun (L, Ns) -> intersection((core_lib:get_anno(L))#a.ns, Ns) end,
	  (core_lib:get_anno(Le))#a.ns, Les);
new_in_all([]) -> [].

%% The AfterVars are the variables which are used afterwards.  We need
%% this to work out which variables are actually exported and used
%% from case/receive.  In subblocks/clauses the AfterVars of the block
%% are just the exported variables.

cbody(B0, St0) ->
    {B1,Es,As,St1} = cexpr(B0, [], St0),
    {B1,St1}.
    %%case catch cexpr(B0, [], St0) of
%%	{B1,Es,Us,St1} -> {B1,St1};
%%	{'EXIT',R} -> {{R,B0},St0}
%%    end.

%% cclause(Lclause, [AfterVar], State) -> {Cclause,State}.
%%  The AfterVars are the exported variables.

cclause(#iclause{pats=Ps,guard=G0,body=B0}, Exp, St0) ->
    {B1,Us1,St1} = cexprs(B0, Exp, St0),
    {G1,St2} = cguard(G0, St1),
    {#c_clause{pats=Ps,guard=G1,body=B1},St2}.

cclauses(Lcs, Es, St0) ->
    mapfoldl(fun (Lc, St) -> cclause(Lc, Es, St) end, St0, Lcs).

cguard([], St) -> {#c_atom{val=true},St};
cguard(Gs, St0) ->
    {G,_,St1} = cexprs(Gs, [], St0),
    {G,St1}.
 
%% cexprs([Lexpr], [AfterVar], State) -> {Cexpr,[AfterVar],State}.
%%  Must be sneaky here at the last expr when combining exports for the
%%  whole sequence and exports for that expr.

cexprs([#iset{var=#c_var{name=Name}=Var}=Iset], As, St) ->
    %% Make return value explicit,, and make Var true top level.
    cexprs([Iset,Var#c_var{anno=#a{us=[Name]}}], As, St);
cexprs([Le], As, St0) ->
    {Ce,Es,Us,St1} = cexpr(Le, As, St0),
    Exp = make_vars(As),			%The export variables
    if
	Es == [] -> {core_lib:make_values([Ce|Exp]),union(Us, As),St1};
	true ->
	    {R,St2} = new_var(St1),
	    {#c_let{vars=[R|make_vars(Es)],arg=Ce,
		    body=core_lib:make_values([R|Exp])},
	     union(Us, As),St2}
    end;
cexprs([#iset{var=V,arg=A0}|Les], As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, As0, St0),
    {A1,Es,Us,St2} = cexpr(A0, As1, St1),
    {#c_let{vars=[V|make_vars(Es)],arg=A1,body=Ces},
     union(Us, As1),St2};
cexprs([Le|Les], As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, As0, St0),
    {Ce,Es,Us,St2} = cexpr(Le, As1, St1),
    if
	Es == [] -> {#c_seq{arg=Ce,body=Ces},union(Us, As1),St2};
	true ->
	    {R,St3} = new_var(St2),
	    {#c_let{vars=[R|make_vars(Es)],arg=Ce,body=Ces},
	     union(Us, As1),St3}
    end.

%% cexpr(Lexpr, [AfterVar], State) -> {Cexpr,[ExpVar],[UsedVar],State}.

cexpr(#iletrec{anno=A,defs=Fs0,body=B0}, As, St0) ->
    {Fs1,{Used,St1}} = mapfoldl(fun ({Name,F0}, {Used,St0}) ->
					{F1,[],Us,St1} = cexpr(F0, [], St0),
					{#c_def{name=#c_fname{id=Name,arity=1},
						val=F1},
					 {union(Us, Used),St1}}
				end, {[],St0}, Fs0),
    Exp = intersection(A#a.ns, As),
    {B1,Us,St2} = cexprs(B0, Exp, St1),
    {#c_letrec{defs=Fs1,body=B1},Exp,A#a.us,St2};
cexpr(#icase{anno=A,args=Largs,clauses=Lcs,fc=Lfc}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Cargs,St1} = foldr(fun (La, {Cas,Sta}) ->
				{Ca,[],Us1,Stb} = cexpr(La, As, Sta),
				{[Ca|Cas],Stb}
			end, {[],St0}, Largs),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Cfc,St3} = cclause(Lfc, [], St2),		%Never exports
    {#c_case{arg=core_lib:make_values(Cargs),clauses=Ccs ++ [Cfc]},
     Exp,A#a.us,St3};
cexpr(#ireceive1{anno=A,clauses=Lcs}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Ccs,St1} = cclauses(Lcs, Exp, St0),
    {#c_receive{clauses=Ccs,
	       timeout=#c_atom{val=infinity},action=#c_atom{val=true}},
     Exp,A#a.us,St1};
cexpr(#ireceive2{anno=A,clauses=Lcs,timeout=Lto,action=Les}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Cto,[],Us1,St1} = cexpr(Lto, As, St0),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Ces,Us2,St3} = cexprs(Les, Exp, St2),
    {#c_receive{clauses=Ccs,timeout=Cto,action=Ces},
     Exp,A#a.us,St3};
cexpr(#itry{anno=A,body=Les,clauses=Lcs}, As, St0) ->
    Exp = intersection(A#a.ns, As),           %Exports
    {Ces,Us1,St1} = cexprs(Les, Exp, St0),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Vs,St3} = new_vars(2, St2),
    C = #c_clause{pats=[#c_tuple{es=Vs}],guard=#c_atom{val=true},
		  body=#c_primop{name=#c_atom{val=raise},args=Vs}},
    {#c_try{expr=Ces,vars=Vs,
	    body=#c_case{arg=#c_tuple{es=Vs},clauses=Ccs ++ [C]}},
     [],A#a.us,St3};
cexpr(#icatch{anno=A,body=Les}, As, St0) ->
    {Ces,Us1,St1} = cexprs(Les, [], St0),	%Never export!
    {#c_catch{body=Ces},[],A#a.us,St1};
cexpr(#ifun{anno=A,id=Id,vars=Args,clauses=Lcs,fc=Lfc}, As, St0) ->
    {Ccs,St1} = cclauses(Lcs, [], St0),		%NEVER export!
    {Cfc,St2} = cclause(Lfc, [], St1),
    {#c_fun{anno=Id,vars=Args,
	    body=#c_case{arg=core_lib:make_values(Args),clauses=Ccs ++ [Cfc]}},
     [],A#a.us,St2};
cexpr(#iapply{anno=A,op=Op,args=Args}, As, St) ->
    {#c_apply{anno=A#a.anno,op=Op,args=Args},[],A#a.us,St};
cexpr(#icall{anno=A,module=Mod,name=Name,args=Args}, As, St) ->
    {#c_call{anno=A#a.anno,module=Mod,name=Name,args=Args},[],A#a.us,St};
cexpr(#iprimop{anno=A,name=Name,args=Args}, As, St) ->
    {#c_primop{anno=A#a.anno,name=Name,args=Args},[],A#a.us,St};
cexpr(#iprotect{anno=A,body=Es}, As, St0) ->
    {Ce,_,St1} = cexprs(Es, [], St0),
    Vs = [#c_var{name='T'},#c_var{name='R'}],   % the names are arbitrary
    {#c_try{expr=Ce,vars=Vs,body=#c_atom{val=false}},[],A#a.us,St1};
cexpr(Lit, As, St) ->
    case core_lib:is_simple(Lit) of
	true -> true;
	false ->
	    ok = io:fwrite("~w: ~p~n", [?LINE,Lit]),
	    true = false
    end,
    %%true = core_lib:is_simple(Lit),		%Sanity check!
    Anno = core_lib:get_anno(Lit),
    Vs = Anno#a.us,
    %%Vs = lit_vars(Lit),
    {core_lib:set_anno(Lit, Anno#a.anno),[],Vs,St}.

%% lit_vars(Literal) -> [Var].

lit_vars(Lit) -> lit_vars(Lit, []).

lit_vars(#c_cons{hd=H,tl=T}, Vs) -> lit_vars(H, lit_vars(T, Vs));
lit_vars(#c_tuple{es=Es}, Vs) -> lit_list_vars(Es, Vs);
lit_vars(#c_binary{segs=Ss}, Vs) -> lit_bin_vars(Ss, Vs);
lit_vars(#c_var{name=V}, Vs) -> add_element(V, Vs); 
lit_vars(Other, Vs) -> Vs.			%These are atomic

lit_bin_vars(Segs, Vs) ->
    foldl(fun (#c_bin_seg{val=V,size=S}, Vs0) ->
		  lit_vars(V, lit_vars(S, Vs0))
	  end, Vs, Segs).

lit_list_vars(Ls) -> lit_list_vars(Ls, []).

lit_list_vars(Ls, Vs) ->
    foldl(fun (L, Vs0) -> lit_vars(L, Vs0) end, Vs, Ls).
