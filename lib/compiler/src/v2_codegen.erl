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
%% Purpose : Code generator for Beam.

%% The following assumptions have been made:
%%
%% 1. Scopes, i.e. things with {scope,M,Ret} wrappers, only return
%% values; no variables are exported. If the scope would have returned
%% extra variables then these have been transformed to multiple return
%% values.
%% 
%% 2. All BIF's called in guards are gc-safe so there is no need to
%% put thing on the stack in the guard.  While this would in principle
%% work it would be difficult to keep track of the stack depth when
%% trimming.
%%
%% The code generation uses variable lifetime information added by
%% the sys_life module to save variables, allocate registers and 
%% move registers to the stack when necessary.

%% -include("sys_kernel.hrl").

%% -deftype akexpr(A) = anno{kexpr(),A}.
%% -author('rv@cslab.ericsson.se').

-module(v2_codegen).


%% The main interface.
-export([module/2]).

-import(lists, [keymember/3,keysort/2,keysearch/3,append/1,
		map/2,flatmap/2,foldl/3,foldr/3,mapfoldl/3,reverse/1,reverse/2]).
-import(v2_life, [vdb_find/2]).

%%-compile([export_all]).

-include("sys_life.hrl").

%% Main codegen structure.
-record(cg, {lcount=1,				%Label counter
	     mod,				%Current module
	     func,				%Current function
	     finfo,				%Function info label
	     fcode,				%Function code label
	     btype,				%Type of bif used.
	     bfail,				%Fail label of bif
	     break,				%Break label
	     recv,				%Receive label
	     is_top_block,			%Boolean: top block or not
	     functable = [],			%Table of local functions:
						%[{{Name, Arity}, Label}...]
	     in_catch=false,			%Inside a catch or not.
	     calls=0}).				%Number of function calls.

%% Stack/register state record.
-record(sr, {reg=[],				%Register table
	     stk=[],				%Stack table
	     res=[]}).				%Reserved regs: [{reserved, I, V}]

module({Mod,Exp,Attr,Forms}, Opts) ->
    {Fs,St} = functions(Forms, Opts, #cg{mod=Mod}),
    {ok,{Mod,Exp,Attr,Fs,St#cg.lcount}}.

functions(Forms, Options, St0) ->
    mapfoldl(fun (F, St) -> function(F, St) end, St0#cg{lcount=1}, Forms).

function({function,Name,Arity,As0,Vb,Vdb}, St0) ->
    St1 = St0#cg{func={Name,Arity}},
    {Fun,St2} = cg_fun(Vb, As0, Vdb, St1),
    {{function,Name,Arity,St2#cg.fcode,Fun},St2};
function({asm,Name,Arity,Code0}, St0) ->
    {Fi,St1} = new_label(St0),
    {Fl,St2} = new_label(St1),
    Code = [{label,Fi},{func_info,{atom,St2#cg.mod},{atom,Name},Arity},
	    {label,Fl}|Code0],
    {{function,Name,Arity,Fl,Code},St2}.

%% cg_fun([Lkexpr], [HeadVar], Vdb, St) -> {Asm,St}

cg_fun(Les, Hvs, Vdb, St0) ->
    {Name,Arity} = St0#cg.func,
    {Fi,St1} = new_label(St0),			%FuncInfo label
    {{f,Fl},St2} = func_id(call, Name, Arity, St1),
    %% Create initial stack/register state, clear unused arguments.
    Bef = clear_dead(#sr{reg=foldl(fun ({var,V}, Reg) ->
					   put_reg_from(V, 0, Reg)
				   end, [], Hvs),
			stk=[]}, 0, Vdb),
    {B2,Aft,St3} = cg_list(Les, 0, Vdb, Bef, St2#cg{btype=exit,
						    finfo=Fi,
						    fcode=Fl,
						    is_top_block=true}),
    A = [{label,Fi},{func_info,{atom,St3#cg.mod},{atom,Name},Arity},
	 {label,Fl}|B2],
    {A,St3}.

%% cg(Lkexpr, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Generate code for a kexpr.
%%  Split function into two steps for clarity, not efficiency.

cg(Le, Vdb, Bef, St) ->
    cg(Le#l.ke, Le, Vdb, Bef, St).

cg({scope,M,Rs}, Le, Vdb, Bef, St) ->
    scope_cg(M, Rs, Le, Vdb, Bef, St);
cg({call,Func,As,Rs}, Le, Vdb, Bef, St) ->
    call_cg(Func, As, Rs, Le, Vdb, Bef, St);
cg({enter,Func,As}, Le, Vdb, Bef, St) ->
    enter_cg(Func, As, Le, Vdb, Bef, St);
cg({bif,Bif,As,Rs}, Le, Vdb, Bef, St) ->
    bif_cg(Bif, As, Rs, Le, Vdb, Bef, St);
cg({receive_loop,Te,Rvar,Rm,Tes,Rs}, Le, Vdb, Bef, St) ->
    recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, Vdb, Bef, St);
cg(receive_next, Le, Vdb, Bef, St) ->
    recv_next_cg(Le, Vdb, Bef, St);
cg(receive_accept, Le, Vdb, Bef, St) -> {[remove_message],Bef,St};
cg(receive_reject, Le, Vdb, Bef, St) -> {[],Bef,St};
cg({'catch',Cb,R}, Le, Vdb, Bef, St) ->
    catch_cg(Cb, R, Le, Vdb, Bef, St);
cg({set,Var,Con}, Le, Vdb, Bef, St) -> set_cg(Var, Con, Le, Vdb, Bef, St);
cg({test,Test,As}, Le, Vdb, Bef, St) ->
    test_cg(Test, As, Le, Vdb, Bef, St);
cg({return,Rs}, Le, Vdb, Bef, St) -> return_cg(Rs, Le, Vdb, Bef, St);
cg({break,Bs}, Le, Vdb, Bef, St) -> break_cg(Bs, Le, Vdb, Bef, St);
cg({need_heap,0}, Le, Vdb, Bef, St) ->
    {[],Bef,St};
cg({need_heap,H}, Le, Vdb, Bef, St) ->
    {[{test_heap,H,max_reg(Bef#sr.reg)}],Bef,St}.

%% cg_list([Kexpr], FirstI, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

cg_list(Kes, I, Vdb, Bef, St0) ->
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun (Ke, {Inta,Sta}) ->
%			     io:fwrite("  %% ~p\n", [Inta]),
			     {Keis,Intb,Stb} = cg(Ke, Vdb, Inta, Sta),
%			     io:fwrite("  ~p\n", [Keis]),
%			     io:fwrite("  %% ~p\n", [Intb]),
			     {comment(Inta) ++ Keis,{Intb,Stb}}
		     end, {Bef,St0}, need_heap(Kes, I, St0#cg.btype)),
    {Keis,Aft,St1}.

%% need_heap([Lkexpr], I, BifType) -> [Lkexpr].
%%  Insert need_heap instructions in Kexpr list.  Try to be smart and
%%  collect them together as much as possible.

need_heap(Kes0, I, BifType) ->
    {Kes1,{H,F}} = flatmapfoldr(fun (Ke, {H0,F0}) ->
				    {Ns,H1,F1} = need_heap(Ke, H0, F0, BifType),
				    {[Ke|Ns],{H1,F1}}
			    end, {0,false}, Kes0),
    %% Prepend need_heap if necessary.
    Kes2 = need_heap_need(I, H, F) ++ Kes1,
%    io:fwrite("need_heap: ~p~n",
%	      [{{H,F},
%		map(fun (#l{ke={scope,M,Rs}}) -> scope;
%			(Lke) -> Lke#l.ke end, Kes2)}]),
    Kes2.

need_heap(#l{ke={set,V,Val}}, H, F, BifType) ->
    %% Just pass through adding to needed heap.
    {[],H + case Val of
		{cons,Es} -> 2;
		{tuple,Es} -> 1 + length(Es);
		{string,S} -> 2 * length(S);
		Other -> 0
	    end,F};
need_heap(#l{ke={call,Func,As,Rs},i=I}, H, F, BifType) ->
    %% Calls generate a need if necessary and also force one.
    {need_heap_need(I, H, F),0,true};
need_heap(#l{ke={bif,Bif,As,Rs},i=I}, H, F, BifType) ->
    {[],H,F};
need_heap(#l{i=I}, H, F, BifType) ->
    %% Others kexprs generate a need if necessary but don't force.
    {need_heap_need(I, H, F),0,false}.

need_heap_need(I, 0, false) -> [];
need_heap_need(I, H, F) -> [#l{ke={need_heap,H},i=I}].


%% scope_cg(Match, [Ret], I, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Generate code for a scope.  First save all variables on the stack
%%  that are to survive after the scope.  We leave saved variables in
%%  their registers as they might actually be in the right place.
%%  Should test this.

scope_cg(M, Rs, Le, Vdb, Bef, St0) ->
    I = Le#l.i,
    {Saves,Sis,Int0} = adjust_stack(Bef, I, I+1, Vdb),
    {B,St1} = new_label(St0),
    {Mis,Int1,St2} = match_cg(M, none, Int0, St1#cg{break=B}),
    %% Put return values in registers.
    Reg = load_vars(Rs, Int1#sr.reg),
    {Sis ++ Mis ++ [{label,B}],
     clear_dead(Int1#sr{reg=Reg}, I, Vdb),
     St2#cg{break=St1#cg.break}}.

%% match_cg(Match, Fail, Bef, St) -> {[Ainstr],Aft,St}.
%%  Generate code for a match tree.  N.B. there is no need pass Vdb
%%  down as each level which uses this takes its own internal Vdb not
%%  the outer one.

match_cg(Le, Fail, Bef, St) ->
    match_cg(Le#l.ke, Le, Fail, Bef, St).

match_cg({try,F,S}, Le, Fail, Bef, St0) ->
    {Tf,St1} = new_label(St0),
    {Fis,Faft,St2} = match_cg(F, Tf, Bef, St1),
    {Sis,Saft,St3} = match_cg(S, Fail, Bef, St2),
    Aft = sr_merge(Faft, Saft),
    {Fis ++ [{label,Tf}] ++ Sis,Aft,St3};
match_cg({select,V,Scs}, Va, Fail, Bef, St) ->
    match_fmf(fun (S, F, Sta) ->
		      select_cg(S, V, Va#l.vdb, F, Fail, Bef, Sta) end,
	      Fail, St, Scs);
match_cg({guard,Gcs}, Le, Fail, Bef, St) ->
    match_fmf(fun (G, F, Sta) -> guard_cg(G, F, Bef, Sta) end,
	      Fail, St, Gcs);
match_cg({block,Es}, Le, Fail, Bef, St0) ->
    %% Must clear registers and stack of dead variables.
    Int = clear_dead(Bef, Le#l.i, Le#l.vdb),
    case St0#cg.is_top_block of
	false ->
	    block_cg(Es, Le#l.i, Le#l.vdb, Int,
		     St0#cg{btype=exit,bfail=St0#cg.finfo});
	true ->
	    {Keis,Aft,St1} = block_cg(Es, Le#l.i, Le#l.vdb, Int,
				      St0#cg{btype=exit,bfail=St0#cg.finfo,
					     is_top_block=false,calls=0}),
	    top_level_block(Keis, Aft, max_reg(Int#sr.reg), St1)
    end;
match_cg({match_fail,function_clause}, Le, Fail, Bef, St) ->
    Int0 = clear_dead(Bef, Le#l.i, Le#l.vdb),
    {Saves,Sis,Int1} = adjust_stack(Int0, Le#l.i, Le#l.i+1, Le#l.vdb),
    {Sis++[{jump,{f,St#cg.finfo}}], Int1#sr{reg=clear_regs(Int1#sr.reg)}, St};
match_cg({match_fail,{badmatch,Term}}, Le, Fail, Bef, St) ->
    {R,Ms,Int0} = cg_reg_arg(Term, Bef),
    Int1 = clear_dead(Int0, Le#l.i, Le#l.vdb),
    {Saves,Sis,Int2} = adjust_stack(Int1, Le#l.i, Le#l.i+1, Le#l.vdb),
    {Ms ++ Sis ++ [{badmatch,R}],
     Int2#sr{reg=clear_regs(Int0#sr.reg)},St};
match_cg({match_fail,{case_clause, Reason}}, Le, Fail, Bef, St) ->
    {R,Ms,Int0} = cg_reg_arg(Reason, Bef),
    Int1 = clear_dead(Int0, Le#l.i, Le#l.vdb),
    {Saves,Sis,Int2} = adjust_stack(Int1, Le#l.i, Le#l.i+1, Le#l.vdb),
    {Ms ++ Sis ++ [{case_end,R}],
     Int2#sr{reg=clear_regs(Int0#sr.reg)},St};
match_cg({match_fail,if_clause}, Le, Fail, Bef, St) ->
    Int0 = clear_dead(Bef, Le#l.i, Le#l.vdb),
    {Saves,Sis,Int1} = adjust_stack(Int0, Le#l.i, Le#l.i+1, Le#l.vdb),
    {Sis++[if_end], Int1#sr{reg=clear_regs(Int1#sr.reg)}, St}.

block_cg([], I, Vdb, Bef, St0) ->
    {[],Bef,St0};
block_cg(Kes0, I, Vdb, Bef, St0) ->
    {Kes2,Int1,St1} =
	case basic_block(Kes0) of
	    {Kes1,LastI,Args,Rest} ->
		Ke = hd(Kes1),
		Fb = Ke#l.i,
		cg_basic_block(Kes1, Fb, LastI, Args, Vdb, Bef, St0);
	    {Kes1,Rest} ->
		cg_list(Kes1, I, Vdb, Bef, St0)
	end,
    {Kes3,Int2,St2} = block_cg(Rest, I, Vdb, Int1, St1),
    {Kes2 ++ Kes3,Int2,St2}.

basic_block(Kes) -> basic_block(Kes, []).

basic_block([], Acc) -> {reverse(Acc),[]};
basic_block([Le|Les], Acc) ->
    case collect_block(Le#l.ke) of
	include -> basic_block(Les, [Le|Acc]);
	{block_end,As} -> {reverse(Acc, [Le]),Le#l.i,As,Les};
	no_block -> {reverse(Acc, [Le]),Les}
    end.
	
collect_block({set,Var,Con})        -> include;
collect_block({call,{var,V},As,Rs}) -> {block_end,As++[{var,V}]};
collect_block({call,Func,As,Rs})    -> {block_end,As};
collect_block({enter,{var,V},As})   -> {block_end,As++[{var,V}]};
collect_block({enter,Func,As})      -> {block_end,As};
collect_block({return,Rs})          -> {block_end,Rs};
collect_block({break,Bs})           -> {block_end,Bs};
collect_block({bif,Bif,As,Rs})      -> include;
collect_block(Other) -> no_block.

%% cg_basic_block([Kexpr], FirstI, LastI, As, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

cg_basic_block(Kes, Fb, Lf, As, Vdb, Bef, St0) ->
    Res = make_reservation(As, 0),
    Regs0 = reserve(Res, Bef#sr.reg, Bef#sr.stk),
    Stk = extend_stack(Bef, Lf, Lf+1, Vdb),
    Int0 = Bef#sr{reg=Regs0,stk=Stk,res=Res},
    X0_v0 = x0_vars(As, Fb, Lf, Vdb),
    {Keis,{Aft,X0_final,St1}} =
	flatmapfoldl(fun(Ke, St) -> cg_basic_block(Ke, St, Lf, Vdb) end,
		     {Int0,X0_v0,St0}, need_heap(Kes, Fb, St0#cg.btype)),
    {Keis,Aft,St1}.

cg_basic_block(Ke, {Inta,X0v,Sta}, Lf, Vdb) when element(1, Ke#l.ke) =:= need_heap ->
    {Keis,Intb,Stb} = cg(Ke, Vdb, Inta, Sta),
    {comment(Inta) ++ Keis, {Intb,X0v,Stb}};
cg_basic_block(Ke, {Inta,X0_v1,Sta}, Lf, Vdb) ->
    {Sis,Intb} = save_carefully(Inta, Ke#l.i, Lf+1, Vdb),
    {X0_v2,Intc} = allocate_x0(X0_v1, Ke#l.i, Intb),
    Intd = reserve(Intc),
    {Keis,Inte,Stb} = cg(Ke, Vdb, Intd, Sta),
    {comment(Inta) ++ Sis ++ Keis, {Inte,X0_v2,Stb}}.

make_reservation([], I) -> [];
make_reservation([{var,V}|As], I) -> [{I,V}|make_reservation(As, I+1)];
make_reservation([A|As], I) -> [{I,A}|make_reservation(As, I+1)].

reserve(Sr) -> Sr#sr{reg=reserve(Sr#sr.res, Sr#sr.reg, Sr#sr.stk)}.

reserve([{I,V}|Rs], [free|Regs], Stk) -> [{reserved,I,V}|reserve(Rs, Regs, Stk)];
reserve([{I,V}|Rs], [{I,V}|Regs], Stk) -> [{I,V}|reserve(Rs, Regs, Stk)];
reserve([{I,V}|Rs], [{I,Var}|Regs], Stk) ->
    case on_stack(Var, Stk) of
	true -> [{reserved,I,V}|reserve(Rs, Regs, Stk)];
	false -> [{I,Var}|reserve(Rs, Regs, Stk)]
    end;
reserve([{I,V}|Rs], [{reserved,I,_}|Regs], Stk) ->
    [{reserved,I,V}|reserve(Rs, Regs, Stk)];
%reserve([{I,V}|Rs], [Other|Regs], Stk) -> [Other|reserve(Rs, Regs, Stk)];
reserve([{I,V}|Rs], [], Stk) -> [{reserved,I,V}|reserve(Rs, [], Stk)];
reserve([], Regs, Stk) -> Regs.

extend_stack(Bef, Fb, Lf, Vdb) ->
    Stk0 = clear_dead_stk(Bef#sr.stk, Fb, Vdb),
    Saves = [V || {V,F,L} <- Vdb,
		  F < Fb,
		  L >= Lf,
		  not on_stack(V, Stk0)],
    Stk1 = foldl(fun (V, Stk) -> put_stack(V, Stk) end, Stk0, Saves),
    Bef#sr.stk ++ lists:duplicate(length(Stk1) - length(Bef#sr.stk), free).

save_carefully(Bef, Fb, Lf, Vdb) ->
    Stk = Bef#sr.stk,
    %% New variables that are in use but not on stack.
    New = [ Vd || {V,F,L}=Vd <- Vdb,
		   F < Fb,
		   L >= Lf,
		   not on_stack(V, Stk) ],
    Saves = [ V || {V,F,L} <- keysort(2, New) ],
    save_carefully(Saves, Bef, []).

save_carefully([], Bef, Acc) -> {reverse(Acc),Bef};
save_carefully([V|Vs], Bef, Acc) ->
    case put_stack_carefully(V, Bef#sr.stk) of
	error -> {reverse(Acc),Bef};
	Stk1 ->
	    SrcReg = fetch_reg(V, Bef#sr.reg),
	    Move = {move,SrcReg,fetch_stack(V, Stk1)},
	    {x,I} = SrcReg,
	    save_carefully(Vs, Bef#sr{stk=Stk1}, [Move|Acc])
    end.

x0_vars([], Fb, Lf, Vdb) ->
    [];
x0_vars([{var,V}|_], Fb, Lf, Vdb) ->
    {V,F,L} = vdb_find(V, Vdb),
    x0_vars1([{V,F,L}], Fb, F, Vdb);
x0_vars([X0|_], Fb, Lf, Vdb) ->
    x0_vars1([{X0,Lf,Lf}], Fb, Lf, Vdb).

x0_vars1(X0, Fb, Xf, Vdb) ->
    Vs0 = [ {V,F,L} || {V,F,L} <- Vdb,
		      F >= Fb,
		      L < Xf ],
    Vs1 = keysort(3, Vs0),
    keysort(2, X0++Vs1).

allocate_x0([], I, Bef) -> {[],Bef#sr{res=[]}};
allocate_x0([{V,F,L}|Vs], I, Bef) when L =< I ->
    allocate_x0(Vs, I, Bef);
allocate_x0([{V,F,L}|Vs], I, Bef) ->
    {[{V,F,L}|Vs],Bef#sr{res=reserve_x0(V, Bef#sr.res)}}.

reserve_x0(V, [_|Res]) -> [{0,V}|Res];
reserve_x0(V, []) -> [{0,V}].

top_level_block(Keis, Bef, MaxRegs, St0) when length(Bef#sr.stk)+St0#cg.calls == 0 ->
    %% This block need no stack frame.  However, we still need to turn the
    %% stack frame upside down.
    MaxY = length(Bef#sr.stk)-1,
    Keis1 = flatmap(fun (Tuple) when tuple(Tuple) ->
			    [turn_yregs(size(Tuple), Tuple, MaxY)];
			(Other) ->
			    [Other]
		    end, Keis),
    {Keis1, Bef, St0#cg{is_top_block=true}};
top_level_block(Keis, Bef, MaxRegs, St0) ->
    %% This top block needs an allocate instruction before it, and a
    %% deallocate instruction before each return.
    FrameSz = length(Bef#sr.stk),
    MaxY = FrameSz-1,
    Keis1 = flatmap(fun ({call_only,Arity,Func}) ->
			    [{call_last,Arity,Func,FrameSz}];
			({call_ext_only,Arity,Func}) ->
			    [{call_ext_last,Arity,Func,FrameSz}];
			(return) ->
			    [{deallocate,FrameSz}, return];
			(Tuple) when tuple(Tuple) ->
			    [turn_yregs(size(Tuple), Tuple, MaxY)];
			(Other) ->
			    [Other]
		    end, Keis),
    {[{allocate_zero,FrameSz,MaxRegs}|Keis1], Bef, St0#cg{is_top_block=true}}.

%% turn_yregs(Size, Tuple, MaxY) -> Tuple'
%%   Renumber y register so that {y, 0} becomes {y, FrameSize-1},
%%   {y, FrameSize-1} becomes {y, 0} and so on.  This is to make nested
%%   catches work.  The code generation algorithm gives a lower register
%%   number to the outer catch, which is wrong.

turn_yregs(0, Tp, MaxY) ->
    Tp;
turn_yregs(El, Tp, MaxY) when element(1, element(El, Tp)) == yy ->
    turn_yregs(El-1, setelement(El, Tp, {y,MaxY-element(2, element(El, Tp))}), MaxY);
turn_yregs(El, Tp, MaxY) when list(element(El, Tp)) ->
    New = map(fun ({yy,YY}) -> {y,MaxY-YY};
		  (Other) -> Other end, element(El, Tp)),
    turn_yregs(El-1, setelement(El, Tp, New), MaxY);
turn_yregs(El, Tp, MaxY) ->
    turn_yregs(El-1, Tp, MaxY).

%% select_cg(Sclause, V, Vdb, TypeFail, ValueFail, Bef, St) -> {Is,Aft,St}.
%%  Selecting type and value needs two failure labels, TypeFail is the
%%  label to jump to of the next type test when this type fails, and
%%  ValueFail is the label when this type is correct but the value is
%%  wrong.  These are different as in the second case there is no need
%%  to try the next type, it will always fail.

select_cg(#l{ke={cons,[S]}}, {var,V}, Vdb, Tf, Vf, Bef, St) ->
    select_cons(S, V, Tf, Vf, Bef, St);
select_cg(#l{ke={nil,[S]}}, {var,V}, Vdb, Tf, Vf, Bef, St) ->
    select_nil(S, V, Tf, Vf, Bef, St);
select_cg(#l{ke={Type,Scs}}, {var,V}, Vdb, Tf, Vf, Bef, St0) ->
    {Vis,{Aft,St1}} =
	mapfoldl(fun (S, {Int,Sta}) ->
			 {Val,Is,Inta,Stb} = select_val(S, V, Tf, Vf, Bef, Sta),
			 {{Is,[Val]},{sr_merge(Int, Inta),Stb}}
		 end, {void,St0}, Scs),
    OptVls = combine(lists:sort(combine(Vis))),
    {Vls,Sis,St2} = select_labels(OptVls, St1, [], []),
    {select_val_cg(Type, fetch_var(V, Bef), Vls, Tf, Vf, Sis), Aft, St2}.

select_val_cg(tuple, R, [Arity, {f,Lbl}], Tf, Vf, [{label,Lb}|Sis]) ->
    [{test,is_tuple,{f,Tf},R}, {test,test_arity,{f,Vf},R,Arity}|Sis];
select_val_cg(tuple, R, Vls, Tf, Vf, Sis) ->
    [{test,is_tuple,{f,Tf},R}, {select_tuple_arity,R,{f,Vf},{list,Vls}}|Sis];
select_val_cg(Type, R, [Val, {f,Lbl}], Fail, Fail, [{label,Lbl}|Sis]) ->
    [{test, is_eq_exact, {f,Fail}, R, {Type,Val}}|Sis];
select_val_cg(Type, R, [Val, {f,Lbl}], Tf, Vf, [{label,Lbl}|Sis]) ->
    [{test,select_type_test(Type),{f,Tf},R},
     {test, is_eq_exact, {f,Vf}, R, {Type,Val}}|Sis];
select_val_cg(Type, R, Vls0, Tf, Vf, Sis) ->
    Vls1 = map(fun ({f,Lbl}) -> {f,Lbl};
		   (Value) -> {Type,Value}
	       end, Vls0),
    [{test,select_type_test(Type),{f,Tf},R}, {select_val,R,{f,Vf},{list,Vls1}}|Sis].
    
select_type_test(tuple) -> is_tuple;
select_type_test(integer) -> is_integer;
select_type_test(atom) -> is_atom;
select_type_test(float) -> is_float.

combine([{Is,Vs1}, {Is,Vs2}|Vis]) -> combine([{Is,Vs1 ++ Vs2}|Vis]);
combine([V|Vis]) -> [V|combine(Vis)];
combine([]) -> [].

select_labels([{Is,Vs}|Vis], St0, Vls, Sis) ->
    {Lbl,St1} = new_label(St0),
    select_labels(Vis, St1, add_vls(Vs, Lbl, Vls), [[{label,Lbl}|Is]|Sis]);
select_labels([], St, Vls, Sis) ->
    {Vls,append(Sis),St}.

add_vls([V|Vs], Lbl, Acc) ->
    add_vls(Vs, Lbl, [V, {f,Lbl}|Acc]);
add_vls([], Lbl, Acc) -> Acc.

select_cons(#l{ke={sclause,{cons,Es},B},i=I,vdb=Vdb}, V, Tf, Vf, Bef, St0) ->
    {Eis,Int,St1} = select_extract_cons(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),
    {[{test,is_nonempty_list,{f,Tf},fetch_var(V, Bef)}] ++ Eis ++ Bis,Aft,St2}.

select_nil(#l{ke={sclause,nil,B}}, V, Tf, Vf, Bef, St0) ->
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    {[{test,is_nil,{f,Tf},fetch_var(V, Bef)}] ++ Bis,Aft,St1}.

select_val(#l{ke={sclause,{tuple,Es},B},i=I,vdb=Vdb}, V, Tf, Vf, Bef, St0) ->
    {Eis,Int,St1} = select_extract_tuple(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),
    {length(Es),Eis ++ Bis,Aft,St2};

select_val(#l{ke={sclause,{Type,Val},B},i=I,vdb=Vdb}, V, Tf, Vf, Bef, St0) ->
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    {Val,Bis,Aft,St1}.

%% -type select_extract_tuple(Src, [V], I, Vdb, Bef, St) -> {[E],Aft,St}.
%%  Extract tuple elements, but only if they do not immediately die.

select_extract_tuple(Src, Vs, I, Vdb, Bef, St) ->
    F = fun ({var,V}, {Int0,Elem}) ->
		case vdb_find(V, Vdb) of
		    {V,F,L} when L =< I -> {[], {Int0,Elem+1}};
		    Other ->
			Reg1 = put_reg(V, Int0#sr.reg),
			Int1 = Int0#sr{reg=Reg1},
			Rsrc = fetch_var(Src, Int1),
			{[{get_tuple_element,Rsrc,Elem,fetch_reg(V, Reg1)}],
			 {Int1,Elem+1}}
		end
	end,
    {Es,{Aft,_}} = flatmapfoldl(F, {Bef,0}, Vs),
    {Es,Aft,St}.

select_extract_cons(Src, [{var,Hd}, {var,Tl}], I, Vdb, Bef, St) ->
    {Es,Aft} = case {vdb_find(Hd, Vdb), vdb_find(Tl, Vdb)} of
		   {{_,_,Lhd}, {_,_,Ltl}} when Lhd =< I, Ltl =< I ->
		       %% Both head and tail are dead.  No need to generate
		       %% any instruction.
		       {[], Bef};
		   _ ->
		       %% At least one of head and tail will be used,
		       %% but we must always fetch both.  We will call
		       %% clear_dead/2 to allow reuse of the register
		       %% in case only of them is used.

		       Reg0 = put_reg(Tl, put_reg(Hd, Bef#sr.reg)),
		       Int0 = Bef#sr{reg=Reg0},
		       Rsrc = fetch_var(Src, Int0),
		       Rhd = fetch_reg(Hd, Reg0),
		       Rtl = fetch_reg(Tl, Reg0),
		       Int1 = clear_dead(Int0, I, Vdb),
		       {[{get_list,Rsrc,Rhd,Rtl}], Int1}
	       end,
    {Es,Aft,St}.
    

guard_cg(#l{ke={gclause,G,B},i=I,vdb=Vdb}, Fail, Bef, St0) ->
    {Gis,Int,St1} = cg_list(G, I, Vdb, Bef,
			    St0#cg{btype=fail,bfail=Fail}),
    {Bis,Aft,St2} = match_cg(B, Fail, Int, St1),
    {Gis ++ Bis,Aft,St2}.

%% match_fmf(Fun, LastFail, State, [Clause]) -> {Is,Aft,State}.
%%  This is a special flatmapfoldl for match code gen where we
%%  generate a "failure" label for each clause. The last clause uses
%%  an externally generated failure label, LastFail.  N.B. We do not
%%  know or care how the failure labels are used.

match_fmf(F, LastFail, St, [H]) ->
    F(H, LastFail, St);
match_fmf(F, LastFail, St0, [H|T]) ->
    {Fail,St1} = new_label(St0),
    {R,Aft1,St2} = F(H, Fail, St1),
    {Rs,Aft2,St3} = match_fmf(F, LastFail, St2, T),
    {R ++ [{label,Fail}] ++ Rs,sr_merge(Aft1, Aft2),St3};
match_fmf(F, LastFail, St, []) -> {[],void,St}.

%% call_cg(Func, [Arg], [Ret], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%% enter_cg(Func, [Arg], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Call and enter first put the arguments into registers and save any
%%  other registers, then clean up and compress the stack and set the
%%  frame size. Finally the actual call is made.  Call then needs the
%%  return values filled in.

call_cg({Func, Arity, Index, Uniq}, As, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {FuncId, St1} = func_id(call, Func, Arity, St0),
    {comment({make_fun,{Func, Arity, Index, Uniq},As}) ++ Sis ++
     [{make_fun,FuncId,Uniq,length(As)}],
     clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),
     St1};
call_cg({var,V}, As, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As++[{var,V}], Bef, Le#l.i, Vdb),
    %% Put return values in registers.
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {comment({call_fun,{var,V},As}) ++ Sis ++
     [{call_fun,Arity}],
     clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),
     St0#cg{calls=St0#cg.calls+1}};
call_cg(Func, As, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    %% Put return values in registers.
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {FuncId, St1} = func_id(call, Func, Arity, St0),
    {comment({call,Func,As}) ++ Sis ++
     [case Func of
	  {remote,erlang,'!'} -> send;
	  {remote,Mod,Name} -> {call_ext,Arity,FuncId};
	  _ -> {call,Arity,FuncId}
      end],
     clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),
     St1}.

enter_cg({Func, Arity, Index, Uniq}, As, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    {FuncId, St1} = func_id(call, Func, Arity, St0),
    {comment({make_fun,{Func, Arity, Index, Uniq},As}) ++ Sis ++
     [{make_fun,FuncId,Uniq,length(As)}, return],
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     St1};
enter_cg({var,V}, As, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As++[{var,V}], Bef, Le#l.i, Vdb),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {comment({call_fun_last,{var,V},As}) ++ Sis ++
     [{call_fun,Arity}, return],
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     St0#cg{calls=St0#cg.calls+1}};
enter_cg(Func, As, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {FuncId, St1} = func_id(enter, Func, Arity, St0),
    {comment({enter,Func,As}) ++ Sis ++
     case Func of
	 {remote,erlang,'!'} -> [send, return];
	 {remote,Mod,Name} -> [{call_ext_only,Arity,FuncId}];
	 _ -> [{call_only,Arity,FuncId}]
     end,
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     St1}.

func_id(enter, {remote,Mod,Name}, Arity, St0) ->
    St1 = case trap_bif(Mod, Name, Arity) of
	      true -> St0#cg{calls=St0#cg.calls+1};
	      false -> St0
	  end,
    {{extfunc,Mod,Name,Arity},St1};
func_id(call, {remote,Mod,Name}, Arity, St0) ->
    {{extfunc,Mod,Name,Arity},St0#cg{calls=St0#cg.calls+1}};
func_id(CallType, Func, Arity, St0) when atom(Func) ->
    St1 = case CallType of
	      call -> St0#cg{calls=St0#cg.calls+1};
	      enter -> St0
	  end,
    Key = {Func,Arity},
    case keysearch(Key, 1, St1#cg.functable) of
	{value, {Key,Label}} ->
	    {{f,Label}, St1};
	false ->
	    {Label, St2} = new_label(St1),
	    {{f,Label}, St2#cg{functable=[{Key,Label}|St2#cg.functable]}}
    end.

%% trap_bif(Mod, Name, Arity) -> true|false
%%   Trap bifs that need a stack frame.

trap_bif(erlang, '!', 2) -> true;
trap_bif(erlang, link, 1) -> true;
trap_bif(erlang, unlink, 1) -> true;
trap_bif(erlang, monitor_node, 2) -> true;
trap_bif(erlang, group_leader, 2) -> true;
trap_bif(erlang, exit, 2) -> true;
trap_bif(M, F, A) -> false.

%% bif_cg(Bif, [Arg], [Ret], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

bif_cg(Bif, As, Rs, Le, Vdb, Bef, St0) ->
    {Ars,Ms,Int0} = cg_reg_args(As, Bef),

    %% If we are inside a catch, we must save everything that will
    %% be alive after the catch (because the BIF might fail and there
    %% we will be a jump to the code after the catch).
    %%   Currently, we are somewhat pessimistic in
    %% that we save any variable that will be live after this BIF call.

    {_Saves,Sis,Int1} =
	case St0#cg.in_catch of
	    true -> adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb);
	    false -> {[],[],Int0}
	end,

    Int2 = clear_dead(Int1, Le#l.i, Vdb),
    Reg = load_vars(Rs, Int2#sr.reg),
    Int3 = Int2#sr{reg=Reg},
    [Dst] = map(fun ({var,V}) -> fetch_reg(V, Reg) end, Rs),
    {Ms ++ Sis ++
     [{bif,Bif,bif_fail(St0#cg.btype, St0#cg.bfail, length(Ars)),Ars,Dst}],
     clear_dead(Int3, Le#l.i, Vdb), St0}.

bif_fail(Type, Fail, 0) -> nofail;
bif_fail(Type, Fail, Ar) -> bif_fail(Type, Fail).
    
bif_fail(exit, _) -> {f,0};
bif_fail(fail, Fail) -> {f,Fail}.


%% recv_loop_cg(TimeOut, ReceiveVar, ReceiveMatch, TimeOutExprs,
%%              [Ret], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, Vdb, Bef, St0) ->
    {Saves,Sis,Int0} = adjust_stack(Bef, Le#l.i, Le#l.i, Vdb),
    Int1 = Int0#sr{reg=clear_regs(Int0#sr.reg)},
    %% Get labels.
    {Rl,St1} = new_label(St0),
    {Tl,St2} = new_label(St1),
    {Bl,St3} = new_label(St2),
    St4 = St3#cg{break=Bl,recv=Rl},		%Set correct receive labels
    {Ris,Raft,St5} = cg_recv_mesg(Rvar, Rm, Tl, Le#l.i, Vdb, Int1, St4),
    {Wis,Taft,St6} = cg_recv_wait(Te, Tes, Le#l.i, Vdb, Int1, St5),
    Int2 = sr_merge(Raft, Taft),		%Merge stack/registers
    Reg = load_vars(Rs, Int2#sr.reg),
    {Sis ++ Ris ++ [{label,Tl}] ++ Wis ++ [{label,Bl}],
     clear_dead(Int2#sr{reg=Reg}, Le#l.i, Vdb),
     St6#cg{break=St0#cg.break,recv=St0#cg.recv}}.

%% cg_recv_mesg( ) -> {[Ainstr],Aft,St}.

cg_recv_mesg({var,R}, Rm, Tl, I, Vdb, Bef, St0) ->
    Int0 = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    Ret = fetch_reg(R, Int0#sr.reg),
    %% Int1 = clear_dead(Int0, I, Rm#l.vdb),
    Int1 = Int0,
    {Mis,Int2,St1} = match_cg(Rm, none, Int1, St0),
    {[{'%live',0},{label,St1#cg.recv},{loop_rec,{f,Tl},Ret}|Mis],Int2,St1}.

%% cg_recv_wait(Te, Tes, I, Vdb, Int2, St3) -> {[Ainstr],Aft,St}.

cg_recv_wait({atom,infinity}, Tes, I, Vdb, Bef, St) ->
    Int = clear_dead(Bef, Tes#l.i, Tes#l.vdb),
    {Savs,Sis,Aft} = adjust_stack(Int, Tes#l.i, Tes#l.i+1, Tes#l.vdb),
    {[{wait,{f,St#cg.recv}}],Aft,St};
cg_recv_wait({integer,0}, Tes, I, Vdb, Bef, St0) ->
    {Tis,Int,St1} = block_cg(Tes#l.ke, Tes#l.i, Tes#l.vdb, Bef, St0),
    {[timeout|Tis],Int,St1};
cg_recv_wait(Te, Tes, I, Vdb, Bef, St0) ->
    {Reg,Ris,Int0} = cg_reg_arg(Te, Bef),
    %% Must have empty registers here!  Bug if anything in registers.
    Int1 = clear_dead(Int0, I, Tes#l.vdb),
    {Tis,Int2,St1} = block_cg(Tes#l.ke, Tes#l.i, Tes#l.vdb,
			     Int1#sr{reg=clear_regs(Int1#sr.reg)}, St0),
    {Ris ++ [{wait_timeout,{f,St1#cg.recv},Reg},timeout] ++ Tis,Int2,St1}.

%% recv_next_cg(Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Use adjust stack to clear stack, but only need it for Aft.

recv_next_cg(Le, Vdb, Bef, St) ->
    {Saves,Sis,Aft} = adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb),
    {[{loop_rec_end,{f,St#cg.recv}}] ++ Sis,Aft,St}.	%Joke

%% catch_cg(CatchBlock, Var, I, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

catch_cg(C, {var,R}, Le, Vdb, Bef, St0) ->
    {B,St1} = new_label(St0),
    CatchTag = Le#l.i,
    Int1 = Bef#sr{stk=put_catch(CatchTag, Bef#sr.stk)},
    CatchReg = fetch_stack({catch_tag,CatchTag}, Int1#sr.stk),
    {Cis,Aft0,St2} = block_cg(C, Le#l.i, Le#l.vdb, Int1,
			     St1#cg{btype=exit,bfail=St1#cg.finfo,
				    break=B,in_catch=true}),
    Aft1 = Aft0#sr{reg=load_reg(R, 0, Aft0#sr.reg),
		   stk=drop_catch(CatchTag, Aft0#sr.stk)},
    {[{'catch',CatchReg,{f,B}}] ++ Cis ++
     [{label,B},{catch_end,CatchReg}],
     clear_dead(Aft1, Le#l.i, Vdb),
     St2#cg{break=St1#cg.break,in_catch=false}}.

%% set_cg(Var, Constr, Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  We have to be careful how a 'set' works. First the structure is
%%  built, then it is filled and finally things can be cleared. The
%%  annotation must reflect this and make sure that the return
%%  variable is allocated first.
%%
%%  In Beam, put_list for constructing a cons is an atomic instruction
%%  which can safely resuse one of the source registers as target.

set_cg({var,R}, {cons,Es}, Le, Vdb, Bef, St) ->
    [S1, S2] = map(fun ({var,V}) -> fetch_var(V, Bef);
		       (Other) -> Other
		   end, Es),
    Int0 = clear_dead(Bef, Le#l.i, Vdb),
    Int1 = Int0#sr{reg=put_reg(R, Int0#sr.reg)},
    Ret = fetch_reg(R, Int1#sr.reg),
    {[{put_list,S1,S2,Ret}], Int1, St};
set_cg({var,R}, Con, Le, Vdb, Bef, St) ->
    %% Find a place for the return register first.
    Int = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    Ret = fetch_reg(R, Int#sr.reg),
    Ais = case Con of
	      {tuple,Es} ->
		  [{put_tuple,length(Es),Ret}] ++ cg_build_args(Es, Bef);
	      {var,V} ->			% Normally removed by kernel optimizer.
		  [{move,fetch_var(V, Int),Ret}];
	      {string,Str} ->
		  [{put_string,length(Str),{string,Str},Ret}];
	      Other ->
		  [{move,Other,Ret}]
	  end,
    {Ais,clear_dead(Int, Le#l.i, Vdb),St}.

cg_build_args(As, Bef) ->
    map(fun ({var,V}) -> {put,fetch_var(V, Bef)};
	    (Other) -> {put,Other}
	end, As).

%% test_cg(TestName, Args, Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Generate test instruction.

test_cg(Test, As, Le, Vdb, Bef, St0) ->
    case test_type(Test, length(As)) of
    	{cond,Op} ->
	    {Ars,Ms,Int0} = cg_reg_args(As, Bef),
	    Int1 = clear_dead(Int0, Le#l.i, Vdb),
	    fail = St0#cg.btype,		%Assertion.
	    {Ms ++ [list_to_tuple([test,Op,{f,St0#cg.bfail}|Ars])],
	     clear_dead(Int1, Le#l.i, Vdb),
	     St0};
	{rev_cond,Op} ->
	    {[S1,S2],Ms,Int0} = cg_reg_args(As, Bef),
	    Int1 = clear_dead(Int0, Le#l.i, Vdb),
	    fail = St0#cg.btype,		%Assertion.
	    {Ms ++ [{test,Op,{f,St0#cg.bfail},S2,S1}],
	     clear_dead(Int1, Le#l.i, Vdb),
	     St0}
    end.

test_type(integer, 1) -> {cond, is_integer};
test_type(float, 1)   -> {cond, is_float};
test_type(number, 1)  -> {cond, is_number};
test_type(atom, 1)    -> {cond, is_atom};
test_type(constant, 1) -> {cond, is_constant};
test_type(list, 1)    -> {cond, is_list};
test_type(tuple, 1)   -> {cond, is_tuple};
test_type(pid, 1)     -> {cond, is_pid};
test_type(reference, 1)     -> {cond, is_ref};
test_type(port, 1)    -> {cond, is_port};
test_type(binary, 1)  -> {cond, is_binary};
test_type(function, 1)  -> {cond, is_function};
test_type('=<', 2)   -> {rev_cond, is_ge};
test_type('>', 2)    -> {rev_cond, is_lt};
test_type('<', 2)    -> {cond, is_lt};
test_type('>=', 2)   -> {cond, is_ge};
test_type('==', 2)   -> {cond, is_eq};
test_type('/=', 2)   -> {cond, is_ne};
test_type('=:=', 2)  -> {cond, is_eq_exact};
test_type('=/=', 2)  -> {cond, is_ne_exact}.

%% return_cg([Val], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%% break_cg([Val], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  These are very simple, just put return/break values in registers
%%  from 0, then return/break.  Use the call setup to clean up stack,
%%  but must clear registers to ensure sr_merge works correctly.

return_cg(Rs, Le, Vdb, Bef, St) ->
    {Ms,Int} = cg_setup_call(Rs, Bef, Le#l.i, Vdb),
    {comment({return,Rs}) ++ Ms ++ [return],
     Int#sr{reg=clear_regs(Int#sr.reg)},St}.

break_cg(Bs, Le, Vdb, Bef, St) ->
    {Ms,Int} = cg_setup_call(Bs, Bef, Le#l.i, Vdb),
    {comment({break,Bs}) ++ Ms ++ [{jump,{f,St#cg.break}}],
     Int#sr{reg=clear_regs(Int#sr.reg)},St}.

%% cg_reg_arg(Arg, Info) -> {Reg,Moves,Aft}.
%% cg_reg_args([Arg], Info) -> {[Reg],Moves,Aft}.
%%  Convert argument[s] into registers, generating moves or puts as
%%  necessary.  N.B. the resultant register info will contain non-variable
%%  values if constants have been put into registers.
%%
%%  For Beam, only floating point literals need to be loaded into registers
%%  (in some cases).

cg_reg_args(As, Bef) ->
    foldr(fun (A, {Rs,Ms0,Int0}) ->
		  {R,Ms,Int1} = cg_reg_arg(A, Int0),
		  {[R|Rs],Ms ++ Ms0,Int1}
	  end, {[],[],Bef}, As).

cg_reg_arg({var,V}, Bef) ->
    R = fetch_var(V, Bef),
    {R,[],Bef};
%cg_reg_arg(Other, Bef) when element(1, Other) == float ->
%    Aft = Bef#sr{reg=put_reg(Other, Bef#sr.reg)},
%    R = fetch_reg(Other, Aft#sr.reg),
%    {R,[{move,Other,R}],Aft};
cg_reg_arg(Other, Bef) ->
    {Other,[],Bef}.

%% cg_setup_call([Arg], Bef, Cur, Vdb) -> {[Instr],Aft}.
%%  Do the complete setup for a call/enter.  We do no optimisations
%%  yet but should probably handle the case when there are no shifts
%%  to compress the stack and save things onto the stack first.

cg_setup_call(As, Bef, I, Vdb) ->
    {Ms,Int0} = cg_call_args(As, Bef, I, Vdb),
    %% Have set up arguments, can now clean up, compress and save to stack.
    Int1 = Int0#sr{stk=clear_dead_stk(Int0#sr.stk, I, Vdb),res=[]},
    {Saves,Sis,Int2} = adjust_stack(Int1, I, I+1, Vdb),
    {Ms ++ Sis ++ [{'%live',length(As)}],Int2}.

%% cg_call_args([Arg], SrState) -> {[Instr],SrState}.
%%  Setup the arguments to a call/enter/bif. Put the arguments into
%%  consecutive registers starting at {r,0} moving any data which
%%  needs to be saved. Return a modified SrState structure with the
%%  new register contents.  N.B. the resultant register info will
%%  contain non-variable values when there are non-variable values.

cg_call_args(As, Bef, I, Vdb) ->
    Arity = length(As),
    Ws0 = worklist(As, 0, []),
    Sis0 = [],
    Int0 = Bef,
    {Sis1,Ws1,Int1} = easy_loads(Ws0, Int0),
    {Sis2,Int2} = cg_call_args(Ws1, Int1),
    {Sis0 ++ Sis1 ++ Sis2,Int2}.

worklist([A|As], I, Acc) -> worklist(As, I+1, [{I,A}|Acc]);
worklist([], I, Acc) -> Acc.

easy_loads(As0, Bef) ->
    case easy_loads1(As0, Bef) of
	{[],As0,Aft} -> {[],As0,Aft};
	{Sis0,As1,Int0} ->
	    {Sis1,As2,Int1} = easy_loads(As1, Int0),
	    {Sis0 ++ Sis1,As2,Int1}
    end.

easy_loads1([{I,A}|As0], Sr0) ->
%%    ok = io:fwrite("~p: ~p, ~p\n", [?LINE, I, Sr0]),
    case easy_load(A, lists:nth(I+1, Sr0#sr.reg), I, Sr0) of
	{loaded,Ms,Sr1} ->
	    {Mss,As1,Sr2} = easy_loads1(As0, Sr1),
	    {Ms ++ Mss,As1,Sr2};
	not_loaded ->
	    {Mss,As1,Sr1} = easy_loads1(As0, Sr0),
	    {Mss,[{I,A}|As1],Sr1}
    end;
easy_loads1([], Sr) -> {[],[],Sr}.

%% Load the tricky arguments.  All easy cases have been taken care
%% of by easy_loads().

cg_call_args([{I,A}|As], Sr0) ->
    {Ms,Sr1} = cg_call_arg(A, lists:nth(I+1, Sr0#sr.reg), I, Sr0),
    {Mss,Sr2} = cg_call_args(As, Sr1),
    {Ms ++ Mss,Sr2};
cg_call_args([], Sr) -> {[],Sr}.

cg_call_arg(Any, {I,U}, I, Sr) ->
    %% U exists only in this register, which will be overwritten.
    Reg = put_reg(U, Sr#sr.reg),
    {loaded,Ms,Sr1} = easy_load(Any, free, I, Sr#sr{reg=Reg}),
    {[{move,{x,I},fetch_reg(U, Sr1#sr.reg)}|Ms],Sr1}.

easy_load({var,V}, {I,V}, I, Sr) -> {loaded,[],Sr}; %Already there
easy_load({var,V}, free, I, Sr) ->
    {loaded, [{move,fetch_var(V, Sr),{x,I}}], Sr#sr{reg=copy_reg(V, I, Sr#sr.reg)}};
easy_load({var,V}, {reserved,I,V}, I, Sr) ->
    easy_load({var,V}, free, I, Sr);
easy_load(NonVar, free, I, Sr) ->
    {loaded, [{move,NonVar,{x,I}}],Sr#sr{reg=load_reg(NonVar, I, Sr#sr.reg)}};
easy_load(NonVar, {reserved,I,NonVar}, I, Sr) ->
    easy_load(NonVar, free, I, Sr);
easy_load(Any, {I,U}, I, Sr) ->
    %% Check if U exists on the stack or in another register so we can overwrite it.
    case on_stack(U, Sr#sr.stk) of
	true -> easy_load(Any, free, I, Sr);
	false ->
	    case find_other_reg(I, U, Sr#sr.reg) of
		{ok,R} -> easy_load(Any, free, I, Sr);
		error -> not_loaded
	    end
    end.


%% clear_dead(Sr, Until, Vdb) -> Aft.
%%  Remove all variables in Sr which have died AT ALL so far.

clear_dead(Sr, Until, Vdb) ->
    Sr#sr{reg=clear_dead_reg(Sr, Until, Vdb),
	  stk=clear_dead_stk(Sr#sr.stk, Until, Vdb)}.

clear_dead_reg(Sr, Until, Vdb) ->
    Reg = map(fun ({I,V}) ->
		      case vdb_find(V, Vdb) of
			  {V,F,L} when L > Until -> {I,V};
			  Other -> free		%Remove anything else
		      end;
		  ({reserved,I,V}) -> {reserved,I,V};
		  (free) -> free
	      end, Sr#sr.reg),
    reserve(Sr#sr.res, Reg, Sr#sr.stk).

clear_dead_stk(Stk, Until, Vdb) ->
    map(fun ({V}) ->
		case vdb_find(V, Vdb) of
		    {V,F,L} when L > Until -> {V};
		    Other -> free		%Remove anything else
		end;
	    (free) -> free
	end, Stk).

%% sr_merge(Sr1, Sr2) -> Sr.
%%  Merge two stack/register states keeping the longest of both stack
%%  and register. Perform consistency check on both, elements must be
%%  the same.  Allow frame size 'void' to make easy creation of
%%  "empty" frame.

sr_merge(#sr{reg=R1,stk=S1,res=[]}, #sr{reg=R2,stk=S2,res=[]}) ->
    #sr{reg=longest(R1, R2),stk=longest(S1, S2),res=[]};
sr_merge(void, S2) -> S2#sr{res=[]};
sr_merge(S1, void) -> S1#sr{res=[]}.

longest([H|T1], [H|T2]) -> [H|longest(T1, T2)];
longest([free|T1], []) -> [free|T1];
longest([], [free|T2]) -> [free|T2];
longest([], []) -> [].

%% adjust_stack(Bef, FirstBefore, LastFrom, Vdb) -> {[SaveVar],[Ainstr],Aft}.
%%  Do complete stack adjustment by compressing stack and adding
%%  variables to be saved.  Try to optimise ordering on stack by
%%  having reverse order to their lifetimes.
%%
%%  In Beam, there is a fixed stack frame and no need to do stack compression.

adjust_stack(Bef, Fb, Lf, Vdb) ->
    Stk0 = Bef#sr.stk,
    {Stk1,Saves} = save_stack(Stk0, Fb, Lf, Vdb),
    {Saves,
     saves(Saves, Bef#sr.reg, Stk1),
     Bef#sr{stk=Stk1}}.

%% save_stack(Stack, FirstBefore, LastFrom, Vdb) -> {[SaveVar],NewStack}.
%%  Save variables which are used past current point and which are not
%%  already on the stack.

save_stack(Stk0, Fb, Lf, Vdb) ->
    %% New variables that are in use but not on stack.
    New = [ {V,F,L} || {V,F,L} <- Vdb,
		   F < Fb,
		   L >= Lf,
		   not on_stack(V, Stk0) ],
    %% Add new variables that are not just dropped immediately.
    %% N.B. foldr works backwards from the end!!
    Saves = [ V || {V,F,L} <- keysort(3, New) ],
    Stk1 = foldr(fun (V, Stk) -> put_stack(V, Stk) end, Stk0, Saves),
    {Stk1,Saves}.

%% saves([SaveVar], Reg, Stk) -> [{move,Reg,Stk}].
%%  Generate move instructions to save variables onto stack.  The
%%  stack/reg info used is that after the new stack has been made.

saves(Ss, Reg, Stk) ->
    Res = map(fun (V) ->
		      {move,fetch_reg(V, Reg),fetch_stack(V, Stk)}
	      end, Ss),
    Res.

%% comment(C) -> ['%'{C}].

%comment(C) -> [{'%',C}].
comment(C) -> [].

%% fetch_var(VarName, StkReg) -> r{R} | sp{Sp}.
%% find_var(VarName, StkReg) -> ok{r{R} | sp{Sp}} | error.
%%  Fetch/find a variable in either the registers or on the
%%  stack. Fetch KNOWS it's there.

fetch_var(V, Sr) ->
    case find_reg(V, Sr#sr.reg) of
	{ok,R} -> R;
	error -> fetch_stack(V, Sr#sr.stk)
    end.

find_var(V, Sr) ->
    case find_reg(V, Sr#sr.reg) of
	{ok,R} -> {ok,R};
	error ->
	    case find_stack(V, Sr#sr.stk) of
		{ok,S} -> {ok,S};
		error -> error
	    end
    end.

load_vars(Vs, Regs) ->
    foldl(fun ({var,V}, Rs) -> put_reg(V, Rs) end, Regs, Vs).

%% put_reg(Val, Regs) -> Regs.
%% put_reg_from(Val, First, Regs) -> Regs.
%% load_reg(Val, Reg, Regs) -> Regs.
%% free_reg(Val, Regs) -> Regs.
%% find_reg(Val, Regs) -> ok{r{R}} | error.
%% fetch_reg(Val, Regs) -> r{R}.
%%  Functions to interface the registers.
%%  put_reg puts a value into a free register,
%%  load_reg loads a value into a fixed register
%%  free_reg frees a register containing a specific value.

put_regs(Vs, Rs) -> foldl(fun put_reg/2, Rs, Vs).

put_reg(V, Rs) -> put_reg_1(V, Rs, 0).

put_reg_from(V, F, Rs) -> put_reg_from(V, F, Rs, 0).

put_reg_from(V, F, Rs, F) -> put_reg_1(V, Rs, F);
put_reg_from(V, F, [R|Rs], C) -> [R|put_reg_from(V, F, Rs, C+1)];
put_reg_from(V, F, [], C) -> [free|put_reg_from(V, F, [], C+1)].

put_reg_1(V, [free|Rs], I) -> [{I,V}|Rs];
put_reg_1(V, [{reserved,I,V}|Rs], I) -> [{I,V}|Rs];
put_reg_1(V, [R|Rs], I) -> [R|put_reg_1(V, Rs, I+1)];
put_reg_1(V, [], I) -> [{I,V}].

load_reg(V, R, Rs) -> load_reg_1(V, R, Rs, 0).

load_reg_1(V, I, [R|Rs], I) -> [{I,V}|Rs];
load_reg_1(V, I, [R|Rs], C) -> [R|load_reg_1(V, I, Rs, C+1)];
load_reg_1(V, I, [], I) -> [{I,V}];
load_reg_1(V, I, [], C) -> [free|load_reg_1(V, I, [], C+1)].

free_reg(V, [{I,V}|Rs]) -> [free|Rs];
free_reg(V, [R|Rs]) -> [R|free_reg(V, Rs)];
free_reg(V, []) -> [].

fetch_reg(V, [{I,V}|SRs]) -> {x,I};
fetch_reg(V, [SR|SRs]) -> fetch_reg(V, SRs).

find_reg(V, [{I,V}|SRs]) -> {ok,{x,I}};
find_reg(V, [SR|SRs]) -> find_reg(V, SRs);
find_reg(V, []) -> error.

find_other_reg(R, V, [{I,V}|SRs]) when R =/= I -> {ok,{x,I}};
find_other_reg(R, V, [SR|SRs]) -> find_other_reg(R, V, SRs);
find_other_reg(R, V, []) -> error.

copy_reg(Val, R, Regs) -> load_reg(Val, R, Regs).
%%move_reg(Val, R, Regs) -> load_reg(Val, R, free_reg(Val, Regs)).

%%clear_regs(Regs) -> map(fun (R) -> free end, Regs).
clear_regs(Regs) -> [].

max_reg(Regs) ->
    foldl(fun ({I,V}, Max) -> I;
	      (Other, Max) -> Max end,
	  -1, Regs) + 1.

%% put_stack(Val, [{Val}]) -> [{Val}].
%% fetch_stack(Var, Stk) -> sp{S}.
%% find_stack(Var, Stk) -> ok{sp{S}} | error.
%%  Functions to interface the stack.

put_stack(Val, []) -> [{Val}];
put_stack(Val, [free|Stk]) -> [{Val}|Stk];
put_stack(Val, [NotFree|Stk]) -> [NotFree|put_stack(Val, Stk)].

put_stack_carefully(Val, Stk0) ->
    case catch put_stack_carefully1(Val, Stk0) of
	error -> error;
	Stk1 when list(Stk1) -> Stk1
    end.

put_stack_carefully1(Val, []) -> throw(error);
put_stack_carefully1(Val, [free|Stk]) -> [{Val}|Stk];
put_stack_carefully1(Val, [NotFree|Stk]) ->
    [NotFree|put_stack_carefully1(Val, Stk)].

fetch_stack(Var, Stk) -> fetch_stack(Var, Stk, 0).

fetch_stack(V, [{V}|Stk], I) -> {yy,I};
fetch_stack(V, [O|Stk], I) -> fetch_stack(V, Stk, I+1).

find_stack(Var, Stk) -> find_stack(Var, Stk, 0).

find_stack(V, [{V}|Stk], I) -> {ok,{yy,I}};
find_stack(V, [O|Stk], I) -> find_stack(V, Stk, I+1);
find_stack(V, [], I) -> error.

on_stack(V, Stk) -> keymember(V, 1, Stk).

%% put_catch(CatchTag, Stack) -> Stack'
%% drop_catch(CatchTag, Stack) -> Stack'
%%  Special interface for putting and removing catch tags, to ensure that
%%  catches nest properly.

put_catch(Tag, Stk0) -> put_catch(Tag, reverse(Stk0), []).

put_catch(Tag, [], Stk) ->
    put_stack({catch_tag,Tag}, Stk);
put_catch(Tag, [{catch_tag, _}|RevStk], Stk) ->
    reverse(RevStk, put_stack({catch_tag,Tag}, Stk));
put_catch(Tag, [Other|Stk], Acc) ->
    put_catch(Tag, Stk, [Other|Acc]).

drop_catch(Tag, Stk) -> reverse(drop_catch1(Tag, reverse(Stk))).

drop_catch1(Tag, [{{catch_tag,Tag}}|Stk]) -> [free|Stk];
drop_catch1(Tag, [Other|Stk]) -> [Other|drop_catch1(Tag, Stk)].

%% new_label(St) -> {L,St}.

new_label(St) ->
    L = St#cg.lcount,
    {L,St#cg{lcount=L+1}}.

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(F, Accu, []) -> {[],Accu}.

flatmapfoldr(F, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = flatmapfoldr(F, Accu0, Tail),
    {R,Accu2} = F(Hd, Accu1),
    {R++Rs,Accu2};
flatmapfoldr(F, Accu, []) -> {[],Accu}.
