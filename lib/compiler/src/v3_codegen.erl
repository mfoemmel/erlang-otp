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
%% 1. Matches, i.e. things with {match,M,Ret} wrappers, only return
%% values; no variables are exported. If the match would have returned
%% extra variables then these have been transformed to multiple return
%% values.
%% 
%% 2. All BIF's called in guards are gc-safe so there is no need to
%% put thing on the stack in the guard.  While this would in principle
%% work it would be difficult to keep track of the stack depth when
%% trimming.
%%
%% The code generation uses variable lifetime information added by
%% the v3_life module to save variables, allocate registers and 
%% move registers to the stack when necessary.
%%
%% We try to use a consistent variable name scheme throughout.  The
%% StackReg record is always called Bef,Int<n>,Aft.

-module(v3_codegen).

%% The main interface.
-export([module/2]).

-import(lists, [member/2,keymember/3,keysort/2,keysearch/3,append/1,
		map/2,flatmap/2,foldl/3,foldr/3,mapfoldl/3,reverse/1,reverse/2]).
-import(v3_life, [vdb_find/2]).

%%-compile([export_all]).

-include("v3_life.hrl").

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
	     need_frame,			%Need a stack frame.
	     new_funs=true}).			%Generate new fun instructions.

%% Stack/register state record.
-record(sr, {reg=[],				%Register table
	     stk=[],				%Stack table
	     res=[]}).				%Reserved regs: [{reserved, I, V}]

%% Always inline these simple functions.

-compile({inline,{comment,1}}).
-compile({inline,{on_stack,2}}).
-compile({inline,{new_label,1}}).

module({Mod,Exp,Attr,Forms}, Options) ->
    NewFunsFlag = not member(no_new_funs, Options),
    {Fs,St} = functions(Forms, Options, #cg{mod=Mod,new_funs=NewFunsFlag}),
    {ok,{Mod,Exp,Attr,Fs,St#cg.lcount}}.

functions(Forms, Options, St0) ->
    mapfoldl(fun (F, St) -> function(F, St) end, St0#cg{lcount=1}, Forms).

function({function,Name,Arity,As0,Vb,Vdb}, St0) ->
    %%ok = io:fwrite("cg ~w:~p~n", [?LINE,{Name,Arity}]),
    St1 = St0#cg{func={Name,Arity}},
    {Fun,St2} = cg_fun(Vb, As0, Vdb, St1),
    {{function,Name,Arity,St2#cg.fcode,Fun},St2};
function({asm,Name,Arity,Code}, St0) ->
    {Fi,St1} = new_label(St0),
    {Fl,St2} = new_label(St1),
    {{function,Name,Arity,Fl,Code},St2}.

%% cg_fun([Lkexpr], [HeadVar], Vdb, State) -> {[Ainstr],State}

cg_fun(Les, Hvs, Vdb, St0) ->
    {Name,Arity} = St0#cg.func,
    {Fi,St1} = new_label(St0),			%FuncInfo label
    {Fl,St2} = local_func_label(Name, Arity, St1),
    %% Create initial stack/register state, clear unused arguments.
    Bef = clear_dead(#sr{reg=foldl(fun ({var,V}, Reg) ->
					   put_reg_from(V, 0, Reg)
				   end, [], Hvs),
			stk=[]}, 0, Vdb),
    {B2,Aft,St3} = cg_list(Les, 0, Vdb, Bef, St2#cg{btype=exit,
						    bfail=Fi,
						    finfo=Fi,
						    fcode=Fl,
						    is_top_block=true}),
    A = [{label,Fi},{func_info,{atom,St3#cg.mod},{atom,Name},Arity},
	 {label,Fl}|B2],
    {A,St3}.

%% cg(Lkexpr, Vdb, StackReg, State) -> {[Ainstr],StackReg,State}.
%%  Generate code for a kexpr.
%%  Split function into two steps for clarity, not efficiency.

cg(Le, Vdb, Bef, St) ->
    cg(Le#l.ke, Le, Vdb, Bef, St).

cg({match,M,Rs}, Le, Vdb, Bef, St) ->
    match_cg(M, Rs, Le, Vdb, Bef, St);
cg({match_fail,F}, Le, Vdb, Bef, St) ->
    match_fail_cg(F, Le, Vdb, Bef, St);
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
cg({return,Rs}, Le, Vdb, Bef, St) -> return_cg(Rs, Le, Vdb, Bef, St);
cg({break,Bs}, Le, Vdb, Bef, St) -> break_cg(Bs, Le, Vdb, Bef, St);
cg({need_heap,0}, Le, Vdb, Bef, St) ->
    {[],Bef,St};
cg({need_heap,H}, Le, Vdb, Bef, St) ->
    {[{test_heap,H,max_reg(Bef#sr.reg)}],Bef,St}.

%% cg_list([Kexpr], FirstI, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.

cg_list(Kes, I, Vdb, Bef, St0) ->
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun (Ke, {Inta,Sta}) ->
%			     ok = io:fwrite("    %% ~p\n", [Inta]),
%			     ok = io:fwrite("cgl:~p\n", [Ke]),
			     {Keis,Intb,Stb} = cg(Ke, Vdb, Inta, Sta),
%			     ok = io:fwrite("    ~p\n", [Keis]),
%			     ok = io:fwrite("    %% ~p\n", [Intb]),
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
%     ok = io:fwrite("need_heap: ~p~n",
% 		   [{{H,F},
% 		     map(fun (#l{ke={match,M,Rs}}) -> match;
% 			     (Lke) -> Lke#l.ke end, Kes2)}]),
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
need_heap(#l{ke={bif,dsetelement,As,Rs},i=I}, H, F, BifType) ->
    {need_heap_need(I, H, F),0,true};
need_heap(#l{ke={bif,Bif,As,Rs},i=I}, H, F, BifType) ->
    {[],H,F};
need_heap(#l{i=I}, H, F, BifType) ->
    %% Others kexprs generate a need if necessary but don't force.
    {need_heap_need(I, H, F),0,false}.

need_heap_need(I, 0, false) -> [];
need_heap_need(I, H, F) -> [#l{ke={need_heap,H},i=I}].


%% match_cg(Match, [Ret], Le, Vdb, StackReg, State) ->
%%	{[Ainstr],StackReg,State}.
%%  Generate code for a match.  First save all variables on the stack
%%  that are to survive after the match.  We leave saved variables in
%%  their registers as they might actually be in the right place.
%%  Should test this.

match_cg(M, Rs, Le, Vdb, Bef, St0) ->
    I = Le#l.i,
    {Saves,Sis,Int0} = adjust_stack(Bef, I, I+1, Vdb),
    {B,St1} = new_label(St0),
    {Mis,Int1,St2} = match_cg(M, none, Int0, St1#cg{break=B}),
    %% Put return values in registers.
    Reg = load_vars(Rs, Int1#sr.reg),
    {Sis ++ Mis ++ [{label,B}],
     clear_dead(Int1#sr{reg=Reg}, I, Vdb),
     St2#cg{break=St1#cg.break}}.

%% match_cg(Match, Fail, StackReg, State) -> {[Ainstr],StackReg,State}.
%%  Generate code for a match tree.  N.B. there is no need pass Vdb
%%  down as each level which uses this takes its own internal Vdb not
%%  the outer one.

match_cg(Le, Fail, Bef, St) ->
    match_cg(Le#l.ke, Le, Fail, Bef, St).

match_cg({alt,F,S}, Le, Fail, Bef, St0) ->
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
    match_fmf(fun (G, F, Sta) -> guard_clause_cg(G, F, Bef, Sta) end,
	      Fail, St, Gcs);
match_cg({block,Es}, Le, Fail, Bef, St0) ->
    %% Must clear registers and stack of dead variables.
    Int = clear_dead(Bef, Le#l.i, Le#l.vdb),
    case St0#cg.is_top_block of
	false ->
	    block_cg(Es, Le#l.i, Le#l.vdb, Int, St0);
	true ->
	    {Keis,Aft,St1} = block_cg(Es, Le#l.i, Le#l.vdb, Int,
				      St0#cg{is_top_block=false,
					     need_frame=false}),
	    top_level_block(Keis, Aft, max_reg(Int#sr.reg), St1)
    end.

%% match_fail_cg(FailReason, Le, Vdb, StackReg, State) ->
%%	{[Ainstr],StackReg,State}.
%%  Generate code for the match_fail "call".  N.B. there is no generic
%%  case for when the fail value has been created elsewhere.

match_fail_cg({function_clause,As}, Le, Vdb, Bef, St) ->
    %% Must have the args in r0,r1,...
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    {Sis ++ [{jump,{f,St#cg.finfo}}],
     Int#sr{reg=clear_regs(Int#sr.reg)},St};
match_fail_cg({badmatch,Term}, Le, Vdb, Bef, St) ->
    {R,Ms,Int0} = cg_reg_arg(Term, Bef),
    Int1 = clear_dead(Int0, Le#l.i, Vdb),
    {Saves,Sis,Int2} = adjust_stack(Int1, Le#l.i, Le#l.i+1, Vdb),
    {Ms ++ Sis ++ [{badmatch,R}],
     Int2#sr{reg=clear_regs(Int0#sr.reg)},St};
match_fail_cg({case_clause,Reason}, Le, Vdb, Bef, St) ->
    {R,Ms,Int0} = cg_reg_arg(Reason, Bef),
    Int1 = clear_dead(Int0, Le#l.i, Vdb),
    {Saves,Sis,Int2} = adjust_stack(Int1, Le#l.i, Le#l.i+1, Vdb),
    {Ms ++ Sis ++ [{case_end,R}],
     Int2#sr{reg=clear_regs(Int0#sr.reg)},St};
match_fail_cg(if_clause, Le, Vdb, Bef, St) ->
    Int0 = clear_dead(Bef, Le#l.i, Vdb),
    {Saves,Sis,Int1} = adjust_stack(Int0, Le#l.i, Le#l.i+1, Vdb),
    {Sis ++ [if_end],
     Int1#sr{reg=clear_regs(Int1#sr.reg)}, St}.

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

%% cg_basic_block([Kexpr], FirstI, LastI, As, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.

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
    New = [ {V,F,L} || {V,F,L} <- Vdb,
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

top_level_block(Keis, Bef, MaxRegs, St0) when St0#cg.need_frame =:= false,
					      length(Bef#sr.stk) =:= 0 ->
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

turn_yregs(0, Tp, MaxY) -> Tp;
turn_yregs(El, Tp, MaxY) when element(1, element(El, Tp)) == yy ->
    turn_yregs(El-1, setelement(El, Tp, {y,MaxY-element(2, element(El, Tp))}), MaxY);
turn_yregs(El, Tp, MaxY) when list(element(El, Tp)) ->
    New = map(fun ({yy,YY}) -> {y,MaxY-YY};
		  (Other) -> Other end, element(El, Tp)),
    turn_yregs(El-1, setelement(El, Tp, New), MaxY);
turn_yregs(El, Tp, MaxY) ->
    turn_yregs(El-1, Tp, MaxY).

%% select_cg(Sclause, V, Vdb, TypeFail, ValueFail, StackReg, State) ->
%%      {Is,StackReg,State}.
%%  Selecting type and value needs two failure labels, TypeFail is the
%%  label to jump to of the next type test when this type fails, and
%%  ValueFail is the label when this type is correct but the value is
%%  wrong.  These are different as in the second case there is no need
%%  to try the next type, it will always fail.

select_cg(#l{ke={type_clause,cons,[S]}}, {var,V}, Vdb, Tf, Vf, Bef, St) ->
    select_cons(S, V, Tf, Vf, Bef, St);
select_cg(#l{ke={type_clause,nil,[S]}}, {var,V}, Vdb, Tf, Vf, Bef, St) ->
    select_nil(S, V, Tf, Vf, Bef, St);
select_cg(#l{ke={type_clause,binary,[S]}}, {var,V}, Vdb, Tf, Vf, Bef, St) ->
    select_binary(S, V, Tf, Vf, Bef, St);
select_cg(#l{ke={type_clause,bin_seg,S}}, {var,V}, Vdb, Tf, Vf, Bef, St) ->
    select_bin_segs(S, V, Tf, Vf, Bef, St);
select_cg(#l{ke={type_clause,bin_end,[S]}}, {var,V}, Vdb, Tf, Vf, Bef, St) ->
    select_bin_end(S, V, Tf, Vf, Bef, St);
select_cg(#l{ke={type_clause,Type,Scs}}, {var,V}, Vdb, Tf, Vf, Bef, St0) ->
    {Vis,{Aft,St1}} =
	mapfoldl(fun (S, {Int,Sta}) ->
			 {Val,Is,Inta,Stb} = select_val(S, V, Tf, Vf, Bef, Sta),
			 {{Is,[Val]},{sr_merge(Int, Inta),Stb}}
		 end, {void,St0}, Scs),
    OptVls = combine(lists:sort(combine(Vis))),
    {Vls,Sis,St2} = select_labels(OptVls, St1, [], []),
    {select_val_cg(Type, fetch_var(V, Bef), Vls, Tf, Vf, Sis), Aft, St2}.

select_val_cg(tuple, R, [Arity, {f,Lbl}], Tf, Vf, [{label,Lb}|Sis]) ->
    [{test,is_tuple,{f,Tf},[R]}, {test,test_arity,{f,Vf},[R,Arity]}|Sis];
select_val_cg(tuple, R, Vls, Tf, Vf, Sis) ->
    [{test,is_tuple,{f,Tf},[R]}, {select_tuple_arity,R,{f,Vf},{list,Vls}}|Sis];
select_val_cg(Type, R, [Val, {f,Lbl}], Fail, Fail, [{label,Lbl}|Sis]) ->
    [{test,is_eq_exact,{f,Fail},[R,{Type,Val}]}|Sis];
select_val_cg(Type, R, [Val, {f,Lbl}], Tf, Vf, [{label,Lbl}|Sis]) ->
    [{test,select_type_test(Type),{f,Tf},[R]},
     {test,is_eq_exact,{f,Vf},[R,{Type,Val}]}|Sis];
select_val_cg(Type, R, Vls0, Tf, Vf, Sis) ->
    Vls1 = map(fun ({f,Lbl}) -> {f,Lbl};
		   (Value) -> {Type,Value}
	       end, Vls0),
    [{test,select_type_test(Type),{f,Tf},[R]}, {select_val,R,{f,Vf},{list,Vls1}}|Sis].
    
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

select_cons(#l{ke={val_clause,{cons,Es},B},i=I,vdb=Vdb}, V, Tf, Vf, Bef, St0) ->
    {Eis,Int,St1} = select_extract_cons(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),
    {[{test,is_nonempty_list,{f,Tf},[fetch_var(V, Bef)]}] ++ Eis ++ Bis,Aft,St2}.

select_nil(#l{ke={val_clause,nil,B}}, V, Tf, Vf, Bef, St0) ->
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    {[{test,is_nil,{f,Tf},[fetch_var(V, Bef)]}] ++ Bis,Aft,St1}.

select_binary(#l{ke={val_clause,{binary,{var,Ivar}},B},i=I,vdb=Vdb},
	      V, Tf, Vf, Bef, St0) ->
    Int0 = clear_dead(Bef, I, Vdb),
    {Bis,Aft,St1} = match_cg(B, Vf, Int0, St0),
    {[{test,bs_start_match,{f,Tf},[fetch_var(V, Bef)]},{bs_save,Ivar}|Bis],
     Aft,St1}.

select_bin_segs(Scs, Ivar, Tf, Vf, Bef, St) ->
    match_fmf(fun(S, Fail, Sta) ->
		      select_bin_seg(S, Ivar, Fail, Bef, Sta) end,
	      Tf, St, Scs).

select_bin_seg(#l{ke={val_clause,{bin_seg,Size,U,T,Fs,Es},B},i=I,vdb=Vdb},
		   Ivar, Fail, Bef, St0) ->
    {Mis,Int,St1} = select_extract_bin(Es, Size, U, T, Fs, Fail,
				       I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Fail, Int, St1),
    {[{bs_restore,Ivar}|Mis] ++ Bis,Aft,St2}.

select_extract_bin([{var,Hd},{var,Tl}], Size0, Unit, Type, Flags, Vf,
		   I, Vdb, Bef, St) ->
    SizeReg = get_bin_size_reg(Size0, Bef),
    {Es,Aft} =
	case vdb_find(Hd, Vdb) of
	    {_,_,Lhd} when Lhd =< I ->
		{[{test,bs_skip_bits,{f,Vf},[SizeReg,Unit,{field_flags,Flags}]},
		  {bs_save,Tl}],Bef};
	    {_,_,_} ->
		Reg0 = put_reg(Hd, Bef#sr.reg),
		Int1 = Bef#sr{reg=Reg0},
		Rhd = fetch_reg(Hd, Reg0),
		Name = get_bits_instr(Type),
		{[{test,Name,{f,Vf},[SizeReg,Unit,{field_flags,Flags},Rhd]},
		  {bs_save,Tl}],Int1}
	end,
    {Es,clear_dead(Aft, I, Vdb),St}.

get_bin_size_reg({var,V}, Bef) ->
    fetch_var(V, Bef);
get_bin_size_reg(Literal, Bef) ->
    Literal.

select_bin_end(#l{ke={val_clause,bin_end,B},i=I,vdb=Vdb},
		   Ivar, Tf, Vf, Bef, St0) ->
    {Bis,Aft,St2} = match_cg(B, Vf, Bef, St0),
    {[{bs_restore,Ivar},{test,bs_test_tail,{f,Tf},[0]}|Bis],Aft,St2}.
    
get_bits_instr(integer) -> bs_get_integer;
get_bits_instr(float)   -> bs_get_float;
get_bits_instr(binary)  -> bs_get_binary.

select_val(#l{ke={val_clause,{tuple,Es},B},i=I,vdb=Vdb}, V, Tf, Vf, Bef, St0) ->
    {Eis,Int,St1} = select_extract_tuple(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),
    {length(Es),Eis ++ Bis,Aft,St2};

select_val(#l{ke={val_clause,{Type,Val},B},i=I,vdb=Vdb}, V, Tf, Vf, Bef, St0) ->
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    {Val,Bis,Aft,St1}.

%% select_extract_tuple(Src, [V], I, Vdb, StackReg, State) ->
%%      {[E],StackReg,State}.
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
    

guard_clause_cg(#l{ke={guard_clause,G,B},i=I,vdb=Vdb}, Fail, Bef, St0) ->
    {Gis,Int,St1} = guard_cg(G, Fail, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Fail, Int, St1),
    {Gis ++ Bis,Aft,St2}.

%% guard_cg(Guard, Fail, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%%  A guard is a boolean expression of tests.  Tests return true or
%%  false.  A fault in a test causes the test to return false.  Tests
%%  never return the boolean, instead we generate jump code to go to
%%  the correct exit point.  Primops and tests all go to the next
%%  instruction on success or jump to a failure label.

guard_cg(#l{ke={guard_not,N},i=I,vdb=Ndb}, Fail, Vdb, Bef, St0) ->
    %% Must do a little label trickery here to invert guard.
    {Succ,St1} = new_label(St0),
    {Nis,Aft,St2} = guard_cg(N, Succ, Ndb, Bef, St1),
    {Nis ++ [{jump,{f,Fail}},{label,Succ}],Aft,St2};
guard_cg(#l{ke={guard_and,Ts},i=I,vdb=Adb}, Fail, Vdb, Bef, St) ->
    guard_and_cg(Ts, Fail, Adb, Bef, St);
guard_cg(#l{ke={guard_or,Ts},i=I,vdb=Odb}, Fail, Vdb, Bef, St0) ->
    {Succ,St1} = new_label(St0),
    {Tis,Aft,St2} = guard_or_cg(Ts, Succ, Fail, Odb, Bef, St1),
    {Tis ++ [{label,Succ}],Aft,St2};		%Here's success
guard_cg(#l{ke={protected,Ts,Rs},i=I,vdb=Pdb}, Fail, Vdb, Bef, St) ->
    protected_cg(Ts, Rs, Fail, I, Pdb, Bef, St);
guard_cg(#l{ke={block,Ts},i=I,vdb=Bdb}, Fail, Vdb, Bef, St) ->
    guard_cg_list(Ts, Fail, I, Bdb, Bef, St);
guard_cg(#l{ke={test,Test,As},i=I,vdb=Tdb}, Fail, Vdb, Bef, St) ->
    test_cg(Test, As, Fail, I, Vdb, Bef, St);
guard_cg(G, Fail, Vdb, Bef, St) ->
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{G,Fail,Vdb,Bef}]),
    {Gis,Aft,St1} = cg(G, Vdb, Bef, St),
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{Aft}]),
    {Gis,Aft,St1}.

%% protected_cg([Kexpr], [Ret], Fail, I, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Do a protected.  Protecteds without return values are just done
%%  for effect, the return value is not checked, success passes on to
%%  the next instruction and failure jumps to Fail.  If there are
%%  return values then these must be set to 'false' on failure,
%%  control always passes to the next instruction.

protected_cg(Ts, [], Fail, I, Vdb, Bef, St0) ->
    %% Protect these calls, revert when done.
    {Tis,Aft,St1} = guard_cg_list(Ts, Fail, I, Vdb, Bef,
				  St0#cg{btype=fail,bfail=Fail}),
    {Tis,Aft,St1#cg{btype=St0#cg.btype,bfail=St0#cg.bfail}};
protected_cg(Ts, Rs, Fail, I, Vdb, Bef, St0) ->
    {Pfail,St1} = new_label(St0),
    {Psucc,St2} = new_label(St1),
    {Tis,Aft,St3} = guard_cg_list(Ts, Pfail, I, Vdb, Bef,
				  St2#cg{btype=fail,bfail=Pfail}),
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{Rs,I,Vdb,Aft}]),
    %% Set return values to false.
    Mis = map(fun ({var,V}) -> {move,{atom,false},fetch_var(V, Aft)} end, Rs),
    {Tis ++ [{jump,{f,Psucc}},{label,Pfail}] ++ Mis ++ [{label,Psucc}],
     Aft,St3#cg{btype=St0#cg.btype,bfail=St0#cg.bfail}}.    

%% test_cg(TestName, Args, Fail, I, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Generate test instruction.  Use explicit fail label here.

test_cg(Test, As, Fail, I, Vdb, Bef, St0) ->
    case test_type(Test, length(As)) of
    	{cond,Op} ->
	    {Ars,Ms,Int0} = cg_reg_args(As, Bef),
	    Int1 = clear_dead(Int0, I, Vdb),
	    {Ms ++ [{test,Op,{f,Fail},Ars}],
	     clear_dead(Int1, I, Vdb),
	     St0};
	{rev_cond,Op} ->
	    {[S1,S2],Ms,Int0} = cg_reg_args(As, Bef),
	    Int1 = clear_dead(Int0, I, Vdb),
	    {Ms ++ [{test,Op,{f,Fail},[S2,S1]}],
	     clear_dead(Int1, I, Vdb),
	     St0}
    end.

test_type(is_atom, 1)      -> {cond, is_atom};
test_type(is_binary, 1)    -> {cond, is_binary};
test_type(is_constant, 1)  -> {cond, is_constant};
test_type(is_float, 1)     -> {cond, is_float};
test_type(is_function, 1)  -> {cond, is_function};
test_type(is_integer, 1)   -> {cond, is_integer};
test_type(is_list, 1)      -> {cond, is_list};
test_type(is_number, 1)    -> {cond, is_number};
test_type(is_pid, 1)       -> {cond, is_pid};
test_type(is_port, 1)      -> {cond, is_port};
test_type(is_reference, 1) -> {cond, is_ref};
test_type(is_tuple, 1)     -> {cond, is_tuple};
test_type('=<', 2)  -> {rev_cond, is_ge};
test_type('>', 2)   -> {rev_cond, is_lt};
test_type('<', 2)   -> {cond, is_lt};
test_type('>=', 2)  -> {cond, is_ge};
test_type('==', 2)  -> {cond, is_eq};
test_type('/=', 2)  -> {cond, is_ne};
test_type('=:=', 2) -> {cond, is_eq_exact};
test_type('=/=', 2) -> {cond, is_ne_exact}.

guard_and_cg([G|Gs], Fail, Vdb, Bef, St0) ->
    {Gis,Int,St1} = guard_cg(G, Fail, Vdb, Bef, St0),
    {Gsis,Aft,St2} = guard_and_cg(Gs, Fail, Vdb, Bef, St1),
    {Gis ++ Gsis,Aft,St2};
guard_and_cg([], Fail, Vdb, Bef, St) -> {[],Bef,St}.

guard_or_cg([G], Succ, Fail, Vdb, Bef, St) ->
    %% This just falls straight through on success.
    guard_cg(G, Fail, Vdb, Bef, St);
guard_or_cg([G|Gs], Succ, Fail, Vdb, Bef, St0) ->
    {Next,St1} = new_label(St0),		%Next or alternative.
    {Gis,Int,St2} = guard_cg(G, Next, Vdb, Bef, St1),
    {Gsis,Aft,St3} = guard_or_cg(Gs, Succ, Fail, Vdb, Int, St2),
    {Gis ++ [{jump,{f,Succ}},{label,Next}] ++ Gsis,Aft,St3}.

%% guard_cg_list([Kexpr], Fail, I, Vdb, StackReg, St) ->
%%      {[Ainstr],StackReg,St}.

guard_cg_list(Kes, Fail, I, Vdb, Bef, St0) ->
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun (Ke, {Inta,Sta}) ->
			     {Keis,Intb,Stb} =
				 guard_cg(Ke, Fail, Vdb, Inta, Sta),
			     {comment(Inta) ++ Keis,{Intb,Stb}}
		     end, {Bef,St0}, need_heap(Kes, I, St0#cg.btype)),
    {Keis,Aft,St1}.

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

%% call_cg(Func, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%% enter_cg(Func, [Arg], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Call and enter first put the arguments into registers and save any
%%  other registers, then clean up and compress the stack and set the
%%  frame size. Finally the actual call is made.  Call then needs the
%%  return values filled in.

call_cg({make_fun,Func,Arity,Index,Uniq}, As, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {FuncLbl,St1} = local_func_label(Func, Arity, St0),
    MakeFun = case St0#cg.new_funs of
		  true -> {make_fun2,{f,FuncLbl},Index,Uniq,length(As)};
		  false -> {make_fun,{f,FuncLbl},Uniq,length(As)}
	      end,
    {comment({make_fun,{Func,Arity,Uniq},As}) ++ Sis ++
     [MakeFun],
     clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),
     St1};
call_cg({var,V}, As, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As++[{var,V}], Bef, Le#l.i, Vdb),
    %% Put return values in registers.
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {Frees,Aft} = free_dead(clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb)),
    {comment({call_fun,{var,V},As}) ++ Sis ++ Frees ++ [{call_fun,Arity}],
     Aft,need_stack_frame(St0)};
call_cg(Func, As, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    %% Put return values in registers.
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {Call,St1} = build_call(Func, Arity, St0),
    {Frees,Aft} = free_dead(clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb)),
    {comment({call,Func,As}) ++ Sis ++ Frees ++ Call,Aft,St1}.

build_call({remote,{atom,erlang},{atom,'!'}}, 2, St0) ->
    {[send],need_stack_frame(St0)};
build_call({remote,{atom,Mod},{atom,Name}}, Arity, St0) ->
    {[{call_ext,Arity,{extfunc,Mod,Name,Arity}}],need_stack_frame(St0)};
build_call(Name, Arity, St0) when atom(Name) ->
    {Lbl,St1} = local_func_label(Name, Arity, need_stack_frame(St0)),
    {[{call,Arity,{f,Lbl}}],St1}.

free_dead(#sr{stk=Stk0}=Aft) ->
    {Instr,Stk} = free_dead(Stk0, 0, [], []),
    {Instr,Aft#sr{stk=Stk}}.

free_dead([dead|Stk], Y, Instr, StkAcc) ->
    %% Note: kill/1 is equivalent to init/1 (translated by beam_asm).
    %% We use kill/1 to help further optimisation passes.
    free_dead(Stk, Y+1, [{kill,{yy,Y}}|Instr], [free|StkAcc]);
free_dead([Any|Stk], Y, Instr, StkAcc) ->
    free_dead(Stk, Y+1, Instr, [Any|StkAcc]);
free_dead([], Y, Instr, StkAcc) -> {Instr,reverse(StkAcc)}.

enter_cg({var,V}, As, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As++[{var,V}], Bef, Le#l.i, Vdb),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {comment({call_fun,{var,V},As}) ++ Sis ++ [{call_fun,Arity},return],
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     need_stack_frame(St0)};
enter_cg(Func, As, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {Call,St1} = build_enter(Func, Arity, St0),
    {comment({enter,Func,As}) ++ Sis ++ Call,
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     St1}.

build_enter({remote,{atom,erlang},{atom,'!'}}, 2, St0) ->
    {[send,return],need_stack_frame(St0)};
build_enter({remote,{atom,Mod},{atom,Name}}, Arity, St0) ->
    St1 = case trap_bif(Mod, Name, Arity) of
	      true -> need_stack_frame(St0);
	      false -> St0
	  end,
    {[{call_ext_only,Arity,{extfunc,Mod,Name,Arity}}],St1};
build_enter(Name, Arity, St0) when atom(Name) ->
    {Lbl,St1} = local_func_label(Name, Arity, St0),
    {[{call_only,Arity,{f,Lbl}}],St1}.

%% local_func_label(Name, Arity, State) -> {Label,State'}
%%  Get the function entry label for a local function.

local_func_label(Name, Arity, St0) ->
    Key = {Name,Arity},
    case keysearch(Key, 1, St0#cg.functable) of
  	{value,{Key,Label}} ->
	    {Label,St0};
  	false ->
  	    {Label,St1} = new_label(St0),
	    {Label,St1#cg{functable=[{Key,Label}|St1#cg.functable]}}
    end.

%% need_stack_frame(State) -> State'
%%  Make a note in the state that this function will need a stack frame.

need_stack_frame(#cg{need_frame=true}=St) -> St;
need_stack_frame(St) -> St#cg{need_frame=true}.

%% trap_bif(Mod, Name, Arity) -> true|false
%%   Trap bifs that need a stack frame.

trap_bif(erlang, '!', 2) -> true;
trap_bif(erlang, link, 1) -> true;
trap_bif(erlang, unlink, 1) -> true;
trap_bif(erlang, monitor_node, 2) -> true;
trap_bif(erlang, group_leader, 2) -> true;
trap_bif(erlang, exit, 2) -> true;
trap_bif(M, F, A) -> false.

%% bif_cg(Bif, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.

bif_cg(dsetelement, [Index0,Tuple0,New0], Rs, Le, Vdb, Bef, St0) ->
    {[New,Tuple,{integer,Index1}],Ms,Int} = cg_reg_args([New0,Tuple0,Index0], Bef),
    Index = Index1-1,
    {Ms ++ [{set_tuple_element,New,Tuple,Index}],
     clear_dead(Int, Le#l.i, Vdb), St0};
bif_cg(Bif, As, Rs, Le, Vdb, Bef, St0) ->
    {Ars,Ms,Int0} = cg_reg_args(As, Bef),

    %% If we are inside a catch, we must save everything that will
    %% be alive after the catch (because the BIF might fail and there
    %% will be a jump to the code after the catch).
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
bif_fail(exit, Fail, Ar) -> {f,0};
bif_fail(fail, Fail, Ar) -> {f,Fail}.

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

cg_recv_wait({atom,infinity}, Tes, I, Vdb, Bef, St0) ->
    %% We know that the 'after' body will never be executed.
    %% But to keep the stack and register information up to date,
    %% we will generate the code for the 'after' body, and then discard it.
    Int1 = clear_dead(Bef, I, Tes#l.vdb),
    {Tis,Int2,St1} = block_cg(Tes#l.ke, Tes#l.i, Tes#l.vdb,
			      Int1#sr{reg=clear_regs(Int1#sr.reg)}, St0),
    {[{wait,{f,St1#cg.recv}}],Int2,St1};
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
    {Cis,Int2,St2} = block_cg(C, Le#l.i, Le#l.vdb, Int1,
			      St1#cg{break=B,in_catch=true}),
    Aft = Int2#sr{reg=load_reg(R, 0, Int2#sr.reg),
		  stk=drop_catch(CatchTag, Int2#sr.stk)},
    {[{'catch',CatchReg,{f,B}}] ++ Cis ++
     [{label,B},{catch_end,CatchReg}],
     clear_dead(Aft, Le#l.i, Vdb),
     St2#cg{break=St1#cg.break,in_catch=St1#cg.in_catch}}.

%% set_cg([Var], Constr, Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  We have to be careful how a 'set' works. First the structure is
%%  built, then it is filled and finally things can be cleared. The
%%  annotation must reflect this and make sure that the return
%%  variable is allocated first.
%%
%%  In Beam, put_list for constructing a cons is an atomic instruction
%%  which can safely resuse one of the source registers as target.

set_cg([{var,R}], {cons,Es}, Le, Vdb, Bef, St) ->
    [S1, S2] = map(fun ({var,V}) -> fetch_var(V, Bef);
		       (Other) -> Other
		   end, Es),
    Int0 = clear_dead(Bef, Le#l.i, Vdb),
    Int1 = Int0#sr{reg=put_reg(R, Int0#sr.reg)},
    Ret = fetch_reg(R, Int1#sr.reg),
    {[{put_list,S1,S2,Ret}], Int1, St};
set_cg([{var,R}], Con, Le, Vdb, Bef, St) ->
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
	      {binary, Bin} ->
%%% AOJ: I'm unsure about the next line - it appears to work at present
		  Fail = bif_fail(St#cg.btype, St#cg.bfail, 42),
		  cg_binary(Bin, Ret, Fail, Bef);
	      Other ->
		  [{move,Other,Ret}]
	  end,
    {Ais,clear_dead(Int, Le#l.i, Vdb),St};
set_cg([], {binary,Bin}, Le, Vdb, Bef, St) ->
    Fail = bif_fail(St#cg.btype, St#cg.bfail, 42),
    Ais = cg_binary(Bin, find_scratch_reg(Bef#sr.reg), Fail, Bef),
    {Ais,clear_dead(Bef, Le#l.i, Vdb),St};
set_cg([], Con, Le, Vdb, Bef, St) ->
    %% This should have been stripped by compiler, just cleanup.
    {[],clear_dead(Bef, Le#l.i, Vdb), St}.

cg_binary(Segs, Ret, Fail, Bef) ->
    Code = cg_bin(Segs, Fail, Bef),
    [cg_bs_init(Code)] ++ need_bin_buf(Code) ++ [{bs_final,Fail,Ret}].

cg_bin({bin_seg,S0,U,T,Fs,[E0,Next]}, Fail, Bef) ->
    S1 = case S0 of
	     {var,Sv} -> fetch_var(Sv, Bef);
	     _ -> S0
	 end,
    E1 = case E0 of
	     {var, V} -> fetch_var(V, Bef);
	     Other ->	   Other
	 end,
    Op = case T of
	     integer -> bs_put_integer;
	     binary  -> bs_put_binary;
	     float   -> bs_put_float
	 end,
    [{Op,Fail,S1,U,{field_flags,Fs},E1}|cg_bin(Next, Fail, Bef)];
cg_bin(bin_end, Fail, Bef) -> [].

cg_bs_init(Code) ->
    {Size,Fs} = foldl(fun ({Op,Fail,{integer,N},U,_,_}, {S,Fs}) ->
		  {S + N*U,Fs};
	      (Other, {S,Fs}) ->
		  {S,[]}
	  end, {0,[exact]}, Code),
    {bs_init,(Size+7) div 8,{field_flags,Fs}}.

need_bin_buf(Code0) ->
    {Code1,F,H} = foldr(fun ({Op,Fail,{integer,N},U,_,_}=Bs, {Code,F,H}) ->
				{[Bs|Code],F,H + N*U};
			    ({Op,Fail,S,U,_,_}=Bs, {Code,F,H}) ->
				{[Bs|need_bin_buf_need(H, F, Code)],true,0}
			end, {[],false,0}, Code0),
    need_bin_buf_need(H, F, Code1).

need_bin_buf_need(0, false, Rest) -> Rest;
need_bin_buf_need(H, F, Rest) -> [{bs_need_buf,H}|Rest].

cg_build_args(As, Bef) ->
    map(fun ({var,V}) -> {put,fetch_var(V, Bef)};
	    (Other) -> {put,Other}
	end, As).

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

cg_reg_args(As, Bef) ->
    foldr(fun (A, {Rs,Ms0,Int0}) ->
		  {R,Ms,Int1} = cg_reg_arg(A, Int0),
		  {[R|Rs],Ms ++ Ms0,Int1}
	  end, {[],[],Bef}, As).

cg_reg_arg({var,V}, Bef) ->
    R = fetch_var(V, Bef),
    {R,[],Bef};
cg_reg_arg(Other, Bef) ->
    {Other,[],Bef}.

%% cg_setup_call([Arg], Bef, Cur, Vdb) -> {[Instr],Aft}.
%%  Do the complete setup for a call/enter.

cg_setup_call(As, Bef, I, Vdb) ->
    {Ms,Int0} = cg_call_args(As, Bef, I, Vdb),
    %% Have set up arguments, can now clean up, compress and save to stack.
    Int1 = Int0#sr{stk=clear_dead_stk(Int0#sr.stk, I, Vdb),res=[]},
    {Saves,Sis,Int2} = adjust_stack(Int1, I, I+1, Vdb),
    {Ms ++ Sis ++ [{'%live',length(As)}],Int2}.

%% cg_call_args([Arg], SrState) -> {[Instr],SrState}.
%%  Setup the arguments to a call/enter/bif. Put the arguments into
%%  consecutive registers starting at {x,0} moving any data which
%%  needs to be saved. Return a modified SrState structure with the
%%  new register contents.  N.B. the resultant register info will
%%  contain non-variable values when there are non-variable values.
%%
%%  This routine is complicated by unsaved values in x registers.
%%  We'll move away any unsaved values that are in the registers
%%  to be overwritten by the arguments.

cg_call_args(As, Bef, I, Vdb) ->
    Regs0 = load_arg_regs(Bef#sr.reg, As),
    Unsaved = unsaved_registers(Regs0, Bef#sr.stk, I, I+1, Vdb),
    {UnsavedMoves,Regs} = move_unsaved(Unsaved, Bef#sr.reg, Regs0),
    Moves0 = gen_moves(As, Bef),
    Moves = order_moves(Moves0, find_scratch_reg(Regs)),
    {UnsavedMoves ++ Moves,Bef#sr{reg=Regs}}.

%% load_arg_regs([Reg], Arguments) -> [Reg]
%%  Update the register descriptor to include the arguments (from {x,0}
%%  and upwards). Values in argument register are overwritten.
%%  Values in x registers above the arguments are preserved.

load_arg_regs(Regs, As) -> load_arg_regs(Regs, As, 0).

load_arg_regs([_|Rs], [{var,V}|As], I) -> [{I,V}|load_arg_regs(Rs, As, I+1)];
load_arg_regs([_|Rs], [A|As], I) -> [{I,A}|load_arg_regs(Rs, As, I+1)];
load_arg_regs([], [{var,V}|As], I) -> [{I,V}|load_arg_regs([], As, I+1)];
load_arg_regs([], [A|As], I) -> [{I,A}|load_arg_regs([], As, I+1)];
load_arg_regs(Rs, [], I) -> Rs.

%% Returns the variables must be saved and are currently in the
%% x registers that are about to be overwritten by the arguments.

unsaved_registers(Regs, Stk, Fb, Lf, Vdb) ->
    [V || {V,F,L} <- Vdb,
	  F < Fb,
	  L >= Lf,
	  not on_stack(V, Stk),
	  not in_reg(V, Regs)].

in_reg(V, Regs) -> keymember(V, 2, Regs).

%% Move away unsaved variables from the registers that are to be
%% overwritten by the arguments.
move_unsaved(Vs, OrigRegs, NewRegs) ->
    move_unsaved(Vs, OrigRegs, NewRegs, []).
    
move_unsaved([V|Vs], OrigRegs, NewRegs0, Acc) ->
    NewRegs = put_reg(V, NewRegs0),
    Src = fetch_reg(V, OrigRegs),
    Dst = fetch_reg(V, NewRegs),
    move_unsaved(Vs, OrigRegs, NewRegs, [{move,Src,Dst}|Acc]);
move_unsaved([], _, Regs, Acc) -> {Acc,Regs}.
    
%% gen_moves(As, Sr)
%%  Generate the basic move instruction to move the arguments
%%  to their proper registers. The list will be sorted on
%%  destinations. (I.e. the move to {x,0} will be first --
%%  see the comment to order_moves/2.)

gen_moves(As, Sr) -> gen_moves(As, Sr, 0, []).

gen_moves([{var,V}|As], Sr, I, Acc) ->
    case fetch_var(V, Sr) of
	{x,I} -> gen_moves(As, Sr, I+1, Acc);
	Reg -> gen_moves(As, Sr, I+1, [{move,Reg,{x,I}}|Acc])
    end;
gen_moves([A|As], Sr, I, Acc) ->
    gen_moves(As, Sr, I+1, [{move,A,{x,I}}|Acc]);
gen_moves([], Sr, I, Acc) -> lists:keysort(3, Acc).

%% order_moves([Move], ScratchReg) -> [Move]
%%  Orders move instruction so that source registers are not
%%  destroyed before they are used. If there are cycles
%%  (such as {move,{x,0},{x,1}}, {move,{x,1},{x,1}}),
%%  the scratch register is used to break up the cycle.
%%    If possible, the first move of the input list is placed
%%  last in the result list (to make the move to {x,0} occur
%%  just before the call to allow the Beam loader to coalesce
%%  the instructions).

order_moves(Ms, Scr) -> order_moves(Ms, Scr, []).

order_moves([{move,Src,Dst}=M|Ms0], ScrReg, Acc0) ->
    {Chain,Ms} = collect_chain(Ms0, [M], ScrReg),
    Acc = reverse(Chain, Acc0),
    order_moves(Ms, ScrReg, Acc);
order_moves([], ScrReg, Acc) -> Acc.

collect_chain(Ms, Path, ScrReg) ->
    collect_chain(Ms, Path, [], ScrReg).

collect_chain([{move,Src,Same}=M|Ms0], [{move,Same,_}|_]=Path, Others, ScrReg) ->
    case keysearch(Src, 3, Path) of
	{value,_} ->				%We have a cycle.
	    {break_up_cycle(M, Path, ScrReg),reverse(Others, Ms0)};
	false ->
	    collect_chain(reverse(Others, Ms0), [M|Path], [], ScrReg)
    end;
collect_chain([M|Ms], Path, Others, ScrReg) ->
    collect_chain(Ms, Path, [M|Others], ScrReg);
collect_chain([], Path, Others, ScrReg) ->
    {Path,Others}.

break_up_cycle({move,Src,Dst}=M, Path, ScrReg) ->
    [{move,ScrReg,Src},M|break_up_cycle1(Src, Path, ScrReg)].

break_up_cycle1(Dst, [{move,Src,Dst}=M|Path], ScrReg) ->
    [{move,Src,ScrReg}|Path];
break_up_cycle1(Dst, [M|Path], LastMove) ->
    [M|break_up_cycle1(Dst, Path, LastMove)].

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
		    Other -> dead		%Remove anything else
		end;
	    (free) -> free;
	    (dead) -> dead
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
longest([dead|T1], [free|T2]) -> [dead|longest(T1, T2)];
longest([free|T1], [dead|T2]) -> [dead|longest(T1, T2)];
longest([dead|T1], []) -> [dead|T1];
longest([], [dead|T2]) -> [dead|T2];
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

% find_var(V, Sr) ->
%     case find_reg(V, Sr#sr.reg) of
% 	{ok,R} -> {ok,R};
% 	error ->
% 	    case find_stack(V, Sr#sr.stk) of
% 		{ok,S} -> {ok,S};
% 		error -> error
% 	    end
%     end.

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

% put_regs(Vs, Rs) -> foldl(fun put_reg/2, Rs, Vs).

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

% free_reg(V, [{I,V}|Rs]) -> [free|Rs];
% free_reg(V, [R|Rs]) -> [R|free_reg(V, Rs)];
% free_reg(V, []) -> [].

fetch_reg(V, [{I,V}|SRs]) -> {x,I};
fetch_reg(V, [SR|SRs]) -> fetch_reg(V, SRs).

find_reg(V, [{I,V}|SRs]) -> {ok,{x,I}};
find_reg(V, [SR|SRs]) -> find_reg(V, SRs);
find_reg(V, []) -> error.

%% For the bit syntax, we need a scratch register if we are constructing
%% a binary that will not be used.

find_scratch_reg(Rs) -> find_scratch_reg(Rs, 0).
    
find_scratch_reg([free|Rs], I) -> {x,I};
find_scratch_reg([R|Rs], I) -> find_scratch_reg(Rs, I+1);
find_scratch_reg([], I) -> {x,I}.

%%copy_reg(Val, R, Regs) -> load_reg(Val, R, Regs).
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
put_stack(Val, [dead|Stk]) -> [{Val}|Stk];
put_stack(Val, [free|Stk]) -> [{Val}|Stk];
put_stack(Val, [NotFree|Stk]) -> [NotFree|put_stack(Val, Stk)].

put_stack_carefully(Val, Stk0) ->
    case catch put_stack_carefully1(Val, Stk0) of
	error -> error;
	Stk1 when list(Stk1) -> Stk1
    end.

put_stack_carefully1(Val, []) -> throw(error);
put_stack_carefully1(Val, [dead|Stk]) -> [{Val}|Stk];
put_stack_carefully1(Val, [free|Stk]) -> [{Val}|Stk];
put_stack_carefully1(Val, [NotFree|Stk]) ->
    [NotFree|put_stack_carefully1(Val, Stk)].

fetch_stack(Var, Stk) -> fetch_stack(Var, Stk, 0).

fetch_stack(V, [{V}|Stk], I) -> {yy,I};
fetch_stack(V, [O|Stk], I) -> fetch_stack(V, Stk, I+1).

% find_stack(Var, Stk) -> find_stack(Var, Stk, 0).

% find_stack(V, [{V}|Stk], I) -> {ok,{yy,I}};
% find_stack(V, [O|Stk], I) -> find_stack(V, Stk, I+1);
% find_stack(V, [], I) -> error.

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
