%%% $Id$
%%% x86 stack frame handling
%%%
%%% - apply temp -> reg/spill map from RA
%%% - map non-register temps to stack slots
%%% - add explicit stack management code to prologue and epilogue,
%%%   and at calls and tailcalls
%%%
%%% TODO:
%%% - Compute max stack in a pre-pass? (get rid of ref cell updates)
%%% - Merge all_temps, defun_minframe, and defun_exnlabs to a single
%%%   pass, for compile-time efficiency reasons.

-module(hipe_x86_frame).
-export([frame/2]).
-include("hipe_x86.hrl").
-include("../main/hipe.hrl").

frame({Defun,TempMap,FpMap}, Options) ->
    NewDefun =
    case get(hipe_inline_fp) of
	true ->
	    Defun1 = finalise_ra(Defun, TempMap, FpMap),
	    %%hipe_x86_pp:pp(Defun1),
	    hipe_x86_float:map(Defun1);
	_ ->
	    finalise_ra(Defun, TempMap, FpMap)
    end,
    %%hipe_x86_pp:pp(NewDefun),
    frame1(NewDefun, Options).

frame1(Defun, _Options) ->
    Formals = fix_formals(hipe_x86:defun_formals(Defun)),
    Temps0 = all_temps(hipe_x86:defun_code(Defun), Formals),
    MinFrame = defun_minframe(Defun),
    Temps = ensure_minframe(MinFrame, Temps0),
    ExnLabs = defun_exnlabs(Defun),
    CFG0 = hipe_x86_cfg:init(Defun),
    Liveness = hipe_x86_liveness:analyse(CFG0),
    CFG1 = do_body(CFG0, Liveness, Formals, Temps),
    CFG2 = hipe_x86_cfg:var_range_update(CFG1, []),
    CFG3 = add_exnlabs(ExnLabs, CFG2),
    hipe_x86_cfg:linearise(CFG3).

fix_formals(Formals) ->
    fix_formals(hipe_x86_registers:nr_args(), Formals).
fix_formals(N, [_|Rest]) when N > 0 -> fix_formals(N-1, Rest);
fix_formals(_, Formals) -> Formals.

do_body(CFG0, Liveness, Formals, Temps) ->
    Context = mk_context(Liveness, Formals, Temps),
    CFG1 = do_blocks(CFG0, Context),
    do_prologue(CFG1, Context).

do_blocks(CFG, Context) ->
    Labels = hipe_x86_cfg:labels(CFG),
    do_blocks(Labels, CFG, Context).

do_blocks([Label|Labels], CFG, Context) ->
    Liveness = context_liveness(Context),
    LiveOut = hipe_x86_liveness:liveout(Liveness, Label),
    Block = hipe_x86_cfg:bb(CFG, Label),
    Code = hipe_bb:code(Block),
    NewCode = do_block(Code, LiveOut, Context),
    NewBlock = hipe_bb:code_update(Block, NewCode),
    NewCFG = hipe_x86_cfg:bb_update(CFG, Label, NewBlock),
    do_blocks(Labels, NewCFG, Context);
do_blocks([], CFG, _) ->
    CFG.

do_block(Insns, LiveOut, Context) ->
    do_block(Insns, LiveOut, Context, context_framesize(Context), []).

do_block([I|Insns], LiveOut, Context, FPoff0, RevCode) ->
    {NewIs, FPoff1} = do_insn(I, LiveOut, Context, FPoff0),
    do_block(Insns, LiveOut, Context, FPoff1, lists:reverse(NewIs, RevCode));
do_block([], _, Context, FPoff, RevCode) ->
    FPoff0 = context_framesize(Context),
    if FPoff =:= FPoff0 -> [];
       true -> exit({?MODULE,do_block,FPoff})
    end,
    lists:reverse(RevCode, []).

do_insn(I, LiveOut, Context, FPoff) ->
    case I of
	#alu{} ->
	    {[do_alu(I, Context, FPoff)], FPoff};
	#cmp{} ->
	    {[do_cmp(I, Context, FPoff)], FPoff};
	#fop{} ->
	    {do_fop(I, Context, FPoff), FPoff};
	#move{} ->
	    {[do_move(I, Context, FPoff)], FPoff};
	#movsx{} ->
	    {[do_movsx(I, Context, FPoff)], FPoff};
	#movzx{} ->
	    {[do_movzx(I, Context, FPoff)], FPoff};
	#pseudo_call{} ->
	    do_pseudo_call(I, LiveOut, Context, FPoff);
	#pseudo_tailcall{} ->
	    {do_pseudo_tailcall(I, Context), context_framesize(Context)};
	#push{} ->
	    {[do_push(I, Context, FPoff)], FPoff+4};
	#ret{} ->
	    {do_ret(I, Context, FPoff), context_framesize(Context)};
	_ ->	% comment, jmp, label, pseudo_jcc, pseudo_tailcall_prepare
	    {[I], FPoff}
    end.

%%%
%%% Convert any pseudo-temp operand in a binary (alu, cmp, move)
%%% or unary (push) instruction to an explicit x86_mem operand.
%%%

do_alu(I, Context, FPoff) ->
    #alu{src=Src0,dst=Dst0} = I,
    Src = conv_opnd(Src0, FPoff, Context),
    Dst = conv_opnd(Dst0, FPoff, Context),
    I#alu{src=Src,dst=Dst}.

do_cmp(I, Context, FPoff) ->
    #cmp{src=Src0,dst=Dst0} = I,
    Src = conv_opnd(Src0, FPoff, Context),
    Dst = conv_opnd(Dst0, FPoff, Context),
    I#cmp{src=Src,dst=Dst}.

do_fop(I, Context, FPoff) ->
    #fop{src=Src0,dst=Dst0} = I,    
    Src = conv_opnd(Src0, FPoff, Context),
    Dst = conv_opnd(Dst0, FPoff, Context),
    [I#fop{src=Src,dst=Dst}].

do_move(I, Context, FPoff) ->
    #move{src=Src0,dst=Dst0} = I,
    Src = conv_opnd(Src0, FPoff, Context),
    Dst = conv_opnd(Dst0, FPoff, Context),
    I#move{src=Src,dst=Dst}.

do_movsx(I, Context, FPoff) ->
    #movsx{src=Src0,dst=Dst0} = I,
    Src = conv_opnd(Src0, FPoff, Context),
    Dst = conv_opnd(Dst0, FPoff, Context),
    I#movsx{src=Src,dst=Dst}.

do_movzx(I, Context, FPoff) ->
    #movzx{src=Src0,dst=Dst0} = I,
    Src = conv_opnd(Src0, FPoff, Context),
    Dst = conv_opnd(Dst0, FPoff, Context),
    I#movzx{src=Src,dst=Dst}.

do_push(I, Context, FPoff) ->
    #push{src=Src0} = I,
    Src = conv_opnd(Src0, FPoff, Context),
    I#push{src=Src}.

conv_opnd(Opnd, FPoff, Context) ->
    case opnd_is_pseudo(Opnd) of
	false ->
	    Opnd;
	true ->
	    conv_pseudo(Opnd, FPoff, Context)
    end.

conv_pseudo(Temp, FPoff, Context) ->
    Off = FPoff + context_offset(Context, Temp),
    conv_pseudo(Temp, Off).

conv_pseudo(Temp, Off) ->
    hipe_x86:mk_mem(mk_esp(), hipe_x86:mk_imm(Off), hipe_x86:temp_type(Temp)).

%%%
%%% Return - deallocate frame and emit 'ret $N' insn.
%%%

do_ret(_I, Context, FPoff) ->
    %% XXX: this conses up a new ret insn, ignoring the one rtl->x86 made
    adjust_esp(FPoff, [hipe_x86:mk_ret(4*context_arity(Context))]).

adjust_esp(N, Rest) ->
    if N =:= 0 ->
	    Rest;
       true ->
	    [hipe_x86:mk_alu('add', hipe_x86:mk_imm(N), mk_esp()) | Rest]
    end.

%%%
%%% Recursive calls.
%%%

do_pseudo_call(I, LiveOut, Context, FPoff0) ->
    JmpCode = [hipe_x86:mk_jmp_label(hipe_x86:pseudo_call_contlab(I))],
    ExnLab = hipe_x86:pseudo_call_exnlab(I),
    Fun0 = hipe_x86:pseudo_call_fun(I),
    Fun1 = conv_opnd(Fun0, FPoff0, Context),
    LiveTemps = [Temp || Temp <- LiveOut, temp_is_pseudo(Temp)],
    SDesc = mk_sdesc(ExnLab, Context, LiveTemps),
    CallCode = [hipe_x86:mk_call(Fun1, SDesc) | JmpCode],
    %% +4 for our RA and +4 for callee's RA should it need to call inc_stack
    context_need_stack(Context, FPoff0 + 8),
    OrigArity = hipe_x86:pseudo_call_arity(I),
    StkArity = max(0, OrigArity - hipe_x86_registers:nr_args()),
    ArgsBytes = 4 * StkArity,
    {CallCode, FPoff0 - ArgsBytes}.


%%%
%%% Create stack descriptors for call sites.
%%%

mk_sdesc(ExnLab, Context, Temps) ->	% for normal calls
    Temps0 = remove_floats(Temps), % Floats aren't tagged so mustn't be alive
    Live = mk_live(Context, Temps0),
    Arity = context_arity(Context),
    FSize = context_framesize(Context),
    hipe_x86:mk_sdesc(ExnLab, FSize div 4, Arity, list_to_tuple(Live)).

remove_floats(Temps)->
    [X || X <- Temps, hipe_x86:temp_type(X) /= 'double'].

mk_live(Context, Temps) ->
    lists:sort([temp_to_slot(Context, Temp) || Temp <- Temps]).

temp_to_slot(Context, Temp) ->
    (context_framesize(Context) + context_offset(Context, Temp)) div 4.

mk_minimal_sdesc(Context) ->		% for inc_stack_0 calls
    hipe_x86:mk_sdesc([], 0, context_arity(Context), {}).

%%%
%%% Tailcalls.
%%%

do_pseudo_tailcall(I, Context) ->	% always at FPoff=context_framesize(Context)
    Arity = context_arity(Context),
    Args = hipe_x86:pseudo_tailcall_stkargs(I) ++ [context_ra(Context)],
    Fun0 = hipe_x86:pseudo_tailcall_fun(I),
    {Insns, FPoff1, Fun1} = do_tailcall_args(Args, Context, Fun0),
    context_need_stack(Context, FPoff1),
    FPoff2 = FPoff1 + 4+4*Arity - 4*length(Args),
    context_need_stack(Context, FPoff2 + 4),	% +4 for callee's inc_stack RA
    I2 = hipe_x86:mk_jmp_fun(Fun1),
    Insns ++ adjust_esp(FPoff2, [I2]).

do_tailcall_args(Args, Context, Fun0) ->
    FPoff0 = context_framesize(Context),
    Arity = context_arity(Context),
    FrameTop = 4 + 4*Arity,
    DangerOff = FrameTop - 4*length(Args),
    Moves = mk_moves(Args, FrameTop, []),
    {Stores, Simple, Conflict} =
	split_moves(Moves, Context, DangerOff, [], [], []),
    %% sanity check (shouldn't trigger any more)
    if DangerOff < -FPoff0 ->
	    exit({?MODULE,do_tailcall_args,DangerOff,-FPoff0});
       true -> []
    end,
    FPoff1 = FPoff0,
    %%
    {Pushes, MoreSimple, FPoff2} = split_conflict(Conflict, FPoff1, [], []),
    %%
    {PushFun0, FPoff3, LoadFun1, Fun1} =
	case opnd_is_pseudo(Fun0) of
	    false ->
		{[], FPoff2, [], Fun0};
	    true ->
		Type = hipe_x86:temp_type(Fun0),
		Temp1 = mk_temp1(Type),
		Fun0Off = context_offset(Context, Fun0),
		MEM0 = conv_pseudo(Fun0, FPoff2 + Fun0Off),
		if Fun0Off >= DangerOff ->
			Fun1Off = hipe_x86:mk_imm(0),
			MEM1 = hipe_x86:mk_mem(mk_esp(), Fun1Off, Type),
			{[hipe_x86:mk_push(MEM0)],
			 FPoff2 + 4,
			 [hipe_x86:mk_move(MEM1, Temp1)],
			 Temp1};
		   true ->
			{[], FPoff2, [hipe_x86:mk_move(MEM0, Temp1)], Temp1}
		end
	end,
    %%
    RegTemp0 = hipe_x86_registers:temp0(),
    TempReg =
	case hipe_x86:is_temp(Fun1) of
	    true ->
		RegFun1 = hipe_x86:temp_reg(Fun1),
		if RegFun1 =/= RegTemp0 -> RegTemp0;
		   true -> hipe_x86_registers:temp1()
		end;
	    false ->
		RegTemp0
	end,
    %%
    {Pushes ++ PushFun0 ++
     store_moves(Stores, FPoff3, LoadFun1 ++
		 simple_moves(Simple, FPoff3, TempReg,
			      simple_moves(MoreSimple, FPoff3, TempReg,
					   []))),
     FPoff3, Fun1}.

mk_moves([Arg|Args], Off, Moves) ->
    Off1 = Off - 4,
    mk_moves(Args, Off1, [{Arg,Off1}|Moves]);
mk_moves([], _, Moves) ->
    Moves.

split_moves([Move|Moves], Context, DangerOff, Stores, Simple, Conflict) ->
    {Src,DstOff} = Move,
    case src_is_pseudo(Src) of
	false ->
	    split_moves(Moves, Context, DangerOff, [Move|Stores],
			Simple, Conflict);
	true ->
	    SrcOff = context_offset(Context, Src),
	    Type = typeof_src(Src),
	    if SrcOff =:= DstOff ->
		    split_moves(Moves, Context, DangerOff, Stores,
				Simple, Conflict);
	       SrcOff >= DangerOff ->
		    split_moves(Moves, Context, DangerOff, Stores,
				Simple, [{SrcOff,DstOff,Type}|Conflict]);
	       true ->
		    split_moves(Moves, Context, DangerOff, Stores,
				[{SrcOff,DstOff,Type}|Simple], Conflict)
	    end
    end;
split_moves([], _, _, Stores, Simple, Conflict) ->
    {Stores, Simple, Conflict}.

split_conflict([{SrcOff,DstOff,Type}|Conflict], FPoff, Pushes, Simple) ->
    Push = hipe_x86:mk_push(hipe_x86:mk_mem(mk_esp(), hipe_x86:mk_imm(FPoff+SrcOff), Type)),
    split_conflict(Conflict, FPoff+4, [Push|Pushes], [{-(FPoff+4),DstOff,Type}|Simple]);
split_conflict([], FPoff, Pushes, Simple) ->
    {lists:reverse(Pushes), Simple, FPoff}.

simple_moves([{SrcOff,DstOff,Type}|Moves], FPoff, TempReg, Rest) ->
    Temp = hipe_x86:mk_temp(TempReg, Type),
    ESP = mk_esp(),
    LoadOff = hipe_x86:mk_imm(FPoff+SrcOff),
    LD = hipe_x86:mk_move(hipe_x86:mk_mem(ESP, LoadOff, Type), Temp),
    StoreOff = hipe_x86:mk_imm(FPoff+DstOff),
    ST = hipe_x86:mk_move(Temp, hipe_x86:mk_mem(ESP, StoreOff, Type)),
    simple_moves(Moves, FPoff, TempReg, [LD, ST | Rest]);
simple_moves([], _, _, Rest) ->
    Rest.

store_moves([{Src,DstOff}|Moves], FPoff, Rest) ->
    Type = typeof_src(Src),
    ESP = mk_esp(),
    StoreOff = hipe_x86:mk_imm(FPoff+DstOff),
    ST = hipe_x86:mk_move(Src, hipe_x86:mk_mem(ESP, StoreOff, Type)),
    store_moves(Moves, FPoff, [ST | Rest]);
store_moves([], _, Rest) ->
    Rest.

%%%
%%% Contexts
%%%

-record(context, {liveness, framesize, arity, map, ra, ref_maxstack}).

mk_context(Liveness, Formals, Temps) ->
    RA = hipe_x86:mk_new_temp('untagged'),
    {Map, MinOff}  = mk_temp_map(Formals, RA, Temps),
    FrameSize = (-MinOff),
    RefMaxStack = hipe_bifs:ref(FrameSize),
    Context = #context{liveness=Liveness,
		       framesize=FrameSize, arity=length(Formals),
		       map=Map, ra=RA, ref_maxstack=RefMaxStack},
    Context.

context_need_stack(#context{ref_maxstack=RM}, N) ->
    M = hipe_bifs:ref_get(RM),
    if N > M -> hipe_bifs:ref_set(RM, N);
       true -> []
    end.

context_maxstack(#context{ref_maxstack=RM}) ->
    hipe_bifs:ref_get(RM).

context_arity(#context{arity=Arity}) ->
    Arity.

context_framesize(#context{framesize=FrameSize}) ->
    FrameSize.

context_liveness(#context{liveness=Liveness}) ->
    Liveness.

context_offset(#context{map=Map}, Temp) ->
    tmap_lookup(Map, Temp).

context_ra(#context{ra=RA}) ->
    RA.

mk_temp_map(Formals, RA, Temps) ->
    {Map, _} = enter_vars(Formals, 4*length(Formals),
			  tmap_bind(tmap_empty(), RA, 0)),
    enter_vars(Temps, -4, Map).

enter_vars([V|Vs], Off, Map) ->
    case hipe_x86:temp_type(V) of
	'double' -> enter_vars(Vs, Off-8, tmap_bind(Map, V, Off-4));
	_ -> enter_vars(Vs, Off-4, tmap_bind(Map, V, Off))
    end;
enter_vars([], Off, Map) ->
    {Map, Off+4}.

tmap_empty() ->
    gb_trees:empty().

tmap_bind(Map, Key, Val) ->
    gb_trees:insert(Key, Val, Map).

tmap_lookup(Map, Key) ->
    gb_trees:get(Key, Map).

%%%
%%% do_prologue: prepend stack frame allocation code.
%%%
%%% NewStart:
%%%	temp0 = esp - MaxStack
%%%	if( temp0 < ESP_LIMIT(P) ) goto IncStack else goto AllocFrame
%%% AllocFrame:
%%%	esp -= FrameSize
%%%	goto OldStart
%%% OldStart:
%%%	...
%%% IncStack:
%%%	call inc_stack
%%%	goto NewStart

do_prologue(CFG, Context) ->
    MaxStack = context_maxstack(Context),
    if MaxStack > 0 ->
	    FrameSize = context_framesize(Context),
	    OldStartLab = hipe_x86_cfg:start_label(CFG),
	    NewStartLab = hipe_gensym:get_next_label(x86),
	    AllocFrameLab = hipe_gensym:get_next_label(x86),
	    IncStackLab = hipe_gensym:get_next_label(x86),
	    %%
	    Type = 'untagged',
	    Preg = hipe_x86_registers:proc_pointer(),
	    Pbase = hipe_x86:mk_temp(Preg, Type),
	    ESP_LIMIT_OFF = hipe_x86:mk_imm(hipe_x86_registers:esp_limit_offset()),
	    Temp0 = mk_temp0(Type),
	    ESP = mk_esp(),
	    NewStartCode =
		%% hopefully this lea is faster than the mov;sub it replaced
		[hipe_x86:mk_lea(hipe_x86:mk_mem(ESP, hipe_x86:mk_imm(-MaxStack), 'untagged'), Temp0),
		 hipe_x86:mk_cmp(hipe_x86:mk_mem(Pbase, ESP_LIMIT_OFF, Type), Temp0),
		 hipe_x86:mk_pseudo_jcc('b', IncStackLab, AllocFrameLab, 0.01)],
	    %%
	    AllocFrameCode =
		%% XXX: use adjust_esp instead?
		case FrameSize of
		    0 ->
			%% XXX: candidate for dummy block removal
			[hipe_x86:mk_jmp_label(OldStartLab)];
		    _ ->
			[hipe_x86:mk_alu('sub', hipe_x86:mk_imm(FrameSize), ESP),
			 hipe_x86:mk_jmp_label(OldStartLab)]
		end,
	    %%
	    IncStackCode =
		[hipe_x86:mk_call(hipe_x86:mk_prim('inc_stack_0'),
				  mk_minimal_sdesc(Context)),
		 hipe_x86:mk_jmp_label(NewStartLab)],
	    %%
	    {LLo,_} = hipe_x86_cfg:label_range(CFG),
	    LHi = hipe_gensym:get_label(x86),
	    CFG0 = hipe_x86_cfg:label_range_update(CFG, {LLo,LHi}),
	    %%
	    CFG1 = hipe_x86_cfg:bb_add(CFG0, NewStartLab, hipe_bb:mk_bb(NewStartCode)),
	    CFG2 = hipe_x86_cfg:bb_add(CFG1, AllocFrameLab, hipe_bb:mk_bb(AllocFrameCode)),
	    CFG3 = hipe_x86_cfg:bb_add(CFG2, IncStackLab, hipe_bb:mk_bb(IncStackCode)),
	    CFG4 = hipe_x86_cfg:start_label_update(CFG3, NewStartLab),
	    %%
	    CFG4;
       true ->
	    CFG
    end.

%%% typeof_src -- what's src's type?

typeof_src(Src) ->
    case Src of
	#x86_imm{} ->
	    'untagged';
	#x86_temp{} ->
	    hipe_x86:temp_type(Src);
	#x86_mem{} ->
	    hipe_x86:mem_type(Src)
    end.

%%% Cons up an '%esp' Temp.

mk_esp() ->
    hipe_x86:mk_temp(hipe_x86_registers:esp(), 'untagged').

%%% Cons up a '%temp0' Temp.

mk_temp0(Type) ->
    hipe_x86:mk_temp(hipe_x86_registers:temp0(), Type).

%%% Cons up a '%temp1' Temp.

mk_temp1(Type) ->
    hipe_x86:mk_temp(hipe_x86_registers:temp1(), Type).

%%% Check if an operand is a pseudo-Temp.

src_is_pseudo(Src) ->
    opnd_is_pseudo(Src).

opnd_is_pseudo(Opnd) ->
    case hipe_x86:is_temp(Opnd) of
	true -> temp_is_pseudo(Opnd);
	false -> false
    end.

temp_is_pseudo(Temp) ->
    case hipe_x86:is_temp(Temp) of
	true -> 
	    not(hipe_x86_registers:is_precoloured(hipe_x86:temp_reg(Temp)));
	false -> 
	    false
    end.

    
%%%
%%% Build the set of all temps used in a Defun's body.
%%%

all_temps(Code, Formals) ->
    S0 = find_temps(Code, tset_empty()),
    S1 = tset_del_list(S0, Formals),
    S2 = tset_filter(S1, fun(T) -> temp_is_pseudo(T) end),
    S2.

find_temps([I|Insns], S0) ->
    S1 = tset_add_list(S0, hipe_x86_defuse:insn_def(I)),
    S2 = tset_add_list(S1, hipe_x86_defuse:insn_use(I)),
    find_temps(Insns, S2);
find_temps([], S) ->
    S.

tset_empty() ->
    ordsets:new().

tset_size(S) ->
    ordsets:size(S).

tset_insert(S, T) ->
    ordsets:add_element(T, S).

tset_add_list(S, Ts) ->
    ordsets:union(S, ordsets:from_list(Ts)).

tset_del_list(S, Ts) ->
    ordsets:subtract(S, ordsets:from_list(Ts)).

tset_filter(S, F) ->
    ordsets:filter(F, S).

%%%
%%% Compute minimum permissible frame size, ignoring spilled temps.
%%% This is done to ensure that we won't have to adjust the frame size
%%% in the middle of a tailcall.
%%%

defun_minframe(Defun) ->
    MaxTailArity = body_mta(hipe_x86:defun_code(Defun), 0),
    MyArity = length(fix_formals(hipe_x86:defun_formals(Defun))),
    max(MaxTailArity - MyArity, 0).

body_mta([I|Code], MTA) ->
    body_mta(Code, insn_mta(I, MTA));
body_mta([], MTA) ->
    MTA.

insn_mta(I, MTA) ->
    case I of
	#pseudo_tailcall{arity=Arity} ->
	    max(MTA, Arity - hipe_x86_registers:nr_args());
	_ -> MTA
    end.

max(X, Y) -> % why isn't max/2 a standard BIF?
    if X > Y -> X; true -> Y end.

%%%
%%% Ensure that we have enough temps to satisfy the minimum frame size,
%%% if necessary by prepending unused dummy temps.
%%%

ensure_minframe(MinFrame, Temps) ->
    ensure_minframe(MinFrame, tset_size(Temps), Temps).

ensure_minframe(MinFrame, Frame, Temps) ->
    if MinFrame > Frame ->
	    Temp = hipe_x86:mk_new_temp('untagged'),
	    ensure_minframe(MinFrame, Frame+1, tset_insert(Temps, Temp));
       true -> Temps
    end.

%%% workaround for "vanishing catch blocks" problem

defun_exnlabs(Defun) ->
    code_exnlabs(hipe_x86:defun_code(Defun), ordsets:new()).

code_exnlabs([I|Code], Set) ->
    code_exnlabs(Code, insn_exnlab(I, Set));
code_exnlabs([], Set) ->
    ordsets:to_list(Set).

insn_exnlab(I, Set) ->
    case I of
	#pseudo_call{exnlab=ExnLab} ->
	    case ExnLab of
		[] -> Set;
		_ -> ordsets:add_element(ExnLab, Set)
	    end;
	_ -> Set
    end.

add_exnlabs([], CFG) -> CFG;
add_exnlabs([L|Ls], CFG) ->
    add_exnlabs(Ls, hipe_x86_cfg:add_fail_entrypoint(CFG, L)).

%%%
%%% Finalise the temp->reg/spill mapping.
%%% (XXX: maybe this should be merged with the main pass,
%%% but I just want this to work now)
%%%

finalise_ra(Defun, [], []) ->
    Defun;
finalise_ra(Defun, TempMap, FpMap) ->
    Code = hipe_x86:defun_code(Defun),
    {_, SpillLimit} = hipe_x86:defun_var_range(Defun),
    Map = mk_ra_map(TempMap, SpillLimit),
    FpMap0 = mk_ra_map_fp(FpMap, SpillLimit),
    NewCode = ra_code(Code, Map, FpMap0),
    Defun#defun{code=NewCode}.

ra_code(Code, Map, FpMap) ->
    [ra_insn(I, Map, FpMap) || I <- Code].

ra_insn(I, Map, FpMap) ->
    case I of
	#alu{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map),
	    Dst = ra_opnd(Dst0, Map),
	    I#alu{src=Src,dst=Dst};
	#call{} ->
	    I;
	#cmovcc{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map),
	    Dst = ra_opnd(Dst0, Map),
	    I#cmovcc{src=Src,dst=Dst};
	#cmp{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map),
	    Dst = ra_opnd(Dst0, Map),
	    I#cmp{src=Src,dst=Dst};
	#comment{} ->
	    I;
	#dec{dst=Dst0} ->
	    Dst = ra_opnd(Dst0, Map),
	    I#dec{dst=Dst};
	#finit{}->
	    I;
	#fmov{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map, FpMap),
	    Dst = ra_opnd(Dst0, Map, FpMap),
	    I#fmov{src=Src,dst=Dst};
	#fop{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map, FpMap),
	    Dst = ra_opnd(Dst0, Map, FpMap),
	    I#fop{src=Src,dst=Dst};
	#inc{dst=Dst0} ->
	    Dst = ra_opnd(Dst0, Map),
	    I#inc{dst=Dst};
	#jcc{} ->
	    I;
	#jmp_fun{'fun'=Fun0} ->
	    Fun = ra_opnd(Fun0, Map),
	    I#jmp_fun{'fun'=Fun};
	#jmp_label{} ->
	    I;
	#jmp_switch{temp=Temp0} ->
	    Temp = ra_temp(Temp0, Map),
	    I#jmp_switch{temp=Temp};
	#label{} ->
	    I;
	#lea{mem=Mem0,temp=Temp0} ->
	    Mem = ra_mem(Mem0, Map),
	    Temp = ra_temp(Temp0, Map),
	    I#lea{mem=Mem,temp=Temp};
	#move{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map),
	    Dst = ra_opnd(Dst0, Map),
	    I#move{src=Src,dst=Dst};
	#movsx{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map),
	    Dst = ra_opnd(Dst0, Map),
	    I#movsx{src=Src,dst=Dst};
	#movzx{src=Src0,dst=Dst0} ->
	    Src = ra_opnd(Src0, Map),
	    Dst = ra_opnd(Dst0, Map),
	    I#movzx{src=Src,dst=Dst};
	#nop{} ->
	    I;
	#pseudo_call{'fun'=Fun0} ->
	    Fun = ra_opnd(Fun0, Map),
	    I#pseudo_call{'fun'=Fun};
	#pseudo_jcc{} ->
	    I;
	#pseudo_tailcall{'fun'=Fun0,stkargs=StkArgs0} ->
	    Fun = ra_opnd(Fun0, Map),
	    StkArgs = ra_args(StkArgs0, Map),
	    I#pseudo_tailcall{'fun'=Fun,stkargs=StkArgs};
	#pseudo_tailcall_prepare{} ->
	    I;
	#push{src=Src0} ->
	    Src = ra_opnd(Src0, Map),
	    I#push{src=Src};
	#ret{} ->
	    I;
	_ ->
	    exit({?MODULE,ra_insn,I})
    end.

ra_args(Args, Map) ->
    [ra_opnd(Opnd, Map) || Opnd <- Args].

ra_opnd(Opnd, Map) ->
    ra_opnd(Opnd, Map, tmap_empty()).
ra_opnd(Opnd, Map, FpMap) ->
    case Opnd of
	#x86_temp{} -> ra_temp(Opnd, Map, FpMap);
	#x86_mem{} -> ra_mem(Opnd, Map);
	_ -> Opnd
    end.

ra_mem(Mem, Map) ->
    #x86_mem{base=Base0,off=Off0} = Mem,
    Base = ra_opnd(Base0, Map),
    Off = ra_opnd(Off0, Map),
   %% #x86_mem{base=Base,off=Off}.
    Mem#x86_mem{base=Base,off=Off}.

ra_temp(Temp, Map) ->
    ra_temp(Temp, Map, tmap_empty()).

ra_temp(Temp, Map, FpMap) ->
    Reg = hipe_x86:temp_reg(Temp),
    case hipe_x86:temp_type(Temp) of
	double ->
	    case gb_trees:lookup(Reg, FpMap) of
		{value,NewReg} -> 
		    case on_fpstack(NewReg) of
			true -> hipe_x86:mk_fpreg(NewReg);
		        false ->
			    Temp#x86_temp{reg=NewReg}
		    end;
		_ ->
		    Temp
	    end;
	_->
	    case hipe_x86_registers:is_precoloured(Reg) of
		true ->
		    Temp;
		_ ->
		    case gb_trees:lookup(Reg, Map) of
			{value,NewReg} -> Temp#x86_temp{reg=NewReg};
			_ -> Temp
		    end
	    end
    end.

mk_ra_map(TempMap, SpillLimit) ->
    %% Build a partial map from pseudo to reg or spill.
    %% Spills are represented as pseudos with indices above SpillLimit.
    %% (I'd prefer to use negative indices, but that breaks
    %% hipe_x86_registers:is_precoloured/1.)
    %% The frame mapping proper is unchanged, since spills look just like
    %% ordinary (un-allocated) pseudos.
    lists:foldl(fun(MapLet, Map) ->
			{Key,Val} = conv_ra_maplet(MapLet, SpillLimit),
			gb_trees:insert(Key, Val, Map)
		end,
		gb_trees:empty(),
		TempMap).

conv_ra_maplet(MapLet = {From,To}, SpillLimit) ->
    %% From should be a pseudo, or a hard reg mapped to itself.
    if is_integer(From), From =< SpillLimit ->
	    case hipe_x86_registers:is_precoloured(From) of
		false -> [];
		_ ->
		    case To of
			{reg, From} -> [];
			_ -> ?EXIT({?MODULE,conv_ra_maplet,MapLet})
		    end
	    end;
       true -> ?EXIT({?MODULE,conv_ra_maplet,MapLet})
    end,
    %% end of From check
    case To of
	{reg, NewReg} ->
	    %% NewReg should be a hard reg, or a pseudo mapped
	    %% to itself (formals are handled this way).
	    if is_integer(NewReg) ->
		    case hipe_x86_registers:is_precoloured(NewReg) of
			true -> [];
			_ -> if From =:= NewReg -> [];
				true ->
				     ?EXIT({?MODULE,conv_ra_maplet,MapLet})
			    end
		    end;
	       true -> ?EXIT({?MODULE,conv_ra_maplet,MapLet})
	    end,
	    %% end of NewReg check
	    {From, NewReg};
	{spill, SpillIndex} ->
	    %% SpillIndex should be >= 0.
	    if is_integer(SpillIndex), SpillIndex >= 0 -> [];
	       true -> ?EXIT({?MODULE,conv_ra_maplet,MapLet})
	    end,
	    %% end of SpillIndex check
	    ToTempNum = SpillLimit+SpillIndex+1,
	    MaxTempNum = hipe_gensym:get_var(),
	    if MaxTempNum >= ToTempNum -> [];
	       true -> hipe_gensym:set_var(ToTempNum)
	    end,
	    {From, ToTempNum};
	_ -> ?EXIT({?MODULE,conv_ra_maplet,MapLet})
    end.

mk_ra_map_fp(FpMap, SpillLimit) ->
    lists:foldl(fun(MapLet, Map) ->
			{Key,Val} = conv_ra_maplet_fp(MapLet, SpillLimit),
			gb_trees:insert(Key, Val, Map)
		end,
		gb_trees:empty(),
		FpMap).

conv_ra_maplet_fp(MapLet = {From,To}, SpillLimit) ->
    %% From should be a pseudo
    if is_integer(From), From =< SpillLimit -> [];
       true -> ?EXIT({?MODULE,conv_ra_maplet_fp,MapLet})
    end,
    %% end of From check
    case To of
	{reg, NewReg} ->
	    case on_fpstack(NewReg) of
		true-> [];
		false -> ?EXIT({?MODULE,conv_ra_maplet_fp,MapLet})
	    end,
	    %% end of NewReg check.
	    {From, NewReg};
	{spill, SpillIndex} ->
	    %% SpillIndex should be >= 0.
	    if is_integer(SpillIndex), SpillIndex >= 0 -> [];
	       true -> ?EXIT({?MODULE,conv_ra_maplet_fp,MapLet})
	    end,
	    %% end of SpillIndex check
	    ToTempNum = SpillLimit+SpillIndex+1,
	    MaxTempNum = hipe_gensym:get_var(),
	    if MaxTempNum >= ToTempNum -> [];
	       true -> hipe_gensym:set_var(ToTempNum)
	    end,
	    {From, ToTempNum};
	_ -> ?EXIT({?MODULE,conv_ra_maplet_fp,MapLet})
    end.

on_fpstack(S)->
    hipe_x86_specific_fp:is_precolored(S).
