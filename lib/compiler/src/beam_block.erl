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
%% Purpose : Partitions assembly instructions into basic blocks and
%% optimizes them.

-module(beam_block).

-export([module/2]).
-export([live_at_entry/1]).			%Used by beam_type, beam_bool.
-export([is_killed/2]).				%Used by beam_dead, beam_type, beam_bool.
-export([is_not_used/2]).			%Used by beam_bool.
-export([merge_blocks/2]).			%Used by beam_jump.
-import(lists, [map/2,mapfoldr/3,reverse/1,reverse/2,foldl/3,
		member/2,sort/1,all/2]).
-define(MAXREG, 1024).

module({Mod,Exp,Attr,Fs,Lc}, _Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    %% Collect basic blocks and optimize them.
    Is1 = beam_jump:remove_unused_labels(Is0),	%Extra labels may thwart optimizations.
    Is2 = blockify(Is1),
    Is = bsm_opt(Is2),

    %% Done.
    {function,Name,Arity,CLabel,Is}.

%% blockify(Instructions0) -> Instructions
%%  Collect sequences of instructions to basic blocks and
%%  optimize the contents of the blocks. Also do some simple
%%  optimations on instructions outside the blocks.

blockify(Is) ->
    blockify(Is, []).

blockify([{loop_rec,{f,Fail},{x,0}},{loop_rec_end,_Lbl},{label,Fail}|Is], Acc) ->
    %% Useless instruction sequence.
    blockify(Is, Acc);

%% New bit syntax matching.
blockify([{bs_save2,R,Point}=I,{bs_restore2,R,Point}|Is], Acc) ->
    blockify([I|Is], Acc);
blockify([{bs_save2,R,Point}=I,{test,is_eq_exact,_,_}=Test,
	  {bs_restore2,R,Point}|Is], Acc) ->
    blockify([I,Test|Is], Acc);

%% Old bit syntax matching.
blockify([{test,bs_test_tail,F,[Bits]}|Is],
	 [{test,bs_skip_bits,F,[{integer,I},Unit,_Flags]}|Acc]) ->
    blockify(Is, [{test,bs_test_tail,F,[Bits+I*Unit]}|Acc]);
blockify([{test,bs_skip_bits,F,[{integer,I1},Unit1,_]}|Is],
	 [{test,bs_skip_bits,F,[{integer,I2},Unit2,Flags]}|Acc]) ->
    blockify(Is, [{test,bs_skip_bits,F,
		   [{integer,I1*Unit1+I2*Unit2},1,Flags]}|Acc]);

%% Do other peep-hole optimizations.
blockify([{test,is_atom,{f,Fail},[Reg]}=I|
	  [{select_val,Reg,{f,Fail},
	    {list,[{atom,false},{f,_}=BrFalse,
		   {atom,true}=AtomTrue,{f,_}=BrTrue]}}|Is]=Is0],
	 [{block,Bl}|_]=Acc) ->
    case is_last_bool(Bl, Reg) of
	false ->
	    blockify(Is0, [I|Acc]);
	true ->
	    blockify(Is, [{jump,BrTrue},
			  {test,is_eq_exact,BrFalse,[Reg,AtomTrue]}|Acc])
    end;
blockify([{test,is_atom,{f,Fail},[Reg]}=I|
	  [{select_val,Reg,{f,Fail},
	    {list,[{atom,true}=AtomTrue,{f,_}=BrTrue,
		   {atom,false},{f,_}=BrFalse]}}|Is]=Is0],
	 [{block,Bl}|_]=Acc) ->
    case is_last_bool(Bl, Reg) of
	false ->
	    blockify(Is0, [I|Acc]);
	true ->
	    blockify(Is, [{jump,BrTrue},
			  {test,is_eq_exact,BrFalse,[Reg,AtomTrue]}|Acc])
    end;
blockify([I|Is0]=IsAll, Acc) ->
    case is_bs_put(I) of
	true ->
	    {BsPuts0,Is} = collect_bs_puts(IsAll),
	    BsPuts = opt_bs_puts(BsPuts0),
	    blockify(Is, reverse(BsPuts, Acc));
	false ->
	    case collect(I) of
		error -> blockify(Is0, [I|Acc]);
		Instr when is_tuple(Instr) ->
		    {Block0,Is} = collect_block(IsAll),
		    Block = opt_block(Block0),
		    blockify(Is, [{block,Block}|Acc])
	    end
    end;
blockify([], Acc) -> reverse(Acc).

is_last_bool([I,{'%live',_}], Reg) ->
    is_last_bool([I], Reg);
is_last_bool([{set,[Reg],As,{bif,N,_}}], Reg) ->
    Ar = length(As),
    erl_internal:new_type_test(N, Ar) orelse erl_internal:comp_op(N, Ar)
	orelse erl_internal:bool_op(N, Ar);
is_last_bool([_|Is], Reg) -> is_last_bool(Is, Reg);
is_last_bool([], _) -> false.

collect_block(Is) ->
    collect_block(Is, []).

collect_block([{allocate_zero,Ns,R},{test_heap,Nh,R}|Is], Acc) ->
    collect_block(Is, [{set,[],[],{alloc,R,{no_opt,Ns,Nh,[]}}}|Acc]);
collect_block([I|Is]=Is0, Acc) ->
    case collect(I) of
	error -> {reverse(Acc),Is0};
	Instr -> collect_block(Is, [Instr|Acc])
    end;
collect_block([], Acc) -> {reverse(Acc),[]}.

collect({allocate_zero,N,R}) -> {set,[],[],{alloc,R,{zero,N,0,[]}}};
collect({test_heap,N,R})     -> {set,[],[],{alloc,R,{nozero,nostack,N,[]}}};
collect({bif,N,nofail,As,D}) -> {set,[D],As,{bif,N}};
collect({bif,N,F,As,D})      -> {set,[D],As,{bif,N,F}};
collect({gc_bif,N,F,R,As,D}) -> {set,[D],As,{alloc,R,{gc_bif,N,F}}};
collect({move,S,D})          -> {set,[D],[S],move};
collect({put_list,S1,S2,D})  -> {set,[D],[S1,S2],put_list};
collect({put_tuple,A,D})     -> {set,[D],[],{put_tuple,A}};
collect({put,S})             -> {set,[],[S],put};
collect({put_string,L,S,D})  -> {set,[D],[],{put_string,L,S}};
collect({get_tuple_element,S,I,D}) -> {set,[D],[S],{get_tuple_element,I}};
collect({set_tuple_element,S,D,I}) -> {set,[],[S,D],{set_tuple_element,I}};
collect({get_list,S,D1,D2})  -> {set,[D1,D2],[S],get_list};
collect(remove_message)      -> {set,[],[],remove_message};
collect({'catch',R,L})       -> {set,[R],[],{'catch',L}};
collect({'%live',_}=Live)    -> Live;
collect(_)                   -> error.

opt_block(Is0) ->
    %% We explicitly move any allocate instruction upwards before optimising
    %% moves, to avoid any potential problems with the calculation of live
    %% registers.
    Is1 = move_allocates(Is0),
    Is = find_fixpoint(fun opt/1, Is1),
    opt_alloc(Is).

find_fixpoint(OptFun, Is0) ->
    case OptFun(Is0) of
	Is0 -> Is0;
	Is1 -> find_fixpoint(OptFun, Is1)
    end.

%% move_allocates(Is0) -> Is
%%  Move allocates upwards in the instruction stream, in the hope of
%%  getting more possibilities for optimizing away moves later.

move_allocates(Is) ->
    move_allocates_1(reverse(Is), []).

move_allocates_1([{set,[],[],{alloc,_,_}=Alloc}|Is0], Acc0) ->
    {Is,Acc} = move_allocates_2(Alloc, Is0, Acc0),
    move_allocates_1(Is, Acc);
move_allocates_1([I|Is], Acc) ->
    move_allocates_1(Is, [I|Acc]);
move_allocates_1([], Is) -> Is.

move_allocates_2({alloc,Live,Info}, [{set,[],[],{alloc,Live0,Info0}}|Is], Acc) ->
    Live = Live0,				% Assertion.
    Alloc = {alloc,Live,combine_alloc(Info0, Info)},
    move_allocates_2(Alloc, Is, Acc);
move_allocates_2({alloc,Live,Info}=Alloc0, [I|Is]=Is0, Acc) ->
    case alloc_may_pass(I) of
	false ->
	    {Is0,[{set,[],[],Alloc0}|Acc]};
	true ->
	    Alloc = {alloc,alloc_live_regs(I, Live),Info},
	    move_allocates_2(Alloc, Is, [I|Acc])
    end;
move_allocates_2(Alloc, [], Acc) ->
    {[],[{set,[],[],Alloc}|Acc]}.

alloc_may_pass({set,_,_,{alloc,_,_}}) -> false;
alloc_may_pass({set,_,_,{set_tuple_element,_}}) -> false;
alloc_may_pass({set,_,_,put_list}) -> false;
alloc_may_pass({set,_,_,{put_tuple,_}}) -> false;
alloc_may_pass({set,_,_,put}) -> false;
alloc_may_pass({set,_,_,{put_string,_,_}}) -> false;
alloc_may_pass({set,_,_,_}) -> true.
    
combine_alloc({_,Ns,Nh1,Init}, {_,nostack,Nh2,[]}) ->
    {zero,Ns,Nh1+Nh2,Init}.

merge_blocks([{set,[],[],{allocate,R,{Attr,Ns,Nh1,Init}}}|B1],
	     [{set,[],[],{allocate,_,{_,nostack,Nh2,[]}}}|B2]) ->
    Alloc = {set,[],[],{allocate,R,{Attr,Ns,Nh1+Nh2,Init}}},
    [Alloc|merge_blocks(B1, B2)];
merge_blocks(B1, B2) -> merge_blocks_1(B1++[{set,[],[],stop_here}|B2]).

merge_blocks_1([{set,[],_,stop_here}|Is]) -> Is;
merge_blocks_1([{set,[D],_,move}=I|Is]) ->
    case is_killed(D, Is) of
	true -> merge_blocks_1(Is);
	false -> [I|merge_blocks_1(Is)]
    end;
merge_blocks_1([I|Is]) -> [I|merge_blocks_1(Is)].

%% opt([Instruction]) -> [Instruction]
%%  Optimize the instruction stream inside a basic block.

opt([{set,[Dst],As,{bif,Bif,Fail}}=I1,
     {set,[Dst],[Dst],{bif,'not',Fail}}=I2|Is]) ->
    %% Get rid of the 'not' if the operation can be inverted.
    case inverse_comp_op(Bif) of
 	none -> [I1,I2|opt(Is)];
 	RevBif -> [{set,[Dst],As,{bif,RevBif,Fail}}|opt(Is)]
    end;
opt([{set,[X],[X],move}|Is]) -> opt(Is);
opt([{set,[D1],[{integer,Idx1},Reg],{bif,element,{f,0}}}=I1,
     {set,[D2],[{integer,Idx2},Reg],{bif,element,{f,0}}}=I2|Is])
  when Idx1 < Idx2, D1 =/= D2, D1 =/= Reg, D2 =/= Reg ->
    opt([I2,I1|Is]);
opt([{set,Ds0,Ss,Op}|Is0]) ->	
    {Ds,Is} = opt_moves(Ds0, Is0),
    [{set,Ds,Ss,Op}|opt(Is)];
opt([I|Is]) -> [I|opt(Is)];
opt([]) -> [].

%% opt_moves([Dest], [Instruction], SafeRegs) -> {[Dest],[Instruction]}
%%  For each Dest, does the optimization described in opt_move/2.

opt_moves([], Is0) -> {[],Is0};
opt_moves([D0]=Ds, Is0) ->
    case opt_move(D0, Is0) of
	not_possible -> {Ds,Is0};
	{D1,Is} -> {[D1],Is}
    end;
opt_moves([X0,Y0], Is0) ->
    {X,Is2} = case opt_move(X0, Is0) of
		  not_possible -> {X0,Is0};
		  {Y0,_} -> {X0,Is0};
		  {X1,Is1} -> {X1,Is1}
	      end,
    case opt_move(Y0, Is2) of
	not_possible -> {[X,Y0],Is2};
	{X,_} -> {[X,Y0],Is2};
	{Y,Is} -> {[X,Y],Is}
    end.

%% opt_move(Dest, [Instruction]) -> {UpdatedDest,[Instruction]} | not_possible
%%  If there is a {move,Dest,FinalDest} instruction
%%  in the instruction stream, remove the move instruction
%%  and let FinalDest be the destination.
%%
%%  For this optimization to be safe, we must be sure that
%%  Dest will not be referenced in any other by other instructions
%%  in the rest of the instruction stream. Not even the indirect
%%  reference by an instruction that may allocate (such as
%%  test_heap/2 or a GC Bif) is allowed.

opt_move(Dest, Is) ->
    opt_move_1(Dest, Is, ?MAXREG, []).

opt_move_1(R, [{set,_,_,{alloc,Live,_}}|_]=Is, SafeRegs, Acc) when Live < SafeRegs ->
    %% Downgrade number of safe regs and rescan the instruction, as it most probably
    %% is a gc_bif instruction.
    opt_move_1(R, Is, Live, Acc);
opt_move_1(R, [{set,[{x,X}=D],[R],move}|Is], SafeRegs, Acc) ->
    case X < SafeRegs andalso is_killed(R, Is) of
	true -> opt_move_2(D, Acc, Is);
	false -> not_possible
    end;
opt_move_1(R, [{set,[D],[R],move}|Is], _SafeRegs, Acc) ->
    case is_killed(R, Is) of
	true -> opt_move_2(D, Acc, Is);
	false -> not_possible
    end;
opt_move_1(R, [I|Is], SafeRegs, Acc) ->
    case is_transparent(R, I) of
	false -> not_possible;
	true -> opt_move_1(R, Is, SafeRegs, [I|Acc])
    end;
opt_move_1(_, [], _, _) -> not_possible.

%% Reverse the instructions, while checking that there are no instructions that
%% would interfere with using the new destination register chosen.

opt_move_2(D, [I|Is], Acc) ->
    case is_transparent(D, I) of
	false -> not_possible;
	true -> opt_move_2(D, Is, [I|Acc])
    end;
opt_move_2(D, [], Acc) -> {D,Acc}.

%% is_transparent(Register, Instruction) -> true | false
%%  Returns true if Instruction does not in any way references Register
%%  (even indirectly by an allocation instruction).
%%  Returns false if Instruction does reference Register, or we are
%%  not sure.

is_transparent({x,X}, {set,_,_,{alloc,Live,_}}) ->
    X >= Live;
is_transparent(R, {set,Ds,Ss,_Op}) ->
    case member(R, Ds) of
	true -> false;
	false -> not member(R, Ss)
    end;
is_transparent(_, _) -> false.

%% is_killed(Register, [Instruction]) -> true|false
%%  Determine whether a register is killed by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.

is_killed({x,N}=R, [{block,Blk}|Is]) ->
    case is_killed(R, Blk) of
	true -> true;
	false ->
	    %% Before looking beyond the block, we must be
	    %% sure that the register is not referenced by
	    %% any allocate instruction in the block.
	    case all(fun({set,_,_,{alloc,Live,_}}) when N < Live -> false;
			(_) -> true
		     end, Blk) of
		true -> is_killed(R, Is);
		false -> false
	    end
    end;
is_killed({x,X}, [{set,_,_,{alloc,Live,_}}|_]) ->
    %% Note: To be safe here, we must return either true or false,
    %% not looking further at the instructions beyond the allocate
    %% instruction. 
    X >= Live;
is_killed(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> false;
	false ->
	    case member(R, Ds) of
		true -> true;
		false -> is_killed(R, Is)
	    end
    end;
is_killed(R, [{block,Blk}|Is]) ->
    case is_killed(R, Blk) of
	true -> true;
	false -> is_killed(R, Is)
    end;
is_killed(R, [{case_end,Used}|_]) -> R =/= Used;
is_killed(R, [{badmatch,Used}|_]) -> R =/= Used;
is_killed(_, [if_end|_]) -> true;
is_killed(R, [{func_info,_,_,Ar}|_]) ->
    case R of
	{x,X} when X < Ar -> false;
	_ -> true
    end;
is_killed(R, [{kill,R}|_]) -> true;
is_killed(R, [{kill,_}|Is]) -> is_killed(R, Is);
is_killed(R, [{bs_init2,_,_,_,_,_,Dst}|Is]) ->
    if
	R =:= Dst -> true;
	true -> is_killed(R, Is)
    end;
is_killed(R, [{bs_put_string,_,_}|Is]) -> is_killed(R, Is);
is_killed({x,R}, [{'%live',Live}|_]) when R >= Live -> true;
is_killed({x,R}, [{'%live',_}|Is]) -> is_killed(R, Is);
is_killed({x,R}, [{call,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_last,Live,_,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_only,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_ext,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_ext_last,Live,_,_}|_]) when R >= Live -> true;
is_killed({x,R}, [{call_ext_only,Live,_}|_]) when R >= Live -> true;
is_killed({x,R}, [return|_]) when R > 0 -> true;
is_killed(_, _) -> false.

%% is_not_used(Register, [Instruction]) -> true|false
%%  Determine whether a register is used by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced directly, but it may be referenced by an allocate
%%  instruction (meaning that it is NOT allowed to contain garbage).

is_not_used(R, [{block,Blk}|Is]) ->
    case is_not_used(R, Blk) of
	true -> true;
	false -> is_not_used(R, Is)
    end;
is_not_used({x,R}=Reg, [{allocate,Live,_}|Is]) ->
    if
	R >= Live -> true;
	true -> is_not_used(Reg, Is)
    end;
is_not_used(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> false;
	false ->
	    case member(R, Ds) of
		true -> true;
		false -> is_not_used(R, Is)
	    end
    end;
is_not_used(R, Is) -> is_killed(R, Is).

%% opt_alloc(Instructions) -> Instructions'
%%  Optimises all allocate instructions.

opt_alloc([{set,[],[],{alloc,R,{_,Ns,Nh,[]}}}|Is]) ->
    [{set,[],[],opt_alloc(Is, Ns, Nh, R)}|opt(Is)];
opt_alloc([I|Is]) -> [I|opt_alloc(Is)];
opt_alloc([]) -> [].
	
%% opt_alloc(Instructions, FrameSize, HeapNeed, LivingRegs) -> [Instr]
%%  Generates the optimal sequence of instructions for
%%  allocating and initalizing the stack frame and needed heap.

opt_alloc(_Is, nostack, Nh, LivingRegs) ->
    {alloc,LivingRegs,{nozero,nostack,Nh,[]}};
opt_alloc(Is, Ns, Nh, LivingRegs) ->
    InitRegs = init_yreg(Is, 0),
    case count_ones(InitRegs) of
	N when N*2 > Ns ->
	    {alloc,LivingRegs,{nozero,Ns,Nh,gen_init(Ns, InitRegs)}};
	_ ->
	    {alloc,LivingRegs,{zero,Ns,Nh,[]}}
    end.

gen_init(Fs, Regs) -> gen_init(Fs, Regs, 0, []).

gen_init(SameFs, _Regs, SameFs, Acc) -> reverse(Acc);
gen_init(Fs, Regs, Y, Acc) when Regs band 1 == 0 ->
    gen_init(Fs, Regs bsr 1, Y+1, [{init,{y,Y}}|Acc]);
gen_init(Fs, Regs, Y, Acc) ->
    gen_init(Fs, Regs bsr 1, Y+1, Acc).

%% init_yreg(Instructions, RegSet) -> RegSetInitialized
%%  Calculate the set of initialized y registers.

init_yreg([{set,_,_,{bif,_,_}}|_], Reg) -> Reg;
init_yreg([{set,_,_,{alloc,_,{gc_bif,_,_}}}|_], Reg) -> Reg;
init_yreg([{set,Ds,_,_}|Is], Reg) -> init_yreg(Is, add_yregs(Ds, Reg));
init_yreg(_Is, Reg) -> Reg.

add_yregs(Ys, Reg) -> foldl(fun(Y, R0) -> add_yreg(Y, R0) end, Reg, Ys).
    
add_yreg({y,Y}, Reg) -> Reg bor (1 bsl Y);
add_yreg(_, Reg)     -> Reg.

count_ones(Bits) -> count_ones(Bits, 0).
count_ones(0, Acc) -> Acc;
count_ones(Bits, Acc) ->
    count_ones(Bits bsr 1, Acc + (Bits band 1)).

%% live_at_entry(Is) -> NumberOfRegisters
%%  Calculate the number of register live at the entry to the code
%%  sequence.

live_at_entry([{set,_,_,{alloc,R,_}}|_]) -> R;
live_at_entry(Is0) ->
    case reverse(Is0) of
	[{'%live',Regs}|Is] -> live_at_entry_1(Is, (1 bsl Regs)-1);
	_ -> unknown
    end.

live_at_entry_1([{set,[],[],{alloc,_,_}}|Is], Rset) ->
    live_at_entry_1(Is, Rset);
live_at_entry_1([{set,Ds,Ss,_}|Is], Rset0) ->
    Rset = x_live(Ss, x_dead(Ds, Rset0)),
    live_at_entry_1(Is, Rset);
live_at_entry_1([], Rset) -> live_regs_1(0, Rset).

%% Calculate the new number of live registers when we move an allocate
%% instruction upwards, passing a 'set' instruction.

alloc_live_regs({set,Ds,Ss,_}, Regs0) ->
    Rset = x_live(Ss, x_dead(Ds, (1 bsl Regs0)-1)),
    live_regs_1(0, Rset).

live_regs_1(N, 0) -> N;
live_regs_1(N, Regs) -> live_regs_1(N+1, Regs bsr 1).

x_dead([{x,N}|Rs], Regs) -> x_dead(Rs, Regs band (bnot (1 bsl N)));
x_dead([_|Rs], Regs) -> x_dead(Rs, Regs);
x_dead([], Regs) -> Regs.

x_live([{x,N}|Rs], Regs) -> x_live(Rs, Regs bor (1 bsl N));
x_live([_|Rs], Regs) -> x_live(Rs, Regs);
x_live([], Regs) -> Regs.

%% inverse_comp_op(Op) -> none|RevOp

inverse_comp_op('=:=') -> '=/=';
inverse_comp_op('=/=') -> '=:=';
inverse_comp_op('==') -> '/=';
inverse_comp_op('/=') -> '==';
inverse_comp_op('>') -> '=<';
inverse_comp_op('<') -> '>=';
inverse_comp_op('>=') -> '<';
inverse_comp_op('=<') -> '>';
inverse_comp_op(_) -> none.

%%%
%%% Evaluation of constant bit fields.
%%%

is_bs_put({bs_put_integer,_,_,_,_,_}) -> true;
is_bs_put({bs_put_float,_,_,_,_,_}) -> true;
is_bs_put(_) -> false.

collect_bs_puts(Is) ->
    collect_bs_puts_1(Is, []).
    
collect_bs_puts_1([I|Is]=Is0, Acc) ->
    case is_bs_put(I) of
	false -> {reverse(Acc),Is0};
	true -> collect_bs_puts_1(Is, [I|Acc])
    end;
collect_bs_puts_1([], Acc) -> {reverse(Acc),[]}.
    
opt_bs_puts(Is) ->
    opt_bs_1(Is, []).

opt_bs_1([{bs_put_float,Fail,{integer,Sz},1,Flags0,Src}=I0|Is], Acc) ->
    try eval_put_float(Src, Sz, Flags0) of
	<<Int:Sz>> ->
	    Flags = force_big(Flags0),
	    I = {bs_put_integer,Fail,{integer,Sz},1,Flags,{integer,Int}},
	    opt_bs_1([I|Is], Acc)
    catch
	error:_ ->
	    opt_bs_1(Is, [I0|Acc])
    end;
opt_bs_1([{bs_put_integer,_,{integer,8},1,_,{integer,_}}|_]=IsAll, Acc0) ->
    {Is,Acc} = bs_collect_string(IsAll, Acc0),
    opt_bs_1(Is, Acc);
opt_bs_1([{bs_put_integer,Fail,{integer,Sz},1,F,{integer,N}}=I|Is0], Acc) when Sz > 8 ->
    case field_endian(F) of
	big ->
	    %% We can do this optimization for any field size without risk
	    %% for code explosion.
	    case bs_split_int(N, Sz, Fail, Is0) of
		no_split -> opt_bs_1(Is0, [I|Acc]);
		Is -> opt_bs_1(Is, Acc)
	    end;
	little when Sz < 128 ->
	    %% We only try to optimize relatively small fields, to avoid
	    %% an explosion in code size.
	    try <<N:Sz/little>> of
		<<Int:Sz>> ->
		    Flags = force_big(F),
		    Is = [{bs_put_integer,Fail,{integer,Sz},1,
			   Flags,{integer,Int}}|Is0],
		    opt_bs_1(Is, Acc)
	    catch
		error:_ ->
		    opt_bs_1(Is0, [I|Acc])
	    end;
	_ -> 					%native or too wide little field
	    opt_bs_1(Is0, [I|Acc])
    end;
opt_bs_1([{Op,Fail,{integer,Sz},U,F,Src}|Is], Acc) when U > 1 ->
    opt_bs_1([{Op,Fail,{integer,U*Sz},1,F,Src}|Is], Acc);
opt_bs_1([I|Is], Acc) ->
    opt_bs_1(Is, [I|Acc]);
opt_bs_1([], Acc) -> reverse(Acc).

eval_put_float(Src, Sz, Flags) ->
    Val = value(Src),
    case field_endian(Flags) of
	little -> <<Val:Sz/little-float-unit:1>>;
	big -> <<Val:Sz/big-float-unit:1>>
        %% native intentionally not handled here - we can't optimize it.
    end.

value({integer,I}) -> I;
value({float,F}) -> F;
value({atom,A}) -> A.

bs_collect_string(Is, [{bs_put_string,Len,{string,Str}}|Acc]) ->
    bs_coll_str_1(Is, Len, reverse(Str), Acc);
bs_collect_string(Is, Acc) ->
    bs_coll_str_1(Is, 0, [], Acc).
    
bs_coll_str_1([{bs_put_integer,_,{integer,Sz},U,_,{integer,V}}|Is],
	      Len, StrAcc, IsAcc) when U*Sz =:= 8 ->
    Byte = V band 16#FF,
    bs_coll_str_1(Is, Len+1, [Byte|StrAcc], IsAcc);
bs_coll_str_1(Is, Len, StrAcc, IsAcc) ->
    {Is,[{bs_put_string,Len,{string,reverse(StrAcc)}}|IsAcc]}.

field_endian({field_flags,F}) -> field_endian_1(F).

field_endian_1([big=E|_]) -> E;
field_endian_1([little=E|_]) -> E;
field_endian_1([native=E|_]) -> E;
field_endian_1([_|Fs]) -> field_endian_1(Fs).

force_big({field_flags,F}) ->
    {field_flags,force_big_1(F)}.

force_big_1([big|_]=Fs) -> Fs;
force_big_1([little|Fs]) -> [big|Fs];
force_big_1([F|Fs]) -> [F|force_big_1(Fs)].

bs_split_int(0, Sz, _, _) when Sz > 64 ->
    %% We don't want to split in this case because the
    %% string will consist of only zeroes.
    no_split;
bs_split_int(N, Sz, Fail, Acc) ->
    FirstByteSz = case Sz rem 8 of
		      0 -> 8;
		      Rem -> Rem
		  end,
    bs_split_int_1(N, FirstByteSz, Sz, Fail, Acc).

bs_split_int_1(0, _, Sz, Fail, Acc) when Sz > 64 ->
    I = {bs_put_integer,Fail,{integer,Sz},1,{field_flags,[big]},{integer,0}},
    [I|Acc];
bs_split_int_1(N, ByteSz, Sz, Fail, Acc) when Sz > 0 ->
    Mask = (1 bsl ByteSz) - 1,
    I = {bs_put_integer,Fail,{integer,ByteSz},1,
	 {field_flags,[big]},{integer,N band Mask}},
    bs_split_int_1(N bsr ByteSz, 8, Sz-ByteSz, Fail, [I|Acc]);
bs_split_int_1(_, _, _, _, Acc) -> Acc.


%%%
%%% Optimization of new bit syntax matching: get rid
%%% of redundant bs_restore2/2 instructions across select_val
%%% instructions.
%%%

bsm_opt(Is0) ->
    D0 = bsm_scan(Is0, []),
    D = gb_trees:from_orddict(ordsets:from_list(D0)),
    Is1 = bsm_opt_1(Is0, D, []),
    Is = beam_clean:bs_clean_saves(Is1),
    bsm_opt_2(Is, []).

bsm_scan([{bs_save2,_,Save},{test,is_integer,{f,F0},[_]},
	  {select_val,_,{f,F1},{list,Lbls}}|Is], Acc0) ->
    Acc = [{F,Save} || {f,F} <- Lbls] ++ [{F0,Save},{F1,Save}|Acc0],
    bsm_scan(Is, Acc);
bsm_scan([{bs_save2,_,Save},{test,bs_test_tail2,{f,F},[_,_]}|Is], Acc) ->
    bsm_scan(Is, [{F,Save}|Acc]);
bsm_scan([_|Is], Acc) ->
    bsm_scan(Is, Acc);
bsm_scan([], Acc) -> Acc.

bsm_opt_1([{label,L}=I,{bs_restore2,_,Save}=R|Is], D, [PrevI|_]=Acc) ->
    %% The call to beam_jump:is_unreachable_after/1 is probably
    %% over-conservative. We want to be absolutely sure that the label
    %% L cannot be reached in any other way (the way the code is generated
    %% should ensure that anyway, but better safe than sorry).
    case {beam_jump:is_unreachable_after(PrevI),gb_trees:lookup(L, D)} of
	{true,{value,Save}} ->
	    bsm_opt_1(Is, D, [I|Acc]);
	_ ->
	    bsm_opt_1(Is, D, [R,I|Acc])
    end;
bsm_opt_1([I|Is], D, Acc) ->
    bsm_opt_1(Is, D, [I|Acc]);
bsm_opt_1([], _, Acc) -> reverse(Acc).

bsm_opt_2([{test,bs_test_tail2,F,[Ctx,Bits]}|Is],
	  [{test,bs_skip_bits2,F,[Ctx,{integer,I},Unit,_Flags]}|Acc]) ->
    bsm_opt_2(Is, [{test,bs_test_tail2,F,[Ctx,Bits+I*Unit]}|Acc]);
bsm_opt_2([{test,bs_skip_bits2,F,[Ctx,{integer,I1},Unit1,_]}|Is],
	  [{test,bs_skip_bits2,F,[Ctx,{integer,I2},Unit2,Flags]}|Acc]) ->
    bsm_opt_2(Is, [{test,bs_skip_bits2,F,
		    [Ctx,{integer,I1*Unit1+I2*Unit2},1,Flags]}|Acc]);
bsm_opt_2([{test,bs_test_tail2,_,[Ctx,0]}|Is],
	  [{test,bs_skip_bits2,_,[Ctx,{atom,all},8,{field_flags,Fl}]}|Acc]=Acc0) ->
    %% The bs_test_tail/2 instruction is not needed here - it can't possibly 
    %% fail after a bs_skip_bits/4 instruction.
    case member(aligned, Fl) of
	false ->
	    bsm_opt_2(Is, Acc0);
	true ->
	    %% Since the bs_skip_bits/4 instruction is aligned, it can't
	    %% fail and we can remove that instruction too.
	    bsm_opt_2(Is, Acc)
    end;
bsm_opt_2([{test,bs_test_tail2,_,[Ctx,0]}|Is],
	  [{test,bs_get_binary2,_,[Ctx,_,{atom,all},8,_,_]}|_]=Acc) ->
    %% The bs_test_tail/2 instruction is not needed here - it can't fail.
    bsm_opt_2(Is, Acc);
bsm_opt_2([I|Is], Acc) ->
    bsm_opt_2(Is, [I|Acc]);
bsm_opt_2([], Acc) -> reverse(Acc).

