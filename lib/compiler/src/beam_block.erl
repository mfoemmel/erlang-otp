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
%% Purpose : Partions assembly instructions into basic blocks and
%% optimizes them.

-module(beam_block).

-export([module/2]).
-import(lists, [map/2,reverse/1,foldl/3,member/2]).

module({Mod,Exp,Attr,Fs,Lc}, Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Asm}) ->
    {function,Name,Arity,CLabel,block(Asm, [], [])}.

block([{allocate_zero,Ns,R},{test_heap,Nh,R}|Is], Block, All) ->
    block(Is, [{allocate,R,{no_opt,Ns,Nh,[]}}|Block], All);
block([{bif,size,Fail,[Tuple],Tmp},
       {test,is_eq,Fail,Tmp,{integer,Size}},
       {bif,element,Fail,[{integer,1},Tuple],Tmp},
       {test,is_eq,Fail,Tmp,RecordTag}|Is], Block, All) ->
    %% XXX This kind of optimisation should be done at a higher level.
    %% The record guard test should be moved into the head before
    %% pattern matching compilation.
    block([{test,is_tuple,Fail,Tuple},
	   {test,test_arity,Fail,Tuple,Size},
	   {get_tuple_element,Tuple,0,Tmp},
	   {test,is_eq,Fail,Tmp,RecordTag}|Is], Block, All);
block([{loop_rec,{f,Fail},{x,0}},{loop_rec_end,Lbl},{label,Fail}|Is], Block, All) ->
    block(Is, Block, All);
block([I|Is], Block, All) ->
    case collect(I) of
	false -> opt_block(Block, I, Is, All);
	Ti when tuple(Ti) -> block(Is, [Ti|Block], All)
    end;
block([], [], All) ->
    reverse(All);
block([], Block, All) ->
    block([], [], [opt_block(reverse(Block), All)]).

collect({allocate_zero,N,R}) -> {allocate,R,{zero,N,0,[]}};
collect({test_heap,N,R})     -> {allocate,R,{nozero,nostack,N,[]}};
collect({bif,N,nofail,As,D}) -> {set,[D],As,{bif,N}};
collect({bif,N,F,As,D})      -> {set,[D],As,{bif,N,F}};
collect({move,S,D})          -> {set,[D],[S],move};
collect({put_list,S1,S2,D})  -> {set,[D],[S1,S2],put_list};
collect({put_tuple,A,D})     -> {set,[D],[],{put_tuple,A}};
collect({put,S})             -> {set,[],[S],put};
collect({put_string,L,S,D})  -> {set,[D],[],{put_string,L,S}};
collect({get_tuple_element,S,I,D}) -> {set,[D],[S],{get_tuple_element,I}};
collect({get_list,S,D1,D2})  -> {set,[D1,D2],[S],get_list};
collect(remove_message)      -> {set,[],[],remove_message};
collect({'catch',R,L})       -> {set,[R],[],{'catch',L}};
collect({'%live',R})  -> {'%live',R};
collect(Other) -> false.

opt_block([], I, Is, All) -> block(Is, [], [I|All]);
opt_block([{'%live',R}], I, Is, All) -> block(Is, [], [I|All]);
opt_block(Block, I, Is, All) ->    
    block(Is, [], [I|opt_block(reverse(Block), All)]).

opt_block(Is0, Tail) ->
    %% We explicitly move any allocate instruction upwards before optimising
    %% moves, to avoid any potential problems with the calculation of living
    %% registers.
    Is1 = find_fixpoint(fun move_allocates/1, Is0),
    Is2 = find_fixpoint(fun opt/1, Is1),
    Is3 = opt_alloc(Is2),
    split_at_catches(Is3, [], Tail).

%% We must split the basic block at each catch, because the catches
%% must be visible for the jump optimizer.

split_at_catches([{set,[R],[],{'catch',L}}|Is], [], Acc) ->
    split_at_catches(Is, [], [{'catch',R,L}|Acc]);
split_at_catches([{set,[R],[],{'catch',L}}|Is], Bl, Acc) ->
    split_at_catches(Is, [], [{'catch',R,L},{block,reverse(Bl)}|Acc]);
split_at_catches([I|Is], Bl, Acc) ->
    split_at_catches(Is, [I|Bl], Acc);
split_at_catches([], [], Acc) ->
    Acc;
split_at_catches([], Bl, Acc) ->
    [{block,reverse(Bl)}|Acc].

find_fixpoint(OptFun, Is0) ->
    case OptFun(Is0) of
	Is0 -> Is0;
	Is1 -> find_fixpoint(OptFun, Is1)
    end.

move_allocates([{set,Ds,Ss,Op}=Set,{allocate,R,Alloc}|Is]) when integer(R) ->
    [{allocate,live_regs(Ds, Ss, R),Alloc},Set|Is];
move_allocates([{allocate,R1,Alloc1},{allocate,R2,Alloc2}|Is]) ->
    R1 = R2,					% Assertion.
    move_allocates([{allocate,R1,combine_alloc(Alloc1, Alloc2)}|Is]);
move_allocates([I|Is]) ->
    [I|move_allocates(Is)];
move_allocates([]) -> [].

combine_alloc({_,Ns,Nh1,Init}, {_,nostack,Nh2,[]}) ->
    {zero,Ns,Nh1+Nh2,Init}.

opt([{set,[X],[X],move}|Is]) -> opt(Is);
opt([{set,[D1],[{integer,Idx1},Reg],{bif,element,{f,0}}}=I1,
     {set,[D2],[{integer,Idx2},Reg],{bif,element,{f,0}}}=I2|Is])
  when Idx1 < Idx2, D1 =/= D2, D1 =/= Reg, D2 =/= Reg ->
    opt([I2,I1|Is]);
opt([{set,Ds0,Ss,Op}|Is0]) ->	
    {Ds1,Is1} =  opt_moves(Ds0, Is0),
    [{set,Ds1,Ss,Op}|opt(Is1)];
opt([I|Is]) -> [I|opt(Is)];
opt([]) -> [].

opt_moves([], Is0) -> {[],Is0};
opt_moves([D0], Is0) ->
    {D1,Is1} = opt_move(D0, Is0),
    {[D1],Is1};
opt_moves([X0,Y0]=Ds, Is0) ->
    {X1,Is1} = opt_move(X0, Is0),
    case opt_move(Y0, Is1) of
	{Y1,Is2} when X1 =/= Y1 -> {[X1,Y1],Is2};
	Other when X1 =/= Y0 -> {[X1,Y0],Is1};
	Other -> {Ds,Is0}
    end.

opt_move(R, [{set,[D],[R],move}|Is]) ->
    case is_killed(R, Is) of
	true -> {D,Is};
	false -> {R,[{set,[D],[R],move}|Is]}
    end;
opt_move(R, [I|Is0]) ->
    case is_transparent(R, I) of
	true ->
	    {D,Is1} = opt_move(R, Is0),
	    case is_transparent(D, I) of
		true ->  {D,[I|Is1]};
		false -> {R,[I|Is0]}
	    end;
	false -> {R,[I|Is0]}
    end;
opt_move(R, []) -> {R,[]}.


is_transparent(R, {set,Ds,Ss,Op}) ->
    case member(R, Ds) of
	true -> false;
	false -> not member(R, Ss)
    end;
is_transparent(R, I) -> false.

is_killed(R, [{set,Ds,Ss,Op}|Is]) ->
    case member(R, Ss) of
	true -> false;
	false -> case member(R, Ds) of
		     true -> true;
		     false -> is_killed(R, Is)
		 end
    end;
is_killed({x,R}, [{'%live',Live}|Is]) when R >= Live -> true;
is_killed(R, Is) -> false.


%% opt_alloc(Instructions) -> Instructions'
%%  Optimises all allocate instructions.

opt_alloc([{allocate,R,{Any,Ns,Nh,[]}}|Is]) ->
    [opt_alloc(Is, Ns, Nh, R)|opt(Is)];
opt_alloc([I|Is]) -> [I|opt_alloc(Is)];
opt_alloc([]) -> [].
	
%% opt_alloc(Instructions, FrameSize, HeapNeed, LivingRegs) -> [Instr]
%%  Generates the optimal sequence of instructions for
%%  allocating and initalizing the stack frame and needed heap.

opt_alloc(Is, nostack, Nh, LivingRegs) ->
    {allocate,LivingRegs,{nozero,nostack,Nh,[]}};
opt_alloc(Is, Ns, Nh, LivingRegs) ->
    InitRegs = init_yreg(Is, 0),
    case count_ones(InitRegs) of
	N when N*2 > Ns ->
	    {allocate,LivingRegs,{nozero,Ns,Nh,gen_init(Ns, InitRegs)}};
	_ ->
	    {allocate,LivingRegs,{zero,Ns,Nh,[]}}
    end.

gen_init(Fs, Regs) -> gen_init(Fs, Regs, 0, []).

gen_init(Fs, Regs, Fs, Acc) -> reverse(Acc);
gen_init(Fs, Regs, Y, Acc) when Regs band 1 == 0 ->
    gen_init(Fs, Regs bsr 1, Y+1, [{init, {y,Y}}|Acc]);
gen_init(Fs, Regs, Y, Acc) ->
    gen_init(Fs, Regs bsr 1, Y+1, Acc).

%% init_yreg(Instructions, RegSet) -> RegSetInitialized

init_yreg([{set,Ds,_,_}|Is], Reg) ->
    init_yreg(Is, add_yregs(Ds, Reg));
init_yreg(Is, Reg) -> Reg.

add_yregs(Ys, Reg) -> foldl(fun(Y, R0) -> add_yreg(Y, R0) end, Reg, Ys).
    
add_yreg({y,Y}, Reg) -> Reg bor (1 bsl Y);
add_yreg(_, Reg)     -> Reg.

count_ones(Bits) -> count_ones(Bits, 0).
count_ones(0, Acc) -> Acc;
count_ones(Bits, Acc) ->
    count_ones(Bits bsr 1, Acc + (Bits band 1)).

%% Calculate the new number of live registers when we move an allocate
%% instruction upwards, passing a 'set' instruction.

live_regs(Ds, Ss, Regs0) ->
    Rset = x_live(Ss, x_dead(Ds, (1 bsl Regs0)-1)),
    live_regs(0, Rset).

live_regs(N, 0) -> N;
live_regs(N, Regs) -> live_regs(N+1, Regs bsr 1).

x_dead([{x,N}|Rs], Regs) -> x_dead(Rs, Regs band (bnot (1 bsl N)));
x_dead([Other|Rs], Regs) -> x_dead(Rs, Regs);
x_dead([], Regs) -> Regs.

x_live([{x,N}|Rs], Regs) -> x_live(Rs, Regs bor (1 bsl N));
x_live([Other|Rs], Regs) -> x_live(Rs, Regs);
x_live([], Regs) -> Regs.
