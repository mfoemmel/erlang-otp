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
-export([merge_blocks/2]).			%Used by beam_jump.
-import(lists, [map/2,mapfoldr/3,reverse/1,reverse/2,foldl/3,member/2,sort/1]).
-define(MAXREG, 1024).

module({Mod,Exp,Attr,Fs,Lc}, Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Asm0}) ->
    Asm1 = block(Asm0, [], []),
    Asm = opt_kill(Asm1),
    {function,Name,Arity,CLabel,Asm}.

block([{allocate_zero,Ns,R},{test_heap,Nh,R}|Is], Block, All) ->
    block(Is, [{allocate,R,{no_opt,Ns,Nh,[]}}|Block], All);
block([{bif,size,Fail,[Tuple],Tmp},
       {test,is_eq,Fail,[Tmp,{integer,Size}]},
       {bif,element,Fail,[{integer,1},Tuple],Tmp},
       {test,is_eq,Fail,[Tmp,RecordTag]}|Is], Block, All) ->
    %% XXX This kind of optimisation should be done at a higher level.
    %% The record guard test should be moved into the head before
    %% pattern matching compilation.
    block([{test,is_tuple,Fail,[Tuple]},
	   {test,test_arity,Fail,[Tuple,Size]},
	   {get_tuple_element,Tuple,0,Tmp},
	   {test,is_eq,Fail,[Tmp,RecordTag]}|Is], Block, All);
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
collect({set_tuple_element,S,D,I}) -> {set,[],[S,D],{set_tuple_element,I}};
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
    %% moves, to avoid any potential problems with the calculation of live
    %% registers.
    Is1 = find_fixpoint(fun move_allocates/1, Is0),
    Is2 = find_fixpoint(fun opt/1, Is1),
    Is3 = opt_alloc(Is2),
    Is = share_floats(Is3),
    split_block(Is, [], Tail).

%% We must split the basic block when we encounter instructions with labels,
%% such as catches and BIFs. All labels must be visible for later passes.

split_block([{set,[R],As,{bif,N,{f,Lbl}}}|Is], Bl, Acc) when Lbl =/= 0 ->
    split_block(Is, [], [{bif,N,{f,Lbl},As,R}|make_block(Bl, Acc)]);
split_block([{set,[R],[],{'catch',L}}|Is], Bl, Acc) ->
    split_block(Is, [], [{'catch',R,L}|make_block(Bl, Acc)]);
split_block([I|Is], Bl, Acc) ->
    split_block(Is, [I|Bl], Acc);
split_block([], Bl, Acc) -> make_block(Bl, Acc).

make_block([], Acc) -> Acc;
make_block(Bl, Acc) -> [{block,reverse(Bl)}|Acc].
    
find_fixpoint(OptFun, Is0) ->
    case OptFun(Is0) of
	Is0 -> Is0;
	Is1 -> find_fixpoint(OptFun, Is1)
    end.

move_allocates([{set,Ds,Ss,{set_tuple_element,I}}|_]=Is) -> Is;
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

merge_blocks([{allocate,R,{Attr,Ns,Nh1,Init}}|B1],
	     [{allocate,_,{_,nostack,Nh2,[]}}|B2]) ->
    Alloc = {allocate,R,{Attr,Ns,Nh1+Nh2,Init}},
    [Alloc|opt(B1++B2)];
merge_blocks(B1, B2) -> opt(B1++B2).

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

opt_move(R, [{set,[D],[R],move}|Is]=Is0) ->
    case is_killed(R, Is) of
	true -> {D,Is};
	false -> {R,Is0}
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
is_killed({x,R}, [{'%live',Live}|Is]) -> is_killed(R, Is);
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
%%  Calculate the set of initialized y registers.

init_yreg([{set,Ds,_,{bif,_,_}}|Is], Reg) -> Reg;
init_yreg([{set,Ds,_,_}|Is], Reg) -> init_yreg(Is, add_yregs(Ds, Reg));
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

%%
%% If a floating point literal occurs more than once, move it into
%% a free register and re-use it.
%%

share_floats([{allocate,_,_}=Alloc|Is]) ->
    [Alloc|share_floats(Is)];
share_floats(Is0) ->
    All = get_floats(Is0, []),
    MoreThanOnce0 =  more_than_once(sort(All), gb_sets:empty()),
    case gb_sets:is_empty(MoreThanOnce0) of
	true -> Is0;
	false ->
	    MoreThanOnce = gb_sets:to_list(MoreThanOnce0),
	    FreeX = highest_used(Is0, -1) + 1,
	    {Regs0,_} = mapfoldr(fun(F, R) -> {{F,{x,R}},R+1} end, FreeX, MoreThanOnce),
	    Regs = gb_trees:from_orddict(Regs0),
	    Is = map(fun({set,Ds,[{float,F}],Op}=I) ->
			     case gb_trees:lookup(F, Regs) of
				 none -> I;
				 {value,R} -> {set,Ds,[R],Op}
			     end;
			(I) -> I
		     end, Is0),
	    [{set,[R],[{float,F}],move} || {F,R} <- Regs0] ++ Is
    end.

get_floats([{set,_,[{float,F}],_}|Is], Acc) ->
    get_floats(Is, [F|Acc]);
get_floats([I|Is], Acc) ->
    get_floats(Is, Acc);
get_floats([], Acc) -> Acc.

more_than_once([F,F|Fs], Set) ->
    more_than_once(Fs, gb_sets:add(F, Set));
more_than_once([F|Fs], Set) ->
    more_than_once(Fs, Set);
more_than_once([], Set) -> Set.

highest_used([{set,Ds,Ss,_}|Is], High) ->
    highest_used(Is, highest(Ds, highest(Ss, High)));
highest_used([{'%live',Live}|Is], High) when Live > High ->
    highest_used(Is, Live);
highest_used([I|Is], High) ->
    highest_used(Is, High);
highest_used([], High) -> High.

highest([{x,R}|Rs], High) when R > High ->
    highest(Rs, R);
highest([R|Rs], High) ->
    highest(Rs, High);
highest([], High) -> High.

%%
%% Remove kill/1 instructions before BIFs known not to need them.
%%

opt_kill(Is) ->
    opt_kill(Is, []).

opt_kill([{call_ext,3,{extfunc,erlang,setelement,3}}=I|Is], Acc) ->
    opt_kill(Is, I, Acc);
opt_kill([I|Is], Acc) ->
    opt_kill(Is, [I|Acc]);
opt_kill([], Acc) -> reverse(Acc).

opt_kill([{block,[{allocate,_,_}|_]}|_]=Is, I, Acc) ->
    opt_kill(Is, [I|Acc]);
opt_kill(Is, I, Acc) ->
    opt_kill(Is, I, Acc, []).

opt_kill([{block,Block},Instr|_]=Is, I, [{kill,Y}=K|Acc], Kills) ->
    case is_tail_call_or_ret(Instr) of
	true -> opt_kill(Is, I, Acc, Kills);
	false ->
	    case is_killed(Y, Block) of
		true -> opt_kill(Is, I, Acc, Kills);
		false -> opt_kill(Is, I, Acc, [K|Kills])
	    end
    end;
opt_kill([Instr|_]=Is, I, [{kill,Y}=K|Acc], Kills) ->
    case is_tail_call_or_ret(Instr) of
	true -> opt_kill(Is, I, Acc, Kills);
	false -> opt_kill(Is, I, Acc, [K|Kills])
    end;
opt_kill(Is, I, Acc, [_|_]=Kills) ->
    opt_kill(Is, [I|reverse(Kills, Acc)]);
opt_kill(Is, I, Acc, []) ->
    opt_kill(Is, [I|Acc]).

is_tail_call_or_ret({call_last,_,_,_}) -> true;
is_tail_call_or_ret({call_ext_last,_,_,_}) -> true;
is_tail_call_or_ret({deallocate,_}) -> true;
is_tail_call_or_ret(Other) -> false.
