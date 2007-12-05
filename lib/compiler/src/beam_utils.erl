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
%% Purpose : Common utilities used by several optimization passes.
%% 

-module(beam_utils).
-export([is_killed/2,is_killed_block/2,is_killed/3,is_killed_at/3,
	 is_not_used/3,
	 empty_label_index/0,index_label/3,index_labels/1,
	 code_at/2,bif_to_test/3]).

-import(lists, [member/2,sort/1]).

-record(live,
	{bl,					%Block check fun.
	 lbl,					%Label to code index.
	 res}).					%Result cache for each label.


%% is_killed(Register, [Instruction]) -> true|false
%%  Determine whether a register is killed by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.

is_killed(R, Is) ->
    is_killed(R, Is, empty_label_index()).

%% is_killed_block(Register, [Instruction]) -> true|false
%%  Determine whether a register is killed by the instruction sequence inside
%%  a block.
%%
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.

is_killed_block(R, Is) ->
    case check_killed_block(R, Is) of
	killed -> true;
	used -> false;
	transparent -> false
    end.

%% is_killed(Register, [Instruction], State) -> true|false
%%  Determine whether a register is killed by the instruction sequence.
%%  If true is returned, it means that the register will not be
%%  referenced in ANY way (not even indirectly by an allocate instruction);
%%  i.e. it is OK to enter the instruction sequence with Register
%%  containing garbage.
%%
%%  The state (constructed by index_instructions/1) is used to allow us
%%  to determine the kill state across branches.

is_killed(R, Is, D) ->
    St = #live{bl=fun check_killed_block/2,lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	killed -> true;
	used -> false;
	unknown -> false
    end.

%% is_killed_at(Reg, Lbl, State) -> true|false
%%  Determine wether Reg is killed at label Lbl.

is_killed_at(R, Lbl, D) when is_integer(Lbl) ->
    St0 = #live{bl=fun check_killed_block/2,lbl=D,res=gb_trees:empty()},
    case check_liveness_at(R, Lbl, St0) of
	{killed,_} -> true;
	{used,_} -> false;
	{unknown,_} -> false
    end.

%% is_not_used(Register, [Instruction], State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still referenced by an allocate instruction, meaning that
%%  it MUST be initialized, but that its value does not matter).
%%    The state is used to allow us to determine the usage state
%%  across branches.

is_not_used(R, Is, D) ->
    St = #live{bl=fun check_used_block/2,lbl=D,res=gb_trees:empty()},
    case check_liveness(R, Is, St) of
	killed -> true;
	used -> false;
	unknown -> false
    end.

%% index_labels(FunctionIs) -> State
%%  Index the instruction sequence so that we can quickly
%%  look up the instruction following a specific label.

index_labels(Is) ->
    index_labels_1(Is, []).

%% empty_label_index() -> State
%%  Create an empty label index.

empty_label_index() ->
    gb_trees:empty().

%% index_label(Label, [Instruction], State) -> State
%%  Add an index for a label.

index_label(Lbl, Is0, Acc) ->
    Is = lists:dropwhile(fun({label,_}) -> true;
			    (_) -> false end, Is0),
    gb_trees:insert(Lbl, Is, Acc).


%% code_at(Label, State) -> [I].
%%  Retrieve the code at the given label.

code_at(L, Ll) ->
    case gb_trees:lookup(L, Ll) of
	{value,Code} -> Code;
	none -> none
    end.

%% bif_to_test(Bif, [Op], Fail) -> {test,Test,Fail,[Op]}
%%  Convert a BIF to a test. Fail if not possible.

bif_to_test(is_atom,     [_]=Ops, Fail) -> {test,is_atom,Fail,Ops};
bif_to_test(is_boolean,  [_]=Ops, Fail) -> {test,is_boolean,Fail,Ops};
bif_to_test(is_binary,   [_]=Ops, Fail) -> {test,is_binary,Fail,Ops};
bif_to_test(is_bitstr,   [_]=Ops, Fail) -> {test,is_bitstr,Fail,Ops}; %XXX Deprecated
bif_to_test(is_bitstring,[_]=Ops, Fail) -> {test,is_bitstr,Fail,Ops};
bif_to_test(is_constant, [_]=Ops, Fail) -> {test,is_constant,Fail,Ops};
bif_to_test(is_float,    [_]=Ops, Fail) -> {test,is_float,Fail,Ops};
bif_to_test(is_function, [_]=Ops, Fail) -> {test,is_function,Fail,Ops};
bif_to_test(is_function, [_,_]=Ops, Fail) -> {test,is_function2,Fail,Ops};
bif_to_test(is_integer,  [_]=Ops, Fail) -> {test,is_integer,Fail,Ops};
bif_to_test(is_list,     [_]=Ops, Fail) -> {test,is_list,Fail,Ops};
bif_to_test(is_number,   [_]=Ops, Fail) -> {test,is_number,Fail,Ops};
bif_to_test(is_pid,      [_]=Ops, Fail) -> {test,is_pid,Fail,Ops};
bif_to_test(is_port,     [_]=Ops, Fail) -> {test,is_port,Fail,Ops};
bif_to_test(is_reference, [_]=Ops, Fail) -> {test,is_reference,Fail,Ops};
bif_to_test(is_tuple,    [_]=Ops, Fail)     -> {test,is_tuple,Fail,Ops};
bif_to_test('=<', [A,B], Fail) -> {test,is_ge,Fail,[B,A]};
bif_to_test('>', [A,B], Fail) -> {test,is_lt,Fail,[B,A]};
bif_to_test('<', [_,_]=Ops, Fail) -> {test,is_lt,Fail,Ops};
bif_to_test('>=', [_,_]=Ops, Fail) -> {test,is_ge,Fail,Ops};
bif_to_test('==', [A,[]], Fail) -> {test,is_nil,Fail,[A]};
bif_to_test('==', [_,_]=Ops, Fail) -> {test,is_eq,Fail,Ops};
bif_to_test('/=', [_,_]=Ops, Fail) -> {test,is_ne,Fail,Ops};
bif_to_test('=:=', [A,[]], Fail) -> {test,is_nil,Fail,[A]};
bif_to_test('=:=', [_,_]=Ops, Fail) -> {test,is_eq_exact,Fail,Ops};
bif_to_test('=/=', [_,_]=Ops, Fail) -> {test,is_ne_exact,Fail,Ops};
bif_to_test(is_record, [_,_,_]=Ops, Fail) -> {test,is_record,Fail,Ops}.

%%%
%%% Local functions.
%%%


%% check_liveness(Reg, [Instruction], {State,BlockCheckFun}) ->
%%                                            killed | used | unknown
%%  Finds out how Reg is used in the instruction sequence. Returns one of:
%%    killed - Reg is assigned a new value or killed by an allocation instruction
%%    used - Reg is used (or possibly referenced by an allocation instruction)
%%    unknown - not possible to determine (perhaps because of an instruction
%%              that we don't recognize)

check_liveness(R, [{set,_,_,_}=I|_], St) ->
    erlang:error(only_allowed_in_blocks, [R,I,St]);
check_liveness(R, [{block,Blk}|Is], #live{bl=BlockCheck}=St) ->
    case BlockCheck(R, Blk) of
	transparent -> check_liveness(R, Is, St);
	Other -> Other
    end;
check_liveness(R, [{label,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{test,Bs,{f,Fail},[Ctx,_,Sz,_,_,Dst]}|Is], St0)
  when Bs =:= bs_get_integer2; Bs =:= bs_get_binary2; Bs =:= bs_get_float2 ->
    case R of
	Ctx -> used;
	Sz -> used;
	Dst -> killed;
	_ ->
	    case check_liveness_at(R, Fail, St0) of
		{killed,St} -> check_liveness(R, Is, St);
		{Other,_} -> Other
	    end
    end;
check_liveness(R, [{test,_,{f,Fail},As}|Is], St0) ->
    case member(R, As) of
	true -> used;
	false ->
	    case check_liveness_at(R, Fail, St0) of
		{killed,St} -> check_liveness(R, Is, St);
		{Other,_} -> Other
	    end
    end;
check_liveness(R, [{select_val,R,_,_}|_], _) -> used;
check_liveness(R, [{select_val,_,Fail,{list,Branches}}|_], St) ->
    check_liveness_everywhere(R, [Fail|Branches], St);
check_liveness(R, [{select_tuple_arity,R,_,_}|_], _) -> used;
check_liveness(R, [{select_tuple_arity,_,Fail,{list,Branches}}|_], St) ->
    check_liveness_everywhere(R, [Fail|Branches], St);
check_liveness(R, [{jump,{f,F}}|_], St) ->
    {Res,_} = check_liveness_at(R, F, St),
    Res;
check_liveness(R, [{case_end,Used}|_], _St) -> 
    check_liveness_ret(R, Used);
check_liveness(R, [{badmatch,Used}|_], _St) ->
    check_liveness_ret(R, Used);
check_liveness(_, [if_end|_], _St) -> killed;
check_liveness(R, [{func_info,_,_,Ar}|_], _St) ->
    case R of
	{x,X} when X < Ar -> used;
	_ -> killed
    end;
check_liveness(R, [{kill,R}|_], _St) -> killed;
check_liveness(R, [{kill,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [bs_init_writable|Is], St) ->
    if
	R =:= {x,0} -> used;
	true -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_private_append,_,Bits,_,Bin,_,Dst}|Is], St) ->
    case R of
	Bits -> used;
	Bin -> used;
	Dst -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_append,_,Bits,_,_,_,Bin,_,Dst}|Is], St) ->
    case R of
	Bits -> used;
	Bin -> used;
	Dst -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_init2,_,_,_,_,_,Dst}|Is], St) ->
    if
	R =:= Dst -> killed;
	true -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_init_bits,_,_,_,_,_,Dst}|Is], St) ->
    if
	R =:= Dst -> killed;
	true -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_string,_,_}|Is], St) ->
    check_liveness(R, Is, St);
check_liveness(R, [{deallocate,_}|Is], St) ->
    case R of
	{y,_} -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [return|_], _St) ->
    check_liveness_live_ret(R, 1);
check_liveness(R, [{call_last,Live,_,_}|_], _St) ->
    check_liveness_live_ret(R, Live);
check_liveness(R, [{call_only,Live,_}|_], _St) ->
    check_liveness_live_ret(R, Live);
check_liveness(R, [{call_ext_last,Live,_,_}|_], _St) ->
    check_liveness_live_ret(R, Live);
check_liveness(R, [{call_ext_only,Live,_}|_], _St) ->
    check_liveness_live_ret(R, Live);
check_liveness(R, [{call,Live,_}|Is], St) ->
    case R of
	{x,X} when X < Live -> used;
	{x,_} -> killed;
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{call_ext,Live,Func}|Is], St) ->
    case R of
	{x,X} when X < Live -> used;
	{x,_} -> killed;
	{y,_} ->
	    {extfunc,Mod,Name,Arity} = Func,
	    case erl_bifs:is_exit_bif(Mod, Name, Arity) of
		false ->
		    check_liveness(R, Is, St);
		true ->
		    %% We must make sure we don't check beyond this instruction
		    %% or we will fall through into random unrelated code and
		    %% get stuck in a loop.
		    %%
		    %% We don't want to overwrite a 'catch', so consider this
		    %% register in use.
		    %% 
		    used
	    end
    end;
check_liveness(R, [{call_fun,Live}|Is], St) ->
    case R of
	{x,X} when X < Live -> used;
	{x,_} -> killed;
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{apply,Args}|Is], St) ->
    case R of
	{x,X} when X < Args+2 -> used;
	{x,_} -> killed;
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{apply_last,Args,_}|_], _) ->
    check_liveness_live_ret(R, Args+2);
check_liveness(R, [send|Is], St) ->
    case R of
	{x,X} when X < 2 -> used;
	{x,_} -> killed;
	{y,_} -> check_liveness(R, Is, St)
    end;
check_liveness({x,R}, [{'%live',Live}|Is], St) ->
    if
	R < Live -> check_liveness(R, Is, St);
	true -> killed
    end;
check_liveness(R, [{bif,Op,{f,Fail},Ss,D}|Is], St) ->
    case check_liveness_fail(R, Op, Ss, Fail, St) of
	killed ->
	    case member(R, Ss) of
		true -> used;
		false when R =:= D -> killed;
		false -> check_liveness(R, Is, St)
	    end;
	Other ->
	    Other
    end;
check_liveness(R, [{gc_bif,Op,{f,Fail},_,Ss,D}|Is], St) ->
    case check_liveness_fail(R, Op, Ss, Fail, St) of
	killed ->
	    case member(R, Ss) of
		true -> used;
		false when R =:= D -> killed;
		false -> check_liveness(R, Is, St)
	    end;
	Other ->
	    Other
    end;
check_liveness(R, [{bs_add,{f,0},Ss,D}|Is], St) ->
    case member(R, Ss) of
	true -> used;
	false when R =:= D -> killed;
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_bits_to_bytes2,Src,Dst}|Is], St) ->
    case R of
	Src -> used;
	Dst -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_binary,{f,0},Sz,_,_,Src}|Is], St) ->
    case member(R, [Sz,Src]) of
	true -> used;
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_integer,{f,0},Sz,_,_,Src}|Is], St) ->
    case member(R, [Sz,Src]) of
	true -> used;
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_put_float,{f,0},Sz,_,_,Src}|Is], St) ->
    case member(R, [Sz,Src]) of
	true -> used;
	false -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_restore2,S,_}|Is], St) ->
    case R of
	S -> used;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_save2,S,_}|Is], St) ->
    case R of
	S -> used;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{move,S,D}|Is], St) ->
    case R of
	S -> used;
	D -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{make_fun2,_,NumFree,_,_}|Is], St) ->
    case R of
	{x,X} when X < NumFree -> used;
	{x,_} -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{try_end,Y}|Is], St) ->
    case R of
	Y -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{catch_end,Y}|Is], St) ->
    case R of
	Y -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{get_tuple_element,S,_,D}|Is], St) ->
    case R of
	S -> used;
	D -> killed;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(R, [{bs_context_to_binary,S}|Is], St) ->
    case R of
	S -> used;
	_ -> check_liveness(R, Is, St)
    end;
check_liveness(_R, _Is, _) ->
%%     case _Is of
%% 	[I|_] ->
%% 	    io:format("~p ~p\n", [_R,I]);
%% 	_ -> ok
%%     end,
    unknown.
    
check_liveness_everywhere(R, [{f,Lbl}|T], St0) ->
    case check_liveness_at(R, Lbl, St0) of
	{killed,St} -> check_liveness_everywhere(R, T, St);
	{Other,_} -> Other
    end;
check_liveness_everywhere(R, [_|T], St) ->
    check_liveness_everywhere(R, T, St);
check_liveness_everywhere(_, [], _) -> killed.

check_liveness_at(R, Lbl, #live{lbl=Ll,res=Res0}=St) ->
    case gb_trees:lookup(Lbl, Res0) of
	{value,Res} ->
	    {Res,St};
	none ->
	    Res = case gb_trees:lookup(Lbl, Ll) of
		      {value,Is} -> check_liveness(R, Is, St);
		      none -> unknown
		  end,
	    {Res,St#live{res=gb_trees:insert(Lbl, Res, Res0)}}
    end.

check_liveness_ret(R, R) -> used;
check_liveness_ret(_, _) -> killed.

check_liveness_live_ret({x,R}, Live) ->
    if
	R < Live -> used;
	true -> killed
    end;
check_liveness_live_ret({y,_}, _) -> killed.

check_liveness_fail(_, _, _, 0, _) -> killed;
check_liveness_fail(R, Op, Args, Fail, St0) ->
    Arity = length(Args),
    case erl_internal:comp_op(Op, Arity) orelse
	erl_internal:new_type_test(Op, Arity) of
	true ->
	    killed;
	false ->
	    {Res,_} = check_liveness_at(R, Fail, St0),
	    Res
    end.

%% check_killed_block(Reg, [Instruction], State) -> killed | transparent | used
%%  Finds out how Reg is used in the instruction sequence inside a block.
%%  Returns one of:
%%    killed - Reg is assigned a new value or killed by an allocation instruction
%%    transparent - Reg is neither used nor killed
%%    used - Reg is used or referenced by an allocation instruction.
%%  
%%    (Unknown instructions will cause an exception.)

check_killed_block({x,X}, [{set,_,_,{alloc,Live,_}}|_]) ->
    if 
	X >= Live -> killed;
	true -> used
    end;
check_killed_block(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> used;
	false ->
	    case member(R, Ds) of
		true -> killed;
		false -> check_killed_block(R, Is)
	    end
    end;
check_killed_block(R, [{'%live',Live}|Is]) ->
    case R of
	{x,X} when X >= Live -> killed;
	_ -> check_killed_block(R, Is)
    end;
check_killed_block(_, []) -> transparent.

%% check_used_block(Reg, [Instruction], State) -> killed | transparent | used
%%  Finds out how Reg is used in the instruction sequence inside a block.
%%  Returns one of:
%%    killed - Reg is assigned a new value or killed by an allocation instruction
%%    transparent - Reg is neither used nor killed
%%    used - Reg is explicitly used by an instruction
%%  
%%    (Unknown instructions will cause an exception.)

check_used_block({x,X}=R, [{set,_,_,{alloc,Live,_}}|Is]) ->
    if 
	X >= Live -> killed;
	true -> check_used_block(R, Is)
    end;
check_used_block(R, [{set,Ds,Ss,_Op}|Is]) ->
    case member(R, Ss) of
	true -> used;
	false ->
	    case member(R, Ds) of
		true -> killed;
		false -> check_used_block(R, Is)
	    end
    end;
check_used_block(R, [{'%live',Live}|Is]) ->
    case R of
	{x,X} when X >= Live -> killed;
	_ -> check_used_block(R, Is)
    end;
check_used_block(_, []) -> transparent.

index_labels_1([{label,Lbl}|Is0], Acc) ->
    Is = lists:dropwhile(fun({label,_}) -> true;
			    (_) -> false end, Is0),
    index_labels_1(Is0, [{Lbl,Is}|Acc]);
index_labels_1([_|Is], Acc) ->
    index_labels_1(Is, Acc);
index_labels_1([], Acc) -> gb_trees:from_orddict(sort(Acc)).
