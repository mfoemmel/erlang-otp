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
%% Purpose : Type-based optimisations.

-module(beam_type).

-export([module/2]).

-import(lists, [map/2,reverse/1,filter/2]).

module({Mod,Exp,Attr,Fs,Lc}, Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Asm0}) ->
    {Asm,Ts} = opt(Asm0, [], tdb_new()),
    {function,Name,Arity,CLabel,Asm}.

%% opt([Instruction], Accumulator, TypeDb) -> {[Instruction'],TypeDb'}
%%  For all instructions, first attempt to simplify the instruction,
%%  then update the type information database.

opt([{block,Body0}|Is], Acc, Ts0) ->
    {Body1,Ts1} = opt(Body0, [], Ts0),
    opt(Is, [{block,Body1}|Acc], Ts1);
opt([I0|Is], Acc, Ts0) ->
    case simplify(I0, Ts0) of
	remove ->
	    opt(Is, Acc, Ts0);
	I1 ->
	    Ts1 = update(I1, Ts0),
	    opt(Is, [I1|Acc], Ts1)
    end;
opt([], Acc, Ts) ->
    {reverse(Acc),Ts}.

%% simplify(Instruction, TypeDb) -> NewInstruction
%%  Simplify an instruction using type information (this is
%%  technically a "strength reduction").

simplify({set,[D],[{integer,Index},Reg],{bif,element,F}}=I, Ts) ->
    case max_tuple_size(Reg, Ts) of
	Sz when 0 < Index, Index =< Sz ->
	    {set,[D],[Reg],{get_tuple_element,Index-1}};
	Other -> I
    end;
simplify(Is, Ts) -> Is.

%% update(Instruction, TypeDb) -> NewTypeDb
%%  Update the type database to account for executing an instruction.
%%
%%  First the cases for instructions inside basic blocks.
update({set,[D],[S],move}, Ts0) ->
    Ops = case tdb_find(S, Ts0) of
	      error -> [{D,kill}];
	      Info -> [{D,Info}]
	  end,
    tdb_update(Ops, Ts0);
update({set,[D],[{integer,I},Reg],{bif,element,F}}, Ts0) ->
    tdb_update([{Reg,{tuple,I}},{D,kill}], Ts0);
update({set,[D],[Index,Reg],{bif,element,F}}, Ts0) ->
    tdb_update([{Reg,{tuple,0}},{D,kill}], Ts0);
update({set,[],Src,Op}, Ts0) -> Ts0;
update({set,[D],Src,Op}, Ts0) ->
    tdb_update([{D,kill}], Ts0);
update({set,[D1,D2],Src,Op}, Ts0) ->
    tdb_update([{D1,kill},{D2,kill}], Ts0);
update({allocate,R,Info}, Ts) -> Ts;
update({'%live',R}, Ts) -> Ts;

%% Instructions outside of blocks.
update({test,test_arity,Fail,Src,Arity}, Ts0) ->
    tdb_update([{Src,{tuple,Arity}}], Ts0);
update({test,Test,Fail,Src}, Ts) -> Ts;
update({test,Test,Fail,S1,S2}, Ts) -> Ts;
update({call_ext,3,{extfunc,erlang,setelement,3}}, Ts0) ->
    Op = case tdb_find({x,1}, Ts0) of
	     error -> kill;
	     Info -> Info
	 end,
    Ts1 = tdb_kill_xregs(Ts0),
    tdb_update([{{x,0},Op}], Ts1);
update({call,Arity,Func}, Ts) -> tdb_kill_xregs(Ts);
update({call_ext,Arity,Func}, Ts) -> tdb_kill_xregs(Ts);

%% The instruction is unknown.  Kill all information.
update(_, _) -> tdb_new().

max_tuple_size(Reg, Ts) ->
    case tdb_find(Reg, Ts) of
	{tuple,Sz} -> Sz;
	Other -> 0
    end.


%%% Routines for maintaining a type database.  The type database 
%%% associates type information with registers.
%%%
%%% Currently, the only type information kept is for tuples.
%%% {tuple,Size}, means that the corresponding register contains a
%%% tuple with *at least* Size elements.  An tuple with unknown
%%% size is represented as {tuple,0}.

%% tdb_new() -> EmptyDataBase
%%  Creates a new, empty type database.

tdb_new() -> [].

%% tdb_find(Register, Db) -> Information|error
%%  Returns type information or the atom error if there are no type
%%  information available for Register.

tdb_find(Key, [{K,Info}|Db]) when Key < K -> error;
tdb_find(Key, [{Key,Info}|Db]) -> Info;
tdb_find(Key, [_|Db]) -> tdb_find(Key, Db);
tdb_find(Key, []) -> error.

%% tdb_update([UpdateOp], Db) -> NewDb
%%        UpdateOp = {Register,kill}|{Register,NewInfo}
%%  Updates a type database.  If a 'kill' operation is given, the type
%%  information for that register will be removed from the database.
%%  A kill operation takes precende over other operations for the same
%%  register (i.e. [{{x,0},kill},{{x,0},{tuple,5}}] means that the
%%  the existing type information, if any, will be discarded, and the
%%  the '{tuple,5}' information ignored.
%%
%%  If NewInfo information is given and there exists information about
%%  the register, the old and new type information will be merged.
%%  For instance, {tuple,5} and {tuple,10} will be merged to produce
%%  {tuple,10}.

tdb_update(Uis0, Ts0) ->
    Uis1 = filter(fun ({{x,R},Op}) -> true;
		      ({{y,R},Op}) -> true;
		      (_) -> false
		  end, Uis0),
    tdb_update1(lists:sort(Uis1), Ts0).

tdb_update1([{Key,kill}|Ops], [{K,Old}|_]=Db) when Key < K ->
    tdb_update1(remove_key(Key, Ops), Db);
tdb_update1([{Key,NewInfo}=New|Ops], [{K,Old}|_]=Db) when Key < K ->
    [New|tdb_update1(Ops, Db)];
tdb_update1([{Key,kill}|Ops], [{Key,OldInfo}|Db]) ->
    tdb_update1(remove_key(Key, Ops), Db);
tdb_update1([{Key,NewInfo}|Ops], [{Key,OldInfo}|Db]) ->
    [{Key,merge_type_info(NewInfo, OldInfo)}|tdb_update1(Ops, Db)];
tdb_update1([{Key,Op}|_]=Ops, [Old|Db]) ->
    [Old|tdb_update1(Ops, Db)];
tdb_update1([{Key,kill}|Ops], []) ->
    tdb_update1(remove_key(Key, Ops), []);
tdb_update1([{Key,NewInfo}=New|Ops], []) ->
    [New|tdb_update1(Ops, [])];
tdb_update1([], Db) -> Db.

%% tdb_kill_xregs(Db) -> NewDb
%%  Kill all information about x registers.

tdb_kill_xregs([{x,R}|Db]) -> tdb_kill_xregs(Db);
tdb_kill_xregs([{y,R}|_]=Db) -> Db;
tdb_kill_xregs([Any|Db]) -> [Any|tdb_kill_xregs(Db)];
tdb_kill_xregs([]) -> [].
    
remove_key(Key, [{Key,Op}|Ops]) -> remove_key(Key, Ops);
remove_key(Key, Ops) -> Ops.
    
merge_type_info(I, I) -> I;
merge_type_info({tuple,Sz1}, {tuple,Sz2}=Max) when Sz1 < Sz2 -> Max;
merge_type_info({tuple,Sz1}=Max, {tuple,Sz2}) -> Max.
