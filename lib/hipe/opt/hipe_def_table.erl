%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		 TURNING CFG INTO A TABLE OF DEF/USE
%
% This file collects info on CFG:s into a number of tables. This is
% later useful for dataflow analyses.
%
% Information collected (see below for order):
%   InstrTable: maps InstrID -> {Instruction,Set(Def),Set(Use),HomeBlock}
%      where Def_list and Use_list are lists of Def/Use IDs
%        and Instruction is the original instruction
%        and HomeBlock is the label of the block of the instruction.
%   DefTable: maps DefID -> {InstrID,VarName}
%     where DefID maps to the instruction and variable where the
%     definition occurs.
%   UseTable: maps UseID -> {InstrID,VarName}
%     where UseID maps to the instruction and variable where the
%     use occurs.
%   VarMap: maps VarName -> DefsKilled_list
%     where DefsKilled_list is a set of the DefIDs killed when VarName
%     is defined.
%   BlockMap: maps labels to the list of instructions in the block
%   NextInstr: next available instruction ID
%   NextDef: as above for defs
%   NextUse: as above for uses
%
% A glimpse of the future:
% Subsequently, reaching definitions will add tables that represent
% def/use links. Naively, these will require quadratic space as m uses of x
% are each reached by n defs of x. Introducing SSA-style joins in the tables
% will remedy this problem.
%
% Returns:
%  { IID, DID, UID,
%    IT, DT, UT,
%    Var, Blk, Param_list, GenKill_map }
% where IID, DID and UID are next ID:s for instructions, defs and uses
%   IT is the instruction table (InstrID -> {Instr,List(Def),List(Use),Home})
%   DT is the def-table (DefID -> {InstrID,Var})
%   UT is the use-table (UseID -> {InstrID,Var})
%   Var is the kill map per variable: (Var -> Set(Killed_Def))
%   Blk maps labels to instr ID lists (Label -> [InstrID])
%   GenKill_map maps instr IDs to the generated and killed defs
%                             (InstrID -> {Set(GenDef),Set(KillDef)}

-module(hipe_def_table).
-export([cfg/1,genkill/2,list_block_map/1,get/2,get_kills/2,
	 block_lookup/2,var_lookup/2]).

-define(cfg,hipe_icode_cfg).
-define(code,hipe_icode).

-define(report(Str,Args),report(Str,Args)).

% IID, DID, UID are the 'next index' for instructions, defs and uses.
% IT, DT, UT are tables that map instructions, defs and uses to their
%  respective info
%   IT: instrID -> {Instruction, Def_list, Use_list}
%   DT: defID -> {InstrID,Var}
%   UT: useID -> {InstrID,Var}
% Var maps variables to the sets of defs that are killed by them
%       Var -> [defID]
% Blk maps labels to instrID lists: (Label -> [instrID])
% Param_list is a list of the CFG parameters
% GenKill_map maps instrIDs to the defs they generate and kill:
%       instrID -> {set(GenDefs),set(KillDefs)}
%
% Returns {IID,DID,UID,IT,DT,UT,Var,Blk,Param_list,GenKill_map}.
%
% Notes:
% - IID, UID start at 1 since this is the base index of a vector.
%   (I presume we will use vectors for these eventually). I have abstracted
%   this into a generic 'starting index'.
% - DID, DT, IID, IT are initialized by taking each parameter as a definition.

cfg(CFG) ->
    {IID,DID,IT,DT,Var,Params} = params_mapped(CFG),
    UT = empty_index_map(),
    UID = starting_index(),
    Blk = empty_block_map(),
    {NewIID,NewDID,NewUID,
     NewIT,NewDT,NewUT,
     NewVar,NewBlk} = deftab([ {L, hipe_bb:code(?cfg:bb(CFG,L))} 
			      || L <- ?cfg:labels(CFG) ],
			     IID, DID, UID,
			     IT, DT, UT,
			     Var,Blk),
    {NewIID,NewDID,NewUID,
     NewIT,NewDT,NewUT,
     NewVar,NewBlk,
     Params,genkill_map(NewIT,NewDT,NewVar)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Adds pseudo-definitions for input parameters.

params_mapped(CFG) ->
    Params = ?cfg:params(CFG),
    InstrID = 1,
    DID = 1,
    Instr = {input_params,Params},
    DT = empty_index_map(),
    IT = empty_index_map(),
    Var = empty_var_map(),
    L = 0,                   % assumes this label can be used!
    {Ds,NewDID,NewDT,NewVar} = add_defs(Params,DID,InstrID,DT,Var),
    NewIT = add_instr(InstrID,Instr,Ds,set:empty(),L,IT),
    {InstrID+1,NewDID,NewIT,NewDT,NewVar,Params}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pass IID, DID, UID, IT, DT, UT and Var through the blocks.

deftab([],IID,DID,UID,IT,DT,UT,Var,Blk) -> {IID,DID,UID,IT,DT,UT,Var,Blk};
deftab([{L,B}|Bs],IID0,DID0,UID0,IT0,DT0,UT0,Var0,Blk0) ->
    {IID1,DID1,UID1,IT1,DT1,UT1,Var1} = deftab_block(B,L,IID0,DID0,UID0,
						     IT0,DT0,UT0,Var0),
    Blk1 = block_defs(L,IID0,IID1,Blk0),
    deftab(Bs,IID1,DID1,UID1,IT1,DT1,UT1,Var1,Blk1).

% Pass IID, DID, UID, IT, DT, UT and Var through the instructions.

deftab_block([],L,IID,DID,UID,IT,DT,UT,Var) ->
    {IID,DID,UID,IT,DT,UT,Var};
deftab_block([I|Is],L,IID0,DID0,UID0,IT0,DT0,UT0,Var0) ->
    {IID1,DID1,UID1,IT1,DT1,UT1,Var1} = deftab_instr(I,L,IID0,DID0,UID0,
						     IT0,DT0,UT0,Var0),
    deftab_block(Is,L,IID1,DID1,UID1,IT1,DT1,UT1,Var1).

deftab_instr(Instr,L,IID,DID,UID,IT,DT,UT,Var) ->
    {D,U} = def_use(Instr),
    InstrID = IID,
    {Ds,NewDID,NewDT,NewVar} = add_defs(D,DID,InstrID,DT,Var),
    {Us,NewUID,NewUT} = add_uses(U,UID,InstrID,UT),
    NewIT = add_instr(InstrID,Instr,Ds,Us,L,IT),
    NewIID = IID+1,
    {NewIID,NewDID,NewUID,NewIT,NewDT,NewUT,NewVar}.

% Add instruction to instruction table. We keep the following info:
%  * original instruction
%  * definitions of instr
%  * uses of instr
%  * home block of instr

add_instr(InstrID,Instr,Ds,Us,HomeBlock,IT) ->
    mapto(InstrID,{Instr,Ds,Us,HomeBlock},IT).

% Add definitions to the appropriate tables.
%
% Note:
% Somewhat clumsy since we lose tail recursion; if it turns out
%  to be a problem, recode it to pass an accumulating parameter of Ds
%  and reverse this list at the end.

add_defs([],DID,InstrID,DT,Var) -> {set:empty(),DID,DT,Var};
add_defs([X|Xs],DID,InstrID,DT,Var) ->
    {NxtDT,NxtVar} = add_def(X,InstrID,DID,DT,Var),
    NxtDID = DID+1,
    {Ds,NewDID,NewDT,NewVar} = add_defs(Xs,NxtDID,InstrID,NxtDT,NxtVar),
    {set:add_singleton(DID,Ds),NewDID,NewDT,NewVar}.

add_def(X,InstrID,DID,DT,Var) ->
    NewDT = mapto(DID,{InstrID,X},DT),
    NewVar = add_kill(X,DID,Var),
    {NewDT,NewVar}.

% Add uses to the appropriate tables.
%
% See comments to add_defs if efficiency is a problem.

add_uses([],UID,InstrID,UT) -> {set:empty(),UID,UT};
add_uses([X|Xs],UID,InstrID,UT) ->
    NxtUID = UID+1,
    NxtUT = add_use(X,UID,InstrID,UT),
    {Us,NewUID,NewUT} = add_uses(Xs,NxtUID,InstrID,NxtUT),
    {set:add_singleton(UID,Us),NewUID,NewUT}.

add_use(X,UID,InstrID,UT) ->
    mapto(UID,{InstrID,X},UT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% We can essentially choose 0 or 1. I use 1 because vectors are 1-indexed.

starting_index() -> 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_index_map() ->
    hash:empty().



mapto(Index,Value,Map) when integer(Index), Index >= 1 ->
    hash:insert(Index,Value,Map).

get(Map,Index) when integer(Index), Index >= 1 ->
    case hash:lookup(Index,Map) of
	not_found ->
	    exit({invalid_get,Index});
	{found,Value} ->
	    Value
    end.

list_map(Map) ->
    hash:list(Map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_var_map() ->
    hash:empty().

add_kill(X,Def,Map) ->
  Old =
    case hash:lookup(X,Map) of
      not_found ->
	set:empty();
      {found,Old0} ->
	Old0
    end,
  New = set:add_singleton(Def,Old),
  hash:update(X,New,Map).

get_kills(X,Map) ->
    case hash:lookup(X,Map) of
	not_found ->
	    ?report('no defs for ~p found~n',[X]),
	    set:empty();
	{found,Def_set} ->
	    Def_set
    end.



var_lookup(X,Map) ->
    get_kills(X,Map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_block_map() ->
    hash:empty().

block_defs(Label,From,To,Blk) ->
    hash:update(Label,from_to(From,To),Blk).

block_lookup(Label,Blk) ->
    case hash:lookup(Label,Blk) of
	not_found ->
	    exit({block_lookup,{no_such_label,Label}});
	{found,B} ->
	    B
    end.

from_to(From,To) ->
    if
	From >= To -> [];
	true -> [From|from_to(From+1,To)]
    end.

list_block_map(Blk) ->
    hash:list(Blk).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The 'kill_map' maps instructions to the definitions killed and generated
% by them.
%
% - for each instruction
%    * get the definition set of the instruction
%    * for each def d in the def.set,
%        find the defs it kills
%    * total kills = union of kills of all d
%    * total gen = def.set
%
% Yields list of {InstrID,{Gen,Kill}} where Gen and Kill are sets of defs.

genkill_map(IT,DT,Var) ->
    hash:init([ {I,
		 {Ds,
		  set:union_list([ get_kills(def_var(DT,D),Var) 
				  || D <- set:list(Ds) ])}}
	       || {I,{Instr,Ds,Us,Home}} <- list_map(IT) ]).

def_var(DT,D) ->
    {IID,X} = get(DT,D),
    X.



genkill(InstrID,GK_map) ->
    get(GK_map,InstrID).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** PORTING ***

def_use(Instr) ->
    {?code:defines(Instr),?code:uses(Instr)}.

report(Str,Args) ->
    io:format(Str,Args).




