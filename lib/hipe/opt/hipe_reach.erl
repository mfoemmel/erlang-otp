%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			 REACHING DEFINITIONS
%
% Compute reaching definitions for a CFG.
%

-module(hipe_reach).
-export([cfg/1,cfg_annot/1]).
-compile(export_all).

-define(cfg,hipe_icode_cfg).
-define(code,hipe_icode).
-define(debug(Str,Xs),io:format(Str,Xs)).

% Returns {Deftab,N,InVals}
%   where Deftab is a definition table (see below)
%         N is
%         InVals is a hash table (Label -> DefSet)

cfg(CFG) ->
    {Deftab,N} = name_defs(CFG),
    Transf = hash:init(rd_transf(?cfg:labels(CFG),CFG,Deftab)),
    Succ = ?cfg:succ_map(CFG),
    {Start,In} = start_invals(CFG),
    {Deftab,N,fix_all(Start,Succ,Transf,In)}.

% Annotate each use with its reaching definitions

cfg_annot(CFG) ->
    ?debug('reach defs~n',[]),
    {Deftab,_,In} = cfg(CFG),
    ?debug('annotation~n',[]),
    [ {L, annot_block(hipe_bb:code(?cfg:bb(CFG,L)),
		      inval(L,In),
		      Deftab,L,start_pos(),CFG)}
     || L <- ?cfg:labels(CFG) ].

annot_block([],In,Deftab,L,Pos,CFG) -> [];
annot_block([I|Is],In,Deftab,L,N,CFG) ->
    NewI = annot_instr(I,In,Deftab,L,N,CFG),
    Out = apply_transf(gen_kill_instr(L,N,I,Deftab),In),
    [NewI|annot_block(Is,Out,Deftab,L,N+1,CFG)].

annot_instr(Instr,In,Deftab,L,N,CFG) ->
    {N, Instr, [ {U, reaching_defs(U,In,Deftab,CFG)} 
		|| U <- ?code:uses(Instr) ]}.

reaching_defs(U,In,Deftab,CFG) ->
    Refs = killed_by_var(U,Deftab),
    Relevant = set:list(set:intersect(Refs,In)),
    [ reaching_instr(CFG,Deftab,R) || R <- Relevant ].

reaching_instr(CFG,Deftab,R) ->
    {L,Pos} = def_of(R,Deftab),
    instr_of(L,Pos,CFG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% name_defs(CFG)
%   returns {DefTable, N}
%    where DefTable is a def table (see below)
%          N is the number of definitions allocated (poss +1?)

name_defs(CFG) ->
    Ps = ?cfg:params(CFG),
    Deftab = empty_def_table(Ps),
    name_defs(?cfg:labels(CFG),CFG,first_eqv(Ps),Deftab).

first_eqv(Ps) -> start_def() + length(Ps).

name_defs([],CFG,Ix,Deftab) -> {Deftab,Ix};
name_defs([L|Ls],CFG,Ix,Deftab) ->
    { NewDeftab, NewIx } = name_defs_block(hipe_bb:code(?cfg:bb(CFG,L)),L,Ix,Deftab),
    name_defs(Ls,CFG,NewIx,NewDeftab).

name_defs_block(Xs,L,Ix,Deftab) ->
    name_defs_block(Xs,L,start_pos(),Ix,Deftab).

name_defs_block([],L,Pos,Ix,Deftab) -> {Deftab,Ix};
name_defs_block([I|Is],L,Pos,Ix,Deftab) ->
    {NewDeftab, NewIx} = add_defs(I,Deftab,L,Pos,Ix),
    name_defs_block(Is,L,Pos+1,NewIx,NewDeftab).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing transfer functions per block
% - yields a list {Label, {Gen, Kill}}

rd_transf0(Ls,CFG,Deftab) ->
    [ {L,gen_kill_block_catch(hipe_bb:code(?cfg:bb(CFG,L)),L,Deftab)} 
     || L <- Ls ].

gen_kill_block_catch(Instrs,L,Deftab) ->
    case catch gen_kill_block(Instrs,L,Deftab) of
	{'EXIT',_} -> exit;
	Res -> {ok,Res}
    end.

rd_transf(Ls,CFG,Deftab) ->
    [ {L,gen_kill_block(hipe_bb:code(?cfg:bb(CFG,L)),L,Deftab)} 
     || L <- Ls ].

gen_kill_block(Instrs,L,Deftab) ->
    Start_gen = set:empty(),
    Start_kill = set:empty(),
    gen_kill_block(Instrs,L,start_pos(),Deftab,Start_gen,Start_kill).

% L label, N position in block, Deftab deftable, Gen = set, Kill = set

gen_kill_block([],L,N,Deftab,Gen,Kill) -> {Gen,Kill};
gen_kill_block([I|Is],L,N,Deftab,Gen0,Kill0) ->
    {Gen,Kill} = gen_kill_instr(L,N,I,Deftab),
    Kill1 = set:union(Kill0,Kill),
    Gen1 = set:union(Gen, set:diff(Gen0, Kill)),
    gen_kill_block(Is, L, N+1, Deftab, Gen1, Kill1).

gen_kill_instr(L, Pos, Instr, Deftab) ->
    Defs = ?code:defines(Instr),
    Gen = indices_of_defs(L,Pos,Defs,Deftab),
    Kill = killed_by_defs(Defs,Deftab),
    { Gen, Kill }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Reaching definitions by fixpoint iteration
% - note: uses chaotic iteration, since I'm feeling lazy :-)
%
% Returns a hash table (Label -> InVal)

% testing function:
fix(CFG) ->
    {Deftab,_} = name_defs(CFG),
    Transf = hash:init(rd_transf(?cfg:labels(CFG),CFG,Deftab)),
    Succ = ?cfg:succ_map(CFG),
    {Start,In} = start_invals(CFG),
    fix_all(Start,Succ,Transf,In).

fix_all([],Succ,Transf,In) -> In;
fix_all([L|Ls],Succ,Transf,In) ->
    fix_block(L,Ls,Succ,Transf,In).

fix_block(L,Ls,Succ,Transf,In) ->
    Out = apply_transf(transf_of(L,Transf),inval(L,In)),
    { NewLs, NewIn } = prop_succ(?cfg:succ(Succ,L), Ls, Out, In),
    fix_all(NewLs,Succ,Transf,NewIn).

prop_succ([],Ls,Out,In) -> {Ls,In};
prop_succ([L|Ls],Rest,Out,In) ->
    case changed(L,Out,In) of
	{yes,NewIn} ->
	    prop_succ(Ls,[L|Rest],Out,NewIn);
	no ->
	    prop_succ(Ls,Rest,Out,In)
    end.

apply_transf({Gen,Kill},Inval) ->
    set:union(Gen,set:diff(Inval,Kill)).

transf_of(L,Transf) ->
    {found,Trfun} = hash:lookup(L,Transf),
    Trfun.

% *** UNFINISHED ***
% - start_invals should make defs of all input params available
%   to start node
% - either should defines/1 be redefined to only return rtl_vars
%   or we must provide which rtl_regs are defined everywhere

start_invals(CFG) ->
    Entry = ?cfg:start(CFG),
    Start = [Entry],
    EntryInval = mk_def_set(?cfg:params(CFG),start_def(),set:empty()),
    { Start, hash:update(Entry,EntryInval,hash:empty()) }.

mk_def_set([],N,Set) -> Set;
mk_def_set([_|Xs],N,Set) ->
    mk_def_set(Xs,N+1,set:add_singleton(N,Set)).

inval(L,In) ->
    case hash:lookup(L,In) of
	{found,Inval} -> Inval;
	not_found -> set:empty()
    end.

changed(L,New,In) ->
    Old = inval(L,In),
    case set:included_by(New,Old) of
	true ->
	    no;
	false ->
	    {yes, hash:update(L,set:union(New,Old),In)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% A Def_table is a tuple of hash tables,
%  { (Def -> Index), (Index -> Def), (Var -> set(Index)) }
% where
%  Def is a tuple {Block_label, Block_position, Var }
%  Index is an integer (unique to this def)
%  Var is a variable name, which maps to the set of indices killed
%    when Var is defined.
%
% Interface:
%
%  When def-table has been constructed:
%
%  - gen_kill_instr( Label, Pos, Instr, Deftable ) => {set(Index), set(Index)}
%  - index_of( Label, Pos, Var, Deftable) => index
%  - def_of(Index, Deftable) => { Label, Pos }
%  - defvar_of(Index, Deftable) => { Label, Pos, Var }
%  - instr_of( {Label, Pos}, Deftable ) => Instr
%  - instr_of( Label, Pos, Deftable ) => Instr
%
%  Constructing the def table:
%  - empty_def_table()
%  - add_defs( Instr, Deftab, Label, Pos )
%

%%%%%%%%%%%%%%%%%%%%
% Def table initially contains only the N parameters, numbered from 1..N
%
% The Deftable has the following data structures:
% - a mapping ({Label,Pos,Var} -> Index), yielding the index of a definition
% - a mapping (Index -> {Label,Pos,Var}), yielding the def of an index
% - a mapping (Var -> set(Index)), yielding the indices killed if Var is
%   defined.

empty_def_table(Ps) ->
    {Deftab,_} = add_ds(Ps,
			param_pos(),
			empty_def_table(),
			param_label(),
			start_def()),
    Deftab.

empty_def_table() -> { hash:empty(), hash:empty(), hash:empty() }.

param_pos() -> start.

param_label() -> start.

start_def() -> 1.

%%%%%%%%%%%%%%%%%%%%

% Given an instruction, a Deftab, a full position (L,Pos) and a starting
% index N, add the new definitions to the deftab.
% * for each variable x defined by Instr,
%   add a new definition N and increment index counter to N+1
%
% Returns {NewDeftab, NewIndex}

add_defs(Instr,Deftab,L,Pos,N) ->
    Def_lst = ?code:defines(Instr),
    case Def_lst of
	[] ->
	    {Deftab,N};
	_ ->
	    add_ds(Def_lst,Pos,Deftab,L,N)
    end.

start_pos() -> 0.

add_ds([],Pos,Deftab,L,N) -> {Deftab,N};
add_ds([X|Xs],Pos,Deftab,L,N) ->
    NewDeftab = map_def(X,L,Pos,N,Deftab),
    add_ds(Xs,Pos,NewDeftab,L,N+1).

% Given a variable X, label L, position Pos and index N, (and a Deftab)
% - map {L,Pos,X} to N.
% - map N to {L,Pos,X}
% - add N to the set of indices killed by defining X

map_def(X,L,Pos,N,{ Def_to_ix, Ix_to_def, Kill_map }) ->
    Def_name = {L,Pos,X},
    { hash:insert(Def_name, N, Def_to_ix), 
      hash:insert(N, Def_name, Ix_to_def),
      add_kill(X,N,Kill_map)
    }.

% Given label L, position Pos and a list of defined variables Defs
% and a Deftab, find the set(Index) of these definitions.

indices_of_defs(L, Pos, Defs, Deftab) -> 
    indices_of_defs(Defs, L, Pos, Deftab, set:empty()).

indices_of_defs([], L, Pos, Deftab, Set) -> Set;
indices_of_defs([X|Xs], L, Pos, Deftab, Set) ->
    N = index_of(L,Pos,X,Deftab),
    indices_of_defs(Xs, L, Pos, Deftab, set:add_singleton(N,Set)).

% Given a position {L,Pos,Var}, return the associated index.

index_of(L,Pos,X, { Def_to_ix, Ix_to_def, Kill_map }) ->
    {found,N} = hash:lookup({L,Pos,X},Def_to_ix),
    N.

% Given an index, return the {Label,Position} of the index

def_of(N, { Def_to_ix, Ix_to_def, Kill_map }) ->
    {found,{L,Pos,X}} = hash:lookup(N,Ix_to_def),
    {L,Pos}.

% Given an index, return the {Label,Position,Variable} of the index.

defvar_of(N, { Def_to_ix, Ix_to_def, Kill_map }) ->
    {found,{L,Pos,X}} = hash:lookup(N,Ix_to_def),
    {L,Pos,X}.

% Given a variable, return the set(Index) killed by the variable
%
% Note: this is simply the set of definitions of the variable,
%   so it is useful for other purposes as well. Thus, referring_defs/2.

killed_by_var(X, { Def_to_ix, Ix_to_def, Kill_map }) ->
    case hash:lookup(X,Kill_map) of
	not_found ->
	    set:empty();
	{found,Set} ->
	    Set
    end.

% Returns the set(Index) of definitions that define X.

referring_defs(X, Deftab) ->
    killed_by_var(X, Deftab).

% Given a list of defined variables, find the total set of defs killed
% by the vars. (Union of the individual kills.)

killed_by_defs(Xs,Deftab) -> kills_of_defs(Xs,Deftab,set:empty()).

kills_of_defs([],Deftab,Set) -> Set;
kills_of_defs([X|Xs],Deftab,Set) ->
    kills_of_defs(Xs,Deftab,set:union(killed_by_var(X,Deftab),Set)).

% Given variable X, an index N killed by X, and a Kill_map,
% return a new Kill_map where N is added to the kill set of X.
%
% (Note: internal function)

add_kill(X,N,Kill_map) ->
  N_set = set:add_singleton(N,set:empty()),
  NewSet = 
    case hash:lookup(X,Kill_map) of
      not_found ->
	N_set;
      {found,OldSet} ->
	set:union(N_set,OldSet)
    end,
  hash:update(X,NewSet,Kill_map).

% Given Label and Position and a CFG, 
% return the instruction at (Label,Pos) in CFG.

instr_of({L,Pos},CFG) -> instr_of(L,Pos,CFG).

instr_of(start,start,CFG) -> param;
instr_of(L,N,CFG) ->
    get_pos(N,hipe_bb:code(?cfg:bb(CFG,L))).

get_pos(0,[I|_]) -> I;
get_pos(N,[_|Is]) when N > 0 ->
    get_pos(N-1,Is).

