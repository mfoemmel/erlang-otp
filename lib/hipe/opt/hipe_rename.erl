%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			RENAMING OF VARIABLES
%
% The stack-to-registers translation scheme introduces unnecessary
% anti-dependences into the code. These hinder scheduling as well
% as dataflow optimization, so we would like to get rid of them.
%
% NOTE: PROTOTYPE
% - Can probably be rewritten to improve speed quite a bit.
%   In particular, the renaming pass could probably be improved
%   considerably with a bit of redesign. (The current version is
%   more suited to debugging :-)

-module(hipe_rename).
-export([cfg/1]).

-define(code,hipe_icode).
-define(cfg,hipe_icode_cfg).

% Renaming is done as follows:
% - first compute reaching definitions
%   * give each def a unique name
%   * compute the set of defs reaching each block
% - merge equivalent defs
%   * initialize all defs into singleton equivalence classes
%   * for each use, merge the classes of all reaching defs
% - rename the defs
%   * each equivalence class gets its own variable name
%   * walk each block, renaming uses and defs
%
% *** UNFINISHED ***
% - rename_cfg/1 is an abomination and should be rewritten.

cfg(CFG) ->
    rename_cfg(CFG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name_defs(CFG) ->
    Ps = ?cfg:params(CFG),
    Ds = empty_def_table(Ps),
    name_defs(?cfg:depth_first_ordering(CFG),CFG,first_eqv(Ps),Ds).

first_eqv(Ps) -> start_def() + length(Ps).

name_defs([],CFG,Ix,Ds) -> {Ds,Ix};
name_defs([L|Ls],CFG,Ix,Ds) ->
    { NewDs, NewIx } = name_defs_block(hipe_bb:code(?cfg:bb(CFG,L)),L,Ix,Ds),
    name_defs(Ls,CFG,NewIx,NewDs).

name_defs_block(Xs,L,Ix,Ds) ->
    name_defs_block(Xs,L,start_pos(),Ix,Ds).

name_defs_block([],L,Pos,Ix,Ds) -> {Ds,Ix};
name_defs_block([I|Is],L,Pos,Ix,Ds) ->
    {NewDs, NewIx} = add_defs(I,Ds,L,Pos,Ix),
    name_defs_block(Is,L,Pos+1,NewIx,NewDs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Computing transfer functions per block
% - yields a list {Label, {Gen, Kill}}

%rd_transf0(Ls,CFG,Ds) ->
%    [ {L,gen_kill_block_catch(hipe_bb:code(?cfg:bb(CFG,L)),L,Ds)} 
%     || L <- Ls ].

%gen_kill_block_catch(Instrs,L,Ds) ->
%    case catch gen_kill_block(Instrs,L,Ds) of
%	{'EXIT',_} -> exit;
%	Res -> {ok,Res}
%    end.

rd_transf(Ls,CFG,Ds) ->
    [ {L,gen_kill_block(hipe_bb:code(?cfg:bb(CFG,L)),L,Ds)} 
     || L <- Ls ].

gen_kill_block(Instrs,L,Ds) ->
    Start_gen = gb_sets:empty(),
    Start_kill = gb_sets:empty(),
    gen_kill_block(Instrs,L,start_pos(),Ds,Start_gen,Start_kill).

% L label, N position in block, Ds deftable, Gen = set, Kill = set

gen_kill_block([],L,N,Ds,Gen,Kill) -> {Gen,Kill};
gen_kill_block([I|Is],L,N,Ds,Gen0,Kill0) ->
    {Gen,Kill} = gen_kill_instr(L,N,I,Ds),
    Kill1 = gb_sets:union(Kill0,Kill),
    Gen1 = gb_sets:union(Gen, gb_sets:difference(Gen0, Kill)),
    gen_kill_block(Is, L, N+1, Ds, Gen1, Kill1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Reaching definitions by fixpoint iteration
% - note: uses chaotic iteration, since I'm feeling lazy :-)

%fix(CFG) ->
%    {Ds,_} = name_defs(CFG),
%    Transf = hipe_hash:init(rd_transf(?cfg:depth_first_ordering(CFG),CFG,Ds)),
%    Succ = ?cfg:succ_map(CFG),
%    {Start,In} = start_invals(CFG),
%    fix_all(Start,Succ,Transf,In).

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
    gb_sets:union(Gen,gb_sets:difference(Inval,Kill)).

transf_of(L,Transf) ->
    {found,Trfun} = hipe_hash:lookup(L,Transf),
    Trfun.

% *** UNFINISHED ***
% - start_invals should make defs of all input params available
%   to start node
% - either should defines/1 be redefined to only return rtl_vars
%   or we must provide which rtl_regs are defined everywhere

start_invals(CFG) ->
    Entry = ?cfg:start(CFG),
    Fail_entries = ?cfg:fail_entrypoints(CFG),
    Start = [Entry|Fail_entries],
    EntryInval = mk_def_set(?cfg:params(CFG),start_def(),gb_sets:empty()),
    { Start, hipe_hash:update(Entry,EntryInval,hipe_hash:empty()) }.


mk_def_set([_|Xs],N,Set) ->
    mk_def_set(Xs,N+1,gb_sets:add(N,Set));
mk_def_set([],N,Set) -> Set.

inval(L,In) ->
    case hipe_hash:lookup(L,In) of
	{found,Inval} -> Inval;
	not_found -> gb_sets:empty()
    end.

changed(L,New,In) ->
  case hipe_hash:lookup(L,In) of
    {found,Old} -> 
      case gb_sets:is_subset(New,Old) of
	true ->
	  no;
	false ->
	  {yes, hipe_hash:update(L,gb_sets:union(New,Old),In)}
      end;
    not_found -> 
      {yes, hipe_hash:update(L,New,In)}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Equivalence class handling.
% for each block
%  for each instr
%   for each use
%    * collect all reaching defs relevant to use
%    * merge their equivalence classes
%
% Notes: this was built on annot_cfg/1 below.

% CFG is the CFG, In is the table of invalues, Ds is deftable, N is #defs

equiv(CFG,In,Ds,N) ->
    %Eqv = hipe_ufind:init(N),
    %Eqv = gb_sets:from_list( lists:seq(1,N) ),
    %Eqv = init_eqv(N),
    Eqv = lists:foldl( fun(X,Acc) -> gb_trees:insert(X,X,Acc) end, gb_trees:empty(), lists:seq(1,N) ),
    equiv_blocks(?cfg:depth_first_ordering(CFG),CFG,In,Ds,Eqv).

equiv_blocks([],CFG,In,Ds,Eqv) -> Eqv;
equiv_blocks([L|Ls],CFG,In,Ds,Eqv) ->
    
    NewEqv = equiv_block(hipe_bb:code(?cfg:bb(CFG,L)),inval(L,In),
			 L,start_pos(),Ds,Eqv),
    equiv_blocks(Ls,CFG,In,Ds,NewEqv).

equiv_block([],In,L,Pos,Ds,Eqv) -> Eqv;
equiv_block([I|Is],In,L,Pos,Ds,Eqv) ->
    NewEqv = equiv_uses(?code:uses(I),In,Ds,Eqv),
    Out = apply_transf(gen_kill_instr(L,Pos,I,Ds),In),
    equiv_block(Is,Out,L,Pos+1,Ds,NewEqv).

equiv_uses([],In,Ds,Eqv) -> Eqv;
equiv_uses([U|Us],In,Ds,Eqv) ->
    Rel_defs = killed_by_var(U,Ds),
    Eq_defs = gb_sets:intersection(Rel_defs,In),
    %NewEqv = merge_defs(gb_sets:to_list(Eq_defs),Eqv),
    NewEqv = merge_defs(Eq_defs,Eqv),
    equiv_uses(Us,In,Ds,NewEqv).

merge_defs(Equiv_defs,Eqv) ->
     %gb_sets:union(Equiv_defs,Eqv).
    %hipe_ufind:union_list(Equiv_defs,Eqv).
    %I = gb_sets:iterator(Equiv_defs),
    {First,NewEqv} = gb_sets:take_smallest(Equiv_defs),
    gb_sets:fold( fun(X,Acc) -> gb_trees:update( X, First, Acc ) end, Eqv, NewEqv ). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Returns the "resolved" renaming for readability

%rename_test(N,Eqv,Ds) ->
%    rename_test_list(start_def(),N,Eqv,Ds).

%rename_test_list(M,N,Eqv,Ds) ->
%    if
%	M >= N ->
%	    [];
%	true ->
%	    [{defvar_of(M,Ds),hipe_ufind:only_find(M,Eqv)}
%	     |rename_test_list(M+1,N,Eqv,Ds)]
%    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Renaming, based on CFG annotation.
%
% Uses a lot of functions defined for annot_cfg/1 too.
%
% *** UNFINISHED ***
% - uses test11/1, which is only a testing function.

rename_cfg(CFG) ->
  {Ds,N,In} = test12(CFG),
  Eqv = equiv(CFG,In,Ds,N),
  Labels = ?cfg:depth_first_ordering(CFG),
  CFG0 = rename_blocks(Labels,CFG,In,Ds,Eqv),
  CFG1 = rename_params(CFG0,Ds,Eqv),
  CFG1.

rename_params(CFG,Ds,Eqv) ->
   NewPs = [ ?code:mk_var(find(index_of(start,start,P,Ds),Eqv))
	    || P <- ?cfg:params(CFG) ],
   NewCFG = ?cfg:params_update(CFG,NewPs).

rename_blocks([],CFG,In,Ds,Eqv) -> CFG;
rename_blocks([L|Ls],CFG,In,Ds,Eqv) ->
   BB = ?cfg:bb(CFG,L),
   NewCode = rename_block(hipe_bb:code(BB), inval(L,In), Eqv,Ds,L,start_pos()),
   NewBlk = hipe_bb:code_update(BB, NewCode),
   NewCFG = ?cfg:bb_update(CFG,L,NewBlk),
   rename_blocks(Ls,NewCFG,In,Ds,Eqv).

rename_block([],Inval,Eqv,Ds,L,Pos) -> [];
rename_block([I|Is],In,Eqv,Ds,L,Pos) ->
    NewI = rename_instr(I,convert_to_defs1(In,Ds),Eqv,Ds,L,Pos),
    Out = apply_transf(gen_kill_instr(L,Pos,I,Ds),In),
    [NewI|rename_block(Is,Out,Eqv,Ds,L,Pos+1)].

convert_to_defs1(In, { _, Ix_to_def,_ }) ->
  %convert_to_defs2(In,Ix_to_def,[]).
  gb_sets:fold( fun(Ix,Acc) -> {found,R} = hipe_hash:lookup(Ix,Ix_to_def), [R|Acc] end, [], In ).

%% convert_to_defs2([Ix|Is],Ix_to_def,Acc) ->
%%   {found,{L,Pos,X}} = hipe_hash:lookup(Ix,Ix_to_def),
%%   convert_to_defs2(Is,Ix_to_def,[{L,Pos,X}|Acc]);
%% convert_to_defs2([],_,Acc) -> Acc.


% You can probably merge def_renames + list comprehension into
% a single function; likewise for reach_uses + list comprehension.
%   At present, inelegant but works.

find( Index, Eqv ) ->
  gb_trees:get( Index, Eqv ).

newdefs([D|Defs],L,Pos,Ds,Eqv,Acc) ->
  newdefs(Defs,L,Pos,Ds,Eqv,
	  %% [{D, ?code:mk_var(hipe_ufind:only_find(index_of(L,Pos,D,Ds),Eqv))}
	  %% | Acc]);
	  [{D, ?code:mk_var(find(index_of(L,Pos,D,Ds),Eqv))} | Acc]);
newdefs([],_,_,_,_,Acc) ->
  Acc.

newuses(Us,L,Pos,In,Ds,Eqv) ->
  newuses(Us,L,Pos,In,Ds,Eqv,[]).

newuses([U|Us],L,Pos,In,Ds,Eqv,Acc)->
  case first_relevant_defs(U,In,Ds,Eqv) of
    none -> newuses(Us,L,Pos,In,Ds,Eqv,Acc);
    New -> newuses(Us,L,Pos,In,Ds,Eqv,[{U,New}|Acc])
  end;
newuses([],_,_,_,_,_,Acc) -> Acc.

first_relevant_defs(Use,[{L,Pos,Use}|Xs],Ds,Eqv) ->
  ?code:mk_var(find(index_of(L,Pos,Use,Ds),Eqv));
first_relevant_defs(Use,[_|Xs],Ds,Eqv) ->
  first_relevant_defs(Use,Xs,Ds,Eqv);
first_relevant_defs(_,[],_,_) -> none.

rename_instr(I,In,Eqv,Ds,L,Pos) ->
  Def_subst = newdefs(?code:defines(I),L,Pos,Ds,Eqv,[]),
%    Use_rs = reach_uses(?code:uses(I),L,Pos,In,Ds,Eqv),
%    Use_subst = [ {U, NewU} || {U,[{_,{rename,NewU}}|_]} <- Use_rs ],
  Use_subst = newuses(?code:uses(I),L,Pos,In,Ds,Eqv),
    NewI = ?code:subst_defines(Def_subst,?code:subst_uses(Use_subst,I)),
    NewI.




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
%  - gen_kill_instr( Label, Pos, Instr, Deftable ) => {set(index), set(index)}
%  - index_of( Label, Pos, Var, Deftable) => index
%  - def_of(Index, Deftable) => { Label, Pos }
%  - defvar_of(Index, Deftable) => { Label, Pos, Var }
%  - instr_of( Label, Pos, Deftable ) => Instr
%
%  Constructing the def table:
%  - empty_def_table()
%  - add_defs( Instr, Deftab, Label, Pos )
%

gen_kill_instr(L, Pos, Instr, Ds) ->
    Defs = ?code:defines(Instr),
    Gen = indices_of_defs(L,Pos,Defs,Ds),
    Kill = killed_by_defs(Defs,Ds),
    { Gen, Kill }.

%%%%%%%%%%%%%%%%%%%%
% Def table initially contains only the N parameters, numbered from 1..N

empty_def_table(Ps) ->
    {Ds,_} = add_ds(Ps,
		    param_pos(),
		    empty_def_table(),
		    param_label(),
		    start_def()),
    Ds.

empty_def_table() -> { hipe_hash:empty(), hipe_hash:empty(), hipe_hash:empty() }.

param_pos() -> start.

param_label() -> start.

start_def() -> 1.

%%%%%%%%%%%%%%%%%%%%

add_defs(Instr,Ds,L,Pos,N) ->
    Def_lst = ?code:defines(Instr),
    case Def_lst of
	[] ->
	    {Ds,N};
	_ ->
	    add_ds(Def_lst,Pos,Ds,L,N)
    end.

start_pos() -> 0.

add_ds([],Pos,Ds,L,N) -> {Ds,N};
add_ds([X|Xs],Pos,Ds,L,N) ->
    NewDs = map_def(X,L,Pos,N,Ds),
    add_ds(Xs,Pos,NewDs,L,N+1).

map_def(X,L,Pos,N,{ Def_to_ix, Ix_to_def, Kill_map }) ->
    Def_name = {L,Pos,X},
    { hipe_hash:insert(Def_name, N, Def_to_ix), 
      hipe_hash:insert(N, Def_name, Ix_to_def),
      add_kill(X,N,Kill_map)
    }.

indices_of_defs(L, Pos, Defs, Ds) -> 
    indices_of_defs(Defs, L, Pos, Ds, gb_sets:empty()).

indices_of_defs([], L, Pos, Ds, Set) -> Set;
indices_of_defs([X|Xs], L, Pos, Ds, Set) ->
    N = index_of(L,Pos,X,Ds),
    indices_of_defs(Xs, L, Pos, Ds, gb_sets:add(N,Set)).

index_of(L,Pos,X, { Def_to_ix, Ix_to_def, Kill_map }) ->
    {found,N} = hipe_hash:lookup({L,Pos,X},Def_to_ix),
    N.



killed_by_var(X, { Def_to_ix, Ix_to_def, Kill_map }) ->
    case hipe_hash:lookup(X,Kill_map) of
	not_found ->
	    gb_sets:empty();
	{found,Set} ->
	    Set
    end.

killed_by_defs(Xs,Ds) -> kills_of_defs(Xs,Ds,gb_sets:empty()).

kills_of_defs([],Ds,Set) -> Set;
kills_of_defs([X|Xs],Ds,Set) ->
    kills_of_defs(Xs,Ds,gb_sets:union(killed_by_var(X,Ds),Set)).

add_kill(X,N,Kill_map) ->
  N_set = gb_sets:add(N,gb_sets:empty()),
  NewSet =
    case hipe_hash:lookup(X,Kill_map) of
      not_found ->
	N_set;
      {found,OldSet} ->
	gb_sets:union(N_set,OldSet)
    end,
  hipe_hash:update(X,NewSet,Kill_map).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** TESTING STUFF ***


test12(CFG) ->
    {Ds,N} = name_defs(CFG),
    Transf = hipe_hash:init(rd_transf(?cfg:depth_first_ordering(CFG),CFG,Ds)),
    Succ = ?cfg:succ_map(CFG),
    {Start,In} = start_invals(CFG),
    {Ds,N,fix_all(Start,Succ,Transf,In)}.




