%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			   LAZY CODE MOTION
%
% Lazy code motion performs optimal partial redundancy elimination
% by a series of dataflow analyses.
%
% Presentation follows Muchnik's book.
%
% *** UNFINISHED ***
% - does not annotate program / perform code motions

-module(hipe_lcm).
-export([analyze/1,test/1]).
-compile(export_all).

-define(cfg,hipe_icode_cfg).
-define(code,hipe_icode).

-define(report(Str,Args),io:format(Str,Args)).
-define(error(Str,Args),error(Str,Args)).

% Univ: set of all expressions (= their indices), used for inverting sets
% PO, RPO: lists of {Label,Succ} or {Label,Preds} in postorder and reverse
%    postorder.
% TRANSloc: table of local transparency per block
% ANTloc:   table of local anticipatability per block
%
% Returns a list of {Label, Optimal_exprs, Redundant_exprs},
%   where the *_exprs are sets of expressions that are optimally
%   or redundantly placed in block.

analyze(CFG) ->
    Succs = ?cfg:succ_map(CFG),
    PO = [ {L,?cfg:succ(Succs,L)} || L <- ?cfg:postorder(CFG) ],
    Preds = ?cfg:pred_map(CFG),
    RPO = [ {L, ?cfg:pred(Preds,L)} || L <- ?cfg:reverse_postorder(CFG) ],
    {_,N} = ?cfg:label_range(CFG),
    EntryNodes = [ L || {L,[]} <- RPO ],
    ExitNodes = [ L || {L,[]} <- PO ],
    {Univ,TRANSloc,ANTloc,Exp} = local_analysis(N,CFG),
    Result = analyze(N,Univ,PO,RPO,EntryNodes,ExitNodes,TRANSloc,ANTloc),
    exp_translate(Result,CFG,Exp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_label([{L,_}|Ls]) ->
    max_l(L,Ls).

max_l(L,[]) -> L;
max_l(L0,[{L1,_}|Ls]) ->
    if
	L0 > L1 ->
	    max_l(L0,Ls);
	true ->
	    max_l(L1,Ls)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyze(N,Univ,PO,RPO,Entry,Exit,TRANSloc,ANTloc) ->
    ANTin = global_anticipation(N,PO,TRANSloc,ANTloc,Univ,Entry,Exit),
    EARLin = earliestness(N,RPO,TRANSloc,ANTin,Univ,Entry,Exit),
    ANEAin = anticip_earl(N,RPO,ANTin,EARLin,Univ,Entry,Exit),
    DELAYin = delayedness(N,ANEAin,ANTloc,Univ,Entry,Exit),
    LATEin = latest(N,PO,DELAYin,ANTloc,Univ,Entry,Exit),
    ISOLout = isolation(N,PO,LATEin,ANTloc,Univ,Entry,Exit),
    OPT = optimality(N,RPO,LATEin,ISOLout,Univ),
    REDN = redundant(N,RPO,ANTloc,LATEin,ISOLout,Univ),
    lcm_info(OPT,REDN).

% Display info legibly:

lcm_info([],[]) -> [];
lcm_info([{L,OPT}|Xs],[{L,REDN}|Ys]) ->
    [{L,
      {optimal,set:list(OPT)},{redundant,set:list(REDN)}}|lcm_info(Xs,Ys)].

%exp_translate(Blocks,CFG,Exp) ->
%    {Blocks, hash:list(hipe_expr_table:index_to_expr(Exp))};
exp_translate(Blocks,CFG,Exp) ->
    InvExp = hipe_expr_table:index_to_expr(Exp),
    [ {L, 
       {optimal, [ get_expr(InvExp,I) || I <- Opt ]},
       {redundant, [ get_expr(InvExp,I) || I <- Red ]},
       hipe_bb:code(?cfg:bb(CFG,L))
      }
     || {L,{optimal,Opt},{redundant,Red}} <- Blocks ].

get_expr(InvExp,I) ->
    hipe_expr_table:get_expr(InvExp,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Returns {Univ,TRANSloc,ANTloc}, where Univ is the universe of expressions,
%  TRANSloc maps (Block -> set(Expr)), ANTloc maps (Block -> set(Expr)),
%  where TRANS means the GLOBAL expressions not killed by block
%  and ANTloc the LOCAL expressions that can be moved to front of block

local_analysis(N,CFG) ->
    {NextExp,Exp,Kill,Names} = hipe_expr_table:cfg(CFG),
    GenKill = hipe_expr_table:genkill(CFG,Exp,Kill),
    Univ = set:universe(NextExp-1),
    TRANSloc = final(N, [ {L, transloc(Is,Univ)}
			 || {L,Is} <- GenKill ] ),
    ANTloc = final(N, [ {L, antloc(Is)}
		       || {L,Is} <- GenKill ]),
    {Univ,TRANSloc,ANTloc,Exp}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% Note: instructions have been converted into {Gen,Kill} pairs
%   prior to getting here.

% Note: traverses block front-to-back.

transloc(Is,Univ) ->
    set:diff(Univ,transloc_instrs(Is,set:empty())).

transloc_instrs([],OPAQUE) -> OPAQUE;
transloc_instrs([{G,K}|Xs],OPAQUE) ->
    transloc_instrs(Xs,set:union(OPAQUE,K)).

% Note: traverses block back-to-front

antloc([]) -> set:empty();
antloc([{G,K}|Xs]) ->
    ANT = antloc(Xs),
    set:union(G,set:diff(ANT,K)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following are full dataflow analyses:
%   ANTin, EARLin, DELAYin, ISOLout
%
% The following can be computed directly:
%   ANEAin, LATEin, OPT, REDN

% Global anticipation:
%
% Transfer function: ANTloc union (TRANSloc isect ANTout)
% Inval: intersection of successors
% Final: inval + transfer fun
%
% Start value: universe

global_anticipation(N,PO,TRANSloc,ANTloc,Univ,Entry,Exit) ->
    banner('ANTin~n'),
    Transf = [ {L, Succ, get(TRANSloc,L), get(ANTloc,L)}
	      || {L,Succ} <- PO ],
    InitOut = init_out(Exit,set:empty(),N,Univ),
    Out = g_fixpoint(Transf,InitOut),
    g_finalize(N,Transf,Out).

g_fixpoint(Bs,Out) ->
    g_fixpoint(1,Bs,Out).

g_fixpoint(N,Bs,Out) ->
    ?report('iteration ~p~n',[N]),
    case g_blocks(Bs,Out,unchanged) of
	{unchanged,NewOut} -> 
	    NewOut;
	{changed,NewOut} ->
	    g_fixpoint(N+1,Bs,NewOut)
    end.

g_blocks([],Out,Ch) -> {Ch,Out};
g_blocks([B|Bs],Out,Ch) ->
    {NewCh,NewOut} = g_block(B,Out,Ch),
    g_blocks(Bs,NewOut,NewCh).

g_block({L,Succ,TRANSloc,ANTloc},Out,Ch) ->
    In = set:intersect_list( [ outval(Out,S) || S <- Succ ]),
    Old = outval(Out,L),
    New = set:union(ANTloc,set:intersect(TRANSloc,In)),
    check_changed(L,Old,New,Out,Ch).

g_finalize(N,Bs,Out) ->
    final(N,[ 'ANTin'(B,Out) || B <- Bs ]).

'ANTin'({L,Succ,TRANSloc,ANTloc},Out) ->
    In = set:intersect_list( [ outval(Out,S) || S <- Succ ]),
    Final = set:union(ANTloc,set:intersect(TRANSloc,In)),
    {L, Final}.

% Earliestness:
%
% Transfer function: inv(TRANSloc) union (inv(ANTin) isect EARLin)
% Inval: union of predecessors
% Final: inval
%
% Start value: empty set

earliestness(N,RPO,TRANSloc,ANTin,Univ,Entry,Exit) ->
    banner('EARLin~n'),
    Transf = [ {L, Preds, 
		set:diff(Univ,get(TRANSloc,L)),
		set:diff(Univ,get(ANTin,L))}
	      || {L,Preds} <- RPO ],
    InitOut = init_out(Entry,Univ,N,set:empty()),
    Out = e_fixpoint(Transf,InitOut),
    e_finalize(N,Transf,Out).

e_fixpoint(Bs,Out) ->
    e_fixpoint(1,Bs,Out).

e_fixpoint(N,Bs,Out) ->
    ?report('iteration ~p~n',[N]),
    case e_blocks(Bs,Out,unchanged) of
	{unchanged,NewOut} -> 
	    NewOut;
	{changed,NewOut} ->
	    e_fixpoint(N+1,Bs,NewOut)
    end.

e_blocks([],Out,Ch) -> {Ch,Out};
e_blocks([B|Bs],Out,Ch) ->
    {NewCh,NewOut} = e_block(B,Out,Ch),
    e_blocks(Bs,NewOut,NewCh).

e_block({L,Preds,InvTRANSloc,InvANTin},Out,Ch) ->
    In = set:union_list( [ outval(Out,P) || P <- Preds ]),
    Old = outval(Out,L),
    New = set:union(InvTRANSloc,
		    set:intersect(InvANTin,In)),
    check_changed(L,Old,New,Out,Ch).

e_finalize(N,Bs,Out) ->
    final(N,[ 'EARLin'(B,Out) || B <- Bs ]).

'EARLin'({L,Preds,InvTRANSloc,InvANTin},Out) ->
    {L, set:union_list( [ outval(Out,P) || P <- Preds ]) }.

% Anticipated-Earliest:
%
% For each block,
%    ANEAin = ANTin union EARLin

anticip_earl(N,RPO,ANTin,EARLin,Univ,Entry,Exit) ->
    banner('ANEAin~n'),
    [ {L, Preds, set:union(get(ANTin,L),get(EARLin,L))}
     || {L,Preds} <- RPO ].

% Delayedness:
%
% Transfer function: inv(ANTloc) isect DELAYin
% Inval: (isect of predecessors) union ANEAin
% Final: invalue
%
% Start value: universe
%
% Note that ANEAin is in reverse post order, which is what we want here.

delayedness(N,ANEAin,ANTloc,Univ,Entry,Exit) ->
    banner('DELAYin~n'),
    Transf = [ {L, Preds, ANEA, set:diff(Univ,get(ANTloc,L))}
	      || {L,Preds,ANEA} <- ANEAin ],
    % Requires a slightly nonstandard initialization:
    InitOut = find_anea(Entry,Transf,init_out([],Univ,N,Univ)),
    Out = d_fixpoint(Transf,InitOut),
    d_finalize(N,Transf,Out).

%%%%%%%%%%%%%%%%%%%%
%
% Initialize all entry blocks as ANEAin

find_anea([],Transf,Out) -> Out;
find_anea([L|Ls],Transf,Out) ->
    init_block(L,block_anea(L,Transf),find_anea(Ls,Transf,Out)).

block_anea(L,[]) -> ?error('block ~p has no ANEA value~n',[L]);
block_anea(L,[{L,_,ANEA,_}|Xs]) -> ANEA;
block_anea(L,[_|Xs]) -> block_anea(L,Xs).

%%%%%%%%%%%%%%%%%%%%

d_fixpoint(Bs,Out) ->
    d_fixpoint(1,Bs,Out).

d_fixpoint(N,Bs,Out) ->
    ?report('iteration ~p~n',[N]),
    case d_blocks(Bs,Out,unchanged) of
	{unchanged,NewOut} -> 
	    NewOut;
	{changed,NewOut} ->
	    d_fixpoint(N+1,Bs,NewOut)
    end.

d_blocks([],Out,Ch) -> {Ch,Out};
d_blocks([B|Bs],Out,Ch) ->
    {NewCh,NewOut} = d_block(B,Out,Ch),
    d_blocks(Bs,NewOut,NewCh).

d_block({L,Preds,ANEA,InvANTloc},Out,Ch) ->
    In = set:union(ANEA, set:intersect_list( [ outval(Out,P) || P <- Preds ])),
    Old = outval(Out,L),
    New = set:intersect(InvANTloc,In),
    check_changed(L,Old,New,Out,Ch).

d_finalize(N,Bs,Out) ->
    final(N,[ 'DELAYin'(B,Out) || B <- Bs ]).

'DELAYin'({L,Preds,ANEA,InvANTloc},Out) ->
    {L,
     set:union(ANEA, set:intersect_list( [ outval(Out,P) || P <- Preds ]))
    }.

% Lateness:
%
% For each block,
%   DELAYin isect (inv(ANTloc) union inv(isect_all_preds DELAYin))

latest(N,PO,DELAYin,ANTloc,Univ,Entry,Exit) ->
    banner('LATEin~n'),
    Transf = [ {L,
		set:intersect(get(DELAYin,L),
			      set:diff(Univ,
				       set:intersect_list(
					     [ get(DELAYin,S) || S <- Succ ])
				      )
			     )}
	      || {L,Succ} <- PO ],
    final(N,Transf).

% Isolation:
%
% Transfer function: LATEin union (inv(ANTloc) isect ISOLout)
% Invalue: isect of successors => ISOLout
% Final: invalue
%
% Start value: universe

isolation(N,PO,LATEin,ANTloc,Univ,Entry,Exit) ->
    banner('ISOLout~n'),
    Transf = [ {L,Succ,get(LATEin,L),set:diff(Univ,get(ANTloc,L))}
	      || {L,Succ} <- PO ],
    InitOut = init_out(Exit,set:empty(),N,Univ),
    Out = i_fixpoint(Transf,InitOut),
    i_finalize(N,Transf,Out).

i_fixpoint(Bs,Out) ->
    i_fixpoint(1,Bs,Out).

i_fixpoint(N,Bs,Out) ->
    ?report('iteration ~p~n',[N]),
    case i_blocks(Bs,Out,unchanged) of
	{unchanged,NewOut} -> 
	    NewOut;
	{changed,NewOut} ->
	    i_fixpoint(N+1,Bs,NewOut)
    end.

i_blocks([],Out,Ch) -> {Ch,Out};
i_blocks([B|Bs],Out,Ch) ->
    {NewCh,NewOut} = i_block(B,Out,Ch),
    i_blocks(Bs,NewOut,NewCh).

i_block({L,Succ,LATEin,InvANTloc},Out,Ch) ->
    In = set:intersect_list( [ outval(Out,S) || S <- Succ ] ),
    Old = outval(Out,L),
    New = set:union(LATEin, set:intersect(InvANTloc, In)),
    check_changed(L,Old,New,Out,Ch).

i_finalize(N,Bs,Out) ->
    final(N,[ 'ISOLout'(B,Out) || B <- Bs ]).

'ISOLout'({L,Succ,LATEin,InvANTloc},Out) ->
    {L, set:intersect_list( [  outval(Out,S) || S <- Succ ] ) }.

% Optimality:
%
% For each block, LATEin isect inv(ISOLout)

optimality(N,RPO,LATEin,ISOLout,Univ) ->
    [ {L, set:intersect(get(LATEin,L),set:diff(Univ,get(ISOLout,L)))}
     || {L,Preds} <- RPO ].

% Redundancy:
%
% For each block, ANTloc isect inv(LATEin union ISOLout)

redundant(N,RPO,ANTloc,LATEin,ISOLout,Univ) ->
    [ {L, set:intersect(get(ANTloc,L),
			set:diff(Univ,
				 set:union(get(LATEin,L),get(ISOLout,L))))}
     || {L,Preds} <- RPO ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This is done in all dataflow analyses, and so we factor it out.

check_changed(L,Old,New,Out,Ch) ->
    if
	Old =:= New ->
	    {Ch,Out};
	true ->
	    {changed,set_outval(Out,L,New)}
    end.

init_out(Special_blocks,Special_val,NumBlocks,Default) ->
    set_each(Special_blocks,Special_val,init_out(NumBlocks,Default)).

init_out(N,Init) ->
    hipe_vectors:init(N,Init).

init_block(L,V,Out) ->
    hipe_vectors:set(Out,L,V).

set_outval(Out,L,New) ->
    hipe_vectors:set(Out,L,New).

outval(Out,L) ->
    hipe_vectors:get(Out,L).

list_out(Out) ->
    [ {L, set:list(OutVal)} || {L,OutVal} <- hipe_vectors:list(Out) ].

final(N,FinalVals) ->
    set_all(FinalVals,hipe_vectors:empty(N)).

set_all([],V) -> V;
set_all([{Ix,Val}|Xs],V) ->
    set_all(Xs,hipe_vectors:set(V,Ix,Val)).

set_each([],V,Out) -> Out;
set_each([L|Ls],V,Out) ->
    set_each(Ls,V,hipe_vectors:set(Out,L,V)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(Table,Ix) -> hipe_vectors:get(Table,Ix).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_tab(Tab) ->
    hipe_vectors:list(Tab).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

banner(Msg) ->
    io:format('~30c ~w ~30c~n',[$-,Msg,$-]).

error(Str,Msg) ->
    io:format('ERROR: '),
    io:format(Str,Msg),
    exit({Str,Msg}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** TESTING ***

local(CFG) ->
    {Univ,TRANSloc,ANTloc} = local_analysis(max_list(?cfg:labels(CFG)),CFG),
    {Univ,list_tab(TRANSloc),list_tab(ANTloc)}.

max_list([L|Ls]) -> max_lst(L,Ls).

max_lst(L,[]) -> L;
max_lst(L0,[L1|Ls]) ->
    if
	L0 > L1 ->
	    max_lst(L0,Ls);
	true ->
	    max_lst(L1,Ls)
    end.

test(MFA) ->
    analyze(hipe_icode_cfg:init(translate:translate(MFA))).
