%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	     BASIC BLOCK LEVEL BACKWARD COPY PROPAGATION
%
% Eliminates copy instructions along the following lines:
%   x = a+b; y = x; use y (x dead)
% rewritten it into:
%   y = a+b; use y
%
% This is more difficult than forward copy propagation because of
% the following:
% - we traverse the block back-to-front
% - when we encounter a copy "y=x" we do NOT know if it should remain
% - so we have to keep track of which moves should go away and then
%   delete them explicitly.

-module(hipe_bwd_cprop).
-export([cfg/1]).

%-define(debug(Str,Args),io:format(Str,Args)).
-define(debug(Str,Args),ok).

% At present only for Icode.
% Could probably be done on RTL/Sparc as well (though for Sparc,
% we can't eliminate some copies that refer to precolored registers)

-define(cfg,hipe_icode_cfg).
-define(liveness,hipe_icode_liveness).

cfg(CFG) ->
    Live = ?liveness:analyze(CFG),
    cprop_all(?cfg:labels(CFG),CFG,Live).

cprop_all([],CFG,Live) -> CFG;
cprop_all([L|Ls],CFG,Live) ->
    cprop_all(Ls,cprop_one(CFG,L,Live),Live).

cprop_one(CFG,L,Live) ->
    Code = hipe_bb:code(?cfg:bb(CFG,L)),
    LiveOut = ?liveness:liveout(Live,L),
    NewBB = hipe_bb:mk_bb(cprop(Code,LiveOut)),
    ?cfg:bb_update(CFG,L,NewBB).

% Three phases:
% - assign an index to each instruction
% - perform copy propagation, collecting the rewritten code + moves to delete
% - delete the now-dead moves

cprop(Code,LiveOut) ->
    { TmpCode, DefDead, PossDead, _ } = bwd_cprop(index(Code,1),LiveOut),
    Delible = concat_dead(DefDead, PossDead),
    ?debug('delible instructions: ~p~n',[Delible]),
    remove_delible(TmpCode, Delible).

index([],N) -> [];
index([I|Is],N) ->
    [{N,I}|index(Is,N+1)].

concat_dead(A,B) -> A.

%concat_dead(A,B) -> 
%    A ++ [ N || {N,D,NewD} <- B ].

% Consider the sequence:
%
%    x = e ; A ; y = x ; B
%
% A and B denote sequences of instructions.
% Given that
% - x is dead in B
% - x and y are not defined or used in A
% we can, at the point where "x = e" is found, mark "y = x" as deleted.

bwd_cprop([],LiveOut) -> { [], [], [], LiveOut };
bwd_cprop([{N,I}|Is],LiveOut) ->
    { NewIs, DefDead, PossDead, Live } = bwd_cprop(Is,LiveOut),
    prop_i(N,I,NewIs,DefDead,PossDead,Live).

% must be rewritten for each intermediate format:

prop_i(Index,Instr,RestIs,DefDead,PossDead,Live) ->
    Defs = hipe_icode:defines(Instr),
    Uses = hipe_icode:uses(Instr),
    NowLive = union(Uses,diff(Live,Defs)),
    case hipe_icode:type(Instr) of
	mov ->
	    Y = hipe_icode:mov_dst(Instr),
	    X = hipe_icode:mov_src(Instr),
	    case hipe_icode:is_const(X) of
		true ->  % uninteresting; treat like normal instruction
		    ?debug('normal ~p~n',[Instr]),
		    {NewI,NewDefDead,TmpPossDead} = 
			rename_instr(Instr,DefDead,PossDead),
		    NewPossDead = delete_refs(TmpPossDead,Uses),
		    { [{Index,NewI}|RestIs], NewDefDead, NewPossDead, NowLive};
		false -> % copy instruction "y = x"
		    % if x is in Live, then we can do nothing
		    % otherwise, add {N,Y,X} to PossDead
		    % apart from this, treat just like other instrs
		    ?debug('mov ~p~n',[Instr]),
		    {NewI,NewDefDead,TmpPossDead} = 
			rename_instr(Instr,DefDead,PossDead),
		    ?debug('is ~p in live set ~p?~n',[Y,Live]),
		    TmpPossDead2 =
			case lists:member(X,Live) of
			    true ->
				?debug('- mov unsuitable for bwd cprop~n',[]),
				TmpPossDead;
			    false ->
				?debug('- mov added for bwd cprop~n',[]),
				[{Index,Y,X}|TmpPossDead]
			end,
		    NewPossDead = delete_refs(TmpPossDead2,Uses),
		    { [{Index,NewI}|RestIs], NewDefDead, NewPossDead, NowLive}
	    end;
	_ ->
	    ?debug('normal ~p~n',[Instr]),
	    {NewI,NewDefDead,TmpPossDead} = 
		rename_instr(Instr,DefDead,PossDead),
	    NewPossDead = delete_refs(TmpPossDead,Uses),
	    { [{Index,NewI}|RestIs], NewDefDead, NewPossDead, NowLive }
    end.

% *** UNFINISHED ***
% what was that correctness condition again?

delete_refs(PossDead,Uses) ->
    PossDead;
delete_refs(PossDead,Uses) ->
    [ {N,Y,X} 
     || {N,Y,X} <- PossDead, not lists:member(X,Uses), not lists:member(Y,Uses)
    ].

% We rename the definitions of Instr: if we are considering definition D
% and find {N,D,NewD} in PossDead, we (a) rename D into NewD and (b)
% move N into DefDead.
%
% Returns { NewInstr, NewDefDead, NewPossDead }

rename_instr(Instr,DefDead,PossDead) ->
    {Subst,NewDefDead,NewPossDead} = rename_defs(hipe_icode:defines(Instr),
						 DefDead, PossDead,[]),
    { hipe_icode:subst_defines(Subst,Instr), NewDefDead, NewPossDead }.

rename_defs([],DefDead, PossDead, Subst) -> { Subst, DefDead, PossDead };
rename_defs([D|Ds],DefDead,PossDead,Subst) ->
    case rename_def(D,PossDead) of
	no ->
	    rename_defs(Ds,DefDead,PossDead,Subst);
	{yes,N,NewD,NewPossDead} ->
	    ?debug('renamed ~p -> ~p~n',[D,NewD]),
	    rename_defs(Ds,[N|DefDead],NewPossDead,[{D,NewD}|Subst])
    end.

% The following _is_ quite clumsy. How can we make it faster? catch/throw?

rename_def(D,PossDead) ->
    ?debug('try ~p in ~p~n',[D,PossDead]),
    rename_def(PossDead,D,[]).

rename_def([],D,NewPossDead) -> no;
rename_def([{N,NewD,D}|Xs],D,PossD) ->
    {yes,N,NewD, PossD ++ Xs };
rename_def([X|Xs],D,PossD) ->
    rename_def(Xs,D,[X|PossD]).

% The moves to be deleted are found in list Delible.
% For all others, simply remove the index given previously.

remove_delible([],Delible) -> [];
remove_delible([{N,I}|Xs],Delible) ->
    case lists:member(N,Delible) of
	true ->
	    [hipe_icode:mk_comment({bwd_cprop,I})|remove_delible(Xs,Delible)];
	false ->
	    [I|remove_delible(Xs,Delible)]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

union(Xs,Ys) -> Xs ++ Ys.

diff([],Ys) -> [];
diff([X|Xs],Ys) ->
    case lists:member(X,Ys) of
	true ->
	    diff(Xs,Ys);
	false ->
	    [X|diff(Xs,Ys)]
    end.
