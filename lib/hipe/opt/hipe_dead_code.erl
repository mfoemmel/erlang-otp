%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			DEAD CODE ELIMINATION
%
% Runs one pass of dead code elimination. Note that we should iterate
% this to eliminate all dead code.
%
% Another approach is to employ use-def chains + marking, which is
% probably a lot faster. Still, this is simple enough and might come
% in useful. If we can save a lot of time, rewrite it to the use-def
% algorithm.

-module(hipe_dead_code).
-export([eliminate/1]).

-define(cfg,hipe_icode_cfg).
-define(liveness,hipe_icode_liveness).
-define(code,hipe_icode).

eliminate(CFG) ->
    dce_blocks(?cfg:labels(CFG),?liveness:analyze(CFG),CFG).

dce_blocks([],Live,CFG) -> CFG;
dce_blocks([L|Ls],Live,CFG) ->
    dce_blocks(Ls,Live,
	       dce_block(L,?cfg:bb(CFG,L),?liveness:liveout(Live,L),CFG)).

dce_block(L,BB,LiveOut,CFG) ->
    {NewXs,_} = dce_instrs(hipe_bb:code(BB),LiveOut),
    ?cfg:bb_update(CFG,L,hipe_bb:mk_bb(NewXs)).

dce_instrs([],LiveOut) -> {[],LiveOut};
dce_instrs([X|Xs],LiveOut) ->
    {NewXs,Live} = dce_instrs(Xs,LiveOut),
    case {eliminable(X), dead(X,Live)} of
       {true, true} ->
	  {NewXs,Live};
       _ ->
	  {[X|NewXs],live_vars(X,Live)}
    end.

live_vars(X,Live) ->
    {D,U} = def_use(X),
    union(U,diff(Live,D)).

dead(X,Live) ->
    {D,U} = def_use(X),
    case intersect(D,Live) of
	[] ->
	    true;
	[_|_] ->
	    false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Here, we would like to use real sets.
% - convert liveness to use set-module
% - convert def_use to return sets

union([],Ys) -> Ys;
union([X|Xs],Ys) ->
    case member(X,Ys) of 
       true ->
	  union(Xs,Ys);
       false ->
	  [X|union(Xs,Ys)]
    end.

diff([],Ys) -> [];
diff([X|Xs],Ys) ->
    case member(X,Ys) of
       true ->
	  diff(Xs,Ys);
       false ->
	  [X|diff(Xs,Ys)]
    end.

intersect([],Ys) -> [];
intersect([X|Xs],Ys) ->
   case member(X,Ys) of 
      true ->
	 [X|intersect(Xs,Ys)];
      false ->
	 intersect(Xs,Ys)
   end.

member(X,[]) -> false;
member(X,[X|_]) -> true;
member(X,[_|Xs]) -> member(X,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** PORTING ***

def_use(X) ->
    { ?code:defines(X), ?code:uses(X) }.

eliminable(X) ->
  case hipe_icode:type(X) of
    mov ->
      true;
    _ -> false
  end.

