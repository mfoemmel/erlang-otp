%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_prop_env.erl
%%  Module   :	hipe_sparc_prop_env
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-12-16 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2004/06/22 10:14:02 $
%%              $Revision: 1.7 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_prop_env).
-export([bind/3, bind_hpos/3, genv__changed/1, env__sp/1, env__hp/1, inc_hp/2, inc_sp/2,
	 genv__changed_clear/1, end_of_bb/1, genv__env/1, find_hpos/2, find_spos/2,
	 kill/2, kill_all/2, kill_hp/1, kill_phys_regs/1, kill_sp/1,
	 kill_uses/2, lookup/2, new_genv/1, set_active_block/2, succ/1,
	 genv__env_update/2, zap_heap/1, zap_stack/1, bind_spos/3]).
-ifdef(SPARC_PROP_ENV_DEGUG).
-export([pp_lenv/1]).
-endif.
-include("../main/hipe.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Global environment.
%% Block    - current BB
%% CFG      - The cfg
%% succ_map - Successors map
%% pred_map - Predecessors map
%% env      - Current local environment
%% envs     - All BBs environments
%% changed  - Flag indicating that a change has taken place.
-record(genv,{block,cfg,succ_map,pred_map,env,envs,changed}).
new_genv(CFG)->  
  #genv{cfg=CFG,
	succ_map=hipe_sparc_cfg:succ_map(CFG),
	pred_map=hipe_sparc_cfg:pred_map(CFG),
	env=env__mk_new(),
	envs=empty(), 
	changed=false
       }.

genv__block(#genv{block=Block}) -> Block.
genv__succ_map(#genv{succ_map=SuccMap}) -> SuccMap.
genv__pred_map(#genv{pred_map=PredMap}) -> PredMap.
genv__env(#genv{env=Env}) -> Env.
genv__envs(#genv{envs=Envs}) -> Envs.
genv__changed(#genv{changed=Changed}) -> Changed.

genv__env_update(Env,GEnv) -> GEnv#genv{env=Env}.
genv__changed_clear(GEnv) -> GEnv#genv{changed=false}.

env(L,GEnv) ->
  case plookup(L, genv__envs(GEnv)) of
    undefined -> env__mk_new();
    E -> E
  end.
      
preds(L,GEnv) ->
  hipe_sparc_cfg:pred(genv__pred_map(GEnv),L).

succ(GEnv) ->
  hipe_sparc_cfg:succ(genv__succ_map(GEnv), genv__block(GEnv)).

end_of_bb(GEnv) ->
  Env = genv__env(GEnv),
  L = genv__block(GEnv),
  Envs = genv__envs(GEnv),
  %% XXX: Change this to a more efficent test!
  case plookup(L,Envs) of
    Env -> GEnv;
    _ ->
      GEnv#genv{envs=enter(L, Env, Envs), changed=true}
  end.

set_active_block(L, GEnv) ->
  case preds(L,GEnv) of
    [Pred] ->
      GEnv#genv{block=L, env=env(Pred,GEnv)};
    [Pred|Preds] ->
      case plookup(Pred, genv__envs(GEnv)) of
	undefined -> GEnv#genv{block=L, env=env__mk_new()};
	E ->
	  Env = join(Preds,E, GEnv),
	  GEnv#genv{block=L, env=Env}
      end;
    _ ->
      GEnv#genv{block=L, env=env__mk_new()}
  end.

join([L|Ls], Env, GEnv) ->
%%  io:format("Joining: L:~w\n",[L]),
  case plookup(L, genv__envs(GEnv)) of
    undefined -> env__mk_new();
    E ->
  %%    pp_env(E),
      join(Ls,merge(E,Env), GEnv)
  end;
join([], Env, _Genv) ->
  %% io:format("Join:\n"),
  %% pp_env(Env),
  Env.

%% ____________________________________________________________________
%% 
%% The local environment
%% {Regs, SP, HP, Stack, Heap}
%%
-record(env,{regs,sp,hp,stack,heap}).
env__mk_new()->
    #env{regs=empty(),
	 sp=0,
	 hp=0,
	 stack=empty(),
	 heap=empty()
	}.
env__regs(#env{regs=Regs}) -> Regs.
env__sp(#env{sp=SP}) -> SP.
env__hp(#env{hp=HP}) -> HP.
env__stack(#env{stack=Stack}) -> Stack.
env__heap(#env{heap=Heap}) -> Heap.

merge(E1, E2) ->
  Env0 = (env__mk_new())#env{regs=m(env__regs(E1),env__regs(E2))},
  Env1 = 
    case env__sp(E1) =:= env__sp(E2) of
      true ->
	Stack = m(env__stack(E1),env__stack(E2)),
	Env0#env{sp=env__sp(E1),stack=Stack};
      false -> Env0
    end,
  Env2 = 
    case env__hp(E1) =:= env__hp(E2) of
      true ->
	Heap = m(env__heap(E1),env__heap(E2)),
	Env1#env{hp=env__hp(E1),heap=Heap};
      false -> Env1
    end,
  Env2.
    
m(R1,R2) ->
  I1 = iterator(R1),
  F = fun(X, V, Acc) ->
	  case find(X, R2) of
	    {value, V} ->
	      enter(X, V, Acc);
	    _ -> Acc
	  end
      end,
  fold(F, I1, empty()).


-ifdef(SPARC_PROP_ENV_DEGUG).
pp_lenv(GEnv) ->
  Env = env(GEnv),
  pp_env(Env).

pp_env(Env) ->
  pp_regs(to_list(env__regs(Env))),
  pp_mem(env__sp(Env),to_list(env__stack(Env)),"STACK"),
  pp_mem(env__hp(Env),to_list(env__heap(Env))," HEAP").

pp_regs([{X,Y}|Regs]) ->
  case hipe_sparc:is_reg(X) of
    false ->
      ?EXIT({bad_reg_in_environment,{X,Y}});
    true ->
      hipe_sparc_pp:pp_arg(standard_io,X),
      io:format(" <- "),
      hipe_sparc_pp:pp_arg(standard_io,Y),
      io:format("\n"),
      pp_regs(Regs)
  end;
pp_regs([]) -> ok.

pp_mem(unknown,_,_) ->
  ok;
pp_mem(_,[],_) ->
  ok;
pp_mem(Pointer,Mem,What) ->
  io:format("~s:\n",[What]),
  case lists:sort(Mem) of
    [] -> ok;
    [First|_] = Slots ->
      pp_slots(min(element(1,First),Pointer),Pointer,Slots,What),
      io:format("               |-----|\n")
  end.

min(X,Y) when X =< Y ->
  X;
min(_,Y) ->
  Y.

pp_slots(Cur, Pointer, Slots = [{Next,V}|Rest], What) ->
  if Cur =:= Pointer ->
      io:format("~s_P -> ",[What]);
     true ->
      io:format("           ")
  end,
  io:format("~3w | ",[Cur]),
  if Cur =:= Next ->
      hipe_sparc_pp:pp_arg(standard_io, V),
      io:format(" |\n"),
      pp_slots(Cur+4, Pointer, Rest, What);
     true ->
      io:format("           |\n"),
      pp_slots(Cur+4, Pointer, Slots, What)
  end;
pp_slots(Cur,Pointer,[],What) ->
  if Cur =< Pointer ->
      pp_empty_slots(Cur,Pointer,What);
     true ->
      ok
  end.

pp_empty_slots(Cur,Pointer,What) ->
  if Cur =:= Pointer ->
      io:format("~s_P -> ~3w |      |\n",[What,Cur]);
     Cur > Pointer ->
      ?EXIT(bad_env);
     true ->
      io:format("           ~3w |      |\n",[Cur]),
      pp_empty_slots(Cur+4,Pointer,What)
  end.
-endif.


zap_stack(Env) -> Env#env{stack=empty()}.
zap_heap(Env) -> Env#env{heap=empty()}.
kill_sp(Env) -> Env#env{sp=unknown}.
kill_hp(Env) -> Env#env{hp=unknown}.
kill_phys_regs(Env)->
  R = env__regs(Env),
  I1 = iterator(R),
  F = fun(X, V, Acc) ->
	  case not_physical(X) andalso not_physical(V) of
	    true ->
	      Acc;
	    false ->
	      delete(X, Acc)
	  end
      end,

  Env0 = Env#env{regs=fold(F, I1, R)},
  F2 = fun(X, V, Acc) ->
	   case not_physical(V) of
	     true ->
	       Acc;
	     false ->
	       delete(X, Acc)
	   end
       end,
  S = env__stack(Env0),
  I2 = iterator(S),
  Env1 = Env0#env{stack=fold(F2, I2, S)},
  H = env__heap(Env1),
  I3 = iterator(H),
  Env2 = Env1#env{heap=fold(F2, I3, H)},
  Env2.

not_physical(R) ->
  case hipe_sparc:is_reg(R) of
    false -> true;
    true ->
      not hipe_sparc_registers:is_precoloured(hipe_sparc:reg_nr(R))
  end.

inc_sp(Env, Val) ->
  case env__sp(Env) of
    unknown ->
      Env;
    PrevVal ->
      Env1 = Env#env{sp=PrevVal+Val},
      case Val < 0 of
	false -> Env1;
	true -> %% SP is decreased, forget pos above.
	  %% XXX: Fix if stack direction is changed.

	  Env1#env{stack=
		   lists:foldl(fun delete/2,env__stack(Env1), 
			       [Pos   || Pos <- lists:seq(env__sp(Env1),env__sp(Env),4),
					 find(Pos,env__stack(Env1)) =/= none])}
      end
  end.
inc_hp(Env, Val) ->  
  case env__hp(Env) of
    unknown ->
      Env;
    PrevVal ->
      Env#env{hp=PrevVal+Val}
  end.


%% Bind a value/reg to a stack position
bind_spos(Pos, Val, Env) ->
  Env#env{stack=enter(Pos,Val,env__stack(Env))}.


%% Bind a value/reg to a heap position
bind_hpos(Pos, Val, Env) ->
  Env#env{heap=enter(Pos,Val,env__heap(Env))}.

%% Find a value on a stackpos
find_spos(Pos, Env) ->
  plookup(Pos, env__stack(Env)).

%% Find a value on a heappos
find_hpos(Pos, Env) ->
  plookup(Pos, env__heap(Env)).

%%
%% Find what memory pos P is bound to.
%% As a last resort undefined is returned.
%%
plookup(P, Map) ->
  case find(P,Map) of
    {value,V} -> V;
    _ ->  undefined
  end.


%%
%% Bind reg X to Y in Map
%%

bind(Map, X, Y) ->
  Regs = env__regs(Map),
  Map#env{regs=enter(X,Y,Regs)}.

%%
%% Find what X is bound to (as a last restort varaibles are bound 
%% to themself).
%%

lookup(X, Env) ->
  case find(X, env__regs(Env)) of
    {value, V} -> V;
    _ -> X
  end.

%%
%% Kill bindings with references to register X
%%

kill_all(Xs, Env)->
  lists:foldl(fun kill/2,Env,Xs).
kill(X, Env0)->
  Env1 = Env0#env{regs=mkill(X,env__regs(Env0))},
  Env2 = Env1#env{stack=mkill(X,env__stack(Env1))},
  Env3 = Env2#env{heap=mkill(X,env__heap(Env2))},
  Env3.

mkill(X, T) ->
  I1 = iterator(T),
  F = fun(K, V, Acc) ->
	  if 
	    K =:= X -> delete(K, Acc);
	    V =:= X -> delete(K, Acc);
	    true -> Acc
	  end
      end,
  fold(F,I1,T).

kill_use(X, T) ->
  I1 = iterator(T),
  F = fun(K, V, Acc) ->
	  if
	    V =:= X -> delete(K, Acc);
	    true -> Acc
	  end
      end,
  fold(F,I1,T).

kill_uses([X|Xs],Env0) ->
  Env1 = Env0#env{regs=kill_use(X,env__regs(Env0))},
  Env2 = Env1#env{stack=kill_use(X,env__stack(Env1))},
  Env3 = Env2#env{heap=kill_use(X,env__heap(Env2))},
  kill_uses(Xs,Env3);
kill_uses([], Env) -> Env.

%% ____________________________________________________________________
%% The mapping datastructure.

empty() ->
  gb_trees:empty().

enter(X, V, T) ->
  gb_trees:enter(X, V, T).

iterator(T) ->
  gb_trees:iterator(T).

next(S) ->
  gb_trees:next(S).

find(X, T)->
  gb_trees:lookup(X, T).

-ifdef(SPARC_PROP_ENV_DEGUG).
to_list(T) ->
  gb_trees:to_list(T).
-endif.

delete(X, T) ->
  gb_trees:delete(X, T).

fold(F,I,T) ->
  case next(I) of 
    none -> T;
    {K,V,I2} ->
      fold(F,I2,F(K,V,T))
  end.
