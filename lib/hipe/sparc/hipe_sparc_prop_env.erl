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
%%              $Author: richardc $
%%              $Date: 2002/05/14 13:32:30 $
%%              $Revision: 1.4 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_prop_env).
-export([bind/3,bind_hpos/3,changed/1, sp/1, hp/1, inc_hp/2, inc_sp/2,
	 clear_changed/1,end_of_bb/1, env/1, find_hpos/2, find_spos/2,
	 kill/2, kill_all/2, kill_hp/1, kill_phys_regs/1, kill_sp/1,
	 kill_uses/2, lookup/2, new_genv/1, set_active_block/2, succ/1,
	 set_env/2, zap_heap/1, zap_stack/1, bind_spos/3, pp_lenv/1]).
-include("../main/hipe.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Global environment.
%% Block      - current BB
%% CFG       - The cfg
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
	env=new_env(),
	envs=empty(), 
	changed=false
       }.
set_env(Env,GEnv) ->
  GEnv#genv{env=Env}.
env(GEnv) ->
  GEnv#genv.env.
env(L,GEnv) ->
  case plookup(L, GEnv#genv.envs) of
    undefined ->new_env();
    E -> E
  end.
      
preds(L,GEnv) ->
  hipe_sparc_cfg:pred(GEnv#genv.pred_map,L).

succ(GEnv) ->
  hipe_sparc_cfg:succ(GEnv#genv.succ_map, GEnv#genv.block).

end_of_bb(GEnv) ->
  Env = GEnv#genv.env,
  L = GEnv#genv.block,
  Envs =  GEnv#genv.envs,
  %% XXX: Change this to a more efficent test!
  case plookup(L,Envs) of
    Env -> GEnv;
    _ ->
      GEnv#genv{envs=enter(L, Env, Envs), changed=true}
  end.

changed(GEnv) ->
   GEnv#genv.changed.

clear_changed(GEnv) ->
   GEnv#genv{changed=false}.


set_active_block(L, GEnv) ->
  case preds(L,GEnv) of
    [Pred] ->
      GEnv#genv{block=L, env=env(Pred,GEnv)};
    [Pred|Preds] ->
      case plookup(Pred, GEnv#genv.envs) of
	undefined -> GEnv#genv{block=L, env=new_env()};
	E ->
	  Env = join(Preds,E, GEnv),
	  GEnv#genv{block=L, env=Env}
      end;
    _ ->
      GEnv#genv{block=L, env=new_env()}
  end.

join([L|Ls], Env, GEnv) ->
%%  io:format("Joining: L:~w\n",[L]),
  case plookup(L, GEnv#genv.envs) of
    undefined -> new_env();
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
-record(env,{regs,sp,hp,stack,heap}).
new_env()->
    #env{regs=empty(),
	 sp=0,
	 hp=0,
	 stack=empty(),
	 heap=empty()
	}.
regs(Env)-> Env#env.regs.
sp(Env)-> Env#env.sp.
hp(Env)-> Env#env.hp.
stack(Env)-> Env#env.stack.
heap(Env)-> Env#env.heap.

merge(E1, E2) ->
  Env0 = (new_env())#env{regs=m(regs(E1),regs(E2))},
  Env1 = 
    case sp(E1) =:= sp(E2) of
      true ->
	Stack = m(stack(E1),stack(E2)),
	Env0#env{sp=sp(E1),stack= Stack};
      false -> Env0
    end,
  Env2 = 
    case hp(E1) =:= hp(E2) of
      true ->
	Heap = m(heap(E1),heap(E2)),
	Env1#env{hp=hp(E1),heap=Heap};
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


pp_lenv(GEnv) ->
  Env = env(GEnv),
  pp_env(Env).

pp_env(Env) ->
  pp_regs(to_list(regs(Env))),
  pp_mem(sp(Env),to_list(stack(Env)),"STACK"),
  pp_mem(hp(Env),to_list(heap(Env))," HEAP").

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


zap_stack(Env)-> Env#env{stack=empty()}.
zap_heap(Env)-> Env#env{heap=empty()}.
kill_sp(Env)->Env#env{sp=unknown}.
kill_hp(Env)->Env#env{hp=unknown}.
kill_phys_regs(Env)->
  R = regs(Env),
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
  S = stack(Env0),
  I2 = iterator(S),
  Env1 = Env0#env{stack=fold(F2, I2, S)},
  H = heap(Env1),
  I3 = iterator(H),
  Env2 = Env1#env{heap=fold(F2, I3, H)},
  Env2.

not_physical(R) ->
  case hipe_sparc:is_reg(R) of
    false -> true;
    true ->
      not hipe_sparc_registers:is_precolored(hipe_sparc:reg_nr(R))
  end.
		
inc_sp(Env, Val) ->  
  case sp(Env) of
    unknown ->
      Env;
    PrevVal ->
      Env1 = Env#env{sp=PrevVal+Val},
      case Val < 0 of
	false -> Env1;
	true -> %% SP is decreased, forget pos above.
	  %% XXX: Fix if stack direction is changed.

	  Env1#env{stack=
		   lists:foldl(fun delete/2,Env1#env.stack, 
			       [Pos   || Pos <- lists:seq(sp(Env1),sp(Env),4),
					 find(Pos,Env1#env.stack) =/= none])}
      end
  end.
inc_hp(Env, Val) ->  
  case hp(Env) of
    unknown ->
      Env;
    PrevVal ->
      Env#env{hp=PrevVal+Val}
  end.


%% Bind a value/reg to a stack position
bind_spos(Pos, Val, Env) ->
  Env#env{stack=enter(Pos,Val,Env#env.stack)}.


%% Bind a value/reg to a heap position
bind_hpos(Pos, Val, Env) ->
  Env#env{heap=enter(Pos,Val,Env#env.heap)}.

%% Find a value on a stackpos
find_spos(Pos, Env) ->
  plookup(Pos,Env#env.stack).

%% Find a value on a heappos
find_hpos(Pos, Env) ->
  plookup(Pos,Env#env.heap).

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
  Regs = Map#env.regs,
  Map#env{regs=enter(X,Y,Regs)}.

%%
%% Find what X is bound to (as a last restort varaibles are bound 
%% to themself).
%%

lookup(X, Env) ->
  case find(X, regs(Env)) of
    {value, V} -> V;
    _ -> X
  end.

%%
%% Kill bindings with references to register X
%%

kill_all(Xs, Env)->
  lists:foldl(fun kill/2,Env,Xs).
kill(X, Env0)->
  Env1 = Env0#env{regs=mkill(X,regs(Env0))},
  Env2 = Env1#env{stack=mkill(X,stack(Env1))},
  Env3 = Env2#env{heap=mkill(X,heap(Env2))},
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
  Env1 = Env0#env{regs=kill_use(X,regs(Env0))},
  Env2 = Env1#env{stack=kill_use(X,stack(Env1))},
  Env3 = Env2#env{heap=kill_use(X,heap(Env2))},
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

to_list(T) ->
  gb_trees:to_list(T).

delete(X, T) ->
  gb_trees:delete(X, T).

fold(F,I,T) ->
  case next(I) of 
    none -> T;
    {K,V,I2} ->
      fold(F,I2,F(K,V,T))
  end.
