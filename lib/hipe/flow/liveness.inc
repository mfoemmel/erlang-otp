%% -*- Erlang -*-
%% -*- erlang-indent-level: 2 -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% LIVENESS ANALYSIS
%%
%% Exports:
%% ~~~~~~~
%% analyze(CFG) - returns a livenes analyzis of CFG.
%% liveout(Liveness, Label) - returns a set of variables that are alive at
%%      exit from basic block named Label.
%% livein(Liveness, Label) - returns a set of variables that are alive at
%%      entry to the basic block named Label.
%% list(Instructions, LiveOut) - Given a list of instructions and a 
%%      liveout-set, returns a set of variables live at the first instruction.
%%

-export([analyze/1,
	 liveout/2,
	 livein/2,
	 list/2,
	 annotate/2,
	 update_livein/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface functions that MUST be implemented in the including file
%%
%%
%% cfg_bb(CFG, L) -> BasicBlock, extract a basic block from a cfg.
%% cfg_postorder(CFG) -> [Labels], the labels of the cfg in postorder
%% cfg_succ_map(CFG) -> SuccMap, a successor mapping.
%% cfg_succ(CFG, L) -> [Labels], 
%% cfg_bb_update(CFG, L, NewBB) ->
%% cfg_labels(CFG) ->
%% uses(Instr) ->
%% defines(Instr) ->
%% mk_comment(Text) ->
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The generic liveness analysis
%%
-ifdef(HIPE_LIVENESS_CALC_LARGEST_LIVESET).
analyze(CFG) ->
  PO = cfg_postorder(CFG),
  InitLiveness = liveness_init(init(cfg_labels(CFG), CFG)),

  _Max = case get(hipe_largest_liveset) of
	   undefined ->
	     put(hipe_largest_liveset,0),
	     0;
	   LL -> LL
	 end,
  Res = merry_go_around(PO, InitLiveness,0),
  case get(hipe_largest_liveset) > _Max of 
    true ->
      io:format("Largest liveset: ~w \n",[get(hipe_largest_liveset)]);
    _ -> ok
  end,
  Res.

-else.

analyze(CFG) ->
  PO = cfg_postorder(CFG),
  InitLiveness = liveness_init(init(PO, CFG)),
  Res = merry_go_around(PO, InitLiveness,0),
  Res.
-endif.
%%
%% The fixpoint iteration
%%

merry_go_around(Labels, Liveness, Count) ->
   case doit_once(Labels, Liveness, 0) of
      {NewLiveness, 0} -> 
         %% io:format("Iterations ~w~n",[Count]),
	 NewLiveness;
      {NewLiveness, _Changed} ->
	 merry_go_around(Labels, NewLiveness,Count+1)
   end.


%%
%% One iteration
%%

-ifdef(HIPE_LIVENESS_CALC_LARGEST_LIVESET).
doit_once([], Liveness, Changed) ->
  {Liveness, Changed};
doit_once([L|Ls], Liveness, Changed) ->
  LiveOut = liveout(Liveness, L),
  Kill = ordsets:subtract(LiveOut, kill(L, Liveness)),
  LiveIn = ordsets:union(Kill, gen(L,Liveness)),
  {NewLiveness, ChangedP} = update_livein(L, LiveIn, Liveness),
  Le = length(LiveIn),
  Max = get(hipe_largest_liveset),
  if Le > Max -> put(hipe_largest_liveset,Le);
     true -> true
  end,
   doit_once(Ls, NewLiveness, Changed+ChangedP).

-else.
doit_once([], Liveness, Changed) ->
  {Liveness, Changed};
doit_once([L|Ls], Liveness, Changed) ->
  LiveOut = liveout(Liveness, L),
  Kill = ordsets:subtract(LiveOut, kill(L, Liveness)),
  LiveIn = ordsets:union(Kill, gen(L,Liveness)),
  {NewLiveness, ChangedP} = update_livein(L, LiveIn, Liveness),

   doit_once(Ls, NewLiveness, Changed+ChangedP).
-endif.

%%
%% Given a list of instructions and liveout, calculates livein
%%
-ifdef(HIPE_LIVENESS_CALC_LARGEST_LIVESET).
list([], LiveOut) ->
  LiveOut;
list([I|Is], LiveOut) ->
  LiveIn = list(Is, LiveOut),
  InstrGen = ordsets:from_list(uses(I)),
  InstrKill = ordsets:from_list(defines(I)),
  Live = ordsets:union(InstrGen, ordsets:subtract(LiveIn, InstrKill)),
  Le = size(Live),
  Max = get(hipe_largest_liveset),
  if Le > Max -> put(hipe_largest_liveset,Le);
     true -> true
  end,
  Live.
-else.

list([], LiveOut) ->
  LiveOut;
list([I|Is], LiveOut) ->
  LiveIn = list(Is, LiveOut),
  InstrGen = ordsets:from_list(uses(I)),
  InstrKill = ordsets:from_list(defines(I)),
  Live = ordsets:union(InstrGen, ordsets:subtract(LiveIn, InstrKill)),
  Live.

-endif.

%%
%% updates liveness for a basic block
%%    - returns: {NewLiveness, ChangedP} 
%%    - ChangedP is 0 if the new LiveIn is equal to the old one
%%      otherwise it's 1.
%%

update_livein(Label, NewLiveIn, Liveness) ->
  {GK, LiveIn, Successors} = liveness_lookup(Label, Liveness),

   NewLiveness = liveness_update(Label, {GK, NewLiveIn, Successors}, Liveness),
   if LiveIn =:= NewLiveIn ->
	 {NewLiveness, 0};
      true ->
	 {NewLiveness, 1}
   end.


%%
%% LiveOut for a block is the union of the successors LiveIn
%%

liveout(Liveness, L) ->
   Succ = successors(L, Liveness),
   case Succ of
      [] ->    % special case if no successors
	 liveout_no_succ();
      _ ->
	 liveout1(Succ, Liveness)
   end.

liveout1(Labels, Liveness) ->
  liveout1(Labels, Liveness, ordsets:new()).
liveout1([], Liveness, Live) ->
   Live;
liveout1([L|Ls], Liveness,Live) ->
   liveout1(Ls, Liveness, ordsets:union(livein(Liveness, L),Live)).



successors(L, Liveness) ->
   {_GK,_LiveIn, Successors} = liveness_lookup(L, Liveness),
   Successors.

livein(Liveness, L) ->
   {_GK, LiveIn,_Successors} = liveness_lookup(L, Liveness),
   LiveIn.

kill(L, Liveness) ->
   {{_Gen,Kill},_LiveIn,_Successors} = liveness_lookup(L, Liveness),
   Kill.

gen(L, Liveness) ->
   {{Gen,_Kill},_LiveIn,_Successors} = liveness_lookup(L, Liveness),
   Gen.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% init returns a list of: {Label, {{Gen, Kill}, LiveIn, Successors}}
%%    - Label is the name of the basic block.
%%    - Gen is the set of varables that are used by this block.
%%    - Kill is the set of varables that are defined by this block.
%%    - LiveIn is the set of variables that are alive at entry to the
%%      block (initially empty).
%%    - Successors is a list of the successors to the block.

init([], _) ->
   [];
init([L|Ls], CFG) ->
   BB = cfg_bb(CFG, L),
   Code = hipe_bb:code(BB),
   SuccMap = cfg_succ_map(CFG),
   Succ = cfg_succ(SuccMap, L),
   Transfer = make_bb_transfer(Code, Succ),
   [{L, {Transfer, ordsets:new(), Succ}} | init(Ls, CFG)].


make_bb_transfer([], _Succ) ->
   {ordsets:new(), ordsets:new()};   % {Gen, Kill}
make_bb_transfer([I|Is], Succ) ->
   {Gen, Kill} = make_bb_transfer(Is, Succ),
   InstrGen = ordsets:from_list(uses(I)),
   InstrKill = ordsets:from_list(defines(I)),
   Gen1 = ordsets:subtract(Gen, InstrKill),
   Gen2 = ordsets:union(Gen1, InstrGen),
   Kill1 = ordsets:union(Kill, InstrKill),
   Kill2 = ordsets:subtract(Kill1, InstrGen),
   {Gen2, Kill2}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Annotate each basic block with liveness info
%%

annotate(CFG, Liveness) ->
   Labels = cfg_labels(CFG),
   annotate_bb(Labels, CFG, Liveness).

annotate_bb([], CFG, _Liveness) ->
   CFG;
annotate_bb([L|Ls], CFG, Liveness) ->
   BB = cfg_bb(CFG, L),
   Code0 = hipe_bb:code(BB),
   LiveIn = strip(livein(Liveness, L)),
   LiveOut = strip(liveout(Liveness, L)),
   Code = [mk_comment({live_in, LiveIn}),
	   mk_comment({live_out, LiveOut})
	   | Code0],
   NewBB = hipe_bb:code_update(BB, Code),
   NewCFG = cfg_bb_update(CFG, L, NewBB),
   annotate_bb(Ls, NewCFG, Liveness).


strip([]) ->
   [];
strip([{_,Y}|Xs]) ->
   [Y|strip(Xs)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
liveness_init(List) ->
  ?vector_from_list(dense(0,lists:sort(List), [])).
% liveness_init(List) -> hipe_hash:init(List).

liveness_lookup(Label, Liveness) ->
   ?vector_get(Label+1, Liveness).
%% liveness_lookup(Label, Liveness) ->
%%  {found, {GK, LiveIn, Successors}} = hipe_hash:lookup(Label, Liveness),
%%  {GK, LiveIn, Successors}.
liveness_update(Label, Val, Liveness) ->
  ?vector_set(Label+1, Liveness, Val).

%% liveness_update(Label, Val, Liveness) ->
%%  hipe_hash:update(Label, Val, Liveness).


%% Build a dense mapping 
dense(_, [], Vs) ->
  %% Done reverse the list.
  lists:reverse(Vs);
dense(N, [{Pos, Data}|Ms], Vs) when N =:= Pos ->
  %% N makes sure the mapping is dense. N is he next key.
  dense(N+1, Ms, [Data|Vs]);
dense(N, Source, Vs) ->
  %% The source was sparce, make up some placeholders...
  dense(N+1, 
	Source, 
	[undef|Vs]).
