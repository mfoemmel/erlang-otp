%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Experimental regalloc for SPARC code.
%% Splits the control flow graph at call sites, then 
%% local linear scan allocation is performed for each subgraph.
%%   code:add_path("ebin"),hipe:c({spillonkel,spillonkel,1},[{regalloc,cs}]).

-module(hipe_sparc_ra_cs).

-export([alloc/2]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

%% XXX: Temporary so that unused function warnings are suppressed
-compile(export_all).

%%-----------------------------------------------------------------------

%% Coloring are given as a list of {Reg, {reg, NewReg}} or 
%% {Reg, {spill, SpillIndex}}.
%%

alloc(_Cfg, _Options) ->
  %% XXX: Fix this or remove this RA.  
  exit(ra_cs_not_supported_at_the_moment).

%  %% Split the graph into subgraphs and allocate them.
%  {Mappings, NewSpillIndex, SpillMap} = split(Cfg),
%  %% io:format("SpillMap ~w",[SpillMap]),

%  %% Rewrite the mappings so that the mapping for 
%  %% each BB can be found by a vector lookup.
%  %%  Get size of CFG (in no BBs).
%  {_,MaxL} = hipe_sparc_cfg:label_range(Cfg),
%  %%  Create a lists [{Label, Mapping}|...]
%  %%  Where Mapping is the register mapping for the BB
%  %%  named Label. 
%  OrgMappings = 
%    lists:flatten(
%      lists:map(fun ({Group,M}) ->
%		    lists:map(fun(Block) ->
%				  {Block,M}
%			      end, Group)
%		end, Mappings)),

%  %%  Turn the list into an O(1) mapping...  
%  TempMaps = 
%    lists:foldl(fun ({B,Map}, Vector) ->
%		    hipe_vectors:set(Vector,B,Map)
%		end,
%		%% Since 2 new BBs might be added for each call
%		%% we play it safe and make the size of
%		%% the vector 3 * Number of BBs.
%		%% (At most one call per BB...)
%		hipe_vectors:new(MaxL*3,[]),
%		OrgMappings),



  %% Rewrite instructions that uses spilled temps.
%  NewCfg =
%    hipe_sparc_ra_post_cs:rewrite(
%      Cfg, TempMaps, Options),
%
%  ?add_spills(Options, NewSpillIndex),

  %% This does not work with the fpregister allocation.


%  {SparcCfg2, NextPos, NewBlocks} = 
%    hipe_sparc_caller_saves:rewrite(
%      NewCfg, SpillMap, NewSpillIndex, Options),
%  NewMappings =
%    [{Block,hipe_vectors:get(TempMaps,Old)} || {Block, Old} <- NewBlocks],
%
%  TempMaps2 =
%    lists:foldl(fun ({B,Map}, Vector) ->
%		    hipe_vectors:set(Vector,B,Map)
%		end,
%		TempMaps,
%		NewMappings),
%  {SparcCfg2, TempMaps2, NextPos}.



split(Cfg) ->
  Labels = hipe_sparc_cfg:labels(Cfg),
  Groups = srewrite_bbs(Labels,Cfg,[]),
  Liveness = liveness(Cfg,hipe_sparc_specific),
  Spill = new_spill(hipe_gensym:var_range(sparc)),
  {Mappings, {Six,Spos}} = ra_groups(Groups, Spill, Liveness, Cfg),
  DenseMappings = [{Group, 
		    make_dense(Mapping,Spos)} ||
		    {Group, Mapping} <- Mappings],

  %% hipe_sparc_cfg:pp(CFG0),
  {DenseMappings, Six, 
   hipe_temp_map:cols2tuple(
     add_indices2(hipe_vectors:vector_to_list(Spos),0),
     hipe_sparc_specific)}.



add_indices2([],N) -> [{N,{reg,N}}];
add_indices2([X|Xs],N) ->
  case X of 
    unknown ->
      add_indices2(Xs,N+1);
    _ ->
      [{N,{spill,X}}|add_indices2(Xs,N+1)]
  end.

new_spill({_First,Last}) ->
  {0,hipe_vectors:new(Last+1,unknown)}.

spill(X,Old = {Next,Spill}) ->
  case hipe_vectors:get(Spill, X+1) of
    unknown ->
      {Next, {Next+1, hipe_vectors:set(Spill,X+1,Next)}};
    Pos ->
      {Pos, Old}
  end.

ra_groups([Group|Groups], Spill, Liveness, Cfg) ->
  Lbls = lists:reverse(Group),
  {Mapping, NewSpill} = ra(Lbls,Spill, Liveness, Cfg),
  {Mappings, FinalSpill} =
    ra_groups(Groups, NewSpill, Liveness, Cfg),
  {[{Group,  Mapping} | Mappings], FinalSpill};
ra_groups([],Spill, _, _) -> {[], Spill}.

make_dense(Map, Spill) ->
  make_dense(0, Map, size(Spill), [], Spill).  

%% Build a dense mapping 
make_dense(N, [], M, Vs, _) when N >= M ->
  %% Convert to temp_map.
  hipe_temp_map:sorted_cols2tuple(lists:reverse(Vs), hipe_sparc_specific);
make_dense(N, [{R, C}|Ms], Max, Vs, Spill) when N =:= R ->
  %% N makes sure the mapping is dense. N is he next key.
  make_dense(N+1, Ms, Max, [{R,C}|Vs], Spill);
make_dense(N, SourceMapping, Max, Vs, Spill) ->
  %% The source was sparce, make up some placeholders...
  Val = 	      
    case hipe_sparc_specific:is_precoloured(N) of
      %% If it is precoloured we know what to map it to.
      true -> {N,{reg,N}};
      false ->
	case hipe_vectors:get(Spill, N+1) of
	  unknown -> {N, unknown};
	  Pos -> {N, {spill, Pos}}
	end
    end,
  make_dense(N+1, 
	     SourceMapping,
	     Max,
	     [Val|Vs],
	     Spill).


srewrite_bbs([], CFG, Code) ->
  Code2 = lists:flatten(lists:reverse(Code)),
  split2(
    [hipe_sparc_cfg:start_label(CFG)],	 
    hipe_sparc_cfg:update_code(CFG, Code2),
    [[]],
    hipe_sparc_cfg:succ_map(CFG),
    [],
    []);
srewrite_bbs([Lbl|Lbls], CFG, AccCode) ->
  BB = hipe_sparc_cfg:bb(CFG, Lbl),
  Code = [hipe_sparc:label_create(Lbl)|hipe_bb:code(BB)],
  NewCode = rewrite_instrs(Code),
  srewrite_bbs(Lbls, CFG, [NewCode| AccCode]).

split2([],_CFG,Groups,_,_,[]) ->
  Groups;
split2([],CFG,Groups, SuccMap, Vis,[NextG|MoreGs]) ->
  case lists:member(NextG,Vis) of
    true -> split2([],CFG,Groups,SuccMap,Vis,MoreGs);
    _ ->
      split2([NextG],CFG, [[]|Groups], SuccMap, Vis, MoreGs)
  end;
split2([Lbl|Ls],CFG,[G|Groups], SuccMap,Vis, MoreGs) ->
  case lists:member(Lbl,G) of
    true ->
      split2(Ls,CFG,[G|Groups], SuccMap,Vis, MoreGs);
    _ ->
      case lists:member(Lbl,Vis) of
	true ->
	  split2(Ls, CFG, merge(Lbl, G, Groups), SuccMap,Vis, MoreGs);
	_ ->
	  BB = hipe_sparc_cfg:bb(CFG, Lbl),
	  case hipe_sparc:type(hipe_bb:last(BB)) of
	    call_link ->
	      
	      split2(Ls,CFG,[[Lbl|G]|Groups], SuccMap,[Lbl|Vis],
		     MoreGs ++ hipe_sparc_cfg:succ(SuccMap,Lbl));
	    _ ->
	      split2(Ls ++ 
		     hipe_sparc_cfg:succ(SuccMap,Lbl),
		     CFG,
		     [[Lbl|G]|Groups], 
		     SuccMap,
		    [Lbl|Vis],MoreGs)
	  end
      end
  end.

merge(Lbl, G, Groups) ->
  {G2, Gs} = find_group(Lbl, Groups),
  [G2 ++ G | Gs].

find_group(Lbl, [G|Gs]) ->
  case lists:member(Lbl, G) of
    true ->
      {G, Gs};
    false ->
      {Gr, Rest} = find_group(Lbl,Gs),
      {Gr, [G|Rest]}
  end;
find_group(_,[]) ->
  exit(my_bad).
  


rewrite_instrs([]) ->
   [];
rewrite_instrs([I|Is]) ->
   [rewrite_instr(I) | rewrite_instrs(Is)].


rewrite_instr(Ins) ->
   case hipe_sparc:type(Ins) of
      call_link ->
       case hipe_sparc:call_link_continuation(Ins) of
	[] ->
	   NewL = hipe_sparc:label_create_new(),
	   [hipe_sparc:call_link_continuation_update(Ins,hipe_sparc:label_name(NewL)),
	    NewL];
	 _ -> Ins
       end;
     _ -> Ins
   end.


ra(Labels, Spill, Liveness, Cfg) ->
  I = empty_interval(hipe_sparc_specific:number_of_temporaries(Cfg)),
  USIntervals = intervals(Labels, I, 1, Cfg, Liveness, 
			  hipe_sparc_cfg:succ_map(Cfg),
			  hipe_sparc_specific),
  Intervals = sort_on_start(USIntervals),
  ?debug_msg("sort intervals (done) ~w\n",[erlang:statistics(runtime)]),
  %% ?debug_msg("Intervals ~w\n",[Intervals]),
  ?debug_msg("No intervals: ~w\n",[length(Intervals)]),
  PhysRegs = hipe_sparc_registers:allocatable() -- 
      [hipe_sparc_registers:temp1(),hipe_sparc_registers:temp2()],
%%       hipe_sparc_registers:temp3()],
  ?debug_msg("count intervals (done) ~w\n",[erlang:statistics(runtime)]),
  Allocation = allocate(Intervals,PhysRegs, Spill, [], hipe_sparc_specific),
  ?debug_msg("allocation (done) ~w\n",[erlang:statistics(runtime)]),
  Allocation.

intervals([L|ToDO],Intervals,InstructionNr,CFG,Liveness,SuccMap, Target) ->
  %% ?debug_msg("Block ~w\n",[L]),
  %% Add all variables that are live at the entry of this block
  %% to the interval data structure.
  LiveIn = livein(Liveness,L, Target),
  Intervals2 = add_def_point(LiveIn,InstructionNr,Intervals,0),
  LiveOut = liveout(Liveness,L, Target),
%%  Unchanged = ordsets:intersection(LiveIn, LiveOut),
  %% ?debug_msg("In ~w -> Out ~w\n",[LiveIn, LiveOut]),
  %% Traverse this block instruction by instruction and add all
  %% uses and defines to the intervals.

  Code = hipe_bb:code(bb(CFG,L, Target)),
  {Intervals3, NewINr} = traverse_block(Code, InstructionNr+1,
					Intervals2,
%% Unchanged,
					Target),
  
  %% Add end points for the registers that are in the live-out set.
  Intervals4 = add_use_point(LiveOut, NewINr+1, Intervals3,0),
  
  intervals(ToDO, Intervals4, NewINr+1, CFG, Liveness, SuccMap, Target);
intervals([],Intervals,_,_,_,_, _) -> 
  %% Return the calculated intervals
  interval_to_list(Intervals).
  %% Intervals.

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% traverse_block(Code, InstructionNo, Intervals, Unchanged) 
%%  Examine each instruction in the Code:
%%   For each temporary T used or defined by instruction number N:
%%    extend the interval of T to include N.
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
traverse_block([Instruction|Is],InstrNo,Intervals, %% Unchanged, 
	       Target) ->
  %% ?debug_msg("Unchanged ~w\n",[Unchanged]),
  %% ?debug_msg("~4w: ~w\n", [InstrNo, Instruction]),
  %% Get used temps.
  UsesSet = %% ordsets:from_list
	      (uses(Instruction, Target)),
  %% ?debug_msg("    Uses ~w\n", [UsesSet]),
  %% Get defined temps.
  DefsSet = %% ordsets:from_list
    (defines(Instruction, Target)),
  %% ?debug_msg("    Defines ~w\n", [DefsSet]),

  %% Only consider those temps that starts or ends their lifetime 
  %%  within the basic block (that is remove all Unchanged temps).
  %% UpdateDef = ordsets:subtract(DefsSet, Unchanged),
  Intervals1 = add_def_point(%% ordsets:to_list(UpdateDef),
		 DefsSet, InstrNo, Intervals,1),

%%  UpdateUse = ordsets:subtract(UsesSet, Unchanged),
  %% Extend the intervals for these temporaries to include InstrNo.
  Intervals2 = add_use_point(UsesSet, %% ordsets:to_list(UpdateUse), 
			     InstrNo, Intervals1,1),

  %% Handle the next instruction.
  traverse_block(Is,InstrNo+1,Intervals2, %% Unchanged, 
		 Target);

%%  Update = ordsets:subtract(ordsets:union(UsesSet, DefsSet), Unchanged),
%%
%%  %% Extend the intervals for these temporaries to include InstrNo.
%%  Intervals1 = add_livepoint(ordsets:to_list(Update), InstrNo, Intervals),
%%
%%  %% Handle the next instruction.
%%   traverse_block(Is,InstrNo+1,Intervals1, Unchanged, Target);

traverse_block([], InstrNo, Intervals, _) -> 
  %% Return the new intervals and the number of the next instruction.
  {Intervals,InstrNo}.
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%    Step 3. Do a linear scan allocation over the live intervals.    %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% allocate(Intervals, PhysicalRegisters, DontSpill, Target)
%%
%% This function performs the linear scan algorithm.
%%  Intervals contains the start and stop position of each register,
%%            sorted on increasing startpositions
%%  PhysicalRegisters is a list of available Physical registers to use.
%%
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
allocate(Intervals, PhysRegs, Spill, DontSpill, Target) ->
  ActiveRegisters =[],
  AllocatedRegisters = empty_allocation(),
  allocate(Intervals, PhysRegs, ActiveRegisters,
	   AllocatedRegisters, Spill, DontSpill, Target).
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% allocate(Intervals, Free, Active, Allocated, Spill, Target) 
%%  Iterates of each register interval.
%%   Intervals: The list of register intervals.
%%   Free: Currently available physical registers.
%%   Active: Currently used physical registers (sorted on increasing 
%%            interval enpoints)
%%   Allocated: The mapping of register names to physical registers or
%%              to spill positions.
%%   Spill:
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
allocate([RegInt|RIS], Free, Active, Alloc, Spill, DontSpill, Target) ->
   ?debug_msg("Alloc interval: ~w, Free ~w\n",[RegInt, Free]),
  %% Remove from the active list those registers who's intervals 
  %% ends before the start of the current interval.
  {NewActive, NewFree} = 
    expire_old_intervals(Active, startpoint(RegInt), Free, Target),
  
  %% Get the name of the temp in the current interval.
  Temp = reg(RegInt), 
  case is_precoloured(Temp, Target) of
    true -> 
      %% This is a precoloured register we don't need to find a color
      %% Get the physical name of the register.
      PhysName = physical_name(Temp, Target), 
      %% Bind it to the precoloured name.
      NewAlloc = alloc(Temp, PhysName, Alloc), 
      case is_global(Temp, Target) of
	true -> 
	  %% this is a global precoloured register 
	  allocate(RIS, NewFree, NewActive,
		   NewAlloc, Spill, DontSpill, Target);
	false ->
	  case is_free(PhysName, NewFree) of
	    true ->
	      allocate(RIS, NewFree -- [PhysName], 
		       add_active(endpoint(RegInt), PhysName, Temp, NewActive),
		       NewAlloc,
		       Spill, DontSpill, Target);
	    false ->
	      %% Some other temp has taken this precoloured register,
	      %% throw it out.
	      {OtherActive, NewActive2} = deactivate(PhysName, NewActive),
	      OtherTemp = active_name(OtherActive),
	      OtherEnd = active_endpoint(OtherActive),
	      {NewAlloc2, NewActive3, NewSpill} = 
		spill(OtherTemp, OtherEnd, NewActive2, NewAlloc,
		      Spill, DontSpill, Target),
	      allocate(RIS, 
		       NewFree, 
		       add_active(endpoint(RegInt), PhysName, Temp, NewActive3),
		       NewAlloc2, NewSpill, DontSpill, Target)
	  
	  end
      end;
    false -> 
      %% This is not a precoloured register.
      case NewFree of 
	[] -> 
	  %% No physical registers available, we have to spill.
	  {NewAlloc, NewActive2, NewSpill} = 
	    spill(Temp, endpoint(RegInt), Active, Alloc,
		  Spill, DontSpill, Target),
	  %% io:format("Spilled ~w\n",[NewAlloc]),
	  allocate(RIS, NewFree, NewActive2, NewAlloc, NewSpill,
		   DontSpill, Target);

	[FreeReg | Regs] -> 
	  %% The register FreeReg is available, let's use it.
	  allocate(RIS,Regs,
		   add_active(endpoint(RegInt), FreeReg, Temp, NewActive),
		   alloc(Temp, FreeReg, Alloc),
		   Spill, DontSpill, Target)
      end
  end;
allocate([],_,_,Alloc,Spill, _, _) -> 
  %% No more register intervals to handle
  %%  return the result.
  {Alloc, Spill}.
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% expire_old_intervals(ActiveRegisters, CurrentPos, FreeRegisters) 
%%   Remove all registers that have live-ranges that ends before the
%%   current position from the active list and put them into the free
%%   list instead.
%%
%% ---------------------------------------------------------------------
expire_old_intervals([Active|Actives], CurrentPos, Free, Target) ->
  %% Does the live-range of the first active register end before 
  %% the current position?
  %%  We remove mmoves first - ignore next two lines...
  %%   %% We don't free registers that end at the current position,
  %%   %%  since a multimove can decide to do the moves in another order...
  case active_endpoint(Active) =< CurrentPos of
    true -> %% Yes -> Then we can free that register.
      Reg = active_reg(Active),
      %% Add the register to the free pool.
      NewFree = 
	case is_arg(Reg, Target) of
	  true ->
	    Free ++ [Reg];
	  false ->
	    [active_reg(Active)|Free]
			   
	    %% Here we could try appending the
	    %% register to get a more widespread
	    %% use of registers.
	    %% Free ++ [active_reg(Active)]);
	    %% At the moment this does not seem to
	    %%  improve performance at all,
	    %%  on the other hand, the cost is very low.
	end,
		
      expire_old_intervals(Actives, CurrentPos, NewFree, Target);


    false -> 
      %% No -> Then we cannot free any more registers.
      %%       (Since they are sorted on endpoints...)    
      {[Active|Actives],Free}
  end;
expire_old_intervals([],_,Free,_) ->
  {[],Free}.
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

deactivate(Reg, [Active|Actives]) ->
  case Reg =:= active_reg(Active) of
    true ->
      {Active, Actives};
    false ->
      {TheActive, NewActives} =
	deactivate(Reg, Actives),
      {TheActive, [Active|NewActives]}
  end;
deactivate(_,[]) -> {no,[]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% spill(CurrentReg, CurrentEndpoint, Active, Alloc, SpillIndex, 
%%       DontSpill, Target)
%%   Find the register with the longest live range and spill it to memory.
%%
%% ---------------------------------------------------------------------
spill(CurrentReg, CurrentEndpoint, 
      Active = [_|_], 
      Alloc, Spill,
      DontSpill, Target) ->
   ?debug_msg("spilling one of ~w\nDOnt spill ~w\n",
	     [[CurrentReg|Active], DontSpill]),

  %% Find a spill candidate (one of the active): 
  %%  The register with the longest live-range.
  {NewActive, SpillCandidate} = butlast_last(Active),

  SpillEndpoint = active_endpoint(SpillCandidate) ,
  SpillName = active_name(SpillCandidate),
  SpillPhysName = active_reg(SpillCandidate), 

  case SpillEndpoint > CurrentEndpoint of
    true -> 
      %% There is an already allocated register that has
      %% a longer live-range than the current register.
      case can_spill(SpillName, DontSpill, Target) of
	false ->
	  {NewAlloc, NewActive2, NewSpill} = 
	    spill(CurrentReg, CurrentEndpoint, NewActive, Alloc,
		  Spill, DontSpill, Target),
	  {NewAlloc, 
	   add_active(SpillEndpoint, SpillPhysName, SpillName,
		      NewActive2), NewSpill};
	true ->
	  %% It is not precoloured...

	  %% Allocate SpillCandidate to spill-slot SpillIndex
	  {SpillIndex, NewSpill} = spill(active_name(SpillCandidate),Spill),
	  SpillAlloc = 
	    spillalloc(active_name(SpillCandidate), SpillIndex, 
		       Alloc),

	  %% Allocated the current register to the physical register
	  %% used by the spill candidate.
	  NewAlloc = alloc(CurrentReg, SpillPhysName, SpillAlloc),
	  
	  %% Add the current register to the active registers
	  NewActive2 = 
	    add_active(CurrentEndpoint, SpillPhysName, CurrentReg, NewActive),
	  
	  {NewAlloc, NewActive2, NewSpill}
      end;
	
    false -> 
      %% The current register has the longest live-range.
      case can_spill(CurrentReg, DontSpill, Target) of 
	false ->
	  %% Cannot spill a precoloured register
	  {NewAlloc, NewActive2, NewSpill} = 
	    spill(SpillName, SpillEndpoint, NewActive, Alloc,
		  Spill, DontSpill, Target),
	  NewActive3 = 
	    add_active(CurrentEndpoint, SpillPhysName, CurrentReg, NewActive2),
	  {NewAlloc, NewActive3, NewSpill};
	true ->
	  %% It is not precoloured...
	  %% Allocate the current register to spill-slot SpillIndex
	  {SpillIndex, NewSpill} = spill(CurrentReg,Spill),
	  {spillalloc(CurrentReg, SpillIndex, Alloc), Active, NewSpill}
      end
  end;
spill(CurrentReg, _CurrentEndpoint, [],
      Alloc, Spill, DontSpill, Target) ->
  case can_spill(CurrentReg, DontSpill, Target) of 
    false -> %% Can't spill current!
      ?error_msg("Can't allocate registers\n",[]),
      ?EXIT({cannot_allocate_regs});
    true -> %% Can spill current.
      %% Allocate the current register to spill-slot SpillIndex
      {SpillIndex, NewSpill} = spill(CurrentReg,Spill),

      {spillalloc(CurrentReg, SpillIndex, Alloc), [], NewSpill}
  end.

can_spill(Name, DontSpill, Target) ->
  (Name < DontSpill) and (not is_precoloured(Name, Target)).

%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%                   D A T A   S T R U C T U R E S                    %%
%%                                &                                   %%
%%               A U X I L I A R Y   F U N C T I O N S                %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The "allocation datastructure"
%%
%% This is an order list of register names paired with their allocations.
%%  {Name, Allocation}
%% The allocation is either {reg, physical register} or
%%                          {spill, spill index}
%%
%% ---------------------------------------------------------------------
empty_allocation() -> [].

alloc(Name,Reg,[{Name,_}|A]) ->
  [{Name,{reg,Reg}}|A];
alloc(Name,Reg,[{Name2,Binding}|Bindings]) when Name > Name2 ->
  [{Name2,Binding}|alloc(Name,Reg,Bindings)];
alloc(Name,Reg,Bindings) ->
  [{Name,{reg,Reg}}|Bindings].

spillalloc(Name,N,[{Name,_}|A]) ->
  ?debug_msg("Spilled ~w\n",[Name]),
  [{Name,{spill,N}}|A];
spillalloc(Name,N,[{Name2,Binding}|Bindings]) when Name > Name2 ->
  [{Name2,Binding}|spillalloc(Name,N,Bindings)];
spillalloc(Name,N,Bindings) ->
  [{Name,{spill,N}}|Bindings].
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
butlast_last([X]) ->
   {[],X};
butlast_last([X|Y]) ->
  {L,Last} = butlast_last(Y),
  {[X|L],Last}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  The active datastructure.
%%   Keeps tracks of currently active (allocated) physical registers.
%%   It is sorted on end points in the intervals
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_active(Endpoint, PhysReg, RegName, [{P1,R1,O1}|Active]) when P1 < Endpoint ->
  [{P1,R1,O1}|add_active(Endpoint, PhysReg, RegName, Active)];
add_active(Endpoint, PhysReg, RegName, Active) ->
  [{Endpoint, PhysReg, RegName}|Active].

active_reg({_,PhysReg,_}) ->
  PhysReg.
active_endpoint({EndPoint,_,_}) ->
  EndPoint.
active_name({_,_,RegName})->
  RegName.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Interval data structure.
%%
%%
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

%% mk_interval(Name, Start, End) ->
%%  {Name, Start, End}.

endpoint({_R,_S,Endpoint}) ->
  Endpoint.
startpoint({_R,Startpoint,_E}) ->
  Startpoint.
reg({RegName,_S,_E}) ->
  RegName.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Intervals data structure.

sort_on_start(I)->
 lists:keysort(2,I).

empty_interval(N) ->
  hipe_vectors:new(N, none).

interval_to_list(Intervals) ->
  add_indices(hipe_vectors:vector_to_list(Intervals),0).

add_indices([{B,E,C}|Xs],N) when C > 0 ->
  [{N,B,E}|add_indices(Xs,N+1)];
add_indices([{_B,_E,0}|Xs],N) ->
  add_indices(Xs,N+1);
add_indices([List|Xs],N) when is_list(List) ->
  flatten(List,N,Xs);
add_indices([none|Xs],N) ->
  add_indices(Xs,N+1);
add_indices([],_) -> [].

flatten([{none, End, _C}|Rest], N, More) -> 
  [{N,End,End} | flatten(Rest, N, More)];
flatten([{Beg, none, _C}|Rest], N ,More) ->
  [{N,Beg,Beg} | flatten(Rest, N, More)];
flatten([],N,More) ->
  add_indices(More,N+1).



add_use_point([Temp|Temps],Pos,Intervals, C) ->
  %% Extend the old interval...
  NewInterval =
    case hipe_vectors:get(Intervals, Temp+1) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos, C};
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_interval(Pos, Value, C)
    end,

  %% Add or update the extended interval.
  Intervals2 = hipe_vectors:set(Intervals, Temp+1, NewInterval),

  %% Add the rest of the temporaries.
  add_use_point(Temps, Pos, Intervals2, C);

add_use_point([], _, I, _) ->
  %% No more to add return the interval.
  I.

add_def_point([Temp|Temps],Pos,Intervals, C) ->
  %% Extend the old interval...
  NewInterval =
    case hipe_vectors:get(Intervals, Temp+1) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos, C};
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_interval(Pos, Value, C)
    end,

  %% Add or update the extended interval.
  Intervals2 = hipe_vectors:set(Intervals, Temp+1, NewInterval), 

  %% Add the rest of teh temporaries.
  add_def_point(Temps, Pos, Intervals2, C);

add_def_point([], _, I, _) ->
  %% No more to add return the interval.
  I.

extend_interval(Pos, {Beginning, End, Count}, AddedCount) ->
  %% If this position occures before the beginning
  %%  of the interval, then extend the beginning to
  %%  this position.

  NewBeginning = 
    if Pos < Beginning -> Pos;
       true            -> Beginning
    end,

  %% If this position occures after the end
  %%  of the interval, then extend the end to
  %%  this position.
  NewEnd = 
    if Pos > End -> Pos;
       true      -> End
    end,

  {NewBeginning, NewEnd, Count+AddedCount}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Freel data structure.
%%
%%
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

is_free(R, [R|_]) ->
  true;
is_free(R, [_|Rs]) ->
  is_free(R, Rs);
is_free(_, [] ) ->
  false.

%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to external functions.
%% XXX: Make this efficient somehow...
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

liveness(CFG, Target) ->
  Target:analyze(CFG).

bb(CFG, L, Target) ->
  Target:bb(CFG,L).

livein(Liveness,L, Target) ->
  regnames(Target:livein(Liveness,L), Target).

liveout(Liveness,L, Target)->
  regnames(Target:liveout(Liveness,L), Target).

uses(I, Target)->
  regnames(Target:uses(I), Target).

defines(I, Target) ->
  regnames(Target:defines(I), Target).

is_precoloured(R, Target) ->
  Target:is_precoloured(R).

is_global(R, Target) ->
  Target:is_global(R).

physical_name(R, Target) ->
  Target:physical_name(R).

regnames(Regs, Target) ->
  [Target:reg_nr(X) || X <- Regs]. 

is_arg(Reg, Target) ->
  Target:is_arg(Reg).
