%% -*- erlang-indent-level: 2 -*-
%% =====================================================================
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% =====================================================================
%%  Filename : 	hipe_ls_regalloc.erl
%%  Module   :	hipe_ls_regalloc
%%  Purpose  :  Perform a register allocation based on the 
%%              "linear-scan algoritm".
%%  Notes    :  * This is an implementation of 
%%                "Linear Scan Register Allocation" by 
%%                Massimiliano Poletto & Vivek Sarkar described in
%%                ACM TOPLAS Vol 21, No 5, September 1999.
%%
%%              * This implementation is target-independent and
%%                requires a target specific interface module
%%                as argument.  
%%                (Still waiting for a modular module system for Erlang.)
%%
%%              
%%  History  :	* 2000-04-07 Erik Johansson (happi@csd.uu.se): Created.
%%              * 2001-07-16 EJ: Made less sparc-specific.
%%
%% CVS:
%%    $Author: kostis $
%%    $Date: 2004/01/23 21:34:52 $
%%    $Revision: 1.10 $
%% =====================================================================
%% Exported functions (short description):
%%   regalloc(CFG,PhysRegs,Entrypoints, Options) -> 
%%    {Coloring, NumberOfSpills}
%%    Takes a (SPARC) CFG and returns a coloring of all used registers.  
%%    PhysRegs should be a list of available physical registers.
%%    Entrypoints should be a list of names of Basic Blocks that have
%%     external entry points.
%%
%%    The Coloring will be in the form of the "allocation datastructure"
%%     described below, that is, a list of tuples on the form
%%      {Name, {reg, PhysicalRegister}} or
%%      {Name, {spill, SpillIndex}}
%%    The NumberOfSpills is either 0 indicating no spill or the 
%%     SpillIndex of the last spilled register.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_ls_uc_regalloc).
-export([regalloc/7]).
%-define(DEBUG,1).
-define(HIPE_INSTRUMENT_COMPILER, true).
-include("../main/hipe.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% regalloc(CFG, PhysRegs, Entrypoints, DontSpill, Options, Target) 
%%   Calculates an allocation of registers using a linear_scan algorithm.
%%   There are three steps in the algorithm:
%%    1. Calculate live-ranges for all registers.
%%    2. Calculate live-intervals for each register.
%%       The live interval consists of a start position and a end position
%%       these are the first definition and last use of the register 
%%       given as instruction numbers in a breadth-first traversal of the
%%       control-flow-graph.
%%    3. Do a linear scan allocation over the live intervals.
%%
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
regalloc(CFG,PhysRegs,Entrypoints, SpillIndex, DontSpill, Options, Target) ->
  ?debug_msg("LinearScan: ~w\n",[erlang:statistics(runtime)]),
  %%     Step 1: Calculate liveness (Call external implementation.)
  Liveness = liveness(CFG, Target),
  ?debug_msg("liveness (done)~w\n",[erlang:statistics(runtime)]),
  USIntervals = calculate_intervals(CFG, Liveness,
				    Entrypoints, Options,
				    Target),
  ?debug_msg("intervals (done) ~w\n",[erlang:statistics(runtime)]),
  Intervals = sort_on_start(USIntervals),
  ?debug_msg("sort intervals (done) ~w\n",[erlang:statistics(runtime)]),
  %% ?debug_msg("Intervals ~w\n",[Intervals]),
  ?debug_msg("No intervals: ~w\n",[length(Intervals)]),

  ?debug_msg("count intervals (done) ~w\n",[erlang:statistics(runtime)]),
  Allocation = allocate(Intervals,PhysRegs, SpillIndex, DontSpill, Target),
  ?debug_msg("allocation (done) ~w\n",[erlang:statistics(runtime)]),
  Allocation.
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%        Step 2: Calculate live-intervals for each register.         %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% calculate_intervals(CFG,Liveness,Entrypoints, Options, Target)
%%  CFG: The Control-Flow Graph.
%%  Liveness: A map of live-in and live-out sets for each Basic-Block.
%%  Entrypoints: A set of BB names that have external entrypoints.
%%
calculate_intervals(CFG,Liveness,Entrypoints, Options, Target) ->
  %% Add start point for the argument registers.
  Args = arg_vars(CFG, Target),
  Interval = 
    add_def_point(Args, 0, 
		  empty_interval(Target:number_of_temporaries(CFG))),

  %% Interval = add_livepoint(Args, 0, empty_interval()),
  Worklist =
    case proplists:get_value(ls_order, Options) of
      reversepostorder ->
	Target:reverse_postorder(CFG);
      breadth ->
	Target:breadthorder(CFG);
      postorder ->
	Target:postorder(CFG);
      prediction ->
	Target:predictionorder(CFG);
      random ->
	Target:labels(CFG);
      _ ->
	Target:reverse_postorder(CFG)
    end,

  ?inc_counter(bbs_counter, length(Worklist)),
  %% ?debug_msg("No BBs ~w\n",[length(Worklist)]),
  intervals(Worklist, Interval, 1, CFG, Liveness, succ_map(CFG, Target), Target).

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% intervals(WorkList, Intervals, InstructionNr,
%%           CFG, Liveness, SuccMap)
%%  WorkList: List of BB-names to handle.
%%  Intervals: Intervals seen so far (sorted on register names).
%%  InstructionNr: The number of examined insturctions.
%%  CFG: The Control-Flow Graph.
%%  Liveness: A map of live-in and live-out sets for each Basic-Block.
%%  SuccMap: A map of successors for each BB name.
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
intervals([L|ToDO],Intervals,InstructionNr,CFG,Liveness,SuccMap, Target) ->
  %% ?debug_msg("Block ~w\n",[L]),
  %% Add all variables that are live at the entry of this block
  %% to the interval data structure.
  LiveIn = livein(Liveness,L, Target),
  Intervals2 = add_def_point(LiveIn,InstructionNr,Intervals),
  LiveOut = liveout(Liveness,L, Target),
%%  Unchanged = ordsets:intersection(LiveIn, LiveOut),
  %% ?debug_msg("In ~w -> Out ~w\n",[LiveIn, LiveOut]),
  %% Traverse this block instruction by instruction and add all
  %% uses and defines to the intervals.

  Code = hipe_bb:code(bb(CFG,L, Target)),
  {Intervals3, NewINr} = traverse_block(Code, InstructionNr,
					Intervals2,
%% Unchanged,
					Target),
  
  %% Add end points for the registers that are in the live-out set.
  Intervals4 = add_use_point(LiveOut, NewINr, Intervals3),
  
  intervals(ToDO, Intervals4, NewINr, CFG, Liveness, SuccMap, Target);
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
		 DefsSet, InstrNo, Intervals),

%%  UpdateUse = ordsets:subtract(UsesSet, Unchanged),
  %% Extend the intervals for these temporaries to include InstrNo.
  Intervals2 = add_use_point(UsesSet, %% ordsets:to_list(UpdateUse), 
			     InstrNo, Intervals1),

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
allocate(Intervals, PhysRegs, SpillIndex, DontSpill, Target) ->
  ActiveRegisters =[],
  AllocatedRegisters = empty_allocation(),
  allocate(Intervals, PhysRegs, ActiveRegisters,
	   AllocatedRegisters, SpillIndex, DontSpill, Target).
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% allocate(Intervals, Free, Active, Allocated, SpillIndex, Target) 
%%  Iterates of each register interval.
%%   Intervals: The list of register intervals.
%%   Free: Currently available physical registers.
%%   Active: Currently used physical registers (sorted on increasing 
%%            interval enpoints)
%%   Allocated: The mapping of register names to physical registers or
%%              to spill positions.
%%   SpillIndex: The number of spilled registers. 
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
allocate([RegInt|RIS], Free, Active, Alloc, SpillIndex, DontSpill, Target) ->
  %% ?debug_msg("Alloc interval: ~w, Free ~w\n",[RegInt, Free]),
  %% Remove from the active list those registers who's intervals 
  %% ends before the start of the current interval.
  {NewActive, NewFree} = expire_old_intervals(Active, startpoint(RegInt), Free),
  
  %% Get the name of the temp in the current interval.
  Temp = reg(RegInt), 
  case is_precoloured(Temp, Target) of
    true -> 
      %% This is a precoloured register we don't need to find a color
      %% Get the physical name of the register.
      PhysName = physical_name(Temp, Target), 
      Usage = count(RegInt),
      %% Bind it to the precoloured name.
      NewAlloc = alloc(Temp, PhysName, Alloc), 
      case is_global(Temp, Target) of
	true -> 
	  %% this is a global precoloured register 
	  allocate(RIS, NewFree, NewActive,
		   NewAlloc, SpillIndex, DontSpill, Target);
	false ->
	  case is_free(PhysName, NewFree) of
	    true ->
	      allocate(RIS, NewFree -- [PhysName], 
		       add_active(endpoint(RegInt), PhysName, Temp,
				  count(RegInt), NewActive),
		       NewAlloc,
		       SpillIndex, DontSpill, Target);
	    false ->
	      %% Some other temp has taken this precoloured register,
	      %% throw it out.
	      {OtherActive, NewActive2} = deactivate(PhysName, NewActive),
	      OtherTemp = active_name(OtherActive),
	      OtherEnd = active_endpoint(OtherActive),
	      OtherCount = active_count(OtherActive),
	      OtherInt = mk_interval(OtherTemp,0,OtherEnd),
	      NewSpillIndex = SpillIndex+1,
	      {NewAlloc2, NewActive3} = 
		spill(OtherTemp, OtherEnd, OtherCount,
		      NewActive2, NewAlloc,
		      NewSpillIndex, DontSpill, Target),
	      allocate(RIS, 
		       NewFree, 
		       add_active(endpoint(RegInt), PhysName, Temp, 
				  count(RegInt), NewActive3),
		       NewAlloc2, NewSpillIndex, DontSpill, Target)
	  end
      end;
    false -> 
      %% This is not a precoloured register.
      case NewFree of 
	[] -> 
	  %% No physical registers available, we have to spill.
	  NewSpillIndex = SpillIndex+1,
	  {NewAlloc, NewActive2} = 
	    spill(Temp, endpoint(RegInt), count(RegInt), Active, Alloc,
		  NewSpillIndex, DontSpill, Target),
	  %% io:format("Spilled ~w\n",[NewAlloc]),
	  allocate(RIS, NewFree, NewActive2, NewAlloc, NewSpillIndex,
		   DontSpill, Target);

	[FreeReg | Regs] -> 
	  %% The register FreeReg is available, let's use it.
	  allocate(RIS,Regs,
		   add_active(endpoint(RegInt), FreeReg, Temp, count(RegInt),
			      NewActive),
		   alloc(Temp, FreeReg, Alloc),
		   SpillIndex, DontSpill, Target)
      end
  end;
allocate([],_,_,Alloc,SpillIndex, _, _) -> 
  %% No more register intervals to handle
  %%  return the result.
  {Alloc, SpillIndex}.
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% expire_old_intervals(ActiveRegisters, CurrentPos, FreeRegisters) 
%%   Remove all registers that have live-ranges that ends before the
%%   current position from the active list and put them into the free
%%   list instead.
%%
%% ---------------------------------------------------------------------
expire_old_intervals([Active|Actives], CurrentPos, Free) ->
  %% Does the live-range of the first active register end before 
  %% the current position?
  %% We don't free registers that end at the current position,
  %%  since a multimove can decide to do the moves in another order...
  case active_endpoint(Active) < CurrentPos of
    true -> %% Yes -> Then we can free that register.

		
      expire_old_intervals(Actives, CurrentPos,
			   %% Add the register to the free pool.
			   [active_reg(Active)|Free]);
			   
			   %% Here we could try appending the
			   %% register to get a more widespread
			   %% use of registers.
			   %% Free ++ [active_reg(Active)]);
                           %% At the moment this does not seem to
                           %%  improve performance at all,
                           %%  on the other hand, the cost is very low.


    false -> 
      %% No -> Then we cannot free any more registers.
      %%       (Since they are sorted on endpoints...)    
      {[Active|Actives],Free}
  end;
expire_old_intervals([],_,Free) ->
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
spill(CurrentReg, CurrentEndpoint, CurrentCount,
      Active = [Active_H|Active_T], 
      Alloc, SpillIndex,
      DontSpill, Target) ->
  %% ?debug_msg("spilling one of ~w\nDOnt spill ~w\n",
	%%     [[CurrentReg|Active], DontSpill]),

  %% Find a spill candidate (one of the active): 
  %%  The register with the longest live-range.
  {NewActive, SpillCandidate} =  lowest_count(Active),

  SpillEndpoint = active_endpoint(SpillCandidate) ,
  SpillName = active_name(SpillCandidate),
  SpillPhysName = active_reg(SpillCandidate), 
  SpillCount = active_count(SpillCandidate),

  case SpillCount < CurrentCount of
    true -> 
      %% There is an already allocated register that has
      %% a lower use-count than the current register.
      case can_spill(SpillName, DontSpill, Target) of
	false ->
	  {NewAlloc, NewActive2} = 
	    spill(CurrentReg, CurrentEndpoint, CurrentCount, NewActive, Alloc,
		  SpillIndex, DontSpill, Target),
	  {NewAlloc, 
	   add_active(SpillEndpoint, SpillPhysName, SpillName,
		      SpillCount,
		      NewActive2)};
	true ->
	  %% It is not precoloured...

	  %% Allocate SpillCandidate to spill-slot SpillIndex
	  SpillAlloc = 
	    spillalloc(active_name(SpillCandidate), SpillIndex, 
		       Alloc),

	  %% Allocated the current register to the physical register
	  %% used by the spill candidate.
	  NewAlloc = alloc(CurrentReg, SpillPhysName, SpillAlloc),
	  
	  %% Add the current register to the active registers
	  NewActive2 = 
	    add_active(CurrentEndpoint, SpillPhysName, CurrentReg, 
		       CurrentCount, NewActive),
	  
	  {NewAlloc, NewActive2}
      end;
	
    false -> 
      %% The current register has the longest live-range.

      case can_spill(CurrentReg, DontSpill, Target) of 
	false ->
	  %% Cannot spill a precoloured register
	  {NewAlloc, NewActive2} = 
	    spill(SpillName, SpillEndpoint, SpillCount, NewActive, Alloc,
		  SpillIndex, DontSpill, Target),
	  NewActive3 = 
	    add_active(CurrentEndpoint, SpillPhysName, CurrentReg,
		       CurrentCount, NewActive2),
	  {NewAlloc, NewActive3};
	true ->
	  %% It is not precoloured...
	  %% Allocate the current register to spill-slot SpillIndex
	  {spillalloc(CurrentReg, SpillIndex, Alloc), Active}
      end
  end;
spill(CurrentReg, CurrentEndpoint, CurrentCount, [],
      Alloc, SpillIndex, DontSpill, Target) ->
  case can_spill(CurrentReg, DontSpill, Target) of 
    false -> %% Can't spill current!
      ?error_msg("Can't allocate registers\n",[]),
      ?EXIT({cannot_allocate_regs});
    true -> %% Can spill current.
      %% Allocate the current register to spill-slot SpillIndex
      {spillalloc(CurrentReg, SpillIndex, Alloc), []}
  end.

can_spill(Name, DontSpill, Target) ->
  (Name < DontSpill) and (not is_precoloured(Name, Target)).

%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
lowest_count([Min|Active]) ->
  Count = active_count(Min),
  lowest_count(Active, Count, [], Min).

lowest_count([{E, P, T, C}|Rest], Count, Acc, Min)  when Count < C ->
  lowest_count(Rest, Count, [{E, P, T, C}|Acc], Min);
lowest_count([{E, P, T, C}|Rest], Count, Acc, 
	      {MinEndpoint, MinPhysName, 
	       MinReg, MinCount}) ->

 NewAcc = add_active(MinEndpoint, MinPhysName, 
		     MinReg, MinCount, Acc),
  lowest_count(Rest, C, NewAcc, {E, P, T, C});
lowest_count([], _, Acc, Min) ->
  {lists:reverse(Acc), Min}.

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
  [{Name,{spill,N}}|A];
spillalloc(Name,N,[{Name2,Binding}|Bindings]) when Name > Name2 ->
  [{Name2,Binding}|spillalloc(Name,N,Bindings)];
spillalloc(Name,N,Bindings) ->
  [{Name,{spill,N}}|Bindings].
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
% butlast_last([X]) ->
%    {[],X};
% butlast_last([X|Y]) ->
%   {L,Last} = butlast_last(Y),
%   {[X|L],Last}.

%butlast([X]) ->
%   [];
% butlast([X|Y]) ->
%  [X|butlast(Y)];
%butlast([])->
%  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  The active datastructure.
%%   Keeps tracks of currently active (allocated) physical registers.
%%   It is sorted on end points in the intervals
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_active(Endpoint, PhysReg, RegName, Count, [{P1,R1,O1,C1}|Active]) when P1 < Endpoint ->
  [{P1,R1,O1,Count}|add_active(Endpoint, PhysReg, RegName, Count, Active)];
add_active(Endpoint, PhysReg, RegName, Count, Active) ->
  [{Endpoint, PhysReg, RegName, Count}|Active].

active_endpoint({ EndPoint, _PhysReg, _RegName, _Count}) ->
  EndPoint.
active_reg(     {_EndPoint,  PhysReg, _RegName, _Count}) ->
  PhysReg.
active_name(    {_EndPoint, _PhysReg,  RegName, _Count}) ->
  RegName.
active_count(   {_EndPoint, _PhysReg, _RegName,  Count}) ->
  Count.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Interval data structure.
%%
%%
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

mk_interval(Name, Start, End) ->
  {Name, Start, End, 0}.

endpoint({R,S,Endpoint,_}) ->
  Endpoint.
startpoint({R,Startpoint,E,_}) ->
  Startpoint.
reg({RegName,S,E,_}) ->
  RegName.
count({_,_,_,C}) ->
  C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Intervals data structure.

sort_on_start(I)->
 lists:keysort(2,I).


-ifdef(gb_intervals).
empty_interval(_) ->
  gb_trees:empty().

interval_to_list(Intervals) ->
  lists:flatten(
    lists:map(
      fun({T, I}) when list(I) ->
	  lists:map(
	    fun ({none, End}) -> 
		{T,End,End};
		({Beg, none}) ->
		{T,Beg, Beg}
	    end,
	    I);
	 ({T,{B,E}}) -> {T, B, E}
      end,
      gb_trees:to_list(Intervals))).

add_use_point([Temp|Temps],Pos,Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case gb_trees:lookup(Temp, Intervals) of
      %% This temp has an old interval...
      {value, Value} ->
	%% ... extend it.
	extend_use_interval(Pos, Value);

      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	[{none, Pos}]
    end,

  %% Add or update the extended interval.
  Intervals2 = gb_trees:enter(Temp, NewInterval, Intervals),

  %% Add the rest of teh temporaries.
  add_use_point(Temps, Pos, Intervals2);

add_use_point([], _, I) ->
  %% No more to add return the interval.
  I.

add_def_point([Temp|Temps],Pos,Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case gb_trees:lookup(Temp, Intervals) of
      %% This temp has an old interval...
      {value, Value} ->
	%% ... extend it.
	extend_def_interval(Pos, Value);

      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	[{Pos, none}]
    end,

  %% Add or update the extended interval.
  Intervals2 = gb_trees:enter(Temp, NewInterval, Intervals),

  %% Add the rest of the temporaries.
  add_def_point(Temps, Pos, Intervals2);

add_def_point([], _, I) ->
  %% No more to add return the interval.
  I.

extend_use_interval(Pos, {Beginning, End}) ->
  %% If this position occures after the end
  %%  of the interval, then extend the end to
  %%  this position.
  NewEnd = 
    if Pos > End -> Pos;
       true      -> End
    end,

  {Beginning, NewEnd};
extend_use_interval(Pos, [{none,End}|More]) ->
  {[{none,Pos},{none,End}|More]};
extend_use_interval(Pos, Intervals) ->
  {Beginning,none} = lists:last(Intervals),
  {Beginning, Pos}.


extend_def_interval(Pos, {Beginning, End}) ->
  %% If this position occures before the beginning
  %%  of the interval, then extend the beginning to
  %%  this position.

  NewBeginning = 
    if Pos < Beginning -> Pos;
       true            -> Beginning
    end,

  {NewBeginning, End}; 
extend_def_interval(Pos, [{Beginning,none}|More]) ->
  [{Pos,none}, {Beginning,none}|More];
extend_def_interval(Pos, Intervals) ->
  {Pos, Pos}.

%% ____________________________________________________________________
-else. %% isdef gb_intervals

empty_interval(N) ->
  hipe_vectors:new(N, none).

interval_to_list(Intervals) ->
  add_indices(hipe_vectors:vector_to_list(Intervals),0).

add_indices([{B,E,U}|Xs],N) ->
  [{N,B,E,U}|add_indices(Xs,N+1)];
add_indices([List|Xs],N) when is_list(List) ->
  flatten(List,N,Xs);
add_indices([none|Xs],N) ->
  add_indices(Xs,N+1);
add_indices([],N) -> [].

flatten([{none, End,U}|Rest], N, More) -> 
  [{N,End,End, U} | flatten(Rest, N, More)];
flatten([{Beg, none, U}|Rest], N ,More) ->
  [{N,Beg,Beg, U} | flatten(Rest, N, More)];
flatten([],N,More) ->
  add_indices(More,N+1).




add_use_point([Temp|Temps],Pos,Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case hipe_vectors:get(Intervals, Temp+1) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	[{none, Pos,0}];
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_use_interval(Pos, Value)
    end,

  %% Add or update the extended interval.
  Intervals2 = hipe_vectors:set(Intervals, Temp+1, NewInterval),

  %% Add the rest of the temporaries.
  add_use_point(Temps, Pos, Intervals2);

add_use_point([], _, I) ->
  %% No more to add return the interval.
  I.

add_def_point([Temp|Temps],Pos,Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case hipe_vectors:get(Intervals, Temp+1) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	[{Pos, none, 0}];
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_def_interval(Pos, Value)
    end,

  %% Add or update the extended interval.
  Intervals2 = hipe_vectors:set(Intervals, Temp+1, NewInterval), 

  %% Add the rest of teh temporaries.
  add_def_point(Temps, Pos, Intervals2);

add_def_point([], _, I) ->
  %% No more to add return the interval.
  I.

extend_use_interval(Pos, {Beginning, End, Use}) ->
  %% If this position occures after the end
  %%  of the interval, then extend the end to
  %%  this position.
  NewEnd = 
    if Pos > End -> Pos;
       true      -> End
    end,

  {Beginning, NewEnd, Use + 1};
extend_use_interval(Pos, [{none,End, Use}|More]) ->
  {[{none,Pos, Use +1},{none,End, Use}|More]};
extend_use_interval(Pos, Intervals) ->
  {Beginning,none, Use} = lists:last(Intervals),
  {Beginning, Pos, Use + 1}.


extend_def_interval(Pos, {Beginning, End, Use}) ->
  %% If this position occures before the beginning
  %%  of the interval, then extend the beginning to
  %%  this position.

  NewBeginning = 
    if Pos < Beginning -> Pos;
       true            -> Beginning
    end,

  {NewBeginning, End, Use+1}; 
extend_def_interval(Pos, [{Beginning,none, Use}|More]) ->
  [{Pos,none, Use +1}, {Beginning,none, Use}|More];
extend_def_interval(Pos, [{none,End,Use}|Intervals]) ->
  {End, Pos, Use}.
-endif. %% gb_intervals











%%linarize(I) ->
%%  linarize(I,[]).

%%linarize({R,Beg,End,Lt,Rt},Acc) ->
%%  linarize(Lt,[{R,Beg,End}|linarize(Rt,Acc)]);
%%linarize([],Acc) -> Acc.

%add_livepoint([Temp|Temps],Pos,Intervals) ->

   
%  %% Extend the old interval...
%  NewInterval =
%    case gb_trees:lookup(Temp, Intervals) of
%      %% This temp has an old interval...
%      {value, Value} ->
%	%% ... extend it.
%	extend_interval(Pos, Value);

%      %% This is the first time we see this temp...
%      none ->
%	%% ... create a new interval
%	{Pos, Pos}
%    end,

%  %% Add or update the extended interval.
%  Intervals2 = gb_trees:enter(Temp, NewInterval, Intervals),

%  %% Add the rest of teh temporaries.
%  add_livepoint(Temps, Pos, Intervals2);

%add_livepoint([], _, I) ->
%  %% No more to add return the interval.
%  I.

%extend_interval(Pos, {Beginning, End}) ->
%  %% If this position occures after the end
%  %%  of the interval, then extend the end to
%  %%  this position.
%  NewEnd = 
%    if Pos > End -> Pos;
%       true      -> End
%    end,

%  %% If this position occures before the beginning
%  %%  of the interval, then extend the beginning to
%  %%  this position.

%  NewBeginning = 
%    if Pos < Beginning -> Pos;
%       true            -> Beginning
%    end,

%  {NewBeginning, NewEnd}. 


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to external functions.
%% XXX: Make this efficient somehow...
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

succ_map(CFG, Target) ->
  Target:succ_map(CFG).

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

arg_vars(CFG, Target) ->
  Target:args(CFG).

