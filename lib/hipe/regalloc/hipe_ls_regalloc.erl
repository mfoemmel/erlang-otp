%% -*- erlang-indent-level: 2 -*-
%% =====================================================================
%% @doc
%% <pre>
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
%%                              * 2001-07-16 EJ: Made less sparc-specific.
%% </pre>
%% <!--
%% CVS:
%%    $Author: richardc $
%%    $Date: 2002/10/01 12:45:59 $
%%    $Revision: 1.20 $
%% -->
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

-module(hipe_ls_regalloc).
-export([regalloc/7,allocate/5]).
%%-define(DEBUG,1).
-define(HIPE_INSTRUMENT_COMPILER, true).
-include("../main/hipe.hrl").
-include("../util/hipe_vector.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec
%% regalloc(CFG, PhysRegs, Entrypoints, DontSpill, Options, Target) ->
%%    {Coloring, NumberOfSpills}
%%  CFG = cfg()
%%  PhysRegs = [reg()]
%%  Entrypoints = [labelname()]
%%  DontSpill = reg()
%%  Options = proplist:proplist()
%%  Target = atom()
%%  Coloring = [{temp(), pos()}]
%%  NumberOfSpills = integer()
%%  reg() = integer()
%%  temp() = integer()
%%  pos() = {reg, reg()} | {spill, integer()}
%% 
%% @doc
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
calculate_intervals(CFG,Liveness,_Entrypoints, Options, Target) ->
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
      inorder ->
	Target:inorder(CFG);
      reverse_inorder ->
	Target:reverse_inorder(CFG);
      preorder ->
	Target:preorder(CFG);
      prediction ->
	Target:predictionorder(CFG);
      random ->
	Target:labels(CFG);
      _ ->
	Target:reverse_postorder(CFG)
    end,

  %% ?inc_counter(bbs_counter, length(Worklist)),
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
  %% Add all variables that are live at the entry of this block
  %% to the interval data structure.
  LiveIn = livein(Liveness,L, Target),
  Intervals2 = add_def_point(LiveIn,InstructionNr,Intervals),
  LiveOut = liveout(Liveness,L, Target),

  %% Traverse this block instruction by instruction and add all
  %% uses and defines to the intervals.
  Code = hipe_bb:code(bb(CFG,L, Target)),
  {Intervals3, NewINr} = 
    traverse_block(Code, InstructionNr+1,
		   Intervals2,
		   Target),
  
  %% Add end points for the registers that are in the live-out set.
  Intervals4 = add_use_point(LiveOut, NewINr+1, Intervals3),
  
  intervals(ToDO, Intervals4, NewINr+1, CFG, Liveness, SuccMap, Target);
intervals([],Intervals,_,_,_,_, _) -> 
  %% Return the calculated intervals
  interval_to_list(Intervals).

%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
%% traverse_block(Code, InstructionNo, Intervals, Unchanged) 
%%  Examine each instruction in the Code:
%%   For each temporary T used or defined by instruction number N:
%%    extend the interval of T to include N.
%%-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
traverse_block([Instruction|Is],InstrNo,Intervals, Target) ->
  %% Get defined temps.
  DefsSet = defines(Instruction, Target),
  Intervals1 = add_def_point(DefsSet, InstrNo, Intervals),

  %% Get used temps.
  UsesSet = uses(Instruction, Target),
  %% Extend the intervals for these temporaries to include InstrNo.
  Intervals2 = add_use_point(UsesSet, InstrNo, Intervals1),

  %% Handle the next instruction.
  traverse_block(Is,InstrNo+1,Intervals2,Target);
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
   ?debug_msg("Alloc interval: ~w, Free ~w\n",[RegInt, Free]),
  %% Remove from the active list those registers who's intervals 
  %% ends before the start of the current interval.
  {NewActive, NewFree} = 
    expire_old_intervals(Active, startpoint(RegInt), Free, Target),
  
  %% Get the name of the temp in the current interval.
  Temp = reg(RegInt), 
  case is_precolored(Temp, Target) of
    true -> 
      %% This is a precolored register we don't need to find a color
      %% Get the physical name of the register.
      PhysName = physical_name(Temp, Target), 
      %% Bind it to the precolored name.
      NewAlloc = alloc(Temp, PhysName, Alloc,Target), 
      case is_global(Temp, Target) of
	true -> 
	  %% this is a global precolored register 
	  allocate(RIS, NewFree, NewActive,
		   NewAlloc, SpillIndex, DontSpill, Target);
	false ->
	  case is_free(PhysName, NewFree) of
	    true ->
	      allocate(RIS, NewFree -- [PhysName], 
		       add_active(endpoint(RegInt), PhysName, Temp, NewActive),
		       NewAlloc,
		       SpillIndex, DontSpill, Target);
	    false ->
	      %% Some other temp has taken this precolored register,
	      %% throw it out.

	      {OtherActive, NewActive2} = 
		deactivate(PhysName, NewActive),
	      OtherTemp = active_name(OtherActive),
	      OtherEnd = active_endpoint(OtherActive),

	      NewSpillIndex = Target:new_spill_index(SpillIndex),
	      {NewAlloc2, NewActive3} = 
		spill(OtherTemp, OtherEnd, NewActive2, NewAlloc,
		      SpillIndex, DontSpill, Target),
	      allocate(RIS, 
		       NewFree, 
		       add_active(endpoint(RegInt), PhysName, Temp, NewActive3),
		       NewAlloc2, NewSpillIndex, DontSpill, Target)
	  
	  end
	    
      end;
    false -> 
      %% This is not a precolored register.
      case NewFree of 
	[] -> 
	  %% No physical registers available, we have to spill.
	  NewSpillIndex = Target:new_spill_index(SpillIndex),
	  {NewAlloc, NewActive2} = 
	    spill(Temp, endpoint(RegInt), Active, Alloc,
		  SpillIndex, DontSpill, Target),
	  %% io:format("Spilled ~w\n",[NewAlloc]),
	  allocate(RIS, NewFree, NewActive2, NewAlloc, NewSpillIndex,
		   DontSpill, Target);

	[FreeReg | Regs] -> 
	  %% The register FreeReg is available, let's use it.
	  allocate(RIS,Regs,
		   add_active(endpoint(RegInt), FreeReg, Temp, NewActive),
		   alloc(Temp, FreeReg, Alloc,Target),
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
expire_old_intervals([Active|Actives], CurrentPos, Free, Target) ->
  %% Does the live-range of the first active register end before 
  %% the current position?

  %% We expand multimove before regalloc, ignore the next 2 lines.
  %%  %% We don't free registers that end at the current position,
  %%  %%  since a multimove can decide to do the moves in another order...
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
      Alloc, SpillIndex,
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
	  {NewAlloc, NewActive2} = 
	    spill(CurrentReg, CurrentEndpoint, NewActive, Alloc,
		  SpillIndex, DontSpill, Target),
	  {NewAlloc, 
	   add_active(SpillEndpoint, SpillPhysName, SpillName,
		      NewActive2)};
	true ->
	  %% It is not precolored...

	  %% Allocate SpillCandidate to spill-slot SpillIndex
	  SpillAlloc = 
	    spillalloc(active_name(SpillCandidate), SpillIndex, 
		       Alloc),

	  %% Allocated the current register to the physical register
	  %% used by the spill candidate.
	  NewAlloc = alloc(CurrentReg, SpillPhysName, SpillAlloc,Target),
	  
	  %% Add the current register to the active registers
	  NewActive2 = 
	    add_active(CurrentEndpoint, SpillPhysName, CurrentReg, NewActive),
	  
	  {NewAlloc, NewActive2}
      end;
	
    false -> 
      %% The current register has the longest live-range.

      case can_spill(CurrentReg, DontSpill, Target) of 
	false ->
	  %% Cannot spill a precolored register
	  {NewAlloc, NewActive2} = 
	    spill(SpillName, SpillEndpoint, NewActive, Alloc,
		  SpillIndex, DontSpill, Target),
	  NewActive3 = 
	    add_active(CurrentEndpoint, SpillPhysName, CurrentReg, NewActive2),
	  {NewAlloc, NewActive3};
	true ->
	  %% It is not precolored...
	  %% Allocate the current register to spill-slot SpillIndex
	  {spillalloc(CurrentReg, SpillIndex, Alloc), Active}
      end
  end;
spill(CurrentReg, _CurrentEndpoint, [],
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
  (Name < DontSpill) and (not is_precolored(Name, Target)).

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

alloc(Name,Reg,[{Name,_}|A],Target) ->
  case Target of
    hipe_sparc_specific_fp ->
      [{Name,{fp_reg,Reg}}|A];
    _->
      [{Name,{reg,Reg}}|A]
  end;
alloc(Name,Reg,[{Name2,Binding}|Bindings],Target) when Name > Name2 ->
  [{Name2,Binding}|alloc(Name,Reg,Bindings,Target)];
alloc(Name,Reg,Bindings,Target) ->
  case Target of
    hipe_sparc_specific_fp ->
      [{Name,{fp_reg,Reg}}|Bindings];
    _->
      [{Name,{reg,Reg}}|Bindings]
  end.

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
%%   {Name, Start, End}.

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
	extend_interval(Pos, Value);

      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos}
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
	extend_interval(Pos, Value);

      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos}
    end,

  %% Add or update the extended interval.
  Intervals2 = gb_trees:enter(Temp, NewInterval, Intervals),

  %% Add the rest of the temporaries.
  add_def_point(Temps, Pos, Intervals2);

add_def_point([], _, I) ->
  %% No more to add return the interval.
  I.

extend_interval(Pos, {Beginning, End}) ->
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

  {NewBeginning, NewEnd}.
%extend_use_interval(Pos, [{none,End}|More]) ->
%  {[{none,Pos},{none,End}|More]};
%extend_use_interval(Pos, Intervals) ->
%  {Beginning,none} = lists:last(Intervals),
%  {Beginning, Pos}.


% extend_def_interval(Pos, {Beginning, End}) ->
%   %% If this position occures before the beginning
%   %%  of the interval, then extend the beginning to
%   %%  this position.

%   NewBeginning = 
%     if Pos < Beginning -> Pos;
%        true            -> Beginning
%     end,
%   %% If this position occures after the end
%   %%  of the interval, then extend the end to
%   %%  this position.
%   NewEnd = 
%     if Pos > End -> Pos;
%        true      -> End
%     end,
%   {NewBeginning, NewEnd}; 
% extend_def_interval(Pos, [{Beginning,none}|More]) ->
%   [{Pos,none}, {Beginning,none}|More];
% extend_def_interval(Pos, Intervals) ->
%   {Pos, Pos}.

%% ____________________________________________________________________
-else. %% isdef gb_intervals

empty_interval(N) ->
  ?vector_new(N, none).

interval_to_list(Intervals) ->
  add_indices(?vector_to_list(Intervals),0).

add_indices([{B,E}|Xs],N) ->
  [{N,B,E}|add_indices(Xs,N+1)];
add_indices([List|Xs],N) when is_list(List) ->
  flatten(List,N,Xs);
add_indices([none|Xs],N) ->
  add_indices(Xs,N+1);
add_indices([],_N) -> [].

flatten([{none, End}|Rest], N, More) -> 
  [{N,End,End} | flatten(Rest, N, More)];
flatten([{Beg, none}|Rest], N ,More) ->
  [{N,Beg,Beg} | flatten(Rest, N, More)];
flatten([],N,More) ->
  add_indices(More,N+1).




add_use_point([Temp|Temps],Pos,Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case ?vector_get(Temp+1, Intervals) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos};
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_interval(Pos, Value)
    end,

  %% Add or update the extended interval.
  Intervals2 = ?vector_set(Temp+1, Intervals, NewInterval),

  %% Add the rest of the temporaries.
  add_use_point(Temps, Pos, Intervals2);

add_use_point([], _, I) ->
  %% No more to add return the interval.
  I.

add_def_point([Temp|Temps],Pos,Intervals) ->
  %% Extend the old interval...
  NewInterval =
    case ?vector_get(Temp+1, Intervals) of
      %% This is the first time we see this temp...
      none ->
	%% ... create a new interval
	{Pos, Pos};
      %% This temp has an old interval...
      Value ->
	%% ... extend it.
	extend_interval(Pos, Value)
    end,

  %% Add or update the extended interval.
  Intervals2 = ?vector_set(Temp+1, Intervals, NewInterval), 

  %% Add the rest of teh temporaries.
  add_def_point(Temps, Pos, Intervals2);

add_def_point([], _, I) ->
  %% No more to add return the interval.
  I.

extend_interval(Pos, {Beginning, End}) ->
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

  {NewBeginning, NewEnd}.
%extend_use_interval(Pos, [{none,End}|More]) ->
%  {[{none,Pos},{none,End}|More]};
%extend_use_interval(Pos, Intervals) ->
%  {Beginning,none} = lists:last(Intervals),
%  {Beginning, Pos}.


%extend_def_interval(Pos, {Beginning, End}) ->
%  %% If this position occures before the beginning
%  %%  of the interval, then extend the beginning to
%  %%  this position.

%  NewBeginning = 
%    if Pos < Beginning -> Pos;
%       true            -> Beginning
%    end,

%  {NewBeginning, End}; 
%extend_def_interval(Pos, [{Beginning,none}|More]) ->
%  [{Pos,none}, {Beginning,none}|More];
%extend_def_interval(Pos, [{none,End}|Intervals]) ->
%  {End, Pos}.
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

%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to external functions.
%% XXX: Make this efficient somehow...
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%function(CFG, Target) ->
%%  Target:function(CFG).

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

is_precolored(R, Target) ->
  Target:is_precolored(R).

is_global(R, Target) ->
  Target:is_global(R).

physical_name(R, Target) ->
  Target:physical_name(R).

regnames(Regs2, Target) ->
  Regs = 
    case Target of
      hipe_sparc_specific ->
	hipe_sparc:keep_registers(Regs2);
      hipe_sparc_specific_fp->
	hipe_sparc:keep_fp_registers(Regs2);
      _ ->
	Regs2
    end,
  [Target:reg_nr(X) || X <- Regs].


arg_vars(CFG, Target) ->
  Target:args(CFG).

is_arg(Reg, Target) ->
  Target:is_arg(Reg).
