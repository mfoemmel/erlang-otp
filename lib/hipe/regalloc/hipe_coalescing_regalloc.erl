%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% File    : hipe_coalescing_regalloc.erl
%% Authors : Andreas Wallin <d96awa@it.uu.se>
%%           Thorild Selén <d95ths@.it.uu.se>
%%           Ingemar Åberg <d95ina@it.uu.se>
%% Purpose : Play paintball with registers on a target machine.  We win
%%           if they are all colored.  This is an iterated coalescing
%%           register allocator.
%% Created : 4 Mar 2000
%%-----------------------------------------------------------------------

-module(hipe_coalescing_regalloc).
-export([regalloc/5]).

%%-ifndef(DEBUG).
%%-define(DEBUG,true).
%%-endif.
-include("../main/hipe.hrl").

%%-----------------------------------------------------------------------
%% Function:    regalloc
%%
%% Description: Creates a K coloring for a function.
%% Parameters:
%%   CFG         -- A control flow graph
%%   SpillIndex  -- Last index of spill variable
%%   SpillLimit  -- Temporaris with numbers higher than this have
%%                  infinit spill cost. 
%%                  Consider changing this to a set.
%%   Target      -- The module containing the target-specific functions.
%%
%% Returns:
%%   Coloring    -- A coloring for specified CFG
%%   SpillIndex0 -- A new spill index
%%-----------------------------------------------------------------------
regalloc(CFG, SpillIndex, SpillLimit, Target, _Options) ->
  %% Build interference graph
  ?debug_msg("Build IG\n",[]),
  IG = hipe_ig:build(CFG, Target),
  %% io:format("IG: ~p\n",[IG]),

  ?debug_msg("Init\n",[]),
  {Min_temporary, Max_temporary} = Target:var_range(CFG),
  No_temporaries = Target:number_of_temporaries(CFG),
  ?debug_msg("Coalescing RA: num_temps = ~p~n", [No_temporaries]),
  Allocatable = Target:allocatable(),
  K = length(Allocatable),
  All_colors = colset_from_list(Allocatable),

  ?debug_msg("Init node sets\n",[]),
  Node_sets = hipe_node_sets:new(Target, Min_temporary, Max_temporary,
				Target:non_alloc(CFG)),
  %% io:format("NodeSet: ~w\n NonAlloc ~w\n",[Node_sets,Target:non_alloc(CFG)]),
  %% Add registers with their own coloring
  ?debug_msg("Moves\n",[]),
  Move_sets = hipe_moves:new(IG),

  ?debug_msg("Build Worklist\n",[]),
  Worklists = hipe_reg_worklists:new(IG, Node_sets, Move_sets, K),
  SelStk = stack_new(),
  Alias = initAlias(No_temporaries),

  ?debug_msg("Do coloring\n~p~n",[Worklists]),
  {_IG0, _Worklists0, _Moves0, Alias0, Node_sets0, SelStk0} = 
    do_coloring(IG, Worklists, Node_sets, Move_sets, Alias,
		SelStk, K, SpillLimit, Target),
  %% io:format("SelStk0 ~w\n",[SelStk0]),
  ?debug_msg("Default coloring\n",[]),
  {Color0,Node_sets1} = 
    defaultColoring(hipe_node_sets:precoloured(Node_sets0),
		    initColor(No_temporaries), Node_sets0, Target),

  ?debug_msg("Assign colors\n",[]),
  {Color1,Node_sets2} =
    assignColors(stack(SelStk0), Node_sets1, Color0, 
		 Alias0, All_colors, Target),
  %% io:format("color0:~w\nColor1:~w\nNodes:~w\nNodes2:~w\nMax_temporary:~w\n",[Color0,Color1,Node_sets,Node_sets2,Max_temporary]),

  ?debug_msg("Build mapping ~p\n",[Node_sets2]),
  Coloring = build_namelist(Node_sets2,SpillIndex,Alias0,Color1),
  ?debug_msg("Coloring ~p\n",[Coloring]),
  Coloring.

%%----------------------------------------------------------------------
%% Function:    do_coloring
%%
%% Description: Create a coloring. That is, play paintball.
%% Parameters:
%%   IG          --  An interference graph
%%   Worklists   --  Worklists, that is simplify, spill and freeze
%%   Node_sets   --  Node sets, that is spilled, coalesced and so on.
%%   Moves       --  Moves sets, that is coalesced, constrained 
%%                   and so on.
%%   Alias       --  Tells if two temporaries can have their value
%%                   in the same register.
%%   SelStk      --  Stack where simplified nodes are pushed.
%%   K           --  Want to create a K coloring.
%%   SpillLimit  --  Try not to spill nodes that are above the spill limit.
%%
%% Returns:
%%   IG          --  Updated interference graph
%%   Worklists   --  Updated Worklists structure
%%   Moves       --  Updated Moves structure 
%%   Alias       --  Updates Alias structure
%%   Node_sets   --  Updated Node_sets structure
%%   SelStk      --  Updated SelStk.
%%   
%%----------------------------------------------------------------------

do_coloring(IG, Worklists, Node_sets, Moves, Alias, SelStk, K, 
	    SpillLimit, Target) ->

  Simplify = not(hipe_reg_worklists:is_empty_simplify(Worklists)),
  Coalesce = not(hipe_moves:is_empty_worklist(Moves)),
  Freeze   = not(hipe_reg_worklists:is_empty_freeze(Worklists)),
  Spill    = not(hipe_reg_worklists:is_empty_spill(Worklists)),
  if Simplify == true ->
      {IG0, Worklists0, Moves0, SelStk0} = 
	simplify(hipe_reg_worklists:simplify(Worklists),
		 IG, 
		 Node_sets,
		 Worklists, 
		 Moves, 
		 SelStk, 
		 K),
      do_coloring(IG0, Worklists0, Node_sets, Moves0, Alias,
		  SelStk0, K, SpillLimit,Target);
     Coalesce == true ->
      {Moves0, IG0, Worklists0, Node_sets0, Alias0} = 
	coalesce(Moves, IG, Worklists, Node_sets, Alias, 
		 SelStk, K, Target),
      do_coloring(IG0, Worklists0, Node_sets0, Moves0, Alias0, 
		  SelStk, K, SpillLimit,Target);
     Freeze == true ->
      {Worklists0,Moves0} = 
	freeze(K, Worklists, Moves, hipe_ig:degree(IG), Alias),
      do_coloring(IG, Worklists0, Node_sets, Moves0, Alias, 
		  SelStk, K, SpillLimit, Target);
     Spill == true ->
      {Worklists0, Moves0} = 
	selectSpill(Worklists, Moves, IG, K, Alias, SpillLimit),
      do_coloring(IG, Worklists0, Node_sets, Moves0, Alias, 
		  SelStk, K, SpillLimit,Target);
     true -> % Catchall case
      {IG, Worklists, Moves, Alias, Node_sets, SelStk}
    end.

%%----------------------------------------------------------------------
%% Function:    adjacent
%%
%% Description: Adjacent nodes that's not coalesced, on the stack or
%%               precoloured.
%% Parameters:
%%   Node        --  Node that you want to adjacents of
%%   IG          --  The interference graph
%%   Coalesced   --  Nodes that are ready for coalesced
%%   SelStk      --  Nodes that we think will get a coloring
%%
%%   Returns: 
%%     A set with nodes/temporaries that are not coalesced, on the 
%%      stack or precoloured.
%%----------------------------------------------------------------------

adjacent(Node, IG, Coalesced, SelStk) ->
  Adjacent_edges = hipe_ig:node_adj_list(Node, IG),
  Removed_coalesced = ordsets:subtract(Adjacent_edges, Coalesced),
  remove_stacked(Removed_coalesced, SelStk).

%%----------------------------------------------------------------------
%% Function:    simplify
%%
%% Description: Simplify graph by removing nodes of low degree. This
%%               function simplify all nodes it can at once.
%% Parameters:
%%   [Node|Nodes]  --  The simplify worklist
%%   IG            --  The interference graph
%%   Node_sets     --  The node_sets data-structure
%%   Worklists     --  The worklists data-structure
%%   Moves         --  The moves data-structure
%%   SelStk        --  The stack data-structure
%%   K             --  Produce a K coloring
%%
%%   Returns: 
%%     IG          --  An updated interference graph
%%     Worklists   --  An updated worklists data-structure
%%     Moves       --  An updated moves data-structure
%%     SelStk      --  An updated stack data-structure
%%----------------------------------------------------------------------

simplify([], IG, _Node_sets, Worklists, Moves, SelStk, _K) -> 
  {IG, Worklists, Moves, SelStk};
simplify([Node|Nodes], IG, Node_sets, Worklists, Moves, SelStk, K) ->
  Worklists0 = hipe_reg_worklists:remove_simplify(Node, Worklists),
  ?debug_msg("putting ~w on stack~n",[Node]),
  Adjacent = adjacent(Node, IG, hipe_node_sets:coalesced(Node_sets), SelStk),
  SelStk0 = push(Node, Adjacent, SelStk),
  {New_ig, Worklists1, New_moves} =
    decrement_degree(Adjacent, IG, Node_sets, Worklists0, 
		     Moves, SelStk0, K),
  simplify(Nodes, New_ig, Node_sets, Worklists1, New_moves, SelStk0, K).

%%----------------------------------------------------------------------
%% Function:    decrement_degree
%%
%% Description: Decrement the degree on a number of nodes/temporaries.
%% Parameters:
%%   [Node|Nodes]  --  Decrement degree on these nodes
%%   IG            --  The interference graph
%%   Node_sets     --  The Node_sets data structure
%%   Worklists     --  The Worklists data structure
%%   Moves         --  The Moves data structure.
%%   SelStk        --  Nodes that we think will get a coloring
%%   K             --  We want to create a coloring with K colors
%%
%%   Returns: 
%%     IG          --  An updated interference graph (the degrees)
%%     Worklists   --  Updated Worklists. Changed if one degree goes
%%                     down to K.
%%     Moves       --  Updated Moves. Changed if a move related temporary
%%                     gets degree K.
%%----------------------------------------------------------------------

decrement_degree([], IG, _Node_sets, Worklists, Moves, _SelStk, _K) -> 
  {IG, Worklists, Moves};
decrement_degree([Node|Nodes], IG, Node_sets, Worklists, Moves, SelStk, K) ->
  Degree0 = hipe_ig:degree(IG),
  PrevDegree = hipe_degree:degree(Node, Degree0),
  Degree1 = hipe_degree:dec(Node, Degree0),
  IG0 = hipe_ig:set_degree(Degree1, IG),
  if PrevDegree =:= K ->
      Adjacent = adjacent(Node, 
			  IG0, 
			  hipe_node_sets:coalesced(Node_sets),
			  SelStk),
      Moves0 = enable_moves(ordsets:add_element(Node, Adjacent), Moves),
      Worklists0 = hipe_reg_worklists:remove_spill(Node, Worklists),
      case hipe_moves:move_related(Node, Moves0) of
	true ->
	  Worklists1 = hipe_reg_worklists:add_freeze(Node, Worklists0),
	  decrement_degree(Nodes, IG0, Node_sets, Worklists1, Moves0, 
			   SelStk, K);
	_ ->
	  Worklists1 = hipe_reg_worklists:add_simplify(Node, Worklists0),
	  decrement_degree(Nodes, IG0, Node_sets, Worklists1, Moves0, 
			   SelStk, K)
      end;
     true ->
      decrement_degree(Nodes, IG0, Node_sets, Worklists, Moves, SelStk,K)
  end.
	    
%%----------------------------------------------------------------------
%% Function:    enable_moves
%%
%% Description: Make (move-related) nodes that are not yet considered for
%%               coalescing, ready for possible coalescing.
%%               
%% Parameters:
%%   [Node|Nodes]   --  A list of move nodes
%%   Moves          --  The moves data-structure
%%
%%   Returns: 
%%     An updated moves data-structure
%%----------------------------------------------------------------------

enable_moves([], Moves) -> Moves;
enable_moves([Node|Nodes], Moves) ->
  Node_moves = hipe_moves:node_moves(Node, Moves),
  New_moves = enable_moves_active_to_worklist(Node_moves, Moves),
  enable_moves(Nodes, New_moves).

%%----------------------------------------------------------------------
%% Function:    enable_moves_active_to_worklist
%%
%% Description: Make (move-related) nodes that are not yeat considered for
%%               coalescing, ready for possible coalescing.
%%               
%% Parameters:
%%   [Node|Nodes]   --  A list of move nodes
%%   Moves          --  The moves data-structure
%%
%%   Returns: 
%%     An updated moves data-structure
%%----------------------------------------------------------------------

enable_moves_active_to_worklist([], Moves) -> Moves;
enable_moves_active_to_worklist([Node|Nodes], Moves) ->
  case hipe_moves:member_active(Node, Moves) of
    true ->
      New_moves = hipe_moves:add_worklist(Node,
				 hipe_moves:remove_active(Node, Moves)),
      enable_moves_active_to_worklist(Nodes, New_moves);
    _ ->
      enable_moves_active_to_worklist(Nodes, Moves)
  end.

%% Build the namelists, these functions are fast hacks, they use knowledge 
%% about data representation that they shouldn't know, bad abstraction.

build_namelist(NodeSets,Index,Alias,Color) ->
  ?debug_msg("Building mapping\n",[]),
  ?debug_msg("Vector to list\n",[]),
  AliasList = 
    build_alias_list(aliasToList(Alias),
		     0, %% The first temporary has index 0
		     []), %% Accumulator
  ?debug_msg("Alias list:~p\n",[AliasList]),
  ?debug_msg("Coalesced\n",[]),
  NL1 = build_coalescedlist(AliasList,Color,Alias,[]),
  ?debug_msg("Coalesced list:~p\n",[NL1]),
  ?debug_msg("Regs\n",[]),
  NL2 = build_reglist(hipe_node_sets:colored(NodeSets),Color,NL1),
  ?debug_msg("Regs list:~p\n",[NL2]),
  ?debug_msg("Spills\n",[]),
  build_spillist(hipe_node_sets:spilled(NodeSets),Index,NL2).

build_spillist([],Index,List) ->
  {List,Index};
build_spillist([Node|Nodes],Index,List) ->
  ?debug_msg("[~p]: Spill ~p to ~p\n", [?MODULE,Node,Index]),
  build_spillist(Nodes,Index+1,[{Node,{spill,Index}}|List]).

build_coalescedlist([],_Color,_Alias,List) ->
  List;
build_coalescedlist([Node|Ns],Color,Alias,List)
when is_integer(Node) ->
  ?debug_msg("Alias of ~p is ~p~n",[Node,getAlias(Node,Alias)]),
  AC = getColor(getAlias(Node,Alias),Color),
  build_coalescedlist(Ns,Color,Alias,[{Node,{reg,AC}}|List]);
build_coalescedlist([_Node|Ns],Color,Alias,List) ->
  build_coalescedlist(Ns,Color,Alias,List).

build_reglist([],_Color,List) -> 
  List;
build_reglist([Node|Ns],Color,List) ->
  build_reglist(Ns,Color,[{Node,{reg,getColor(Node,Color)}}|List]).

build_alias_list([],_I,List) ->
  List;
build_alias_list([Alias|Aliases],I,List) when is_integer(Alias) ->
  build_alias_list(Aliases,I+1,[I|List]);
build_alias_list([_Alias|Aliases],I,List) ->
  build_alias_list(Aliases,I+1,List).

%%----------------------------------------------------------------------
%% Function:    assignColors
%%
%% Description: Tries to assign colors to nodes in a stack.
%% Parameters:
%%   Stack          --  The SelectStack built by the Select function, 
%%                      this stack contains tuples in the form {Node,Edges} 
%%                      where  Node is the Node number and Edges is an ordset 
%%                      containing the numbers of all the adjacent nodes.
%%   NodeSets       --  This is a record containing all the different node 
%%                      sets that are used in the register allocator.
%%   Alias          --  This is a mapping from nodes to nodes, if a node has 
%%                      been coalesced this mapping shows the alias for that 
%%                      node.
%%   AllColors      --  This is an ordset containing all the available colors
%%
%%   Target         --  The module containing the target-specific functions.
%%
%% Returns:
%%   Color          --  A mapping from nodes to their respective color.
%%   NodeSets       --  The updated node sets.
%%----------------------------------------------------------------------

assignColors(Stack,NodeSets,Color,Alias,AllColors,Target) ->
  case Stack of
    [] ->
      {Color,NodeSets};
    [{Node,Edges}|Stack1] ->
      ?debug_msg("Coloring Node: ~p~n",[Node]),
      lists:foreach(fun (_E) ->
			?debug_msg("  Edge ~w-><~w>->~w~n",
				   begin A = getAlias(_E,Alias),
					 [_E,A,getColor(A,Color)]
				   end)
		    end, Edges),
      case Target:is_precoloured(Node) of
	true ->                             % Already coloured
	  ?debug_msg("Node ~p is already colored~n",[Node]),
	  assignColors(Stack1,NodeSets,Color,Alias,AllColors,Target);
	false ->                            % Try to find color
	  OkColors = findOkColors(Edges,AllColors,Color,Alias),
	  case colset_is_empty(OkColors) of
	    true ->                           % Spill case
	      NodeSets1 = hipe_node_sets:add_spilled(Node, NodeSets),
	      
	      assignColors(Stack1,NodeSets1,Color,
			   Alias,AllColors,Target);
	    false ->                  % Color case
	      Col = colset_smallest(OkColors),
	      NodeSets1 = hipe_node_sets:add_colored(Node, NodeSets),
	      Color1 = 
		setColor(Node, Target:physical_name(Col), Color),
	      
	      assignColors(Stack1,NodeSets1,Color1,Alias,AllColors,Target)
	  end
      end
  end.

%%---------------------------------------------------------------------
%% Function:     defaultColoring
%% 
%% Description: Make the default coloring
%% Parameters:
%%   Regs           -- The list of registers to be default colored
%%   Color          -- The color mapping that shall be changed
%%   NodeSets       -- The node sets that shall be updated
%%   Target         -- The module containing the target-specific functions.
%%
%% Returns:
%%   NewColor       -- The updated color mapping
%%   NewNodeSets    -- The updated node sets
%%---------------------------------------------------------------------

defaultColoring([],Color,NodeSets,_Target) ->
  {Color,NodeSets};
defaultColoring([Reg|Regs],Color,NodeSets,Target) ->
  Color1 = setColor(Reg,Target:physical_name(Reg),Color),
  NodeSets1 = hipe_node_sets:add_colored(Reg, NodeSets),
  defaultColoring(Regs,Color1,NodeSets1,Target).

%% Find the colors that are OK for a node with certain edges.

findOkColors(Edges,AllColors,Color,Alias) ->
  find(Edges, AllColors, Color, Alias).

%% Find all the colors of the nodes in the list [Node|Nodes] and remove them 
%% from the set OkColors, when the list is empty, return OkColors.

find([],OkColors,_Color,_Alias) ->
  OkColors;
find([Node0|Nodes],OkColors,Color,Alias) ->
  Node = getAlias(Node0, Alias),
  case getColor(Node, Color) of
    [] ->
      find(Nodes,OkColors,Color,Alias);
    Col ->
      OkColors1 = colset_del_element(Col, OkColors),
      find(Nodes,OkColors1,Color,Alias)
  end.

%%%
%%% ColSet -- ADT for the set of available colours while
%%% assigning colours.
%%%
-ifdef(notdef). % old ordsets-based implementation
colset_from_list(Allocatable) ->
  ordsets:from_list(Allocatable).

colset_del_element(Colour, ColSet) ->
  ordsets:del_element(Colour, ColSet).

colset_is_empty(ColSet) ->
  case ColSet of
    [] -> true;
    [_|_] -> false
  end.

colset_smallest([Colour|_]) ->
  Colour.
-endif.

-ifdef(notdef). % new gb_sets-based implementation
colset_from_list(Allocatable) ->
  gb_sets:from_list(Allocatable).

colset_del_element(Colour, ColSet) ->
  %% Must use gb_sets:delete_any/2 since gb_sets:del_element/2
  %% fails if the element isn't present. Bummer.
  gb_sets:delete_any(Colour, ColSet).

colset_is_empty(ColSet) ->
  gb_sets:is_empty(ColSet).

colset_smallest(ColSet) ->
  gb_sets:smallest(ColSet).
-endif.

%%-ifdef(notdef). % new bitmask-based implementation
colset_from_list(Allocatable) ->
  colset_from_list(Allocatable, 0).
colset_from_list([], ColSet) ->
  ColSet;
colset_from_list([Colour|Allocatable], ColSet) ->
  colset_from_list(Allocatable, ColSet bor (1 bsl Colour)).

colset_del_element(Colour, ColSet) ->
  ColSet band bnot(1 bsl Colour).

colset_is_empty(0) -> true;
colset_is_empty(_) -> false.

colset_smallest(ColSet) ->
  bitN_log2(ColSet band -ColSet, 0).

bitN_log2(BitN, ShiftN) ->
  if BitN > 16#ffff ->
      bitN_log2(BitN bsr 16, ShiftN + 16);
     true ->
      ShiftN + hweight16(BitN - 1)
  end.

hweight16(W) ->
  Res1 = (   W band 16#5555) + ((   W bsr 1) band 16#5555),
  Res2 = (Res1 band 16#3333) + ((Res1 bsr 2) band 16#3333),
  Res3 = (Res2 band 16#0F0F) + ((Res2 bsr 4) band 16#0F0F),
         (Res3 band 16#00FF) + ((Res3 bsr 8) band 16#00FF).
%%-endif.

%%%
%%% Colour ADT providing a partial mapping from nodes to colours.
%%%

initColor(NrNodes) ->
  {colmap, hipe_bifs:array(NrNodes, [])}.

getColor(Node, {colmap,ColMap}) ->
  hipe_bifs:array_sub(ColMap, Node).

setColor(Node, Colour, {colmap,ColMap}) ->
  hipe_bifs:array_update(ColMap, Node, Colour),  
  {colmap,ColMap}.

%%%
%%% Alias ADT providing a partial mapping from nodes to nodes.
%%%

initAlias(NrNodes) ->
  {alias, hipe_bifs:array(NrNodes, [])}.

getAlias(Node, {alias,AliasMap}) ->
  case hipe_bifs:array_sub(AliasMap, Node) of
    [] ->
      Node;
    AliasNode ->
      getAlias(AliasNode, {alias,AliasMap})
  end.

setAlias(Node, AliasNode, {alias,AliasMap}) ->
  hipe_bifs:array_update(AliasMap, Node, AliasNode),
  {alias,AliasMap}.

aliasToList({alias,AliasMap}) ->
  aliasToList(AliasMap, hipe_bifs:array_length(AliasMap), []).
aliasToList(AliasMap, I1, Tail) ->
  I0 = I1 - 1,
  if I0 >= 0 ->
      aliasToList(AliasMap, I0, [hipe_bifs:array_sub(AliasMap, I0)|Tail]);
     true ->
      Tail
  end.

%%----------------------------------------------------------------------
%% Function:    coalesce
%%
%% Description: Coalesces nodes in worklist
%% Parameters:
%%   Moves       -- Current move information
%%   IG          -- Interference graph
%%   Worklists   -- Current worklists
%%   Node_sets   -- Current node information
%%   Alias       -- Current aliases for temporaries
%%   SelStk      -- Nodes selected for coloring
%%   K           -- Number of registers
%%   
%% Returns:
%%   {Moves, IG, Worklists, Node_sets, Alias}
%%         (Updated versions of above structures, after coalescing)
%%----------------------------------------------------------------------

coalesce(Moves, IG, Worklists, Node_sets, Alias, SelStk, K, Target) ->
  case hipe_moves:worklist(Moves) of
    [] ->
      ?error_msg("ERROR: ~p: No moves in worklist", [?MODULE]);
    [Move|_Rest] ->
      {move, Dest, Source} = Move,
      
      ?debug_msg("Testing nodes ~p and ~p for coalescing~n",[Dest,Source]),
      
      Alias_src = getAlias(Source, Alias),
      Alias_dst = getAlias(Dest, Alias),
      {U, V} = case Target:is_precoloured(Alias_dst) of
		 true -> {Alias_dst, Alias_src};
		 false -> {Alias_src, Alias_dst}
	       end,

      Moves0 = hipe_moves:remove_worklist(Move, Moves),
      Degree0 = hipe_ig:degree(IG),

      %% XXX: (Happi) This is probably not the right fix -- but it
      %%                       works better... 
      %% FIX: If an aliased dst already is on the stack it should not colaesced.
      case on_stack(V,SelStk) orelse 
	   on_stack(U,SelStk) of
	true -> 
	  Moves1 = Moves0, % drop constrained move Move
	  Worklists1 = 
	    add_worklist(add_worklist(Worklists,
				      U,
				      K,
				      Moves1,
				      Degree0,
				      Target),
			 V,
			 K,
			 Moves1,
			 Degree0,
			 Target),
	  {Moves1,
	   IG,
	   Worklists1,
	   Node_sets,
	   Alias};
	_ -> %% U and V not on the stack.
	  if U == V ->
	      Moves1 = Moves0, % drop coalesced move Move
	      Worklists0 = add_worklist(Worklists, U, K, Moves1, Degree0, Target),
	      {Moves1,
	       IG,
	       Worklists0,
	       Node_sets,
	       Alias};
	     true ->
	      case Target:is_precoloured(V) orelse
		hipe_ig:nodes_are_adjacent(U, V, IG) of 
		true ->
		  Moves1 = Moves0, % drop constrained move Move
		  Worklists1 = 
		    add_worklist(add_worklist(Worklists,
					      U,
					      K,
					      Moves1,
					      Degree0,
					      Target),
				 V,
				 K,
				 Moves1,
				 Degree0,
				 Target),
		  {Moves1,
		   IG,
		   Worklists1,
		   Node_sets,
		   Alias};
		false ->
		  Coalesced_nodes = hipe_node_sets:coalesced(Node_sets),
		  AdjU = adjacent(U, IG, Coalesced_nodes, SelStk),
		  AdjV = adjacent(V, IG, Coalesced_nodes, SelStk),
		  case (Target:is_precoloured(U)
			andalso all_adjacent_ok(AdjV, U, IG, K, Target))
		    orelse (not(Target:is_precoloured(U))
			andalso (conservative(ordsets:union(AdjU, AdjV),
					  IG,
					  K))) of
		    true ->
		      Moves1 = Moves0, % drop coalesced move Move
		      {IG0,
		       Node_sets0,
		       Worklists1,
		       Moves2,
		       Alias0} = combine(U,
					 V,
					 IG,
					 Node_sets,
					 Worklists,
					 Moves1,
					 Alias,
					 SelStk,
					 K,
					 Target),
		      
		      Degree1 = hipe_ig:degree(IG0),
		      Worklists2 = add_worklist(Worklists1,
						U,
						K,
						Moves2,
						Degree1,
						Target),
		      {Moves2,
		       IG0,
		       Worklists2,
		       Node_sets0,
		       Alias0};
		    false ->
		      {hipe_moves:add_active(Move, Moves0),
		       IG,
		       Worklists,
		       Node_sets,
		       Alias}
		  end
	      end
	  end
      end
  end.

%%----------------------------------------------------------------------
%% Function:    add_worklist
%%
%% Description: Builds new worklists where U is transferred from freeze
%%              to simplify, if possible
%%
%% Parameters:
%%   Worklists     -- Current worklists
%%   U             -- Node to operate on
%%   K             -- Number of registers
%%   Moves         -- Current move information
%%   Degree        -- Degree information from interference graph
%%   Target        -- The containing the target-specific functions
%%   
%% Returns:
%%   Worklists (updated)
%%----------------------------------------------------------------------

add_worklist(Worklists, U, K, Moves, Degree, Target) ->
  case (not(Target:is_precoloured(U))
	andalso not(hipe_moves:move_related(U, Moves))
	andalso (hipe_degree:is_trivially_colorable(U, K, Degree))) of
    true ->
      hipe_reg_worklists:transfer_freeze_simplify(U, Worklists);
    false ->
      Worklists
  end.

%%----------------------------------------------------------------------
%% Function:    combine
%%
%% Description: Combines two nodes into one (used when coalescing)
%%
%% Parameters:
%%   U          -- First node to operate on
%%   V          -- Second node to operate on
%%   IG         -- Interference graph
%%   Node_sets  -- Current node information
%%   Worklists  -- Current worklists
%%   Moves      -- Current move information
%%   Alias      -- Current aliases for temporaries
%%   SelStk     -- Nodes selected for coloring
%%   K          -- Number of registers
%%
%% Returns:
%%   {IG, Node_sets, Worklists, Moves, Alias} (updated)
%%----------------------------------------------------------------------
       
combine(U, V, IG, Node_sets, Worklists, Moves, Alias, SelStk, K, Target) ->
  Worklists1 = case hipe_reg_worklists:member_freeze(V, Worklists) of
		 true -> hipe_reg_worklists:remove_freeze(V, Worklists);
		 false -> hipe_reg_worklists:remove_spill(V, Worklists)
	       end,
  Node_sets1 = hipe_node_sets:add_coalesced(V, Node_sets),
  
  ?debug_msg("Coalescing ~p and ~p to ~p~n",[V,U,U]),
  
  Alias1 = setAlias(V, U, Alias),
  
  %% Typo in published algorithm: s/nodeMoves/moveList/g to fix.
  Moves1 = hipe_moves:set_movelist(hipe_vectors_wrapper:set(hipe_moves:movelist(Moves),
							    U,
							    ordsets:union(hipe_moves:node_moves(U, Moves),
									  hipe_moves:node_moves(V, Moves))),
				   Moves),
  %% Missing in published algorithm. From Tiger book Errata.
  Moves2 = enable_moves([V], Moves1),
  Adjacent =
    adjacent(V, IG, hipe_node_sets:coalesced(Node_sets1), SelStk),
  
  {IG1, Worklists2, Moves3} =
    combine_edges(Adjacent, U, IG, Node_sets1, Worklists1,
		  Moves2, SelStk, K, Target),

  New_worklists = case (not(hipe_degree:is_trivially_colorable(U, K, hipe_ig:degree(IG1)))
			andalso hipe_reg_worklists:member_freeze(U, Worklists2)) of
		    true -> hipe_reg_worklists:transfer_freeze_spill(U, Worklists2);
		    false -> Worklists2
		  end,
  {IG1, Node_sets1, New_worklists, Moves3, Alias1}.

%%----------------------------------------------------------------------
%% Function:    combine_edges
%%
%% Description: For each node in a list, make an edge between that node
%%              and node U, and decrement its degree by 1
%%              (Used when two nodes are coalesced, to connect all nodes
%%              adjacent to one node to the other node)
%%
%% Parameters:
%%   [T|Ts]      -- List of nodes to make edges to
%%   U           -- Node to make edges from
%%   IG          -- Interference graph
%%   Node_sets   -- Current node information
%%   Worklists   -- Current worklists
%%   Moves       -- Current move information
%%   SelStk      -- Stack of nodes selected for coloring
%%   K           -- Number of registers
%%
%% Returns:
%%   {IG, Worklists, Moves} (updated)
%%----------------------------------------------------------------------

combine_edges([], _U, IG, _Node_sets, Worklists, Moves, _SelStk, _K, _Target) ->
  {IG, Worklists, Moves};
combine_edges([T|Ts], U, IG, Node_sets, Worklists, Moves, SelStk, K, Target) ->
  IG1 = hipe_ig:add_edge(T, U, IG, Target),
  {IG2, Worklists1, Moves1} =
    decrement_degree([T], IG1, Node_sets, Worklists, Moves, SelStk, K),
  combine_edges(Ts, U, IG2, Node_sets, Worklists1, Moves1, SelStk, K, Target).

%%----------------------------------------------------------------------
%% Function:    ok
%%
%% Description: Checks if a node T is suitable to coalesce with R
%%
%% Parameters:
%%   T             -- Node to test
%%   R             -- Other node to test
%%   IG            -- Interference graph
%%   K             -- Number of registers
%%   Target        -- The module containing the target-specific functions
%%   
%% Returns:
%%   true iff coalescing is OK
%%----------------------------------------------------------------------

ok(T, R, IG, K, Target) ->
    ((hipe_degree:is_trivially_colorable(T, K, hipe_ig:degree(IG)))
     orelse Target:is_precoloured(T)
     orelse hipe_ig:nodes_are_adjacent(T, R, IG)).

%%----------------------------------------------------------------------
%% Function:    all_ok
%%
%% Description: True iff, for every T in the list, OK(T,U)
%%
%% Parameters:
%%   [T|Ts]        -- Nodes to test
%%   U             -- Node to test for coalescing
%%   IG            -- Interference graph
%%   K             -- Number of registers
%%   Target        -- The module containing the target-specific functions
%%   
%% Returns:
%%   true iff coalescing is OK for all nodes in the list
%%----------------------------------------------------------------------

all_adjacent_ok([], _U, _IG, _K, _Target) -> false;
all_adjacent_ok([T|Ts], U, IG, K, Target) ->
    ok(T, U, IG, K, Target) andalso all_adjacent_ok(Ts, U, IG, K, Target).

%%----------------------------------------------------------------------
%% Function:    conservative
%%
%% Description: Checks if nodes can be safely coalesced according to
%%              the Briggs' conservative coalescing heuristic
%%
%% Parameters:
%%   Nodes         -- Adjacent nodes
%%   IG            -- Interference graph
%%   K             -- Number of registers
%%   
%% Returns:
%%   true iff coalescing is safe
%%----------------------------------------------------------------------

conservative(Nodes, IG, K) ->
    conservative_count(Nodes, hipe_ig:degree(IG), K, 0) < K.

%%----------------------------------------------------------------------
%% Function:    conservative_count
%%
%% Description: Counts degrees for conservative (Briggs' heuristic)
%%
%% Parameters:
%%   Nodes         -- (Remaining) adjacent nodes
%%   Degree        -- Degree information from interference graph
%%   K             -- Number of registers
%%   Cnt           -- Accumulator for counting
%%   
%% Returns:
%%   Final value of accumulator
%%----------------------------------------------------------------------

conservative_count([], _Degree, _K, Cnt) -> Cnt;
conservative_count([Node|Nodes], Degree, K, Cnt) ->
  case hipe_degree:is_trivially_colorable(Node, K, Degree) of
    true -> conservative_count(Nodes, Degree, K, Cnt);
    false -> conservative_count(Nodes, Degree, K, Cnt+1)
  end.

%%---------------------------------------------------------------------
%% Function:    selectSpill
%% 
%% Description: Select the node to spill and spill it
%% Parameters:
%%   WorkLists      -- A datatype containing the different worklists
%%   Moves          -- A datatype containing the move sets
%%   IG             -- The interference graph
%%   K              -- The number of available registers
%%   Alias          -- The alias mapping
%%   SpillLimit     -- Try not to spill any nodes above the spill limit
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated moves
%%---------------------------------------------------------------------

selectSpill(WorkLists, Moves, IG, K, Alias, SpillLimit) ->
  [CAR|CDR] = hipe_reg_worklists:spill(WorkLists),
  
  SpillCost = getCost(CAR, IG,SpillLimit),
  M = findCheapest(CDR,IG,SpillCost,CAR, SpillLimit),
  
  WorkLists1 = hipe_reg_worklists:remove_spill(M, WorkLists),
  WorkLists2 = hipe_reg_worklists:add_simplify(M, WorkLists1),
  freezeMoves(M, K, WorkLists2, Moves, hipe_ig:degree(IG), Alias).

%% Find the node that is cheapest to spill

findCheapest([], _IG, _Cost, Cheapest, _SpillLimit) ->
  Cheapest;
findCheapest([Node|Nodes], IG, Cost, Cheapest, SpillLimit) ->
  ThisCost = getCost(Node, IG, SpillLimit),
  case ThisCost <  Cost of
    true ->
      findCheapest(Nodes, IG, ThisCost, Node, SpillLimit);
    false ->
      findCheapest(Nodes, IG, Cost, Cheapest, SpillLimit)
  end.

%% Get the cost for spilling a certain node, node numbers above the spill 
%% limit are extremely expensive.

getCost(Node, IG, SpillLimit) ->
  case Node > SpillLimit of
    true ->  inf;
    false -> hipe_spillcost:spill_cost(Node, IG)
  end.

%%----------------------------------------------------------------------
%% Function:    freeze
%%
%% Description: When both simplifying and coalescing is impossible we 
%%              rather freezes a node in stead of spilling, this function
%%              selects a node for freezing (it just picks the first one in
%%              the list)
%%
%% Parameters:
%%   K              -- The number of available registers
%%   WorkLists      -- A datatype containing the different worklists
%%   Moves          -- A datatype containing the different movelists
%%   Degrees        -- A Datatype containing the degrees of the nodes
%%   Alias          -- An alias mapping, shows the alias of all coalesced 
%%                      nodes  
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated movelists
%%----------------------------------------------------------------------

freeze(K,WorkLists,Moves,Degrees,Alias) ->
  [U|_] = hipe_reg_worklists:freeze(WorkLists),         % Smarter routine?
  ?debug_msg("freezing node ~p~n",[U]),
  WorkLists0 = hipe_reg_worklists:transfer_freeze_simplify(U, WorkLists),
  freezeMoves(U,K,WorkLists0,Moves,Degrees,Alias).

%%----------------------------------------------------------------------
%% Function:    freezeMoves
%%
%% Description: Make all move related interferences for a certain node 
%%              into ordinary interference arcs.
%%              
%% Parameters:
%%   U              -- The node we want to freeze
%%   K              -- The number of available registers
%%   WorkLists      -- A datatype containing the different worklists
%%   Moves          -- A datatype containing the different movelists
%%   Degrees        -- A Datatype containing the degrees of the nodes
%%   Alias          -- An alias mapping, shows the alias of all coalesced 
%%                     nodes  
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated movelists
%%----------------------------------------------------------------------

freezeMoves(U,K,WorkLists,Moves,Degrees,Alias) ->
  Nodes = hipe_moves:node_moves(U, Moves),
  freezeEm(U,Nodes,K,WorkLists,Moves,Degrees,Alias).

%% Find what the other value in a copy instruction is, return false if 
%% the instruction isn't a move with the first argument in it.

moves (U,{move,U,V}) ->
  V;
moves(U,{move,V,U}) ->
  V;
moves(_U,_Node) ->
  false.

freezeEm(_U,[],_K,WorkLists,Moves,_Degrees,_Alias) -> 
  {WorkLists,Moves};
freezeEm(U,[M|Ms],K,WorkLists,Moves,Degrees,Alias) ->
  case moves(U,M) of
    false ->
      freezeEm(U,Ms,K,WorkLists,Moves,Degrees,Alias);
    V ->
      {WorkLists2,Moves2} = freezeEm2(U,V,M,K,WorkLists,
				      Moves,Degrees,Alias),
      freezeEm(U,Ms,K,WorkLists2,Moves2,Degrees,Alias)
  end.

freezeEm2(U,V,M,K,WorkLists,Moves,Degrees,Alias) ->
  case hipe_moves:member_active(M, Moves) of
    true ->
      Moves1 = hipe_moves:remove_active(M, Moves),
      freezeEm3(U,V,M,K,WorkLists,Moves1,Degrees,Alias);	
    false ->
      Moves1 = hipe_moves:remove_worklist(M, Moves),
      freezeEm3(U,V,M,K,WorkLists,Moves1,Degrees,Alias)
  end.

freezeEm3(_U,V,_M,K,WorkLists,Moves,Degrees,Alias) ->
  Moves1 = Moves, % drop frozen move M
  V1 = getAlias(V,Alias),
  %% We know that hipe_moves:node_moves/2 returns an ordset (a list).
  case (hipe_moves:node_moves(V1,Moves1) == []) andalso
    hipe_degree:is_trivially_colorable(V1,K,Degrees) of
    true ->
      ?debug_msg("freezing move to ~p~n", [V]),
      Worklists1 = hipe_reg_worklists:transfer_freeze_simplify(V1, WorkLists),
      {Worklists1,Moves1};
    false ->
      {WorkLists,Moves1}
  end.

%%----------------------------------------------------------------------
%% SelStack
%%
%%
stack_new() -> {[],gb_sets:new()}.

push(Node,Adj,{Stack,OnStack}) ->
  {[{Node,Adj}|Stack], gb_sets:add_element(Node,OnStack)}.

remove_stacked(List,{_Stack,OnStack}) ->
  [Node || Node <- List,
	   not gb_sets:is_member(Node,OnStack)].
	   
on_stack(Node, {_Stack,OnStack}) ->
  gb_sets:is_member(Node,OnStack).

stack({Stack,_}) -> Stack.
