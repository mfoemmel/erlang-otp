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
-author(['Thorild Selén', 'Andreas Wallin', 'Ingemar Åberg']).

-export([regalloc/4]).

%-ifndef(DEBUG).
%-define(DEBUG,true).
%-endif.
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
regalloc(CFG, SpillIndex, SpillLimit, Target) ->
  %% Build interference graph
  ?debug_msg("Build IG\n",[]),
  IG = hipe_ig:build(CFG, Target),
  %% io:format("IG: ~p\n",[IG]),

  ?debug_msg("Init\n",[]),
  {Min_temporary, Max_temporary} = Target:var_range(CFG),
  No_temporaries = Target:number_of_temporaries(CFG),
  ?debug_msg("Coalescing RA: num_temps = ~p~n", [No_temporaries]),
  All_colors = ordsets:from_list(Target:allocatable()),
  K = length(All_colors),

  ?debug_msg("Init node sets\n",[]),
  Node_sets = hipe_node_sets:new(Target, Min_temporary, Max_temporary,
				Target:non_alloc(CFG)),
  %% io:format("NodeSet: ~w\n NonAlloc ~w\n",[Node_sets,	Target:non_alloc(CFG) ]),
  %% Add registers with their own coloring
  ?debug_msg("Moves\n",[]),
  Move_sets = hipe_moves:new(IG),

  ?debug_msg("Build Worklist\n",[]),
  Worklists = hipe_reg_worklists:new(IG, Node_sets, Move_sets, K),
  SelStk = [],
  Alias = initAlias(No_temporaries),

  ?debug_msg("Do coloring\n",[]),
  {IG0, Worklists0, Moves0, Alias0, Node_sets0, SelStk0} = 
    do_coloring(IG, Worklists, Node_sets, Move_sets, Alias,
		SelStk, K, SpillLimit, Target),
  %% io:format("SelStk0 ~w\n",[SelStk0]),
  ?debug_msg("Default coloring\n",[]),
  {Color0,Node_sets1} = 
    defaultColoring(hipe_node_sets:precolored(Node_sets0),
		    initColor(No_temporaries), Node_sets0, Target),

  ?debug_msg("Assign colors\n",[]),
  {Color1,Node_sets2} =
    assignColors(SelStk0, Node_sets1, Color0, Alias0, All_colors, Target),
  %% io:format("color0:~w\nColor1:~w\nNodes:~w\nNodes2:~w\nMax_temporary:~w\n",[Color0,Color1,Node_sets,Node_sets2,Max_temporary]),

  ?debug_msg("Build mapping\n",[]),
  Coloring = build_namelist(Node_sets2,SpillIndex,Alias0,Color1),
  %% io:format("Coloring ~p\n",[Coloring]),
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
  Simplify = not(hipe_reg_worklists:is_empty(simplify, Worklists)),
  Coalesce = not(hipe_moves:is_empty(worklist, Moves)),
  Freeze   = not(hipe_reg_worklists:is_empty(freeze, Worklists)),
  Spill    = not(hipe_reg_worklists:is_empty(spill, Worklists)),
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
	freeze(K, Worklists, Moves, hipe_ig:degree(IG), Node_sets, Alias),
      do_coloring(IG, Worklists0, Node_sets, Moves0, Alias, 
		  SelStk, K, SpillLimit, Target);
     Spill == true ->
      {Worklists0, Moves0} = 
	selectSpill(Worklists, Moves, IG, K, Node_sets, Alias, SpillLimit),
      do_coloring(IG, Worklists0, Node_sets, Moves0, Alias, 
		  SelStk, K, SpillLimit,Target);
     true -> % Catchall case
      {IG, Worklists, Moves, Alias, Node_sets, SelStk}
    end.

%%----------------------------------------------------------------------
%% Function:    adjacent
%%
%% Description: Adjacent nodes that's not coalesced, on the stack or
%%               precolored.
%% Parameters:
%%   Node        --  Node that you want to adjacents of
%%   Adj_list    --  An adjacent nodes list (created in IG)
%%   Coalesced   --  Nodes that are ready for coalesced
%%   SelStk      --  Nodes that we think will get a coloring
%%
%%   Returns: 
%%     A set with nodes/temporaries that are not coalesced, on the 
%%      stack or precolored.
%%----------------------------------------------------------------------

adjacent(Node, Adj_list, Coalesced, SelStk) ->
  Adjacent_edges = hipe_adj_list:edges(Node, Adj_list),
  Removed_coalesced = ordsets:subtract(Adjacent_edges, Coalesced),
  Stack_nodes = stack_nodes(SelStk, []),
  ordsets:subtract(Removed_coalesced, ordsets:from_list(Stack_nodes)).

%%----------------------------------------------------------------------
%% Function:    stack_nodes
%%
%% Description: Extract the nodes only from Stack_elements.
%% Parameters:
%%   Stack_elements --  A list of stack elements
%%   Node_list      --  An empty list
%%
%%   Returns: 
%%     A list of nodes found on stack.
%%----------------------------------------------------------------------

stack_nodes([], Node_list) ->
  Node_list;
stack_nodes([{Node, _}|Stack_elements], Node_list) -> 
  stack_nodes(Stack_elements, [Node|Node_list]).

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

simplify([], IG, Node_sets, Worklists, Moves, SelStk, K) -> 
  {IG, Worklists, Moves, SelStk};
simplify([Node|Nodes], IG, Node_sets, Worklists, Moves, SelStk, K) ->
  Worklists0 = hipe_reg_worklists:remove(simplify, Node, Worklists),
  Adj_nodes = hipe_adj_list:edges(Node, hipe_ig:adj_list(IG)),
  SelStk0 = [{Node, Adj_nodes} | SelStk],
  Adjacent = adjacent(Node, hipe_ig:adj_list(IG), 
		      hipe_node_sets:coalesced(Node_sets), SelStk0),
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

decrement_degree([], IG, Node_sets, Worklists, Moves, SelStk, K) -> 
  {IG, Worklists, Moves};
decrement_degree([Node|Nodes], IG, Node_sets, Worklists, Moves, SelStk, K) ->
  Degree0 = hipe_ig:degree(IG),
  Degree1 = hipe_degree:dec(Node, Degree0),
  IG0 = hipe_ig:set_degree(Degree1, IG),
  %% case (hipe_degree:degree(Node, Degree0) == K) of
  %% The degree has to drop *below* K before it is colorable.
  case (hipe_degree:degree(Node, Degree0) < K) of
    true ->
      Adjacent = adjacent(Node, 
			  hipe_ig:adj_list(IG0), 
			  hipe_node_sets:coalesced(Node_sets),
			  SelStk),
      Moves0 = enable_moves(ordsets:add_element(Node, Adjacent), Moves),
      Worklists0 = hipe_reg_worklists:remove(spill, Node, Worklists),
      case hipe_moves:move_related(Node, Moves0) of
	true ->
	  Worklists1 = hipe_reg_worklists:add(freeze, Node, Worklists0),
	  decrement_degree(Nodes, IG0, Node_sets, Worklists1, Moves0, 
			   SelStk, K);
	_ ->
	  Worklists1 = hipe_reg_worklists:add(simplify, Node, Worklists0),
	  decrement_degree(Nodes, IG0, Node_sets, Worklists1, Moves0, 
			   SelStk, K)
      end;
    _ ->
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
  case ordsets:is_element(Node, hipe_moves:active(Moves)) of
    true ->
      New_moves = hipe_moves:add(worklist, Node,
				 hipe_moves:remove(active, Node, Moves)),
      enable_moves_active_to_worklist(Nodes, New_moves);
    _ ->
      enable_moves_active_to_worklist(Nodes, Moves)
  end.

%% Build the namelists, these functions are fast hacks, they use knowledge 
%% about data representation that they shouldn't know, bad abstraction.

build_namelist(NodeSets,Index,Alias,Color) ->
  ?debug_msg("Building mapping\n",[]),
  {alias,AliasVector} = Alias,
  ?debug_msg("Vector to list\n",[]),
  AliasList = 
    build_alias_list(hipe_vectors_wrapper:vector_to_list(AliasVector),
		     0, %% The first temporary has index 0
		     []), %% Accumulator
  ?debug_msg("Coalesced\n",[]),
  NL1 = build_coalescedlist(AliasList,NodeSets,Color,Alias,[]),
  ?debug_msg("Regs\n",[]),
  NL2 = build_reglist(hipe_node_sets:colored(NodeSets),Color,NL1),
  ?debug_msg("Spills\n",[]),
  build_spillist(hipe_node_sets:spilled(NodeSets),Index,NL2).

build_spillist([],Index,List) ->
  {List,Index};
build_spillist([Node|Nodes],Index,List) ->
  ?debug_msg("[~p]: Spill ~p to ~p\n", [?MODULE,Node,Index]),
  build_spillist(Nodes,Index+1,[{Node,{spill,Index}}|List]).

build_coalescedlist([],NodeSets,Color,Alias,List) ->
  List;
build_coalescedlist([Node|Ns],NodeSets,Color,Alias,List) when integer(Node) ->
  AC = getColor(getAlias(Node,NodeSets,Alias),Color),
  build_coalescedlist(Ns,NodeSets,Color,Alias,[{Node,{reg,AC}}|List]);
build_coalescedlist([Node|Ns],NodeSets,Color,Alias,List) ->
  build_coalescedlist(Ns,NodeSets,Color,Alias,List).

build_reglist([],Color,List) -> 
  List;
build_reglist([Node|Ns],Color,List) ->
  build_reglist(Ns,Color,[{Node,{reg,getColor(Node,Color)}}|List]).

build_alias_list([],I,List) ->
  List;
build_alias_list([Alias|Aliases],I,List) when integer(Alias) ->
  build_alias_list(Aliases,I+1,[I|List]);
build_alias_list([Alias|Aliases],I,List) ->
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

assignColors (Stack,NodeSets,Color,Alias,AllColors, Target) ->
  case Stack of
    [] ->
      {Color,NodeSets};
    [{Node,Edges}|Stack1] ->
      
      case hipe_node_sets:member(precolored,Node, NodeSets) of
	true ->                                 % Already colored
	  ?debug_msg("Node ~p is already colored~n",[Node]),
	  assignColors(Stack1,NodeSets,Color,Alias,AllColors,Target);
	false ->                                % Try to find color
	  OkColors = findOkColors(Edges,AllColors,Color,NodeSets,Alias),
	  case OkColors of
	    [] ->                           % Spill case
	      NodeSets1 = hipe_node_sets:add(spilled,Node,NodeSets),
	      
	      assignColors(Stack1,NodeSets1,Color,
			   Alias,AllColors,Target);
	    [Col|Cols] ->                   % Colorize case
	      NodeSets1 = hipe_node_sets:add(colored,Node,NodeSets),
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
  NodeSets1 = hipe_node_sets:add(colored,Reg,NodeSets),
  defaultColoring(Regs,Color1,NodeSets1,Target).

%% Find the colors that are OK for a node with certain edges.

findOkColors(Edges,AllColors,Color,NodeSets,Alias) ->
  Edges2 = lists:map(fun(Node) -> getAlias(Node,NodeSets,Alias) end, Edges),
  find(Edges2,AllColors,Color,NodeSets,Alias).

%% Find all the colors of the nodes in the list [Node|Nodes] and remove them 
%% from the set OkColors, when the list is empty, return OkColors.

find([],OkColors,Color,NodeSets,Alias) ->
  ordsets:to_list(OkColors); 
find([Node|Nodes],OkColors,Color,NodeSets,Alias) ->
  case (hipe_node_sets:member(colored,Node,NodeSets) or 
	hipe_node_sets:member(precolored,Node,NodeSets)) of
    true ->  
      Col = getColor(Node,Color),
      OkColors1 = ordsets:del_element(Col,OkColors),
      find(Nodes,OkColors1,Color,NodeSets,Alias);
    false ->
      find(Nodes,OkColors,Color,NodeSets,Alias)
  end.

%%----------------------------------------------------------------------
%% Function: initColor
%%
%% Description:  Initialize a color mapping with NrOfNodes slots.
%% Parameters:
%%   NrOfNodes      -- The maximum number of the nodes that shall be colored.
%%
%% Returns:
%%   Color          -- The brand new color mapping.
%%----------------------------------------------------------------------

initColor(NrOfNodes) ->
  {color, hipe_vectors_wrapper:empty(NrOfNodes,undef)}.

%%----------------------------------------------------------------------
%% Function: getColor
%%
%% Description:  Get the color of a node in a certain color mapping
%% Parameters:
%%   Node          -- The node whose color shall be found
%%   Color         -- The color mapping we shall find the color in
%%
%% Returns:
%%   Col           -- The color of the node
%%----------------------------------------------------------------------

getColor(Node, {color,Color}) when integer(Node) ->
  hipe_vectors_wrapper:get(Color,Node);
getColor(Node, {color,Color}) ->
  ?error_msg("ERROR: ~p: Node is not an integer ~p",
	     [{?MODULE,getColor,2},Node]).

%%----------------------------------------------------------------------
%% Function: setColor
%%
%% Description: Set the color of a node in a certain color mapping.
%% Parameters: 
%%   Node          -- The node to color
%%   NodeColor     -- The color the node shall have
%%   Color         -- The colormapping the info shall be stored in
%% Returns:
%%   NewColor      -- The new mapping.
%%----------------------------------------------------------------------

setColor(Node, NodeColor, {color,Color}) when integer(Node) ->
  {color, hipe_vectors_wrapper:set(Color,Node,NodeColor)};
setColor(Node, NodeColor, {color,Color}) ->
  ?error_msg("ERROR: ~p: Node is not an integer ~p",
	     [{?MODULE,setColor,3},Node]).


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
    [Move|Rest] ->
      {move, Dest, Source} = Move,
      
      ?debug_msg("Testing nodes ~p and ~p for coalescing~n",[Dest,Source]),
      
      Alias_src = getAlias(Source, Node_sets, Alias),
      Alias_dst = getAlias(Dest, Node_sets, Alias),
      {U, V} = case Target:is_precolored(Alias_dst) of
		 true -> {Alias_dst, Alias_src};
		 false -> {Alias_src, Alias_dst}
	       end,
      Moves0 = hipe_moves:remove(worklist, Move, Moves),
      Degree0 = hipe_ig:degree(IG),
      if U == V ->
	  Moves1 = hipe_moves:add(coalesced, Move, Moves0),
	  Worklists0 = add_worklist(Worklists, U, K, Moves1, Degree0, Target),
	  {Moves1,
	   IG,
	   Worklists0,
	   Node_sets,
	   Alias};
	 true ->
	  case Target:is_precolored(V) or 
	    hipe_adj_set:adjacent(U, V, hipe_ig:adj_set(IG)) of 
	    true ->
	      Moves1 = 
		hipe_moves:add(constrained, Move, Moves0),
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
	      Adj_list = hipe_ig:adj_list(IG),
	      Coalesced_nodes = hipe_node_sets:coalesced(Node_sets),
	      AdjU = adjacent(U, Adj_list, Coalesced_nodes, SelStk),
	      AdjV = adjacent(V, Adj_list, Coalesced_nodes, SelStk),
	      
	      case (Target:is_precolored(U)
		    and all_adjacent_ok(AdjV, U, IG, K, Target))
		or (not(Target:is_precolored(U))
		    and (conservative(ordsets:union(AdjU, AdjV),
				      IG,
				      K))) of
		true ->
		  Moves1 = hipe_moves:add(coalesced, Move, Moves0),
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
		  {hipe_moves:add(active, Move, Moves0),
		   IG,
		   Worklists,
		   Node_sets,
		   Alias}
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
  case (not(Target:is_precolored(U))
	and not(hipe_moves:move_related(U, Moves))
	and (hipe_degree:is_simple(U, K, Degree))) of
    true ->
      hipe_reg_worklists:transfer(freeze, simplify, U, Worklists);
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
  Worklists1 = case hipe_reg_worklists:member(freeze, V, Worklists) of
		 true -> hipe_reg_worklists:remove(freeze, V, Worklists);
		 false -> hipe_reg_worklists:remove(spill, V, Worklists)
	       end,
  Node_sets1 = hipe_node_sets:add(coalesced, V, Node_sets),
  
  ?debug_msg("Coalescing ~p and ~p to ~p~n",[V,U,U]),
  
  Alias1 = setAlias(V, U, Alias),
  
  %% NOTE: Here there is an error in the pseudocode for the algorithm!
  Moves1 = hipe_moves:set_movelist(hipe_vectors_wrapper:set(hipe_moves:movelist(Moves),
							    U,
							    ordsets:union(hipe_moves:node_moves(U, Moves),
									  hipe_moves:node_moves(V, Moves))),
				   Moves),
  
  Adj_list = hipe_ig:adj_list(IG),
  Adjacent =
    adjacent(V, Adj_list, hipe_node_sets:coalesced(Node_sets1), SelStk),
  
  {IG1, Worklists2, Moves2} =
    combine_edges(Adjacent, U, IG, Node_sets1, Worklists1,
		  Moves1, SelStk, K, Target),

  New_worklists = case (not(hipe_degree:is_simple(U, K, hipe_ig:degree(IG1)))
			and hipe_reg_worklists:member(freeze, U, Worklists2)) of
		    true -> hipe_reg_worklists:transfer(freeze, spill, U, 
							Worklists2);
		    false -> Worklists2
		  end,
  {IG1, Node_sets1, New_worklists, Moves2, Alias1}.

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

combine_edges([], U, IG, Node_sets, Worklists, Moves, SelStk, K, Target) ->
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
    ((hipe_degree:is_simple(T, K, hipe_ig:degree(IG)))
     or Target:is_precolored(T)
     or hipe_adj_set:adjacent(T, R, hipe_ig:adj_set(IG))).

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

all_adjacent_ok([], U, IG, K, Target) -> false;
all_adjacent_ok([T|Ts], U, IG, K, Target) ->
    ok(T, U, IG, K, Target) and all_adjacent_ok(Ts, U, IG, K, Target).

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

conservative_count([], Degree, K, Cnt) -> Cnt;
conservative_count([Node|Nodes], Degree, K, Cnt) ->
  case hipe_degree:is_simple(Node, K, Degree) of
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
%%   NodeSets       -- A datatype containing the node sets
%%   Alias          -- The alias mapping
%%   SpillLimit     -- Try not to spill any nodes above the spill limit
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated moves
%%---------------------------------------------------------------------

selectSpill(WorkLists, Moves, IG, K, NodeSets, Alias, SpillLimit) ->
  [CAR|CDR] = hipe_reg_worklists:spill(WorkLists),
  
  SpillCost = getCost(CAR, IG,SpillLimit),
  M = findCheapest(CDR,IG,SpillCost,CAR, SpillLimit),
  
  WorkLists1 = hipe_reg_worklists:remove(spill,M,WorkLists),
  WorkLists2 = hipe_reg_worklists:add(simplify,M,WorkLists1),
  freezeMoves(M, K, WorkLists2, Moves, hipe_ig:degree(IG), NodeSets, Alias).

%% Find the node that is cheapest to spill

findCheapest([], IG, Cost, Cheapest, SpillLimit) ->
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
%%   NodeSets       -- A Datatype containing the different NodeSets
%%   Alias          -- An alias mapping, shows the alias of all coalesced 
%%                      nodes  
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated movelists
%%----------------------------------------------------------------------

freeze(K,WorkLists,Moves,Degrees,NodeSets,Alias) ->
  [U|_] = hipe_reg_worklists:freeze(WorkLists),         % Smarter routine?
  ?debug_msg("freezing node ~p~n",[U]),
  WorkLists0 = hipe_reg_worklists:transfer(freeze, simplify, U, WorkLists),
  freezeMoves(U,K,WorkLists0,Moves,Degrees,NodeSets,Alias).

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
%%   NodeSets       -- A Datatype containing the different NodeSets
%%   Alias          -- An alias mapping, shows the alias of all coalesced 
%%                     nodes  
%%
%% Returns:
%%   WorkLists      -- The updated worklists
%%   Moves          -- The updated movelists
%%----------------------------------------------------------------------

freezeMoves(U,K,WorkLists,Moves,Degrees,NodeSets,Alias) ->
  Nodes = hipe_moves:node_moves(U, Moves),
  freezeEm(U,Nodes,K,WorkLists,Moves,Degrees,NodeSets,Alias).

%% Find what the other value in a copy instruction is, return false if 
%% the instruction isn't a move with the first argument in it.

moves (U,{move,U,V}) ->
  V;
moves(U,{move,V,U}) ->
  V;
moves(U,Node) ->
  false.

freezeEm(U,[],K,WorkLists,Moves,Degrees,NodeSets,Alias) -> 
  {WorkLists,Moves};
freezeEm(U,[M|Ms],K,WorkLists,Moves,Degrees,NodeSets,Alias) ->
  case moves(U,M) of
    false ->
      freezeEm(U,Ms,K,WorkLists,Moves,Degrees,NodeSets,Alias);
    V ->
      {WorkLists2,Moves2} = freezeEm2(U,V,M,K,WorkLists,
				      Moves,Degrees,NodeSets,Alias),
      freezeEm(U,Ms,K,WorkLists2,Moves2,Degrees,NodeSets,Alias)
  end.

freezeEm2(U,V,M,K,WorkLists,Moves,Degrees,NodeSets,Alias) ->
  case hipe_moves:member(active, M, Moves) of
    true ->
      Moves1 = hipe_moves:remove(active, M, Moves),
      freezeEm3(U,V,M,K,WorkLists,Moves1,Degrees,NodeSets,Alias);	
    false ->
      Moves1 = hipe_moves:remove(worklist, M, Moves),
      freezeEm3(U,V,M,K,WorkLists,Moves1,Degrees,NodeSets,Alias)
  end.

freezeEm3(U,V,M,K,WorkLists,Moves,Degrees,NodeSets,Alias) ->
  Moves1 = hipe_moves:add(frozen,M,Moves),
  V1 = getAlias(V,NodeSets,Alias),
  %% We know that hipe_moves:node_moves/2 returns an ordset (a list).
  case (hipe_moves:node_moves(V1,Moves1) == []) and 
    hipe_degree:is_simple(V1,K,Degrees) of
    true ->
      ?debug_msg("freezing move between ~p and ~p(~p)~n", [U,V,V]),
      Worklists1 = hipe_reg_worklists:transfer(freeze, simplify, V1, WorkLists),
      {Worklists1, Moves1};
    false ->
      {WorkLists,Moves1}
  end.


%%----------------------------------------------------------------------
%% Function:     initAlias
%%
%% Description:  Initialize an alias mapping with NrOfNodes slots.
%% Parameters:
%%   NrOfNodes      -- The number of registers that might need an alias.
%%
%% Returns:
%%  Alias           -- The initial alias mapping.
%%----------------------------------------------------------------------

initAlias(NrOfNodes) ->
  {alias, hipe_vectors_wrapper:init(NrOfNodes)}.

%%----------------------------------------------------------------------
%% Function getAlias
%%
%% Description: Get the alias of a node.
%% Parameters:
%%   Node           -- The node, whose alias shall be found 
%%   NodeSets       -- A structure containing the different NodeSets
%%   Alias          -- The alias mapping
%%
%% Returns:
%%   Name           -- The requested alias
%%----------------------------------------------------------------------

getAlias(Node, NodeSets, {alias,Alias}) when integer(Node) ->
  case hipe_node_sets:member(coalesced,Node,NodeSets) of 
    true ->
      getAlias(hipe_vectors_wrapper:get(Alias,Node),NodeSets,{alias, Alias});
    false ->
      Node
  end;
getAlias(Node, NodeSets, {alias,Alias}) ->
  ?error_msg("ERROR: ~p: Node not integer: ~p", [{?MODULE,getAlias,3},Node]).

%%----------------------------------------------------------------------
%% Function: setAlias
%%
%% Description: Set the alias of a node.
%% 
%% Parameters:
%%   Node           -- The node that shall get an alias
%%   ToNode         -- The new name for the node
%%   Alias          -- The alias mapping the info shall be stored in
%%
%% Returns:
%%   Alias          -- The new alias mapping
%%----------------------------------------------------------------------

setAlias(Node, ToNode, {alias,Alias}) when integer(Node), integer(ToNode) ->
  {alias, hipe_vectors_wrapper:set(Alias,Node,ToNode)};
setAlias(Node, ToNode, {alias,Alias}) ->
  ?error_msg("ERROR: ~p: Node not integer: ~p or ~p",
	     [{?MODULE,setAlias,3},Node,ToNode]).
