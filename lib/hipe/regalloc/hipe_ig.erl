%% -*- erlang-indent-level: 4 -*-
%%%----------------------------------------------------------------------
%%% File    : hipe_ig.erl
%%% Author  : Andreas Wallin <d96awa@ida.dis.uu.se>
%%% Purpose : Creates an interference graph that tells what temporaries
%%%            interfere with each other.
%%% Created : 5 Feb 2000
%%%----------------------------------------------------------------------

-module(hipe_ig).
-author('Andreas Wallin').

-export([build/2, 
	 adj_set/1, 
	 adj_list/1,      
	 ig_moves/1,
	 degree/1,
	 spill_costs/1,
	 add_edge/4,
	 set_adj_set/2,
	 set_adj_list/2,
	 set_ig_moves/2,
	 set_degree/2,
	 set_spill_costs/2
	]).


-record(igraph, {adj_set, adj_list, ig_moves, degree, spill_costs}).

% Selectors

%%%----------------------------------------------------------------------
% Function:    adj_set, adj_list, degree, spill_costs
%
% Description: Selector functions. Used to get one of the encapsulated 
%               data-structure contained in the IG structure.
% Parameters:
%   IG             --  An interference graph
%
% Returns: 
%   One of the encapsulated data-structures.
%%%----------------------------------------------------------------------
adj_set(IG)     -> IG#igraph.adj_set.
adj_list(IG)    -> IG#igraph.adj_list.
ig_moves(IG)    -> IG#igraph.ig_moves.    
degree(IG)      -> IG#igraph.degree.
spill_costs(IG) -> IG#igraph.spill_costs.
    

%%%----------------------------------------------------------------------
% Function:    set_adj_set, set_adj_list, set_degree, set_spill_costs
%
% Description: Modifier functions. Used to set one of the encapsulated 
%               data-structure contained in the IG structure.
% Parameters:
%   Data-structure --  Data-structure you want to set. An adj_set 
%                       data-structure for example.
%   IG             --  An interference graph
%
% Returns: 
%   An updated interference graph.
%%%----------------------------------------------------------------------
set_adj_set(Adj_set, IG)         -> IG#igraph{adj_set  = Adj_set}.
set_adj_list(Adj_list, IG)       -> IG#igraph{adj_list = Adj_list}.
set_ig_moves(IG_moves, IG)       -> IG#igraph{ig_moves = IG_moves}.
set_degree(Degree, IG)           -> IG#igraph{degree   = Degree}.
set_spill_costs(Spill_costs, IG) -> IG#igraph{spill_costs = Spill_costs}.



%%----------------------------------------------------------------------
% Function:    initial_ig
%
% Description: The initial interference record that we start with when
%               building the interference graph.
% Parameters:
%   No_temporaries --  Number of temporaries that exists in
%                       the CFG we work on. This is because we have some
%                       datastructures built out of vectors.
%
% Returns: 
%   A new interference record
%%----------------------------------------------------------------------
initial_ig(No_temporaries, Target) ->
    #igraph{adj_set     = hipe_adj_set:new(), 
	    adj_list    = hipe_adj_list:new(No_temporaries),
	    ig_moves    = hipe_ig_moves:new(No_temporaries),
	    degree      = hipe_degree:new(No_temporaries, Target),
	    spill_costs = hipe_spillcost:new(No_temporaries)
	   }.



%%%----------------------------------------------------------------------
% Function:    build
%
% Description: Constructs an interference graph for the specifyed CFG.
%
% Parameters:
%   CFG          --  A Control Flow Graph
%   Target       --  The module that contains the target-specific functions
%
% Returns: 
%   An interference graph for the given CFG.
%%%----------------------------------------------------------------------

build(CFG, Target) ->
    BBs_in_out_liveness = Target:analyze(CFG),
    Labels = Target:labels(CFG),
    %% How many temporaries exist?
    No_temporaries = Target:number_of_temporaries(CFG),
    IG = initial_ig(No_temporaries, Target),
    FinalIG = analyze_bbs(Labels, BBs_in_out_liveness, IG, CFG, Target),
    FinalIG.



%%----------------------------------------------------------------------
% Function:    analyze_bbs
%
% Description: Looks up the code that exists in all basic blocks and
%               analyse instructions use and def's to see what 
%               temporaries that interfere with each other.
%
% Parameters:
%   L                    --  A label
%   Ls                   --  Other labels that exits in the CFG
%   BBs_in_out_liveness  --  The in and out liveness on all basic blocks
%   IG                   --  The interference graph in it's current state
%   CFG                  --  The Control Flow Graph that we constructs 
%                            the interference graph from.
%   Target               --  The module containing the target-specific
%                            functions
%
% Returns: 
%   An interference graph for the given CFG.
%%----------------------------------------------------------------------
analyze_bbs([], _, IG, _, _) -> IG;
analyze_bbs([L|Ls], BBs_in_out_liveness, IG, CFG, Target) ->
    % Get basic block associated with label L
    BB = Target:bb(CFG, L),
    % Get basic block code
    BB_code = hipe_bb:code(BB),
    % Temporaries that are live out from this basic block
    BB_liveout = Target:liveout(BBs_in_out_liveness, L),
    % Only temporary numbers
    BB_liveout_numbers = reg_numbers(BB_liveout, [], Target),
    % {Liveness, New Interference Graph}
    {_, New_ig, Ref} = analyze_bb_instructions(BB_code,
					       ordsets:from_list(BB_liveout_numbers),
					       IG,
					       Target),
    Newer_ig = hipe_spillcost:ref_in_bb(Ref, New_ig),
    analyze_bbs(Ls, BBs_in_out_liveness, Newer_ig, CFG, Target).


%%----------------------------------------------------------------------
% Function:    analyze_bb_instructions
%
% Description: Analyzes all instructions that is contained in a basic
%               block in reverse order. 
%
% Parameters:
%   Instruction    --  An instruction
%   Instructions   --  The remaining instructions
%   Live           --  All temporaries that are live at the time.
%                       Live is a set of temporary "numbers only".
%   IG             --  The interference graph in it's current state
%   Target         --  The mopdule containing the target-specific functions
%
% Returns: 
%   Live  --  Temporaries that are live at entery of basic block
%              that we analyze.
%   IG    --  Updated interference graph.
%   Ref   --  Set of temporaries referred to in this bb.
%%----------------------------------------------------------------------

% Ref: set of temporaries referred to in this bb
analyze_bb_instructions([], Live, IG, _) -> {Live, IG, ordsets:new()};
analyze_bb_instructions([Instruction|Instructions], Live, IG, Target) ->
    %% Analyze last instruction first.
    {Live0, IG0, Ref} = analyze_bb_instructions(Instructions, Live, 
						IG, Target),
    %% Check for temporaries that are defined and used in instruction
    {Def, Use} = Target:def_use(Instruction),
    %% Convert to register numbers
    Def_numbers = ordsets:from_list(reg_numbers(Def, [], Target)),
    Use_numbers = ordsets:from_list(reg_numbers(Use, [], Target)),
    Ref_numbers = ordsets:union(Ref, ordsets:union(Def_numbers, Use_numbers)),
    %% Increase spill cost on all used temporaries
    IG1 = hipe_spillcost:inc_costs(Use_numbers, IG0),
    {Live1, IG2} = analyze_move_instruction(Instruction, 
					    Live0, 
					    Def_numbers, 
					    Use_numbers, 
					    IG1, 
					    Target),
    Live2 = ordsets:union(Live1, Def_numbers),
    IG3 = interfere(Def_numbers, Live2, IG2, Target),
    Live3 = ordsets:union(Use_numbers, ordsets:subtract(Live2, Def_numbers)),
    {Live3, IG3, Ref_numbers}.


%%----------------------------------------------------------------------
% Function:    analyze_move_instruction
%
% Description: If a move instructions is discovered, this function is
%               called. It is used to remember what move instructions
%               a temporary is associated with and all moves that exists
%               in the CFG. 
%
% Parameters:
%   Instruction    --  An instruction
%   Live           --  All temporaries that are live at the time.
%                      Live is a set of temporary "numbers only".
%   Def_numbers    --  Temporarys that are defined at this instruction
%   Use_numbers    --  Temporarys that are used at this instruction
%   IG             --  The interference graph in it's current state
%   Target         --  The module containing the target-specific
%                       functions
% Returns: 
%   Live  --  An updated live set
%   IG    --  An updated interference graph
%%----------------------------------------------------------------------

analyze_move_instruction(Instruction, Live, Def_numbers, Use_numbers, IG, 
			 Target) ->
    case (Target:is_move(Instruction) and 
	  is_copy_instruction(Def_numbers, Use_numbers)) of
	true ->
	    New_live = ordsets:subtract(Live, Use_numbers),
	    IG_moves = ig_moves(IG),
	    Copy_instruction = 
		create_copy_instruction(Def_numbers, Use_numbers),
	    IG_moves0 = move_relate(Def_numbers, Copy_instruction, IG_moves),
	    IG_moves1 = move_relate(Use_numbers, Copy_instruction, IG_moves0),
	    IG_moves2 = 
		hipe_ig_moves:add(worklist_moves, Copy_instruction, IG_moves1),	    	    New_IG = set_ig_moves(IG_moves2, IG),
	    {New_live, New_IG};
	false -> 
	    {Live, IG}
    end.



%%----------------------------------------------------------------------
% Function:    is_copy_instruction
%
% Description: After tested that an instruction is a move instruction
%               this function tests if it's a copy instruction. That
%               is, it copys one temporary to another. 
%
% Parameters:
%   [Dest]         --  Destination register number(s) in a move instruction
%   [Source]       --  Source register number(s) in a move instruction
%
% Returns: 
%   true  --  If this is a copy instruction
%   false --  Otherwise
%%----------------------------------------------------------------------
is_copy_instruction([Dest], [Source])->
    true;
is_copy_instruction(_, _)->
    false.


%%----------------------------------------------------------------------
% Function:    create_copy_instruction
%
% Description: Create a copy instruction that looks in a way coalesce 
%               requires it to look like.
%
% Parameters:
%   Dest           --  Destination temporary number
%   Source         --  Source temporary number
%
% Returns: 
%   A move instruction in the format that coalesce wants it.
%%----------------------------------------------------------------------
create_copy_instruction([], Source)->
    {no_move, [], Source};
create_copy_instruction([Dest], [])->
    {no_move, Dest, []};
create_copy_instruction([Dest], [Source])->
    {move, Dest, Source}.


%%----------------------------------------------------------------------
% Function:    move_relate
%
% Description: Used to associate a bunch of temporaries with a
%               move instruction.
%
% Parameters:
%   Temporary      --  A temporary
%   Temporarys     --  A bunch of other temporaries.
%   Instruction    --  An instruction
%   IG_moves       --  A structure that have information about move
%                      instructions.
%
% Returns: 
%   A new IG_moves structure.
%%----------------------------------------------------------------------
move_relate([], _, IG_moves) -> IG_moves;
move_relate([Temporary|Temporarys], Instruction, IG_moves) ->
    New_IG_moves = hipe_ig_moves:add(movelist, Instruction, Temporary, IG_moves),
    move_relate(Temporarys, Instruction, New_IG_moves).
	    

%%----------------------------------------------------------------------
% Function:    interfere
%
% Description: A number of temporaries that are defined interfere with
%               everything in the current live set.
%
% Parameters:
%   Define         --  A Define temporary
%   Defines        --  Rest of temporaries.
%   Live           --  Current live set
%   IG             --  An interference graph
%
% Returns: 
%   An updated interference graph.
%%----------------------------------------------------------------------

interfere([], _, IG, Target) -> IG;
interfere([Define|Defines], Living, IG, Target) ->
    New_ig = interfere_with_living(Define, Living, IG, Target),
    interfere(Defines, Living, New_ig, Target).

%%----------------------------------------------------------------------
% Function:    interfere_with_living
%
% Description: Let one temporary that is in the define set interfere 
%               with all live temporaries.
%
% Parameters:
%   Define         --  A Define temporary
%   Live           --  Current live set
%   Lives          --  Rest of living temporaries.
%   IG             --  An interference graph
%   Target         --  The module containing the target-specific
%                       functions
% Returns: 
%   An updated interference graph
%%----------------------------------------------------------------------

interfere_with_living(_, [], IG, Target) -> IG;
interfere_with_living(Define, [Live|Living], IG, Target) ->
    New_ig = add_edge(Define, Live, IG, Target),
    interfere_with_living(Define, Living, New_ig, Target).



%%%----------------------------------------------------------------------
% Function:    add_edge
%
% Description: Adds an edge to the adj_set data-structure if it's
%               not already a part of it and if U is not precolored
%               we add V to it's adj_list. If V is not precolored
%               we add U ti it's adj_list.
%
% Parameters:
%   U              --  A temporary number
%   V              --  A temporary number
%   Target         --  The module containing the target-specific
%                       functions
% Returns: 
%   An updated interference graph.
%%%----------------------------------------------------------------------
add_edge(U, U, IG, Target) -> IG;
add_edge(U, V, IG, Target) ->
    case hipe_adj_set:adjacent(U, V, adj_set(IG)) of
	true ->
	    IG;
	false ->
	    Adj_set0 = hipe_adj_set:add_edge(U, V, adj_set(IG)),
	    {Adj_list0, Degree0} = interfere_if_uncolored(U, V, adj_list(IG), 
							  degree(IG), Target),
	    {Adj_list1, Degree1} = interfere_if_uncolored(V, U, Adj_list0, 
							  Degree0, Target),
	    IG0 = set_adj_set(Adj_set0, IG),
	    IG1 = set_adj_list(Adj_list1, IG0),
	    set_degree(Degree1, IG1)
    end.


%%----------------------------------------------------------------------
% Function:    interfere_if_uncolored
%
% Description: Let a not precolored temporary interfere with another.
%
% Parameters:
%   Temporary            --  A temporary that is added to the adjacent 
%                             list if it's not precolored.
%   Interfere_temporary  --  Temporary will interfere with 
%                             Interfere_temporary if temporary is not
%                             precolored.
%   Adj_list             --  An adj_list
%   Degree               --  The degree that all nodes currently have
%   Target               --  The module containing the target-specific 
%                            functions
%
% Returns: 
%   Adj_list  --  An updated adj_list data-structure
%   Degree    --  An updated degree data-structure
%%----------------------------------------------------------------------
interfere_if_uncolored(Temporary, Interfere_temporary, Adj_list, Degree, 
		       Target) ->
    case Target:is_precolored(Temporary) of
	
	false ->
	    New_adj_list = hipe_adj_list:add_edge(Temporary, Interfere_temporary, 
					     Adj_list),
	    New_degree   = hipe_degree:inc(Temporary, Degree),
	    {New_adj_list, New_degree};
	true ->
	    {Adj_list, Degree}
    end.


%%----------------------------------------------------------------------
% Function:    reg_numbers
%
% Description: Converts a list of tupple with {something, reg_number}
%               to a list of register numbers.
%
% Parameters:
%   TR             --  A Temorary register
%   TRs            --  A list of temporary registers
%   Reg_numbers    --  The list of reg numbers only that we are
%                       building. An empty list from start.
%   Target         --  The module containing the target-specific functions
% Returns: 
%   A list of register numbers.
%%----------------------------------------------------------------------
reg_numbers([], Reg_numbers, _) -> Reg_numbers;
reg_numbers([TR|TRs], Reg_numbers, Target) -> 
    reg_numbers(TRs, [Target:reg_nr(TR)|Reg_numbers], Target).
