%%% $Id$
%%% File: hipe_x86_ra
%%% Description: Aggresive register allocation for the x86
%%% Implementing a Graph coloring register allocator with iterative coalescing
%%% Copyright (C) 2001 Ulf Magnusson
%%% Email: ulf.magnusson@ubm-computing.com

-module(hipe_x86_ra).
-export([ra/1]).
-include("hipe_x86.hrl").
-include("../main/hipe.hrl").

-define(load_cost,1).
-define(write_cost,1).

ra(CFG) ->
    ra( CFG, first ).

% First time we make dead copy elimination as well
ra(CFG,first) ->
    % Build should also return MovesWorkList
    Ig = build(CFG),
    {SimplifyWorkList,FreezeWorkList,SpillWorkList} = mk_work_lists(),
    dead_copy_elimination(),

    % Run simplify,coalesce,freeze,potential_spill until all lists are empty
    simplify( SimplifyWorkList ),
    %coalesce( MovesWorkList ),
    freeze( FreezeWorkList ),
    potential_spill( SpillWorkList ),

    select(), % If no spilling occured we are done.
    actual_spill(),
    io:format("hej"),
    Ig;

ra(CFG,_) ->
    build(CFG).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% BUILD FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function builds the interference graph
% 1. compute defs(v), uses(v) and spillcost(v) for all vars
%    by scanning the program
% 2. Calculate liveness information for all 'u' which uses('v')
build( CFG ) ->
    %Live = hipe_x86_liveness:analyse(hd(CFG)),
    Code0 = hipe_x86:defun_code( hipe_x86_cfg:linearise( CFG ) ),
    DefsEmpty = empty_data_set(),
    UsesEmpty = empty_data_set(),
    {Freq, Defs, Uses, LastInstr} = preprocess_code( Code0, [], 1, DefsEmpty, UsesEmpty, 0, [] ),
    {Minvar, Maxvar} = hipe_x86_cfg:var_range( CFG ),
    calc_spill_cost( Maxvar, Minvar, Freq, Defs, Uses, [] ),
    % make tuple with a sequence of all instr
    Instrs = list_to_tuple( Code0 ),
    % Build pred map
    PredMap = hipe_x86_cfg:pred_map( CFG ),
    {Minlab,Maxlab} = hipe_x86_cfg:label_range( CFG ),
    InstrPredMap = create_pred_map( Minlab, Maxlab, LastInstr, PredMap, [] ),
    % calc liveness information...
    %hipe_x86_cfg:pp(CFG),
    lists:foldl(fun(V,Ig)->
      lists:foldl(fun(U,AccIg)->
			  live_in(V,U,empty_vis(),InstrPredMap,Instrs,AccIg,Defs) end, 
		  Ig, defuse_lookup(V,Uses) ) end, empty_ig(),
		lists:seq(Minvar,Maxvar)).
        
live_in( V,U,Visited,InstrPredMap, Instrs, Ig, Defs ) ->
    %io:format("live_in V: ~w, U: ~w, Ig: ~w\n", [V,U,Ig]),
    case visited( U,Visited ) of
	false ->
	    NewVisited = visit(U,Visited),

	    I = curinstr(U,Instrs),
	    case hipe_x86:is_label( I ) of
		true ->
		    Preds = find_pred_instrs(hipe_x86:label_label(I),InstrPredMap),
		    lists:foldl(fun(X,Acc) -> 
				live_out(V,X,NewVisited,InstrPredMap, Instrs, Acc, Defs) end, 
				Ig, Preds );
		_ ->
		    live_out(V,U-1,NewVisited,InstrPredMap, Instrs, Ig, Defs)
	    end;
	_ ->
	    Ig
    end.

find_pred_instrs(L,InstrPredMap) ->
    case lists:keysearch( L, 1, InstrPredMap ) of
	{value,{_,V}} ->
	    V;
	_ ->
	    ?EXIT({badinstrpredmap,L,InstrPredMap})
    end.


visited( X,Vis ) ->
    lists:member(X,Vis).

visit( X,Vis ) ->
    [X|Vis].

empty_vis() ->
    [].

curinstr(U,Instrs) ->
    element(U,Instrs).

live_out( V,U,Visited,InstrPredMap, Instrs, Ig, Defs ) ->
    %io:format("live_out V: ~w, U: ~w, Ig: ~w\n", [V,U,Ig]),
    Udefs = lists:map( fun(X)->hipe_x86:temp_reg(X) end, hipe_x86_defuse:def(curinstr(U,Instrs)) ),
    Ig1 = lists:foldl( fun(Vprim,Acc) -> 
			 case Vprim == V of
			     true ->
				 Acc;
			     _ ->
				 add_edge(V,Vprim,Ig)
			 end
		 end,
		 Ig, Udefs ),
    case lists:member(V,Udefs) of
	true ->
	    Ig1;
	_ ->
	    live_in(V,U,Visited,InstrPredMap, Instrs, Ig1, Defs )
    end.

add_edge( V,Vprim,Ig ) ->
    case lists:member({V,Vprim},Ig) of
	true ->
	    Ig;
	_ ->
	    [{V,Vprim}|Ig]
    end.

empty_ig() -> 
    [].

create_pred_map( Minlab, Maxlab, LastInstr, PredMap, Acc ) when Minlab =< Maxlab ->    
    create_pred_map( Minlab+1, Maxlab, LastInstr, PredMap, 
		     [{Minlab,lists:map(fun(X) -> find_last(X,LastInstr) end, 
					hipe_x86_cfg:pred(PredMap,Minlab) )}|Acc]);
create_pred_map( Minlab, Maxlab, LastInstr, PredMap, Acc ) ->
    Acc.

find_last( X, LastInstr ) ->
    case lists:keysearch( X, 1, LastInstr ) of
	{value,{_,V}} ->
	    V;
	_ ->
	    ?EXIT(badlastinstr)
    end.
						

calc_spill_cost( Maxvar, Minvar, Freq, Defs, Uses, Acc ) when Maxvar >= Minvar ->
    calc_spill_cost( Maxvar-1, Minvar, Freq, Defs, Uses, 
		     [spill_cost( Maxvar, Freq, Defs, Uses )|Acc]);
calc_spill_cost( Maxvar, Minvar, Freq, Defs, Uses, Acc ) ->
    list_to_tuple( lists:reverse( Acc ) ).
    

% Help function to build
% Calculates the Defs and Uses for all instructions
preprocess_code( [], DefsAndUsesList, Num, Defs, Uses, Curblock, LastInstr ) ->
    NewList = lists:reverse( DefsAndUsesList ),
    NewLastInstr = lists:sort( LastInstr ),
    % rebuild to tuple, by traversing the list...

    {list_to_tuple( NewList ),Defs, Uses, NewLastInstr };
preprocess_code( [Insn|Insns], List, Num, Defs, Uses, Curblock, LastInstr ) ->
    Def = hipe_x86_defuse:def( Insn ),
    Use = hipe_x86_defuse:use( Insn ),
    Freq = 1, % TODO
    NewDefs = add_defuses( Num, Def, Defs ),
    NewUses = add_defuses( Num, Use, Uses ),
    Newblock = case hipe_x86:is_label( Insn ) of
		   true ->
		       hipe_x86:label_label(Insn);
		   _ ->
		       Curblock
	       end,
    NewLastInstr = case hipe_x86_cfg:is_branch( Insn ) of
		       true ->
			   [{Curblock,Num}|LastInstr];
		       _ ->
			   LastInstr
		   end,
    preprocess_code( Insns, [Freq|List], Num+1, NewDefs, NewUses, Newblock, NewLastInstr ).

% Help function insert defines into data structure
add_defuses( Num, Defs, Data ) ->
    lists:foldl( fun(X,Acc) -> add_defuse( Num, hipe_x86:temp_reg(X), Acc ) end, Data, Defs ).

% Help functions to add_defs
add_defuse( Num, Def, Data ) ->
    case gb_trees:lookup( Def, Data ) of
	none ->
	    gb_trees:insert( Def, [Num], Data );
	{value,List} ->
	    gb_trees:update( Def, [Num|List], Data )
    end.

empty_data_set() ->
    gb_trees:empty().

freq( Num, Data ) ->
    element(Num, Data).

defuse_lookup( V, Data ) ->
    case gb_trees:lookup( V, Data ) of
	none ->
	    [];
	{value,List} -> List
    end.

% spill_cost calculates the cost for each Live range
spill_cost( V, Freq, Defs, Uses ) ->
    DefList = defuse_lookup( V, Defs ),
    UseList = defuse_lookup( v, Uses ),
    Ucost = lists:foldl( fun(X,Acc) -> Acc+?load_cost*freq(X,Freq) end, 0, UseList ),
    lists:foldl( fun(X,Acc) -> Acc+?write_cost*freq(X,Freq) end, Ucost, DefList ).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% MAKE WORK LISTS FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This functions generates the worklist in the following order
% SimplifyWorkList,FreezeWorkList,SpillWorkList
mk_work_lists( ) ->
    Init = hipe_x86:phys_regs(),
    
    {[],[],[]}.
			  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% DEAD COPY ELIMINATION FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This function eliminates dead copies from the program
% A copy [dst <- src] is dead if 'dst' not are used.
dead_copy_elimination() ->
    notfinished.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% SIMPLIFY FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
simplify( SimplifyWorkList ) ->
    notfinished.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% COALESCE FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Coalesce moves in decreasing order of its cost.
% Cost is defined as execution frequence in the program.
coalesce( MovesWorkList ) ->
    notfinished.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% FREEZE FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Freeze the node with the lowest freeze cost
% Freeze cost(v) = sum of all uncoalesced moves assoc. with v
freeze( FreezeWorkList ) ->
    notfinished.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% POTENTIAL SPILL FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
potential_spill( SpillWorkList ) ->
    notfinished.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% SELECT FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select() ->
    notfinished.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL SPILL FUNCTIONS %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actual_spill() ->
    spill_coalescing(),
    spill_propagation(),
    spill_coloring(),
    emit_spill_code(),
    notfinished.

% Help function to actual_spill
spill_coalescing() ->
    notfinished.

% Help function to actual_spill
spill_propagation() ->
    notfinished.

% Help function to actual_spill
spill_coloring() ->
    notfinished.

% Help function to actual_spill
emit_spill_code() ->
    notfinished.
