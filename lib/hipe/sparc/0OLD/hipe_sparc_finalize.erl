%%% -*- erlang-indent-level: 2 -*-
%%%----------------------------------------------------------------------
%%% File    : hipe_sparc_finalize.erl
%%% Author  : Christoffer Vikström <chvi3471@student.uu.se>
%%% Purpose : 
%%% Created : 16 Feb 2004 by Christoffer Vikström <chvi3471@student.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_sparc_finalize).
-export([finalize/2]).

-include("hipe_sparc.hrl").

finalize(CFG,Options) ->
  OptCFG = case proplists:get_bool(sparc_peephole,Options) of
	     true  -> hipe_sparc_cfg:remove_trivial_bbs(CFG);
	     false -> CFG
	   end,
  LinearCode = hipe_sparc:sparc_code(hipe_sparc_cfg:linearize(OptCFG)),
  OptLinearCode = hipe_sparc_peephole:peep(LinearCode),
  OptLinearCode2 = fill_delay(OptLinearCode, Options),
  {OptCFG, lists:flatten(OptLinearCode2)}.


%% opt(Code) ->
%%     lists:reverse(opt(Code,[])).

%% opt([G,L|Code], Acc) ->
%%   case hipe_sparc:is_goto(G) of
%%       true ->
%% 	  case hipe_sparc:is_label(L) of
%% 	      true ->
%% 		  case hipe_sparc:goto_label(G) =:= hipe_sparc:label_name(L) of
%% 		      true ->
%% 			  opt(Code,[L|Acc]);
%% 		      false ->
%% 			  opt(Code,[L,G|Acc])
%% 		  end;
%% 	      false ->
%% 		  opt([L|Code], [G|Acc])
%% 	  end;
%%       false ->
%% 	  opt([L|Code], [G|Acc])
%%   end;
%% opt([I],Acc) ->
%%     [I|Acc];
%% opt([],Acc) ->
%%     Acc.

%% %%
%% %% 
%% %%

%% peephole([]) ->
%%   [];
%% peephole([I|Is]) ->
%%    case I of
%%      #move{} ->
%%        case hipe_sparc:move_src(I) =:= hipe_sparc:move_dest(I) of
%% 	 true ->
%% 	   peephole(Is);
%% 	 false ->
%% 	   [I | peephole(Is)]
%%        end;
%%       _ ->
%%        [I | peephole(Is)]
%%    end.

%%
%% Code is a list of instructions. 
%%

fill_delay(Code, Options) ->
  Codes =  split_at_branch(Code),
  case proplists:get_bool(fill_delayslot,Options) of
    true  -> [fill_delay0(Is) || Is <- Codes];
    false -> [nofill_delay(Is) || Is <- Codes]
  end.

nofill_delay(Code) ->
  [Code | [hipe_sparc:nop_create()]].

%%
%% Code is a list of instructions where a branch/jump 
%%

fill_delay0(Code) ->
  try find_delay(Code) of
    {NewCode, _, _, _} ->
      [NewCode | [hipe_sparc:nop_create()]];
    {NewCode, Delay} ->
      [NewCode | [Delay]]
  catch
    throw:no_branch ->
      Code
  end.

%%
%% Extracts a delay instruction from a list 
%%

find_delay([Jmp]) ->
  case hipe_sparc:has_delayslot(Jmp) of
    true ->
      {[Jmp], 
       ordsets:from_list(hipe_sparc:uses(Jmp)), 
       ordsets:from_list(hipe_sparc:defines(Jmp)),
       []};
    false ->
      throw(no_branch)
  end;
find_delay([I|Is]) ->
  case find_delay(Is) of
    {NewIs, Uses, Defs, Loads} ->
      IUses = ordsets:from_list(hipe_sparc:uses(I)),
      IDefs = ordsets:from_list(hipe_sparc:defines(I)),
      NewUses = ordsets:union(Uses, IUses),
      NewDefs = ordsets:union(Defs, IDefs),
      IsStore = hipe_sparc:is_store(I),
      NewLoads = case hipe_sparc:is_load(I) of 
		   true -> [I|Loads];
		   false -> Loads
		 end,
      case is_delay_instr(I) of
	true ->
	  %% Make sure the instruction isn't defining a reg that is 
	  %% used later or uses a reg that is defined later or
	  %% defines a reg that is defined later
	  X = {ordsets:intersection(Uses, IDefs), 
	       ordsets:intersection(Defs, IUses),
	       ordsets:intersection(Defs, IDefs)},
	  case X of
	    {[], [], []} ->  %% No register conflicts
	      case {IsStore,Loads} of
		{_,[]} -> % No following loads
		  {NewIs, I}; % found a delay instr.
		{false,_} -> % This is not a store
		  {NewIs, I}; % found a delay instr.
		_ -> %% A store with following loads.
		  %% XXX: TODO check whether the store 
		  %%      and the load REALY conflicts.
		  %%      (Stack and Heap does not conflict)
		  {[I|NewIs], NewUses, NewDefs, NewLoads} %% Search more
	      end;
	    _ -> %% Register conflicts.
	      {[I|NewIs], NewUses, NewDefs, NewLoads}
	  end;
	false ->
	  {[I|NewIs], NewUses, NewDefs, NewLoads}
      end;
    {NewIs, Delay} ->
      {[I|NewIs], Delay}
  end.

%%
%% true if I is an instruction that can be moved to a delay slot
%%

is_delay_instr(I) ->
  case I of
    #label{} -> false;
    #comment{} -> false;
    #load_address{} -> false;
    #load_atom{} -> false;
    #load_word_index{} -> false;
    #fop{} -> false;
    %%         in the delayslot can slow down code...
    %%         ... but it can also speed up code.
    %%         the impact is about 10 - 20 % on small bms
    %%         on the average you loose 1-2 % by not putting
    %%         loads in delayslots
    %% #load{} -> false;
    _ -> true
  end.

%%
%% Split a list of instructions to a list of lists of instructions
%% where each sublist ends with a branch.
%%

split_at_branch([]) ->
   [];
split_at_branch([I|Rest]) ->
  case Rest of
    [L|_] ->
      case hipe_sparc:is_label(L) of
	true -> 
	  Lname = hipe_sparc:label_name(L),
	  split(I,Lname,Rest);
	false ->
	  split(I,[],Rest)
      end;
    _ ->
      split(I,[],Rest)
  end.

split(I,Lname,Is)->
  case I of
    #b{} -> 
      case hipe_sparc:b_false_label(I) of
	Lname ->
	  [[hipe_sparc:b_false_label_update(I,[])] |
	   split_at_branch(Is)];
	FL ->
	  [[hipe_sparc:b_false_label_update(I,[])], 
	   [hipe_sparc:goto_create(FL)]|
	   split_at_branch(Is)]
      end;
%%  #br{} -> 
%%       case hipe_sparc:br_false_label(I) of
%% 	Lname ->
%% 	  [[hipe_sparc:br_false_label_update(I,[])] |
%% 	   split_at_branch(Is)];
%% 	FL ->
%% 	  [[hipe_sparc:br_false_label_update(I,[])], 
%% 	   [hipe_sparc:goto_create(FL)]|
%% 	   split_at_branch(Is)]
%%       end;
    #goto{} -> 
      [[I] | split_at_branch(Is)];
    #jmp{} -> 
      [[I] | split_at_branch(Is)];
    #jmp_link{} -> 
      [[I] | split_at_branch(Is)];
    #call_link{} ->
%%      [[I] | split_at_branch(Is)];
      case hipe_sparc:call_link_continuation(I) of
	[] -> 
%%	  NL = hipe_sparc:label_create_new(),
%%	  NLname = hipe_sparc:label_name(NL),
	 [[I] | split_at_branch(Is)];
%%	  [[hipe_sparc:goto_create(NLname)]|
%%	   split_at_branch([NL|Is])]];
	Lname -> 
	  [[hipe_sparc:call_link_continuation_update(I,[])] |
	   split_at_branch(Is)];
	CCL ->
	  [[hipe_sparc:call_link_continuation_update(I,[])],
	   [hipe_sparc:goto_create(CCL)]|
	   split_at_branch(Is)]
      end;
    #label{} ->
      [[I] | split_at_branch(Is)];
    _ -> 
      [Same|Lists] = split_at_branch(Is),
      [[I|Same]|Lists]
  end.
