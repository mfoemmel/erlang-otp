%%% -*- Erlang -*-
%%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			 CONTROL FLOW GRAPHS
%
% Construct and manipulate the control flow graph of a function (program?).
%
% Exports:
% ~~~~~~~~
%  init(Code) - makes a cfg out of code.
%  bb(CFG, Label) - returns the basic block named 'Label' from the cfg.
%  bb_update(CFG, Label, NewBB) - makes NewBB the basic block associated
%       with Label.
%  succ_map(CFG) - returns a mapping from labels to succesors.
%  succ(Map, Label) - returns a list of successors of basic block 'Label'.
%  pred_map(CFG) - returns a mapping from labels to predecessors.
%  pred(Map, Label) - returns the predecessors of basic block 'Label'.
%  fallthrough(CFG, Label) - returns fall-through successor of basic 
%       block 'Label' (or 'none').
%  cond(CFG, Label) - returns conditional successor (or 'none')
%  start(CFG) - returns the label of the entry basic block.
%  fail_entrypoints/1,
%  params(CFG) - returns the list of parameters to the cfg.
%  labels(CFG) - returns a list of labels of all basic blocks in the cfg.
%  postorder(CFG) - returns a list of labels in postorder.
%  reverse_postorder(CFG) - returns a list of labels in reverse postorder.
%  linearize(CFG) - converts cfg to icode
%  var_range(CFG) - {Min, Max}
%  label_range(CFG) - {Min, Max}
%
%  other_entrypoints(CFG) - gives a list of labels that are refered to
%                           from the data section.

-record(cfg, {hashtable, info, data}).
-record(cfg_info, {start_label,
		   fail_entrypoints=[],
		   var_range,
		   label_range,
		   extra,
		   info=[]}).

-export([init/1,
	 data/1,
	 update_data/2,
	 update_code/2,
	 labels/1,
	 fail_entrypoints/1,
	 other_entrypoints/1,
	 add_fail_entrypoint/2,
	 is_entry/2,
	 info/1,
	 info_update/2,
	 start/1,    % deprecated
	 start_label/1,
	 start_label_update/2,  
	 succ/2,
	 succ_map/1,
	 pred/2,
	 pred_map/1,
	 fallthrough/2,
	 cond/2,
	 depth_first_ordering/1,
	 postorder/1,
	 reverse_postorder/1,
	 preorder/1,
	 reverse_preorder/1,
	 breadthorder/1,
	 reverse_breadthorder/1,
	 bb/2,
	 bb_add/3,
	 bb_update/3,
	 bb_remove/2,
	 redirect/4,
	 var_range/1,
	 var_range_update/2,
	 label_range/1,
	 label_range_update/2,
	 info/1,
	 info_add/2,
	 info_update/2,
	 extra_update/2,
	 pp/1]).


-export([none_visited/1,
	 visit/2,
	 visited/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface functions that MUST be implemented in the including file
%
%
% init(Code) -> CFG, constructs the cfg.
% is_fail_entrypoint(Label) -> bool(), true if a label is an fail entrypoint.
% is_label(Instr) -> bool(), true if instruction is a label.
% label_name(Instr) -> term(), the name of a label.
% branch_successors(Instr) -> [term()], the successors of a branch.
% is_branch(Instr) -> bool(), true if instruction is a branch.
% redirect_jmp(Jmp, ToOld, ToNew) -> NewJmp, 
% pp(CFG) -> ok, do some nifty output.
% linearize(CFG) -> 
% mk_goto(Label) -> instruction
%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Primitives (not all of these are exported)
%

mk_empty_cfg(StartLbl, Vrange, Lrange, Data, Extra) ->
  Info = #cfg_info{start_label=StartLbl,
		    var_range=Vrange,
		    label_range=Lrange,
		    extra=Extra},
   #cfg{hashtable=hipe_hash:empty(), data=Data, info=Info}.


start(CFG) -> (CFG#cfg.info)#cfg_info.start_label.
start_label(CFG) -> (CFG#cfg.info)#cfg_info.start_label.
start_label_update(CFG, NewStartLabel) ->
   Info = CFG#cfg.info,
   CFG#cfg{info=Info#cfg_info{start_label=NewStartLabel}}.

%% Don't forget to do a start_label_update if neccessary.
update_code(CFG, NewCode) ->
  take_bbs(NewCode, CFG).


data(CFG) -> CFG#cfg.data.
update_data(CFG, D) ->
   CFG#cfg{data=D}.

extra(CFG) -> (CFG#cfg.info)#cfg_info.extra.
extra_update(CFG, NewExtra) ->
   Info = CFG#cfg.info,
   CFG#cfg{info=Info#cfg_info{extra=NewExtra}}.

info(CFG) -> (CFG#cfg.info)#cfg_info.info.
info_add(CFG, A) ->
   As = info(CFG),
   Info = CFG#cfg.info,
   CFG#cfg{info=Info#cfg_info{info=[A|As]}}.
info_update(CFG, I) ->
   Info = CFG#cfg.info,
   CFG#cfg{info=Info#cfg_info{info=I}}.


var_range(CFG) -> (CFG#cfg.info)#cfg_info.var_range.
var_range_update(CFG, NewRange) ->
   Info = CFG#cfg.info,
   CFG#cfg{info=Info#cfg_info{var_range=NewRange}}.


label_range(CFG) -> (CFG#cfg.info)#cfg_info.label_range.
label_range_update(CFG, NewRange) ->
   Info = CFG#cfg.info,
   CFG#cfg{info=Info#cfg_info{label_range=NewRange}}.


fail_entrypoints(CFG) -> (CFG#cfg.info)#cfg_info.fail_entrypoints.

other_entrypoints(CFG) ->
  hipe_consttab:referred_labels(data(CFG)).

add_fail_entrypoint(CFG, EP) ->
   Info = CFG#cfg.info,
   OEP = Info#cfg_info.fail_entrypoints,
   CFG#cfg{info=Info#cfg_info{fail_entrypoints=[EP|OEP]}}.


is_entry(Lbl, CFG) ->
   lists:member(Lbl, [start(CFG)|fail_entrypoints(CFG)]) or
    lists:member(Lbl, other_entrypoints(CFG)).


bb_add(CFG, Label, NewBB) ->
   Last = hipe_bb:last(NewBB),
   case is_branch(Last) of
      false -> 
       throw({cfg, {"Basic block ends without branch", Last}});
      true -> ok
   end,
   Succ = branch_successors(Last),
   CFGBlock = {NewBB, Succ},
   HT = CFG#cfg.hashtable,
   CFG#cfg{hashtable=hipe_hash:update(Label, CFGBlock, HT)}.


bb_remove(CFG, Label) ->
   HT = CFG#cfg.hashtable,
   CFG#cfg{hashtable=hipe_hash:delete(Label, HT)}.


bb_update(CFG, Label, NewBB) ->
   bb_add(CFG, Label, NewBB).


bb(CFG, Label) ->
   HT = CFG#cfg.hashtable,
   case hipe_hash:lookup(Label, HT) of
      {found, {Block, Succ}} ->
	 Block;
      not_found ->
	 not_found
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Construct a cfg from a list of instructions
%

take_bbs([], CFG) ->
    CFG;
take_bbs(Xs, CFG) ->
    Lbl = hd(Xs),
    case is_label(Lbl) of
	true ->
	    CFG1 = case is_fail_entrypoint(Lbl) of
		       true ->
			   add_fail_entrypoint(CFG, label_name(Lbl));
		       false ->
			   CFG
		   end,
	    case take_bb(tl(Xs), []) of
		{Code, Rest} ->
		    NewCFG = bb_add(CFG1, label_name(Lbl),
				    hipe_bb:mk_bb(Code, label_annot(Lbl))),
		    take_bbs(Rest, NewCFG)
	    end;
	false ->
	    exit({cfg, "basic block doesn't start with a label", Xs})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Take_bb returns:
%   - {Code, Rest}.
%      * Code is a list of all the instructions.
%      * Rest is the remainder of the instructions

take_bb([], Code) ->
    {lists:reverse(Code), []};
take_bb([X, Y |Xs], Code) ->
    case is_branch(X) of
	true ->
	    case is_label(Y) of
		true ->
		    {lists:reverse([X|Code]), [Y|Xs]};
		false ->
		    %% This should not happen...
		    %% move the problem to the next BB.
		    {lists:reverse([X|Code]), [Y|Xs]}
	    end;
	false -> %% X not branch
	    case is_label(Y) of
		true ->
		    {lists:reverse([mk_goto(label_name(Y)),X|Code]),
		     [Y|Xs]};
		false ->
		    take_bb([Y|Xs], [X|Code])
	    end
    end;
take_bb([X], []) ->
    case is_label(X) of 
	true -> 
	    %% We don't want the CFG to just end with a label...
	    %% We loop for ever instead...
	    {[X, mk_goto(label_name(X))],[]};
	false ->
	    {[X],[]}
    end;
take_bb([X], Code) ->
    case is_label(X) of 
	true -> 
	    %% We don't want the CFG to just end with a label...
	    %% We loop for ever instead...
	    {lists:reverse(Code), [X, mk_goto(label_name(X))]};
	false ->
	    {lists:reverse([X|Code]),[]}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Functions for extracting the names of the basic blocks in various
% orders.
%

labels(CFG) ->
    HT = CFG#cfg.hashtable,
    List = hipe_hash:list(HT),
    lists:map(fun(T)->element(1, T) end, List).

depth_first_ordering(CFG) ->
    Start = start(CFG),
    Succ = succ_map(CFG),
    {Ordering, Visited} =
	depth_search([Start|other_entrypoints(CFG)]++fail_entrypoints(CFG), none_visited(),  Succ,
		     []),
    Ordering.

depth_search([N|Ns], Visited, Succ, Acc) ->
    case visited(N, Visited) of 
	true ->
	    depth_search(Ns, Visited, Succ, Acc);
	false ->
	    {Order, Vis} = 
		depth_search(succ(Succ, N), visit(N, Visited),
			     Succ, Acc),
	    depth_search(Ns, Vis, Succ, [N | Order])
    end;
depth_search([], Visited, _, Ordering) -> {Ordering, Visited}.

reverse_postorder(CFG) ->
  depth_first_ordering(CFG).

postorder(CFG) ->
  lists:reverse(depth_first_ordering(CFG)).


% post(X, Vis, Succ, PO) ->
%    case visited(X,Vis) of 
%       true ->
% 	 {Vis, PO};
%       false ->
% 	 post_list(succ(Succ,X), visit(X,Vis), Succ, [X|PO])
%    end.
% 
% post_list([], Vis, Succ, PO) -> 
%    {Vis, PO};
% post_list([X|Xs], Vis, Succ, PO) ->
%    {Vis1, PO_1} = post(X, Vis, Succ, PO),
%    post_list(Xs, Vis1, Succ, PO_1).


reverse_preorder(CFG) ->
    postorder(CFG).

preorder(CFG) ->
    reverse_postorder(CFG).

breadthorder(CFG) ->
    lists:reverse(reverse_breadthorder(CFG)).

reverse_breadthorder(CFG) ->
    Start = start(CFG),
    Succ = succ_map(CFG),
    {Vis, RBO1} = breadth_list([Start], none_visited(), Succ, []),
    {Vis1, RBO2} = breadth_list(other_entrypoints(CFG), Vis, Succ, RBO1),
    {_, RBO3} = breadth_list(fail_entrypoints(CFG), Vis1, Succ, RBO2),
    RBO3.

breadth_list([X|Xs], Vis, Succ, BO) ->
    case visited(X,Vis) of 
	true ->
	    breadth_list(Xs, Vis, Succ, BO);
	false ->
	    breadth_list(Xs ++ succ(Succ,X), visit(X,Vis), Succ, [X|BO])
    end;  
breadth_list([], Vis, Succ, BO) -> 
    {Vis, BO}.


none_visited(CFG) -> 
    %% Preparation for implementation with updatable struct
    none_visited().

none_visited() -> 
    gb_sets:empty().

visit(X, Vis) -> 
    gb_sets:add(X, Vis).

visited(X, Vis) ->
    gb_sets:is_member(X, Vis).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 
%

succ_map(CFG) ->
   CFG.

succ(SuccMap, Label) ->
   HT = SuccMap#cfg.hashtable,
   case hipe_hash:lookup(Label, HT) of
     {found, {Block, Succ}} ->
       Succ;
     _ -> exit({successor_not_found,Label,SuccMap})
   end.


pred_map(CFG) ->
   Labels = labels(CFG),
   HT = hipe_hash:init(map_to(Labels, [])),
   add_pred(Labels, succ_map(CFG), HT).

map_to([],Y) -> [];
map_to([X|Xs],Y) -> [{X,Y}|map_to(Xs,Y)].


add_pred([], SuccMap, HT) ->
   HT;
add_pred([L|Ls], SuccMap, HT) ->
   SuccList = succ(SuccMap, L),
   NewHT = add_succ_pred(L, SuccList, HT),
   add_pred(Ls, SuccMap, NewHT).


add_succ_pred(Lbl, [], HT) ->
   HT;
add_succ_pred(Lbl, [S|Ss], HT) ->
   {found, SPred} = hipe_hash:lookup(S, HT),
   add_succ_pred(Lbl, Ss, hipe_hash:update(S, [Lbl|SPred], HT)).


pred(Map, Label) ->
   {found, Pred} = hipe_hash:lookup(Label, Map),
   Pred.


fallthrough(CFG, Label) ->
  HT = CFG#cfg.hashtable,
  case hipe_hash:lookup(Label, HT) of
    {found, {Block, Succ}} ->
      case Succ of
	[] -> none;
	[X] -> X;
	[X, Y] -> Y;
	  _ -> none

      end;
    _ ->
      exit({cfg, {"fallthrough label not found",Label}})
  end.


cond(CFG, Label) ->
   HT = CFG#cfg.hashtable,
   {found, {Block, Succ}} = hipe_hash:lookup(Label, HT),
   case Succ of
      [] -> none;
      [_] -> none;
      [X, Y] -> X;
      Labels -> Labels
   end.


redirect(CFG, From, ToOld, ToNew) ->
   Code = hipe_bb:code(bb(CFG, From)),
   Last = lists:last(Code),
   NewLast = redirect_jmp(Last, ToOld, ToNew),
   bb_update(CFG, From, hipe_bb:mk_bb(butlast(Code)++[NewLast])).

butlast([X|Xs]) -> butlast(Xs,X).

butlast([],X) -> [];
butlast([X|Xs],Y) -> [Y|butlast(Xs,X)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Linearize the code in a cfg. Returns a list of instructions
%

linearize_cfg(CFG) ->
   Start = start(CFG),
   Vis = none_visited(),
   {Vis0, NestedCode} = lin_succ(Start, CFG, Vis),
   BlocksInData = hipe_consttab:referred_labels(data(CFG)),
   CatchBlocks = fail_entrypoints(CFG),
   AllCode = lin_other_entries(NestedCode, CFG, BlocksInData ++ CatchBlocks, Vis0),
   lists:flatten(AllCode).


lin_succ(none, CFG, Vis) ->
   {Vis, []};
lin_succ([Label|Labels], CFG, Vis) ->
    {Vis1, Code1} = lin_succ(Label, CFG, Vis),
    {Vis2, Code2} = lin_succ(Labels, CFG, Vis1),
    {Vis2, [Code1,Code2]};
lin_succ([], CFG, Vis) ->
    {Vis, []};
lin_succ(Label, CFG, Vis) ->
   case visited(Label, Vis) of
      true ->
	 {Vis, []};      % already visited
      false ->
	 Vis0 = visit(Label, Vis),
	 Block = bb(CFG, Label),
	 Fallthrough = fallthrough(CFG, Label),
	 Cond = cond(CFG, Label),
	 LblInstr = mk_label(Label, hipe_bb:annot(Block)),
	 {Vis1, Code1} = lin_succ(Fallthrough, CFG, Vis0),
	 {Vis2, Code2} = lin_succ(Cond, CFG, Vis1),
	 {Vis2, [[LblInstr|hipe_bb:code(Block)], Code1, Code2]}
   end.


lin_other_entries(Code, CFG, [], Vis) ->
   Code;
lin_other_entries(Code, CFG, [E|Es], Vis) ->
   {Vis0, MoreCode} = lin_succ(E, CFG, Vis),
   lin_other_entries([Code, MoreCode], CFG, Es, Vis0).
