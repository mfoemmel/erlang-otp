%% -*- Erlang -*-
%% -*- erlang-indent-level: 2 -*-
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			 CONTROL FLOW GRAPHS
%%
%% Construct and manipulate the control flow graph of a function (program?).
%%
%% Exports:
%% ~~~~~~~~
%%  init(Code) - makes a cfg out of code.
%%  bb(CFG, Label) - returns the basic block named 'Label' from the cfg.
%%  bb_update(CFG, Label, NewBB) - makes NewBB the basic block associated
%%       with Label.
%%  succ_map(CFG) - returns a mapping from labels to succesors.
%%  succ(Map, Label) - returns a list of successors of basic block 'Label'.
%%  pred_map(CFG) - returns a mapping from labels to predecessors.
%%  pred(Map, Label) - returns the predecessors of basic block 'Label'.
%%  fallthrough(CFG, Label) - returns fall-through successor of basic 
%%       block 'Label' (or 'none').
%%  conditional(CFG, Label) - returns conditional successor (or 'none')
%%  start(CFG) - returns the label of the entry basic block.
%%  params(CFG) - returns the list of parameters to the cfg.
%%  labels(CFG) - returns a list of labels of all basic blocks in the cfg.
%%  postorder(CFG) - returns a list of labels in postorder.
%%  reverse_postorder(CFG) - returns a list of labels in reverse postorder.
%%  linearize(CFG) - converts cfg to linearized code
%%  var_range(CFG) - {Min, Max}
%%  label_range(CFG) - {Min, Max}
%%  remove_dead_code(CFG) - removes empty BBs.
%%  remove_unreachable_code(CFG) - removes unreachable BBs.
%%
%%  other_entrypoints(CFG) - gives a list of labels that are refered to
%%                           from the data section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TODO:
%%       Remove fail-entries. -- still needed by last part of x86.
%%

-record(cfg, {hashtable, info, data}).
-record(cfg_info, {'fun',
		   start_label,
		   var_range,
		   label_range,
		   closure,
		   leaf,
		   params,
		   extra,
		   info=[],
		   %% These are only used by the last stage of 
                           %% the x86 backend - after frame.
		   fail_entrypoints=[]}).

-export([init/1,
	 data/1,
	 update_data/2,
	 update_code/2,
	 labels/1,
	 other_entrypoints/1,
	 is_entry/2,
	 is_leaf/1,
	 info/1,
	 info_update/2,
	 start/1,    % deprecated
	 start_label/1,
	 start_label_update/2,  
	 succ/2,
	 succ_map/1,
	 pred/2,
	 pred_map/1,
	 pred_remove/3,
	 fallthrough/2,
	 conditional/2,
	 depth_first_ordering/1,
	 postorder/1,
	 inorder/1,
	 reverse_inorder/1,
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
	 extra/1,
	 extra_update/2,
	 params_update/2,
	 pp/1,
	 remove_dead_code/1,
	 remove_unreachable_code/1]).

-export([none_visited/1,
	 visit/2,
	 visited/2]).

-compile(inline). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface functions that MUST be implemented in the including file
%
%
% init(Code) -> CFG, constructs the cfg.
% is_label(Instr) -> bool(), true if instruction is a label.
% label_name(Instr) -> term(), the name of a label.
% branch_successors(Instr) -> [term()], the successors of a branch.
% is_branch(Instr) -> bool(), true if instruction is a branch.
% is_comment(Instr) -> bool(), true if instruction is a comment,
%                          used by remmove dead code.
% is_goto(Instr) -> bool(), true if instruction is a pure goto, 
%                          used by remove dead code.
% redirect_jmp(Jmp, ToOld, ToNew) -> NewJmp, 
% redirect_ops(Labels, CFG, Map) -> CFG.
%                                   Rewrite instructions with labels
%                                   in operands to use the new label
%                                   as given by map.
%                                   Use find_new_label(OldLab,Map) to
%                                   get the new label.
%                                   (See hipe_sparc_cfg for example)
% pp(CFG) -> ok, do some nifty output.
% linearize(CFG) -> 
% mk_goto(Label) -> instruction
%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Primitives (not all of these are exported)
%%

mk_empty_cfg(Fun, StartLbl, Vrange, Lrange, Data, Closure, Leaf, Params, Extra) ->
  Info = #cfg_info{
    'fun' = Fun,
    start_label=StartLbl,
    var_range=Vrange,
    label_range=Lrange,
    closure=Closure,
    leaf=Leaf,
    params=Params,
    extra=Extra},
  #cfg{hashtable=hipe_hash:empty(), data=Data, info=Info}.


start(CFG) -> (CFG#cfg.info)#cfg_info.start_label.
start_label(CFG) -> (CFG#cfg.info)#cfg_info.start_label.
start_label_update(CFG, NewStartLabel) ->
   Info = CFG#cfg.info,
   CFG#cfg{info=Info#cfg_info{start_label=NewStartLabel}}.

function(CFG) -> (CFG#cfg.info)#cfg_info.'fun'.
is_closure(CFG) -> (CFG#cfg.info)#cfg_info.closure.
is_leaf(CFG) -> (CFG#cfg.info)#cfg_info.leaf.
params(CFG) -> (CFG#cfg.info)#cfg_info.params.

params_update(CFG, NewParams) ->
  Info = CFG#cfg.info,
  CFG#cfg{info=Info#cfg_info{params=NewParams}}.


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

%% Note this function does not seem to be used...
is_entry(Lbl, CFG) ->
    Lbl =:= start(CFG) orelse
	lists:member(Lbl, other_entrypoints(CFG)).


bb_add(CFG, Label, NewBB) ->
   Last = hipe_bb:last(NewBB),
   case is_branch(Last) of
      false -> 
       throw({cfg, {"Basic block ends without branch", Last}});
      true -> ok
   end,
  Succ = branch_successors(Last),
  %% Special handliing of the case when a BB is its own predecessor
  SelfPreds = 
    case lists:member(Label,Succ) of
      true -> [Label];
      false -> []
    end,

  HT = CFG#cfg.hashtable,
  {HT1,CFGBlock, AddPreds} = 
    case hipe_hash:lookup(Label, HT) of
      {found, {_Block, OldSucc, Preds}} ->
	%% Delete this block as pred of old successors.
	HTtemp = 
	  lists:foldl(fun (S,HTAcc) ->
			remove_pred(HTAcc, S, Label)
		    end,
		    HT,
		    OldSucc -- Succ),

	{HTtemp,
	 {NewBB, Succ, SelfPreds++Preds}, Succ -- OldSucc};
      not_found ->
	{HT,{NewBB, Succ, SelfPreds}, Succ}
    end,
  %% Add this block as predecessor to its successors
  NewHT = lists:foldl(fun (P,HTAcc) ->
			add_pred(HTAcc, P, Label)
		    end,
		    HT1, AddPreds),
    
  CFG#cfg{hashtable=hipe_hash:update(Label, CFGBlock, NewHT)}.

remove_pred(HT, FromL, PredL) ->
  case hipe_hash:lookup(FromL, HT) of
    {found, {Block, Succ, Preds}} ->
      hipe_hash:update(FromL,{Block,Succ,lists:delete(PredL,Preds)},HT);
    not_found ->
	HT
  end.

add_pred(HT, ToL, PredL) ->
  case hipe_hash:lookup(ToL, HT) of
    {found, {Block, Succ, Preds}} ->
      hipe_hash:update(ToL,
		       {Block,Succ,[PredL | 
				    lists:delete(PredL,Preds)]},
		       HT);
    not_found ->
      hipe_hash:update(ToL,{[],[],[PredL]},
		       HT)
  end.


bb_remove(CFG, Label) ->
  HT = CFG#cfg.hashtable,
  case hipe_hash:lookup(Label, HT) of
    {found, {_Block, Succ, _Preds}} ->
      %% Remove this block as a pred from all successors.
      HT1 = lists:foldl(fun (S,HTAcc) ->
			    remove_pred(HTAcc, S, Label)
			end,
			HT, Succ),
      CFG#cfg{hashtable=hipe_hash:delete(Label, HT1)};
    _ -> CFG
  end.


bb_update(CFG, Label, NewBB) ->
   bb_add(CFG, Label, NewBB).


bb(CFG, Label) ->
   HT = CFG#cfg.hashtable,
   case hipe_hash:lookup(Label, HT) of
      {found, {Block, _Succ, _Pred}} ->
	 Block;
      not_found ->
	 not_found
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Construct a cfg from a list of instructions
%%

take_bbs([], CFG) ->
    CFG;
take_bbs(Xs, CFG) ->
    Lbl = hd(Xs),
     case is_label(Lbl) of
	true ->
%	    CFG1 = case is_fail_entrypoint(Lbl) of
%		       true ->
%			   add_fail_entrypoint(CFG, label_name(Lbl));
%		       false ->
%			   CFG
%		   end,
	     CFG1 = CFG,
	    case take_bb(tl(Xs), []) of
		{Code, Rest} ->
		    NewCFG = bb_add(CFG1, label_name(Lbl),
				    hipe_bb:mk_bb(Code, label_annot(Lbl))),
		    take_bbs(Rest, NewCFG)
	    end;
	false ->
	    erlang:fault({cfg, "basic block doesn't start with a label", Xs})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Take_bb returns:
%%   - {Code, Rest}.
%%      * Code is a list of all the instructions.
%%      * Rest is the remainder of the instructions

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
%%
%% Functions for extracting the names of the basic blocks in various
%% orders.
%%

labels(CFG) ->
    HT = CFG#cfg.hashtable,
    List = hipe_hash:list(HT),
    lists:map(fun(T)->element(1, T) end, List).

depth_first_ordering(CFG) ->
    Start = start(CFG),
    Succ = succ_map(CFG),
    {Ordering, _Visited} =
	df_search([Start|other_entrypoints(CFG)]
		  ++fail_entrypoints(CFG),
		  none_visited(), Succ, []),
    Ordering.

df_search([N|Ns], Visited, Succ, Acc) ->
  case visited(N, Visited) of 
    true ->
      df_search(Ns, Visited, Succ, Acc);
    false ->
      {Order, Vis} = 
	df_search(succ(Succ, N), visit(N, Visited),
		     Succ, Acc),
      df_search(Ns, Vis, Succ, [N|Order])
  end;
df_search([], Visited, _, Ordering) -> {Ordering, Visited}.



depth_search([N|Ns], Visited, Succ, Acc) ->
    case visited(N, Visited) of 
	true ->
	    depth_search(Ns, Visited, Succ, Acc);
	false ->
	    {Order, Vis} = 
		depth_search(succ(Succ, N), visit(N, Visited),
			     Succ, Acc),
	    depth_search(Ns, Vis, Succ, [N|Order])
    end;
depth_search([], Visited, _, Ordering) -> {Ordering, Visited}.

reverse_postorder(CFG) ->
    Start = start(CFG),
    Succ = succ_map(CFG),
    {Ordering, _Visited} =
	depth_search([Start|other_entrypoints(CFG)]
		    ++fail_entrypoints(CFG),
		     none_visited(), Succ, []),
    Ordering.


postorder(CFG) ->
  lists:reverse(reverse_postorder(CFG)).

inorder(CFG) ->
  lists:reverse(reverse_inorder(CFG)).

reverse_inorder(CFG) ->
  Start = start(CFG),
  Succ = succ_map(CFG),
  {Ordering, _Visited} =
    inorder_search([Start|other_entrypoints(CFG)]
		   ++fail_entrypoints(CFG), 
		   none_visited(), Succ, []),
  Ordering. 

inorder_search([N|Ns], Visited, Succ, Acc) ->
  case visited(N, Visited) of 
    true ->
      inorder_search(Ns, Visited, Succ, Acc);
    false ->
	case succ(Succ, N) of
	  [First|Rest] ->
	    {Order, Vis} = 
		inorder_search([First], visit(N, Visited),
			     Succ, Acc),
	    inorder_search(Rest++Ns, Vis, Succ, [N|Order]);
	  [] ->
	    inorder_search(Ns, visit(N, Visited), Succ, [N|Acc])
	end
    end;
inorder_search([], Visited, _, Ordering) -> {Ordering, Visited}.


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
    Start = start(CFG),
    Succ = succ_map(CFG),
    {Ordering, _Visited} =
	preorder_search([Start|other_entrypoints(CFG)]
			++fail_entrypoints(CFG),
			none_visited(), Succ, []),
    lists:reverse(Ordering).

preorder_search([N|Ns], Visited, Succ, Acc) ->
    case visited(N, Visited) of 
	true ->
	    preorder_search(Ns, Visited, Succ, Acc);
	false ->
	    {Order, Vis} = 
		preorder_search(succ(Succ, N), visit(N, Visited),
				Succ, [N|Acc]),
	    preorder_search(Ns, Vis, Succ, Order)
    end;
preorder_search([], Visited, _, Ordering) -> {Ordering, Visited}.


breadthorder(CFG) ->
    lists:reverse(reverse_breadthorder(CFG)).

reverse_breadthorder(CFG) ->
    Start = start(CFG),
    Succ = succ_map(CFG),
    {Vis, RBO1} = breadth_list([Start], none_visited(), Succ, []),
    {_Vis1, RBO2} = breadth_list(other_entrypoints(CFG)++fail_entrypoints(CFG), Vis, Succ, RBO1),
    RBO2.

breadth_list([X|Xs], Vis, Succ, BO) ->
    case visited(X,Vis) of
	true ->
	    breadth_list(Xs, Vis, Succ, BO);
	false ->
	    breadth_list(Xs ++ succ(Succ,X), visit(X,Vis), Succ, [X|BO])
    end;  
breadth_list([], Vis, _Succ, BO) -> 
    {Vis, BO}.


none_visited(_CFG) -> 
    %% Preparation for implementation with updatable struct
    none_visited().

none_visited() -> 
    gb_sets:empty().

visit(X, Vis) -> 
    gb_sets:add(X, Vis).

visited(X, Vis) ->
    gb_sets:is_member(X, Vis).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% 
%%

succ_map(CFG) ->
   CFG.

succ(SuccMap, Label) ->
   HT = SuccMap#cfg.hashtable,
   case hipe_hash:lookup(Label, HT) of
     {found, {_Block, Succ,_Pred}} ->
       Succ;
     _ -> erlang:fault({successor_not_found,Label,SuccMap})
   end.


pred_map(CFG) ->
  CFG.
%  Labels = labels(CFG),
%  %% {_,Max} = label_range(CFG),
%  HT = hipe_hash:init(map_to(Labels, [])),
%  add_pred(Labels, succ_map(CFG), HT).

%map_to([],_) -> [];
%map_to([X|Xs],Y) -> [{X,Y}|map_to(Xs,Y)].


%add_pred([], _SuccMap, HT) ->
%   HT;
%add_pred([L|Ls], SuccMap, HT) ->
%   SuccList = succ(SuccMap, L),
%   NewHT = add_succ_pred(L, SuccList, HT),
%   add_pred(Ls, SuccMap, NewHT).


%add_succ_pred(_Lbl, [], HT) ->
%   HT;
%add_succ_pred(Lbl, [S|Ss], HT) ->
%   {found, SPred} = hipe_hash:lookup(S, HT),
%   add_succ_pred(Lbl, Ss, hipe_hash:update(S, [Lbl|SPred], HT)).


pred(Map, Label) ->
   HT = Map#cfg.hashtable,
   case hipe_hash:lookup(Label, HT) of
     {found, {_Block, _Succ,Pred}} ->
       Pred;
     _ -> erlang:fault({predecessor_not_found,Label,Map})
   end.
%   {found, Pred} = hipe_hash:lookup(Label, Map),
%   Pred.

%pred_update(Map, Label, NewPred) ->
%  hipe_hash:update(Label, NewPred, Map).

pred_remove(Map, Label, Pred)->
  NewHT = remove_pred(Map#cfg.hashtable, Label, Pred),
  Map#cfg{hashtable=NewHT}.

fallthrough(CFG, Label) ->
    HT = CFG#cfg.hashtable,
    case hipe_hash:lookup(Label, HT) of
	{found, {_Block, Succ,_}} ->
	    case Succ of
		[X|_] -> X;
		_ -> none
	    end;
	Res ->
	    erlang:fault({Res, {"fallthrough label not found",Label}})
    end.

conditional(CFG, Label) ->
    HT = CFG#cfg.hashtable,
    {found, {_Block, Succ,_}} = hipe_hash:lookup(Label, HT),
    case Succ of
	[] -> none;
	[_] -> none;
	[_|Labels] -> Labels
    end.

redirect(CFG, From, ToOld, ToNew) ->
   Code = hipe_bb:code(bb(CFG, From)),
   Last = lists:last(Code),
   NewLast = redirect_jmp(Last, ToOld, ToNew),
   bb_update(CFG, From, hipe_bb:mk_bb(butlast(Code)++[NewLast])).

butlast([X|Xs]) -> butlast(Xs,X).

butlast([],_) -> [];
butlast([X|Xs],Y) -> [Y|butlast(Xs,X)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Linearize the code in a cfg. Returns a list of instructions
%%

linearize_cfg(CFG) ->
   Start = start(CFG),
   Vis = none_visited(),
   {Vis0, NestedCode} = lin_succ(Start, CFG, Vis),
   BlocksInData = hipe_consttab:referred_labels(data(CFG)),
   CatchBlocks = fail_entrypoints(CFG),
   AllCode = lin_other_entries(NestedCode, CFG, BlocksInData 
			       ++ CatchBlocks
			       , Vis0),
   lists:flatten(AllCode).


lin_succ(none, _CFG, Vis) ->
   {Vis, []};
lin_succ([Label|Labels], CFG, Vis) ->
    {Vis1, Code1} = lin_succ(Label, CFG, Vis),
    {Vis2, Code2} = lin_succ(Labels, CFG, Vis1),
    {Vis2, [Code1,Code2]};
lin_succ([], _CFG, Vis) ->
    {Vis, []};
lin_succ(Label, CFG, Vis) ->
   case visited(Label, Vis) of
      true ->
	 {Vis, []};      % already visited
      false ->
	 Vis0 = visit(Label, Vis),
	 Block = bb(CFG, Label),
	 Fallthrough = fallthrough(CFG, Label),
	 Cond = conditional(CFG, Label),
	 LblInstr = mk_label(Label, hipe_bb:annot(Block)),
	 {Vis1, Code1} = lin_succ(Fallthrough, CFG, Vis0),
	 {Vis2, Code2} = lin_succ(Cond, CFG, Vis1),
	 {Vis2, [[LblInstr|hipe_bb:code(Block)], Code1, Code2]}
   end.


lin_other_entries(Code, _CFG, [], _Vis) ->
   Code;
lin_other_entries(Code, CFG, [E|Es], Vis) ->
   {Vis0, MoreCode} = lin_succ(E, CFG, Vis),
   lin_other_entries([Code, MoreCode], CFG, Es, Vis0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Remove empty BBs.
%% 
%% Removes basic blocks containing only a goto to another BB.
%% Branches to removed blocks are updated to the successor of the
%% removed block.
%% Loads (or other operations) on the label of the BB are also
%% updated. So are any references from the data section.
%%
%% XXX: This implementation does not handle empty infinite loops
%%      such as L1: goto L2
%%              L2: goto L1 
%%      The occurrence of such loops should be checked for and handled.
%%
remove_dead_code(CFG) ->
  %% pp(CFG),
  HT = CFG#cfg.hashtable,
  ?opt_start_timer("HT to list"),
  List = hipe_hash:list(HT),
  ?opt_stop_timer("HT to list"),
  ?opt_start_timer("Labels"),
  Labels = lists:map(fun(T)->element(1, T) end, List),
  ?opt_stop_timer("Labels"),
  ?opt_start_timer("Predmap"),
  SuccMap = succ_map(CFG),
  PredMap = pred_map(CFG),
  ?opt_stop_timer("Predmap"),
  ?opt_start_timer("Merge BBs"),
  {CFG0,SuccMap0,PredMap0,_} = merge_bbs(Labels,CFG,SuccMap,PredMap),
  ?opt_stop_timer("Merge BBs"),
  %% pp(CFG0),
  ?opt_start_timer("FindDead"),
  {NewMap,CFG1} = remap(labels(CFG0),SuccMap0,PredMap0,rd_map_new(),CFG0),
  ?opt_stop_timer("FindDead"),
  ?opt_start_timer("Labels"),
  Labels1 = labels(CFG1),
  ?opt_stop_timer("Labels1"),
  ?opt_start_timer("RedirectBranches"),
  CFG2 = redirect_branches(NewMap, CFG1),
  ?opt_stop_timer("RedirectBranches"),
  ?opt_start_timer("RedirectOps"),
  CFG3 = redirect_ops(Labels1, CFG2, NewMap),
  ?opt_stop_timer("RedirectOps"),
  ?opt_start_timer("RedirectData"),
  CFG4 = redirect_data(CFG3, NewMap),
  ?opt_stop_timer("RedirectData"),
  ?opt_start_timer("RedirectStart"),
  CFG5 = redirect_start(CFG4, NewMap),
  ?opt_stop_timer("RedirectStart"),
  %% pp(CFG5),
  CFG5.

redirect_start(CFG, Map) ->
  Start = start_label(CFG),
  case forward(Start,Map) of
    Start -> CFG;
    NewStart ->
      start_label_update(CFG, NewStart)

  end.


find_new_label(Old, Map) ->
  forward(Old, Map).


redirect_data(CFG, Map) ->
  Data = data(CFG),
  NewData = 
    hipe_consttab:update_referred_labels(
      Data, 
      rd_succs(Map)),
  update_data(CFG, NewData).

redirect_branches(Map,CFG) ->
  lists:foldl(fun ({From,{newsuccs,Redirects}},CFGAcc) ->
		  lists:foldl(
		    fun({ToOld,ToNew},CFG1) ->
			case bb(CFG1, From) of
			  not_found -> 
			    CFG1;
			  _ ->
			    To = forward(ToNew,Map),
			    redirect(CFG1, From, ToOld, To)
			end
		    end,
		    CFGAcc,
		    Redirects);
		  (_,CFGAcc) -> CFGAcc
	      end,
	      CFG,
	      gb_trees:to_list(Map)).



remap([L|Rest], SuccMap,PredMap, NewMap, CFG) ->
  case is_empty(bb(CFG, L)) of
    true ->
      case succ(SuccMap, L) of
	[SuccL] ->
	  CFG1 = bb_remove(CFG, L), 
	  NewMap1 = remap_to_succ(L, SuccL, NewMap, PredMap),
	  remap(Rest, SuccMap, PredMap, NewMap1,CFG1);
	_ -> erlang:fault({is_empty_true_for_nonemptyblock, L})
      end;
    false ->
	 remap(Rest, SuccMap, PredMap, NewMap, CFG)
  end;
remap([],_,_,NewMap,CFG) -> {NewMap, CFG}.


remap_to_succ(L, SuccL, NewMap, PredMap) ->
  insert_remap(L,forward(SuccL,NewMap),pred(PredMap,L), NewMap).



%% Find the proxy for a BB
forward(L, Map) ->
  case gb_trees:lookup(L,Map) of 
    {value, {dead,To}} -> 
      forward(To,Map); %% Hope this terminates.
    _ -> L 
  end.


%% A redirection map contains mappings from labels to
%%  none -> this BB is not affected by the remapping.
%%  {dead,To} -> this BB is dead, To is the new proxy.
%%  {newsuccs,[{X,Y}|...]} -> The successor X is redirected to Y. 

rd_map_new() -> gb_trees:empty().

rd_succs(M) ->
  R = 
    lists:foldl(fun ({From,{dead,To}},Acc) -> 
		  [{From,forward(To,M)}|Acc];
		  (_,Acc) -> Acc
	      end,
		[],
		gb_trees:to_list(M)),
  R.

add_redirecedto(L,From,To,Map) ->
  case gb_trees:lookup(L,Map) of 
    {value, {newsuccs,NS}} ->
      gb_trees:update(L,{newsuccs,[{From,To}|lists:keydelete(From,1,NS)]},Map);
    {value, {dead,_}} -> Map;
    none ->
      gb_trees:insert(L,{newsuccs,[{From,To}]},Map)
  end.
insert_remap(L,ToL,Preds,Map) ->
  Map2 = gb_trees:enter(L,{dead,ToL},Map),
  lists:foldl(fun (Pred,AccMap) ->
		   add_redirecedto(Pred,L,ToL,AccMap)
		end,
		Map2,
		Preds).


is_empty(BB) ->
  is_empty_bb(hipe_bb:code(BB)).

is_empty_bb([I]) ->
  is_goto(I); %% A BB with just a 'goto' is empty.
is_empty_bb([I|Is]) ->
  case is_comment(I) of 
    true ->
      is_empty_bb(Is);
    false ->
      false
  end;
is_empty_bb([]) ->
  true.


%% Go through the CFG and find pairs of BBs that can be merged to one BB.
%% They are of the form:
%%
%%           L
%% 
%%          |
%%         v
%%  
%%   Successor
%%
%% That is, the block L has only one succesor (Successor) and that
%% successor has no other predecessors than L.
%%
%% Note: calls might end a basic block 

merge_bbs(Labels,CFG,SuccMap,PredMap) ->
  lists:foldl(fun merge_successor/2,
	      {CFG,SuccMap,PredMap,start_label(CFG)},
	      Labels).

%% If L fulfills the requirements, merge it with its successor.
merge_successor(L,Acc={NewCFG,NewSuccMap,NewPredMap,StartLabel}) ->
  case catch 
    begin
      %% Get the BB L (If it still exists).
      BB = bb(NewCFG, L),
      
      %% Make sure we only have one successor
      [Successor]= succ(NewSuccMap,L),
      %% We do not want to merge with the entry point...
      true = StartLabel /= Successor,
      %% Make sure we only have one predecessor to this successor
      [L] = pred(NewPredMap,Successor),
      
      %% Remove the goto or remap fall-through in BB and merge the BBs
      NewCode = merge(BB,bb(NewCFG,Successor),Successor),
      NewBB = hipe_bb:mk_bb(NewCode),
%%      NewCFG2 = bb_update(NewCFG, L, NewBB),
      NewCFG2 = bb_update(bb_remove(NewCFG, Successor), L, NewBB),
		    
      %% Remap the predecessor of all successors to the successor of block L to be L
      UpdatedPredMap = 
	pred_map(NewCFG2),
      %%remap_predecessors(L, NewPredMap, Successor, succ(NewSuccMap,Successor)),
      {NewCFG2, succ_map(NewCFG2),UpdatedPredMap,StartLabel}
    
    end of
    %% Some precondition was not met.
    {'EXIT',_} ->
      Acc;
    Val -> Val
  end.
    


%% Merge BB and BB2 
merge(BB, BB2, BB2_Label) ->
  Head = hipe_bb:butlast(BB),
  Last = hipe_bb:last(BB),
  Tail = hipe_bb:code(BB2),
  case is_goto(Last) of
    true ->
      %% Just ignore the goto.
      Head ++ Tail;
    _ -> 
      %% The last instr is not a goto,
      %%  e.g. a call with only fall-through
      %% Remove the fall-through with the []-label. 
      Head ++ [redirect_jmp(Last,BB2_Label,[])| Tail]
  end.

%remap_predecessors(L, NewPredMap, Successor, SuccSuccessors)->
%  lists:foldl(fun (SSL,PredMapAcc) ->
%		  pred_update(PredMapAcc, SSL,
%			      [case E of
%				 Successor -> L;
%				 _ -> E
%			       end || E <- pred(PredMapAcc, SSL)])
%	      end, NewPredMap, SuccSuccessors).

% OLD VERSION
%merge_bbs(Labels,CFG,SuccMap,PredMap) ->
%  StartLabel = start_label(CFG),
%  lists:foldl(
%    fun (L,{NewCFG,NewSuccMap,NewPredMap}) ->
%	BB = bb(NewCFG, L),
%	case succ(NewSuccMap,L) of
%	  [Successor] ->
%	    case StartLabel of
%	      %% We do not want to merge with the entry point...
%	      Successor -> {NewCFG,NewSuccMap,NewPredMap};
%	      _ ->
%		case pred(NewPredMap,Successor) of
%		  [L] -> 
%		    %% The current block has a succesor
%		    %% with just this block as predecessor.
%		    BB2 = bb(NewCFG, Successor),
%		    Head = hipe_bb:butlast(BB),
%		    Last = hipe_bb:last(BB),
%		    NewCode = 
%		      case is_goto(Last) of
%			true ->
%			  Head ++ hipe_bb:code(BB2);
%			false -> %% The last instr is a call with only fallthrough
%			  %% Remove the fallthroug with the []-label. 
%			  Head ++ [redirect_jmp(Last,Successor,[])| hipe_bb:code(BB2)]
%		      end,
		    
%		    %% Get the successors to the sucessor
%		    SuccSuccessors = succ(NewSuccMap,Successor),
%		    %% Remap the predecessor of all successor to the block L
%		    UpdatedPredMap = 
%		      lists:foldl(fun (SSL,PredMapAcc) ->
%				      pred_update(PredMapAcc, SSL,
%						  [case E of
%						     Successor -> L;
%						     _ -> E
%						   end || E <- pred(PredMapAcc, SSL)])
%				      end, NewPredMap, SuccSuccessors),
%		    NewBB = hipe_bb:mk_bb(NewCode),
%		    NewCFG2 = bb_update(NewCFG, L, NewBB),
%		    {NewCFG2, succ_map(NewCFG2),
%		     %% The BB Successor does not exist -- just in case - delete its pred.
%		     pred_update(UpdatedPredMap,Successor, [])};
%		  _ -> %% To many predecessors
%		    {NewCFG,NewSuccMap,NewPredMap}	
%		end
%	    end;
%	  _ -> %% Not just one successor
%	    {NewCFG,NewSuccMap,NewPredMap}
%	end
%    end,
%    {CFG,SuccMap,PredMap},
%    Labels).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Remove unreachable BBs.
%% 
%% A BB is unreachable if it can't be reached by any path from the
%% start label of the function.
%%

remove_unreachable_code(Cfg)->
  Start = start_label(Cfg),
  SuccMap = succ_map(Cfg),
  Reachable = find_reachable([Start], SuccMap, gb_sets:from_list([Start])),
  Labels = labels(Cfg),
  case Labels--Reachable of
    [] ->
      Cfg;
    Remove ->      
      NewCfg = lists:foldl(fun(X, Acc)->bb_remove(Acc, X)end, Cfg, Remove),
      remove_unreachable_code(NewCfg)
  end.

find_reachable([Label|Left], SuccMap, Acc)->
  NewAcc = gb_sets:add(Label, Acc),
  Succ = succ(SuccMap, Label),
  find_reachable([X||X<-Succ, not gb_sets:is_member(X, Acc)]++Left,
		 SuccMap, NewAcc);
find_reachable([], _SuccMap, Acc) ->
  gb_sets:to_list(Acc).

