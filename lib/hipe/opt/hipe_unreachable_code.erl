%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		 REMOVAL OF UNREACHABLE BASIC BLOCKS
%
% Starting at the entry points of the CFG, mark all reachable blocks.
% Unreachable blocks are then deleted from the CFG.
%
% Returns {changed,CFG} if some blocks could be deleted
% or unchanged if all blocks were reachable.
%
% This pass is intended to run after optimizations that may cause
% unreachable code.
%
% *** UNFINISHED ***
% at present, delete_unmarked/2 and thus remove/1 returns a list of
% the delible blocks, rather than deleting them.

-module(hipe_unreachable_code).
-export([remove/1]).

-define(report(MFA,Msg,Args),report(MFA,Msg,Args)).

remove(CFG) ->
    EntryPoints = [cfg:start(CFG)|cfg:fail_entrypoints(CFG)],
    Marked = mark_list(EntryPoints,cfg:succ_map(CFG),none_visited()),
    delete_unmarked(CFG,Marked).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mark_list([],Succ,Vis) -> Vis;
mark_list([L|Ls],Succ,Vis) ->
    mark_list(Ls,Succ,mark(L,Succ,Vis)).

mark(L,Succ,Vis) ->
   case visited(L,Vis) of
      true ->
	 Vis;
      false ->
	 mark_list(cfg:succ(Succ,L),Succ,visit(L,Vis))
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This function deletes unreachable blocks from the CFG
% and returns {changed,NewCFG} or unchanged.

delete_unmarked(CFG,Marked) ->
    Ls = check_each(cfg:labels(CFG),Marked),
    ?report(delete_unmarked,'unreachable blocks ~p~n',[Ls]),
    case Ls of
	[] -> unchanged;
	_ ->
	    {changed,del_blocks(Ls,CFG)}
    end.

check_each([],Mark) -> [];
check_each([L|Ls],Mark) ->
   case visited(L,Mark) of
      true ->
	 check_each(Ls,Mark);
      false ->
	 [L|check_each(Ls,Mark)]
   end.

del_blocks([],CFG) -> CFG;
del_blocks([L|Ls],CFG) ->
   del_blocks(Ls,cfg:bb_remove(L,CFG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

none_visited() -> hash:empty().

visit(L,Vis) ->
    hash:update(L,visited,Vis).

visited(L,Vis) ->
    case hash:lookup(L,Vis) of
	{found,visited} -> true;
	_ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report(MFA,Msg,Args) ->
    io:format('~p: ',[MFA]),
    io:format(Msg,Args).
