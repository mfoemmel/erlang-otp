%%%----------------------------------------------------------------------
%%% File    : hipe_icode_mulret.erl
%%% Author  : Christoffer Vikström <chvi3471@it.uu.se>
%%% Purpose : 
%%% Created : 23 Jun 2004 by Christoffer Vikström <chvi3471@it.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_icode_mulret).
-author('chvi3471@it.uu.se').

-include("hipe_icode.hrl").
-export([mult_ret/4]).

%%>----------------------------------------------------------------------<
%% Procedure : mult_ret/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
mult_ret(List, Mod, Opts, Exports) ->
    case length(List) > 1 of
	true ->
	    Table = analyse(List, Mod, Exports),
	    %% printTable(Mod, Exports, Table),
	    NewList = optimize(List, Mod, Opts, Table),
	    NewList;
	false ->
	    List
    end.


%%>-----------------------< Analysis Steps >-----------------------------<

%%>----------------------------------------------------------------------<
%% Procedure : analyse/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
analyse(List, _Mod, Exports) ->
    MaxRets = hipe_rtl_arch:nr_of_return_regs(),
    Table = mkTable(List),
    %% printTable(Mod, Exports, Table),
    Table2 = filterTable(Table, MaxRets, Exports),
    %% printTable(Mod, Exports, Table2),
    Table2.

%%>----------------------------------------------------------------------<
%% Procedure : mkTable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
mkTable(List) -> mkTable(List, {[], []}).

mkTable([{Mfa, Icode} | List], Table) ->
    %% New Icode
    {_LMin,LMax} = hipe_icode:icode_label_range(Icode),
    hipe_gensym:set_label(icode,LMax+1),
    {_VMin,VMax} = hipe_icode:icode_var_range(Icode),
    hipe_gensym:set_var(icode,VMax+1),
    %% 
    case isFunDef(Mfa) of
	true ->
	    mkTable(List, Table);
	false ->
	    CallList = mkCallList(Mfa, Icode),
	    Optimizable = isOptimizable(Icode),
	    NewTable = addToTable(Mfa, Optimizable, CallList, Table),
	    mkTable(List, NewTable)
    end;
mkTable([_|List], Table) -> mkTable(List, Table);
mkTable([], Table) -> Table.


%%>----------------------------------------------------------------------<
%% Procedure : isFunDef/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
isFunDef({_,F,_}) ->
    case hd(atom_to_list(F)) of
	45 ->   %% 45 is the character '-'
	    true;
	_ ->
	    false
    end.


%%>----------------------------------------------------------------------<
%% Procedure : mkCallList/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
mkCallList(Mfa, Icode) ->
    Code = hipe_icode:icode_code(Icode),
    mkCallList(Code, Mfa, []).
mkCallList([#call{'fun'=F, dstlist=Var, type=local}|Code], Mfa, Res) ->
    {Size, DstList} = lookForDef(Code, Var),
    mkCallList(Code, Mfa, [{callPair,Mfa,{F,{matchSize,Size,DstList}}}|Res]);
mkCallList([_|Code], Mfa, Res) -> mkCallList(Code, Mfa, Res);
mkCallList([], _, Res) -> Res.


%%>----------------------------------------------------------------------<
%% Procedure : lookForDef/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
lookForDef([#type{type={tuple,Size}, args=Var, true_label=L}|Code], Var) ->
    Code2 = skipToLabel(Code, L),
    DstLst = lookForUnElems(Code2, Var),
    case DstLst of
	[] -> {1, [Var]};
	_  ->
	    DstLst2 = fixDstLst(DstLst, Size),
	    {Size, DstLst2}
    end;
lookForDef([#move{src=Var, dst=NewVar}|Code], [Var]) ->
    lookForDef(Code, [NewVar]);
lookForDef([#label{}|_], Var) -> 
    {1, [Var]};
lookForDef([I|Code], Var) ->
    Defs = hipe_icode:defines(I),
    case lists:member(Var, Defs) of
	true ->
	    {1, [Var]};
	false ->
	    lookForDef(Code, Var)
    end;
lookForDef([], Var) -> {1, [Var]}.

%%>----------------------------------------------------------------------<
%% Procedure : skipToLabel/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
skipToLabel(Code, L) ->
    case skipToLabel2(Code,L) of
	noLabel ->
	    Code;
	NewCode ->
	    NewCode
    end.
skipToLabel2([#label{name=L}|Code],L) -> Code;
skipToLabel2([_|Code], L) -> skipToLabel2(Code, L);
skipToLabel2([], _) -> noLabel.


%%>----------------------------------------------------------------------<
%% Procedure : lookForUnElems/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
lookForUnElems(Code, Var) -> lookForUnElems(Code, Var, []).
lookForUnElems([#call{'fun'={unsafe_element,Nr}, args=Var, 
		      dstlist=[Ret]}|Code], Var, Res) ->
    lookForUnElems(Code, Var, [{Nr, Ret}|Res]);
lookForUnElems([#move{dst=Var}|_], [Var], Res) -> 
    lists:flatten(Res);
lookForUnElems([#fmove{dst=Var}|_], [Var], Res) -> 
    lists:flatten(Res);
lookForUnElems([#call{dstlist=Var}|_], Var, Res) -> 
    lists:flatten(Res);
lookForUnElems([_|Code], Var, Res) -> 
    lookForUnElems(Code, Var, Res);
lookForUnElems([], _, Res) -> lists:flatten(Res).


%%>----------------------------------------------------------------------<
%% Procedure : fixDstLst/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
fixDstLst(DstLst, Size) -> fixDstLst(DstLst, Size, 1, []).
fixDstLst(DstLst, Size, Cnt, Res) when Cnt =< Size ->
    case isInLst(Cnt, DstLst) of
	{true, Var} ->
	    fixDstLst(DstLst, Size, Cnt+1, [Var|Res]);
	false  ->
	    Var = {var, hipe_gensym:new_var(icode)},
	    fixDstLst(DstLst, Size, Cnt+1, [Var|Res])
    end;
fixDstLst(_,Size,Cnt,Res) when Cnt > Size -> lists:reverse(Res).
    

%%>----------------------------------------------------------------------<
%% Procedure : isInLst/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
isInLst(Nr, [{Nr,Var}|_]) -> {true, Var};
isInLst(Cnt, [_|DstLst]) -> isInLst(Cnt, DstLst);
isInLst(_, []) -> false.


%%>----------------------------------------------------------------------<
%% Procedure : isOptimizable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
isOptimizable(Icode) ->    
    %% Icode2 = hipe_icode:fixup_fallthroughs(Icode),
    Icode2 = hipe_icode:strip_comments(Icode),
    Cfg = hipe_icode_cfg:linear_to_cfg(Icode2),
    %% hipe_icode_cfg:pp(Cfg),
    PredMap = hipe_icode_cfg:pred_map(Cfg),
    case findReturnBlocks(Cfg) of
     	noReturn ->
	    {false, -1};
	BlockList ->
	    processReturnBlocks(BlockList, PredMap, Cfg)
    end.

%%>----------------------------------------------------------------------<
%% Procedure : findReturnBlocks/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
findReturnBlocks(IcodeCfg) ->
    Labels = hipe_icode_cfg:labels(IcodeCfg),
    case searchBlocks(Labels, IcodeCfg) of 
	[] ->
	    noReturn;
	BlockList->
	    BlockList
    end.

%%>----------------------------------------------------------------------<
%% Procedure : searchBlocks/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
searchBlocks(Labels, IcodeCfg) -> searchBlocks(Labels, IcodeCfg, []).
searchBlocks([Label|Labels], IcodeCfg, Res) ->
    Block = hipe_icode_cfg:bb(IcodeCfg, Label),
    Code = hipe_bb:code(Block),
    case searchBlockCode(Code) of
	{hasReturn, RetVar} ->
	    searchBlocks(Labels, IcodeCfg, [{Label, RetVar}|Res]);
	noReturn ->
	    searchBlocks(Labels, IcodeCfg, Res)
    end;
searchBlocks([], _, Res) -> Res.

%%>----------------------------------------------------------------------<
%% Procedure : searchBlockCode/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
searchBlockCode([#return{vars=Vars}|_]) -> {hasReturn, Vars};
searchBlockCode([_|Icode]) ->
    searchBlockCode(Icode);
searchBlockCode([]) -> noReturn.


%%>----------------------------------------------------------------------<
%% Procedure : processReturnBlock/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
processReturnBlocks(Blocks, PredMap, Cfg) -> 
    processReturnBlocks(Blocks, PredMap, Cfg, {true, -1}, []).
processReturnBlocks([{Label, Var}|BlockList], PredMap, 
		    IcodeCfg,{Opts,Size},TypeLst) ->
    {Opt, Type, Size2} = traverseCode(Label, Var, PredMap, IcodeCfg),
    case (Size == -1) orelse (Size == Size2) of
	true ->
	    processReturnBlocks(BlockList, PredMap, IcodeCfg, 
				{Opt and Opts, Size2}, [Type|TypeLst]);
	false ->
	    {false, -1}
    end;
processReturnBlocks([], _, _, Res, TypeLst) -> 
    case lists:member(var, TypeLst) of
	true ->
	    {_, Size} = Res,
	    case Size > 1 of
		true ->
		    Res;
		false ->
		    {false, -1}
	    end;
	false ->
	    {false, -1}
    end.


%%>----------------------------------------------------------------------<
%% Procedure : traverseCode/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
traverseCode(Label, Var, PredMap, IcodeCfg) -> 
    traverseCode(Label, Var, PredMap, IcodeCfg, []).
traverseCode(Label, Var, PredMap, IcodeCfg, LabLst) ->
    Preds = hipe_icode_cfg:pred(PredMap, Label),
    Block = hipe_icode_cfg:bb(IcodeCfg, Label),
    Code = hipe_bb:code(Block),
    case findDefine(lists:reverse(Code), Var) of
	{found, Type, NumRets} ->
	    {true, Type, NumRets};
	{notFound, SrcVar} ->
	    case length(Preds) of
		0 ->
		    {false, none, -1};
		1 ->
		    case lists:member(Label, LabLst) of
			false ->
			    [Pred] = Preds,
			    traverseCode(Pred, SrcVar, PredMap, 
					 IcodeCfg, [Label|LabLst]);
			true ->
			    {false, none, -1}
			end;
		_ ->
		    {false, none, -1}
	    end
    end.

%%>----------------------------------------------------------------------<
%% Procedure : findDefine/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
findDefine([#call{dstlist=Var,'fun'=mktuple,args=Vs}|_], Var) ->
    case length(Vs) of
	1 ->
	    [{Type, _}] = Vs,
	    {found, Type, 1};
    	_ ->
	    case lists:keymember(var, 1, Vs) of
		true ->
		    {found, var, length(Vs)};
		false  ->
		    {found, const, length(Vs)}
	    end
    end;
findDefine([#move{dst=Var, src=Src}|Code], [Var]) ->
    case Src of
	{var, _} ->
	    findDefine(Code, [Src]);
	{const,{flat, Tuple}} when is_tuple(Tuple) ->
	    {found, const, size(Tuple)};
	{const, {flat, _}} ->
	    {found, const, 1};
	_ ->
	    findDefine(Code, [Var])
    end;
findDefine([_|Code], Var) ->
    findDefine(Code, Var);
findDefine([], Var) -> {notFound, Var}.


%%>----------------------------------------------------------------------<
%% Procedure : addToTable/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
addToTable(Mfa, Optimizable, CallList, {FunLst, CallLst}) ->
    NewFunLst = [{Mfa, Optimizable}|FunLst],
    {NewFunLst, CallList ++ CallLst}.


%%>----------------------------------------------------------------------<
%% Procedure : filterTable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
filterTable({FunLst, CallLst}, MaxRets, Exports) -> 
    filterTable(FunLst, CallLst, MaxRets, Exports, {[],[]}).
filterTable([Fun|FunLst], CallLst, MaxRets, Exports, {Funs,Calls}) ->
    {Mfa, {ReturnOpt, Rets}} = Fun,
    {CallOpt, CallsToKeep} = checkCalls(CallLst, Mfa, Rets),
    CallsToKeep2 = removeDuplicateCalls(CallsToKeep),
    NotExported = checkExported(Mfa, Exports),
    case (CallOpt andalso ReturnOpt) andalso 
	(Rets =< MaxRets) andalso NotExported
	andalso (not containRecursiveCalls(CallsToKeep2, Mfa)) of
	true ->
	    filterTable(FunLst, CallLst, MaxRets, Exports, 
			{[Fun|Funs], CallsToKeep2 ++ Calls});
	false ->
	    filterTable(FunLst, CallLst, MaxRets, Exports, 
			{Funs, Calls})
    end;
filterTable([],_,_,_,Res) -> Res.


removeDuplicateCalls(Calls) -> removeDuplicateCalls(Calls, []).
removeDuplicateCalls([Call|CallsToKeep], Res) -> 
    case lists:member(Call, CallsToKeep) of
	true ->
	    removeDuplicateCalls(CallsToKeep, Res);
	false ->
	    removeDuplicateCalls(CallsToKeep, [Call|Res])
    end;
removeDuplicateCalls([], Res) -> lists:reverse(Res).

containRecursiveCalls([Call|Calls], Fun) ->
    {callPair, Caller, {Callee, _}} = Call,
    case (Callee == Fun) andalso (Caller == Fun) of
	true ->
	    true;
	 false->
	    containRecursiveCalls(Calls, Fun)
    end;
containRecursiveCalls([], _) -> false.

%%>----------------------------------------------------------------------<
%% Procedure : checkCalls/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
checkCalls(CallLst, Mfa, Rets) -> checkCalls(CallLst, Mfa, Rets, [], []).
checkCalls([C = {callPair, _, {Mfa, {matchSize,Rets,_}}}|CallLst], 
	   Mfa, Rets, Res, Opt) ->
    checkCalls(CallLst, Mfa, Rets, [C|Res], [true|Opt]);
checkCalls([{callPair, _, {Mfa, {matchSize,_,_}}}|CallLst], 
	   Mfa, Rets, Res, Opt) ->
    checkCalls(CallLst, Mfa, Rets, Res, [false|Opt]);
checkCalls([_|CallLst], Mfa, Rets, Res, Opt) ->
    checkCalls(CallLst, Mfa, Rets, Res, Opt);
checkCalls([], _, _, Res, Opt) -> {combineOpts(Opt), Res}.


%%>----------------------------------------------------------------------<
%% Procedure : combineOpts/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
combineOpts([]) -> false;
combineOpts([Opt]) -> Opt;
combineOpts([Opt|Opts]) -> Opt and combineOpts(Opts).


%%>----------------------------------------------------------------------<
%% Procedure : checkCalls/2
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
checkExported({_,F,A}, [{F,A}|_]) -> false;
checkExported(Mfa, [_|Exports]) -> checkExported(Mfa, Exports);
checkExported(_, []) -> true.



%%>----------------------< Optimization Steps >--------------------------<

%%>----------------------------------------------------------------------<
%% Procedure : optimize/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimize(List, _Mod, Opts, Table) -> 
    {FunLst, CallLst} = Table,
    List2 = optimizeFuns(FunLst, Opts, List),
    List3 = optimizeCalls(CallLst, Opts, List2),
    List3.


%%>----------------------------------------------------------------------<
%% Procedure : optimizeFuns/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimizeFuns([{Fun, _}|FunList], Opts, List) ->
    NewList = findFun(List, Fun),
    optimizeFuns(FunList, Opts, NewList);
optimizeFuns([],_,List) -> List.


findFun(List, Fun) -> findFun(List, Fun, []).
findFun([{Fun, Icode}|List], Fun, Res) ->
    NewIcode = optimizeFun(Icode),
    findFun(List, Fun, [{Fun, NewIcode}|Res]);
findFun([I|List], Fun, Res) -> findFun(List, Fun, [I|Res]);
findFun([], _, Res) -> lists:reverse(Res).



optimizeFun(Icode) ->
    {_LMin,LMax} = hipe_icode:icode_label_range(Icode),
    hipe_gensym:set_label(icode,LMax+1),
    {_VMin,VMax} = hipe_icode:icode_var_range(Icode),
    hipe_gensym:set_var(icode,VMax+1),
    %% Icode2 = hipe_icode:fixup_fallthroughs(Icode),
    Icode2 = hipe_icode:strip_comments(Icode),
    Cfg = hipe_icode_cfg:linear_to_cfg(Icode2),
    PredMap = hipe_icode_cfg:pred_map(Cfg),
    case findReturnBlocks(Cfg) of
     	noReturn ->
	    false;
	BlockList ->
	    NewCfg = optimizeReturnBlocks(BlockList, PredMap, Cfg),
	    hipe_icode_cfg:cfg_to_linear(NewCfg)
    end.

optimizeReturnBlocks([Block|BlockList], PredMap, Cfg) -> 
    {NewCfg, Vars} = optimizeReturnBlock(Block, PredMap, Cfg),
    NewCfg2 = case length(Vars) of 
		  1 -> 
		      Cfg;
		  _ ->
		      {Label,_} = Block,
		      updateReturnBlock(Label, Vars, NewCfg)
    end,
    optimizeReturnBlocks(BlockList, PredMap, NewCfg2);
optimizeReturnBlocks([], _,Cfg) -> Cfg. 

optimizeReturnBlock(Block, PredMap, Cfg) -> 
    optimizeReturnBlock(Block, PredMap, Cfg, []).
optimizeReturnBlock({Label,Var}, PredMap, Cfg, UpdateMap) ->
    Preds = hipe_icode_cfg:pred(PredMap, Label),
    Block = hipe_icode_cfg:bb(Cfg, Label),
    Code = hipe_bb:code(Block),
    case optimizeDefine(Code, Var) of
	{found, NewBlockCode, Vars} ->
	    NewBlock = hipe_bb:code_update(Block, NewBlockCode),
	    NewCfg = resolveUpdateMap(UpdateMap, Cfg),
	    {hipe_icode_cfg:bb_add(NewCfg, Label, NewBlock), Vars};
	{none, NewBlockCode, NewVar} ->
	    case Preds of
		[] ->
		    erroneous_code;
		[Pred] ->
		    NewBlock = hipe_bb:code_update(Block, NewBlockCode),
		    optimizeReturnBlock({Pred,NewVar}, PredMap, Cfg, 
					[{Label, NewBlock}|UpdateMap]);
		_ ->
		    {Cfg, Var}
	    end;
	{none, noOpt} ->
	    {Cfg, Var}
    end.


optimizeDefine(Code, Dst) -> optimizeDefine(lists:reverse(Code), Dst, [], []).
optimizeDefine([I|Code], Dst, DstLst, Res) -> 
    [Ds] = Dst,
    case isCallPrimop(I, mktuple) and (length(DstLst) == 0) of
	true ->
	    case (hipe_icode:call_dstlist(I) == Dst) of
		true ->
		    case (hipe_icode:call_args(I) > 1) of 
			true ->
			    optimizeDefine(Code, Dst, 
					    hipe_icode:call_args(I), Res);
			false ->
			    {none, noOpt}
		    end;
		false ->
		    optimizeDefine(Code, Dst, DstLst, [I|Res])
	    end;
	false ->
	    case hipe_icode:is_move(I) and (length(DstLst) == 0) of
		true ->
		    case hipe_icode:move_dst(I) == Ds of
			true ->
			    case hipe_icode:move_src(I) of
				{var, _} ->
				    NewDst = hipe_icode:move_src(I),
				    optimizeDefine(Code, [NewDst], 
						   DstLst, Res);
				{const, {flat, T}} when is_tuple(T) ->
				    NewLst = tuple_to_list(T),
				    optimizeDefine(Code, Dst, NewLst, Res);
				_ ->
				    {none, noOpt}
			    end;
			false ->
			    optimizeDefine(Code, Dst, DstLst, [I|Res])
		    end;
		false ->
		    case lists:member(Ds, hipe_icode:defines(I)) and 
			(length(DstLst) == 0) of
			true ->
			    {none, noOpt};
			false ->
			    optimizeDefine(Code, Dst, DstLst, [I|Res])
		    end
	    end
    end;
optimizeDefine([], Dst, DstLst, Res) -> 
    case DstLst of
	[] -> 
	    {none, Res, Dst};
	_  ->
	    {found, Res, DstLst}
    end.

resolveUpdateMap([{Label, Block}|UpdateMap], Cfg) ->
    resolveUpdateMap(UpdateMap, hipe_icode_cfg:bb_add(Cfg, Label, Block));
resolveUpdateMap([], Cfg) -> Cfg.


%%>----------------------------------------------------------------------<
%% Procedure : updateReturnBlock/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
updateReturnBlock(Label, Vars, IcodeCfg) -> 
    Block = hipe_icode_cfg:bb(IcodeCfg, Label),
    Code = hipe_bb:code(Block),
    NewCode = updateReturnCode(Code, Vars),
    NewBlock = hipe_bb:code_update(Block, NewCode),
    hipe_icode_cfg:bb_add(IcodeCfg, Label, NewBlock).

updateReturnCode(Code, DstLst) -> updateReturnCode(Code, DstLst, []).
updateReturnCode([I| Code], DstLst, Res) -> 
    case hipe_icode:is_return(I) of
	true ->
	    updateReturnCode(Code, DstLst, [hipe_icode:mk_return(DstLst)|Res]);
	false ->
	    updateReturnCode(Code, DstLst, [I|Res])
    end;
updateReturnCode([], _, Res) -> lists:reverse(Res).  


%%>----------------------------------------------------------------------<
%% Procedure : optimizeCalls/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimizeCalls([Call|CallLst], _Opts, List) ->
    {callPair, Caller, {Callee, {matchSize, _, DstLst}}} = Call,
    NewList = optimizeCall(List, Caller, Callee, DstLst),
    optimizeCalls(CallLst, _Opts, NewList);
optimizeCalls([], _Opts, List) -> List.


%%>----------------------------------------------------------------------<
%% Procedure : optimizeCall/4
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
optimizeCall(List, Caller, Callee, DstLst) -> 
    optimizeCall(List, Caller, Callee, DstLst, []).
optimizeCall([{Mfa, Icode}|List], Mfa, Callee, DstLst, Res) ->
    {_LMin,LMax} = hipe_icode:icode_label_range(Icode),
    hipe_gensym:set_label(icode,LMax+1),
    {_VMin,VMax} = hipe_icode:icode_var_range(Icode),
    hipe_gensym:set_var(icode,VMax+1),
    %% Icode2 = hipe_icode:fixup_fallthroughs(Icode),
    Icode2 = hipe_icode:strip_comments(Icode),
    Cfg = hipe_icode_cfg:linear_to_cfg(Icode2),
    NewIcode = findAndUpdateCalls(Cfg, Callee, DstLst),
    optimizeCall(List, Mfa, Callee, DstLst, [{Mfa, NewIcode}|Res]);
optimizeCall([I|List], Caller, Callee, DstLst, Res) ->
    optimizeCall(List, Caller, Callee, DstLst, [I|Res]);
optimizeCall([],_,_,_,Res) -> lists:reverse(Res).


%%>----------------------------------------------------------------------<
%% Procedure : findAndUpdateCall/3
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
findAndUpdateCalls(Cfg, Callee, DstLst) ->
    Labels = hipe_icode_cfg:labels(Cfg), 
    Cfg2 = findAndUpdateCalls(Cfg, Labels, Callee, DstLst, []),
    hipe_icode_cfg:cfg_to_linear(Cfg2).
findAndUpdateCalls(Cfg, [L|Labels], Callee, DstLst, Visited) ->
    %% Block = hipe_icode_cfg:bb(Cfg, L),
    %% Code = hipe_bb:code(Block),
    case containCorrectCall(Cfg, L, Callee, DstLst) of
	true ->
	    Block = hipe_icode_cfg:bb(Cfg,L),
	    Code = hipe_bb:code(Block),
	    {NewCode, OldVar} = updateCode(Code, Callee, DstLst),
	    NewBlock = hipe_bb:code_update(Block, NewCode),
	    Cfg2 = hipe_icode_cfg:bb_add(Cfg, L, NewBlock),
	    Cfg3 = cleanUpAffectedCode(Cfg2, OldVar, Callee, L, Visited),
	    findAndUpdateCalls(Cfg3, Labels, Callee, DstLst, [L|Visited]);
	false ->
	    findAndUpdateCalls(Cfg, Labels, Callee, DstLst, [L|Visited])
    end;
findAndUpdateCalls(Cfg,[],_,_,_) -> Cfg.

containCorrectCall(Cfg, Label, Callee, DstLst) ->
    Block = hipe_icode_cfg:bb(Cfg,Label),
    Code = hipe_bb:code(Block),
    case containCallee(Code, Callee) of
	{true, OldVar} ->
	    SuccMap = hipe_icode_cfg:succ_map(Cfg),
	    Succs = hipe_icode_cfg:succ(SuccMap,Label),
	    checkForUnElems(Succs, OldVar, DstLst, Cfg);
	false ->
	    false
    end.

checkForUnElems([], _,_,_) -> false;
checkForUnElems([Succ|Succs], OldVar, DstLst, Cfg) ->
    Block = hipe_icode_cfg:bb(Cfg,Succ),
    Code = hipe_bb:code(Block),
    case checkForUnElems2(Code, OldVar, DstLst, []) of
	true ->
	    true;
	false ->
	    checkForUnElems(Succs, OldVar, DstLst, Cfg)
    end.
    
checkForUnElems2([I|Code], OldVar, DstLst, DstRes) ->
    case isCallPrimop(I, unsafe_element) of
	true ->
	    case (hipe_icode:call_args(I) == OldVar) of
		true ->
		    [Dst] = hipe_icode:call_dstlist(I),
		    case lists:member(Dst, DstLst) of
			true ->
			    checkForUnElems2(Code, OldVar, 
					     DstLst, [Dst|DstRes]);
			false ->
			    checkForUnElems2(Code, OldVar, DstLst, DstRes)
		    end;
		false ->
		    checkForUnElems2(Code, OldVar, DstLst, DstRes)
	    end;
	false ->
	    checkForUnElems2(Code, OldVar, DstLst, DstRes)
    end;
checkForUnElems2([], _, DstLst, DstRes) -> DstLst == lists:reverse(DstRes).


containCallee([I|Code], Callee) ->
    case isCallLocal(I, Callee) of
	true ->
	    {true, hipe_icode:call_dstlist(I)};
	false ->
	    containCallee(Code, Callee)
    end;
containCallee([],_) -> false.


updateCode(Code, Callee, DstLst) -> 
    updateCode(Code, Callee, DstLst, [], none).
updateCode([I|Code], Callee, DstLst, Res, OldVar) ->
    case isCallLocal(I, Callee) of
	true ->
	    Var = hipe_icode:call_dstlist(I),
	    I2 = hipe_icode:call_dstlist_update(I, DstLst),
	    updateCode(Code, Callee, DstLst, [I2|Res], Var);
	false ->
	    updateCode(Code, Callee, DstLst, [I|Res], OldVar)
    end;
updateCode([],_,_,Res, OldVar) -> {lists:reverse(Res), OldVar}.


cleanUpAffectedCode(Cfg, OldVar, Callee, Label, Visited) ->
    Block = hipe_icode_cfg:bb(Cfg,Label),
    Code = hipe_bb:code(Block),
    {CodeBefore, CodeAfter, DstLst} = divideAtCall(Code, Callee),
    {NewCodeAfter, ContLab, FailLab} = findType(CodeAfter, OldVar),
    ContBlock = hipe_icode_cfg:bb(Cfg,ContLab),
    SuccMap = hipe_icode_cfg:succ_map(Cfg),
    Succs = hipe_icode_cfg:succ(SuccMap,ContLab),
    ContCode = hipe_bb:code(ContBlock),
    {NewContCode, NewFailLab} = removeUnElems(ContCode, OldVar, DstLst),
    NewBlock = hipe_bb:code_update(Block, 
				   CodeBefore ++ NewCodeAfter ++ NewContCode),
    Cfg2 = hipe_icode_cfg:bb_add(Cfg, Label, NewBlock),
    Cfg3 = resolveSuccBlocks(Succs, OldVar, DstLst, [Label|Visited],
			     NewFailLab, Cfg2),
    Cfg4 = insertMiddleFailBlock(Cfg3, NewFailLab, FailLab, OldVar, DstLst),
    Cfg4.



divideAtCall(Code, Caller) ->
    divideAtCall(Code, Caller, []).
divideAtCall([I|Code], Caller, Tail) ->
    case isCallLocal(I, Caller) of
	true ->
	    {lists:reverse([I|Tail]), Code, hipe_icode:call_dstlist(I)};
	false ->
	    divideAtCall(Code, Caller, [I|Tail])
    end;
divideAtCall([], _, Tail) -> {Tail, []}.

findType(CodeAfter, OldVar) -> findType(CodeAfter, OldVar, [], {none,none}).
findType([I|Code], OldVar, Rest, Succs) ->
    case hipe_icode:is_type(I) of
	true ->
	    case hipe_icode:type_args(I) == OldVar of
		true ->
		    TrueLab = hipe_icode:type_true_label(I),
		    FalseLab = hipe_icode:type_false_label(I),
		    findType(Code, OldVar, Rest, {TrueLab, FalseLab});
		false ->
		    findType(Code, OldVar, [I|Rest], Succs)
	    end;
	false ->
	    case hipe_icode:is_move(I) of
		true ->
		    case [hipe_icode:move_src(I)] == OldVar of
			true ->
			    findType(Code, hipe_icode:move_dst(I), 
				     [I|Rest], Succs);
			false ->
			    findType(Code, OldVar, [I|Rest], Succs)
		    end;
		false ->
		    findType(Code, OldVar, [I|Rest], Succs)
	    end
    end;
findType([],_,Rest, {TrueLab, FalseLab}) ->
    {lists:reverse(Rest), TrueLab, FalseLab}.


%% Nesting hell... check for redundancies.
%% ------------------------------------------
removeUnElems(Code, OldVar, DstLst) -> 
    removeUnElems(Code, OldVar, DstLst, [], false, none).

removeUnElems([I|Code], OldVar, DstLst, Res, Def, Lab)  ->
    case isCallPrimop(I, unsafe_element) of
	true ->
	    case (hipe_icode:call_args(I) == OldVar) of
		true ->
		    removeUnElems(Code, OldVar, DstLst, Res, Def, Lab);
		false ->
		    case lists:member(OldVar, hipe_icode:call_args(I)) of
			true ->
			    %% XXX: the following test seems mucho redundant,
			    %% hence commented out -- KOSTIS
			    %% case Def of
			    %%	true ->
				    removeUnElems(Code, OldVar, DstLst,  
						  [I|Res], Def, Lab);
			    %%	false ->
			    %%	    removeUnElems(Code, OldVar, DstLst, 
			    %%			  [I|Res], Def, Lab)
			    %% end;
			false ->
			    io:format("Borde aldrig kunna hamna här!", []),
			    removeUnElems(Code, OldVar, DstLst, 
					  [I|Res], Def, Lab)
		    end
	    end;
	false  ->
	    case hipe_icode:is_move(I) of
		true ->
		    case hipe_icode:move_src(I) == OldVar of
			true ->
			    NewVar = hipe_icode:move_dst(I),
			    removeUnElems(Code, NewVar, DstLst, 
					  [I|Res], Def, Lab);
			false ->
			    removeUnElems(Code, OldVar, DstLst, 
					  [I|Res], Def, Lab)
		    end;
		false ->
		    case hipe_icode:is_type(I) and not Def of
			true ->
			    case Lab == none of
				true ->
				    NewFalseLab = 
					hipe_gensym:get_next_label(icode);
				false ->
				    NewFalseLab = Lab
			    end,
			    _I2 = updateTypeFalseLabel(I, NewFalseLab),
			    removeUnElems(Code, OldVar, DstLst, 
					  [I|Res], Def, NewFalseLab);
			false ->
			    case lists:member(OldVar, hipe_icode:uses(I)) 
				and Def of
				true ->
				    removeUnElems(Code, OldVar, 
						  DstLst,
						  [I|Res], Def, Lab);
				false ->
				    case lists:member(OldVar, 
						      hipe_icode:defines(I)) of
					true ->
					    removeUnElems(Code,OldVar,
							  DstLst,
							  [I|Res],true, Lab);
					false ->
					    removeUnElems(Code, OldVar, 
							  DstLst, 
							  [I|Res], Def, Lab)
				    end
			    end
		    end
	    end
    end;
removeUnElems([], _, _,Res,_, Lab) -> {lists:reverse(Res), Lab}.
			    

updateTypeFalseLabel(Instr, NewFalseLabel) ->
    TrueLabel = hipe_icode:type_true_label(Instr),
    Args = hipe_icode:type_args(Instr),
    Type = hipe_icode:type_type(Instr),
    hipe_icode:mk_type(Args, Type, TrueLabel, NewFalseLabel).
    

resolveSuccBlocks(Succs, OldVar, DstLst, Visited, FailLab, Cfg) -> 
    NewSuccs = [X || X <- Succs, not lists:member(X, Visited)],
    resolveSuccBlocks2(NewSuccs, OldVar, DstLst, Visited, FailLab, Cfg). 
resolveSuccBlocks2([Succ|Succs], OldVar, DstLst, Vis, FailLab, Cfg) -> 
    Block = hipe_icode_cfg:bb(Cfg,Succ),
    Code = hipe_bb:code(Block),
    {NewCode, ReDefined} = checkUsesDefs(Code, OldVar, DstLst, FailLab),
    NewBlock = hipe_bb:code_update(Block, NewCode),
    Cfg2 = hipe_icode_cfg:bb_add(Cfg, Succ, NewBlock),
    case ReDefined of
	true ->
	    resolveSuccBlocks2(Succs, OldVar, DstLst, 
			       [Succ|Vis],FailLab,Cfg2);
	false ->
	    SuccMap = hipe_icode_cfg:succ_map(Cfg),
	    NewSuccs = hipe_icode_cfg:succ(SuccMap,Succ),
	    NewSuccs2 = [X || X <- NewSuccs, not lists:member(X, Vis++Succs)],
	    resolveSuccBlocks2(NewSuccs2++Succs,OldVar,DstLst,
			       [Succ|Vis],FailLab,Cfg2)
    end;
resolveSuccBlocks2([], _, _, _, _, Cfg) -> Cfg.


checkUsesDefs(Code,OldVar,DstLst, FailLab) -> 
    checkUsesDefs(Code,OldVar,DstLst,FailLab,[],false).
checkUsesDefs([I|Code],OldVar,DstLst,FailLab,Res,Defined) ->
    [OVar] = OldVar,
    case hipe_icode:is_move(I) of
	true ->
	    case hipe_icode:move_src(I) == OldVar of
		true ->
		    NewVar = hipe_icode:move_dst(I),
		    checkUsesDefs(Code, NewVar, DstLst, 
				  FailLab, [I|Res], true);
		false ->
		    case lists:member(OVar, hipe_icode:defines(I)) of
			true ->
			    checkUsesDefs(Code,OldVar, DstLst, 
					  FailLab, [I|Res], true);
			false ->
			    checkUsesDefs(Code,OldVar,DstLst,
					  FailLab, [I|Res], Defined)
		    end
	    end;
	false ->
	    case hipe_icode:is_type(I) and not Defined of
		true ->
		    case FailLab =/= none of
			true ->
			    _I2 = updateTypeFalseLabel(I, FailLab),
			    checkUsesDefs(Code,OldVar, DstLst, 
					  FailLab, [I|Res], Defined);
			false ->
			    checkUsesDefs(Code,OldVar, DstLst, 
					  FailLab, [I|Res], Defined)
		    end;
		false ->
		    case (lists:member(OVar, hipe_icode:uses(I))) and 
			(not Defined) and (FailLab =/= none) of
			true ->
			    Tpl =  hipe_icode:mk_primop(OldVar, mktuple, 
							DstLst), 
			    checkUsesDefs(Code, OldVar, DstLst, 
					  FailLab, [I, Tpl|Res], true);
			false ->
			    case lists:member(OVar, hipe_icode:defines(I)) of
				true ->
				    checkUsesDefs(Code,OldVar, DstLst, 
						  FailLab, [I|Res], true);
				false ->
				    checkUsesDefs(Code, OldVar, DstLst, 
						  FailLab, [I|Res],Defined)
			    end
		    end
	    end
    end;
checkUsesDefs([],_,_,_,Res,Defined) -> {lists:reverse(Res),Defined}.


insertMiddleFailBlock(Cfg, NewFailLabel, OldFailLabel, OldVar, DstLst) ->
    case NewFailLabel == none of
	true ->
	    Cfg;
	false ->
	    NewCode = [hipe_icode:mk_primop(OldVar, mktuple, DstLst), 
		       hipe_icode:mk_goto(OldFailLabel)],
	    NewBlock = hipe_bb:mk_bb(NewCode),
	    hipe_icode_cfg:bb_add(Cfg, NewFailLabel, NewBlock)
    end.


isCallLocal(Instr, Fun) ->
    case hipe_icode:is_call(Instr) of
	true ->
	    ((hipe_icode:call_type(Instr) == local) and
	     (hipe_icode:call_fun(Instr) == Fun));
	false ->
	    false
    end.

isCallPrimop(Instr, Fun) ->
    case hipe_icode:is_call(Instr) of
	true ->
	    case is_tuple(hipe_icode:call_fun(Instr)) of
		true ->
		    ((hipe_icode:call_type(Instr) == primop) and
		     (element(1,hipe_icode:call_fun(Instr)) == Fun));
		false ->
		    ((hipe_icode:call_type(Instr) == primop) and
		     (hipe_icode:call_fun(Instr) == Fun))
	    end;
	false ->
	    false
    end.


%% >-------------------------< Debug code >------------------------------<

-ifdef(DEBUG_MULRET).

%%>----------------------------------------------------------------------<
%% Procedure : printTable/1
%% Purpose   : 
%% Arguments : 
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<
printTable(Mod, Exports, {FunLst, CallLst}) ->
    {Y,Mo,D} = date(), {H,Mi,S} = time(),
    io:format("Module: ~w - (~w/~w-~w, ~w:~w:~w)~n=======~n", 
	      [Mod,D,Mo,Y,H,Mi,S]),
    io:format("Exports: ~w~n", [Exports]), 
    io:format("FunList: ~n"),
    printFunList(FunLst),
    io:format("CallList: ~n"),
    printCallList(CallLst).

printFunList([Fun|FunLst]) ->
    io:format("       ~w~n", [Fun]),
    printFunList(FunLst);
printFunList([]) -> io:format("~n").

printCallList([Call|CallLst]) ->
    io:format("       ~w~n", [Call]),
    printCallList(CallLst);
printCallList([]) -> io:format("~n").

-endif.

%% >----------------------------< Old code >--------------------------------<

%% %%>----------------------------------------------------------------------<
%% %  Procedure : findCallCode/3
%% %  Purpose   : 
%% %  Arguments : 
%% %  Return    : 
%% %  Notes     : 
%% %%>----------------------------------------------------------------------<
%% findCallCode(List, Callee, DstLst) -> findCallCode(List, Callee, DstLst, []).
%% findCallCode([I=#call{'fun'=Callee, dstlist=Var, type=local}, I2, I3|List], 
%% 	     Callee, DstLst, Res) ->
%%     NewList = removeUnElems(List, Var),
%%     %% _Uses = checkForUses(NewList, Var, DstLst),
%%     Size = length(DstLst),
%%     case I2 of
%% 	#type{type={tuple, Size}, args=Var, true_label=Label} ->
%% 	    case I3 of
%% 		#label{name=Label} ->
%% 		    findCallCode(NewList, Callee, DstLst, 
%% 				 [I#call{dstlist=DstLst}|Res]);
%% 		_ ->
%% 		    findCallCode(NewList, Callee, DstLst, 
%% 				 [#goto{label=Label}, 
%% 				  I#call{dstlist=DstLst}|Res])
%% 	    end;
%% 	_ ->
%% 	    findCallCode(NewList, Callee, DstLst, 
%% 			 [I2,I#call{dstlist=DstLst}|Res])
%%     end;
%% findCallCode([I|List], Callee, DstLst, Res) ->
%%     findCallCode(List, Callee, DstLst, [I|Res]);
%% findCallCode([],_,_,Res) -> lists:reverse(Res).


%% %%>----------------------------------------------------------------------<
%% %  Procedure : checkForUses
%% %  Purpose   : 
%% %  Arguments : 
%% %  Return    : 
%% %  Notes     : 
%% %%>----------------------------------------------------------------------<
%% checkForUses(List, Var, Dsts) -> checkForUses(List, Var, Dsts, [], List).
%% checkForUses([I|List], Var, Dsts, Rest, Code) ->
%%     Defs = hipe_icode:defines(I),
%%     Uses = hipe_icode:uses(I),
%%     case lists:member(Var, Uses) of
%% 	true ->
%% 	    true;
%% 	false ->
%% 	    case lists:member(Var, Defs) of
%% 		true ->
%% 		    false;
%% 		false ->
%% 		    case hipe_icode:is_branch(I) of
%% 			true ->
%% 			    Succs = hipe_icode:successors(I),
%% 			    checkSuccsForUses(Succs, Var, Dsts, Rest, Code);
%% 			false ->
%% 			    checkForUses(List, Var, Dsts, [I|Rest], Code)
%% 		    end
%% 	    end
%%     end;
%% checkForUses([],_,_,_,_) -> false.

%% checkSuccsForUses(Succs, Var, Dsts, Rest, Code) -> 
%%     checkSuccsForUses(Succs, Var, Dsts, Rest, Code, false).
%% checkSuccsForUses([S|Succs], Var, Dsts, Rest, Code, Res) ->
%%     List = gotoLabel(S, Code),
%%     Used = checkForUses(List, Var, Dsts, Rest, Code),
%%     checkSuccsForUses(Succs, Var, Code, Dsts, Used and Res); 
%% checkSuccsForUses([],_,_,_,_,Res) -> Res.

%% gotoLabel(L, [L|List]) -> List;
%% gotoLabel(L, [_|List]) -> gotoLabel(L, List);
%% gotoLabel(_, []) -> [].
    


%% %%>----------------------------------------------------------------------<
%% %  Procedure : removeUnElems/2
%% %  Purpose   : 
%% %  Arguments : 
%% %  Return    : 
%% %  Notes     : Fixa så att funktionen använder defines(I) istället och 
%% %              selektorer istället för att matcha på #call{}. Lätt gjort.  
%% %%>----------------------------------------------------------------------<
%% removeUnElems(List, Var) -> removeUnElems(List, Var, []).
%% removeUnElems([#call{'fun'={unsafe_element,_}, args=Var}|List], Var, Res) ->
%%     removeUnElems(List, Var, Res);
%% removeUnElems([I=#move{dst=Var}|List], [Var], Res) ->
%%     lists:reverse(Res) ++ [I|List];
%% removeUnElems([I=#fmove{dst=Var}|List], [Var], Res) ->
%%     lists:reverse(Res) ++ [I|List];
%% removeUnElems([I=#call{dstlist=Var}|List], Var, Res) ->
%%     lists:reverse(Res) ++ [I|List];
%% removeUnElems([I|List], Var, Res) ->
%%     removeUnElems(List, Var, [I|Res]);
%% removeUnElems([],_,Res) -> lists:reverse(Res).

%% removeUnElems(List, Var) -> removeUnElems(List, Var, []).
%% removeUnElems([I|List], Var, Res) ->
%%     Defs = hipe_icode:defines(I),
%%     case hipe_icode:is_call(I) of
%% 	true ->
%% 	    Fn = hipe_icode:call_fun(I),
%% 	    case (hipe_icode:call_args(I) == Var) and is_tuple(Fn) of
%% 		true ->
%% 		    case element(1,Fn) == unsafe_element of 
%% 			true ->
%% 			    removeUnElems(List, Var, Res);
%% 			false ->
%% 			    case lists:member(Var, Defs) of
%% 				true ->
%% 				    lists:reverse(Res) ++ [I|List];
%% 				false ->
%% 				    removeUnElems(List, Var, [I|Res])
%% 			    end 
%% 		    end;
%% 		false ->
%% 		    case lists:member(Var, Defs) of
%% 			true ->
%% 			    lists:reverse(Res) ++ [I|List];
%% 			false ->
%% 			    removeUnElems(List, Var, [I|Res])
%% 		    end
%% 	    end;
%% 	false ->
%% 	    case lists:member(Var, Defs) of
%% 		true ->
%% 		    lists:reverse(Res) ++ [I|List];
%% 		false ->
%% 		    removeUnElems(List, Var, [I|Res])
%% 	    end
%%     end;
%% removeUnElems([],_,Res) -> lists:reverse(Res).
    

%% Old findDefine that also could update it.
%% -----------------------------------------
%% findDefine(Code, Var) -> findDefine(Code, Var, [], []).
%% findDefine([#call{dstlist=Var,'fun'=mktuple,args=Vs}|Code],Var,NewCode,_)-> 
%%     findDefine(Code, Var, NewCode, Vs);
%% findDefine([I=#move{dst=Var, src=Src}|Code], [Var], NewCode, _) ->
%%     case Src of
%% 	{var, _} ->
%% 	    findDefine(Code, [Src], [I|NewCode], [Src]);
%% 	{const,{flat, Tuple}} ->
%% 	    findDefine(Code, [Var], [I|NewCode], []) %% Check this case! [Var]
%%     end;
%% findDefine([I|Code], Var, NewCode, Vars) ->
%%     findDefine(Code, Var, [I|NewCode], Vars);
%% findDefine([], _, NewCode, Vars) ->
%%     case length(Vars) of
%% 	0 ->
%% 	    notFound;
%% 	1 ->
%% 	    {notFound, Vars};
%% 	_ ->
%% 	    {found, lists:reverse(NewCode), Vars}
%%     end.
    
%% modifyCode(Code, Var) ->
%%     [#return{vars=Var}|Code2] = lists:reverse(Code),
%%     case (length(Var) =< hipe_rtl_arch:nr_of_return_regs()) of
%% 	true ->
%% 	    {Arity, Code3} = modifyCode(Code2, Var, []),
%% 	    {Arity, Code3};
%% 	false ->
%% 	    {1, Code}
%%     end.

%% modifyCode([I|Code], Var, Res) ->
%%     case scanInstr(I, Var) of
%% 	{move, Arity, VarLst} ->
%% 	    Code2 = [#return{vars=VarLst}, I |lists:reverse(Res) ++ Code],
%% 	    {Arity, lists:reverse(Code2)};
%% 	{mktuple, Arity, VarLst} ->
%% 	    Code2 = [#return{vars=VarLst}|lists:reverse(Res) ++ Code],
%% 	    {Arity, lists:reverse(Code2)};
%% 	other ->
%% 	    modifyCode(Code, Var, [I|Res])
%%     end;
%% modifyCode([], Var, Res) ->
%%     {1, lists:reverse(Res) ++ [#return{vars=Var}]}.
    
%% scanInstr(#call{dstlist=Var, 'fun'=mktuple, args=Lst}, Var) ->
%%     {mktuple, length(Lst), Lst};
%% scanInstr(_,_) -> other.


%% DomTree = hipe_dominators:domTree_create(IcodeCfg),
%% hipe_dominators:domTree_pp(DomTree),
%% IcodeCfg.

%% Block = hipe_icode_cfg:bb(IcodeCfg, Label),
%% Code = hipe_bb:code(Block),
%% Block2 = hipe_bb:code_update(Block, Code2),
%% IcodeCfg2 = hipe_icode_cfg:bb_add(IcodeCfg, Label, Block2).
    
    
%% printCode(Cfg) ->
%%     Labels = hipe_icode_cfg:labels(Cfg),
%%     {_,_,{_,F,_,_,_,_,_,_},_} = Cfg,
%%     io:format("~nFunction: ~w~n", [F]),
%%     Print = fun(Label) ->
%% 		    Block = hipe_icode_cfg:bb(Cfg, Label),
%% 		    Code = hipe_bb:code(Block),
%% 		    io:format("Label: ~w~n", [Label]),
%% 		    lists:foreach(fun(I) -> io:format("~w~n", [I]) end, Code),
%% 		    io:format("~n")
%% 	    end,
%%     lists:foreach(Print, Labels).
				
%% io:format("Before modification...~n"),
%% lists:foreach(fun(A) -> io:format("~w~n", [A]) end, Code),
%% io:format("~n"),
    
%% io:format("After modification...~n"),
%% lists:foreach(fun(A) -> io:format("~w~n", [A]) end, Code2),
%% io:format("~n"),

%% printList(File, [{Mfa, #icode{code=Code, params=Parms}}|List]) ->
%%     io:format(File, "Mfa: ~w - Params: ~w~n", [Mfa, Parms]), 
%%     printList2(File, Code),
%%     printList(File, List);
%% printList(_, []) -> ok.

%% printList2(File, []) -> io:format(File, "~n~n", []);
%% printList2(File, IList) when is_list(IList) ->  
%%     [I|List] = IList,
%%     io:format(File, "~w~n", [I]),
%%     printList2(File, List);
%% printList2(File, SomethingElse) -> 
%%     io:format(File, "Got: ~w~n", [SomethingElse]).

%% optimizeDefine([#call{dstlist=Var,'fun'=mktuple,args=Vs}|Code], 
%% 	       Var, _, Res) ->
%%     case length(Vs) of
%% 	1 ->
%% 	    {none, noOpt};
%%     	_ ->
%% 	    optimizeDefine(Code, Var, Vs, Res)
%%     end;
%% optimizeDefine([I=#move{dst=Var, src=Src}|Code], [Var], Rets, Res) ->
%%     case Src of
%% 	{var, _} ->
%% 	    optimizeDefine(Code, [Src], Rets, Res);
%% 	{const,{flat, Tuple}} when is_tuple(Tuple) ->
%% 	    optimizeDefine(Code, [Var], tuple_to_list(Tuple), [I|Res]);
%% 	{const, {flat, _}} ->
%% 	    {none, noOpt};
%% 	_ ->
%% 	    optimizeDefine(Code, [Var], Rets, [I|Res])
%%     end;
%% optimizeDefine([I|Code], Var, Rets, Res) ->
%%     optimizeDefine(Code, Var, Rets, [I|Res]);
%% optimizeDefine([], Var, Rets, Res) ->
%%     case Rets of
%% 	[] ->
%% 	    {none, Res, Var};
%% 	_ ->
%% 	    {found, Res, Rets}
%%     end.

%% Ett redundant fall ur removeUnElems(...)
%% case lists:member(OldVar, hipe_icode:uses(I)) of
%% 				true ->
%% 				    case Def of
%% 					defined ->
%% 					    removeUnElems(Code, OldVar, 
%% 							  [I|Res], Def);
%% 					not_defined ->
%% 					    removeUnElems(Code, OldVar, 
%% 							  [I|Res], Def)
%% 				    end;
%% 				false ->
%% 				    case lists:member(OldVar, 
%% 						      hipe_icode:defines(I)) of
%% 					true ->
%% 					    removeUnElems(Code,OldVar,
%% 							  [I|Res],defined);
%% 					false ->
%% 					    removeUnElems(Code, OldVar, 
%% 							  [I|Res], Def)
%% 				    end
%% 			    end
