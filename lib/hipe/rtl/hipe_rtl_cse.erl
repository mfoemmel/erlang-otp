%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			COMMON SUBEXPRESSIONS
%%
%% Get rid of pesky CSE's in Sparc code.
%%
%% - works per basic block
%% - probably slow
%%
%% *** UNFINISHED ***
%% - add the stuff that keeps track of available expressions!

-module(hipe_rtl_cse).
-export([block/1,blocks/1,cfg/1,ebb/1,fix/1]).

% Performed on RTL code


% test(CFG) ->
%     io:format('==================== LOCAL CSE ====================~n',[]),
%     hipe_rtl_cfg:pp(cfg(CFG)),
%     io:format('====================  EBB  CSE ====================~n',[]),
%     hipe_rtl_cfg:pp(ebb(CFG)),
%     io:format('==================  EBB  CSE + OPT ==================~n',[]),
%     hipe_rtl_cfg:pp(hipe_rtl_prop:cfg(ebb(CFG))),
%     ok.

cfg(CFG) ->
    blocks(CFG).

blocks(CFG) ->
    cse_blocks(hipe_rtl_cfg:labels(CFG),CFG).

% test_mfa(MFA) ->
%     test(main2:to_rtl(MFA,[fast])).

% mfas([]) -> ok;
% mfas([MFA|MFAs]) ->
%     io:format('translating ~p ... ',[MFA]),
%     RTL = main2:to_rtl(MFA,[fast]),
%     io:format('(cse) ... ',[]),
%     case catch cfg(RTL) of
% 	{'EXIT',_} ->
% 	    io:format('failed~n',[]);
% 	_ ->
% 	    io:format('done~n',[])
%     end,
%     mfas(MFAs).

cse_blocks([],CFG) -> CFG;
cse_blocks([L|Ls],CFG) ->
    NewBlk = hipe_bb:mk_bb(block(hipe_bb:code(hipe_rtl_cfg:bb(CFG,L)))),
    NewCFG = hipe_rtl_cfg:bb_update(CFG,L,NewBlk),
    cse_blocks(Ls,NewCFG).

block(Blk) ->
    cse(Blk,empty_cache()).

ebb(CFG) ->
    EBBs = hipe_rtl_ebb:cfg(CFG),
    NewCFG = cse_ebbs(EBBs,CFG,empty_cache()).

% cse_ebb(EBB,CFG) ->
%    cse_ebb(EBB,CFG,empty_cache()).

cse_ebb(EBB,CFG,Cache) ->
    case hipe_rtl_ebb:type(EBB) of
	leaf ->
	    CFG;
	node ->
	    L = hipe_rtl_ebb:node_label(EBB),
	    BB = hipe_bb:code(hipe_rtl_cfg:bb(CFG,L)),
	    {NewBB,NewCache} = cse0(BB,Cache),
	    NewCFG = hipe_rtl_cfg:bb_update(CFG,L,hipe_bb:mk_bb(NewBB)),
	    SuccEBBs = hipe_rtl_ebb:node_successors(EBB),
	    cse_ebbs(SuccEBBs,NewCFG,NewCache)
    end.

cse_ebbs([],CFG,Cache) -> CFG;
cse_ebbs([EBB|EBBs],CFG,Cache) ->
    cse_ebbs(EBBs, cse_ebb(EBB,CFG,Cache), Cache).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Here is the actual CSE finder.

cse(Xs,Cache) ->
    {NewXs,_} = cse0(Xs,Cache),
    NewXs.

% Note: we could return NewI as a single instruction, except
% that we want to add a comment when CSE occurs. This occurs
% in the call to move/3.
%
% If it is unnecessary, change all NewI = [I] into NewI = I,
% rewrite move/3 to just return the move-instr, and change
% NewI ++ NewIs => [NewI|NewIs].

cse0([],Cache) -> {[],Cache};
cse0([I|Is],Cache) ->
    {NewI,NewCache} = cse_i(I,Cache),
    {NewIs,FinalCache} = cse0(Is,NewCache),
    { NewI ++ NewIs , FinalCache }.

cse_i(I,Cache) ->
    {T,Expr,Dst,Vs,Ws} = i(I),
    case T of
	barrier ->
	    {[I], empty_cache()};     % kill only untagged vars?
	no_cse ->
	    {[I], kill_defs(Cache,Vs)};
	maybe_cse ->
	    case scan_cache(Cache,Expr) of
		{found,Src} ->
		    NewI = move(Dst,Src,I),
		    NewCache = kill_defs(Cache,Vs),
		    { NewI, NewCache };
		not_found ->
		    NewI = [I],
		    NewCache = add_expr(Expr,Dst,Ws,kill_defs(Cache,Vs)),
		    { NewI, NewCache }
	    end
    end.

% Returns
%   { InstrType, Expression, DstVar, KilledVars, DependentVars }
%
% Where KilledVars and DependentVars are sets (as per the `sets' module)

i(Instr) ->
    Ds = hipe_rtl:defines(Instr),
    Us = hipe_rtl:uses(Instr),
    Killed = var_set(Ds),
    Dependent = gb_sets:union(var_set(Us),Killed),
    {Type,Dst,Expr} =
	case hipe_rtl:type(Instr) of
	    move -> 
		{maybe_cse,[hipe_rtl:move_dst(Instr)],
		 {move, hipe_rtl:move_src(Instr)}};
	    alu ->
		{maybe_cse,
		 [hipe_rtl:alu_dst(Instr)],
		 {alu,
		  hipe_rtl:alu_src1(Instr), hipe_rtl:alu_op(Instr),
		  hipe_rtl:alu_src2(Instr)}};
	    load_address ->
		{maybe_cse,
		 [hipe_rtl:load_address_dst(Instr)],
		 {address, hipe_rtl:load_address_address(Instr)}};
	    load_atom ->
		{maybe_cse,
		 [hipe_rtl:load_atom_dst(Instr)],
		 {atom, hipe_rtl:load_atom_atom(Instr)}};
	    call ->
		Fun = hipe_rtl:call_fun(Instr),
		case pure_bif(Fun) of
		    true ->
			{maybe_cse,
			 hipe_rtl:call_dst(Instr),
			 {bif, Fun, hipe_rtl:call_args(Instr)}};
		    false ->
			{barrier,none,none}
		end;
	    jsr -> 
		{barrier,none,none};
	    %% What is esr??
	    esr -> 
		{barrier,none,none};
	    jmp_link ->
		{barrier,none,none};
	    %% The following two
	    enter -> 
		{barrier,none,none};
	    return ->
		{barrier,none,none};
	    %% The rest are unsuitable as common subexprs
	    %% ... though you could make a case for loads from the heap.
	    _ ->
		{no_cse,none,none}
	end,
    { Type, Expr, Dst, Killed, Dependent }.

% *** UNFINISHED ***

pure_bif(X) when atom(X) -> true;
pure_bif(_) -> false.

move(Xs,Ys,Instr) ->
    [hipe_rtl:mk_comment({cse,Instr}) | move_list(Xs,Ys) ].

move_list([],[]) -> [];
move_list([X|Xs],[Y|Ys]) ->
    [ hipe_rtl:mk_move(X,Y) | move_list(Xs,Ys) ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The expression cache ADT
%
% Possibly use a hash table instead?

empty_cache() -> [].

scan_cache([],E) -> not_found;
scan_cache([{E,X,_}|_],E) -> {found,X};
scan_cache([_|Xs],E) -> scan_cache(Xs,E).

add_expr(Expr,Dst,Vars,Cache) ->
    [{Expr,Dst,Vars}|Cache].

kill_defs(Items,Vs) ->
    [ {Exp,Dst,Ws} || {Exp,Dst,Ws} <- Items, non_overlap(Vs,Ws) ].

non_overlap(Xs,Ys) ->
    gb_sets:is_empty(gb_sets:intersection(Xs,Ys)).

% Make a list of RTL vars into a set

var_set(Xs) ->
    vs(Xs,gb_sets:empty()).

vs([],Set) -> Set;
vs([X|Xs],Set) ->
    I = case hipe_rtl:is_var(X) of
	    true ->
		hipe_rtl:var_name(X);
	    false -> % must be 'reg'
		hipe_rtl:reg_name(X)
	end,
    NewSet = gb_sets:add(I,Set),
    vs(Xs, NewSet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Merging two caches into one is useful when we analyze non-trees.
% We do this in the simplest way possible: if an expression is present
% in the identical form in two caches, then CSE it.

% use a simple chaotic fixpoint: if a block's invalue changes,
% reanalyze it and its successors.
%
% - note: this version is grossly inefficient!

fix(CFG) ->
    Start = starting_points(CFG),
    In0 = empty_tab(),
    Succ = hipe_rtl_cfg:succ_map(CFG),
    In1 = analyze_all(Start,CFG,Succ,In0),
    cse_blks(hipe_rtl_cfg:labels(CFG),CFG,In1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyze_all([],CFG,Succ,In) -> In;
analyze_all([L|Ls],CFG,Succ,In) ->
    analyze_block(L,Ls,CFG,Succ,In).

analyze_block(L,Ls,CFG,Succ,In) ->
    Blk = hipe_bb:code(hipe_rtl_cfg:bb(CFG,L)),
    {_,OutCache} = cse0(Blk, cache_of(L,In)),
    { NewLs, NewIn } = prop_succ(hipe_rtl_cfg:succ(Succ,L),Ls,OutCache,In),
    analyze_all(NewLs,CFG,Succ,NewIn).

prop_succ([],Ls,OutCache,In) -> {Ls,In};
prop_succ([S|Ss],Ls,OutCache,In) ->
    case changed(S,OutCache,In) of
	{yes,NewIn} ->
	    prop_succ(Ss,[S|Ls],OutCache,NewIn);
	no ->
	    prop_succ(Ss,Ls,OutCache,In)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cse_blks([],CFG,In) ->
    CFG;
cse_blks([L|Ls],CFG,In) ->
    NewBlk = hipe_bb:mk_bb(cse(hipe_bb:code(hipe_rtl_cfg:bb(CFG,L)),cache_of(L,In))),
    NewCFG = hipe_rtl_cfg:bb_update(CFG,L,NewBlk),
    cse_blks(Ls,NewCFG,In).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_tab() -> hipe_hash:empty().

% For CSE, the starting value is 'all expressions' and the LUB operator
% is intersection.
%
% We represent this as follows:
% - when the block is first found, set its value to the incoming value
%   (= the intersection of all exprs and the incoming exprs)
% - less_than(Xs,Ys): if 

changed(L,NewCache,In) ->
    case hipe_hash:lookup(L,In) of
	not_found ->
	    {yes, hipe_hash:update(L,NewCache,In)};
	{found,OldCache} ->
	    case less_than(NewCache,OldCache) of
		true ->
		    no;
		false ->
		    {yes, hipe_hash:update(L, lub(NewCache,OldCache), In) }
	    end
    end.

cache_of(L,In) ->
    case hipe_hash:lookup(L,In) of
	{found,Cache} ->
	    Cache;
	not_found ->
	    empty_cache()
    end.

% least upper bound: must be present in both caches

lub(Cache1,Cache2) ->
    [ E || E <- Cache1, lists:member(E,Cache2) ].

% less_than(X,Y): if all elements in X are present in Y, then
% X is smaller than Y (in our lattice).

less_than([],_) -> true;
less_than([X|Xs],Ys) ->
    case lists:member(X,Ys) of
	true ->
	    less_than(Xs,Ys);
	false ->
	    false
    end.

starting_points(CFG) ->
    [ hipe_rtl_cfg:start(CFG) | hipe_rtl_cfg:fail_entrypoints(CFG) ].
