%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		  EXPRESSION-BASED DATAFLOW ANALYSES
%
% This file provides support for building analyses based on expressions.
%
%   table(CFG): returns {NextExpr,Exp,Kill,Names}
%      where NextExpr is the next index to assign an expression
%            Exp maps (Expr -> ExprID), the ID of the expression
%            Kill maps (Var -> set(ExprID)), the exprs killed by defining Var
%            Names maps (Expr -> Var), the variable name to use when ref.
%                  to Expr
%  

-module(hipe_expr_table).
-export([cfg/1,genkill/3]).
-compile(export_all).

-define(cfg,hipe_icode_cfg).
-define(code,hipe_icode).

cfg(CFG) ->
    exp_tab(?cfg:labels(CFG),CFG,1,no_exprs(),no_kills(),no_names()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute {Gen,Kill} per instruction, where Gen and Kill are
% sets of expressions. Returns [ {Label, [ {Gen,Kill} ]} ]

genkill(CFG,Exp,Kills) ->
    [ {L, genkill_block(hipe_bb:code(?cfg:bb(CFG,L)),Exp,Kills)}
     || L <- ?cfg:labels(CFG) ].

genkill_block(Is,Exp,Kills) ->
    [ genkill_instr(I,Exp,Kills) || I <- Is ].

genkill_instr(Instr,Exp,Kills) ->
    Kill = set:union_list( [ kills_of(D,Kills) 
			    || D <- ?code:defines(Instr) ] ),
    Gen = init_set(find_instr_index(Instr,Exp)),
    {Gen,Kill}.

init_set({yes,I}) -> set:init([I]);
init_set(no) -> set:empty().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Walk the CFG to find all expressions, name them, etc.
% Returns:
%   N: next index to assign an expression
%   Exp: maps an expression to an integer index
%     'Names' each unique expression uniquely and concisely
%   Kill: maps a variable to the list of indices it kills
%     Used in computing transfer functions.
%   Names: maps an expression to a new variable name
%     This is used to insert "x := e; t := x" in the code;
%     now t can substitute for occurrences of e, even if x is overwritten.

exp_tab([],CFG,N,Exp,Kill,Names) ->
    {N,Exp,Kill,Names};
exp_tab([L|Ls],CFG,N,Exp,Kill,Names) ->
    {NewN,NewExp,NewKill,NewNames} = exp_block(?cfg:bb(CFG,L),
					       N,Exp,Kill,Names),
    exp_tab(Ls,CFG,NewN,NewExp,NewKill,NewNames).

exp_block(BB,N,Exp,Kill,Names) ->
    exp_instrs(hipe_bb:code(BB),N,Exp,Kill,Names).

exp_instrs([],N,Exp,Kill,Names) -> {N,Exp,Kill,Names};
exp_instrs([X|Xs],N,Exp,Kill,Names) ->
    {NewN,NewExp,NewKill,NewNames} = exp_instr(X,N,Exp,Kill,Names),
    exp_instrs(Xs,NewN,NewExp,NewKill,NewNames).

exp_instr(X,N,Exp,Kill,Names) ->
    case suitable_rhs(X) of
	no ->
	    {N,Exp,Kill,Names};
	{yes,E} ->
	    case has_index(E,Exp) of 
	       true ->
		  {N,Exp,Kill,Names};
	       false ->
		  NewN = N+1,
		  NewExp = add_exp(E,N,Exp),
		  NewKill = kill_map(uses(X),N,Kill),
		  NewNames = new_name(E,N,Names),
		  {NewN,NewExp,NewKill,NewNames}
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

no_exprs() -> hash:empty().

has_index(E,Exp) ->
    case hash:lookup(E,Exp) of
	not_found -> false;
	{found,N} when integer(N) -> true
    end.

add_exp(E,N,Exp) ->
    hash:update(E,N,Exp).

instr_index(Instr,Exp) ->
    case suitable_rhs(Instr) of
	{yes,E} ->
	    expr_index(E,Exp);
	no ->
	    error(instr_index,'instr ~p has no index~n',[Instr])
    end.

find_instr_index(Instr,Exp) ->
    case suitable_rhs(Instr) of
	{yes,E} ->
	    {yes,expr_index(E,Exp)};
	no ->
	    no
    end.

expr_index(E,Exp) ->
    case hash:lookup(E,Exp) of
	{found,N} -> N;
	Else ->
	    error(expr_index,'expr ~p has no index~n',[E])
    end.

list_exprs(Exp) ->
    hash:list(Exp).

% Invert (expression -> index) mapping to be (index -> expression)
% Relies on 1-1 mapping

index_to_expr(Exp) ->
    hash:init( [ {Index,Exp1} || {Exp1,Index} <- list_exprs(Exp) ] ).

get_expr(InvExp,I) ->
    case hash:lookup(I,InvExp) of
	not_found ->
	    {index_not_found,I};
	{found,Exp} ->
	    Exp
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% At present, maps (Var -> Set(Killed_exprs))

no_kills() -> hash:empty().

kill_map([],N,Kill) -> Kill;
kill_map([X|Xs],N,Kill) ->
    kill_map(Xs,N,kill_map_var(X,N,Kill)).

kill_map_var(X,N,Kill) ->
  Old =
    case hash:lookup(X,Kill) of
      not_found ->
	set:empty();
      {found,Ns} ->
	Ns
    end,
  New = set:add_singleton(N,Old),
  hash:update(X,New,Kill).

kills_of(X,Kill) ->
    case hash:lookup(X,Kill) of
	{found,K} -> 
	    K;
	not_found -> 
	    set:empty()
    end.

list_kills(Kill) ->
    [ {X,set:list(S)} || {X,S} <- hash:list(Kill) ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** UNFINISHED ***
% new_name/3 should generate a real variable.
% (not essential right at this moment)

no_names() -> hash:empty().

new_name(E,N,Name) ->
    hash:update(E,N,Name).

list_names(Name) ->
    hash:list(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% We might use bitvectors here, so let's keep it abstract.
% At present, lists of bits.

no_gen() -> set:empty().

no_kill() -> set:empty().

union(Xs,Ys) ->
    set:union(Xs,Ys).

union_list([]) -> set:empty();
union_list([X|Xs]) ->
    union(X,union_list(Xs)).

diff(Xs,Ys) ->
    set:diff(Xs,Ys).

intersect(Xs,Ys) ->
    set:intersect(Xs,Ys).

member(X,Xs) ->
    set:member(X,Xs).

add_singleton(X,Xs) ->
    set:add_singleton(X,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(MFA,Msg,Args) ->
    io:format('ERROR (~p): ',[MFA]),
    io:format(Msg,Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** MISC ***
% (might be deleted)
%
% Compute transfer functions for all blocks in a CFG.

transf(CFG) ->
    transf(CFG,?cfg:pred_map(CFG),cfg(CFG)).

transf(CFG,Preds,{N,Exp,Kill,Names}) ->
    transf_blocks(CFG,Preds,Exp,Kill).

transf_blocks(CFG,Preds,Exp,Kill) ->
    [ {L, ?cfg:pred(Preds,L), transf_block(hipe_bb:code(?cfg:bb(CFG,L)),Exp,Kill)}
     || L <- ?cfg:reverse_postorder(CFG) ].

transf_block(Xs,Exp,Kill) ->
    transf_instrs(Xs,no_gen(),no_kill(),Exp,Kill).

transf_instrs([],G,K,Exp,Kill) -> {G,K};
transf_instrs([X|Xs],G0,K0,Exp,Kill) ->
    {G1,K1} = transf_instr(X,G0,K0,Exp,Kill),
    transf_instrs(Xs,G1,K1,Exp,Kill).

transf_instr(X,G0,K0,Exp,Kill) ->
    D = defines(X),
    {G1,K1} = kill_all(D,Kill,G0,K0),
    G2 = gen_expr(X,Exp,G1),
    {G2,K1}.

kill_all([],Kill,G,K) -> {G,K};
kill_all([X|Xs],Kill,G,K) ->
    Kills = kills_of(X,Kill),
    NewG = diff(G,Kills),
    NewK = union(K,Kills),
    {NewG,NewK}.

gen_expr(X,Exp,G0) ->
    case suitable_rhs(X) of
	no ->
	    G0;
	{yes,E} ->
	    add_singleton(expr_index(E,Exp),G0)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** PORTING ***
%
% These need to be redefined for each setting. At present, we assume
% it's Icode that's in use. The actual use will probably be for RTL or Sparc.

defines(X) ->
    ?code:defines(X).

uses(X) ->
    ?code:uses(X).


% The following function decides if the expression computed by the
% instruction is suitable for code motion.
%
% UN-suitable instructions include:
% - impure primitive operations, tests
% - conditional branches, stores, loads from the stack...
%
% Suitable instructions may be:
% - arithmetic instructions
% - calls to pure procedures
% - some loads (loads from the heap, which are never overwritten)
%
% Returns no | {yes,RHS}
%
% *** UNFINISHED ***
% - modify suitably; this version is for testing only
% - in order to do PRE on loads from the stack, the compiler must
%   do alias analysis to see which loads are killed by stores or
%   are generated by stores. This is NOT done at present and
%   MIGHT NOT be a good idea here. (Though it could be done 
%   separately, possibly?)

suitable_rhs(Instr) ->
  case hipe_icode:type(Instr) of
    mov -> {yes,{mov,hipe_icode:mov_src(Instr)}};
    _ -> no
  end.


