%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Purpose : Load a new version of a module to be
%%           interpreted.

-module(dbg_iload).

%% ------------------------------------------------
%% -- Exported functions.
%% ------------------------------------------------

-export([load_mod/3,load_mod1/3]).

%%% =================================================
%%% Load a new module into the database.
%%%
%%% We want the loading of a module to be syncronous
%%% so no other process tries to interpret code in
%%% a module not being completely loaded. This is
%%% achieved as this function is called from the
%%% 'interpret' server.
%%% We are suspended until the module has been loaded.
%%% =================================================

load_mod(Mod,File,Binary) ->
    Flag = process_flag(trap_exit,true),
    Pid = spawn_link(?MODULE,load_mod1,[Mod,File,Binary]),
    receive
	{'EXIT',Pid,What} ->
	    process_flag(trap_exit,Flag),
	    What
    end.

load_mod1(Mod,File,Binary) ->
    store_module(Mod,File,Binary),
    int:int_module(Mod),
    exit({ok,Mod}).

store_module(Mod, File, Binary) ->
    {interpreter_module,Exp,Defs,Forms,Src} = binary_to_term(Binary),
    Db = dbg_idb:cr_new_module(Mod),
    dbg_idb:insert(Db, mod_file, File),
    dbg_idb:insert(Db, exports, Exp),
    dbg_idb:insert(Db, defs, Defs),

    put(fun_count, 0),
    put(funs, []),
    Attr = store_forms(Forms, Mod, Db, Exp, []),
    erase(current_function),
    store_funs(Db, Mod),
    erase(funs),
    erase(fun_count),
    
    dbg_idb:insert(Db, attributes, Attr),
    NewBinary = store_mod_line_no(Mod, Db, binary_to_list(Src)),
    dbg_idb:insert(Db, mod_bin, NewBinary),
    dbg_idb:insert(Db, module, Mod).

store_funs(Db, Mod) ->
    store_funs(get(funs), Db, Mod).

store_funs([{Name,I,Uniq,Arity,Cs}|Fs], Db, Mod) ->
    dbg_idb:insert(Db, {'fun',Mod,I,Uniq}, {Name,Arity,Cs}),
    store_funs(Fs, Db, Mod);
store_funs([], Db, Mod) -> ok.

store_forms([{function,Line,module_info,0,Cs0}|Fs], Mod, Db, Exp, Attr) ->
    Cs = [{clause,0,[],[], [{module_info_0,0,Mod}]}],
    dbg_idb:insert(Db, {Mod,module_info,0,true}, Cs),
    store_forms(Fs, Mod, Db, Exp, Attr);
store_forms([{function,Line,module_info,1,Cs0}|Fs], Mod, Db, Exp, Attr) ->
    Cs = [{clause,0,[{var,0,'What'}],[], [{module_info_1,0,Mod,[{var,0,'What'}]}]}],
    dbg_idb:insert(Db, {Mod,module_info,1,true}, Cs),
    store_forms(Fs, Mod, Db, Exp, Attr);
store_forms([{function,Line,Name,Arity,Cs0}|Fs], Mod, Db, Exp, Attr) ->
    FA = {Name,Arity},
    put(current_function, FA),
    Cs = clauses(Cs0),
    Exported = lists:member(FA, Exp),
    dbg_idb:insert(Db, {Mod,Name,Arity,Exported}, Cs),
    store_forms(Fs, Mod, Db, Exp, Attr);
store_forms([{attribute,Line,Name,Val}|Fs], Mod, Db, Exp, Attr) ->
    store_forms(Fs, Mod, Db, Exp, [{Name,Val}|Attr]);
store_forms([F|Fs], Mod, Db, Exp, Attr) ->
    io:format("~p: Unrecognized form ~P when loading\n", [Mod,F,12]),
    exit({unknown_form,F});
store_forms([], _, _, Exp, Attr) ->
    lists:reverse(Attr).

store_mod_line_no(Mod, Db, Contents) ->
    store_mod_line_no(Mod, Db, Contents, 1, 0, []).

store_mod_line_no(Mod, Db, [], LineNo, Pos, NewCont) ->
    list_to_binary(lists:reverse(NewCont));
store_mod_line_no(Mod, Db, Contents, LineNo, Pos, NewCont) when integer(LineNo) ->
    {ContTail,Pos1,NewCont1} = store_line(Mod, Db, Contents, LineNo, Pos, NewCont),
    store_mod_line_no(Mod, Db, ContTail, LineNo+1, Pos1, NewCont1).

store_line(Mod, Db, Contents, LineNo, Pos, NewCont) ->
    {ContHead,ContTail,PosNL} = get_nl(Contents,Pos+8,[]),
    dbg_idb:insert(Db,LineNo,{Pos+8,PosNL}),
    {ContTail,PosNL+1,[make_lineno(LineNo, 8, ContHead)|NewCont]}.

make_lineno(N, P, Acc) ->
    S = integer_to_list(N),
    S ++ [$:|spaces(P-length(S)-1, Acc)].

spaces(P, Acc) when P > 0 ->
    spaces(P-1, [$\s|Acc]);
spaces(_, Acc) -> Acc.

get_nl([10|T],Pos,Head) -> {lists:reverse([10|Head]),T,Pos};
get_nl([H|T],Pos,Head) ->
    get_nl(T,Pos+1,[H|Head]);
get_nl([],Pos,Head) -> {lists:reverse(Head),[],Pos}.


%%% Rewrite the abstract syntax tree to that it will be easier (== faster)
%%% to interpret.

clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].

clause({clause,Line,H0,G0,B0}) ->
    H1 = head(H0),
    G1 = guard(G0),
    B1 = exprs(B0),
    {clause,Line,H1,G1,B1}.

head(Ps) -> patterns(Ps).

%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|patterns(Ps)];
patterns([]) -> [].

%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}) -> {var,Line,V};
pattern({integer,Line,I}) -> {value,Line,I};
pattern({match,Line,Pat1,Pat2}) ->
    {match,Line,pattern(Pat1),pattern(Pat2)};
pattern({float,Line,F}) -> {value,Line,F};
pattern({atom,Line,A}) -> {value,Line,A};
pattern({string,Line,S}) -> {value,Line,S};
pattern({nil,Line}) -> {value,Line,[]};
pattern({cons,Line,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,Line,Ps1};
pattern({op,Line1,'-',{integer,Line,I}}) ->
    {value,Line,-I};
pattern({op,Line1,'+',{integer,Line,I}}) ->
    {value,Line,I};
pattern({op,Line1,'-',{float,Line,I}}) ->
    {value,Line,-I};
pattern({op,Line1,'+',{float,Line,I}}) ->
    {value,Line,I};
%% BITS:
pattern({bin,Line,Grp}) ->
    Grp1 = pattern_list(Grp),
    {bin,Line,Grp1};
pattern({bin_element,Line,Expr,Size,Type}) ->
    Expr1 = pattern(Expr),
    Size1 = expr(Size),
    {bin_element,Line,Expr1,Size1,Type};
pattern({bin_tail,Line,{var,Ln,V}}) ->
    {bin_tail,Line,{var,Ln,V}}.

%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|pattern_list(Ps)];
pattern_list([]) -> [].

guard([G0|Gs]) ->
    G1 = and_guard(G0),
    [G1|guard(Gs)];
guard([]) -> [].

and_guard([G0|Gs]) ->
    G1 = guard_test(G0),
    [G1|and_guard(Gs)];
and_guard([]) -> [].

%%  All function calls here must be type tests and only comparison
%%  operators are allowed here!

guard_test({call,Line,{remote,Lr,{atom,Lm,erlang},{atom,Lf,F}},As0}) ->
    As = gexpr_list(As0),
    case map_guard_bif(F, length(As0)) of
	{ok,Name} ->
	    {safe_bif,Line,erlang,Name,As};
	false ->
	    {safe_bif,Line,erlang,F,As};
	Other ->
	    exit({?LINE,Other})
    end;
guard_test({op,Line,Op,L0,R0}) ->
    case erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0),
	    R1 = gexpr(R0),			%They see the same variables
	    {safe_bif,Line,erlang,Op,[L1,R1]};
	Other ->
	    exit({?LINE,Other})
    end.

map_guard_bif(integer, 1) -> {ok,is_integer};
map_guard_bif(float, 1) -> {ok,is_float};
map_guard_bif(number, 1) -> {ok,is_number};
map_guard_bif(atom, 1) -> {ok,is_atom};
map_guard_bif(constant, 1) -> {ok,is_constant};
map_guard_bif(list, 1) -> {ok,is_list};
map_guard_bif(tuple, 1) -> {ok,is_tuple};
map_guard_bif(pid, 1) -> {ok,is_pid};
map_guard_bif(reference, 1) -> {ok,is_reference};
map_guard_bif(port, 1) -> {ok,is_port};
map_guard_bif(binary, 1) -> {ok,is_binary};
map_guard_bif(function, 1) -> {ok,is_function};
map_guard_bif(_, _) -> false.

gexpr({var,Line,V}) -> {var,Line,V};
gexpr({integer,Line,I}) -> {value,Line,I};
gexpr({float,Line,F}) -> {value,Line,F};
gexpr({atom,Line,A}) -> {value,Line,A};
gexpr({string,Line,S}) -> {value,Line,S};
gexpr({nil,Line}) -> {value,Line,[]};
gexpr({cons,Line,H0,T0}) ->
    case {gexpr(H0),gexpr(T0)} of
	{{value,Line,H1},{value,Line,T1}} -> {value,Line,[H1|T1]};
	{H1,T1} -> {cons,Line,H1,T1}
    end;
gexpr({tuple,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,Line,Es1};
%%% The previous passes have added the module name 'erlang' to
%%% all BIF calls, even in guards.
gexpr({call,Line,{remote,L1,{atom,L2,erlang},{atom,La,self}},[]}) ->
    self;
gexpr({call,Line,{remote,L1,{atom,L2,erlang},{atom,La,F}},As0}) ->
    As = gexpr_list(As0),
    {safe_bif,Line,erlang,F,As};
gexpr({op,Line,Op,A0}) ->
    case erl_internal:arith_op(Op, 1) of
	true ->
	    A1 = gexpr(A0),
	    {safe_bif,Line,erlang,Op,[A1]}
    end;
gexpr({op,Line,Op,L0,R0}) ->
    case erl_internal:arith_op(Op, 2) of
	true ->
	    L1 = gexpr(L0),
	    R1 = gexpr(R0),			%They see the same variables
	    {safe_bif,Line,erlang,Op,[L1,R1]}
    end.

%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

expr({var,Line,V}) -> {var,Line,V};
expr({integer,Line,I}) -> {value,Line,I};
expr({float,Line,F}) -> {value,Line,F};
expr({atom,Line,A}) -> {value,Line,A};
expr({string,Line,S}) -> {value,Line,S};
expr({nil,Line}) -> {value,Line,[]};
expr({cons,Line,H0,T0}) ->
    case {expr(H0),expr(T0)} of
	{{value,Line,H1},{value,Line,T1}} -> {value,Line,[H1|T1]};
	{H1,T1} -> {cons,Line,H1,T1}
    end;
expr({tuple,Line,Es0}) ->
    Es1 = expr_list(Es0),
    {tuple,Line,Es1};
expr({block,Line,Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block,Line,Es1};
expr({'if',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'fun',Line,{clauses,Cs0},{Uniq,Hvss,Free}}) ->
    I = get(fun_count),
    put(fun_count, I+1),
    {F,A} = get(current_function),
    Name = new_fun_name(I, F, A),
    Cs = fun_clauses(Cs0, Hvss, Free),
    [{clause,_,H,G,B}|_] = Cs,
    Arity = length(H),
    put(funs, [{Name,I,Uniq,Arity,Cs}|get(funs)]),
    {make_fun,Line,I,Uniq,Free};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,self}},[]}) ->
    self;
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,throw}},As0}) when length(As0) == 1 ->
    [As] = expr_list(As0),
    {throw,Line,As};
expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,apply}},As0}) when length(As0) == 3 ->
    As = expr_list(As0),
    {apply,Line,As};
expr({call,Line,{remote,_,{atom,_,Mod},{atom,_,Func}},As0}) ->
    As = expr_list(As0),
    case erlang:is_builtin(Mod, Func, length(As)) of
	false ->
	    {call_remote,Line,Mod,Func,As};
	true ->
	    case bif_type(Mod, Func) of
		safe -> {safe_bif,Line,Mod,Func,As};
		spawn -> {spawn_bif,Line,Mod,Func,As};
		unsafe ->{bif,Line,Mod,Func,As}
	    end
    end;
expr({call,Line,{atom,_,Func},As0}) ->
    As = expr_list(As0),
    {local_call,Line,Func,As};
expr({call,Line,Fun0,As0}) ->
    Fun = expr(Fun0),
    As = expr_list(As0),
    {apply_fun,Line,Fun,As};
expr({'catch',Line,E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch',Line,E1};
expr({'query', Line, E0}) ->
    %% lc expression
    E = expr(E0),
    {'query', Line, E};
expr({match,Line,P0,E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match,Line,P1,E1};
expr({op,Line,Op,A0}) ->
    A1 = expr(A0),
    {op,Line,Op,[A1]};
expr({op,Line,'++',L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,append,[L1,R1]};
expr({op,Line,'--',L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,subtract,[L1,R1]};
expr({op,Line,'!',L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {send,Line,L1,R1};
expr({op,Line,Op,L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,Op,[L1,R1]};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0}) ->
    M1 = expr(M0),
    F1 = expr(F0),
    {remote,Line,M1,F1};
%% BITS:
expr({bin,Line,Grp}) ->
    Grp1 = expr_list(Grp),
    {bin,Line,Grp1};
expr({bin_element,Line,Expr,Size,Type}) ->
    Expr1 = expr(Expr),
    Size1 = expr(Size),
    {bin_element,Line,Expr1,Size1,Type};
expr({bin_tail,Line,V}) ->
    V1 = expr(V),
    {bin_tail,Line,V1};
expr(Other) ->
    exit({?MODULE,{unknown_expr,Other}}).

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0),
    [E1|expr_list(Es)];
expr_list([]) -> [].

icr_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|icr_clauses(Cs)];
icr_clauses([]) -> [].

fun_clauses([{clause,L,H,G,B}|Cs], [Hvs|Hvss], Free) ->
    [{clause,L,head(H++free_vars(Free, Hvs, L)),guard(G),exprs(B)}|
     fun_clauses(Cs, Hvss, Free)];
fun_clauses([], [], Free) -> [].

free_vars(Vs, Hvs, Line) ->
    [ case lists:member(V, Hvs) of
	  true -> {var,Line,'_'};
	  false -> {var,Line,V}
      end || V <- Vs ].

new_fun_name(I, F, A) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)
	++ "-fun-" ++ integer_to_list(I) ++ "-",
    list_to_atom(Name).

bif_type(erlang, Name) -> bif_type(Name);
bif_type(_, _) -> unsafe.

bif_type(register)           -> safe;
bif_type(unregister)         -> safe;
bif_type(whereis)            -> safe;
bif_type(registered)         -> safe;
bif_type(abs)                -> safe;
bif_type(float)              -> safe;
bif_type(trunc)              -> safe;
bif_type(round)              -> safe;
bif_type(math)               -> safe;
bif_type(node)               -> safe;
bif_type(length)             -> safe;
bif_type(hd)                 -> safe;
bif_type(tl)                 -> safe;
bif_type(size)               -> safe;
bif_type(element)            -> safe;
bif_type(setelement)         -> safe;
bif_type(atom_to_list)       -> safe;
bif_type(list_to_atom)       -> safe;
bif_type(integer_to_list)    -> safe;
bif_type(list_to_integer)    -> safe;
bif_type(float_to_list)      -> safe;
bif_type(list_to_float)      -> safe;
bif_type(tuple_to_list)      -> safe;
bif_type(list_to_tuple)      -> safe;
bif_type(make_ref)           -> safe;
bif_type(time)               -> safe;
bif_type(date)               -> safe;
bif_type(processes)          -> safe;
bif_type(process_info)       -> safe;
bif_type(load_module)        -> safe;
bif_type(delete_module)      -> safe;
bif_type(halt)               -> safe;
bif_type(check_process_code) -> safe;
bif_type(purge_module)       -> safe;
bif_type(pid_to_list)        -> safe;
bif_type(list_to_pid)        -> safe;
bif_type(module_loaded)      -> safe;
bif_type(binary_to_term)     -> safe;
bif_type(term_to_binary)     -> safe;
bif_type(alive)              -> safe;
bif_type(notalive)           -> safe;
bif_type(nodes)              -> safe;
bif_type(is_alive)           -> safe;
bif_type(disconnect_node)    -> safe;
bif_type(binary_to_list)     -> safe;
bif_type(list_to_binary)     -> safe;
bif_type(split_binary)       -> safe;
bif_type(concat_binary)      -> safe;
bif_type(term_to_atom)       -> safe;
bif_type(hash)               -> safe;
bif_type(pre_loaded)         -> safe;
bif_type(info)               -> safe;
bif_type(set_cookie)         -> safe;
bif_type(get_cookie)         -> safe;
bif_type(spawn)              -> spawn;
bif_type(spawn_link)         -> spawn;
bif_type(spawn_opt)          -> spawn;
bif_type(_)                  -> unsafe.
