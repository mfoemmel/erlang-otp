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
-module(systools_rc).
-export([translate_scripts/2, translate_scripts/3, format_error/1]).

-import(lists, [map/2, foreach/2, zf/2, filter/2, foldl/3, mapfoldl/3,
		keysearch/3]).

-include("systools.hrl").

%%-----------------------------------------------------------------
%% High-level
%% ==========
%% mnesia_backup
%% {update, Mod, Change, PrePurge, PostPurge, [Mod]}
%% {update, Mod, Timeout, Change, PrePurge, PostPurge, [Mod]}
%% {update, Mod, ModType, , Change, PrePurge, PostPurge, [Mod]}
%% {update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, [Mod]}
%% {load_module, Mod, PrePurge, PostPurge, [Mod]}
%% {add_module, Mod}
%% {restart_application, Appl}
%% {remove_application, Appl}
%% {add_application, Appl}
%%
%% Low-level
%% =========
%% {load_object_code, {Lib, LibVsn, Mods}}
%% point_of_no_return
%% {load, {Mod, PrePurge, PostPurge}}
%% {remove, {Mod, PrePurge, PostPurge}}
%% {purge, Mods}
%% {suspend, Mods}
%% {resume, Mods}
%% {code_change, [{Mod, Extra}]}
%% {code_change, Mode, [{Mod, Extra}]}
%% {stop, Mods}
%% {start, Mods}
%% {sync_nodes, Id, {M, F, A}}
%% {sync_nodes, Id, Nodes}
%% {apply, {M, F, A}}
%% application_remove
%% restart_new_emulator
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Merge of high-level script is better than merge of the low-level
%% variants, because they contain more information.  If two scripts
%% are merged, where one module in one script depends on a module
%% in the other, the information is only available in the high-
%% level variants.  So you may get strange results when merging
%% those as low-level scripts.
%%-----------------------------------------------------------------
translate_scripts(Scripts, Applications) ->
    translate_scripts(up, Scripts, Applications).
%% Mode == up | dn
translate_scripts(Mode, Scripts, Applications) ->
    case catch do_tr(Mode, Scripts, Applications) of
	{ok, NewScript} -> {ok, NewScript};
	{error, Reason} -> {error, ?MODULE, Reason};
	{'EXIT', Reason} -> {error, ?MODULE, Reason}
    end.

do_tr(Mode, Scripts, Applications) ->
    {Before, After} = 
	foldl(fun(Script, {B1, A1}) ->
		      {B2, A2} = tr_point_of_no_return(Script),
		      {B1 ++ B2, A1 ++ A2}
	      end,
	      {[], []},
	      Scripts),
    do_check(Mode, Before ++ [point_of_no_return | After], Applications).


%%-----------------------------------------------------------------
%% All check_ functions performs checks, and throws {error, Reason}
%% (or fails) in case of error.
%% All tr_ translates some instructions.  May throw error or fail
%% as well.
%% The script is split into instructions before and after
%% point_of_no_return.  Before, only load_object_code and apply
%% are allowed.
%%-----------------------------------------------------------------
do_check(Mode, Script, Applications) ->
    check_syntax(Script),
    {Before, After} = tr_point_of_no_return(Script),
    check_load(Before, After),
    check_suspend_resume(After),
    check_start_stop(After),
    {Before1, After1} = tr_dependencies(Mode, Before, After, Applications),
    After2            = tr_applications(After1, Applications),
    {Before3, After3} = tr_add_module(Before1, After2, Applications),
    After4            = tr_mnesia(After3),
    Before4           = merge_load_object_code(Before3),
    NewScript = Before4 ++ [point_of_no_return | After4],
    check_syntax(NewScript),
    {ok, NewScript}.

%%-----------------------------------------------------------------
%% Checks the syntax of all instructions.
%%-----------------------------------------------------------------
check_syntax([H|T]) ->
    check_op(H),
    check_syntax(T);
check_syntax([]) -> ok.

%%-----------------------------------------------------------------
%% Checks that there is max 1 point_of_no_return.  Makes sure
%% that if there was a point_of_no_return, only apply and
%% load_object_code are before the point.
%% Collects all load_object_code before the point_of_no_return.
%%-----------------------------------------------------------------
tr_point_of_no_return(Script) ->
    {Before, After} = split_instructions(Script),
    foreach(fun({load_object_code, _}) -> ok;
	       ({apply, _}) -> ok;
	       (Instruction) ->
		    throw({error, {bad_op_before_point_of_no_return,
				   Instruction}})
	    end, Before),
    {Found, Rest} = split(fun({load_object_code, _}) -> true;
			     (_) -> false
			  end, After),
    {Before ++ Found, Rest}.
	    
%%-----------------------------------------------------------------
%% Checks that each load has a corresponding load_object_code.
%%-----------------------------------------------------------------
check_load(Before, After) ->
    foreach(fun({load, {Mod, _, _}}) ->
		    case find_object_code(Mod, Before) of
			true -> ok;
			false -> throw({error, {no_object_code, Mod}})
		    end;
	       (_) -> ok
	    end, After).

%%-----------------------------------------------------------------
%% Checks that all suspended Mods are resumed, and that none are
%% resumed/code_changed but not suspended.
%%-----------------------------------------------------------------
check_suspend_resume(Script) ->
    Suspended   = map(fun({Mod, _Timeout}) -> Mod;
			 (Mod) -> Mod
		      end,
		      lists:flatten([X || {suspend, X} <- Script])),
    Resumed     = lists:flatten([X || {resume, X} <- Script]),
    CodeChanged = lists:flatten([X || {code_change, _, {X, _}} <- Script]),
    case difference(Suspended, Resumed) of
	[] -> ok;
	S2 -> throw({error, {suspended_not_resumed, S2}})
    end,
    case difference(Resumed, Suspended) of
	[] -> ok;
	R2 -> throw({error, {resumed_not_suspended, R2}})
    end,
    case difference(CodeChanged, Suspended) of
	[] -> ok;
	C2 -> throw({error, {code_change_not_suspended, C2}})
    end.

%%-----------------------------------------------------------------
%% Checks that all stops are started, and that all starts are
%% stopped.
%%-----------------------------------------------------------------
check_start_stop(Script) ->
    Start = lists:flatten([X || {start, X} <- Script]),
    Stop  = lists:flatten([X || {stop, X}  <- Script]),
    case difference(Start, Stop) of
	[] -> ok;
	S2 -> throw({error, {start_not_stop, S2}})
    end,
    case difference(Stop, Start) of
	[] -> ok;
	S3 -> throw({error, {stop_not_start, S3}})
    end.

%%-----------------------------------------------------------------
%% Translates update and load_module.
%%-----------------------------------------------------------------
tr_dependencies(Mode, Before, After, Appls) -> 
    {NBefore, NAfter} = tr_dep(After, Appls, [], [], Mode),
    {Before ++ NBefore, NAfter}.

tr_dep([{update, Mod, Change, PreP, PostP, Mods} | T],
       Appls, Before, After, Mode) ->
    Instrs = do_tr_dep(Mod, [{update, Mod, Change, PreP, PostP, Mods} | T]),
    {B2, A2} = high_to_low(Mode, Instrs, Appls),
    tr_dep(difference(T, Instrs), Appls, Before ++ B2, After ++ A2, Mode);
tr_dep([{update, Mod, Timeout, Change, PreP, PostP, Mods} | T],
       Appls, Before, After, Mode) ->
    Instrs = do_tr_dep(Mod, [{update, Mod, Timeout, Change, PreP, PostP, Mods} |
			     T]),
    {B2, A2} = high_to_low(Mode, Instrs, Appls),
    tr_dep(difference(T, Instrs), Appls, Before ++ B2, After ++ A2, Mode);
tr_dep([{update, Mod, ModType, Timeout, Change, PreP, PostP, Mods} | T],
       Appls, Before, After, Mode) ->
    Instrs = 
	do_tr_dep(Mod,
		  [{update, Mod, ModType, Timeout, Change, PreP, PostP, Mods} |
		   T]),
    {B2, A2} = high_to_low(Mode, Instrs, Appls),
    tr_dep(difference(T, Instrs), Appls, Before ++ B2, After ++ A2, Mode);
tr_dep([{load_module, Mod, PreP, PostP, Mods} | T],
       Appls, Before, After, Mode) ->
    Instrs = do_tr_dep(Mod, [{load_module, Mod, PreP, PostP, Mods} | T]),
    {B2, A2} = high_to_low(Mode, Instrs, Appls),
    tr_dep(difference(T, Instrs), Appls, Before ++ B2, After ++ A2, Mode);
tr_dep([H|T], Appls, Before, After, Mode) ->
    tr_dep(T, Appls, Before, After ++ [H], Mode);  % hmm
tr_dep([], Appls, Before, After, Mode) ->
    {Before, After}.

%%-----------------------------------------------------------------
%% Translates add_application, remove_application  and restart_application
%% into add_module, application_remove and apply.
%%-----------------------------------------------------------------
tr_applications(Script, Applications) ->
%    io:format("Applications ~n~p~n",[Applications]),
    L = map(fun({add_application, Appl}) ->
		    case keysearch(Appl, #application.name, Applications) of
			{value, Application} ->
			    Mods = 
				remove_vsn(Application#application.modules),
			    App = systools_make:pack_app(Application),
			    [{add_module, M} || M <- Mods] ++
				[{apply, {application, load, [App]}},
				 {apply, {application, start, 
					  [Appl, permanent]}}];
			false ->
			    throw({error, {no_such_application, Appl}})
		    end;

	       ({remove_application, Appl}) ->
		    case keysearch(Appl, #application.name, Applications) of
			{value, _Application} ->
			    throw({error, {removed_application_present, 
					   Appl}});
			false ->
			    [{apply, {application, stop, [Appl]}},
			     {application_remove, Appl},
			     {apply, {application, unload, [Appl]}}]
		    end;
	       ({restart_application, Appl}) ->
		    case keysearch(Appl, #application.name, Applications) of
			{value, Application} ->
			    Mods = 
				remove_vsn(Application#application.modules),
			    App = systools_make:pack_app(Application),
			    [{apply, {application, stop, [Appl]}},
			     {application_remove, Appl},
			     {apply, {application, unload, [Appl]}}] ++
				[{add_module, M} || M <- Mods] ++
				[{apply, {application, load, [App]}},
				 {apply, {application, start, 
					  [Appl, permanent]}}];
			false ->
			    throw({error, {no_such_application, Appl}})
		    end;
	       (X) -> X
	    end, Script),
    lists:flatten(L).

remove_vsn(Mods) ->
    remove_vsn(Mods, []).
remove_vsn([], Res) ->
    Res;
remove_vsn([H|Mods], Res) ->
    case H of
	{Mod, Vsn} ->
	    remove_vsn(Mods, [Mod | Res]);
	Mod ->
	    remove_vsn(Mods, [Mod | Res])
    end.


%%-----------------------------------------------------------------
%% Translates add_module into load_object_code and load.
%%-----------------------------------------------------------------
tr_add_module(Before, After, Appls) ->
    % Correct the strange return value from mapfoldl
    {NAfter, NBefore} = 
	mapfoldl(fun({add_module, Mod}, Before2) ->
			 [{Lib, LibVsn}] = get_lib(Mod, Appls),
			 % purge method really doesn't matter - it's a
			 % new module => no old processes.
			 {{load, {Mod, brutal_purge, brutal_purge}},
			  [{load_object_code, {Lib, LibVsn, [Mod]}} | Before2]};
		    (X, Before2) -> {X, Before2}
		 end, Before, After),
    {NBefore, NAfter}.

%%-----------------------------------------------------------------
%% Makes sure there's only one mnesia_backup present.  Translates
%% into apply.  Checks that it's the first op in the script.
%%-----------------------------------------------------------------
tr_mnesia([mnesia_backup | T]) ->
    case lists:member(mnesia_backup, T) of
	true -> throw({error, dup_mnesia_backup});
	false -> [nyi | T]			% XXX
    end;
tr_mnesia(T) ->
    case lists:member(mnesia_backup, T) of
	true -> throw({error, bad_mnesia_backup});
	false -> T
    end.

%%-----------------------------------------------------------------
%% Makes sure there's only one load_object_code per lib.
%% (This is actually just an optimization)
%%-----------------------------------------------------------------
merge_load_object_code(Before) ->
    {Found, Rest} = split(fun({load_object_code, _}) -> true;
			     (_) -> false
			  end, Before),
    mlo(Found) ++ Rest.

mlo([{load_object_code, {Lib, LibVsn, Mods}} | T]) ->
    {Same, Other} = split(fun({load_object_code, {Lib2, LibVsn2, Mods2}})
			       when Lib == Lib2, LibVsn == LibVsn2 -> true;
			     ({load_object_code, {Lib2, LibVsn2, Mods2}})
			       when Lib == Lib2 ->
				  throw({error, {conflicting_versions,
						 Lib, LibVsn, LibVsn2}});
			     (_) -> false
			  end, T),
    OCode = foldl(fun({load_object_code, {_, _, Ms}}, Res) ->
			  union(Ms, Res)
		  end, Mods, Same),
    [{load_object_code, {Lib, LibVsn, OCode}} | mlo(Other)];
mlo([]) -> [].

find_object_code(Mod, [{load_object_code, {_, _, Mods}} | T]) ->
    case lists:member(Mod, Mods) of
	true -> true;
	false -> find_object_code(Mod, T)
    end;
find_object_code(Mod, [_|T]) ->
    find_object_code(Mod, T);
find_object_code(Mod, []) ->
    false.

get_lib(Mod, [#application{name = Name, vsn = Vsn, modules = Modules} | T]) ->
    %% Module = {Mod, Vsn} | Mod
    case keysearch(Mod, 1, Modules) of
	{value, _} ->
	    [{Name, Vsn}];
	false ->
	    case lists:member(Mod, Modules) of
		true -> [{Name, Vsn}];
		false ->   get_lib(Mod, T)
	    end
    end;
get_lib(Mod, []) ->
    throw({error, {no_such_module, Mod}}).

split_instructions(Script) ->
    split_instructions(Script, []).
split_instructions([point_of_no_return | T], Before) ->
    case lists:member(point_of_no_return, T) of
	true -> throw({error, too_many_point_of_no_return});
	false -> {lists:reverse(Before), T}
    end;
split_instructions([H | T], Before) ->
    split_instructions(T, [H | Before]);
split_instructions([], Before) ->
    {[], lists:reverse(Before)}.


check_op(mnesia_backup) -> ok;
check_op({update, Mod, Change, PrePurge, PostPurge, Mods}) ->
    check_mod(Mod),
    check_change(Change),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({update, Mod, Timeout, Change, PrePurge, PostPurge, Mods}) ->
    check_mod(Mod),
    check_timeout(Timeout),
    check_change(Change),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, Mods}) ->
    check_mod(Mod),
    check_mod_type(ModType),
    check_timeout(Timeout),
    check_change(Change),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({load_module, Mod, PrePurge, PostPurge, Mods}) ->
    check_mod(Mod),
    check_purge(PrePurge),
    check_purge(PostPurge),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({add_module, Mod}) ->
    check_mod(Mod);
check_op({remove_application, Appl}) ->
    check_appl(Appl);
check_op({add_application, Appl}) ->
    check_appl(Appl);
check_op({restart_application, Appl}) ->
    check_appl(Appl);
check_op(restart) -> ok;
check_op(reboot) -> ok;
check_op({load_object_code, {Lib, LibVsn, Mods}}) ->
    check_lib(Lib),
    check_lib_vsn(LibVsn),
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op(point_of_no_return) -> ok;
check_op({load, {Mod, PrePurge, PostPurge}}) ->
    check_mod(Mod),
    check_purge(PrePurge),
    check_purge(PostPurge);
check_op({remove, {Mod, PrePurge, PostPurge}}) ->
    check_mod(Mod),
    check_purge(PrePurge),
    check_purge(PostPurge);
check_op({purge, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({suspend, Mods}) ->
    check_list(Mods),
    lists:foreach(fun({M,T}) -> check_mod(M), check_timeout(T);
		     (M) -> check_mod(M)
		  end, Mods);
check_op({resume, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({code_change, Mods}) ->
    check_list(Mods),
    lists:foreach(fun({M, Extra}) -> check_mod(M);
		     (X) -> throw({error, {bad_code_change, X}})
		  end, Mods);
check_op({code_change, Mode, Mods}) ->
    check_list(Mods),
    check_mode(Mode),
    lists:foreach(fun({M, Extra}) -> check_mod(M);
		     (X) -> throw({error, {bad_code_change, X}})
		  end, Mods);
check_op({stop, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({start, Mods}) ->
    check_list(Mods),
    lists:foreach(fun(M) -> check_mod(M) end, Mods);
check_op({sync_nodes, Id, {M, F, A}}) ->
    check_mod(M),
    check_func(F),
    check_args(A);
check_op({sync_nodes, Id, Nodes}) ->
    check_list(Nodes),
    lists:foreach(fun(Node) -> check_node(Node) end, Nodes);
check_op({apply, {M, F, A}}) ->
    check_mod(M),
    check_func(F),
    check_args(A);
check_op(restart_new_emulator) -> ok;
check_op({application_remove,Appl}) -> ok;
check_op(X) -> throw({error, {bad_instruction, X}}).

check_mod(Mod) when atom(Mod) -> ok;
check_mod(Mod) -> throw({error, {bad_module, Mod}}).

check_change(soft) -> ok;
check_change({advanced, _}) -> ok;
check_change(Change) -> throw({error, {bad_change, Change}}).

check_mod_type(static) -> ok;
check_mod_type(dynamic) -> ok;
check_mod_type(ModType) -> throw({error, {bad_mod_type, ModType}}).

check_purge(soft_purge) -> ok;
check_purge(brutal_purge) -> ok;
check_purge(Purge) -> throw({error, {bad_purge_method, Purge}}).

check_list(List) when list(List) -> ok;
check_list(List) -> throw({error, {bad_list, List}}).

check_args(Args) when list(Args) -> ok;
check_args(Args) -> throw({error, {bad_args_list, Args}}).

check_node(Node) when atom(Node) -> ok;
check_node(Node) -> throw({error, {bad_node, Node}}).

check_appl(Appl) when atom(Appl) -> ok;
check_appl(Appl) -> throw({error, {bad_application, Appl}}).

check_func(Func) when atom(Func) -> ok;
check_func(Func) -> throw({error, {bad_func, Func}}).

check_lib(Lib) when atom(Lib) -> ok;
check_lib(Lib) -> throw({error, {bad_lib, Lib}}).

check_lib_vsn(LibVsn) when list(LibVsn) -> ok;
check_lib_vsn(LibVsn) -> throw({error, {bad_lib_vsn, LibVsn}}).

check_timeout(default) -> ok;
check_timeout(infinity) -> ok;
check_timeout(Int) when integer(Int), Int > 0 -> ok;
check_timeout(T) -> throw({error, {bad_timeout, T}}).

check_mode(up) -> ok;
check_mode(down) -> ok;
check_mode(Mode) -> throw({error, {bad_mode, Mode}}).

split(Fun, [H | T]) ->
    {Found, Rest} = split(Fun, T),
    case Fun(H) of
	true -> {[H | Found], Rest};
	false -> {Found, [H | Rest]}
    end;
split(Fun, []) ->
    {[], []}.

union([H|T], L) ->
    case lists:member(H, L) of
	true -> union(T,L);
	false -> [H | union(T, L)]
    end;
union([], L) -> L.

difference([H | T], L) ->
    case lists:member(H, L) of
	true -> difference(T, L);
	false -> [H | difference(T, L)]
    end;
difference([], _) -> [].
			 


do_tr_dep(Mod, Instructions) ->
    G = digraph:new(), 
    case catch do_tr_dep(G, Mod, Instructions) of
	{'EXIT', Reason} ->
	    digraph:delete(G),
	    exit(Reason);
	{error, Reason} ->
	    digraph:delete(G),
	    throw({error, Reason});
	Order ->
	    digraph:delete(G),
	    Order
    end.

do_tr_dep(G, Mod, Instructions) ->
    build_graph(G, [Mod], order_instructions(Instructions)),
    lists:reverse(make_order(G, [])).

%%-----------------------------------------------------------------
%% Func: build_graph(G, UnHandled, Instructions)
%% Alg: For each Mod in UnHandled, add it to the graph, add all
%%      modules that it depends on, and add directed edges between
%%      them.  Also, find all mods that are dependent on Mod, and
%%      add these edges as well.   For each such module that wasn't
%%      added already, put it in the UnHandled list in the recursive
%%      call.
%%-----------------------------------------------------------------
build_graph(G, [Mod | T], Instructions) ->
    ensure_add_vertex(G, Mod, Instructions),
    % Find all mods that depend on Mod.
    Other = find_dependent_mods(Mod, Instructions),
    UnHandled = ensure_add_vertices(G, Other, Instructions, T),
    add_edges(G, Other, Mod),
    % Find all mods that Mod depends on.
    Mods = get_mods(G, Mod),
    UnHandled2 = ensure_add_vertices(G, Mods, Instructions, UnHandled),
    add_edges(G, Mod, Mods),
    build_graph(G, UnHandled2, Instructions);
build_graph(G, [], _Instructions) ->
    G.

%%-----------------------------------------------------------------
%% Observation: If there exists an edge from mod A to mod B,
%%              we must first suspend/load B, and then A.
%% Alg: As long as there exist nodes with outdegree 0, we pick them
%%      and suspend/load them, and delete them from the graph.  If
%%      the graph is empty, we're done.  If there still are nodes
%%      left, but none with outdegree 0, there's a cycle.  In this
%%      case, we want to suspend/load modules in the same order as
%%      they were originally written.
%%-----------------------------------------------------------------
make_order(G, Order) ->
    case exists_outdegree_zero(G) of
	{true, V} ->
	    NewOrder = [op(G, V) | Order],
	    digraph:del_vertex(G, V),
	    make_order(G, NewOrder);
	false ->
	    find_cycle(G, Order)
    end.

find_cycle(G, Order) ->
    case digraph:vertices(G) of
	[] -> Order;
	[Vertex | _] ->
	    Vertex2 = find_node_in_sink_cycle(G, Vertex),
	    [_ | Cycle] = digraph:get_cycle(G, Vertex2),
	    NewOrder = original_ops(G, Cycle),
	    digraph:del_vertices(G, Cycle),
	    make_order(G, NewOrder ++ Order)
    end.

%%-----------------------------------------------------------------
%% Suppose:
%%        A --> B
%%        B --> A
%%        B --> C
%%        C --> D
%%        D --> C
%%
%% In this case there are two cycles (A,B) and (C,D).  This all
%% means that A and B are dependent on C and D.  This function
%% finds the 'sink cycle' (C,D).
%%-----------------------------------------------------------------
find_node_in_sink_cycle(G, Vertex) ->
    G2 = copy_graph(G), % What is this?  I don't want to care about this shit!
    Node = fnisc(G2, Vertex),
    digraph:delete(G2),
    Node.

fnisc(G, V) ->
    case digraph:out_edges(G, V) of
	[] -> V;
	[E | _] ->
	    {_E, _, EndVertex, _} = digraph:edge(G, E),
	    digraph:del_edge(G, E),
	    fnisc(G, EndVertex)
    end.

ensure_add_vertex(G, Mod, Instructions) ->
    case digraph:vertex(G, Mod) of
	false ->
	    Instr = get_instr(Mod, Instructions),
	    digraph:add_vertex(G, Mod, Instr),
	    true;
	_ -> false
    end.

get_instr(Mod, Instrs) ->
    case filter(fun({_Op, Mod2, _Mods, _N}) when Mod == Mod2 -> true;
		   (_) -> false
		end,
		Instrs) of
	[Instruction] -> Instruction;
	[] -> throw({error, {undef_module, Mod}});
	_ -> throw({error, {muldef_module, Mod}})
    end.

ensure_add_vertices(G, [Mod | Mods], Instructions, UnHandled) ->
    case ensure_add_vertex(G, Mod, Instructions) of
	true ->
	    ensure_add_vertices(G, Mods, Instructions, [Mod|UnHandled]);
	false ->
	    ensure_add_vertices(G, Mods, Instructions, UnHandled)
    end;
ensure_add_vertices(_G, [], _Instructions, Res) ->
    Res.

add_edges(G, Mod, Mods) when list(Mods) ->
    foreach(fun(EndVertex) ->
		    digraph:add_edge(G, Mod, EndVertex)
	    end,
	    difference(Mods, digraph:out_neighbours(G, Mod)));
add_edges(G, Mods, Mod) when list(Mods) ->
    foreach(fun(StartVertex) ->
		    digraph:add_edge(G, StartVertex, Mod)
	    end,
	    difference(Mods, digraph:in_neighbours(G, Mod))).

get_mods(G, Mod) ->
    {_V, {_Op, _Mod, Mods, _N}} = digraph:vertex(G, Mod),
    Mods.

find_dependent_mods(Mod, Instructions) ->
    [Mod2 || {_Op, Mod2, Mods, _N} <- Instructions,
	     lists:member(Mod, Mods)].

exists_outdegree_zero(G) ->
    eoz(G, digraph:vertices(G)).

eoz(G, [V | Vs]) ->
    case digraph:out_degree(G, V) of
	0 -> {true, V};
	_ -> eoz(G, Vs)
    end;
eoz(G, []) -> false.

op(G, Mod) ->
    {V, {Op, _Mod, Mods, N}} = digraph:vertex(G, Mod),
    Op.

original_ops(G, Cycle) ->
    Ops = map(fun(Mod) -> {V, D} = digraph:vertex(G, Mod), D end, Cycle),
    SortedOps = lists:keysort(4, Ops),
    ops(SortedOps, []).

ops([{Op, _, _, _} | T], Res) -> ops(T, [Op | Res]);
ops([], Res) -> Res.

copy_graph(G) ->
    G2 = digraph:new(),
    foreach(fun(V) ->
		    {_, Data} = digraph:vertex(G, V),
		    digraph:add_vertex(G2, V, Data)
	    end,
	    digraph:vertices(G)),
    foreach(fun(E) ->
		    {_E, V1, V2, D} = digraph:edge(G, E),
		    digraph:add_edge(G2, V1, V2, D)
	    end,
	    digraph:edges(G)),
    G2.

order_instructions(Instructions) ->
    Ops = filter(fun(Op) when tuple(Op), element(1, Op) == update -> true;
		    (Op) when tuple(Op), element(1, Op) == load_module -> true;
		    (_) -> false
		 end,
		 Instructions),
    {L, _} =
	mapfoldl(fun({update, Mod, Change, PreP, PostP, Mods}, N) ->
			 {{{update, Mod, Change, PreP, PostP, Mods},
			   Mod, Mods, N}, N+1};
		    ({update, Mod, Timeout, Change, PreP, PostP, Mods}, N) ->
			 {{{update, Mod, Timeout, Change, PreP, PostP, Mods},
			   Mod, Mods, N}, N+1};
		    ({update, Mod, MType, Tout, Chnge, PreP, PostP, Mods}, N) ->
			 {{{update, Mod, MType, Tout, Chnge, PreP, PostP, Mods},
			   Mod, Mods, N}, N+1};
		    ({load_module, Mod, PreP, PostP, Mods}, N) ->
			 {{{load_module, Mod, PreP, PostP, Mods},
			   Mod, Mods, N}, N+1}
		 end,
		 1,
		 Ops),
    L.

high_to_low(Mode, Instructions, Appls) ->
    Suspend = [Mod || {update, Mod, _, _, _, _} <- Instructions] ++
	      [{Mod, T} || {update, Mod, T, _, _, _, _} <- Instructions] ++
	      [{Mod, T} || {update, Mod, _, T, _, _, _, _} <- Instructions],
    Resume = [Mod || {update, Mod, _, _, _, _} <- Instructions] ++
	     [Mod || {update, Mod, _, _, _, _, _} <- Instructions] ++
   	     [Mod || {update, Mod, _, _, _, _, _, _} <- Instructions],
    Load = zf(fun({update, Mod, _, PreP, PostP, _}) ->
		      {true, {load, {Mod, PreP, PostP}}};
		 ({update, Mod, _, _, PreP, PostP, _}) ->
		      {true, {load, {Mod, PreP, PostP}}};
		 ({update, Mod, _, _, _, PreP, PostP, _}) ->
		      {true, {load, {Mod, PreP, PostP}}};
		 ({load_module, Mod, PreP, PostP, _}) ->
		      {true, {load, {Mod, PreP, PostP}}};
		 (_) -> false
	      end,
	      Instructions),
    LoadObjCode = [{load_object_code, {Lib, LibVsn, [Mod]}} ||
		      {load_module, Mod, _, _, _} <- Instructions,
		      {Lib, LibVsn} <- get_lib(Mod, Appls)] ++
	          [{load_object_code, {Lib, LibVsn, [Mod]}} ||
		      {update, Mod, _, _, _, _} <- Instructions,
		      {Lib, LibVsn} <- get_lib(Mod, Appls)] ++
	          [{load_object_code, {Lib, LibVsn, [Mod]}} ||
		      {update, Mod, _, _, _, _, _} <- Instructions,
		      {Lib, LibVsn} <- get_lib(Mod, Appls)] ++
	          [{load_object_code, {Lib, LibVsn, [Mod]}} ||
		      {update, Mod, _, _, _, _, _, _} <- Instructions,
		      {Lib, LibVsn} <- get_lib(Mod, Appls)],
    %% PreCodeC are all module that should be code_changed *before*
    %% the code is loaded when downgrading.
    PreCodeC =
	[{Mod, Extra} || 
	    {update, Mod, {advanced, Extra}, _, _, _}
		<- Instructions] ++
	[{Mod, Extra} || 
	    {update, Mod, _, {advanced, Extra}, _, _, _}
		<- Instructions] ++
	[{Mod, Extra} || 
	    {update, Mod, dynamic, _, {advanced, Extra}, _, _, _}
		<- Instructions],
    %% PostCodeC are all module that should be code_changed *after*
    %% the code is loaded when downgrading.
    PostCodeC =
	[{Mod, Extra} || 
	    {update, Mod, static, _, {advanced, Extra}, _, _, _} 
		<- Instructions],
    PreCodeCInst = 
	if
	    PreCodeC == [] -> [];
	    Mode == up -> [{code_change, up, PreCodeC}];
	    true -> [{code_change, down, PreCodeC}]
	end,
    PostCodeCInst = 
	if
	    PostCodeC == [] -> [];
	    Mode == up -> [{code_change, up, PostCodeC}];
	    true -> [{code_change, down, PostCodeC}]
	end,
    {LoadObjCode, [{suspend, Suspend} |
		   if
		       Mode == up ->
			   Load ++ PreCodeCInst ++ PostCodeCInst;
		       Mode == dn ->
			   PreCodeCInst ++ Load ++ PostCodeCInst
		   end ++
		   [{resume, Resume}]
		  ]}.

%%-----------------------------------------------------------------
%% Format error
%%-----------------------------------------------------------------
format_error({bad_op_before_point_of_no_return, Instruction}) ->
    io_lib:format("Bad instruction ~p~nbefore point_of_no_return~n",
		  [Instruction]);
format_error({no_object_code, Mod}) ->
    io_lib:format("No load_object_code found for module: ~p~n", [Mod]);
format_error({suspended_not_resumed, Mods}) ->
    io_lib:format("Suspended but not resumed: ~p~n", [Mods]);
format_error({resumed_not_suspended, Mods}) ->
    io_lib:format("Resumed but not suspended: ~p~n", [Mods]);
format_error({code_change_not_suspended, Mods}) ->
    io_lib:format("Code changed but not suspended: ~p~n", [Mods]);
format_error({start_not_stop, Mods}) ->
    io_lib:format("Started but not stopped: ~p~n", [Mods]);
format_error({stop_not_start, Mods}) ->
    io_lib:format("Stopped but not started: ~p~n", [Mods]);
format_error({no_such_application, App}) ->
    io_lib:format("Started undefined application: ~p~n", [App]);
format_error({removed_application_present, App}) ->
    io_lib:format("Removed application present: ~p~n", [App]);
format_error(dup_mnesia_backup) ->
    io_lib:format("Duplicate mnesia_backup~n", []);
format_error(bad_mnesia_backup) ->
    io_lib:format("mnesia_backup in bad position~n", []);
format_error({conflicting_versions, Lib, V1, V2}) ->
    io_lib:format("Conflicting versions for ~p, ~p and ~p~n", [Lib, V1, V2]);
format_error({no_appl_vsn, Appl}) ->
    io_lib:format("No version specified for application: ~p~n", [Appl]);
format_error({no_such_module, Mod}) ->
    io_lib:format("No such module: ~p~n", [Mod]);
format_error(too_many_point_of_no_return) ->
    io_lib:format("Too many point_of_no_return~n", []);

format_error({bad_instruction, X}) ->
    io_lib:format("Bad instruction: ~p~n", [X]);
format_error({bad_module, X}) ->
    io_lib:format("Bad module: ~p(should be atom())~n", [X]);
format_error({bad_code_change, X}) ->
    io_lib:format("Bad code_change: ~p(should be {Mod, Extra})~n", [X]);
format_error({bad_change, X}) ->
    io_lib:format("Bad change spec: ~p(should be soft | {advanced, E})~n", [X]);
format_error({bad_mod_type, X}) ->
    io_lib:format("Bad module type: ~p(should be static | dynamic)~n", [X]);
format_error({bad_purge_method, X}) ->
    io_lib:format("Bad purge method: ~p(should be soft_purge | brutal_purge)~n",
		  [X]);
format_error({bad_list, X}) ->
    io_lib:format("Bad list: ~p~n", [X]);
format_error({bad_args_list, X}) ->
    io_lib:format("Bad argument list: ~p~n", [X]);
format_error({bad_node, X}) ->
    io_lib:format("Bad node: ~p(should be atom())~n", [X]);
format_error({bad_application, X}) ->
    io_lib:format("Bad application: ~p(should be atom())~n", [X]);
format_error({bad_func, X}) ->
    io_lib:format("Bad function: ~p(should be atom())~n", [X]);
format_error({bad_lib, X}) ->
    io_lib:format("Bad library: ~p(should be atom())~n", [X]);
format_error({bad_lib_vsn, X}) ->
    io_lib:format("Bad library version: ~p(should be string())~n", [X]);
format_error({bad_timeout, X}) ->
    io_lib:format("Bad timeout: ~p(should be infinity | int() > 0)~n", [X]);

format_error({undef_module, Mod}) ->
    io_lib:format("Undefined module: ~p~n", [Mod]);
format_error({muldef_module, Mod}) ->
    io_lib:format("Multiply defined module: ~p~n", [Mod]);
format_error(E) ->
    io_lib:format("~p~n",[E]).
