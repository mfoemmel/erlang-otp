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
%%
%% Contains functions that perform various types of
%% analysis on a cross reference graph.

-module(exref_analysis).

-export([do_analyse/3]).

-include("exref_state.hrl").




do_analyse(call, [], S) ->
    %% format("# CALL GRAPH FOR FUNCTIONS IN MODULES ~w~n", [S#state.modules]),
    Vs0 = digraph:vertices(S#state.fgraph),
    Vs1 = keysplit(1, Vs0),
    {call, S#state.modules, calla2(Vs1, S)};

do_analyse(call, {M,F,A}, S) ->
    %% format("# CALL GRAPH FOR FUNCTION ~w:~w/~w~n", [M, F, A]),
    {call, {M,F,A}, calla1([{M,F,A}], S)};

do_analyse(call, M, S) when atom(M) ->
    %% format("# CALL GRAPH FOR FUNCTIONS IN MODULE ~w~n", [M]),
    Vs0 = digraph:vertices(S#state.fgraph),
    Vs1 = keysublist(M, 1, Vs0),
    Vs2 = lists:keysort(1, Vs1),
    {call, [M], calla1(Vs2, S)};

do_analyse(use, [], S) ->
    %% format("# USE GRAPH FOR FUNCTIONS IN MODULES ~w~n",[S#state.modules]),
    Vs0 = digraph:vertices(S#state.fgraph),
    Vs1 = keysplit(1, Vs0),
    {use, S#state.modules, usea2(Vs1, S)};

do_analyse(use, {M,F,A}, S) ->
    %% format("# USE GRAPH FOR FUNCTION ~w:~w/~w~n", [M, F, A]),
    {use,{M,F,A}, usea1([{M,F,A}], S)};

do_analyse(use, M, S) when atom(M) ->
    %% format("# USE GRAPH FOR FUNCTIONS IN MODULE ~w~n", [M]),
    Vs0 = digraph:vertices(S#state.fgraph),
    Vs1 = keysublist(M, 1, Vs0),
    {use, M, usea1(Vs1, S)};

do_analyse(module_call, [], S) ->
    %% format("# MODULE GRAPH FOR MODULES ~w~n", [S#state.modules]),
    Ms = digraph:edges(S#state.mgraph),
    Ms1 = lists:keysort(1, Ms),
    {module_call, S#state.modules, {mcall, Ms1}};

do_analyse(module_use, [], S) ->
    %% format("# MODULE USE FOR MODULES ~w~n", [S#state.modules]),
    Ms = digraph:edges(S#state.mgraph),
    Ms1 = lists:keysort(2, Ms),
    {module_use, S#state.modules, {muse, Ms1}};

do_analyse(undefined_functions, [], S) ->
    %% format("# UNDEFINED FUNCTIONS IN MODULES ~w~n", [S#state.modules]),
    Vs0 = digraph:vertices(S#state.fgraph),
    Vs1 = keysplit(1, Vs0),
    {undefined_functions, S#state.modules, undef2(Vs1, S)};

do_analyse(exports_not_called, [], S) ->
    %% format("# EXPORTED FUNCTIONS NOT CALLED IN MODULES ~w~n",[S#state.modules]),
    {exports_not_called, S#state.modules, not_called(S, exported)};

do_analyse(locals_not_called, [], S) ->
    %% format("# LOCALS FUNCTIONS NOT CALLED IN MODULES ~w~n",[S#state.modules]),
    {locals_not_called, S#state.modules, not_called(S, local)};

do_analyse(recursive_modules, [], S) ->
    %% format("# RECURSIVE MODULES ~n", []),
    Vs = digraph:vertices(S#state.mgraph),
    {recursive_modules, recursive_modules(Vs, S)};


do_analyse(user_defined, {M, F}, S) ->
    {user_defined, catch apply(M, F, [S#state.fgraph])};

do_analyse(user_defined, {M, F, ExtraArgs}, S) ->
    {user_defined, catch apply(M, F, [S#state.fgraph|ExtraArgs])};

do_analyse(Type, Arg, S) ->
    {error, {Type, Arg}}.

%%
%% For each module
%%

calla2(M,S) ->
    calla2(M,[],S).

calla2([M|Ms], Acc, S) ->
    calla2(Ms,[calla1(lists:keysort(2,M),[], S)|Acc], S);
calla2([], Acc, _) ->
    lists:reverse(Acc).

%%
%% For each function
%%
calla1(V,S) ->
    calla1(V,[],S).

calla1([V|Vs], Acc, S) ->
    case digraph:vertex(S#state.fgraph, V) of
	{{M,F,A},{local,File,Line}} ->
	    %{_M,File} = digraph:vertex(S#state.mgraph,M),
	    calla1(Vs, [{call1, local, 
			[File,Line,M,F,A],
			called(digraph:out_neighbours(
				 S#state.fgraph,V), M, S)}|Acc],S);
	{{M,F,A},{exported,File,Line}} ->
	    %{_M,File} = digraph:vertex(S#state.mgraph,M),
	    calla1(Vs, [{call1, exported, 
			 [File,Line,M,F,A],
			 called(digraph:out_neighbours(
				  S#state.fgraph,V), M, S)}|Acc],S);
	{{M,F,A},_} ->
	    calla1(Vs,Acc,S);
	false  ->
	    calla1(Vs,Acc,S) % should never happen
    end;
calla1([],Acc, _) -> lists:reverse(Acc).

%%
%% Do out neighbours
%%
called(Vs, M, S) -> 
    called(Vs, M, S, [], []).

called([V|Vs], M, S, Acc, Out) ->
    case lists:member(V, Acc) of
	true -> called(Vs, M, S, Acc, Out);
	false ->
	    Out1 = case V of
		       {M,F,A} -> 
			   %% format("~w/~w ", [F,A]),
			   [{called1,M,F,A}|Out];
		       {M1,F,A} -> 
			   %% format("~w:~w/~w ", [M1,F,A]),
			   [{called2,M1,F,A}|Out]
		       end,
	    called(Vs, M, S, [V|Acc], Out1)
    end;
called([], _, S, _, Out) -> 
    %% format("~n", []).
    {called, Out}.

%%
%% Do modules
%%
usea2(M, S) ->
    usea2(M,[],S).

usea2([M|Ms], Acc, S) ->
    usea2(Ms,[usea1(M, S)|Acc],S);
usea2([],Acc, _) ->
    lists:reverse(Acc).

%%
%% Do functions
%%
usea1(V, S) ->
    usea1(V,[],S).

usea1([V|Vs], Acc, S) ->
    case digraph:vertex(S#state.fgraph, V) of
	{{M,F,A},{local,File,Line}} ->
	    %{_M,File} = digraph:vertex(S#state.mgraph,M),
	    usea1(Vs,[{local_def, {File,Line,M,F,A},
	      caller(digraph:in_edges(S#state.fgraph,V), S)}|Acc], S);
	{{M,F,A},{exported,File,Line}} ->
	    %{_M,File} = digraph:vertex(S#state.mgraph,M),
	    usea1(Vs,[{exported_def, {File,Line,M,F,A},
	      caller(digraph:in_edges(S#state.fgraph,V), S)}|Acc], S);
	{{M,F,A},{{_,record},_,_}} ->
		usea1(Vs,Acc,S); % a record definition cannot be called, Skipped!
	{{M,F,A},{{Exp,Attr},File,Line}} ->
	    %{_M,File} = digraph:vertex(S#state.mgraph,M),
	    usea1(Vs, [{{Exp,Attr}, {File,Line,M,F,A},
			caller(digraph:in_edges(S#state.fgraph,V), S)}|Acc], S);  
	{{M,F,A}, _} ->
	    %% Undefined def 
	    case lists:member(M, S#state.modules) of
		true ->
		    usea1(Vs,[{undefined_def, {M,F,A},
		      caller(digraph:in_edges(S#state.fgraph, V), S)}|Acc], S);
		false ->
		    usea1(Vs,[{module_not_loaded,{M,F,A},
		      caller(digraph:in_edges(S#state.fgraph, V), S)}|Acc], S)
	    end;
	_ ->
	    usea1(Vs,Acc,S)
    end;

usea1([],Acc, _) -> 
    lists:reverse(Acc).



caller_not_local(M, [E|Es], S) ->
    {_,W,_,Line} = digraph:edge(S#state.fgraph, E),
    Ts = caller_not_local(M, Es, S),
    case digraph:vertex(S#state.fgraph, W) of
	{{M,F,A},{_,File,_}}  -> % skip this - caller is local
	    Ts;
	{{M1,F,A},{_,File,_}} ->  % caller of local is in another module
	    [{called_by, {File,Line,M,F,A}}|Ts];
	_ ->
	    Ts
    end;
caller_not_local(_, [], _) -> 
    [].

caller([E|Es], S) ->
    {_,W,_,Line} = digraph:edge(S#state.fgraph, E),
    Ts = caller(Es, S),
    case digraph:vertex(S#state.fgraph, W) of
	{{Mod,F,A},{_,File,_}} ->
	    %{_Mod,File} = digraph:vertex(S#state.mgraph,Mod),
	    %% format("~s:~w: called by ~w:~w/~w~n", [File,Line,Mod,F,A]),
	    [{called_by, {File,Line,Mod,F,A}}|Ts];
	Unexp ->
	    io:format("EXREF INTERNAL ERROR:~w~n",[Unexp])
    end;
caller([], _) -> 
    [].

not_called(S, Type) ->
    Vs0 = digraph:vertices(S#state.fgraph),
    Vs1 = lists:keysort(1, Vs0),  %% sort modules
    not_called(Vs1, S, Type).

not_called([V|Vs], S, Type) ->
    case digraph:vertex(S#state.fgraph, V) of
	{{M,F,A},{Type,File,Line}} ->
	    case digraph:in_degree(S#state.fgraph, V) of
		0 -> 
		    % format("~s:~w: ~w:~w/~w~n", [File,Line,M,F,A]),
		    [{not_called, {File,Line,M,F,A}}|
		     not_called(Vs, S, Type)];
		_ ->
		    not_called(Vs, S, Type)
	    end;
	_ ->
	    not_called(Vs, S, Type)
    end;
not_called([], S, Type) -> 
    [].

%%
%% For each module
%%
undef2(M, S) -> undef2(M, S, []).

undef2([M|Ms], S, Out) ->
    Out1 = undef1(lists:keysort(2,M), S, Out),
    undef2(Ms, S, Out1);
undef2([], _, Out) -> Out.

%%
%% For each functions
%%

undef1([V | Vs], S, Out) ->
    case digraph:vertex(S#state.fgraph, V) of
	{{erlang,F,A}, []} ->
	    case exref_internal:internal(erlang, F, A) of
		true -> 
		    undef1(Vs, S, Out);
		false ->
		    Out1 = undefined(erlang,F,A,S,Out),
		    undef1(Vs, S, Out1)
	    end;    
	{{'$M_EXPR',_,A}, []} -> % Not a real undef
	    undef1(Vs, S, Out);
	{{_, '$F_EXPR',A}, []} -> % Not a real undef
	    undef1(Vs, S, Out);
	{{M,F,A}, []} -> 
	    case exref_internal:internal(M, F, A) of
		true ->    % Not a real undef
		    undef1(Vs, S, Out);
		false ->   % F is called but not defined
		    case lists:member(M, S#state.modules) of
			true ->
			    Out1 = undefined(M, F, A, S, Out),
			    undef1(Vs, S, Out1);
			false ->
			    case is_lib_function(M, F, A) of
				true ->
				    undef1(Vs, S, Out);
				false ->
				    Out1 = undefined(M, F, A, S, Out),
				    undef1(Vs, S, Out1)
			    end
		    end
	    end;
	{{M,F,A}, {local,_,_}} ->
	    Out1 = 
		call_of_local(M,F,A,S,Out),
	    undef1(Vs, S, Out1);
	_ ->
	    undef1(Vs, S, Out)
    end;
undef1([], S, Out) -> Out.

call_of_local(M, F, A, S, Out) ->
    L = caller_not_local(M,
	  digraph:in_edges(S#state.fgraph, {M,F,A}), S),
    case L of
	[] -> Out;
	L -> [ {undefined, {M,F,A},L}|Out]
    end.

undefined(M, F, A, S, Out) ->
    %% format("Undefined ~w:~w/~w~n", [M, F, A]),
    [{undefined, {M,F,A}, 
     caller(digraph:in_edges(S#state.fgraph, {M,F,A}), S)}|Out].
    
%%
%% Check if a function is a library function
%%
is_lib_function(M, F, A) ->
    case code:which(M) of
	non_existing ->
	    false;
	preloaded ->
	    erlang:function_exported(M, F, A);
	File ->
	    Lib = as_string(code:lib_dir()),
	    Path = as_string(File),
	    case lists:prefix(Lib, Path) of
		true ->
		    code:ensure_loaded(M),
		    erlang:function_exported(M, F, A);
		false ->
		    false
	    end
    end.

as_string(X) when list(X) -> X;
as_string(X) when atom(X) -> atom_to_list(X);
as_string(X) when integer(X) -> integer_to_list(X).


%%
%% List recursive modules
%%    
recursive_modules(L, S) ->
    recursive_modules(L, S, []).

recursive_modules([M|Ms], S, Out) ->
    case digraph:get_cycle(S#state.mgraph, M) of
	false -> recursive_modules(Ms, S, Out);
	[M]   -> recursive_modules(Ms, S, Out);
	C -> 
%	    format("~w : ~w~n", [M, C]),
	    recursive_modules(Ms, S, [[M,C]|Out])
    end;
recursive_modules([], _, Out) -> Out.
    
%%
%% keysublist(Key, N, TupleList)
%%
%% Return a list where element(N,Tuple) == Key
%%
keysublist(Key, N, TupleList) ->
    keysublist(TupleList, N, Key, []).

keysublist([T|Ts], N, Key, Acc) when element(N,T) == Key ->
    keysublist(Ts, N, Key, [T|Acc]);
keysublist([T|Ts], N, Key, Acc) ->
    keysublist(Ts, N, Key, Acc);
keysublist([], _, _, Acc) -> Acc.

%%
%% keysplit(N, TupleList)
%%
%% Return a list of lists in wich
%% Each list has the same element(N, T).
%% Example: keysplit(1, [{1, a}, {2, b}, {1, b}, {3, a}])
%% returns [[{1, a}, {1, b}], [{2, b}], [{3, a}]].
%% There are 3 different lists, one for each represented
%% value in the first (since N=1) position of tuples (here: 1, 2, 3).
%% 
keysplit(N, TupleList) ->
    keysplit(lists:keysort(N, TupleList), N, [], [], []).

keysplit([T|Ts], N, Key, Acc1, Acc2) when element(N,T) == Key ->
    keysplit(Ts, N, Key, [T|Acc1], Acc2);
keysplit([T|Ts], N, [], Acc1, Acc2) ->
    keysplit(Ts, N, element(N,T), [T|Acc1], Acc2);
keysplit([T|Ts], N, Key, Acc1, Acc2) ->
    keysplit(Ts, N, element(N, T), [T], [Acc1|Acc2]);
keysplit([], _, _, Acc1, Acc2) -> 
    [Acc1|Acc2].



