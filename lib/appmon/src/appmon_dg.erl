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
%%------------------------------------------------------------
%%
%% Digraph handling for process view GUI. Feeble attempt at data
%% separation. Provides functional interface to the data structures
%% vdata and edata, v for vertex and e for edge.
%%
%%------------------------------------------------------------



-module(appmon_dg).

-include("appmon_dg.hrl").

%% Exports for vertices
-export([get/3, get/2, set/4, av/3, add/4, del/2, visited/3]).

%% Exports for edges
-export([eget/2, eget/3, eset/4, eadd/4, edel/2, ae/3]).

%% Exports for convenience
-export([print_dg/1]).


%%------------------------------------------------------------


eget(all, DG) ->
    digraph:edges(DG).

eget(data, DG, E) ->
    case digraph:edge(DG, E) of
	{_, V1, V2, Data} -> Data;
	Other    -> false
    end;
%%eget(from, DG, E) ->
%%    case digraph:edge(DG, E) of
%%	{_, V1, V2, Data} -> V1;
%%	Other    -> false
%%    end;
%%eget(to, DG, E) ->
%%    case digraph:edge(DG, E) of
%%	{_, V1, V2, Data} -> V2;
%%	Other    -> false
%%    end;
eget(edge, DG, {V1, V2}) ->
    case digraph:edge(DG, {V1, V2}) of
	{E, W1, W2, ED} -> {E, W1, W2, ED};
	Other ->
	    case digraph:edge(DG, {V2, V1}) of
		{E, W1, W2, ED} -> {E, W1, W2, ED};
		Other -> false
	    end
    end;

%% Weight in edge name
eget(edge, DG, {V1, V2, Weight}) ->
    case digraph:edge(DG, {V1, V2, Weight}) of
	{E, W1, W2, ED} -> {E, W1, W2, ED};
	Other -> false
    end;
%%	    case digraph:edge(DG, {V2, V1, Weight}) of
%%		{E, W1, W2, ED} -> {E, W1, W2, ED};
%%		Other -> false
%%	    end
%%    end;

%%eget(out, DG, V) ->
%%    efilter(DG, digraph:out_edges(DG, V));
eget(in, DG, V) ->
    efilter(DG, digraph:in_edges(DG, V)).


efilter(DG, Es) -> 
%%    io:format("efilter: ~p~n", [Es]),
    lists:filter(fun({V1, V2, primary}) -> true;
		 (E) -> false end,
		 Es).

%%			D = eget(data, DG, E),
%%			%%io:format("edata: ~p~n", [D]),
%%			case D#edata.weight of
%%			    primary -> true;
%%			    Other -> false
%%			end end,

eset(ref, DG, E, Ref) ->
    {E2, V1, V2, D} = eget(edge, DG, E),
%%    if  E2 /= E -> io:format("REAL ERROR: ~p ~p~n", [E, E2]);
%%	true -> ok
%%    end,
    update_e(DG, E2, D#edata{ref=Ref});
%%eset(weight, DG, E, Weight) ->
%%    {E2, V1, V2, D} = eget(edge, DG, E),
%%    ae(DG, E2, V1, V2, D#edata{weight=Weight});
eset(line, DG, E, Line) ->
    {E2, V1, V2, D} = eget(edge, DG, E),
%%    if  E2 /= E -> io:format("REAL ERROR: ~p ~p~n", [E, E2]);
%%	true -> ok
%%    end,
    update_e(DG, E2, D#edata{line=Line}).


edel(DG, E) ->
    digraph:del_edge(DG, E).


eadd(DG, E, D, Ref) ->
    case eget(edge, DG, E) of
	{_, _, _, ED} when record(ED, edata), ED#edata.ref == Ref ->
	    known;
	{_, _, _, ED} when record(ED, edata), ED#edata.ref /= Ref ->
	    update_e(DG, E, ED#edata{ref=Ref}),
	    updated;
	Other ->
	    ae(DG, E, D)
    end.

%%eadd(DG, V1, V2, D, Ref) ->
%%    E = {V1, V2},
%%    ET = {V2, V1},
%%    case eget(edge, DG, E) of
%%	{E2, _, _, ED} when record(ED, edata), ED#edata.ref == Ref ->
%%	    true;
%%	{E, _, _, ED} ->
%%	    eset(ref, DG, E, Ref), 
%%	    eset(weight, DG, E, primary), 
%%	    true;
%%	{ET, _, _, ED} ->
%%	    eturn(DG, ET),
%%	    eadd(DG, V1, V2, D, Ref);
%%	Other ->
%%	    R = ae(DG, E, D),
%%	    %%io:format("~p add edge ~p (~p)~n", [self(), E, R]),
%%	    R
%%    end.

%%%%	    edel(DG, {V2, V1}),
%%	    ae(DG, E, V1, V2, ED),
	%%eset(ref, DG, E, Ref), 
%%true;

%% NOTE: does not set ref on failure (unsure if someone depends on this)
%%evisited(DG, E, Ref) ->
%%    case eget(edge, DG, E) of
%%	{E2, V1, V2, ED} when record(ED, edata), ED#edata.ref == Ref -> true;
%%	Other -> false
%%    end.

%% No need for the weight repr. here
%%eturn(DG, E) ->
%%    {E2, V1, V2, D} = eget(edge, DG, E),
%%    edel(DG, E2),
%%    ae(DG, {V2, V1}, V2, V1, D).

%% Use sparingly (??)
%ae(DG, {V1, V2}, D) ->
%    R = digraph:add_edge(DG, {V1, V2}, V1, V2, D);
%% The weight repr.
ae(DG, {V1, V2, Weight}, D) ->
    digraph:add_edge(DG, {V1, V2, Weight}, V1, V2, D).
update_e(DG, {V1, V2, Weight}, D) ->
    digraph:del_edge(DG, {V1, V2, Weight}),
    digraph:add_edge(DG, {V1, V2, Weight}, V1, V2, D).

%ae(DG, E, V1, V2, D) ->
%    digraph:add_edge(DG, E, V1, V2, D).

%% Filter destination vertex from a list of edges
vfilter(DG, Vs) ->
%%    io:format("vfilter: ~p~n", [Vs]),
    lists:map(fun({V1, V2, Weight}) -> V2;
	      ({V1, V2}) -> V2
	     end, Vs).

get(all, DG) ->
    digraph:vertices(DG).

get(data, DG, {V1, V2}) ->
    case digraph:edge(DG, {V1, V2}) of
	{_,_,_,Data} -> Data;
	Other    -> false
    end;
get(data, DG, V) ->
    case digraph:vertex(DG, V) of
	{_,Data} -> Data;
	Other    -> false
    end;

%% Return all children of vertex V (those which V has edges to)
get(out, DG, V) ->
    vfilter(DG, efilter(DG, digraph:out_edges(DG, V)));
get(in, DG, V) ->
    digraph:in_neighbours(DG, V);
get(edges, DG, V) ->
    digraph:edges(DG, V);
get(w, DG, V) ->
    Data = get(data, DG, V),
    Data#vdata.width;
get(x, DG, V) ->
    Data = get(data, DG, V),
    Data#vdata.x.

set(type, DG, V, Type) ->
    D = get(data, DG, V),
    av(DG, V, D#vdata{type=Type});

set(ref, DG, V, Ref) ->
    D = get(data, DG, V),
    av(DG, V, D#vdata{ref=Ref});

set(y, DG, V, Y) ->
    D = get(data, DG, V),
    av(DG, V, D#vdata{y=Y});

set(data, DG, V, D) when record(D, vdata)->
    av(DG, V, D);

set(x, DG, V, X) ->
    %%io:format("Setting x (~p) of ~p (~p)~n", [V, X, get(data, DG, V)]),
    D = get(data, DG, V),
    if  D#vdata.x /= X -> 
	    av(DG, V, D#vdata{x=X});
	true -> true
    end.

%%set(y, DG, V, Y) ->
%%    %%io:format("Setting y (~p) of ~p (~p)~n", [V, Y, get(data, DG, V)]),
%%    D = get(data, DG, V),
%%    if  D#vdata.ref == Ref -> 
%%	    known;
%%	true ->
%%	    av(DG, V, D#vdata{y=Y, ref=Ref}), updated
%%    end.

visited(DG, {V1, V2}, Ref) ->			% for edge
    D = eget(data, DG, {V1, V2}),
    if  record(D, edata), D#edata.ref == Ref -> true;
	true -> false
    end;
visited(DG, V, Ref) ->
    D = get(data, DG, V),
    if  record(D, vdata), D#vdata.ref == Ref -> true;
	true -> false
    end.

add(DG, V, D, Ref) ->
    case get(data, DG, V) of
	D2 when record(D2, vdata), D2#vdata.ref==Ref -> 
	    io:format("Ooops in ~p:add vertex~n", [?MODULE]),
	    known;
	D2 when record(D2, vdata) -> 
	    %%io:format("~p touch vertex ~p~n", [self(), V]),
	    set(ref, DG, V, Ref), 
	    set(type, DG, V, D#vdata.type), 
	    save_coords(DG, V),
	    updated;
	Other -> 
	    %%io:format("~p add vertex ~p~n", [self(), V]),
	    av(DG, V, D), added
    end.

save_coords(DG, V) ->
    D = get(data, DG, V),
    D2 = D#vdata{origx=D#vdata.x, origy=D#vdata.y},
    av(DG, V, D2).

del(DG, V) ->
    digraph:del_vertex(DG, V).


av(DG, V, D) ->
    %%io:format("~p add vertex ~p~n", [self(), V]),
    digraph:add_vertex(DG, V, D).


    
print_dg(DG) ->
    io:format("Vertices:~n", []),
    lists:foreach(fun(V) -> io:format("  ~p ~p~n", 
				      [V, get(data, DG, V)]) end,
		  get(all, DG)),
    io:format("Edges:~n", []),
    lists:foreach(fun(V) -> io:format("  ~p ~p~n",
				      [V, eget(edge, DG, V)]) end,
		  eget(all, DG)),
    true.


